use Moops;
use MarpaX::Languages::M4::Impl::Parser;

# PODCLASSNAME

# ABSTRACT: M4 pre-processor - default implementation

#
# General note: having API'sed M4 introduce a difficulty when dealing
# with diversions: M4 is primilarly designed to act as a command-line
# and thus have a clear distinction between its internal buffer that is
# constantly being rewriten, and the stdout.
# But in the API version, undiverting number 0 (i.e. stdout) should go
# to the internal buffer, /without/ rescanning what has been undiverted.
#
# Therefore the position in variable output can be changed by undiverting number 0
# without rescanning.
#
# This is achieved in the parser implementation, that is maintaining itself
# the next position for scanning.
#
#
# Note: GNU-like extension but with different semantics:
# ------------------------------------------------------
# regexp   Perl regexp implementation, replacement string limited to & and digits.
# patsubst Perl regexp implementation, replacement string limited to & and digits.
# format   Perl sprintf implementation.
# incr     C.f. policy_integer_type, defaults to a 32 bits integer. "native" policy uses int, like GNU.
# decr     C.f. policy_integer_type, defaults to a 32 bits integer. "native" policy uses int, like GNU.
#
class MarpaX::Languages::M4::Impl::Default {
    extends 'MarpaX::Languages::M4::Impl::Parser';

    # VERSION

    use Bit::Vector;
    use Carp qw/croak/;
    use Encode::Locale;
    use Encode;
    use Env::Path qw/M4PATH/;
    use Errno;
    use File::Find;
    use File::Spec;
    use File::Temp;
    use IO::CaptureOutput qw/capture_exec/;
    use IO::Handle;
    use IO::File;
    use IO::Scalar;
    use MarpaX::Languages::M4::Impl::Default::BaseConversion;
    use MarpaX::Languages::M4::Impl::Default::Eval;
    use MarpaX::Languages::M4::Impl::Macros;
    use MarpaX::Languages::M4::Impl::Macro;
    use MarpaX::Languages::M4::Role::Impl;
    use MarpaX::Languages::M4::Type::Macro -all;
    use MarpaX::Languages::M4::Type::Impl -all;
    use MarpaX::Languages::M4::Type::Token -all;
    use Marpa::R2;
    use MooX::HandlesVia;

    BEGIN {
        #
        # Decode ARGV eventually
        #
        if ( !Undef->check( $ENV{M4_ARGV_ENCODING} )
            && length( $ENV{M4_ARGV_ENCODING} ) > 0 )
        {
#
# C.f. http://www.perl.com/pub/2012/04/perlunicookbook-decode-argv-as-local-encoding.html
#
            my @NEWARGV;
            try {
                @NEWARGV = map {
                    decode( $ENV{M4_ARGV_ENCODING} => $_, Encode::FB_CROAK )
                } @ARGV;
            }
            catch {
                my $error = join( "\n", @_ );
                $error =~ s/\s*\z//;
                croak(
                    "Cannot decode command-line arguments: $error. Change or remove M4_ARGV_ENCODING environment variable"
                );
                return;
            }
            finally {
                if ( !@_ ) {
                    @ARGV = @NEWARGV;
                }
            };
        }
        #
        # I want to process options and non-options in order BUT
        # I also want --help, --man and --usage to be processed
        # first. So I do an internal Getopt::Long at load time
        # and BEFORE loading MooX::Options to catch them.
        # It appears that I have no possible clash with my own options.
        #
        use Getopt::Long
            qw/:config bundling no_auto_help no_ignore_case pass_through/;
        my %opts = ( usage => 0, help => 0, man => 0 );
        GetOptions(
            "usage"  => \$opts{usage},
            "help|h" => \$opts{help},
            "man"    => \$opts{man}
        );
        my @BUILTIN_OPTIONS_FIRST = @ARGV;
        foreach ( keys %opts ) {
            if ( $opts{$_} ) {
                unshift( @BUILTIN_OPTIONS_FIRST, "--$_" );
            }
        }
        @ARGV = @BUILTIN_OPTIONS_FIRST;
    }

    use MooX::Options protect_argv => 0, flavour => [qw/require_order/];
    use MooX::Role::Logger;
    use POSIX qw/EXIT_SUCCESS EXIT_FAILURE/;
    use Perl::OSType ':all';
    use Types::Common::Numeric -all;

    # --------
    # Defaults
    # --------

    # -----------------------------------------------------------------
    # The list of GNU-like extensions is known in advanced and is fixed
    # -----------------------------------------------------------------
    our %Default_EXTENSIONS = (

        #        __file__    => 1, # TO DO
        #        __line__    => 1, # TO DO
        __program__ => 1,
        builtin     => 1,
        changeword  => 1,
        debugmode   => 1,
        debugfile   => 1,
        esyscmd     => 1,
        format      => 1,
        indir       => 1,
        patsubst    => 1,
        regexp      => 1,
        __gnu__     => 1,
        __os2__     => 1,
        os2         => 1,
        __unix__    => 1,
        unix        => 1,
        __windows__ => 1,
        windows     => 1,
    );

    #
    # Comments are recognized in preference to macros.
    # Comments are recognized in preference to argument collection.
    # Macros are recognized in preference to the begin-quote string.
    # Quotes are recognized in preference to argument collection.
    #

    #
    # Eval: constants for radix and the grammar
    #
    our @nums = ( 0 .. 9, 'a' .. 'z', 'A' .. 'Z' );
    our %nums = map { $nums[$_] => $_ } 0 .. $#nums;
    our $EVAL_G = Marpa::R2::Scanless::G->new(
        {   source => \<<EVAL_GRAMMAR
:default ::= action => ::first
:start ::= eval
eval ::= Expression                             action => _eval

Expression ::=
    Number
    | ('(') Expression (')') assoc => group
    # Catch common invalid operations for a nice error message
    # Uncatched stuff will have the Marpa native exception.
   || '++' (Expression)                         action => _invalidOp
    | (Expression) '+='  (Expression)           action => _invalidOp
    | (Expression) '--'  (Expression)           action => _invalidOp
    | (Expression) '-='  (Expression)           action => _invalidOp
    | (Expression) '*='  (Expression)           action => _invalidOp
    | (Expression) '/='  (Expression)           action => _invalidOp
    | (Expression) '%='  (Expression)           action => _invalidOp
    | (Expression) '>>=' (Expression)           action => _invalidOp
    | (Expression) '<<=' (Expression)           action => _invalidOp
    | (Expression) '^='  (Expression)           action => _invalidOp
    | (Expression) '&='  (Expression)           action => _invalidOp
    | (Expression) '|='  (Expression)           action => _invalidOp
   || '+' Expression                            action => _noop
    | '-' Expression                            action => _neg
    | '~' Expression                            action => _bneg
    | '!' Expression                            action => _lneg
   || Expression '**' Expression assoc => right action => _exp
   || Expression '*'  Expression                action => _mul
    | Expression '/'  Expression                action => _div
    | Expression '%'  Expression                action => _mod
   || Expression '+'  Expression                action => _add
    | Expression '-'  Expression                action => _sub
   || Expression '<<' Expression                action => _left
    | Expression '>>' Expression                action => _right
   || Expression '>'  Expression                action => _gt
    | Expression '>=' Expression                action => _ge
    | Expression '<'  Expression                action => _lt
    | Expression '<=' Expression                action => _le
   || Expression '==' Expression                action => _eq
    # Special case of '=' aliased to '=='
    | Expression '='  Expression                action => _eq2
    | Expression '!=' Expression                action => _ne
   || Expression '&'  Expression                action => _band
   || Expression '^'  Expression                action => _bxor
   || Expression '|'  Expression                action => _bor
   || Expression '&&' Expression                action => _land
   || Expression '||' Expression                action => _lor

Number ::= decimalNumber                        action => _decimal
         | octalNumber                          action => _octal
         | hexaNumber                           action => _hex
         | binaryNumber                         action => _binary
         | radixNumber                          action => _radix

_DECDIGITS   ~ [0-9]+
_OCTDIGITS   ~ [0-7]+
_HEXDIGITS   ~ [0-9a-fA-F]+
_BINDIGITS   ~ [0-1]+
_RADIXDIGITS ~ [0-9a-zA-Z]+
_RADIX ~  '1' |  '2' |  '3' |  '4' |  '5' |  '6' |  '7' |  '8' |  '9'
| '10' | '11' | '12' | '13' | '14' | '15' | '16' | '17' | '18' | '19'
| '20' | '21' | '22' | '23' | '24' | '25' | '26' | '27' | '28' | '29'
| '30' | '31' | '32' | '33' | '34' | '35' | '36'

decimalNumber ~      _DECDIGITS
octalNumber   ~ '0'  _OCTDIGITS
hexaNumber    ~ '0x' _HEXDIGITS
binaryNumber  ~ '0b' _BINDIGITS
radixNumber   ~ '0r' _RADIX     ':' _RADIXDIGITS

_WS_many ~ [\\s]+
:discard ~ _WS_many
EVAL_GRAMMAR
        }
    );

    # ------------------------
    # PROCESS OPTIONS IN ORDER
    # ------------------------
    around new_with_options {
        #
        # $self is in reality a $class
        #
        my $class = $self;
        $self = $class->${^NEXT}(@_);
        while (@ARGV) {
            #
            # Process this non-option
            #
            my $file = shift(@ARGV);
            my $fh;
            try {
                $fh = IO::File->new(
                    $ENV{M4_ENCODE_LOCALE}
                    ? encode( locale_fs => $file )
                    : $file,
                    'r'
                    )
                    || die "$file: $!";
                if ( $ENV{M4_ENCODE_LOCALE} ) {
                    binmode( $fh, ':encoding(locale)' );
                }
            }
            catch {
                $self->logger_error( '%s: %s', $file, $@ );
                return;
            };
            if ( !Undef->check($fh) ) {
                $self->impl_parse(
                    do { local $/; <$fh> }
                );
            }
            #
            # Merge next option values
            #
            my %nextOpts = $class->parse_options();
            foreach ( keys %nextOpts ) {
                #
                # Look to options. I made sure all ArrayRef have
                # an 'elements' handle.
                #
                if ( ArrayRef->check( $nextOpts{$_} ) ) {
                    my $elementsMethod = $_ . '_elements';
                    $self->$_(
                        [ $self->$elementsMethod, @{ $nextOpts{$_} } ] );
                }
                else {
                    $self->$_( $nextOpts{$_} );
                }
            }
        }
        return $self;
    }

    # ---------------------------------------------------------------
    # OPTIONS
    # ---------------------------------------------------------------
    # * Options always have triggers
    # * If an option xxx maps to an internal attribute _xxx,
    #   this attribute is always rwp + lazy + builder
    # ---------------------------------------------------------------

    # =========================
    # --cmdtounix
    # =========================
    option cmdtounix => (
        is      => 'rw',
        isa     => Bool,
        trigger => 1,
        doc =>
            q{Convert any command output from platform's native end-of-line character set to Unix style (LF). Default to a false value.}
    );
    has _cmdtounix => ( is => 'rwp', isa => Bool, lazy => 1, builder => 1 );

    method _trigger_cmdtounix (Bool $cmdtounix, @rest --> Undef) {
        $self->_set__cmdtounix($cmdtounix);
        return;
    }

    method _build__cmdtounix {false}

    # =========================
    # --inctounix
    # =========================
    option inctounix => (
        is      => 'rw',
        isa     => Bool,
        trigger => 1,
        doc =>
            q{Convert any included file from platform's native end-of-line character set to Unix style (LF). Default to a false value.}
    );
    has _inctounix => ( is => 'rwp', isa => Bool, lazy => 1, builder => 1 );

    method _trigger_inctounix (Bool $inctounix, @rest --> Undef) {
        $self->_set__inctounix($inctounix);
        return;
    }

    method _build__inctounix {false}

    # =========================
    # --tokens-priority
    # =========================
    our $DEFAULT_TOKENS_PRIORITY = [qw/COMMENT WORD QUOTEDSTRING CHARACTER/];
    option tokens_priority => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        format      => 's@',
        autosplit   => ',',
        trigger     => 1,
        handles_via => 'Array',
        handles     => { tokens_priority_elements => 'elements' },
        doc =>
            "Tokens priority. If setted, it is highly recommended to list all allowed values, that are : \"WORD\", \"MACRO\", \"QUOTEDSTRING\", and \"COMMENT\". The order of appearance on the command-line will be the prefered order when parsing M4 input. Multiple values can be given in the same switch if separated by the comma character ','. Unlisted values will keep their relative order from the default, which is: "
            . join( ',',
            @{$DEFAULT_TOKENS_PRIORITY}
                . ". Please note that when doing arguments collection, the parser forces unquoted parenthesis and comma to have higher priority to quoted strings and comments."
            )
    );
    has _tokens_priority => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => ArrayRef [M4Token],
        handles_via => 'Array',
        handles     => {
            _tokens_priority_elements => 'elements',
            _tokens_priority_count    => 'count',
            _tokens_priority_get      => 'get'
        },
    );

    method _trigger_tokens_priority (ArrayRef[Str] $tokens_priority, @rest --> Undef) {
        my %tokens_priority = ();
        my $currentMaxIndex = $#{$tokens_priority};
        foreach ( 0 .. $currentMaxIndex ) {
            $tokens_priority{ $tokens_priority->[$_] } = $_;
        }
        foreach ( 0 .. $self->_tokens_priority_count - 1 ) {
            my $lexeme = $self->_tokens_priority_get($_);
            if ( !exists( $tokens_priority{$lexeme} ) ) {
                $tokens_priority{$lexeme} = ++$currentMaxIndex;
            }
        }

        $self->_set__tokens_priority(
            [   sort { $tokens_priority{$a} <=> $tokens_priority{$b} }
                    keys %tokens_priority
            ]
        );
        return;
    }

    method _build__tokens_priority {$DEFAULT_TOKENS_PRIORITY}

    # =========================
    # --integer-type
    # =========================
    option integer_type => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            q{Integer type. Possible values: "native" (will use what your hardware provides), "bitvector" (will use s/w-driven bit-per-bit manipulations; this is the only portable option value). Default: "bitvector".}
    );
    has _integer_type => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Enum [qw/native bitvector/]
    );

    method _trigger_integer_type (Str $integer_type, @rest --> Undef) {
        $self->_set__integer_type($integer_type);
        return;
    }

    method _build__integer_type {'bitvector'}

    # =========================
    # --integer-bits
    # =========================
    our $INTEGER_BITS_DEFAULT_VALUE = 32;
    option integer_bits => (
        is      => 'rw',
        isa     => PositiveInt,
        trigger => 1,
        format  => 'i',
        doc =>
            "Number of bits for integer arithmetic. Possible values: any positive integer. Default is \"unsigned int\" C-type number of bits from the libc used to build this version of perl. Meaningul only when policy_integer_type is \"bitvector\". Default: $INTEGER_BITS_DEFAULT_VALUE."
    );

    has _integer_bits => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveInt,
    );

    method _trigger_integer_bits (Str $integer_bits, @rest --> Undef) {
        $self->_set__integer_bits($integer_bits);
        return;
    }

    method _build__integer_bits {$INTEGER_BITS_DEFAULT_VALUE}

    # =========================
    # --m4wrap-order
    # =========================
    option m4wrap_order => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            q{M4wrap unbuffer mode. Possible values: "LIFO" (Last In, First Out), "FIFO" (First In, First Out). Default: "LIFO".}
    );

    has _m4wrap_order => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Enum [qw/LIFO FIFO/]
    );

    method _trigger_m4wrap_order (Str $m4wrap_order, @rest --> Undef) {
        $self->_set__m4wrap_order($m4wrap_order);
        return;
    }

    method _build__m4wrap_order {'LIFO'}

    # =========================
    # --divert-type
    # =========================
    option divert_type => (
        is      => 'rw',
        trigger => 1,
        isa     => Str,
        format  => 's',
        doc =>
            q{Divertion type. Possible values: "memory" (all diversions are kept in memory), "temp" (all diversions are kept in temporary files). Default: "memory".}
    );

    has _divert_type => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Enum [qw/memory file/]
    );

    method _trigger_divert_type (Str $divert_type, @rest --> Undef) {
        $self->_set__divert_type($divert_type);
        return;
    }

    method _build__divert_type {'memory'}

    # =========================
    # --builtin-need-param
    # =========================
    our $NEED_PARAM_DEFAULT_VALUE = [
        qw/
            define
            undefine
            defn
            pushdef
            popdef
            indir
            builtin
            ifdef
            ifelse
            shift
            changeword
            m4wrap
            include
            sinclude
            len
            index
            regexp
            substr
            translit
            patsubst
            format
            incr
            decr
            eval
            syscmd
            esyscmd
            mkstemp
            maketemp
            errprint
            /
    ];
    option builtin_need_param => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        trigger     => 1,
        format      => 's@',
        autosplit   => ',',
        handles_via => 'Array',
        handles     => { builtin_need_param_elements => 'elements', },
        doc =>
            "Recognized-only-with-parameters policy. Repeatable option. Multiple values can be given in the same switch if separated by the comma character ','. Says if a macro is recognized only if it is immediately followed by a left parenthesis. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then the option is considered. Any attempt to set it on the command-line will completely overwrite the default. Default: "
            . join( ',', @{$NEED_PARAM_DEFAULT_VALUE} ) . '.'
    );

    has _builtin_need_param => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => HashRef [Bool],
        handles_via => 'Hash',
        handles     => {
            _builtin_need_param_set    => 'set',
            _builtin_need_param_get    => 'get',
            _builtin_need_param_exists => 'exists',
            _builtin_need_param_keys   => 'keys',
            _builtin_need_param_delete => 'delete'
        },
    );

    method _trigger_builtin_need_param (ArrayRef[Str] $builtin_need_param, @rest --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$builtin_need_param} ) {
            if ( $_ =~ /^$word_regexp$/ ) {
                if ( !Undef->check( $-[1] ) && $+[1] > $-[1] ) {
                    $self->_builtin_need_param_set(
                        substr( $_, $-[1], $+[1] - $-[1] ), true );
                }
                else {
                    $self->_builtin_need_param_set( $_, true );
                }
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'builtin_need_param', $_ );
            }
        }
        return;
    }

    method _build__builtin_need_param {
        my %ref = map { $_ => true } @{$NEED_PARAM_DEFAULT_VALUE};
        \%ref;
    }

    # =========================
    # --param-can-be-macro
    # =========================
    our $PARAMCANBEMACRO_DEFAULT_VALUE = {
        define => {
            0 => true,    # To trigger a warning
            1 => true
        },
        pushdef => { 1 => true },
        indir   => {
            '*' => true    # To trigger a warning
        },
        builtin => {
            '*' => true    # To trigger a warning
        },
    };
    option param_can_be_macro => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        trigger     => 1,
        format      => 's@',
        autosplit   => ',',
        handles_via => 'Array',
        handles     => { param_can_be_macro_elements => 'elements', },
        doc =>
            "Can-a-macro-parameter-be-an-internal-macro-token policy. Repeatable option. Multiple values can be given in the same switch if separated by the comma character ','. Says if a macro parameter can be an internal token, i.e. a reference to another macro. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then the option is considered. On the command-line, the format has to be: word-regexp=?numbersOrStarSeparatedByColon?. For example: --policy_paramcanbemacro popdef,ifelse=,define=1,xxx=3:4,yyy=* says that popdef and ifelse do not accept any parameter as macro, but parameter at indice 1 of the define macro can be such internal token, as well as indices 3 and 4 of xxx macro, and any indices of macro yyy. Any attempt to set it on the command-line will completely overwrite the default. Default: "
            . join(
            ',',
            map {
                my $macroName = $_;
                "$macroName=" . join(
                    ':',
                    grep {
                        $PARAMCANBEMACRO_DEFAULT_VALUE->{$macroName}->{$_}
                        } keys
                        %{ $PARAMCANBEMACRO_DEFAULT_VALUE->{$macroName} }
                    )
            } keys %{$PARAMCANBEMACRO_DEFAULT_VALUE}
            )
            . '.'
    );

    has _param_can_be_macro => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => HashRef [ HashRef [ PositiveOrZeroInt | Enum [qw/*/] ] ],
        handles_via => 'Hash',
        handles     => {
            _param_can_be_macro_set    => 'set',
            _param_can_be_macro_get    => 'get',
            _param_can_be_macro_exists => 'exists',
            _param_can_be_macro_keys   => 'keys',
            _param_can_be_macro_delete => 'delete'
        },
    );

    method _trigger_param_can_be_macro (ArrayRef[Str] $param_can_be_macro, @rest --> Undef) {
        my $word_regexp = $self->_word_regexp;
        my %ref         = ();
        foreach ( @{$param_can_be_macro} ) {
            if ( $_ =~ /^($word_regexp)(?:=(.*))/ ) {
                #
                # If word_regexp has a $1 submatch, this is the macro name.
                # Since we add another group on top, we detect the presence of
                # word_regexp's $1 by comparing total number of groups v.s.
                # the regexp writen upper.
                #
                my $nbSubGroups = $#+;
                #
                # If $#+ > 2, then $word_regexp has a $1.
                # Else, it has no $1.
                #
                my $macroName
                    = ( $#+ > 2 )
                    ? substr( $_, $-[2], $+[2] - $-[2] )
                    : substr( $_, $-[1], $+[1] - $-[1] );
                my $indicesToSplit = substr( $_, $-[-1], $+[-1] - $-[-1] );
                my @indices = grep { !Undef->check($_) && length("$_") > 0 }
                    split( /,/, $indicesToSplit );
                $ref{$macroName} = {};
                foreach (@indices) {
                    if ( PositiveOrZeroInt->check($_)
                        || ( Str->check($_) && $_ eq '*' ) )
                    {
                        $ref{$macroName}->{$_} = true;
                    }
                    else {
                        $self->logger_warn(
                            '%s: %s: %s does not look like a positive or zero integer, or star character',
                            'policy_paramcanbemacro', $macroName, $_
                        );
                    }
                }
            }
            else {
                $self->logger_warn( '%s: %s does not match a word regexp',
                    'policy_paramcanbemacro', $_ );
            }
        }
        $self->_set__param_can_be_macro( \%ref );
        return;
    }

    sub _build__param_can_be_macro {
        $PARAMCANBEMACRO_DEFAULT_VALUE;
    }    #__METHOD 17

    # =========================
    # --interactive
    # =========================
    option interactive => (
        is      => 'rw',
        isa     => Bool,
        short   => 'i',
        trigger => 1,
        doc     => q{Read STDIN and parse it line by line, until EOF.}
    );

    method _trigger_interactive {
        my $buf;
        my $unparsed;

        STDOUT->autoflush();

        #
        # If interactive mode is triggered via new_with_options()
        # the caller is likely to not have $self yet.
        #
        local $MarpaX::Languages::M4::SELF = $self;
        while ( defined( $_ = <STDIN> ) ) {
            if ( !defined($buf) ) {
                $buf = $_;
            }
            else {
                $buf = $self->impl_unparsed . $_;
            }
            $self->impl_parseIncremental($buf);
            print $self->impl_value;
            ${ $self->impl_valueRef } = '';
        }
    }

    # =========================
    # --version
    # =========================
    option version => (
        is      => 'rw',
        isa     => Bool,
        short   => 'v',
        trigger => 1,
        doc =>
            q{Print the version number of the program on standard output, then immediately exit.}
    );

    method _trigger_version {
        my $CURRENTVERSION;
        {
           #
           # Because $VERSION is generated by dzil, not available in dev. tree
           #
            no strict 'vars';
            $CURRENTVERSION = $VERSION;
        }
        $CURRENTVERSION ||= 'dev';

        print "Version $CURRENTVERSION\n";
        exit(EXIT_SUCCESS);
    }

    # =========================
    # --prefix-builtins
    # =========================
    option prefix_builtins => (
        is      => 'rw',
        isa     => Bool,
        short   => 'P',
        trigger => 1,
        doc =>
            q{Prefix of all builtin macros with 'm4_'. Default: a false value.}
    );

    has _prefix_builtins => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Str,
    );

    method _trigger_prefix_builtins (Bool $prefix_builtins, @rest --> Undef) {
        $self->_set__prefix_builtins('m4_');
        return;
    }
    method _build__prefix_builtins {''}

    # =========================
    # --fatal-warnings
    # =========================
    option fatal_warnings => (
        is         => 'rw',
        isa        => PositiveInt,
        repeatable => 1,
        repeatable => 1,
        short      => 'E',
        trigger    => 1,
        doc =>
            q{If unspecified, have no effect. If specified once, impl_rc() will return EXIT_FAILURE. If specified more than once, any warning is fatal. Default: a false value.}
    );

    has _fatal_warnings => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveOrZeroInt
    );

    method _trigger_fatal_warnings (PositiveInt $fatal_warnings, @rest --> Undef) {
        $self->_set__fatal_warnings($fatal_warnings);
        return;
    }

    method _build__fatal_warnings {0}

    # =========================
    # --silent
    # =========================
    option silent => (
        is      => 'rw',
        default => false,
        short   => 'Q',
        doc =>
            q{Silent mode. If true all warnings will disappear. Default: a false value.}
    );

    has _silent => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
    );

    method _trigger_silent (Bool $silent, @rest --> Undef) {
        $self->_set__silent($silent);
        return;
    }

    method _build__silent {false}

    # =========================
    # --trace
    # =========================
    option trace => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        format      => 's@',
        short       => 't',
        autosplit   => ',',
        trigger     => 1,
        handles_via => 'Array',
        handles     => { trace_elements => 'elements', },
        doc =>
            q{Trace mode. Repeatable option. Multiple values can be given in the same switch if separated by the comma character ','. Every option value will set trace on the macro sharing this name. Default is empty.}
    );

    has _trace => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => HashRef [Bool],
        handles_via => 'Hash',
        handles     => {
            _trace_set    => 'set',
            _trace_get    => 'get',
            _trace_exists => 'exists',
            _trace_keys   => 'keys',
            _trace_delete => 'delete'
        }
    );

    method _trigger_trace (ArrayRef[Str] $arrayRef, @rest --> Undef) {
        foreach ( @{$arrayRef} ) {
            $self->_trace_set($_);
        }
        return;
    }
    method _build__trace { {} }

    # =========================
    # --define
    # =========================
    option define => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        handles_via => 'Array',
        handles     => { define_elements => 'elements', },
        format      => 's@',
        short       => 'D',
        trigger     => 1,
        doc =>
            q{Macro definition. Repeatable option. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then a macro is declared. For example: --define myMacro. Or --word_regexp x= --define x=. Default expansion is void, unless the matched name is followed by '=', then any remaining character will be the expansion of this new macro. For example: --define myMacro=myExpansion. Or --word_regexp x= --define x==myExpansion. Default is empty.}
    );

    method _trigger_define (ArrayRef[Str] $arrayRef, @rest --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$arrayRef} ) {
            if ( $_ =~ /^($word_regexp)(?:=(.*))/ ) {
                #
                # If $#+ > 2, then $word_regexp has a $1.
                # Else, it has no $1.
                #
                my $macroName
                    = ( $#+ > 2 )
                    ? substr( $_, $-[2], $+[2] - $-[2] )
                    : substr( $_, $-[1], $+[1] - $-[1] );
                my $value = substr( $_, $-[-1], $+[-1] - $-[-1] );
                $self->builtin_define( $macroName, $value );
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'define', $_ );
            }
        }
        return;
    }

    # =========================
    # --undefine
    # =========================
    option undefine => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        handles_via => 'Array',
        handles     => { undefine_elements => 'elements', },
        format      => 's',
        short       => 'U',
        repeatable  => 1,
        trigger     => 1,
        doc =>
            q{Macro undefinition. Repeatable option. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then a macro is deleted if it exists. Default is empty.}
    );

    method _trigger_undefine (ArrayRef[Str] $arrayRef, @rest --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$arrayRef} ) {
            if ( $_ =~ /^$word_regexp$/ ) {
                if ( !Undef->check( $-[1] ) && $+[1] > $-[1] ) {
                    $self->builtin_undefine(
                        substr( $_, $-[1], $+[1] - $-[1] ) );
                }
                else {
                    $self->builtin_undefine($_);
                }
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'undefine', $_ );
            }
        }
        return;
    }

    # =========================
    # --prepend-include
    # =========================
    option prepend_include => (
        is      => 'rw',
        isa     => ArrayRef [Str],
        format  => 's@',
        short   => 'B',
        trigger => 1,
        doc =>
            q{Include directory. Repeatable option. Will be used in reverse order and before current directory when searching for a file to include. Default is empty.}
    );

    has _prepend_include => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => ArrayRef [Str],
        handles_via => 'Array',
        handles     => { _prepend_include_elements => 'elements', },
    );

    method _trigger_prepend_include (ArrayRef[Str] $prepend_include, @rest --> Undef) {
        $self->_set__prepend_include($prepend_include);
        return;
    }
    method _build__prepend_include { [] }

    # =========================
    # --include
    # =========================
    option include => (
        is      => 'rw',
        isa     => ArrayRef [Str],
        format  => 's@',
        short   => 'I',
        trigger => 1,
        doc =>
            q{Include directory. Repeatable option. Will be used in order and after current directory when searching for a file to include. Default is empty.}
    );

    has _include => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => ArrayRef [Str],
        handles_via => 'Array',
        handles     => { _include_elements => 'elements', },
    );

    method _trigger_include (ArrayRef[Str] $include, @rest --> Undef) {
        $self->_set__include($include);
        return;
    }
    method _build__include { [] }

    # =========================
    # --gnu
    # =========================
    option gnu => (
        is      => 'rw',
        isa     => Bool,
        short   => 'g',
        trigger => 1,
        doc     => q{Enable all extensions.}
    );

    has _no_gnu_extensions => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Bool
    );

    method _trigger_gnu (Bool $gnu, @rest --> Undef) {
        $self->_set__no_gnu_extensions( !$gnu );
        return;
    }
    method _build__no_gnu_extensions {false}

    # =========================
    # --traditional
    # =========================
    option traditional => (
        is      => 'rw',
        isa     => Bool,
        short   => 'G',
        trigger => 1,
        doc     => q{Suppress all extensions.}
    );

    method _trigger_traditional (Bool $traditional, @rest --> Undef) {
        $self->_set__no_gnu_extensions($traditional);
        return;
    }

    # =========================
    # --debugmode
    # =========================
    our @DEBUG_FLAGS         = qw/a c e f i l p q t x/;
    our @DEFAULT_DEBUG_FLAGS = qw/a e q/;
    option debugmode => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        short   => 'd',
        doc => 'Debug mode. This is a combinaison of flags, that can be: "'
            . join( '", "', @DEBUG_FLAGS )
            . '", or "V" wich will put everything on. Default: "'
            . join( '', @DEFAULT_DEBUG_FLAGS ) . '".'
    );

    has _debug => (
        is          => 'rwp',
        lazy        => 1,
        builder     => 1,
        isa         => HashRef [Bool],
        handles_via => 'Hash',
        handles     => {
            _debug_set    => 'set',
            _debug_get    => 'get',
            _debug_exists => 'exists',
            _debug_keys   => 'keys',
            _debug_delete => 'delete'
        }
    );

    method _trigger_debugmode (Str $flags, @rest --> Undef) {

        map { $self->_debug_set( $_, false ) } @DEBUG_FLAGS;

        if ( length($flags) <= 0 ) {
            map { $self->_debug_set( $_, true ) } @DEFAULT_DEBUG_FLAGS;
        }
        else {
            #
            # Only know debug flags are accepted
            #
            my $ok = 1;
            my @flags = split( //, $flags );
            foreach ( @flags, 'V' ) {
                if ( !$self->_debug_exists($_) && $_ ne 'V' ) {
                    $self->logger_warn( '%s: unknown debug flag: %c',
                        'debugmode', $_ );
                    $ok = 0;
                    last;
                }
            }
            if ( !$ok ) {
                return;
            }
            if ( index( $flags, 'V' ) >= 0 ) {
                #
                # Everything is on
                #
                map { $self->_debug_set( $_, true ) } @DEBUG_FLAGS;
            }
            else {
                map { $self->_debug_set( $_, false ) } @DEBUG_FLAGS;
                map { $self->_debug_set( $_, true ) } @flags;
            }
        }

        return;
    }

    method _build__debug {
        my %ref = ();
        map { $ref{$_} = false } @DEBUG_FLAGS;
        map { $ref{$_} = true } @DEFAULT_DEBUG_FLAGS;
        return \%ref;
    }

    # =========================
    # --debugfile
    # =========================
    option debugfile => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        short   => 'o',
        doc =>
            q{Debug file. An empty value disable debug output. A null value redirects to standard error. Default is a null value.}
    );

    has _debugfile => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Undef | Str,
    );

    method _trigger_debugfile (Str $debugfile, @rest --> Undef) {
        $self->_set__debugfile($debugfile);
    }

    method _build__debugfile {undef}

    # =========================
    # --quote-start
    # =========================
    our $DEFAULT_QUOTE_START = '`';
    option quote_start => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            "Quote start. An empty option value is ignored. Default: \"$DEFAULT_QUOTE_START\"."
    );

    has _quote_start => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        trigger => 1,
        isa     => Str,
    );

    has _quoteStartLength => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveOrZeroInt
    );

    method _trigger_quote_start (Str $quote_start, @rest --> Undef) {
        if ( length($quote_start) > 0 ) {
            $self->_set__quote_start($quote_start);
        }
    }

    method _trigger__quote_start (Str $quote_start, @rest --> Undef) {
        $self->_set__quoteStartLength( length($quote_start) );
    }

    method _build__quote_start      {$DEFAULT_QUOTE_START}
    method _build__quoteStartLength { length($DEFAULT_QUOTE_START) }

    # =========================
    # --quote-end
    # =========================
    our $DEFAULT_QUOTE_END = '\'';
    option quote_end => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            "Quote end. An empty option value is ignored. Default: \"$DEFAULT_QUOTE_END\"."
    );

    has _quote_end => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        trigger => 1,
        isa     => Str,
    );

    has _quoteEndLength => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveOrZeroInt
    );

    method _trigger_quote_end (Str $quote_end, @rest --> Undef) {
        if ( length($quote_end) > 0 ) {
            $self->_set__quote_end($quote_end);
        }
    }

    method _trigger__quote_end (Str $quote_end, @rest --> Undef) {
        $self->_set__quoteEndLength( length($quote_end) );
    }

    method _build__quote_end      {$DEFAULT_QUOTE_END}
    method _build__quoteEndLength { length($DEFAULT_QUOTE_END) }

    # =========================
    # --comment-start
    # =========================
    our $DEFAULT_COMMENT_START = '#';
    option comment_start => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            "Comment start. An empty option value is ignored. Default: \"$DEFAULT_COMMENT_START\"."
    );

    has _comment_start => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        trigger => 1,
        isa     => Str,
    );

    has _commentStartLength => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveOrZeroInt
    );

    method _trigger_comment_start (Str $comment_start, @rest --> Undef) {
        if ( length($comment_start) > 0 ) {
            $self->_set__comment_start($comment_start);
        }
    }

    method _trigger__comment_start (Str $comment_start, @rest --> Undef) {
        $self->_set__commentStartLength( length($comment_start) );
    }

    method _build__comment_start {$DEFAULT_COMMENT_START}

    sub _build__commentStartLength {
        length($DEFAULT_COMMENT_START);
    }    #__METHOD 52

    # =========================
    # --comment-end
    # =========================
    our $DEFAULT_COMMENT_END = "\n";
    option comment_end => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            "Comment end. An empty option value is ignored. Default value: the newline character."
    );

    has _comment_end => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        trigger => 1,
        isa     => Str,
    );

    has _commentEndLength => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => PositiveOrZeroInt
    );

    method _trigger_comment_end (Str $comment_end, @rest --> Undef) {
        if ( length($comment_end) > 0 ) {
            $self->_set__comment_end($comment_end);
        }
    }

    method _trigger__comment_end (Str $comment_end, @rest --> Undef) {
        $self->_set__commentEndLength( length($comment_end) );
    }

    method _build__comment_end      {$DEFAULT_COMMENT_END}
    method _build__commentEndLength { length($DEFAULT_COMMENT_END) }

    # =========================
    # --word-regexp
    # =========================
    our $DEFAULT_WORD_REGEXP = '[_a-zA-Z][_a-zA-Z0-9]*';
    option word_regexp => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        format  => 's',
        short   => 'W',
        doc =>
            "Word perl regular expression. Default: \"$DEFAULT_WORD_REGEXP\"."
    );

    has _word_regexp => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => RegexpRef
    );

    method _trigger_word_regexp (Str $regexp, @rest --> Undef) {
        try {
            $self->_set__word_regexp(qr/^$regexp/s);
        }
        catch {
            $self->logger_error( '%s: %s', $self->impl_quote($regexp), $_ );
            return;
        };

        return;
    }

    method _build__word_regexp {qr/^$DEFAULT_WORD_REGEXP/}

    # =========================
    # --warn-macro-sequence
    # =========================
    our $DEFAULT_WARN_MACRO_SEQUENCE = '\$(?:\{[^\}]*\}|[0-9][0-9]+)';
    option warn_macro_sequence => (
        is      => 'rw',
        isa     => Str,
        default => $DEFAULT_WARN_MACRO_SEQUENCE,
        trigger => 1,
        format  => 's',
        doc =>
            "Issue a warning if a macro defined via builtins define or pushdef is matching this regexp. An empty string disables the check. Default: \"$DEFAULT_WARN_MACRO_SEQUENCE\"."
    );

    has _warn_macro_sequence => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => Undef | RegexpRef
    );

    method _trigger_warn_macro_sequence (Str $regexp, @rest --> Undef) {
        if ( length($regexp) <= 0 ) {
            $self->_set__warn_macro_sequence(undef);
            return;
        }
        try {
            $self->_set__warn_macro_sequence(qr/$regexp/s);
        }
        catch {
            $self->logger_error( '%s: %s', $self->impl_quote($regexp), $_ );
            return;
        };

        return;
    }

    sub _build__warn_macro_sequence {
        qr/$DEFAULT_WARN_MACRO_SEQUENCE/;
    }    #__METHOD 60

    # ---------------------------------------------------------------
    # PARSER REQUIRED METHODS
    # ---------------------------------------------------------------

    method parser_isWord (Str $input, PositiveOrZeroInt $pos, PositiveOrZeroInt $maxPos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        pos($input) = $pos;

        my $word_regexp  = $self->_word_regexp;
        my $lastPos      = $pos;
        my $lexemeLength = 0;
        my $lexemeValue;
        my $thisInput = substr( $input, $pos, 1 );
        while ( $lastPos <= $maxPos ) {
            if ( $thisInput =~ $word_regexp ) {
                my $thisLength = $+[0] - $-[0];
                if ( $lexemeLength > 0 && $lexemeLength == $thisLength ) {
                    #
                    # We went one character too far
                    #
                    last;
                }
                $lexemeLength = $thisLength;
                #
                # Take care: Undef->check() is likely to modify %-
                #
                my ( $beg, $end ) = ( $-[1], $+[1] );
                #
                # It is perfectly legal to have an empty string
                # as word "value"
                #
                if ( !Undef->check($beg) && !Undef->check($end) ) {
                    $lexemeValue = substr( $thisInput, $beg, $end - $beg );
                }
                else {
                    $lexemeValue = substr( $thisInput, $-[0], $lexemeLength );
                }
                $thisInput .= substr( $input, ++$lastPos, 1 );
            }
            else {
                last;
            }
        }
        if ( $lexemeLength > 0 ) {
            ${$lexemeLengthRef} = $lexemeLength;
            ${$lexemeValueRef}  = $lexemeValue;
            return true;
        }
        return false;
    }

    method parser_isComment (Str $input, PositiveOrZeroInt $pos, PositiveOrZeroInt $maxPos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {

        #
        # We want to catch EOF in comment. So we do it ourself.
        #
        my $comStart           = $self->_comment_start;
        my $comEnd             = $self->_comment_end;
        my $commentStartLength = $self->_commentStartLength;
        my $commentEndLength   = $self->_commentEndLength;
        if ( $commentStartLength > 0 && $commentEndLength > 0 ) {

            if ( index( $input, $comStart, $pos ) == $pos ) {
                my $lastPos = $pos + $commentStartLength;
                while ( $lastPos <= $maxPos ) {
                    if ( index( $input, $comEnd, $lastPos ) == $lastPos ) {
                        $lastPos += $commentEndLength;
                        ${$lexemeLengthRef} = $lastPos - $pos;
                        ${$lexemeValueRef}
                            = substr( $input, $pos, ${$lexemeLengthRef} );
                        return true;
                    }
                    else {
                        ++$lastPos;
                    }
                }
                #
                # If we are here, it is an error if End-Of-Input is flagged
                #
                if ( $self->_eoi ) {
                    $self->logger_error('EOF in comment');
                    croak('EOF in comment');
                }
            }
        }
        return false;
    }

    method parser_isQuotedstring (Str $input, PositiveOrZeroInt $pos, PositiveOrZeroInt $maxPos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {

        #
        # We cannot rely on a balanced regexp a-la-Regexp::Common
        # because if end-string is a prefix of start-string, it has precedence
        #
        my $quoteStart       = $self->_quote_start;
        my $quoteEnd         = $self->_quote_end;
        my $quoteStartLength = $self->_quoteStartLength;
        my $quoteEndLength   = $self->_quoteEndLength;
        if ( $quoteStartLength > 0 && $quoteEndLength > 0 ) {

            if ( index( $input, $quoteStart, $pos ) == $pos ) {
                my $nested  = 0;
                my $lastPos = $pos + $quoteStartLength;
                while ( $lastPos <= $maxPos ) {
                    if ( index( $input, $quoteEnd, $lastPos ) == $lastPos ) {
                        $lastPos += $quoteEndLength;
                        if ( $nested == 0 ) {
                            ${$lexemeLengthRef} = $lastPos - $pos;
                            ${$lexemeValueRef}  = $self->impl_unquote(
                                substr( $input, $pos, ${$lexemeLengthRef} ) );
                            return true;
                        }
                        else {
                            $nested--;
                        }
                    }
                    elsif (
                        index( $input, $quoteStart, $lastPos ) == $lastPos )
                    {
                        $lastPos += $quoteStartLength;
                        $nested++;
                    }
                    else {
                        ++$lastPos;
                    }
                }
                #
                # If we are here, it is an error if End-Of-Input is flagged
                #
                if ( $self->_eoi ) {
                    $self->logger_error('EOF in string');
                    croak('EOF in string');
                }
            }
        }
        return false;
    }

    method parser_isCharacter (Str $input, PositiveOrZeroInt $pos, PositiveOrZeroInt $maxPos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        pos($input) = $pos;
        if ( $input =~ /\G./s ) {
            ${$lexemeLengthRef} = $+[0] - $-[0];
            ${$lexemeValueRef} = substr( $input, $-[0], ${$lexemeLengthRef} );
            return true;
        }
        return false;
    }

    method _getMacro (Str $word --> M4Macro) {
        return $self->_macros_get($word)->macros_get(-1);
    }

    method parser_isMacro (Str $input, PositiveOrZeroInt $pos, PositiveOrZeroInt $maxPos, Str $wordValue, PositiveInt $wordLength, Ref $macroRef, Ref $lparenPosRef --> Bool) {

        #
        # If a macro with this name exist, we have to check if it is accepted.
        # The condition is if it is recognized only with parameters
        #
        if ( $self->_macros_exists($wordValue) ) {
            my $macro     = $self->_getMacro($wordValue);
            my $lparenPos = $pos + $wordLength;
            my $dummy;
            my $lparen
                = (
                $self->parser_isQuotedstring( $input, $lparenPos, $maxPos,
                    \$dummy, \$dummy )
                    || $self->parser_isComment(
                    $input, $lparenPos, $maxPos, \$dummy, \$dummy
                    )
                ) ? ''
                : ( $lparenPos <= $maxPos ) ? substr( $input, $lparenPos, 1 )
                :                             '';
            if ( $lparen eq '(' || !$macro->macro_needParams ) {
                ${$macroRef} = $macro;
                ${$lparenPosRef} = ( $lparen eq '(' ) ? $lparenPos : -1;
                return true;
            }
        }

        return false,;
    }

    method parser_tokensPriority {
        return $self->_tokens_priority_elements;
    }

    # ---------------------------------------------------------------
    # LOGGER REQUIRED METHODS
    # ---------------------------------------------------------------
    method logger_error (@args --> Undef) { $self->_logger->errorf(@args); return; }

    method logger_warn (@args --> Undef) {
        if ( !$self->silent ) {
            $self->_logger->warnf(@args);
        }
        if ( $self->_fatal_warnings >= 1 ) {
            $self->_set__rc(EXIT_FAILURE);
        }
        if ( $self->_fatal_warnings > 1 ) {
            #
            # Say we do not accept more input
            #
            $self->impl_setEoi;
            $self->logger_error('Warning configured as fatal');
            croak('Warning configured as fatal');
        }
        return;
    }

    method logger_debug (@args --> Undef) {
        $self->_logger->debugf(@args);
        return;
    }

    # ---------------------------------------------------------------
    # PRIVATE ATTRIBUTES
    # ---------------------------------------------------------------
    has _lastSysExitCode => ( is => 'rw', isa => Int, default => 0 );

    has __file__ => ( is => 'rw', isa => Str, default => '' );
    has __line__ => ( is => 'rw', isa => PositiveOrZeroInt, default => 0 );

    # Saying directly $0 failed in taint mode
    has __program__ => ( is => 'rw', isa => Str, default => sub {$0} );

    has _value => (
        is      => 'rwp',
        isa     => Str,
        default => ''
    );

    # ----------------------------------------------------
    # builders
    # ----------------------------------------------------

    method _build_quote_start {$DEFAULT_QUOTE_START}

    method _build__logger_category {'M4'}

    #
    # Diversion 0 is special and maps directly to an internal variable
    #
    method _build__diversions { { 0 => IO::Scalar->new } }

    method _build__lastDiversion { $self->_diversions_get(0) }

    method _build__builtins {
        my %ref = ();
        foreach (
            qw/
            define undefine defn pushdef popdef indir builtin
            ifdef ifelse
            shift
            dumpdef
            traceon traceoff
            debugmode debugfile
            dnl
            changequote changecom changeword
            m4wrap
            include sinclude
            divert undivert divnum
            len index
            regexp substr translit patsubst
            format
            incr decr
            eval
            syscmd esyscmd sysval
            mkstemp maketemp
            errprint
            __file__ __line__ __program__
            /
            )
        {

            if (   $self->_no_gnu_extensions
                && exists( $Default_EXTENSIONS{$_} )
                && $Default_EXTENSIONS{$_} )
            {
                next;
            }
            $ref{$_} = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $_,
                expansion => 'TO BE REPLACED',
                stub      => $self->meta->get_method("builtin_$_")->body
            );
            #
            # The expansion of a builtin is the builtin itself
            #
            $ref{$_}->expansion( $ref{$_} );
            if ( $self->_builtin_need_param_exists($_) ) {
                $ref{$_}->needParams( $self->_builtin_need_param_get($_) );
            }
            if ( $self->_param_can_be_macro_exists($_) ) {
                $ref{$_}
                    ->paramCanBeMacro( $self->_param_can_be_macro_get($_) );
            }
            if ( $_ eq 'dnl' ) {
                $ref{$_}->postMatchLength(
                    sub {
                        my ( $self, $input, $pos, $maxPos ) = @_;
                        pos($input) = $pos;
                        if ( $input =~ /\G.*?\n/s ) {
                            return $+[0] - $-[0];
                        }
                        elsif ( $self->_eoi && $input =~ /\G[^\n]*\z/ ) {
                            $self->logger_warn( '%s: %s',
                                'dnl', 'EOF without a newline' );
                            return $+[0] - $-[0];
                        }
                        else {
                            return 0;
                        }
                    }
                );
            }
        }
        if ( !$self->_no_gnu_extensions ) {
            my $name = '__gnu__';
            $ref{$name} = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $name,
                expansion => "<$_>",
                stub      => sub { return ''; }
            );
        }
        if ( is_os_type('Windows') ) {
            #
            # A priori I assume this is reliable
            #
            my $name;
            if ( $^O eq 'os2' ) {
                $name = $self->_no_gnu_extensions ? 'os2' : '__os2__';
            }
            else {
                $name = $self->_no_gnu_extensions ? 'windows' : '__windows__';
            }
            $ref{$name} = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $name,
                expansion => "<$_>",
                stub      => sub { return ''; }
            );
        }
        if ( is_os_type('Unix') ) {
            my $name = $self->_no_gnu_extensions ? 'unix' : '__unix__';
            $ref{$name} = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $name,
                expansion => "<$_>",
                stub      => sub { return ''; }
            );
        }

        return \%ref;
    }

    method _build__macros {
        my %ref = ();
        foreach ( $self->_builtins_keys ) {
            my $macros = MarpaX::Languages::M4::Impl::Macros->new();
            $macros->macros_push( $self->_builtins_get($_) );
            $ref{ $self->_prefix_builtins . $_ } = $macros;
        }
        return \%ref;
    }

    # ----------------------------------------------------
    # Triggers
    # ----------------------------------------------------
    method _trigger__eof (Bool $eof, @rest --> Undef) {
        if ($eof) {
            #
            # First, m4wrap stuff is rescanned
            #
            while ( $self->_m4wrap_count > 0 ) {
                my @m4wrap = $self->_m4wrap_elements;
                $self->_set___m4wrap( [] );
                $self->impl_parseIncremental( join( '', @m4wrap ) );
            }
            #
            # Then, diverted thingies, that are not rescanned
            # We make sure current diversion is number 0
            $self->builtin_divert();
            $self->builtin_undivert();
        }
        return;
    }

    # ----------------------------------------------------
    # Internal attributes
    # ----------------------------------------------------
    has _rc => (
        is      => 'rwp',
        isa     => Int,
        default => EXIT_SUCCESS,
    );

    has _builtins => (
        is          => 'lazy',
        isa         => HashRef [M4Macro],
        handles_via => 'Hash',
        handles     => {
            _builtins_set    => 'set',
            _builtins_get    => 'get',
            _builtins_exists => 'exists',
            _builtins_keys   => 'keys',
            _builtins_delete => 'delete'
        }
    );

    has _macros => (
        is  => 'lazy',
        isa => HashRef [ InstanceOf ['MarpaX::Languages::M4::Impl::Macros'] ],
        handles_via => 'Hash',
        handles     => {
            _macros_set    => 'set',
            _macros_get    => 'get',
            _macros_exists => 'exists',
            _macros_keys   => 'keys',
            _macros_delete => 'delete'
        }
    );

    has __m4wrap => (
        is          => 'rwp',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        handles_via => 'Array',
        handles     => {
            _m4wrap_push     => 'push',
            _m4wrap_unshift  => 'unshift',
            _m4wrap_elements => 'elements',
            _m4wrap_count    => 'count',
        }
    );

    has _eof => (
        is      => 'rw',
        isa     => Bool,
        trigger => 1,
        default => false
    );

    has _eoi => (
        is      => 'rwp',
        isa     => Bool,
        default => false
    );

    has impl_unparsed => (
        is      => 'rwp',
        isa     => Str,
        default => ''
    );

    has _diversions => (
        is          => 'lazy',
        isa         => HashRef [ ConsumerOf ['IO::Handle'] ],
        handles_via => 'Hash',
        handles     => {
            _diversions_set    => 'set',
            _diversions_get    => 'get',
            _diversions_exists => 'exists',
            _diversions_keys   => 'keys',
            _diversions_delete => 'delete'
        }
    );

    has _lastDiversion => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => ConsumerOf ['IO::Handle']
    );
    has _lastDiversionNumbers => (
        is          => 'rwp',
        isa         => ArrayRef [Int],
        default     => sub { [0] },
        handles_via => 'Array',
        handles     => {
            _lastDiversionNumbers_push        => 'push',
            _lastDiversionNumbers_first_index => 'first_index',
            _lastDiversionNumbers_get         => 'get',
            _lastDiversionNumbers_splice      => 'splice'
        }
    );

    method impl_quote (Str $string --> Str) {
        if ( $self->_quoteStartLength > 0 && $self->_quoteEndLength > 0 ) {
            return $self->_quote_start . $string . $self->_quote_end;
        }
        else {
            return $string;
        }
    }

    method impl_unquote (Str $string --> Str) {
        if ( $self->_quoteStartLength > 0 && $self->_quoteEndLength > 0 ) {
            substr( $string, 0, $self->_quoteStartLength, '' );
            my $quoteEndLength = $self->_quoteEndLength;
            substr( $string, -$quoteEndLength, $quoteEndLength, '' );
        }
        return $string;
    }

    method _checkIgnored (Str $name, @ignored --> Undef) {
        if (@ignored) {
            $self->logger_warn( 'excess arguments to builtin %s ignored',
                $self->impl_quote($name) );
        }
        return;
    }

    method builtin_define (Undef|Str|M4Macro $name?, Undef|Str|M4Macro $defn?, @ignored --> Str) {
        if ( Undef->check($name) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('define')
            );
            return '';
        }
        $defn //= '';

        $self->_checkIgnored( 'define', @ignored );

        if ( M4Macro->check($name) ) {
            $self->logger_warn(
                '%s: invalid macro name ignored',
                $self->impl_quote('define')
            );
            return '';
        }

        my $macro;
        if ( Str->check($defn) ) {
            #
            # Make a M4Macro out of $defn
            #
            $macro = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $name,
                stub      => $self->_expansion2CodeRef( $name, $defn ),
                expansion => $defn
            );
        }
        else {
            $macro = $defn;
        }
        if ( !$self->_macros_exists($name) ) {
            my $macros = MarpaX::Languages::M4::Impl::Macros->new();
            $macros->macros_push($macro);
            $self->_macros_set( $name, $macros );
        }
        else {
            $self->_macros_get($name)->macros_set( -1, $macro );
        }
        return '';
    }

    method builtin_undefine (Str @names --> Str) {
        $self->_macros_delete(@names);
        return '';
    }

    #
    # defn can only concatenate text macros
    #
    method builtin_defn (Str @names --> Str|M4Macro) {
        my @rc      = ();
        my $nbMacro = 0;
        my $nbStr   = 0;
        foreach (@names) {
            if ( $self->_macros_exists($_) ) {
                push( @rc, $self->_getMacro($_)->macro_expansion );
            }
        }
        my $rc = '';
        foreach ( 0 .. $#rc ) {
            if ( M4Macro->check( $rc[$_] ) ) {
                if ( $rc[$_]->macro_isBuiltin ) {
                    if (   ( $_ == 0 && $#rc > 0 )
                        || ( $_ > 0 ) )
                    {
                        $self->logger_warn(
                            '%s: cannot concatenate builtin %s',
                            'defn',
                            $self->impl_quote( $rc[$_]->macro_name ) );
                    }
                    else {
           #
           # Per def this is ok only if @rc has one element, that is a builtin
           #
                        $rc = $rc[$_];
                    }
                }
                else {
                    $rc .= $self->impl_quote( $rc[$_]->macro_expansion );
                }
            }
            else {
                $rc .= $self->impl_quote( $rc[$_] );
            }
        }
        return $rc;
    }

    method builtin_pushdef (Undef|Str $name?, Undef|Str|M4Macro $defn?, @ignored --> Str) {
        if ( Undef->check($name) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('pushdef')
            );
            return '';
        }

        my $macro;
        $defn //= '';

        $self->_checkIgnored( 'pushdef', @ignored );

        if ( Str->check($defn) ) {
            #
            # Make a M4Macro out of $defn
            #
            $macro = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $name,
                stub      => $self->_expansion2CodeRef( $name, $defn ),
                expansion => $defn
            );
        }
        else {
            $macro = $defn;
        }
        if ( !$self->_macros_exists($name) ) {
            my $macros = MarpaX::Languages::M4::Impl::Macros->new();
            $macros->macros_push($macro);
            $self->_macros_set( $name, $macros );
        }
        else {
            $self->_macros_get($name)->macros_push($macro);
        }
        return '';
    }

    method builtin_popdef (Str @names --> Str) {

        foreach (@names) {
            if ( $self->_macros_exists($_) ) {
                $self->_macros_get($_)->macros_pop();
                if ( $self->_macros_get($_)->macros_isEmpty ) {
                    $self->_macros_delete($_);
                }
            }
        }
        return '';
    }

    method builtin_indir (Undef|Str|M4Macro $name, @args --> Str|M4Macro) {
        if ( Undef->check($name) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('indir')
            );
            return '';
        }
        #
        # If $name is a builtin, check the other arguments
        #
        if ( M4Macro->check($name) ) {
            $self->logger_warn(
                'indir: invalid macro name ignored',
                $self->impl_quote( $name->macro_name )
            );
            return '';
        }
        if ( $self->_macros_exists($name) ) {
            my $macro = $self->_getMacro($name);
            #
            # Check the args
            #
            foreach ( 0 .. $#args ) {
                if ( M4Macro->check( $args[$_] )
                    && !$macro->macro_paramCanBeMacro($_) )
                {
                    #
                    # Macro not authorized: flattened to the empty string
                    #
                    $args[$_] = '';
                }
            }
            return $macro->macro_execute( $self, @args );
        }
        else {
            $self->logger_error( 'indir: undefined macro %s',
                $self->impl_quote($name) );
            return '';
        }
    }

    method builtin_builtin (Undef|Str|M4Macro $name?, @args --> Str|M4Macro) {
        if ( Undef->check($name) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('builtin')
            );
            return '';
        }
        if ( M4Macro->check($name) ) {
            #
            # Not supported
            #
            $self->logger_error(
                '%s: invalid macro name ignored',
                $self->impl_quote('builtin')
            );
            return '';
        }
        if ( $self->_builtins_exists($name) ) {
            #
            # We do not check the args to eventually flatten them. Thus this
            # can croak
            #
            my $rc = '';
            try {
                $rc = $self->_builtins_get($name)
                    ->macro_execute( $self, @args );
            }
            catch {
                $self->logger_error( '%s', $_ );
            };
            return $rc;
        }
        else {
            $self->logger_error( 'builtin: undefined builtin %s',
                $self->impl_quote($name) );
            return '';
        }
    }

    method builtin_ifdef (Undef|Str $name?, Undef|Str $string1?, Undef|Str $string2?, @ignored --> Str) {
        if ( Undef->check($name) || Undef->check($string1) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('ifdef')
            );
            return '';
        }

        $self->_checkIgnored( 'ifdef', @ignored );

        if ( $self->_macros_exists($name) ) {
            return $string1;
        }
        else {
            return $string2 // '';
        }
    }

    method builtin_ifelse (@args --> Str) {
        while (@args) {
            if ( scalar(@args) <= 1 ) {
                return '';
            }
            elsif ( scalar(@args) == 2 ) {
                $self->logger_error(
                    'too few arguments to builtin %s',
                    $self->impl_quote('ifelse')
                );
                return '';
            }
            elsif ( scalar(@args) >= 3 && scalar(@args) <= 5 ) {
                my ( $string1, $string2, $equal, $notEqual, $ignored )
                    = @args;
                $string1  //= '';
                $string2  //= '';
                $equal    //= '';
                $notEqual //= '';
                if ( !Undef->check($ignored) ) {
                    $self->logger_warn(
                        'excess arguments to builtin %s ignored',
                        $self->impl_quote('ifelse') );
                }
                return ( $string1 eq $string2 ) ? $equal : $notEqual;
            }
            else {
                my ( $string1, $string2, $equal, @rest ) = @args;
                $string1 //= '';
                $string2 //= '';
                $equal   //= '';
                if ( $string1 eq $string2 ) {
                    return $equal;
                }
                @args = @rest;
            }
        }
    }

    method builtin_shift (@args --> Str) {
        shift(@args);

        if (@args) {
            return join( ',', map { $self->impl_quote($_) } @args );
        }
        else {
            return '';
        }
    }

    method builtin_dumpdef (@args --> Str) {

        if ( !@args ) {
            @args = $self->_macros_keys;
        }

        foreach ( sort @args ) {
            if ( !$self->_macros_exists($_) ) {
                $self->logger_warn( 'dumpdef: undefined macro %s',
                    $self->impl_quote($_) );
            }
            else {
                $self->logger_debug(
                    '%s: %s',
                    $_,
                    $self->_getMacro($_)->macro_isBuiltin
                    ? "<$_>"
                    : $self->_getMacro($_)->macro_expansion
                );
            }
        }

        return '';
    }

    method builtin_traceon (@names --> Str) {
        foreach (@names) {
            $self->_trace_set( $_, true );
        }
        return '';
    }

    method builtin_traceoff (@names --> Str) {
        foreach (@names) {
            $self->_trace_set( $_, false );
        }
        return '';
    }

    method builtin_debugmode (Undef|Str $flags?, @ignored --> Str) {
        if ( Str->check($flags) && length($flags) <= 0 ) {
            $flags = 'aeq';
        }
        if ( Undef->check($flags) ) {
            $flags = '';
        }

        $self->_checkIgnored( 'debugmode', @ignored );
        $self->debugmode($flags);
        return '';
    }

    method builtin_debugfile (Undef|Str $file?, @ignored --> Str) {

        $self->_checkIgnored( 'debugfile', @ignored );
        $self->_set_debugfile($file);
        return '';
    }

    method builtin_dnl (@ignored --> Str) {
        $self->_checkIgnored( 'dnl', @ignored );
        return '';
    }

    method builtin_changequote (Undef|Str $start?, Undef|Str $end?, @ignored --> Str) {
        if ( Undef->check($start) && Undef->check($end) ) {
            $start = $DEFAULT_QUOTE_START;
            $end   = $DEFAULT_QUOTE_END;
        }

        $self->_checkIgnored( 'changequote', @ignored );

        $start //= '';
        if ( length($start) <= 0 ) {
            $end = '';
        }
        else {
            $end ||= $DEFAULT_QUOTE_END;
        }

        $self->_set__quote_start($start);
        $self->_set__quote_end($end);

        return '';
    }

    method builtin_changecom (Undef|Str $start?, Undef|Str $end?, @ignored --> Str) {
        if ( Undef->check($start) && Undef->check($end) ) {
            $start = '';
            $end   = '';
        }

        $self->_checkIgnored( 'changecom', @ignored );

        $start //= '';
        if ( length($start) <= 0 ) {
            $end = '';
        }
        else {
            $end ||= $DEFAULT_COMMENT_END;
        }

        $self->_set__comment_start($start);
        $self->_set__comment_end($end);

        return '';
    }

    method builtin_changeword (Undef|Str $string?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('changeword')
            );
            return '';
        }
        $self->_checkIgnored( 'changeword', @ignored );

        $self->word_regexp( $string || $DEFAULT_WORD_REGEXP );

        return '';
    }

    method builtin_m4wrap (@args --> Str) {

        my $text = join( ' ', grep { !Undef->check($_) } @args );
        if ( $self->_m4wrap_order eq 'LIFO' ) {
            $self->_m4wrap_unshift($text);
        }
        else {
            $self->_m4wrap_push($text);
        }

        return '';
    }

    method _includeFile (Bool $silent, Str $file? --> Str) {

        if ( length($file) <= 0 ) {
            if ( !$silent ) {
                #
                # Fake a ENOENT
                #
                if ( exists &Errno::ENOENT ) {
                    $! = &Errno::ENOENT;
                    $self->logger_error( 'cannot open %s: %s',
                        $self->impl_quote($file), $! );
                }
                else {
                    $self->logger_error( 'cannot open %s',
                        $self->impl_quote($file) );
                }
            }
            return '';
        }
        my @paths = ();

        my @includes = (
            reverse( $self->_prepend_include_elements ),
            File::Spec->curdir(), reverse( $self->_include_elements ),
            M4PATH->List
        );

        my @candidates;
        if ( File::Spec->file_name_is_absolute($file) ) {
            @candidates = ($file);
        }
        else {
            use filetest 'access';
            @candidates = grep { -r $_ }
                map { File::Spec->catfile( $_, $file ) } @includes;
        }

        if ( !@candidates ) {
            #
            # It is guaranteed that #includes have at least one element.
            # Therefore, $! should be setted
            #
            if ( !$silent ) {
                $self->logger_error( 'cannot open %s: %s',
                    $self->impl_quote($file), $! );
            }
            return '';
        }

        foreach my $file (@candidates) {
            my $fh;
            try {
                $fh = IO::File->new(
                    $ENV{M4_ENCODE_LOCALE}
                    ? encode( locale_fs => $file )
                    : $file,
                    'r'
                    )
                    || die "$file: $!";
                if ( $ENV{M4_ENCODE_LOCALE} ) {
                    binmode( $fh, ':encoding(locale)' );
                }
            }
            catch {
                if ( !$silent ) {
                    $self->logger_error( '%s: %s', $file, $_ );
                }
                return;
            };
            if ( !Undef->check($fh) ) {
                my $content;
                try {
                    $content = do { local $/; <$fh>; };
                    $fh->close;
                }
                catch {
                    if ( !$silent ) {
                        $self->logger_warn( '%s: %s', $file, $_ );
                    }
                    return;
                };
                if ( !Undef->check($content) ) {
                    if ( $self->_inctounix ) {
                        $content =~ s/\R/\n/g;
                    }
                    return $content;
                }
            }
        }

        return '';
    }

    method builtin_include (Undef|Str $file, @ignored --> Str) {
        if ( Undef->check($file) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('include')
            );
            return '';
        }
        $self->_checkIgnored( 'include', @ignored );

        return $self->_includeFile( false, $file );
    }

    method builtin_sinclude (Undef|Str $file, @ignored --> Str) {
        if ( Undef->check($file) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('sinclude')
            );
            return '';
        }
        $self->_checkIgnored( 'sinclude', @ignored );

        return $self->_includeFile( true, $file );
    }

    method _apply_diversion (Int $number, ConsumerOf ['IO::Handle'] $fh --> Undef) {
        my $index
            = $self->_lastDiversionNumbers_first_index( sub { $_ == $number }
            );
        if ( $index >= 0 ) {
            $self->_lastDiversionNumbers_splice( $index, 1 );
        }
        $self->_lastDiversionNumbers_push($number);
        if ( !$self->_diversions_exists($number) ) {
            $self->_diversions_set( $number, $fh );
        }
        $fh->autoflush(1);
        $self->_set__lastDiversion($fh);

        return;
    }

    method _remove_diversion (Int $number --> Undef) {
        my $index
            = $self->_lastDiversionNumbers_first_index( sub { $_ == $number }
            );
        if ( $index >= 0 ) {
            $self->_lastDiversionNumbers_splice( $index, 1 );
            $self->_diversions_delete($number);
        }
        else {
            #
            # This should not happen
            #
            $self->logger_error(
                '%s: cannot find internal diversion number %d',
                'divert', $number );
        }
        #
        # We don't know the $fh of previous diversion,
        # it is stored in diversions hash.
        #
        $self->_set__lastDiversion(
            $self->_diversions_get( $self->builtin_divnum ) );
        return;
    }

    method builtin_divert (Undef|Str $number?, @ignored --> Str) {
        $self->_checkIgnored( 'divert', @ignored );

        $number //= 0;
        if ( length("$number") <= 0 ) {
            $self->logger_warn( 'empty string treated as 0 in builtin %s',
                $self->impl_quote('divert') );
            $number = 0;
        }
        if ( !Int->check($number) ) {
            $self->logger_error( '%s: %s: does not look like an integer',
                'divert', $number );
            return '';
        }

        my $fh;
        if ( $number == 0 ) {
            #
            # Diversion number 0 is a noop and always goes to STDOUT.
            # We will just make sure this is current diversion number.
            # Per def this diversion always exist.
            #
            $fh = $self->_diversions_get($number);
        }
        else {
            if ( !$self->_diversions_exists($number) ) {
                #
                # Create diversion
                #
                try {
                    if ( $self->_divert_type eq 'memory' ) {
                        $fh = IO::Scalar->new;
                    }
                    else {
                        $fh = File::Temp->new;
                        #
                        # We do not want to be exposed to any wide-character
                        # warning
                        #
                        binmode($fh);
                    }
                }
                catch {
                    $self->logger_error($_);
                    return;
                };
                if ( Undef->check($fh) ) {
                    return '';
                }
            }
            else {
                #
                # Get diversion $fh
                #
                $fh = $self->_diversions_get($number);
            }
        }
        #
        # Make sure latest diversion number is $number
        #
        $self->_apply_diversion( $number, $fh );
        return '';
    }

    method _diversions_sortedKeys {
        return sort { $a <=> $b } $self->_diversions_keys;
    }

    method builtin_undivert (Str @diversions --> Str) {

        #
        # Undiverting the empty string is the same as specifying diversion 0
        #
        foreach ( 0 .. $#diversions ) {
            if ( length( $diversions[$_] ) <= 0 ) {
                $diversions[$_] = '0';
            }
        }

        if ( !@diversions ) {
            @diversions = $self->_diversions_sortedKeys;
        }

        foreach (@diversions) {
            my $number = $_;
            if ( Int->check($number) ) {
                #
                # Undiverting the current diversion, or number 0,
                # or a unknown diversion is silently ignored.
                #
                if (   $number == $self->builtin_divnum
                    || $number == 0
                    || !$self->_diversions_exists($number) )
                {
                    next;
                }
                #
                # Only positive numbers are merged
                #
                if ( $number > 0 ) {
                    #
                    # This is per-def a IO::Handle consumer
                    #
                    my $fh = $self->_diversions_get($number);
                    #
                    # Get its size
                    #
                    $fh->seek( 0, SEEK_END );
                    my $size = $fh->tell;
                    #
                    # Go to the beginning
                    #
                    $fh->seek( 0, SEEK_SET );
                    #
                    # Read it
                    #
                    my $content = '';
                    $fh->read( $content, $size );
                    #
                    # Now we can really remove this diversion
                    #
                    $self->_remove_diversion($number);
                    #
                    # And append to the now-current diversion
                    #
                    $self->impl_appendValue($content);
                }
                else {
                    $self->_remove_diversion($number);
                }
            }
            else {
                #
                # Treated as name of a file
                #
                $self->impl_appendValue( $self->builtin_include($number) );
            }
        }

        return '';
    }

    method builtin_divnum (@ignored --> Str) {
        $self->_checkIgnored( 'divnum', @ignored );

        return $self->_lastDiversionNumbers_get(-1);
    }

    method builtin_len (Undef|Str $string?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->impl_quote('len') );
            return '';
        }
        $self->_checkIgnored( 'len', @ignored );

        $string //= '';
        return length($string);
    }

    method builtin_index (Undef|Str $string?, Undef|Str $substring?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('index')
            );
            return '';
        }
        if ( Undef->check($substring) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('index')
            );
            return 0;
        }
        $self->_checkIgnored( 'index', @ignored );

        if ( Undef->check($substring) ) {
            $self->logger_warn( '%s: undefined string to search for',
                'index', $_ );
            $substring = '';
        }
        return index( $string, $substring );
    }

    method _regexpDollarOneToIndice (Str $dollarOne --> PositiveOrZeroInt) {
        $dollarOne =~ s/^\\\{//;
        #
        # Note: int('&') will return 0
        #
        return int($dollarOne);
    }

    method _regexpIndiceToReplacement (PositiveOrZeroInt $indice, HashRef $wantedIndicesRef) {
        $wantedIndicesRef->{$indice}++;
        return "\$match\[$indice\]";
    }

    method builtin_regexp (Undef|Str $string?, Undef|Str $regexpString?, Undef|Str $replacement?, @ignored --> Str) {
        if ( Undef->check($string) || Undef->check($regexpString) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('regexp')
            );
            return '0';
        }

        my $regexp = eval "qr/$regexpString/sm";
        if ($@) {
            $self->logger_error( '%s: %s', $self->impl_quote('regexp'), $@ );
            return '';
        }

        $self->_checkIgnored( 'regexp', @ignored );

        if ( Undef->check($replacement) ) {
            #
            # Expands to the index of first match in string
            #
            my $index = ( $string =~ $regexp ) ? $-[0] : -1;
            return "$index";
        }
        else {
            #
            # Sanitize
            #
            my $safeReplacement = quotemeta($replacement);
            #
            # We allow only:
            # * $&                       quotemeta: \$\&
            # * $digits                  quotemeta: \$digits
            # * ${&}                     quotemeta: \$\{\&\}
            # * ${digits}                quotemeta: \$\{digits\}
            #
            # We want to warn about unexpanded thingies so, as in
            # _expansion2CodeRef we will count expected
            # replacements.
            #
            my $maxRegexpIndice    = -1;
            my %wantedRegexpIndice = ();
            $safeReplacement = $self->_allowBlockInSanitizedString(
                $safeReplacement,
                qr/\\\$((?:\\\&|\\\{\\\&\\\})|(?:[0-9]+|\\\{[0-9]+\\\}))/,
                \&_regexpDollarOneToIndice,
                \&_regexpIndiceToReplacement,
                \%wantedRegexpIndice,
                \$maxRegexpIndice
            );
            if ( $string =~ $regexp ) {
                my @match = ();
                foreach ( 0 .. $maxRegexpIndice ) {
                    if ( $_ <= $#+ ) {
                        $match[$_]
                            = substr( $string, $-[$_], $+[$_] - $-[$_] );
                    }
                    else {
                        $self->logger_warn(
                            '%s: sub-expression number %d not present',
                            $self->impl_quote('regexp'), $_ );
                        $match[$_] = '';
                    }
                }
                my $rc = eval "\"$safeReplacement\"";
                if ($@) {
                 #
                 # Should not happen, we have sanitized the replacement string
                 #
                    $self->logger_error( '%s: Internal error %s',
                        $self->impl_quote('regexp'), $@ );
                    return '';
                }
                else {
                    return $rc;
                }
            }
            else {
                return '';
            }
        }
    }

    method builtin_substr (Undef|Str $string?, Undef|Str $from?, Undef|Str $length?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('substr')
            );
            return '';
        }
        if ( Undef->check($from) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('substr')
            );
            return $string;
        }
        $self->_checkIgnored( 'substr', @ignored );

        if ( length($from) <= 0 ) {
            $self->logger_warn( '%s: empty string treated as zero',
                'substr' );
            $from = 0;
        }

        if ( !PositiveOrZeroInt->check($from) ) {
            $self->logger_error(
                '%s: %s: does not look like a positive or zero integer',
                'substr', $from );
            return '';
        }
        if ( Str->check($length) ) {
            if ( !Int->check($length) ) {
                $self->logger_error( '%s: %s: does not look like an integer',
                    'substr', $length );
                return '';
            }
        }

        return ( !Undef->check($length) )
            ? substr( $string, $from, $length )
            : substr( $string, $from );
    }

    method _expandRanges (Str $range --> Str) {
        my $rc = '';
        my @chars = split( //, $range );
        for (
            my $from = undef, my $i = 0;
            $i <= $#chars;
            $from = ord( $chars[ $i++ ] )
            )
        {
            my $s = $chars[$i];
            if ( $s eq '-' && defined($from) ) {
                my $to = ( ++$i <= $#chars ) ? ord( $chars[$i] ) : undef;
                if ( !defined($to) ) {
                    #
                    # Trailing dash
                    #
                    $rc .= '-';
                    last;
                }
                elsif ( $from <= $to ) {
                    while ( $from++ < $to ) {
                        $rc .= chr($from);
                    }
                }
                else {
                    while ( --$from >= $to ) {
                        $rc .= chr($from);
                    }
                }
            }
            else {
                $rc .= $chars[$i];
            }
        }
        return $rc;
    }

    method builtin_translit (Undef|Str $string?, Undef|Str $from?, Undef|Str $to?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('translit')
            );
            return '';
        }
        if ( Undef->check($from) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('translit')
            );
            return $string;
        }
        $self->_checkIgnored( 'translit', @ignored );

        my $fromLength = length($from);
        if ( $fromLength <= 0 ) {
            return '';
        }

        #
        # We duplicate the algorithm of GNU m4: translit
        # is part of M4 official spec, so we cannot use
        # perl's tr, which is not stricly equivalent.
        # De-facto, we will get GNU behaviour.
        #
        $to //= '';
        if ( index( $to, '-' ) >= 0 ) {
            $to = $self->_expandRanges($to);
        }
        #
        # In case of small $from, let's go to the range algorithm
        # anyway.
        # GNU m4 implementation is correct doing direct
        # transformation if there is only one or two bytes.
        # Well, for us, I'd say one of two characters.

        if ( index( $from, '-' ) >= 0 ) {
            $from = $self->_expandRanges($from);
        }

        my %map         = ();
        my $toMaxIndice = length($to) - 1;
        my $ito         = 0;
        foreach ( split( //, $from ) ) {
            if ( !exists( $map{$_} ) ) {
                if ( $ito <= $toMaxIndice ) {
                    $map{$_} = substr( $to, $ito, 1 );
                }
                else {
                    $map{$_} = '';
                }
            }
            if ( $ito <= $toMaxIndice ) {
                $ito++;
            }
        }

        my $rc = '';
        foreach ( split( //, $string ) ) {
            if ( exists( $map{$_} ) ) {
                $rc .= $map{$_};
            }
            else {
                $rc .= $_;
            }
        }

        return $rc;
    }

    #
    # Almost same thing as regexp but with a /g modifier
    #
    method builtin_patsubst (Undef|Str $string?, Undef|Str $regexpString?, Undef|Str $replacement?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('patsubst')
            );
            return '';
        }

        if ( Undef->check($regexpString) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('patsubst')
            );
            return $string;
        }

        my $regexp = eval "qr/$regexpString/sm";
        if ($@) {
            $self->logger_error( '%s: %s', $self->impl_quote('patsubst'),
                $@ );
            return '';
        }

        $self->_checkIgnored( 'patsubst', @ignored );

        #
        # If not supplied, default replacement is deletion
        $replacement //= '';

        #
        # This is quite the same as builtin_regexp
        #
        #
        # Sanitize
        #
        my $safeReplacement = quotemeta($replacement);
        #
        # We allow only:
        # * $&                       quotemeta: \$\&
        # * $digits                  quotemeta: \$digits
        # * ${&}                     quotemeta: \$\{\&\}
        # * ${digits}                quotemeta: \$\{digits\}
        #
        # We want to warn about unexpanded thingies so, as in
        # _expansion2CodeRef we will count expected
        # replacements.
        #
        my $maxRegexpIndice    = -1;
        my %wantedRegexpIndice = ();
        $safeReplacement = $self->_allowBlockInSanitizedString(
            $safeReplacement,
            qr/\\\$((?:\\\&|\\\{\\\&\\\})|(?:[0-9]+|\\\{[0-9]+\\\}))/,
            \&_regexpDollarOneToIndice,
            \&_regexpIndiceToReplacement,
            \%wantedRegexpIndice,
            \$maxRegexpIndice
        );
        $string =~ s/$regexp/
          {
           my @match = ();
           foreach (0..$maxRegexpIndice) {
             if ($_ <= $#+) {
               $match[$_] = substr($string, $-[$_], $+[$_] - $-[$_]);
             } else {
               $self->logger_warn( '%s: sub-expression number %d not present',
                                   $self->impl_quote('patsubst'), $_ );
               $match[$_] = '';
             }
           }
           my $rc = eval "\"$safeReplacement\"";
           if ($@) {
             #
             # Should not happen, we have sanitized the replacement string
             #
             $self->logger_error( '%s: Internal error %s',
                                  $self->impl_quote('patsubst'), $@ );
             '';
           } else {
             $rc;
           }
          }/eg;
        return $string;
    }

    method builtin_format (Undef|Str $format?, Str @arguments --> Str) {
        if ( Undef->check($format) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('format')
            );
            return '';
        }
        my $rc = '';
        try {
            $rc = sprintf( $format, @arguments );
        }
        catch {
            $self->logger_error( 'format: %s', $_ );
            return;
        };
        return $rc;
    }

    method builtin_incr (Undef|Str $number?, Str @ignored --> Str) {
        $self->_checkIgnored( 'incr', @ignored );
        $number //= '';
        if ( length($number) <= 0 ) {
            $self->logger_error( 'empty string treated as 0 in builtin %s',
                $self->impl_quote('incr') );
            $number = 0;
        }
        if ( !Int->check($number) ) {
            $self->logger_error(
                '%s: %s: does not look like an integer',
                $self->impl_quote('incr'),
                $self->impl_quote($number)
            );
            return '';
        }
        my $rc = '';
        if ( $self->_integer_type eq 'native' ) {
            use integer;
            $rc = $number + 1;
        }
        else {
            try {
                my $v1
                    = Bit::Vector->new_Dec( $self->_integer_bits, $number );
                my $v2 = Bit::Vector->new( $self->_integer_bits );
                $v2->inc($v1);
                $rc = $v2->to_Dec();
            }
            catch {
                $self->logger_warn( '%s: %s', $self->impl_quote('incr'), $! );
                return;
            }
        }
        return $rc;
    }

    method builtin_decr (Undef|Str $number?, Str @ignored --> Str) {
        $self->_checkIgnored( 'decr', @ignored );
        $number //= '';
        if ( length($number) <= 0 ) {
            $self->logger_error( 'empty string treated as 0 in builtin %s',
                $self->impl_quote('decr') );
            $number = 0;
        }
        if ( !Int->check($number) ) {
            $self->logger_error(
                '%s: %s: does not look like an integer',
                $self->impl_quote('decr'),
                $self->impl_quote($number)
            );
            return '';
        }
        my $rc = '';
        if ( $self->_integer_type eq 'native' ) {
            use integer;
            $rc = $number - 1;
        }
        else {
            try {
                my $v1
                    = Bit::Vector->new_Dec( $self->_integer_bits, $number );
                my $v2 = Bit::Vector->new( $self->_integer_bits );
                $v2->dec($v1);
                $rc = $v2->to_Dec();
            }
            catch {
                $self->logger_warn( '%s: %s', $self->impl_quote('decr'), $! );
                return;
            }
        }
        return $rc;
    }

    method builtin_eval (Undef|Str $expression?, Undef|Str $radix?, Undef|Str $width?, Str @ignored --> Str) {
        if ( Undef->check($expression) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote('decr')
            );
            return '';
        }
        $self->_checkIgnored( 'eval', @ignored );

        if ( Undef->check($expression) ) {
            $self->logger_error( '%s: empty string treated  as zero',
                $self->impl_quote('eval') );
            return 0;
        }
        #
        # Validate radix
        #
        if ( Undef->check($radix) || length($radix) <= 0 ) {
            $radix = 10;
        }
        if ( !PositiveInt->check($radix) ) {
            $self->logger_error(
                '%s: %s: does not look like a positive integer',
                $self->impl_quote('eval'),
                $self->impl_quote($radix)
            );
            return '';
        }
        if ( $radix < 1 || $radix > 36 ) {
            $self->logger_error(
                '%s: %s: should be in the range [1..36]',
                $self->impl_quote('eval'),
                $self->impl_quote($radix)
            );
            return '';
        }
        #
        # Validate width
        #
        if ( Undef->check($width) || length($width) <= 0 ) {
            $width = 1;
        }
        if ( !PositiveOrZeroInt->check($width) ) {
            $self->logger_error(
                '%s: %s: width does not look like a positive or zero integer',
                $self->impl_quote('eval'), $self->impl_quote($width)
            );
            return '';
        }
        #
        # Check expression
        #
        if ( length($expression) <= 0 ) {
            $self->logger_error( '%s: empty string treated as zero',
                $self->impl_quote('eval') );
            $expression = 0;
        }
        #
        # Eval
        #
        my $rc = '';
        try {
            local $MarpaX::Languages::M4::Impl::Default::INTEGER_BITS
                = $self->_integer_bits;
            local $MarpaX::Languages::M4::Impl::Default::SELF = $self;
            my $valuep = $EVAL_G->parse(
                \$expression,
                {   semantics_package =>
                        'MarpaX::Languages::M4::Impl::Default::Eval',
                }
            );
            $rc = MarpaX::Languages::M4::Impl::Default::BaseConversion
                ->to_base( $radix, ${$valuep}, $width );
        }
        catch {
            #
            # Marpa::R2::Context::bail() is adding
            # something like e.g.:
            # User bailed at line 37 in file "xxx"
            # we strip this line if any
            #
            $_ =~ s/^User bailed.*?\n//;
            $self->logger_error( '%s: %s', $self->impl_quote('eval'), $_ );
            return;
        };

        return $rc;
    }

    method _syscmd (Str $macroName, Bool $appendValue, Undef|Str $command?, Str @ignored --> Str) {
        if ( Undef->check($command) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote($macroName)
            );
            return '';
        }
        $self->_checkIgnored( $macroName, @ignored );

        $command //= '';
        if ( length($command) > 0 ) {
            my ( $stdout, $stderr, $success, $exitCode );
            my $executed = false;
            try {
                ( $stdout, $stderr, $success, $exitCode )
                    = capture_exec($command);
            }
            catch {
                $self->logger_error( '%s: %s',
                    $self->impl_quote($macroName), $_ );
            }
            finally {
                if ( !$@ ) {
                    $executed = true;
                }
            };
            if ($executed) {
                $self->_lastSysExitCode( $exitCode >> 8 );
                if ( $self->_cmdtounix ) {
                    $stderr =~ s/\R/\n/g;
                    $stdout =~ s/\R/\n/g;
                }
                if ( length($stderr) > 0 ) {
                    $self->logger_error( '%s', $stderr );
                }
                if ($appendValue) {
                    $self->impl_appendValue($stdout);
                    return '';
                }
                else {
                    return $stdout;
                }
            }
        }
        return '';
    }

    method builtin_syscmd (Undef|Str $command?, Str @ignored --> Str) {
        return $self->_syscmd( 'syscmd', true, $command, @ignored );
    }

    method builtin_esyscmd (Undef|Str $command?, Str @ignored --> Str) {
        return $self->_syscmd( 'esyscmd', false, $command, @ignored );
    }

    method builtin_sysval (Str @ignored --> Str) {
        $self->_checkIgnored( 'sysval', @ignored );

        return $self->_lastSysExitCode;
    }

    method _mkstemp (Str $macro, Undef|Str $template?, Str @ignored --> Str) {
        if ( Undef->check($template) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->impl_quote($macro)
            );
            return '';
        }
        $self->_checkIgnored( $macro, @ignored );

        $template //= '';
        while ( !( $template =~ /XXXXXX$/ ) ) {
            $template .= 'X';
        }
        my $tmp = '';
        try {
            $tmp = File::Temp->new( TEMPLATE => $template );
        }
        catch {
            $self->logger_error( '%s: %s', $macro, $_ );
            return;
        };

        return $self->impl_quote( $tmp->filename );
    }

    method builtin_mkstemp (Str @args --> Str) {
        return $self->_mkstemp( 'mkstemp', @args );
    }

    method builtin_maketemp (Str @args --> Str) {
        return $self->_mkstemp( 'maketemp', @args );
    }

    method builtin_errprint (Str @args --> Str) {
        $self->logger_error( '%s', join( ' ', @args ) );
        return '';
    }

    method builtin___file__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__file__', @ignored );
        return $self->__file__;
    }

    method builtin___line__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__line__', @ignored );
        return $self->__line__;
    }

    method _allowBlockInSanitizedString (Str $string, RegexpRef $regexp, CodeRef $dollarOneToIndiceRef, CodeRef $indiceToReplacementRef, HashRef $wantedIndicesRef, Ref['SCALAR'] $maxArgumentIndiceRef --> Str) {

        $string =~ s/$regexp/
          {
           #
           # Writen like this to show that this is a BLOCK on the right-side of eval
           #
           my $indice = $self->$dollarOneToIndiceRef(substr($string, $-[1], $+[1] - $-[1]));
           if ($indice > ${$maxArgumentIndiceRef}) {
             ${$maxArgumentIndiceRef} = $indice;
           }
           $self->$indiceToReplacementRef($indice, $wantedIndicesRef);
          }/eg;

        return $string;
    }

    method builtin___program__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__program__', @ignored );
        return $self->__program__;
    }
    #
    # $0 is replaced by $name
    # arguments are in the form $1, $2, etc...
    # mapped to $args[0], $args[1], etc...
    # $# is the number of arguments
    # $* is all arguments separated by comma
    # $@ is all quoted arguments separated by comma
    #
    method _expansion2CodeRef (Str $name, Str $expansion --> CodeRef) {
                                #
                                # Check macro content
                                #
        my $warnMacroSequence = $self->_warn_macro_sequence;
        if ( !Undef->check($warnMacroSequence) ) {
            while ( $expansion =~ m/$warnMacroSequence/smg ) {
                $self->logger_warn(
                    'Definition of %s contains sequence %s',
                    $self->impl_quote($name),
                    $self->impl_quote(
                        substr( $expansion, $-[0], $+[0] - $-[0] )
                    )
                );
            }
        }

        my $maxArgumentIndice    = -1;
        my %wantedArgumentIndice = ();
        my $newExpansion         = quotemeta($expansion);
        #
        # Arguments and $0
        #
        $newExpansion =~ s/\\\$([0-9]+)/
          {
           #
           # Writen like this to how that this is a BLOCK on the right-side of eval
           #
           my $dollarOne = substr($newExpansion, $-[1], $+[1] - $-[1]);
           if ($dollarOne > $maxArgumentIndice) {
             $maxArgumentIndice = $dollarOne;
           }
           if ($dollarOne == 0) {
             # "\$0";
             quotemeta($name);
           } else {
             my $indice = $dollarOne - 1;
             $wantedArgumentIndice{$indice}++;
             "\$args\[$indice\]";
           }
          }/eg;
        my $prepareArguments = "\n\tmy (\$self, \@args) = \@_;\n";
        $prepareArguments .= "\n";
        #
        # We use unused argument indices from now on.
        #
        # Number of arguments.
        #
        if ( $newExpansion =~ s/\\\$\\\#/\$nbArgs/g ) {
            $prepareArguments .= "\tmy \$nbArgs = scalar(\@args);\n";
        }
        #
        # Arguments expansion, unquoted.
        #
        if ( $newExpansion =~ s/\\\$\\\*/\$listArgs/g ) {
            $prepareArguments
                .= "\tmy \$listArgs = join(',', map {\$_ // ''} \@args);\n";
        }
        #
        # Arguments expansion, quoted.
        #
        if ( $newExpansion =~ s/\\\$\\\@/\$listArgsQuoted/g ) {
            $prepareArguments
                .= "\tmy \$listArgsQuoted = join(',', map {\$self->impl_quote(\$_)} \@args);\n";
        }
        #
        # Take care: a macro can very well try to access
        # something outside of @args
        # We do this only NOW, because the //= will eventually
        # increase @args
        #
        if (%wantedArgumentIndice) {
            $prepareArguments .= "\n";
            foreach ( sort { $a <=> $b } keys %wantedArgumentIndice ) {
                $prepareArguments .= "\t\$args[$_] //= '';\n";
            }
        }
        my $stub;
        my $error;
        #
        # If it fails, our fault
        #
        my $stubSource = <<"STUB";
sub {
$prepareArguments
\treturn \"$newExpansion\";
}
STUB
        my $codeRef = eval "$stubSource";
        if ($@) {
            $self->logger_error( 'Internal: %s', $@ );
        }
        return $codeRef;
    }

    method impl_parseIncremental (Str $input --> ConsumerOf[M4Impl]) {
        try {
            #
            # This can croak and we will log when necessary
            #
            $self->_set_impl_unparsed( $self->parser_parse($input) );
        };
        return $self;
    }

    method impl_appendValue (Str $result --> ConsumerOf[M4Impl]) {
        $self->_lastDiversion->print($result);
        return $self;
    }

    method impl_parse (Str $input --> Str) {
        if ( $self->_eoi ) {
            $self->logger_error('No more input is accepted');
            return '';
        }
        $self->impl_setEoi;
        return $self->impl_parseIncremental($input)->impl_value;
    }

    method impl_setEoi (--> ConsumerOf[M4Impl]) {
        $self->_set__eoi(true);
        return $self;
    }

    method impl_valueRef (--> Ref['SCALAR']) {
            #
            # If not already done, say user input is finished
            #
        $self->impl_setEoi;
        #
        # Trigger EOF
        #
        $self->_eof(true);
        #
        # Return a reference to the value
        #
        return $self->_diversions_get(0)->sref;
    }

    method impl_value (--> Str) {
        return ${ $self->impl_valueRef };
    }

    method impl_line (@args --> Str) {
        return $self->__line__(@args);
    }

    method impl_debugfile ( --> Undef|Str) {
        return $self->_debugfile;
    }

    method impl_rc (@args --> Str) {
        return $self->_rc(@args);
    }

    with 'MarpaX::Languages::M4::Role::Impl';
    with 'MooX::Role::Logger';
}

1;
