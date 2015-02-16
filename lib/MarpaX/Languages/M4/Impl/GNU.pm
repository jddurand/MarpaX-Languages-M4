use Moops;
use MarpaX::Languages::M4::Impl::Parser;

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

# PODCLASSNAME

class MarpaX::Languages::M4::Impl::GNU::BaseConversion {
        #
        # Eval: constants for radix and the grammar
        #
    our @nums = ( 0 .. 9, 'a' .. 'z', 'A' .. 'Z' );
    our %nums = map { $nums[$_] => $_ } 0 .. $#nums;

    # Adaptation of http://www.perlmonks.org/?node_id=27148
    method to_base (ClassName $class: Int $base, Int $number, Int $min --> Str) {

        return $nums[0] if ( $number == 0 );

        my $rep = '';    # this will be the end value.
                         #
                         # Per construction $base is in the range [1..36]
                         #
        if ( $base == 1 ) {
            if ( $number < 0 ) {
                $rep .= '-';
                $number = -$number;
            }
            while ( $min-- - $number > 0 ) {
                $rep .= '0';
            }
            while ( $number-- != 0 ) {
                $rep .= '1';
            }
        }
        else {
            my $s      = '';
            my $signed = ( $number < 0 ) ? 1 : 0;
            my $abs    = abs($number);
            while ( $abs > 0 ) {
                $s   = $nums[ $abs % $base ] . $s;
                $abs = int( $abs / $base );
            }
            if ($signed) {
                $s = "-$s";
            }
            if ( substr( $s, 0, 1 ) eq '-' ) {
                $rep .= '-';
                substr( $s, 0, 1, '' );
            }
            for ( $min -= length($s); --$min >= 0; ) {
                $rep .= '0';
            }
            $rep .= $s;
        }

        return $rep;
    }

    method fr_base (ClassName $class: Int $base, Int $rep) {
        my $number = 0;
        for ( $rep =~ /./g ) {
            $number *= $base;
            $number += $nums{$_};
        }
        return $number;
    }
}

# PODCLASSNAME

class MarpaX::Languages::M4::Impl::GNU::Eval::Actions {
    use Bit::Vector;
    use Types::Common::Numeric -all;
    use MarpaX::Languages::M4::Impl::GNU::BaseConversion;

    #
    # Marpa dislike exceptions throws as objects, because of wanted
    # backward compatibility with very old versions of Perl.
    # So we will use Marpa::R2::Context::bail() method
    #

    has bits => {
        is      => 'ro',
        isa     => PositiveInt,
        default => sub {$MarpaX::Languages::M4::Impl::GNU::INTEGER_BITS}
    };

    has SELF => {
        is      => 'ro',
        isa     => ConsumerOf ['MarpaX::Languages::M4::Roles::Impl'],
        default => sub {$MarpaX::Languages::M4::Impl::GNU::SELF}
    };

    method _eval (ConsumerOf['Bit::Vector'] $expression) {
        return $expression->to_Dec();
    }

    method _invalidOp (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        Marpa::R2::Context::bail("Invalid operand: $op");
    }

    method _noop (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        return $expression;
    }

    method _lneg (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        return Bit::Vector->new_Dec( $self->bits, $expression->is_empty() );
    }

    method _exp (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        if ( $expression2->to_Dec() < 0 ) {
            Marpa::R2::Context::bail( "Negative exponent: "
                    . $expression1->to_Dec . " $op "
                    . $expression2->to_Dec );
        }

        if ( $expression1->to_Dec() == 0 && $expression2->to_Dec() == 0 ) {
            Marpa::R2::Context::bail( "Divide by zero: "
                    . $expression1->to_Dec . " $op "
                    . $expression2->to_Dec );
        }

        my $s = $expression1->Shadow;
        $s->Power( $expression1, $expression2 );
        return $s;
    }

    method _neg (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        my $s = $expression->Shadow;
        $s->Negate($expression);
        return $s;
    }

    method _bneg (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        my $s = $expression->Shadow;
        $s->Complement($expression);
        return $s;
    }

    method _mul (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->Multiply( $expression1, $expression2 );
        return $s;
    }

    method _div (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->Divide( $expression1, $expression2, $expression1->Shadow );
        return $s;
    }

    method _mod (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $expression1->Shadow->Divide( $expression1, $expression2, $s );
        return $s;
    }

    method _add (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->add( $expression1, $expression2, 0 );
        return $s;
    }

    method _sub (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->subtract( $expression1, $expression2, 0 );
        return $s;
    }

    # From GNU M4 source code:
    # Minimize undefined C behavior (shifting by a negative number,
    # shifting by the width or greater, left shift overflow, or
    # right shift of a negative number). Implement Java 32-bit
    # wrap-around semantics. This code assumes that the
    # implementation-defined overflow when casting unsigned to
    # a signed is a silent twos-complement wrap-around. */
    method _left (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        $expression1->Insert(
            0,
            $self->_band(
                $expression2,
                Bit::Vector->new_Hex(
                    $self->bits, '1' . ( 'f' x $self->bits - 1 )
                )
            )->to_Dec()
        );
        return $expression1;
    }

    method _right (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $u1 = Bit::Vector->new( $self->bits );
        $u1->Abs($expression1);
        $u1->Delete(
            0,
            $self->_band(
                $expression2,
                Bit::Vector->new_Hex(
                    $self->bits, '1' . ( 'f' x $self->bits - 1 )
                )
            )->to_Dec()
        );
        return ( $expression1->Sign() < 0 ) ? $self->_neg($u1) : $u1;
    }

    method _gt (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) > 0 ) ? 1 : 0 );
    }

    method _ge (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) >= 0 ) ? 1 : 0 );
    }

    method _lt (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) < 0 ) ? 1 : 0 );
    }

    method _le (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) <= 0 ) ? 1 : 0 );
    }

    method _eq (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) == 0 ) ? 1 : 0 );
    }

    method _eq2 (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        $self->SELF->logger_warn('Warning: recommend == instead oft =');
        return $self->_eq( $expression1, $op, $expression2 );
    }

    method _ne (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        return Bit::Vector->new_Dec( $self->bits,
            ( $expression1->Compare($expression2) != 0 ) ? 1 : 0 );
    }

    method _band (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->Intersection( $expression1, $expression2 );
        return $s;
    }

    method _bxor (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->ExclusiveOr( $expression1, $expression2 );
        return $s;
    }

    method _bor (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        $s->Union( $_[1], $_[2] );
        return $s;
    }
#
# M4 is short-circuiting valid syntax in case of '||' and '&&', so that things like
# 2 || 1 / 0 will not produce a fatal error. To produce such a behaviour
# only '||' or '&&' specific actions will be able to handle eventual undef value from
# prior actions
#
    method _land (ConsumerOf['Bit::Vector'] $expression1, Str $op, Undef|ConsumerOf['Bit::Vector'] $expression2) {
        my $rc;
        if ( !Undef->check($expression2) ) {
            $rc = Bit::Vector->new_Dec( $self->bits,
                ( !$expression1->is_empty() && !$expression2->is_empty() )
                ? 1
                : 0 );
        }
        elsif ( $expression1->is_empty() ) {
            #
            # Already zero
            #
            $rc = $expression1;
        }
        else {
            Marpa::R2::Context::bail( "Undefined right-hand expression : "
                    . $expression1->to_Dec
                    . " $op <undef>" );
        }
        return $rc;
    }

    method _lor (ConsumerOf['Bit::Vector'] $expression1, Str $op, Undef|ConsumerOf['Bit::Vector'] $expression2) {
        my $rc;
        if ( !Undef->check($expression2) ) {
            $rc = Bit::Vector->new_Dec( $self->bits,
                ( !$expression1->is_empty() || !$expression2->is_empty() )
                ? 1
                : 0 );
        }
        elsif ( !$expression1->is_empty() ) {
            $rc = Bit::Vector->new_Dec( $self->bits, 1 );
        }
        else {
            Marpa::R2::Context::bail( "Undefined right-hand expression : "
                    . $expression1->to_Dec
                    . " $op <undef>" );
        }
        return $rc;
    }
    #
    # Raw inputs are not allowed to fail. Eventual croaks are left alive.
    #
    method _decimal (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, $lexeme + 0 );
    }

    method _octal (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, oct( $_[1] ) );
    }

    method _hex (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, oct( $_[1] ) );
    }    # oct() supportx 0x notation

    method _binary (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, oct( $_[1] ) );
    }    # oct() supportx 0b notation

    method _radix (Str $lexeme) {
                    #
                    # Per def this is this regexp
                    #
        $lexeme =~ /0r([\d]+):([\da-zA-Z]+)/;
        my $radix = substr( $lexeme, $-[1], $+[1] - $-[1] );
        my $digits = lc( substr( $lexeme, $-[2], $+[2] - $-[2] ) )
            ;       # Because max base is 36
        if ( $radix == 1 ) {

  # For radix 1, leading zeros are ignored, and all remaining digits must be 1
            $digits =~ s/^0*//;
            if ( $digits =~ /[^1]/ ) {
                Marpa::R2::Context::bail(
                    "$lexeme: for radix 1, digits must be eventual zeroes followed by 1's"
                );
            }
        }
        return Bit::Vector->new_Dec(
            $self->bits,
            MarpaX::Languages::M4::Impl::GNU::BaseConversion->fr_base(
                $radix, $digits
            )
        );
    }

}

# PODCLASSNAME

class MarpaX::Languages::M4::Impl::GNU {
    extends 'MarpaX::Languages::M4::Impl::Parser';

    # ABSTRACT: M4 pre-processor perl implementation

    # VERSION

    use Bit::Vector;
    use Encode::Locale;
    use Encode;
    use Env::Path qw/M4PATH/;
    use File::Find;
    use File::Spec;
    use File::Temp;
    use IO::File;
    use IO::Scalar;
    use IPC::Cmd qw/run_forked/;
    use MarpaX::Languages::M4::Impl::GNU::BaseConversion;
    use MarpaX::Languages::M4::Impl::GNU::Eval::Actions;
    use MarpaX::Languages::M4::Impl::Macros;
    use MarpaX::Languages::M4::Impl::Macro;
    use MarpaX::Languages::M4::Roles::Impl;
    use MarpaX::Languages::M4::Types::Macro -all;
    use Marpa::R2;
    use MooX::HandlesVia;
    use Throwable::Factory EncodeError => [qw/$message $error proposal/];
    use Types::Common::Numeric -all;

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
                EncodeError->throw(
                    message => "Cannot decode command-line arguments",
                    error   => $error,
                    proposal =>
                        'Change or remove M4_ARGV_ENCODING environment variable'
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

    our @DEBUG_FLAGS         = qw/a c e f i l p q t x/;
    our @DEFAULT_DEBUG_FLAGS = qw/a e q/;
    our $DEFAULT_QUOTE_START = '`';
    our $DEFAULT_QUOTE_END   = "'";
    our $DEFAULT_COM_START   = '#';
    our $DEFAULT_COM_END     = "\n";
    our $DEFAULT_WORD_REGEXP = '[_a-zA-Z][_a-zA-Z0-9]*';

    our $TOKENS_PRIORITY_DEFAULT_VALUE
        = [qw/COMMENT QUOTEDSTRING WORD CHARACTER/];
    our $TOKENS_PRIORITY_TYPE
        = ArrayRef [ Enum [qw/COMMENT QUOTEDSTRING WORD CHARACTER/] ];

    our $INTEGER_TYPE_TYPE = Enum [qw/native bitvector/];
    our $INTEGER_TYPE_DEFAULT_VALUE = 'bitvector';

    our $INTEGER_BITS_TYPE          = PositiveInt;
    our $INTEGER_BITS_DEFAULT_VALUE = Bit::Vector->Word_Bits;

    our $M4WRAP_TYPE = Enum [qw/LIFO FIFO/];
    our $M4WRAP_DEFAULT_VALUE = 'LIFO';

    our $DIVERT_TYPE_TYPE = Enum [qw/memory file/];
    our $DIVERT_TYPE_DEFAULT_VALUE = 'memory';

    our $EXIT_TYPE          = Bool;
    our $EXIT_DEFAULT_VALUE = false;

    our $NEED_PARAM_TYPE = ArrayRef [Str];
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

    our $PARAMCANBEMACRO_PARAM_TYPE = ArrayRef [Str];
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

    #
    # Eval: constants for radix and the grammar
    #
    our @nums = ( 0 .. 9, 'a' .. 'z', 'A' .. 'Z' );
    our %nums = map { $nums[$_] => $_ } 0 .. $#nums;
    our $EVAL_G = Marpa::R2::Scanless::G->new(
        {   source => \<<EVAL_GRAMMAR
:default ::= action => ::first
:start ::= eval
eval ::= Expression                             action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_eval

Expression ::=
    Number
    | ('(') Expression (')') assoc => group
    # Catch common invalid operations for a nice error message
    # Uncatched stuff will have the Marpa native exception.
   || '++'  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '+='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '--'  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '-='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '*='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '/='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '%='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '>>=' Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '<<=' Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '^='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '&='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
    | '|='  Expression                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_invalidOp
   || '+' Expression                            action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_noop
    | '-' Expression                            action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_neg
    | '~' Expression                            action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_bneg
    | '!' Expression                            action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_lneg
   || Expression '**' Expression assoc => right action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_exp
   || Expression '*' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_mul
    | Expression ('/') Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_div
    | Expression '%' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_mod
   || Expression '+' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_add
    | Expression '-' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_sub
   || Expression '<<' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_left
    | Expression '>>' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_right
   || Expression '>' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_gt
    | Expression '>=' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_ge
    | Expression '<' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_lt
    | Expression '<=' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_le
   || Expression '==' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_eq
    # Special case of '=' aliased to '=='
    | Expression '=' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_eq2
    | Expression '!=' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_ne
   || Expression '&' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_band
   || Expression '^' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_bxor
   || Expression '|' Expression                 action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_bor
   || Expression '&&' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_land
   || Expression '||' Expression                action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_lor

Number ::= decimalNumber                        action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_decimal
         | octalNumber                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_octal
         | hexaNumber                           action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_hex
         | binaryNumber                         action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_binary
         | radixNumber                          action => MarpaX::Languages::M4::Impl::GNU::Eval::Actions::_radix

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
                $self->parse($fh);
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
    # POLICY OPTIONS
    # EVERY POLICY named policy_xxx maps to an internal attribute
    # _policy_xxx, that is modified using a trigger. Therefore the default
    # option value is the default value of the _xxx attribute.
    # Type constraint is done via the trigger.
    # We rely on automatic coercion from Str to a subtype if any.
    # The only exception is when the option is a boolean: we type
    # the option as such, and add the negativable extra-option.
    # ---------------------------------------------------------------
    option policy_tokens_priority => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        format      => 's',
        repeatable  => 1,
        autosplit   => ',',
        trigger     => 1,
        handles_via => 'Array',
        handles     => { policy_tokens_priority_elements => 'elements', },
        doc =>
            "Tokens priority. If setted, it is highly recommended to list all allowed values, that are : \"WORD\", \"MACRO\", \"QUOTEDSTRING\", and \"COMMENT\". The order of appearance on the command-line will be the prefered order when parsing M4 input. Multiple values can be given in the same switch if separated by the comma character ','. Unlisted values will keep their relative order from the default, which is: "
            . join( ',', @{$TOKENS_PRIORITY_DEFAULT_VALUE} )
    );

    option policy_integer_type => (
        is      => 'rw',
        default => '',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            q{Integer type. Possible values: "native" (will use what your hardware provides), "bitvector" (will use s/w-driven bit-per-bit manipulations, this is the only portable option value). Default: "bitvector".}
    );

    option policy_integer_bits => (
        is      => 'rw',
        isa     => Str,
        trigger => 1,
        default => '',
        format  => 'i',
        doc =>
            "Number of bits for integer arithmetic. Possible values: any positive integer. Default is \"unsigned int\" C-type number of bits from the libc used to build this version of perl. Meaningul only when policy_integer_type is \"bitvector\". Default: $INTEGER_BITS_DEFAULT_VALUE."
    );

    option policy_m4wrap => (
        is      => 'rw',
        default => '',
        isa     => Str,
        trigger => 1,
        format  => 's',
        doc =>
            q{M4wrap unbuffer mode. Possible values: "LIFO" (Last In, First Out), "FIFO" (First In, First Out). Default: "LIFO".}
    );

    option policy_divert_type => (
        is      => 'rw',
        default => '',
        trigger => 1,
        isa     => Str,
        format  => 's',
        doc =>
            q{Divertion type. Possible values: "memory" (all diversions are kept in memory), "temp" (all diversions are kept in temporary files). Default: "memory".}
    );

    option policy_exit => (
        is          => 'rw',
        isa         => Bool,
        default     => false,
        trigger     => 1,
        negativable => 1,
        doc =>
            q{Exit policy. Negativable option. Default: a false value in the API mode, a true value in the command-line mode.}
    );

    option policy_need_param => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        trigger     => 1,
        format      => 's',
        repeatable  => 1,
        autosplit   => ',',
        handles_via => 'Array',
        handles     => { policy_need_param_elements => 'elements', },
        doc =>
            "Recognized-only-with-parameters policy. Repeatable option. Multiple values can be given in the same switch if separated by the comma character ','. Says if a macro is recognized only if it is immediately followed by a left parenthesis. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then the option is considered. Any attempt to set it on the command-line will completely overwrite the default. Default: "
            . join( ',', @{$NEED_PARAM_DEFAULT_VALUE} ) . '.'
    );

    option policy_paramcanbemacro => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        trigger     => 1,
        format      => 's',
        repeatable  => 1,
        autosplit   => ',',
        handles_via => 'Array',
        handles     => { policy_paramcanbemacro_elements => 'elements', },
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

    option prefix_builtins => (
        is      => 'rw',
        isa     => Str,
        format  => 's',
        default => '',
        doc     => q{Prefix of all builtin macros. Default is empty.}
    );

    option silent => (
        is          => 'rw',
        isa         => Bool,
        default     => false,
        negativable => 1,
        short       => 's',
        doc =>
            q{Silent mode. Negativable option. If true all warnings will disappear. Default: a false value.}
    );

    option trace => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        format      => 's',
        repeatable  => 1,
        autosplit   => ',',
        trigger     => 1,
        handles_via => 'Array',
        handles     => { trace_elements => 'elements', },
        doc =>
            q{Trace mode. Repeatable option. Multiple values can be given in the same switch if separated by the comma character ','. Every option value will set trace on the macro sharing this name. Default is empty.}
    );

    option define => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        handles_via => 'Array',
        handles     => { define_elements => 'elements', },
        format      => 's',
        short       => 'D',
        repeatable  => 1,
        trigger     => 1,
        doc =>
            q{Macro definition. Repeatable option. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then a macro is declared. For example: --define myMacro. Or --word_regexp x= --define x=. Default expansion is void, unless the matched name is followed by '=', then any remaining character will be the expansion of this new macro. For example: --define myMacro=myExpansion. Or --word_regexp x= --define x==myExpansion. Default is empty.}
    );

    option undefine => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        handles_via => 'Array',
        handles     => { undefine_elements => 'elements', },
        format      => 's',
        short       => 'U',
        repeatable  => 1,
        trigger     => 1,
        doc =>
            q{Macro undefinition. Repeatable option. Every option value is subject to the value of word_regexp: if it matches word_regexp at the beginning, then a macro is deleted if it exists. Default is empty.}
    );

    option prepend_include => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        format      => 's',
        short       => 'B',
        repeatable  => 1,
        trigger     => 1,
        handles_via => 'Array',
        handles     => { prepend_include_elements => 'elements', },
        doc =>
            q{Include directory. Repeatable option. Will be used in reverse order and before current directory when searching for a file to include. Default is empty.}
    );

    option include => (
        is          => 'rw',
        isa         => ArrayRef [Str],
        default     => sub { [] },
        format      => 's',
        short       => 'I',
        repeatable  => 1,
        handles_via => 'Array',
        handles     => { include_elements => 'elements', },
        doc =>
            q{Include directory. Repeatable option. Will be used in order and after current directory when searching for a file to include. Default is empty.}
    );

    # ---------------------------------------------------------------
    # RUNTIME DEFAULT OPTIONS
    # ---------------------------------------------------------------
    option debugmode => (
        is      => 'rw',
        isa     => Str,
        default => 'aeq',
        trigger => 1,
        format  => 's',
        short   => 'd',
        doc => 'Debug mode. This is a combinaison of flags, that can be: "'
            . join( '", "', @DEBUG_FLAGS )
            . '", or "V" wich will put everything on. Default: "'
            . join( '', @DEFAULT_DEBUG_FLAGS ) . '".'
    );

    option debugfile => (
        is      => 'rw',
        isa     => Undef | Str,
        default => undef,
        format  => 's',
        short   => 'o',
        doc =>
            q{Debug file. An empty value disable debug output. A null value redirects to standard error. Default is a null value.}
    );

    option quote_start => (
        is          => 'rw',
        isa         => Str,
        default     => $DEFAULT_QUOTE_START,
        trigger     => 1,
        handles_via => 'String',
        handles     => { quote_start_length => 'length' },
        format      => 's',
        doc =>
            "Quote start. An empty option value disables support of quoted string. Default: \"$DEFAULT_QUOTE_START\"."
    );

    option quote_end => (
        is          => 'rw',
        isa         => Str,
        default     => $DEFAULT_QUOTE_END,
        trigger     => 1,
        handles_via => 'String',
        handles     => { quote_end_length => 'length' },
        format      => 's',
        doc         => "Quote end. Default: \"$DEFAULT_QUOTE_END\"."
    );

    option com_start => (
        is          => 'rw',
        isa         => Str,
        default     => $DEFAULT_COM_START,
        trigger     => 1,
        handles_via => 'String',
        handles     => { com_start_length => 'length' },
        format      => 's',
        doc         => "Comment start. Default: \"$DEFAULT_COM_START\"."
    );

    option com_end => (
        is          => 'rw',
        isa         => Str,
        default     => $DEFAULT_COM_END,
        trigger     => 1,
        handles_via => 'String',
        handles     => { com_end_length => 'length' },
        format      => 's',
        doc         => "Comment end. Default: the newline character."
    );

    option word_regexp => (
        is      => 'rw',
        isa     => Str,
        default => $DEFAULT_WORD_REGEXP,
        trigger => 1,
        format  => 's',
        short   => 'W',
        doc =>
            "Word perl regular expression. Default: \"$DEFAULT_WORD_REGEXP\"."
    );

    # ---------------------------------------------------------------
    # BUILDERS
    # ---------------------------------------------------------------

    # ---------------------------------------------------------------
    # POLICY MAPPED ATTRIBUTES
    # ---------------------------------------------------------------
    has _policy_tokens_priority => (
        is          => 'rw',
        isa         => $TOKENS_PRIORITY_TYPE,
        default     => sub {$TOKENS_PRIORITY_DEFAULT_VALUE},
        handles_via => 'Array',
        handles     => {
            _policy_tokens_priority_elements => 'elements',
            _policy_tokens_priority_count    => 'count',
            _policy_tokens_priority_get      => 'get'
        },
    );

    has _policy_integer_type => (
        is      => 'rw',
        isa     => $INTEGER_TYPE_TYPE,
        default => $INTEGER_TYPE_DEFAULT_VALUE,
    );

    has _policy_integer_bits => (
        is      => 'rw',
        isa     => $INTEGER_BITS_TYPE,
        default => $INTEGER_BITS_DEFAULT_VALUE,
    );

    has _policy_m4wrap => (
        is      => 'rw',
        isa     => $M4WRAP_TYPE,
        default => $M4WRAP_DEFAULT_VALUE,
    );

    has _policy_divert_type => (
        is      => 'rw',
        isa     => $DIVERT_TYPE_TYPE,
        default => $DIVERT_TYPE_DEFAULT_VALUE,
    );

    has _policy_exit => (
        is      => 'rw',
        isa     => $EXIT_TYPE,
        default => $EXIT_DEFAULT_VALUE,
    );

    has _policy_need_param => (
        is      => 'rw',
        isa     => HashRef [Bool],
        default => sub {
            my %ref = map { $_ => true } @{$NEED_PARAM_DEFAULT_VALUE};
            \%ref;
        },
        handles_via => 'Hash',
        handles     => {
            _policy_need_param_set    => 'set',
            _policy_need_param_get    => 'get',
            _policy_need_param_exists => 'exists',
            _policy_need_param_keys   => 'keys',
            _policy_need_param_delete => 'delete'
        },
    );

    has _policy_paramcanbemacro => (
        is  => 'rw',
        isa => HashRef [ HashRef [ PositiveOrZeroInt | Enum [qw/*/] ] ],
        default     => sub {$PARAMCANBEMACRO_DEFAULT_VALUE},
        handles_via => 'Hash',
        handles     => {
            _policy_paramcanbemacro_set    => 'set',
            _policy_paramcanbemacro_get    => 'get',
            _policy_paramcanbemacro_exists => 'exists',
            _policy_paramcanbemacro_keys   => 'keys',
            _policy_paramcanbemacro_delete => 'delete'
        },
    );

    # ---------------------------------------------------------------
    # PARSER REQUIRED METHODS
    # ---------------------------------------------------------------
    method parse_isWord (Str $input, PositiveOrZeroInt $pos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        my $word_regexp = $self->_word_regexp;
        pos($input) = $pos;
        if ( $input =~ /\G$word_regexp/s ) {
            ${$lexemeLengthRef} = $+[0] - $-[0];
            if ( !Undef->check( $-[1] ) && $+[1] > $-[1] ) {
                ${$lexemeValueRef} = substr( $input, $-[1], $+[1] - $-[1] );
            }
            else {
                ${$lexemeValueRef}
                    = substr( $input, $-[0], ${$lexemeLengthRef} );
            }
            return true;
        }
        return false;
    }

    method parse_isComment (Str $input, PositiveOrZeroInt $pos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        my $com_regexp = $self->_com_regexp;
        if ( Undef->check($com_regexp) ) {
            return false;
        }

        pos($input) = $pos;
        if ( $input =~ /\G$com_regexp/s ) {
            ${$lexemeLengthRef} = $+[0] - $-[0];
            ${$lexemeValueRef} = substr( $input, $-[0], ${$lexemeLengthRef} );
            return true;
        }
        return false;
    }

    method parse_isQuotedstring (Str $input, PositiveOrZeroInt $pos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        my $quotedstring_regexp = $self->_quotedstring_regexp;
        if ( Undef->check($quotedstring_regexp) ) {
            return false;
        }

        pos($input) = $pos;
        if ( $input =~ /\G$quotedstring_regexp/s ) {
            ${$lexemeLengthRef} = $+[0] - $-[0];
            ${$lexemeValueRef}  = $self->unquote(
                substr( $input, $-[0], ${$lexemeLengthRef} ) );
            return true;
        }
        return false;
    }

    method parse_isCharacter (Str $input, PositiveOrZeroInt $pos, Ref $lexemeValueRef, Ref $lexemeLengthRef --> Bool) {
        pos($input) = $pos;
        if ( $input =~ /\G./s ) {
            ${$lexemeLengthRef} = $+[0] - $-[0];
            ${$lexemeValueRef} = substr( $input, $-[0], ${$lexemeLengthRef} );
            return true;
        }
        return false;
    }

    method _getMacro (Str $word --> M4Macro) {
      return $self->_macros_get($word)->get(-1);
    }

    method parse_isMacro (Str $word, Ref $macroRef? --> Bool) {
        if ( $self->_macros_exists($word) ) {
            ${$macroRef} = $self->_getMacro($word);
            return true;
        }
        return false,;
    }

    method parse_tokensPriority {
        return $self->_policy_tokens_priority_elements;
    }

    # ---------------------------------------------------------------
    # LOGGER REQUIRED METHODS
    # ---------------------------------------------------------------
    method logger_error (@args --> Undef) { $self->_logger->errorf(@args); return; }

    method logger_warn (@args --> Undef) {
        if ( !$self->silent ) {
            $self->_logger->warnf(@args);
        }
        return;
    }

    method logger_debug (@args --> Undef) {
        if ( Undef->check( $self->debugfile ) ) {
            $self->_logger->debugf(@args);
        }
        else {
            Log::Any::Adapter->set( { lexically => \my $lex },
                'File', $self->debugfile );
            $self->_logger->debugf(@args);
        }
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

    has _trace => (
        is          => 'rwp',
        isa         => HashRef [Bool],
        default     => sub { {} },
        handles_via => 'Hash',
        handles     => {
            _trace_set    => 'set',
            _trace_get    => 'get',
            _trace_exists => 'exists',
            _trace_keys   => 'keys',
            _trace_delete => 'delete'
        }
    );

    has _debug => (
        is          => 'lazy',
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

    has _value => (
        is      => 'rwp',
        isa     => Str,
        default => ''
    );

    # ----------------------------------------------------
    # builders
    # ----------------------------------------------------
    method _build__debug {
        my %ref = ();
        map { $ref{$_} = false } @DEBUG_FLAGS;
        map { $ref{$_} = true } @DEFAULT_DEBUG_FLAGS;
        return \%ref;
    }

    method _build__com_regexp {
        return __PACKAGE__->_generate_com_regexp( $DEFAULT_COM_START,
            $DEFAULT_COM_END );
    }

    method _build__quotedstring_regexp {
        return __PACKAGE__->_generate_quotedstring_regexp(
            $DEFAULT_QUOTE_START, $DEFAULT_QUOTE_END );
    }

    method _build__logger_category {
        return 'M4';
    }

    method _build__diversions {
            #
            # Diversion 0 is special and maps directly to an internal variable
            #
        return { 0 => IO::Scalar->new };
    }

    method _build__lastDiversion {
        return $self->_diversions_get(0);
    }

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
            $ref{$_} = MarpaX::Languages::M4::Impl::Macro->new(
                name      => $_,
                expansion => 'TO BE REPLACED',
                stub      => $self->meta->get_method("_m4_$_")->body
            );
            #
            # The expansion of a builtin is the builtin itself
            #
            $ref{$_}->expansion( $ref{$_} );
            if ( $self->_policy_need_param_exists($_) ) {
                $ref{$_}->needParams( $self->_policy_need_param_get($_) );
            }
            if ( $self->_policy_paramcanbemacro_exists($_) ) {
                $ref{$_}->paramCanBeMacro(
                    $self->_policy_paramcanbemacro_get($_) );
            }
            if ( $_ eq 'dnl' ) {
                $ref{$_}->postMatchLength(
                    sub {
                        my ( $self, $input, $pos ) = @_;
                        pos($input) = $pos;
                        if ( $input =~ /\G.*?\n/s ) {
                            return $+[0] - $-[0];
                        }
                        elsif ( $input =~ /\G[^\n]*\z/ ) {
                            $self->logger_warn( '%s: %s',
                                'dnl', 'EOF without a newline' );
                        }
                    }
                );
            }
        }
        if ( is_os_type('Windows') ) {
            foreach (qw/__windows__ windows/) {
                $ref{$_} = MarpaX::Languages::M4::Impl::Macro->new(
                    name      => $_,
                    expansion => "<$_>",
                    stub      => sub { return ''; }
                );
            }
            #
            # A priori I assume this is reliable
            #
            if ( $^O eq 'os2' ) {
                foreach (qw/__os2__ os2/) {
                    $ref{$_} = MarpaX::Languages::M4::Impl::Macro->new(
                        name      => $_,
                        expansion => "<$_>",
                        stub      => sub { return ''; }
                    );
                }
            }
        }
        if ( is_os_type('Unix') ) {
            foreach (qw/__unix__ unix/) {
                $ref{$_} = MarpaX::Languages::M4::Impl::Macro->new(
                    name      => $_,
                    expansion => "<$_>",
                    stub      => sub { return ''; }
                );
            }
        }
        return \%ref;
    }

    method _build__macros {
        my %ref = ();
        foreach ( $self->_builtins_keys ) {
            my $macros = MarpaX::Languages::M4::Impl::Macros->new();
            $macros->push( $self->_builtins_get($_) );
            $ref{ $self->prefix_builtins . $_ } = $macros;
        }
        return \%ref;
    }

    # ----------------------------------------------------
    # Triggers
    # ----------------------------------------------------
    method _trigger_policy_tokens_priority (ArrayRef[Str] $policy_tokens_priority --> Undef) {
        my %tokens_priority = ();
        my $currentMaxIndex = $#{$policy_tokens_priority};
        foreach ( 0 .. $currentMaxIndex ) {
            $tokens_priority{ $policy_tokens_priority->[$_] } = $_;
        }
        foreach ( 0 .. $self->_policy_tokens_priority_count - 1 ) {
            my $lexeme = $self->_policy_tokens_priority_get($_);
            if ( !exists( $tokens_priority{$lexeme} ) ) {
                $tokens_priority{$lexeme} = ++$currentMaxIndex;
            }
        }

        $self->_policy_tokens_priority(
            [   sort { $tokens_priority{$a} <=> $tokens_priority{$b} }
                    keys %tokens_priority
            ]
        );
        return;
    }

    method _trigger_policy_integer_type (Str $policy_integer_type --> Undef) {
        $self->_policy_integer_type($policy_integer_type);
        return;
    }

    method _trigger_policy_integer_bits (Str $policy_integer_bits --> Undef) {
        $self->_policy_integer_bits($policy_integer_bits);
        return;
    }

    method _trigger_policy_m4wrap (Str $policy_m4wrap --> Undef) {
        $self->_policy_m4wrap($policy_m4wrap);
        return;
    }

    method _trigger_policy_divert_type (Str $policy_divert_type --> Undef) {
        $self->_policy_divert_type($policy_divert_type);
        return;
    }

    method _trigger_policy_exit (Bool $policy_exit --> Undef) {
        $self->_policy_exit($policy_exit);
        return;
    }

    method _trigger_policy_need_param (ArrayRef[Str] $policy_need_param --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$policy_need_param} ) {
            if ( $_ =~ /^$word_regexp$/ ) {
                $self->_policy_need_param_set( $_, true );
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'policy_need_param', $_ );
            }
        }
        return;
    }

    method _trigger_policy_paramcanbemacro (ArrayRef[Str] $policy_paramcanbemacro --> Undef) {
        my $word_regexp = $self->_word_regexp;
        my %ref         = ();
        foreach ( @{$policy_paramcanbemacro} ) {
            if ( $_ =~ /^($word_regexp)(?:=(.*))/ ) {
                my $macroName = $1;
                my $indicesToSplit = $3 // '';
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
        $self->_policy_paramcanbemacro_set( \%ref );
        return;
    }

    method _trigger_word_regexp (Str $regexp, @rest --> Undef) {
        $self->_word_regexp(qr/$regexp/);
        return;
    }

    method _trigger_trace (ArrayRef[Str] $arrayRef --> Undef) {
        foreach ( @{$arrayRef} ) {
            $self->_trace_set($_);
        }
        return;
    }

    method _trigger_define (ArrayRef[Str] $arrayRef --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$arrayRef} ) {
            if ( $_ =~ /^($word_regexp)(?:=(.*))/ ) {
                $self->_m4_define( $1, $3 || '' );
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'define', $_ );
            }
        }
        return;
    }

    method _trigger_undefine (ArrayRef[Str] $arrayRef --> Undef) {
        my $word_regexp = $self->_word_regexp;
        foreach ( @{$arrayRef} ) {
            if ( $_ =~ /^$word_regexp$/ ) {
                $self->_m4_undefine($_);
            }
            else {
                $self->logger_warn( '%s: %s: does not match word regexp',
                    'undefine', $_ );
            }
        }
        return;
    }

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

    method _trigger_com_start (Str $com_start, @args --> Undef) {
        $self->_set__com_regexp(
            __PACKAGE__->_generate_com_regexp( $com_start, $self->com_end ) );
        return;
    }

    method _trigger_com_end (Str $com_end, @args --> Undef) {
        $self->_set__com_regexp(
            __PACKAGE__->_generate_com_regexp( $self->com_start, $com_end ) );
        return;
    }

    method _trigger_quote_start (Str $quote_start, @args --> Undef) {
        $self->_set__quotedstring_regexp(
            __PACKAGE__->_generate_quotedstring_regexp(
                $quote_start, $self->quote_end
            )
        );
        return;
    }

    method _trigger_quote_end (Str $quote_end, @args --> Undef) {
        $self->_set__quotedstring_regexp(
            __PACKAGE__->_generate_quotedstring_regexp(
                $self->quote_start, $quote_end
            )
        );
        return;
    }

    method _trigger_P (Bool $P, @args --> Undef) {
        $self->prefix_builtins('m4_');
    }

    # ----------------------------------------------------
    # Internal attributes
    # ----------------------------------------------------
    has _debugfh => (
        is      => 'rwp',
        isa     => Undef | ConsumerOf ['IO::Handle'],
        default => sub { IO::Handle->new_from_fd( fileno(STDERR), 'w' ) }
    );

    method _generate_com_regexp (ClassName $class: Str $com_start, Str $com_end --> RegexpRef|Undef) {
                                  #
                                  # In M4 comments are not balanced
                                  #
        if ( length($com_start) > 0 && length($com_end) > 0 ) {
            my $start = join( '',
                map { "\\x{" . sprintf( '%x', ord($_) ) . "}" }
                    split( '', $com_start ) );
            my $end = join( '',
                map { "\\x{" . sprintf( '%x', ord($_) ) . "}" }
                    split( '', $com_end ) );
            return qr/\G$start(.*?)$end/;
        }
        else {
            return undef;
        }
    }

    method _generate_quotedstring_regexp (ClassName $class: Str $quote_start, Str $quote_end --> RegexpRef|Undef) {
            #
            # We apply the same technique as Regexp::Common::balanced
            # but without its pre-processing on the '|' character and
            # by forcing the \G at the beginning
            #
        if ( length($quote_start) > 0 && length($quote_end) > 0 ) {
            local $" = "|";
            my @re;
            my $qb = quotemeta $quote_start;
            my $qe = quotemeta $quote_end;
            my $fb = quotemeta substr $quote_start => 0, 1;
            my $fe = quotemeta substr $quote_end => 0, 1;
            my $tb = quotemeta substr $quote_start => 1;
            my $te = quotemeta substr $quote_end => 1;
            my $add;

            if ( $fb eq $fe ) {
                push @re =>
                    qq /(?:$qb(?:(?>[^$fb]+)|$fb(?!$tb)(?!$te)|(?-1))*$qe)/;
            }
            else {
                my @clauses = "(?>[^$fb$fe]+)";
                push @clauses => "$fb(?!$tb)" if length $tb;
                push @clauses => "$fe(?!$te)" if length $te;
                push @clauses => "(?-1)";
                push @re      => qq /(?:$qb(?:@clauses)*$qe)/;
            }
            return qr/(@re)/;
        }
        else {
            return undef;
        }
    }

    method _exit (Int $exitValue) {
        if ( $self->policy_exit ) {
            exit($exitValue);
        }
    }

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

    has _word_regexp => (
        is      => 'rw',
        isa     => RegexpRef,
        default => sub {qr/$DEFAULT_WORD_REGEXP/}
    );

    has _com_regexp => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => RegexpRef | Undef
    );

    has _quotedstring_regexp => (
        is      => 'rwp',
        lazy    => 1,
        builder => 1,
        isa     => RegexpRef | Undef
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
        }
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

    method quote (Str $string --> Str) {
        if ( $self->quote_start_length > 0 && $self->quote_end_length > 0 ) {
            return $self->quote_start . $string . $self->quote_end;
        }
        else {
            return $string;
        }
    }

    method unquote (Str $string --> Str) {
        if ( $self->quote_start_length > 0 && $self->quote_end_length > 0 ) {
            substr( $string, 0, $self->quote_start_length, '' );
            substr( $string, -$self->quote_end_length,
                $self->quote_start_length, '' );
        }
        return $string;
    }

    method _checkIgnored (Str $name, @ignored --> Undef) {
        if (@ignored) {
            $self->logger_warn( 'excess arguments to builtin %s ignored',
                $self->quote($name) );
        }
        return;
    }

    method _m4_define (Undef|Str|M4Macro $name?, Undef|Str|M4Macro $defn?, @ignored --> Str) {
        if ( Undef->check($name) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('define') );
            return '';
        }
        $defn //= '';

        $self->_checkIgnored( 'define', @ignored );

        if ( M4Macro->check($name) ) {
            $self->logger_warn( '%s: invalid macro name ignored', 'define' );
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
            $macros->push($macro);
            $self->_macros_set( $name, $macros );
        }
        else {
            $self->_macros_get($name)->set( -1, $macro );
        }
        return '';
    }

    method _m4_undefine (Str @names --> Str) {
        $self->_macros_delete(@names);
        return '';
    }

    #
    # defn can only concatenate text macros
    #
    method _m4_defn (Str @names --> Str|M4Macro) {
        my @rc      = ();
        my $nbMacro = 0;
        my $nbStr   = 0;
        foreach (@names) {
            if ( $self->_macros_exists($_) ) {
                push( @rc, $self->_getMacro($_)->expansion );
            }
        }
        my $rc = '';
        foreach ( 0 .. $#rc ) {
            if ( M4Macro->check( $rc[$_] ) ) {
                if ( $rc[$_]->is_builtin ) {
                    if (   ( $_ == 0 && $#rc > 0 )
                        || ( $_ > 0 ) )
                    {
                        $self->logger_warn(
                            '%s: cannot concatenate builtin %s',
                            'defn', $self->quote( $rc[$_]->name ) );
                    }
                    else {
           #
           # Per def this is ok only if @rc has one element, that is a builtin
           #
                        $rc = $rc[$_];
                    }
                }
                else {
                    $rc .= $self->quote( $rc[$_]->expansion );
                }
            }
            else {
                $rc .= $self->quote( $rc[$_] );
            }
        }
        return $rc;
    }

    method _m4_pushdef (Undef|Str $name?, Undef|Str|M4Macro $defn?, @ignored --> Str) {
        if ( Undef->check($name) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('pushdef') );
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
            $macros->push($macro);
            $self->_macros_set( $name, $macros );
        }
        else {
            $self->_macros_get($name)->push($macro);
        }
        return '';
    }

    method _m4_popdef (Str @names --> Str) {

        foreach (@names) {
            if ( $self->_macros_exists($_) ) {
                $self->_macros_get($_)->pop();
                if ( $self->_macros_get($_)->is_empty ) {
                    $self->_macros_delete($_);
                }
            }
        }
        return '';
    }

    method _m4_indir (Undef|Str|M4Macro $name, @args --> Str|M4Macro) {
        if ( Undef->check($name) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('indir') );
            return '';
        }
        #
        # If $name is a builtin, check the other arguments
        #
        if ( M4Macro->check($name) ) {
            $self->logger_warn(
                'indir: invalid macro name ignored',
                $self->quote( $name->name )
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
                    && !$macro->paramCanBeMacro_check($_) )
                {
                    #
                    # Macro not authorized: flattened to the empty string
                    #
                    $args[$_] = '';
                }
            }
            return $macro->execute( $self, @args );
        }
        else {
            $self->logger_error( 'indir: undefined macro %s',
                $self->quote($name) );
            return '';
        }
    }

    method _m4_builtin (Undef|Str|M4Macro $name?, @args --> Str|M4Macro) {
        if ( Undef->check($name) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('builtin') );
            return '';
        }
        if ( M4Macro->check($name) ) {
            #
            # Not supported
            #
            $self->logger_error( '%s: invalid macro name ignored',
                $self->quote('builtin') );
            return '';
        }
        if ( $self->_builtins_exists($name) ) {
            #
            # We do not check the args to eventually flatten them. Thus this
            # can croak
            #
            my $rc = '';
            try {
                $rc = $self->_builtins_get($name)->execute( $self, @args );
            }
            catch {
                $self->logger_error( '%s', $_ );
            };
            return $rc;
        }
        else {
            $self->logger_error( 'builtin: undefined builtin %s',
                $self->quote($name) );
            return '';
        }
    }

    method _m4_ifdef (Undef|Str $name?, Undef|Str $string1?, Undef|Str $string2?, @ignored --> Str) {
        if ( Undef->check($name) || Undef->check($string1) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('ifdef') );
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

    method _m4_ifelse (@args --> Str) {
        while (@args) {
            if ( scalar(@args) <= 1 ) {
                return '';
            }
            elsif ( scalar(@args) == 2 ) {
                $self->logger_error( 'too few arguments to builtin %s',
                    $self->quote('ifelse') );
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
                        $self->quote('ifelse') );
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

    method _m4_shift (@args --> Str) {
        shift(@args);

        if (@args) {
            return join( ',', map { $self->quote($_) } @args );
        }
        else {
            return '';
        }
    }

    method _m4_dumpdef (@args --> Str) {

        if ( !@args ) {
            @args = $self->_macros_keys;
        }

        foreach ( sort @args ) {
            if ( !$self->_macros_exists($_) ) {
                $self->logger_warn( 'dumpdef: undefined macro %s',
                    $self->quote($_) );
            }
            else {
                $self->logger_debug( '%s: %s', $_,
                    $self->_getMacro($_)->is_builtin ? "<$_>" : $self->_getMacro($_)->expansion );
            }
        }

        return '';
    }

    method _m4_traceon (@names --> Str) {
        foreach (@names) {
            $self->_trace_set( $_, true );
        }
        return '';
    }

    method _m4_traceoff (@names --> Str) {
        foreach (@names) {
            $self->_trace_set( $_, false );
        }
        return '';
    }

    method _m4_debugmode (Undef|Str $flags?, @ignored --> Str) {
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

    method _m4_debugfile (Undef|Str $file?, @ignored --> Str) {

        $self->_checkIgnored( 'debugfile', @ignored );
        $self->debugfile($file);
        return '';
    }

    method _m4_dnl (@ignored --> Str) {
        $self->_checkIgnored( 'dnl', @ignored );
        return '';
    }

    method _m4_changequote (Undef|Str $start?, Undef|Str $end?, @ignored --> Str) {
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

        $self->quote_start($start);
        $self->quote_end($end);

        return '';
    }

    method _m4_changecom (Undef|Str $start?, Undef|Str $end?, @ignored --> Str) {
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
            $end ||= $DEFAULT_COM_END;
        }

        $self->com_start($start);
        $self->com_end($end);

        return '';
    }

    method _m4_changeword (Undef|Str $string?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error(
                'too few arguments to builtin %s',
                $self->quote('changeword')
            );
            return '';
        }
        $self->_checkIgnored( 'changeword', @ignored );

        try {
            $self->word_regexp( $string || $DEFAULT_WORD_REGEXP );
        }
        catch {
            $self->logger_error( 'changeword: %s', $_ );
            return;
        }
        return '';
    }

    method _m4_m4wrap (@args --> Str) {

        my $text = join( ' ', grep { !Undef->check($_) } @args );
        if ( $self->_policy_m4wrap eq 'LIFO' ) {
            $self->_m4wrap_unshift($text);
        }
        else {
            $self->_m4wrap_push($text);
        }

        return '';
    }

    method _includeFile (Bool $silent, Str $file? --> Str) {

        if ( length($file) <= 0 ) {
            return '';
        }
        my @paths = ();

        my @includes = (
            reverse( $self->prepend_include_elements ),
            File::Spec->curdir(), reverse( $self->include_elements ),
            M4PATH->List
        );

        my @candidates;
        {
            use filetest 'access';
            @candidates = grep { -r $_ }
                map { File::Spec->catfile( $_, $file ) } @includes;
        }

        if (! @candidates) {
          #
          # It is guaranteed that #includes have at least one element.
          # Therefore, $! should be setted
          #
          if ( !$silent ) {
            $self->logger_error( 'cannot open %s: %s', $self->quote($file), $! );
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
                    return $content;
                }
            }
        }

        return '';
    }

    method _m4_include (Undef|Str $file, @ignored --> Str) {
        if ( Undef->check($file) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('include') );
            return '';
        }
        $self->_checkIgnored( 'include', @ignored );

        return $self->_includeFile( false, $file );
    }

    method _m4_sinclude (Undef|Str $file, @ignored --> Str) {
        if ( Undef->check($file) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('sinclude') );
            return '';
        }
        $self->_checkIgnored( 'sinclude', @ignored );

        return $self->_includeFile( true, $file );
    }

    method DESTROY {
                          # $self->_m4_undivert();
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
# We don't know the $fh of previous diversion, it is stored in diversions hash.
#
        $self->_set__lastDiversion(
            $self->_diversions_get( $self->_m4_divnum ) );
        return;
    }

    method _m4_divert (Undef|Str $number?, @ignored --> Str) {
        $self->_checkIgnored( 'divert', @ignored );

        $number //= 0;
        if ( length("$number") <= 0 ) {
            $self->logger_warn( 'empty string treated as 0 in builtin %s',
                $self->quote('divert') );
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
                    if ( $self->_policy_divert_type eq 'memory' ) {
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

    method _m4_undivert (Str @diversions --> Str) {

        if ( !@diversions ) {
            @diversions = $self->_diversions_sortedKeys;
        }

        foreach (@diversions) {
            my $number = $_;
            if ( Int->check($number) ) {
#
# Undiverting the current diversion, or number 0, or a unknown diversion is silently ignored.
#
                if (   $number == $self->divnum
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
                    $self->appendValue($content);
                }
                else {
                    $self->_remove_diversion($number);
                }
            }
            else {
                #
                # Treated as name of a file
                #
                $self->appendValue( $self->_m4_include($number) );
            }
        }

        return '';
    }

    method _m4_divnum (@ignored --> Str) {
        $self->_checkIgnored( 'divnum', @ignored );

        return $self->_lastDiversionNumbers_get(-1);
    }

    method _m4_len (Undef|Str $string?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('len') );
            return '';
        }
        $self->_checkIgnored( 'len', @ignored );

        $string //= '';
        return length($string);
    }

    method _m4_index (Undef|Str $string?, Undef|Str $substring?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('index') );
            return '';
        }
        if ( Undef->check($substring) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('index') );
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

    method _m4_regexp (Undef|Str $string?, Undef|Str $regexp?, Undef|Str $replacement?, @ignored --> Str) {
        if ( Undef->check($string) || Undef->check($regexp) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('index') );
            return 0;
        }
        $self->_checkIgnored( 'regexp', @ignored );

        $string //= '';

        if ( Str->check($replacement) ) {
            #
            # Expands to the index of first match in string
            #
            my $index = '';
            try {
                if ( $string =~ /$regexp/ ) {
                    $index = $-[0];
                }
                catch {
                    $self->logger_warn( '%s: %s: %s', 'regexp', $regexp, $_ );
                    return;
                }
            }
            return $index;
        }
        else {
            #
            # This should not happen but who knows
            #
            try {
                $string =~ s/$regexp/$replacement/;
            }
            catch {
                $self->logger_warn( '%s: %s', 'regexp', $_ );
                return;
            }
        }
        return $string;
    }

    method _m4_substr (Undef|Str $string?, Undef|Str $from?, Undef|Str $length?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('substr') );
            return '';
        }
        if ( Undef->check($from) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('substr') );
            return $string;
        }
        $self->_checkIgnored( 'substr', @ignored );

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

    method _m4_translit (Undef|Str $string?, Undef|Str $chars?, Undef|Str $replacement?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('translit') );
            return '';
        }
        if ( Undef->check($chars) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('translit') );
            return $string;
        }
        $self->_checkIgnored( 'translit', @ignored );

        $string //= '';
        if ( Undef->check($chars) ) {
            $self->logger_warn( '%s: missing second argument', 'translit' );
            return $string;
        }
        $replacement //= '';

#
# Perl does not like try/catch here and I really had to use eval with runtime error
#
        $_ = $string;
        eval "tr/$chars/$replacement";
        if ($@) {
            $self->logger_warn( '%s: %s', 'translit', $@ );
            return '';
        }

        return $_;
    }

    #
    # Same thing than regexp but with a /g modifier
    #
    method _m4_patsubst (Undef|Str $string?, Undef|Str $regexp?, Undef|Str $replacement?, @ignored --> Str) {
        if ( Undef->check($string) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('patsubst') );
            return '';
        }
        if ( Undef->check($regexp) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('patsubst') );
            return $string;
        }

        $self->_checkIgnored( 'regexp', @ignored );

        $string //= '';

        if ( Undef->check($replacement) ) {
            #
            # Expands to the index of first match in string
            #
            if ( Undef->check($regexp) ) {
                $self->logger_warn( '%s: too few arguments', 'regexp' );
                return '0';
            }
            my $index = '';
            try {
                if ( $string =~ /$regexp/g ) {
                    $index = $-[0];
                }
                catch {
                    $self->logger_warn( '%s: %s: %s', 'regexp', $regexp, $_ );
                    return;
                }
            }
            return $index;
        }
        else {
            #
            # This should not happen but who knows
            #
            if ( Undef->check($regexp) ) {
                $self->logger_warn( '%s: undefined regular expression',
                    'regexp' );
                return '0';
            }
            try {
                $string =~ s/$regexp/$replacement/g;
            }
            catch {
                $self->logger_warn( '%s: %s', 'regexp', $_ );
                return;
            }
        }
        return $string;
    }

    method _m4_format (Undef|Str $format?, Str @arguments --> Str) {
        if ( Undef->check($format) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('format') );
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

    method _m4_incr (Undef|Str $number?, Str @ignored --> Str) {
        if ( Undef->check($number) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('incr') );
            return '';
        }
        $self->_checkIgnored( 'incr', @ignored );
        $number //= 0;
        if ( !Int->check($number) ) {
            $self->logger_error( '%s: %s: does not look like an integer',
                'incr', $number );
            return '';
        }
        my $rc = '';
        if ( $self->_policy_integer_type eq 'native' ) {
            use integer;
            $rc = $number + 1;
        }
        else {
            try {
                my $v1
                    = Bit::Vector->new_Dec( $self->_policy_integer_bits,
                    $number );
                my $v2 = Bit::Vector->new( $self->_policy_integer_bits );
                $v2->inc($v1);
                $rc = $v2->to_Dec();
            }
            catch {
                $self->logger_warn( '%s: %s', 'incr', $! );
                return;
            }
        }
        return $rc;
    }

    method _m4_decr (Undef|Str $number?, Str @ignored --> Str) {
        if ( Undef->check($number) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('decr') );
            return '';
        }
        $self->_checkIgnored( 'decr', @ignored );
        $number //= 0;
        if ( !Int->check($number) ) {
            $self->logger_error( '%s: %s: does not look like an integer',
                'incr', $number );
            return '';
        }
        my $rc = '';
        if ( $self->_policy_integer_type eq 'native' ) {
            use integer;
            $rc = $number - 1;
        }
        else {
            try {
                my $v1
                    = Bit::Vector->new_Dec( $self->_policy_integer_bits,
                    $number );
                my $v2 = Bit::Vector->new( $self->_policy_integer_bits );
                $v2->dec($v1);
                $rc = $v2->to_Dec();
            }
            catch {
                $self->logger_warn( '%s: %s', 'incr', $! );
                return;
            }
        }
        return $rc;
    }

    method _m4_eval (Undef|Str $expression?, Undef|Str $radix?, Undef|Str $width?, Str @ignored --> Str) {
        if ( Undef->check($expression) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('decr') );
            return '';
        }
        $self->_checkIgnored( 'eval', @ignored );

        if ( Undef->check($expression) ) {
            $self->logger_error( '%s: empty string treated  as zero',
                'eval' );
            return 0;
        }
        $radix //= 10;
        if ( !PositiveInt->check($radix) ) {
            $self->logger_error(
                '%s: %s: does not look like a positive integer',
                'eval', $radix );
            return '';
        }
        if ( $radix < 1 || $radix > 36 ) {
            $self->logger_error( '%s: %s: should be in the range [1..36]',
                'eval', $radix );
            return '';
        }
        $width //= 1;
        if ( !PositiveInt->check($radix) ) {
            $self->logger_error(
                '%s: %s: does not look like a positive integer',
                'eval', $radix );
            return '';
        }

        my $rc = '';
        try {
            local $MarpaX::Languages::M4::Impl::GNU::INTEGER_BITS
                = $self->_policy_integer_bits;
            local $MarpaX::Languages::M4::Impl::GNU::SELF = $self;
            my $valuep = $EVAL_G->parse(
                \$expression,
                {   semantics_package =>
                        'MarpaX::Languages::M4::Impl::GNU::Eval::Actions'
                }
            );
            $rc = MarpaX::Languages::M4::Impl::GNU::BaseConversion->to_base(
                $radix, ${$valuep}, $width );
        }
        catch {
            $self->logger_error( '%s: %s', 'eval', $_ );
            return;
        };

        return $rc;
    }

    method _m4_syscmd (Undef|Str $command?, Str @ignored --> Str) {
        if ( Undef->check($command) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('syscmd') );
            return '';
        }
        $self->_checkIgnored( 'syscmd', @ignored );

        $command //= '';
        if ( length($command) > 0 ) {
            my $hashref = run_forked($command);
            if ( HashRef->check($hashref) ) {
                $self->_lastSysExitCode( $hashref->{exit_code} // 0 );
                $self->_diversions_get(-1)->print( $hashref->{stdout} // '' );
                if ( !Undef->check( $hashref->{stderr} ) ) {
                    $self->logger_error( '%s', $hashref->{stderr} );
                }
            }
        }

        return '';
    }

    method _m4_esyscmd (Undef|Str $command?, Str @ignored --> Str) {
        if ( Undef->check($command) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote('esyscmd') );
            return '';
        }
        $self->_checkIgnored( 'esyscmd', @ignored );

        $command //= '';
        if ( length($command) > 0 ) {
            my $hashref = run_forked($command);
            if ( HashRef->check($hashref) ) {
                $self->_lastSysExitCode( $hashref->{exit_code} // 0 );
                if ( !Undef->check( $hashref->{stderr} ) ) {
                    $self->logger_error( '%s', $hashref->{stderr} );
                }
                return $hashref->{stdout} // '';
            }
        }

        return '';
    }

    method _m4_sysval (Str @ignored --> Str) {
        $self->_checkIgnored( 'sysval', @ignored );

        return $self->_lastSysExitCode;
    }

    method _mkstemp (Str $macro, Undef|Str $template?, Str @ignored --> Str) {
        if ( Undef->check($template) ) {
            $self->logger_error( 'too few arguments to builtin %s',
                $self->quote($macro) );
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

        return $self->quote($tmp);
    }

    method _m4_mkstemp (Str @args --> Str) {
        return $self->_mkstemp( 'mkstemp', @args );
    }

    method _m4_maketemp (Str @args --> Str) {
        return $self->_mkstemp( 'maketemp', @args );
    }

    method _m4_errprint (Str @args --> Str) {
        $self->logger_error( '%s', join( ' ', @args ) );
        return '';
    }

    method _m4___file__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__file__', @ignored );
        return $self->__file__;
    }

    method _m4___line__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__line__', @ignored );
        return $self->__line__;
    }

    method _m4___program__ (Str @ignored --> Str) {
        $self->_checkIgnored( '__program__', @ignored );
        return $self->__program__;
    }
#
# $0 is replaced by $name
# arguments are in the form $1, $2, etc... mapped to $args[0], $args[1], etc...
# $# is the number of arguments
# $* is all arguments separated by comma
# $@ is all quoted arguments separated by comma
#
    method _expansion2CodeRef (Str $name, Str $expansion --> CodeRef) {
        my $maxArgumentIndice    = -1;
        my %wantedArgumentIndice = ();
        my $newExpansion         = quotemeta($expansion);
        #
        # Arguments and $0
        #
        $newExpansion =~ s/\\\$([0-9]+)/
      if ($1 > $maxArgumentIndice) {
	$maxArgumentIndice = $1;
      }
      if ($1 == 0) {
	# "\$0";
	quotemeta($name);
      } else {
	my $indice = $1 - 1;
	$wantedArgumentIndice{$indice}++;
	"\$args\[$indice\]";
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
                .= "\tmy \$listArgsQuoted = join(',', map {\$self->quote(\$_)} \@args);\n";
        }
   #
   # Take care: a macro can very well try to access something outside of @args
   # We do this only NOW, because the //= will eventually increase @args
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

    method parseBuffer (Str $input --> PositiveOrZeroInt) {
        return $self->parse($input);
    }

    method appendValue (Str $result) {
        $self->_lastDiversion->print($result);
    }

    method valueRef (--> Ref['SCALAR']) {
        return $self->_diversions_get(0)->sref;
    }

    method value (--> Str) {
        return ${ $self->valueRef };
    }

    with 'MarpaX::Languages::M4::Roles::Impl';
    with 'MooX::Role::Logger';
}
