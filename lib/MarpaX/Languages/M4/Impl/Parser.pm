use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro Parser Marpa actions

class MarpaX::Languages::M4::Impl::Parser::Actions {
    use MarpaX::Languages::M4::Impl::Value;
    use MarpaX::Languages::M4::Type::Value -all;
    use MarpaX::Languages::M4::Type::Macro -all;
    use Types::Common::Numeric -all;

    # VERSION

    has macro => (
        is  => 'rw',
        isa => Undef | M4Macro,
        #
        # In a sub because it has to re-evaluated at every new()
        #
        default => sub {$MarpaX::Languages::M4::Impl::Parser::macro}
    );

    has paramPos => (
        is      => 'rw',
        isa     => PositiveOrZeroInt,
        default => 0
    );

    method createArgumentsGroup (Undef|ConsumerOf[M4Value] $tokens?) {
        if ( !Undef->check($tokens) ) {
            return $tokens;
        }
        else {
            return MarpaX::Languages::M4::Impl::Value->new();
        }
    }

    method mergeArgumentsGroup (ConsumerOf[M4Value] $argumentsGroupLeft, Str $lparen, ConsumerOf[M4Value] $argumentsGroupMiddle, Str $rparen, ConsumerOf[M4Value] $argumentsGroupRight) {
        return MarpaX::Languages::M4::Impl::Value->new(
            $argumentsGroupLeft->_value_elements,   $lparen,
            $argumentsGroupMiddle->_value_elements, $rparen,
            $argumentsGroupRight->_value_elements
        );
    }

    method create (Str|M4Macro @lexemes --> ConsumerOf[M4Value]) {
        return MarpaX::Languages::M4::Impl::Value->new(@lexemes);
    }

    method fakeOneVoidParam (--> ConsumerOf[M4Value]) {
        return MarpaX::Languages::M4::Impl::Value->new('');
    }

    method firstArg (Undef|ConsumerOf[M4Value] $argumentsGroup? --> ConsumerOf[M4Value]) {
                                  #
                                  # $argumentsGroup is nullable
                                  #
        if ( Undef->check($argumentsGroup) ) {
            return MarpaX::Languages::M4::Impl::Value->new('');
        }
        else {
            return $argumentsGroup->value_concat( $self->macro,
                $self->paramPos );
        }
    }

    method nextArg (ConsumerOf[M4Value] $arguments, Undef|ConsumerOf[M4Value] $argumentsGroup) {
        $self->paramPos( $self->paramPos + 1 );
        #
        # $argumentsGroup is nullable
        #
        if ( !Undef->check($argumentsGroup) ) {
            $argumentsGroup->value_concat( $self->macro, $self->paramPos );
            return $arguments->value_push( $argumentsGroup->value_elements )
                ;    # Per def there is one element
        }
        else {
            return $arguments->value_push('');  # Per def there is one element
        }
    }

}

#
# Expose only the base implementation of parse() method
# required by MarpaX::Languages::M4::Role::Parser
#
class MarpaX::Languages::M4::Impl::Parser {
    use MarpaX::Languages::M4::Type::Macro -all;
    use MarpaX::Languages::M4::Impl::Value;
    use Marpa::R2;                               # 2.103_004;
    use Scalar::Util qw/readonly/;
    use Types::Common::Numeric -all;
    use Throwable::Factory NoLexeme => undef;

    our $BASE_GRAMMAR = q{
inaccessible is ok by default
:default ::= action => ::first
lexeme default = latm => 1

#
# Note: this part of the grammar is giving ambiguity
#       but we do not mind because the effect is always
#       the same: global concatenatation
#
argumentsGroup ::= tokens                                                      action => createArgumentsGroup rank => 1
                 | argumentsGroup LPAREN argumentsGroup RPAREN  argumentsGroup action => mergeArgumentsGroup

arguments ::= argumentsGroup                   action => firstArg
            | arguments (COMMA) argumentsGroup action => nextArg

tokens ::= token*                      action => create
event '^token' = predicted <token>     # This event is to force $r->read() to not advance its location
event 'token$' = completed <token>
token ::= WORD
        | macro
        | QUOTEDSTRING
        | CHARACTER
        | COMMENT
        | ANYTHING

macro ::= MACRO                        # Never lexeme_read'ed
        | MACRO_ARGS macroArguments    # Ditto

event 'macroArguments$' = completed <macroArguments>
macroArguments ::= (LPAREN) arguments (RPAREN)
macroArguments ::= (NOPARAM)           action => fakeOneVoidParam

_ANYTHING    ~ [\s\S]
_NOTHING     ~ [^\s\S]
WORD         ~ _NOTHING
MACRO        ~ _NOTHING
MACRO_ARGS   ~ _NOTHING
QUOTEDSTRING ~ _NOTHING
CHARACTER    ~ _NOTHING
COMMENT      ~ _NOTHING
#
# ANYTHING will automatically eat a character each time we resume the SLIF: we
# do not want this to happen
#
:lexeme ~ <ANYTHING> pause => before event => '^ANYTHING'
ANYTHING     ~ _ANYTHING

_WS ~ [\s]
_WS_any ~ _WS*

:lexeme ~ <LPAREN> priority => 1 pause => before event => '^LPAREN'
LPAREN ~ '(' _WS_any

:lexeme ~ <RPAREN> priority => 1 pause => before event => '^RPAREN'
RPAREN ~ ')'

:lexeme ~ <NOPARAM> priority => 1
NOPARAM ~ '(' _WS_any ')'

#
# Quote from the spec (M4 POSIX):
# If a macro name is followed by a <left-parenthesis>, its arguments are the <comma>-separated tokens
# between the <left-parenthesis> and the matching <right-parenthesis>. Unquoted white-space characters
# preceding each argument shall be ignored. All other characters, including trailing white-space characters,
# are retained.
# <comma> characters enclosed between <left-parenthesis> and <right-parenthesis> characters do not delimit arguments.

:lexeme ~ <COMMA> priority => 1
COMMA ~ ',' _WS_any
};

    our $BYTOKEN_G = do {
        my $g = Marpa::R2::Scanless::G->new(
            {   source => \do {":start ::= tokens\n$BASE_GRAMMAR"}
            }
        );
    };
    our $BYMACROARGUMENTS_G = do {
        my $g = Marpa::R2::Scanless::G->new(
            {   source => \do {":start ::= macroArguments\n$BASE_GRAMMAR"}
            }
        );
    };

    has _parse_level => (
        is      => 'rwp',
        isa     => PositiveOrZeroInt,
        default => 0
    );

    method parser_parse (Str $input --> Str) {
            #
            # We will modify the buffer in-place, therefore we want to
            # have OUR version of the input.
            # In addition, this will solve the eventual problem of $input
            # being a read-only variable
            #
        my $work = $input;
        return $self->_parseByTokens( \$work );
    }

 #
 # By token => ConsumerOf['MarpaX::Languages::M4::Role::Value by default']
 # By arguments =>  ArrayRef[ConsumerOf['MarpaX::Languages::M4::Role::Value']]
 #
    method _parseByGrammar (Ref['SCALAR'] $inputRef, PositiveOrZeroInt $pos, InstanceOf['Marpa::R2::Scanless::G'] $g, Undef|M4Macro $macro? --> Undef|Dict[pos => PositiveOrZeroInt, value => ConsumerOf['MarpaX::Languages::M4::Role::Value']]) {

        my $maxPos = length( ${$inputRef} ) - 1;
        #
        # Protect the case of empty string
        #
        if ( $pos > $maxPos ) {
            return undef;
        }
        #
        # Get the lexemes ordering
        #
        my @lexemeNames = $self->parser_tokensPriority();
        #
        # In the context of a macroArguments, unquoted parenthesis
        # have higher priorities over everything, except quoted string
        # and comment.
        #
        if ( $g == $BYMACROARGUMENTS_G ) {
            unshift( @lexemeNames, 'NOPARAM', 'LPAREN', 'RPAREN', 'COMMA' );
        }

        my $value = MarpaX::Languages::M4::Impl::Value->new();
        my %rc = ( pos => $pos, value => $value );

        #
        # Instanciate and start recognizer
        #
        my $r = Marpa::R2::Scanless::R->new(
            {   grammar => $g,
                semantics_package =>
                    sprintf( '%s::%s', __PACKAGE__, 'Actions' ),
                exhaustion => 'event',

                # trace_terminals => 1,
                # trace_values => 1
            }
        );
        #
        # $prevPos is used only for logging
        #
        my $prevPos = $rc{pos} = $pos;
        #
        # The stream itself is of no importance.
        # We use Marpa to drive the context, nothing more.
        # But is it NOT a hasard that I use the constant '(':
        # when doing a recursive call using
        # the macroByarguments grammar, Marpa will expect this
        # character at the first read, EVEN
        # if we paused before it.
        #
        $r->read( \'(' );

    again:
        while ( $rc{pos} <= $maxPos ) {

            # $self->logger_debug( '[%d..%d/%d] 20 first characters: %s',
            #     $rc{pos}, $rc{pos}, $maxPos,
            #     substr( ${$inputRef}, $rc{pos}, 20 ) );

            my %expected = map { $_ => 1 } @{ $r->terminals_expected };

            # $self->logger_debug(
            #    '[%d..%d/%d] Expected terminals: %s',
            #    $rc{pos}, $rc{pos}, $maxPos,
            #    [ keys %expected ]
            # );

            if ( $r->exhausted ) {

                # $self->logger_debug( '[%d..%d/%d] Parse is exhausted',
                #     $rc{pos}, $rc{pos}, $maxPos );

                last;
            }

            my $lexemeValue;
            my $lexemeLength;
            my $lexeme;

            my $QuotedstringValue;
            my $QuotedstringLength;
            my $isQuotedString = false;

            my $CommentValue;
            my $CommentLength;
            my $isComment = false;

            my $canCollectArguments = true;
            if ( $g == $BYMACROARGUMENTS_G ) {
                $isQuotedString
                    = $self->parser_isQuotedstring( ${$inputRef}, $rc{pos},
                    $maxPos, \$QuotedstringValue, \$QuotedstringLength );
                $isComment = $self->parser_isComment( ${$inputRef}, $rc{pos},
                    $maxPos, \$CommentValue, \$CommentLength );
                $canCollectArguments = !$isQuotedString && !$isComment;
            }

            foreach (@lexemeNames) {
                if ( $_ eq 'NOPARAM' ) {
                    if ( exists( $expected{NOPARAM} )
                        && $canCollectArguments )
                    {
                        pos( ${$inputRef} ) = $rc{pos};
                        if ( ${$inputRef} =~ /\G\(\s*\)/s ) {
                            $lexeme = 'NOPARAM';
                            $lexemeValue = substr( ${$inputRef}, $-[0],
                                $+[0] - $-[0] );
                            $lexemeLength = $+[0] - $-[0];
                            last;
                        }
                    }
                }
                elsif ( $_ eq 'LPAREN' ) {
                    if (   exists( $expected{LPAREN} )
                        && $canCollectArguments
                        && substr( ${$inputRef}, $rc{pos}, 1 ) eq '(' )
                    {
                        $lexeme       = 'LPAREN';
                        $lexemeValue  = '(';
                        $lexemeLength = 1;
                        last;
                    }
                }
                elsif ( $_ eq 'RPAREN' ) {
                    if (   exists( $expected{RPAREN} )
                        && $canCollectArguments
                        && substr( ${$inputRef}, $rc{pos}, 1 ) eq ')' )
                    {
                        $lexeme       = 'RPAREN';
                        $lexemeValue  = ')';
                        $lexemeLength = 1;
                        last;
                    }
                }
                elsif ( $_ eq 'COMMA' ) {
                    if ( exists( $expected{COMMA} ) && $canCollectArguments )
                    {
                        pos( ${$inputRef} ) = $rc{pos};
                        if ( ${$inputRef} =~ /\G,\s*/s ) {
                            $lexeme = 'COMMA';
                            $lexemeValue = substr( ${$inputRef}, $-[0],
                                $+[0] - $-[0] );
                            $lexemeLength = $+[0] - $-[0];
                            last;
                        }
                    }
                }
                else {
                    if ( $g == $BYMACROARGUMENTS_G && $_ eq 'QUOTEDSTRING' ) {
                      #
                      # Already done in the context of macro arguments grammar
                      #
                        if ($isQuotedString) {
                            $lexeme       = $_;
                            $lexemeValue  = $QuotedstringValue;
                            $lexemeLength = $QuotedstringLength;
                            last;
                        }
                    }
                    elsif ( $g == $BYMACROARGUMENTS_G && $_ eq 'COMMENT' ) {
                      #
                      # Already done in the context of macro arguments grammar
                      #
                        if ($isComment) {
                            $lexeme       = $_;
                            $lexemeValue  = $CommentValue;
                            $lexemeLength = $CommentLength;
                            last;
                        }
                    }
                    else {
                        #
                        # QUOTEDSTRING is not already done if not
                        # in the context of macro arguments grammar
                        # Ditto for COMMENT.
                        #
                        my $method = 'parser_is' . ucfirst( lc($_) );
                        if ($self->$method(
                                ${$inputRef}, $rc{pos},
                                $maxPos,      \$lexemeValue,
                                \$lexemeLength
                            )
                            )
                        {
                            $lexeme = $_;
                            last;
                        }
                    }
                }
            }
            #
            # Nothing ?
            #
            if ( Undef->check($lexeme) ) {
                NoLexeme->throw(
                    "No token found at position $rc{pos} (20 first characters): "
                        . substr( ${$inputRef}, $rc{pos}, 20 ) );
            }
            #
            # If it is a word, check if this is eventually a macro
            #
            if ( $lexeme eq 'WORD' ) {
                my $thisMacro;
                my $lparenPos;
                if ($self->parser_isMacro(
                        ${$inputRef}, $rc{pos},      $maxPos,
                        $lexemeValue, $lexemeLength, \$thisMacro,
                        \$lparenPos
                    )
                    )
                {

                    my $macroName = $lexemeValue;
                    my $printableMacroName
                        = $self->_printable( $macroName, true );

                    # $self->logger_debug(
                    #     '[%d..%d/%d] %s is an acceptable macro call',
                    #     $rc{pos}, $rc{pos}, $maxPos, $lexemeValue );
                    $self->logger_debug( '[%d..%d/%d] %s ...',
                        $rc{pos}, $rc{pos}, $maxPos, $printableMacroName );

                    if ( $lparenPos < 0 ) {
                        #
                        # Execute the macro without argument
                        #
                        $self->logger_debug( '[%d..%d/%d] %s -> ???',
                            $rc{pos}, $rc{pos}, $maxPos,
                            $printableMacroName );
                        $lexemeValue = $thisMacro->macro_execute($self);
                        if ( length($lexemeValue) > 0 ) {
                            $self->logger_debug(
                                '[%d..%d/%d] %s -> %s',
                                $rc{pos},
                                $rc{pos},
                                $maxPos,
                                $printableMacroName,
                                $self->_printable($lexemeValue)
                            );
                        }
                        else {
                            $self->logger_debug( '[%d..%d/%d] %s',
                                $rc{pos}, $rc{pos}, $maxPos,
                                $printableMacroName );
                        }
                    }
                    else {
                        #
                        # Call us recursively.
                        # This will change $lexemeValue and next position
                        #
                        my $afterRparenPos;
                        $self->_set__parse_level( $self->_parse_level + 1 );
                        my $dict = $self->_parseByGrammar(
                            $inputRef,           $lparenPos,
                            $BYMACROARGUMENTS_G, $thisMacro
                        );
                        $self->_set__parse_level( $self->_parse_level - 1 );
                        #
                        # This will croak if dict is not defined
                        #

                        # $self->logger_debug( '[%d..%d/%d] <= %s',
                        #     $rc{pos}, $rc{pos}, $maxPos, $dict );

                        my $parametersValue = $dict->{value};
                        $afterRparenPos = $dict->{pos};
                        #
                        # Execute the macro
                        #
                        my $printableArguments = join( ', ',
                            map { $self->_printable($_) }
                                $parametersValue->value_elements );
                        $self->logger_debug(
                            '[%d..%d/%d] %s(%s) -> ???',
                            $rc{pos},
                            $rc{pos},
                            $maxPos,
                            $printableMacroName,
                            $printableArguments
                        );
                        $lexemeValue = $thisMacro->macro_execute( $self,
                            $parametersValue->value_elements );
                        if ( length($lexemeValue) > 0 ) {
                            $self->logger_debug(
                                '[%d..%d/%d] %s(...) -> %s',
                                $rc{pos},
                                $rc{pos},
                                $maxPos,
                                $printableMacroName,
                                $self->_printable($lexemeValue)
                            );
                        }
                        else {
                            $self->logger_debug( '[%d..%d/%d] %s(...)',
                                $rc{pos}, $rc{pos}, $maxPos,
                                $printableMacroName );
                        }
                        $lexemeLength = $afterRparenPos - $rc{pos};
                    }
                    #
                    # Eventual postmatch length
                    #
                    $lexemeLength
                        += $thisMacro->macro_postMatchLengthExecute( $self,
                        ${$inputRef}, $rc{pos} + $lexemeLength, $maxPos );
                    #
                    # Input is changing
                    #
                    if ( M4Macro->check($lexemeValue) ) {
                        #
                        # Protect the case of M4Macro
                        #
                        $lexeme = 'ANYTHING';
                    }
                    else {
                       # $self->logger_debug(
                       #    '[%d..%d/%d] Input is changing: replace %s by %s',
                       #    $rc{pos},
                       #    $rc{pos},
                       #     $maxPos,
                       #     substr(
                       #         ${$inputRef}, $rc{pos}, $lexemeLength
                       #     ),
                       #     $lexemeValue
                       # );
                        substr(
                            ${$inputRef},  $rc{pos},
                            $lexemeLength, $lexemeValue
                        );

                        # $self->logger_debug( '[%d..%d/%d] New input: %s',
                        #     $rc{pos}, $rc{pos}, $maxPos, ${$inputRef} );

                        $maxPos = length( ${$inputRef} ) - 1;
                        #
                        # Protect the case of empty string
                        #
                        if ( $rc{pos} > $maxPos ) {
                            return undef;
                        }
                        goto again;
                    }
                }
                else {
                    # $self->logger_debug(
                    #     '[%d..%d/%d] %s is not an acceptable macro call',
                    #     $rc{pos}, $rc{pos}, $maxPos, $lexemeValue );
                }
            }
            #
            # When _parse_level is zero, the current token has been fully
            # parsed, and its output is to
            # to be immediately "flushed".
            # We do not need to do a lexeme_read(), nor a resume in this case:
            # we know where we are.
            #
            if ( $self->_parse_level > 0 ) {
                $r->lexeme_read( $lexeme, 0, 1, $lexemeValue );
                $prevPos = $rc{pos};
                $rc{pos} += $lexemeLength;

                # $self->logger_debug( '[%d->%d/%d] %s: %s',
                #     $prevPos, $rc{pos}, $maxPos, $lexeme, $lexemeValue );

                #
                # We can safely ignore the events from lexeme_read(),
                # because we made sure in the grammar that resume() will
                # NOT advance the position, thanks to those events:
                #
                # ^token
                # ^ANYTHING
                # macroArguments$
                # token$
                #
                # macroArguments$, if it happens will always be standalone.
                # token$, when it happen, can be mixed with ^token or
                # ^ANYTHING.
                #
                # Please note that we do not use resume().
                #
            }
            else {
                my $tmpValue = MarpaX::Languages::M4::Impl::Value->new()
                    ->value_push($lexemeValue);
                $self->impl_appendValue(
                    $tmpValue->value_concat->value_firstElement );
                $prevPos = $rc{pos};
                $rc{pos} += $lexemeLength;
            }
        }

        if ( $self->_parse_level > 0 ) {
            #
            # We are in the context of a recursive call: the output
            # is of concern for a macro that called us.
            #
            local $MarpaX::Languages::M4::Impl::Parser::macro = $macro;
            $rc{value} = ${ $r->value };
        }
        else {
            #
            # We are at the top level: the output has already been
            # "flushed" to whatever the current diversion said to.
            #
        }

        # $self->logger_debug( '[%d..%d/%d] => %s',
        #     $rc{pos}, $rc{pos}, $maxPos, \%rc );

        return \%rc;
    }

    #
    # M4 says that a token is processed as soon as it is recognized.
    # So we loop on token recognition
    #
    method _parseByTokens (Ref['SCALAR'] $inputRef --> Str) {

        my $rc = $self->_parseByGrammar( $inputRef, 0, $BYTOKEN_G );
        if ( !Undef->check($rc) ) {
            return substr( ${$inputRef}, $rc->{pos} );
        }

        return ${$inputRef};
    }

    method _printable (Str|M4Macro $input, Bool $noQuote? --> Str) {
        $noQuote //= false;
        #
        # If M4Macro let's get the object representation stringified
        #
        my $printable = Str->check($input) ? $input : "$input";

        # $printable =~ s/([^[:print:]])/sprintf('0x%x', ord($1))/eg;
        return Str->check($input)
            ? ( $noQuote ? $printable : $self->impl_quote($printable) )
            : $printable;
    }

}

1;
