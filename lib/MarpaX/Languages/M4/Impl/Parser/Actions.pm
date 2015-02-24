use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Marpa parser actions

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

1;
