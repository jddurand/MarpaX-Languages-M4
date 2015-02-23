use Moops;

# PODCLASSNAME

# ABSTRACT: Eval Marpa actions

class MarpaX::Languages::M4::Impl::Default::Eval {
    use Bit::Vector;
    use Types::Common::Numeric -all;
    use MarpaX::Languages::M4::Impl::Default::BaseConversion;

    # VERSION

    #
    # Marpa dislike exceptions throws as objects, because of wanted
    # backward compatibility with very old versions of Perl.
    # So we will use Marpa::R2::Context::bail() method
    #

    has bits => {
        is      => 'ro',
        isa     => PositiveInt,
        default => sub {$MarpaX::Languages::M4::Impl::Default::INTEGER_BITS}
    };

    has SELF => {
        is      => 'ro',
        isa     => ConsumerOf ['MarpaX::Languages::M4::Role::Impl'],
        default => sub {$MarpaX::Languages::M4::Impl::Default::SELF}
    };

    method _eval (ConsumerOf['Bit::Vector'] $expression) {
        return $expression->to_Dec();
    }

    method _invalidOp (Str $op) {
        Marpa::R2::Context::bail( 'Invalid operator in '
                . $self->SELF->impl_quote('eval') . ': '
                . $self->SELF->impl_quote($op) );
    }

    method _noop (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        return $expression;
    }

    method _lneg (Str $op, ConsumerOf['Bit::Vector'] $expression) {
        return Bit::Vector->new_Dec( $self->bits, $expression->is_empty() );
    }

    method _exp (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        if ( $expression2->to_Dec() < 0 ) {
            Marpa::R2::Context::bail( 'Negative exponent in '
                    . $self->SELF->impl_quote('eval') . ': '
                    . $self->SELF->impl_quote( $expression1->to_Dec ) . ' '
                    . $self->SELF->impl_quote($op) . ' '
                    . $self->SELF->impl_quote( $expression2->to_Dec ) );
        }

        if ( $expression1->to_Dec() == 0 && $expression2->to_Dec() == 0 ) {
            Marpa::R2::Context::bail( 'Divide by zero in '
                    . $self->SELF->impl_quote('eval') . ': '
                    . $self->SELF->impl_quote( $expression1->to_Dec ) . ' '
                    . $self->SELF->impl_quote($op) . ' '
                    . $self->SELF->impl_quote( $expression2->to_Dec ) );
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
        try {
            $s->Divide( $expression1, $expression2, $expression1->Shadow );
        }
        catch {
            $s = undef;
        };
        return $s;
    }

    method _mod (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $s = $expression1->Shadow;
        try {
            $expression1->Shadow->Divide( $expression1, $expression2, $s );
        }
        catch {
            $s = undef;
        };
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
        $expression1->Insert( 0, $expression2->to_Dec() % $self->bits );
        return $expression1;
    }

    method _right (ConsumerOf['Bit::Vector'] $expression1, Str $op, ConsumerOf['Bit::Vector'] $expression2) {
        my $u1 = $expression1->Clone;
        if ( $expression1->Sign < 0 ) {
            $u1->Complement($u1);
        }
        $u1->Delete( 0, $expression2->to_Dec() % $self->bits );
        if ( $expression1->Sign < 0 ) {
            $u1->Complement($u1);
        }
        return $u1;
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
        $self->SELF->logger_warn('Warning: recommend == instead of =');
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
        $s->Union( $expression1, $expression2 );
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
            Marpa::R2::Context::bail( 'Undefined right-hand expression in '
                    . $self->SELF->impl_quote('eval') . ': '
                    . $self->SELF->impl_quote( $expression1->to_Dec )
                    . ' '
                    . $self->SELF->impl_quote($op) );
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
            Marpa::R2::Context::bail( 'Undefined right-hand expression in '
                    . $self->SELF->impl_quote('eval') . ': '
                    . $self->SELF->impl_quote( $expression1->to_Dec )
                    . ' '
                    . $self->SELF->impl_quote($op) );
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
        return Bit::Vector->new_Dec( $self->bits, oct($lexeme) );
    }

    # oct() supportx 0x notation
    method _hex (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, oct($lexeme) );
    }

    # oct() supportx 0b notation
    method _binary (Str $lexeme) {
        return Bit::Vector->new_Dec( $self->bits, oct($lexeme) );
    }

    method _radix (Str $lexeme) {
                      #
                      # Per def this is this regexp
                      #
        $lexeme =~ /0r([\d]+):([\da-zA-Z]+)/;
        my $radix = substr( $lexeme, $-[1], $+[1] - $-[1] );
        my $digits = lc( substr( $lexeme, $-[2], $+[2] - $-[2] ) )
            ;         # Because max base is 36
        if ( $radix == 1 ) {

            # For radix 1, leading zeros are ignored,
            # and all remaining digits must be 1
            $digits =~ s/^0*//;
            if ( $digits =~ /[^1]/ ) {
                Marpa::R2::Context::bail(
                    $self->SELF->impl_quote($lexeme) . ': '
                        . 'for radix 1, digits must be eventual zeroes followed by 1\'s'
                );
            }
        }
        return Bit::Vector->new_Dec(
            $self->bits,
            MarpaX::Languages::M4::Impl::Default::BaseConversion->fr_base(
                $radix, $digits
            )
        );
    }

}

1;
