use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro generic implementation

class MarpaX::Languages::M4::Impl::Macro {

    # VERSION

    # AUTHORITY

    use MarpaX::Languages::M4::Role::Macro;
    use MarpaX::Languages::M4::Type::Macro -all;
    use MooX::HandlesVia;
    use Types::Common::Numeric -all;

    has name => (
        is       => 'rw',
        isa      => Str,
        required => 1
    );
    has stub => (
        is          => 'rw',
        isa         => CodeRef,
        required    => 1,
        handles_via => 'Code',
        handles     => { macro_execute => 'execute' }
    );
    has expansion => (
        is       => 'rw',
        isa      => Str | M4Macro,
        required => 1
    );
    has needParams => (
        is      => 'rw',
        isa     => Bool,
        default => false
    );
    has paramCanBeMacro => (
        is      => 'rw',
        isa     => HashRef [ PositiveOrZeroInt | Enum [qw/*/] ],
        default => sub {
            {};
        },
        handles_via => 'Hash',
        handles     => {
            _paramCanBeMacro_get    => 'get',
            _paramCanBeMacro_exists => 'exists'
        }
    );
    has postMatchLength => (
        is      => 'rw',
        isa     => CodeRef,
        default => sub {
            sub { return 0; }
        },
        handles_via => 'Code',
        handles     => { macro_postMatchLengthExecute => 'execute' }
    );

    method macro_name (--> Str) {
        return $self->name;
    }

    method macro_expansion (--> Str | M4Macro) {
        return $self->expansion;
    }

    method macro_needParams (--> Bool ) {
        return $self->needParams;
    }

    method macro_isBuiltin (--> Bool) {
        return M4Macro->check( $self->expansion );
    }

    method macro_paramCanBeMacro (PositiveOrZeroInt $paramPos --> Bool) {
        if ((      $self->_paramCanBeMacro_exists($paramPos)
                && $self->_paramCanBeMacro_get($paramPos)
            )
            || (   $self->_paramCanBeMacro_exists('*')
                && $self->_paramCanBeMacro_get('*') )
            )
        {
            return true;
        }
        else {
            return false;
        }
    }

    method macro_clone (Str $name --> M4Macro) {
        my $macro = __PACKAGE__->new(
            name       => $name,
            stub       => $self->stub,
            needParams => $self->needParams,
            expansion  => $self->expansion,     # But c.f. below
            #
            # No need of clone: the hash is ro once created
            #
            paramCanBeMacro => $self->paramCanBeMacro,
            postMatchLength => $self->postMatchLength
        );
        if ( $self->macro_isBuiltin ) {
            #
            # Avoid circular links
            #
            $macro->expansion($macro);
        }

        return $macro;
    }

    with 'MarpaX::Languages::M4::Role::Macro';
}

1;
