use Moops;

# PODCLASSNAME

class MarpaX::Languages::M4::Impl::Macro {
    use MarpaX::Languages::M4::Roles::Macro;
    use MarpaX::Languages::M4::Types::Macro -all;
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
        handles     => { execute => 'execute' }
    );
    has expansion => (
        is       => 'rw',
        isa      => Str|M4Macro,
        required => 1
    );
    has needParams => (
        is      => 'rw',
        isa     => Bool,
        default => false
    );
    has paramCanBeMacro => (
        is      => 'rw',
        isa     => HashRef [PositiveOrZeroInt|Enum[qw/*/]],
        default => sub {
            {};
        },
        handles_via => 'Hash',
        handles     => {
            paramCanBeMacro_get    => 'get',
            paramCanBeMacro_exists => 'exists'
        }
    );
    has postMatchLength => (
        is      => 'rw',
        isa     => CodeRef,
        default => sub {
            sub { return 0; }
        },
        handles_via => 'Code',
        handles     => { postMatchLength_execute => 'execute' }
    );

    method is_builtin(--> Bool) {
      return ($self->expansion == $self) ? true : false;
    }

    method paramCanBeMacro_check(PositiveOrZeroInt $paramPos --> Bool) {
      if (($self->paramCanBeMacro_exists($paramPos) && $self->paramCanBeMacro_get($paramPos))
          ||
          ($self->paramCanBeMacro_exists('*') &&  $self->paramCanBeMacro_get('*'))
         ) {
        return true;
      } else {
        return false;
      }
    }

    with 'MarpaX::Languages::M4::Roles::Macro';
}