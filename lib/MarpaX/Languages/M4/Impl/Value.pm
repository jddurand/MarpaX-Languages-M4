use Moops;

# PODCLASSNAME

class MarpaX::Languages::M4::Impl::Value {
  use MarpaX::Languages::M4::Roles::Value;
  use MarpaX::Languages::M4::Types::Macro -all;
  use MarpaX::Languages::M4::Types::Value -all;
  use MooX::HandlesVia;
  use Types::Common::Numeric -all;

  has _value => (is => 'rw',
                 isa => ArrayRef[Str|M4Macro],
                 default => sub { [] },
                 handles_via => 'Array',
                 handles => {
                             _value_unshift => 'unshift',
                             _value_push => 'push',
                             _value_get => 'get',
                             _value_count => 'count',
                             _value_grep => 'grep',
                             _value_elements => 'elements'
                            }
                );

  around BUILDARGS(CodeRef $orig: ClassName $class, @args) {
    if ( @args ) {
      return $class->$orig( _value => \@args );
    }
    else {
      return $class->$orig();
    }
  };

  method elements {
    return $self->_value_elements;
  }

  method firstElement {
    return $self->_value_get(0);
  }

  method unshift (Str|M4Macro @elements --> M4Value) {
    $self->_value_unshift(@elements);
    return $self;
  }

  method push (Str|M4Macro @elements --> M4Value) {
    $self->_value_push(@elements);
    return $self;
  }

  method concat (Undef|M4Macro $macro?, Undef|PositiveOrZeroInt $paramPos? --> M4Value) {
    if ($self->_value_count <= 0) {
      $self->_value(['']);
      return $self;
    }

    if (M4Macro->check($macro)) {
      #
      # If we are providing a macro parameter, then a M4Macro is allowed if
      # it is the first element
      #
      my $firstElement = $self->_value_get(0);
      $paramPos //= 0;
      if (M4Macro->check($firstElement) && $macro->paramCanBeMacro_check($paramPos)) {
        #
        # Return first M4Macro element
        #
        $self->_value([ $firstElement ]);
      } else {
        #
        # Skip all M4Macro elements
        #
        $self->_value([ join('', $self->_value_grep(sub { Str->check($_) })) ]);
      }
    } else {
      #
      # Skip all M4Macro elements
      #
      $self->_value([ join('', $self->_value_grep(sub { Str->check($_) })) ]);
    }

    return $self;
  }

  with 'MarpaX::Languages::M4::Roles::Value';
}
