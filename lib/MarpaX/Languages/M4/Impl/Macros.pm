use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro list generic implementation

class MarpaX::Languages::M4::Impl::Macros {
    use MarpaX::Languages::M4::Role::Macros;
    use MarpaX::Languages::M4::Type::Macro -all;
    use MooX::HandlesVia;

    # VERSION

    # AUTHORITY

    has _macrosList => (
        is          => 'rwp',
        isa         => ArrayRef [M4Macro],
        default     => sub { [] },
        handles_via => 'Array',
        handles     => {
            macros_isEmpty  => 'is_empty',
            macros_push     => 'push',
            macros_pop      => 'pop',
            macros_set      => 'set',
            macros_get      => 'get'
        }
    );

    with 'MarpaX::Languages::M4::Role::Macros';
}

1;
