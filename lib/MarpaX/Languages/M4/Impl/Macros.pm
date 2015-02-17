use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro generic implementation

class MarpaX::Languages::M4::Impl::Macros {
    use MarpaX::Languages::M4::Type::Macro -all;
    use MooX::HandlesVia;

    # VERSION

    has _macrosList => (
        is          => 'rwp',
        isa         => ArrayRef [M4Macro],
        default     => sub { [] },
        handles_via => 'Array',
        handles     => {
            is_empty => 'is_empty',
            push     => 'push',
            pop      => 'pop',
            set      => 'set',
            get      => 'get'
        }
    );
}
