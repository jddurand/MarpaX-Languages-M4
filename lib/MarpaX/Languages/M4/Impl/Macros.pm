use Moops;

class MarpaX::Languages::M4::Impl::Macros {
    use MarpaX::Languages::M4::Types::Macro -all;
    use MooX::HandlesVia;

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
