use Moops;

# PODCLASSNAME

# ABSTRACT: M4 implementation role

role MarpaX::Languages::M4::Role::Impl {

    # VERSION

    use MarpaX::Languages::M4::Role::Builtin;
    use MarpaX::Languages::M4::Role::Logger;
    use MarpaX::Languages::M4::Role::Parser;

    requires 'impl_quote';
    requires 'impl_unquote';
    requires 'impl_appendValue';
    requires 'impl_value';
    requires 'impl_valueRef';
    requires 'impl_parseBuffers';
    requires 'impl_pos';
    requires 'impl_eof';

    with 'MarpaX::Languages::M4::Role::Builtin';
    with 'MarpaX::Languages::M4::Role::Logger';
    with 'MarpaX::Languages::M4::Role::Parser';
}
