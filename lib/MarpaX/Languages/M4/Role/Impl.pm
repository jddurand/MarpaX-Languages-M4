use Moops;

# PODCLASSNAME

# ABSTRACT: M4 implementation role

role MarpaX::Languages::M4::Role::Impl {

    # VERSION

    use MarpaX::Languages::M4::Role::Builtins;
    use MarpaX::Languages::M4::Role::Logger;
    use MarpaX::Languages::M4::Role::Parser;

    requires 'quote';
    requires 'unquote';
    requires 'appendValue';
    requires 'value';
    requires 'valueRef';
    requires 'eof';

    with 'MarpaX::Languages::M4::Role::Builtins';
    with 'MarpaX::Languages::M4::Role::Logger';
    with 'MarpaX::Languages::M4::Role::Parser';
}
