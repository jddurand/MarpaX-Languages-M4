use Moops;

# PODCLASSNAME

role MarpaX::Languages::M4::Roles::Impl {
    use MarpaX::Languages::M4::Roles::Builtins;
    use MarpaX::Languages::M4::Roles::Logger;
    use MarpaX::Languages::M4::Roles::Parser;

    requires 'quote';
    requires 'unquote';
    requires 'appendValue';
    requires 'value';
    requires 'valueRef';

    with 'MarpaX::Languages::M4::Roles::Builtins';
    with 'MarpaX::Languages::M4::Roles::Logger';
    with 'MarpaX::Languages::M4::Roles::Parser';
}
