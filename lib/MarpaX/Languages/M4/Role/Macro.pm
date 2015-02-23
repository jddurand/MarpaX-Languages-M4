use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro role

role MarpaX::Languages::M4::Role::Macro {

    # VERSION

    requires 'macro_name';
    requires 'macro_execute';
    requires 'macro_expansion';
    requires 'macro_needParams';
    requires 'macro_paramCanBeMacro';
    requires 'macro_postMatchLengthExecute';
    requires 'macro_isBuiltin';
}

1;
