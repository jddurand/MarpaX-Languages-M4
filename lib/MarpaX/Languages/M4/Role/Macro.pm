use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro role

role MarpaX::Languages::M4::Role::Macro {

    # VERSION

    requires 'name';
    requires 'execute';
    requires 'expansion';
    requires 'needParams';
    requires 'paramCanBeMacro_get';
    requires 'paramCanBeMacro_exists';
    requires 'paramCanBeMacro_check';
    requires 'postMatchLength_execute';
    requires 'is_builtin';
}
