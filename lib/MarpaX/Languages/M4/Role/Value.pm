use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro Parse Value role

role MarpaX::Languages::M4::Role::Value {

    # VERSION

    requires 'concat';
    requires 'push';
    requires 'unshift';
    requires 'elements';
    requires 'firstElement';
}
