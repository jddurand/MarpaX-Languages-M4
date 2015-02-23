use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro Parse Value role

role MarpaX::Languages::M4::Role::Value {

    # VERSION

    requires 'value_concat';
    requires 'value_push';
    requires 'value_elements';
    requires 'value_firstElement';
}

1;
