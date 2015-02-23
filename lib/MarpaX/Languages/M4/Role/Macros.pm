use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macros role

role MarpaX::Languages::M4::Role::Macros {

    # VERSION

    requires 'macros_isEmpty';
    requires 'macros_push';
    requires 'macros_pop';
    requires 'macros_set';
    requires 'macros_get';
}

1;
