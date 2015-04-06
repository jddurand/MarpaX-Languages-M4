use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macros role

role MarpaX::Languages::M4::Role::Macros {

    # VERSION

    # AUTHORITY

    requires 'macros_isEmpty';
    requires 'macros_push';
    requires 'macros_pop';
    requires 'macros_set';
    requires 'macros_get';
    requires 'macros_elements';
}

1;
