use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Input role

role MarpaX::Languages::M4::Role::Input {

    # VERSION

    # AUTHORITY

    requires 'input_push';
    requires 'input_resize';
    requires 'input_rename';
    requires 'input_consumed';
}

1;
