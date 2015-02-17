use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Logger role

role MarpaX::Languages::M4::Role::Logger {

    # VERSION

    requires 'logger_error';
    requires 'logger_warn';
    requires 'logger_debug';
}
