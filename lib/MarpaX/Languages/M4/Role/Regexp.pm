use Moops;

# PODCLASSNAME

# ABSTRACT: M4 regexp role

role MarpaX::Languages::M4::Role::Regexp {

    # VERSION

    # AUTHORITY

    requires 'regexp_syntax';
    requires 'regexp_pattern';
    requires 'regexp_errstr';
    requires 'regexp_comp';
    requires 'regexp_exec';
    requires 'regexp_matches';
}

1;
