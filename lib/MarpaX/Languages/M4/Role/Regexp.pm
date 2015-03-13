use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Regexp role

role MarpaX::Languages::M4::Role::Regexp {

    # VERSION

    # AUTHORITY

    requires 'regexp_compile';
    requires 'regexp_lpos';
    requires 'regexp_lpos_count';
    requires 'regexp_lpos_get';
    requires 'regexp_rpos';
    requires 'regexp_rpos_count';
    requires 'regexp_rpos_get';
    requires 'regexp_exec';
    requires 'regexp_substitute';
}

1;
