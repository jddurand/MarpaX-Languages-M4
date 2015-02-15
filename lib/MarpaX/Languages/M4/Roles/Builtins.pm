use Moops;

# PODCLASSNAME

role MarpaX::Languages::M4::Roles::Builtins {
    #
    # Only the POSIX builtins are explicitely listed
    # here. Extensions per implementation are
    # of course allowed.
    #
    requires '_m4_define';
    requires '_m4_undefine';
    requires '_m4_defn';
    requires '_m4_pushdef';
    requires '_m4_popdef';
    requires '_m4_indir';
    requires '_m4_builtin';
    requires '_m4_ifdef';
    requires '_m4_ifelse';
    requires '_m4_dumpdef';
    requires '_m4_traceon';
    requires '_m4_traceoff';
    requires '_m4_debugmode';
    requires '_m4_debugfile';
    requires '_m4_dnl';
    requires '_m4_changequote';
    requires '_m4_changecom';
    requires '_m4_changeword';
    requires '_m4_m4wrap';
    requires '_m4_include';
    requires '_m4_sinclude';
    requires '_m4_divert';
    requires '_m4_undivert';
    requires '_m4_divnum';
    requires '_m4_len';
    requires '_m4_index';
    requires '_m4_regexp';
    requires '_m4_substr';
    requires '_m4_translit';
    requires '_m4_patsubst';
    requires '_m4_format';
    requires '_m4_incr';
    requires '_m4_decr';
    requires '_m4_eval';
    requires '_m4_syscmd';
    requires '_m4_esyscmd';
    requires '_m4_sysval';
    requires '_m4_mkstemp';
    requires '_m4_maketemp';
    requires '_m4_errprint';
    requires '_m4___file__';
    requires '_m4___line__';
    requires '_m4___program__';
}
