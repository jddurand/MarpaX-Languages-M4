use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Regexp type

library MarpaX::Languages::M4::Type::Regexp declares M4Regexp, M4RegexpEngine, M4RegexpReplacement {

    # VERSION

    # AUTHORITY

    declare M4Regexp, as ConsumerOf ['MarpaX::Languages::M4::Role::Regexp'];
    declare M4RegexpEngine, as Enum[qw/GNU perl/];
    declare M4RegexpReplacement, as Enum[qw/GNU GNUext perl/];
}

1;
