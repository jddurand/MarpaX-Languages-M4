use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Token type

library MarpaX::Languages::M4::Type::Token declares M4Token {

    # VERSION

    # AUTHORITY

    declare M4Token, as Enum [qw/COMMENT QUOTEDSTRING WORD CHARACTER/];
}

1;
