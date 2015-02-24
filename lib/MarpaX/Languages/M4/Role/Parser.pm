use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro Parser role

role MarpaX::Languages::M4::Role::Parser {

    # VERSION

    # AUTHORITY

    requires 'parser_isWord';
    requires 'parser_isComment';
    requires 'parser_isQuotedstring';
    requires 'parser_isMacro';
    requires 'parser_isCharacter';
    requires 'parser_tokensPriority';
    requires 'parser_parse';
}

1;
