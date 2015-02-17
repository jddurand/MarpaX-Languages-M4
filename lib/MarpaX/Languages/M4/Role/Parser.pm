use Moops;

# PODCLASSNAME

# ABSTRACT: M4 Macro Parser role

role MarpaX::Languages::M4::Role::Parser {

    # VERSION

    requires 'parse_isWord';
    requires 'parse_isComment';
    requires 'parse_isQuotedstring';
    requires 'parse_isMacro';
    requires 'parse_isCharacter';
    requires 'parse_tokensPriority';
    requires 'parse';
}
