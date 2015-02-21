divert(`-1')
# upcase(text)
# downcase(text)
# capitalize(text)
#   change case of text, simple version
# Version using perl regexp
define(`upcase', `translit(`$*', `a-z', `A-Z')')
define(`downcase', `translit(`$*', `A-Z', `a-z')')
#
# Using ${x} notation is highly recommended, to avoid
# mis-interpretation of $x as a macro argument
#
define(`_capitalize',
       `regexp(`$1', `^(\w)(\w*)',
               `upcase(`${1}')`'downcase(`${2}')')')
define(`capitalize', `patsubst(`$1', `\w+', `_$0(`${&}')')')
divert`'dnl
