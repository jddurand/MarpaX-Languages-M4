#!perl
use strict;
use warnings FATAL   => 'all';
use Test::More;
use Log::Handler;
use Log::Any::Adapter;
use Data::Section -setup => {
  header_re => qr/
    \A                # start
      _+!             # __!
        \s*           # any whitespace
          ([^!]+?)    # this is the actual name of the section
        \s*           # any whitespace
      !_+             # !__
      [\x0d\x0a]{1,2} # possible cariage return for windows files
    \z                # end
  /x};

my $log = Log::Handler->new(
    screen => {
        log_to   => "STDOUT",
        maxlevel => "debug",
        minlevel => "error",
        dateformat     => "%Y-%m-%d",
        timeformat     => "%H:%M:%S",
        message_layout => "%D %T %L %m",
    }
    );
Log::Any::Adapter->set('Handler', logger => $log);

BEGIN {
    use_ok('MarpaX::Languages::M4::Impl::GNU')
        || print "Bail out!\n";
}

foreach (grep {/: input/} sort {$a cmp $b} __PACKAGE__->section_data_names) {
  my $testName = $_;
  my $inputRef = __PACKAGE__->section_data($testName);
  my $init = sub {};
  if ($testName =~ /: input\((.+)\)/) {
    $init = eval "sub $1";
  }
  $testName =~ s/: input.*//;
  my $outputRef = __PACKAGE__->section_data("$testName: output");

  my $input = ${$inputRef};
  my $gnu = MarpaX::Languages::M4::Impl::GNU->new();
  $gnu->$init;
  my $pos = $gnu->parseBuffer($input);
  my $got = $gnu->value;
  my $expected = ${$outputRef};

  cmp_ok($got, 'eq', $expected, $testName);
}
done_testing();

__DATA__
__! 001 empty input: input !__
__! 001 empty input: output !__
__! 002 define - Hello World: input !__
define(`foo', `Hello world.')
foo
__! 002 define - Hello World: output !__

Hello world.
__! 003 define - Composites array/array_set: input !__
define(`array', `defn(format(``array[%d]'', `$1'))')
define(`array_set', `define(format(``array[%d]'', `$1'), `$2')')
array_set(`4', `array element no. 4')
array_set(`17', `array element no. 17')
array(`4')
array(eval(`10 + 7'))
__! 003 define - Composites array/array_set: output !__




array element no. 4
array element no. 17
__! 004 define - Composite exch: input !__
define(`exch', `$2, $1')
exch(`arg1', `arg2')
__! 004 define - Composite exch: output !__

arg2, arg1
__! 005 define - Composite exch on define: input !__
define(`exch', `$2, $1')
define(exch(``expansion text'', ``macro''))
macro
__! 005 define - Composite exch on define: output !__


expansion text
__! 006 define - $0: input !__
define(`test', ``Macro name: $0'')
test
__! 006 define - $0: output !__

Macro name: test
__! 007 define - Quoted text: input !__
define(`foo', `This is macro `foo'.')
foo
__! 007 define - Quoted text: output !__

This is macro foo.
__! 008 define - ${: input !__
define(`foo', `single quoted $`'{1} output')
define(`bar', ``double quoted $'`{2} output'')
foo(`a', `b')
bar(`a', `b')
__! 008 define - ${: output !__


single quoted ${1} output
double quoted ${2} output
__! 009 define - Composite nargs: input !__
define(`nargs', `$#')
nargs
nargs()
nargs(`arg1', `arg2', `arg3')
nargs(`commas can be quoted, like this')
nargs(arg1#inside comments, commas do not separate arguments
still arg1)
nargs((unquoted parentheses, like this, group arguments))
__! 009 define - Composite nargs: output !__

0
1
3
1
1
1
__! 010 define - #: input !__
dnl Attempt to define a macro to just `$#'
define(underquoted, $#)
oops)
underquoted
__! 010 define - #: output !__

0)
oops
__! 011 define - $*: input !__
define(`echo', `$*')
echo(arg1,    arg2, arg3 , arg4)
__! 011 define - $*: output !__

arg1,arg2,arg3 ,arg4
__! 012 define - $@: input !__
define(`echo', `$@')
echo(arg1,    arg2, arg3 , arg4)
__! 012 define - $@: output !__

arg1,arg2,arg3 ,arg4
__! 013 define - $* and $@: input !__
define(`echo1', `$*')
define(`echo2', `$@')
define(`foo', `This is macro `foo'.')
echo1(foo)
echo1(`foo')
echo2(foo)
echo2(`foo')
__! 013 define - $* and $@: output !__



This is macro This is macro foo..
This is macro foo.
This is macro foo.
foo
__! 014 define - $* and $@ and #: input !__
define(`echo1', `$*')
define(`echo2', `$@')
define(`foo', `bar')
echo1(#foo'foo
foo)
echo2(#foo'foo
foo)
__! 014 define - $* and $@ and #: output !__



#foo'foo
bar
#foobar
bar'
__! 015 define - $: input !__
define(`foo', `$$$ hello $$$')
foo
__! 015 define - $: output !__

$$$ hello $$$
__! 016 define - expand to e.g. $12: input !__
define(`foo', `no nested quote: $1')
foo(`arg')
define(`foo', `nested quote around $: `$'1')
foo(`arg')
define(`foo', `nested empty quote after $: $`'1')
foo(`arg')
define(`foo', `nested quote around next character: $`1'')
foo(`arg')
define(`foo', `nested quote around both: `$1'')
foo(`arg')
__! 016 define - expand to e.g. $12: output !__

no nested quote: arg

nested quote around $: $1

nested empty quote after $: $1

nested quote around next character: $1

nested quote around both: arg
__! 017 undefine: input !__
foo bar blah
define(`foo', `some')define(`bar', `other')define(`blah', `text')
foo bar blah
undefine(`foo')
foo bar blah
undefine(`bar', `blah')
foo bar blah
__! 017 undefine: output !__
foo bar blah

some other text

foo other text

foo bar blah
__! 018 undefine a macro inside macros's expansion: input !__
define(`f', ``$0':$1')
f(f(f(undefine(`f')`hello world')))
f(`bye')
__! 018 undefine a macro inside macros's expansion: output !__

f:f:f:hello world
f(bye)
__! 019 defn - rename undefine: input !__
define(`zap', defn(`undefine'))
zap(`undefine')
undefine(`zap')
__! 019 defn - rename undefine: output !__


undefine(zap)
__! 020 defn - use of $0: input !__
define(`foo', `This is `$0'')
define(`bar', defn(`foo'))
bar
__! 020 defn - use of $0: output !__


This is bar
__! 021 defn - avoid unwanted expansion of text: input !__
define(`string', `The macro dnl is very useful
')
string
defn(`string')
__! 021 defn - avoid unwanted expansion of text: output !__

The macro 
The macro dnl is very useful

__! 022 defn - avoid unbalanced end-quote: input !__
define(`foo', a'a)
define(`a', `A')
define(`echo', `$@')
foo
defn(`foo')
echo(foo)
__! 022 defn - avoid unbalanced end-quote: output !__



A'A
aA'
AA'
__! 023 defn - join unbalanced quotes: input !__
define(`l', `<[>')define(`r', `<]>')
changequote(`[', `]')
defn([l])defn([r])
])
defn([l], [r])
__! 023 defn - join unbalanced quotes: output !__


<[>]defn([r])
)
<[>][<]>
__! 024 defn - special tokens outside of expected context: input !__
defn(`defn')
define(defn(`divnum'), `cannot redefine a builtin token')
divnum
len(defn(`divnum'))
__! 024 defn - special tokens outside of expected context: output !__


0
0
__! 025 defn can only join text macros: input !__
define(`a', `A')define(`AA', `b')
defn(`a', `divnum', `a')
define(`mydivnum', defn(`divnum', `divnum'))mydivnum
__! 025 defn can only join text macros: output !__

AA

__! 026 pushdef/popdef: input !__
define(`foo', `Expansion one.')
foo
pushdef(`foo', `Expansion two.')
foo
pushdef(`foo', `Expansion three.')
pushdef(`foo', `Expansion four.')
popdef(`foo')
foo
popdef(`foo', `foo')
foo
popdef(`foo')
foo
__! 026 pushdef/popdef: output !__

Expansion one.

Expansion two.



Expansion three.

Expansion one.

foo
__! 027 pushdef/popdef rather than define: input !__
define(`foo', `Expansion one.')
foo
pushdef(`foo', `Expansion two.')
foo
define(`foo', `Second expansion two.')
foo
undefine(`foo')
foo
__! 027 pushdef/popdef rather than define: output !__

Expansion one.

Expansion two.

Second expansion two.

foo
__! 028 indir - invalid name: input !__
define(`$$internal$macro', `Internal macro (name `$0')')
$$internal$macro
indir(`$$internal$macro')
__! 028 indir - invalid name: output !__

$$internal$macro
Internal macro (name $$internal$macro)
__! 029 indir - arguments collection: input !__
define(`f', `1')
f(define(`f', `2'))
indir(`f', define(`f', `3'))
indir(`f', undefine(`f'))
__! 029 indir - arguments collection: output !__

1
3

__! 030 indir on defn output: input !__
indir(defn(`defn'), `divnum')
indir(`define', defn(`defn'), `divnum')
indir(`define', `foo', defn(`divnum'))
foo
indir(`divert', defn(`foo'))
__! 030 indir on defn output: output !__



0

__! 031 builtin: input !__
pushdef(`define', `hidden')
undefine(`undefine')
define(`foo', `bar')
foo
builtin(`define', `foo', defn(`divnum'))
foo
builtin(`define', `foo', `BAR')
foo
undefine(`foo')
foo
builtin(`undefine', `foo')
foo
__! 031 builtin: output !__


hidden
foo

0

BAR
undefine(foo)
BAR

foo
__! 032 builtin does not depend on --prefix-builtin: input( { shift->prefix_builtins('m4_') }) !__
m4_builtin(`divnum')
m4_builtin(`m4_divnum')
m4_indir(`divnum')
m4_indir(`m4_divnum')
__! 032 builtin does not depend on --prefix-builtin: output !__
0


0
__! 033 builtin used to call a builtin without argument: input !__
builtin
builtin()
builtin(`builtin')
builtin(`builtin',)
builtin(`builtin', ``'
')
indir(`index')
__! 033 builtin used to call a builtin without argument: output !__
builtin





__! 034 ifdef: input !__
ifdef(`foo', ``foo' is defined', ``foo' is not defined')
define(`foo', `')
ifdef(`foo', ``foo' is defined', ``foo' is not defined')
ifdef(`no_such_macro', `yes', `no', `extra argument')
__! 034 ifdef: output !__
foo is not defined

foo is defined
no
__! 035 ifelse - one or two arguments: input !__
ifelse(`some comments')
ifelse(`foo', `bar')
__! 035 ifelse - one or two arguments: output !__


__! 035 ifelse - three or four arguments: input !__
ifelse(`foo', `bar', `true')
ifelse(`foo', `foo', `true')
define(`foo', `bar')
ifelse(foo, `bar', `true', `false')
ifelse(foo, `foo', `true', `false')
__! 035 ifelse - three or four arguments: output !__

true

true
false
__! 036 ifelse - reproduce behaviour of blind builtins: input !__
define(`foo', `ifelse(`$#', `0', ``$0'', `arguments:$#')')
foo
foo()
foo(`a', `b', `c')
__! 036 ifelse - reproduce behaviour of blind builtins: output !__

foo
arguments:1
arguments:3
__! 037 ifelse - more than four arguments: input !__
ifelse(`foo', `bar', `third', `gnu', `gnats')
ifelse(`foo', `bar', `third', `gnu', `gnats', `sixth')
ifelse(`foo', `bar', `third', `gnu', `gnats', `sixth', `seventh')
ifelse(`foo', `bar', `3', `gnu', `gnats', `6', `7', `8')
__! 037 ifelse - more than four arguments: output !__
gnu

seventh
7
__! 038 shift: input !__
shift
shift(`bar')
shift(`foo', `bar', `baz')
__! 038 shift: output !__
shift

bar,baz
__! 039 shift - composite reverse: input !__
define(`reverse', `ifelse(`$#', `0', , `$#', `1', ``$1'',
                          `reverse(shift($@)), `$1'')')
reverse
reverse(`foo')
reverse(`foo', `bar', `gnats', `and gnus')
__! 039 shift - composite reverse: output !__


foo
and gnus, gnats, bar, foo
__! 040 shift - composite cond: input !__
define(`cond',
`ifelse(`$#', `1', `$1',
        `ifelse($1, `$2', `$3',
                `$0(shift(shift(shift($@))))')')')dnl
define(`side', `define(`counter', incr(counter))$1')dnl
define(`example1',
`define(`counter', `0')dnl
ifelse(side(`$1'), `yes', `one comparison: ',
       side(`$1'), `no', `two comparisons: ',
       side(`$1'), `maybe', `three comparisons: ',
       `side(`default answer: ')')counter')dnl
define(`example2',
`define(`counter', `0')dnl
cond(`side(`$1')', `yes', `one comparison: ',
     `side(`$1')', `no', `two comparisons: ',
     `side(`$1')', `maybe', `three comparisons: ',
     `side(`default answer: ')')counter')dnl
example1(`yes')
example1(`no')
example1(`maybe')
example1(`feeling rather indecisive today')
example2(`yes')
example2(`no')
example2(`maybe')
example2(`feeling rather indecisive today')
__! 040 shift - composite cond: output !__
one comparison: 3
two comparisons: 3
three comparisons: 3
default answer: 4
one comparison: 1
two comparisons: 2
three comparisons: 3
default answer: 4
__! 041 shift - composites join/joinall: input( { shift->include(['inc']) }) !__
include(`join.m4')
join,join(`-'),join(`-', `'),join(`-', `', `')
joinall,joinall(`-'),joinall(`-', `'),joinall(`-', `', `')
join(`-', `1')
join(`-', `1', `2', `3')
join(`', `1', `2', `3')
join(`-', `', `1', `', `', `2', `')
joinall(`-', `', `1', `', `', `2', `')
join(`,', `1', `2', `3')
define(`nargs', `$#')dnl
nargs(join(`,', `1', `2', `3'))
__! 041 shift - composites join/joinall: output !__

,,,
,,,-
1
1-2-3
123
1-2
-1---2-
1,2,3
1
__! 042 shift - composites join/joinall v2: input( { shift->include(['inc']) }) !__
undivert(`join.m4')dnl
__! 042 shift - composites join/joinall v2: output !__
divert(`-1')
# join(sep, args) - join each non-empty ARG into a single
# string, with each element separated by SEP
define(`join',
`ifelse(`$#', `2', ``$2'',
  `ifelse(`$2', `', `', ``$2'_')$0(`$1', shift(shift($@)))')')
define(`_join',
`ifelse(`$#$2', `2', `',
  `ifelse(`$2', `', `', ``$1$2'')$0(`$1', shift(shift($@)))')')
# joinall(sep, args) - join each ARG, including empty ones,
# into a single string, with each element separated by SEP
define(`joinall', ``$2'_$0(`$1', shift($@))')
define(`_joinall',
`ifelse(`$#', `2', `', ``$1$3'$0(`$1', shift(shift($@)))')')
divert`'dnl
__! 043 shift - composites quote/dquote/dquote_elt: input( { shift->include(['inc']) }) !__
include(`quote.m4')
-quote-dquote-dquote_elt-
-quote()-dquote()-dquote_elt()-
-quote(`1')-dquote(`1')-dquote_elt(`1')-
-quote(`1', `2')-dquote(`1', `2')-dquote_elt(`1', `2')-
define(`n', `$#')dnl
-n(quote(`1', `2'))-n(dquote(`1', `2'))-n(dquote_elt(`1', `2'))-
dquote(dquote_elt(`1', `2'))
dquote_elt(dquote(`1', `2'))
__! 043 shift - composites quote/dquote/dquote_elt: output !__

----
--`'-`'-
-1-`1'-`1'-
-1,2-`1',`2'-`1',`2'-
-1-1-2-
``1'',``2''
``1',`2''
__! 044 shift - composites quote/dquote/dquote_elt v2: input( { shift->include(['inc']) }) !__
undivert(`quote.m4')dnl
__! 044 shift - composites quote/dquote/dquote_elt v2: output !__
divert(`-1')
# quote(args) - convert args to single-quoted string
define(`quote', `ifelse(`$#', `0', `', ``$*'')')
# dquote(args) - convert args to quoted list of quoted strings
define(`dquote', ``$@'')
# dquote_elt(args) - convert args to list of double-quoted strings
define(`dquote_elt', `ifelse(`$#', `0', `', `$#', `1', ```$1''',
                             ```$1'',$0(shift($@))')')
divert`'dnl
__! 045 shift - composite argn: input( { shift->include(['inc']) }) !__
define(`argn', `ifelse(`$1', 1, ``$2'',
  `argn(decr(`$1'), shift(shift($@)))')')
argn(`1', `a')
define(`foo', `argn(`11', $@)')
foo(`a', `b', `c', `d', `e', `f', `g', `h', `i', `j', `k', `l')
__! 045 shift - composite argn: output !__

a

k
__! 046 composite forloop: input( { shift->include(['inc']) }) !__
include(`forloop.m4')
forloop(`i', `1', `8', `i ')
__! 046 composite forloop: output !__

1 2 3 4 5 6 7 8 
__! 047 composite forloop - nested: input( { shift->include(['inc']) }) !__
include(`forloop.m4')
forloop(`i', `1', `4', `forloop(`j', `1', `8', ` (i, j)')
')
__! 047 composite forloop - nested: output !__

 (1, 1) (1, 2) (1, 3) (1, 4) (1, 5) (1, 6) (1, 7) (1, 8)
 (2, 1) (2, 2) (2, 3) (2, 4) (2, 5) (2, 6) (2, 7) (2, 8)
 (3, 1) (3, 2) (3, 3) (3, 4) (3, 5) (3, 6) (3, 7) (3, 8)
 (4, 1) (4, 2) (4, 3) (4, 4) (4, 5) (4, 6) (4, 7) (4, 8)

__! 048 composite forloop - source: input( { shift->include(['inc']) }) !__
undivert(`forloop.m4')dnl
__! 048 composite forloop - source: output !__
divert(`-1')
# forloop(var, from, to, stmt) - simple version
define(`forloop', `pushdef(`$1', `$2')_forloop($@)popdef(`$1')')
define(`_forloop',
       `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')
divert`'dnl
__! 049 composites foreach/foreachq: input( { shift->include(['inc']) }) !__
include(`foreach.m4')
foreach(`x', (foo, bar, foobar), `Word was: x
')dnl
include(`foreachq.m4')
foreachq(`x', `foo, bar, foobar', `Word was: x
')dnl
__! 049 composites foreach/foreachq: output !__

Word was: foo
Word was: bar
Word was: foobar

Word was: foo
Word was: bar
Word was: foobar
