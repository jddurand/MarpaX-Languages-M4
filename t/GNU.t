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
        maxlevel => "error",
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
  my $got = $gnu->impl_parseBuffers($input)->impl_value;
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
__! 032 builtin does not depend on --prefix-builtin: input( {my ($self) = @_; $self->prefix_builtins('m4_'); $self }) !__
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
__! 041 shift - composites join/joinall: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
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
__! 042 shift - composites quote/dquote/dquote_elt: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`quote.m4')
-quote-dquote-dquote_elt-
-quote()-dquote()-dquote_elt()-
-quote(`1')-dquote(`1')-dquote_elt(`1')-
-quote(`1', `2')-dquote(`1', `2')-dquote_elt(`1', `2')-
define(`n', `$#')dnl
-n(quote(`1', `2'))-n(dquote(`1', `2'))-n(dquote_elt(`1', `2'))-
dquote(dquote_elt(`1', `2'))
dquote_elt(dquote(`1', `2'))
__! 042 shift - composites quote/dquote/dquote_elt: output !__

----
--`'-`'-
-1-`1'-`1'-
-1,2-`1',`2'-`1',`2'-
-1-1-2-
``1'',``2''
``1',`2''
__! 043 shift - composite argn: input !__
define(`argn', `ifelse(`$1', 1, ``$2'',
  `argn(decr(`$1'), shift(shift($@)))')')
argn(`1', `a')
define(`foo', `argn(`11', $@)')
foo(`a', `b', `c', `d', `e', `f', `g', `h', `i', `j', `k', `l')
__! 043 shift - composite argn: output !__

a

k
__! 044 composite forloop: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`forloop.m4')
forloop(`i', `1', `8', `i ')
__! 044 composite forloop: output !__

1 2 3 4 5 6 7 8 
__! 045 composite forloop - nested: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`forloop.m4')
forloop(`i', `1', `4', `forloop(`j', `1', `8', ` (i, j)')
')
__! 045 composite forloop - nested: output !__

 (1, 1) (1, 2) (1, 3) (1, 4) (1, 5) (1, 6) (1, 7) (1, 8)
 (2, 1) (2, 2) (2, 3) (2, 4) (2, 5) (2, 6) (2, 7) (2, 8)
 (3, 1) (3, 2) (3, 3) (3, 4) (3, 5) (3, 6) (3, 7) (3, 8)
 (4, 1) (4, 2) (4, 3) (4, 4) (4, 5) (4, 6) (4, 7) (4, 8)

__! 046 composites foreach/foreachq: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`foreach.m4')
foreach(`x', (foo, bar, foobar), `Word was: x
')dnl
include(`foreachq.m4')
foreachq(`x', `foo, bar, foobar', `Word was: x
')dnl
__! 046 composites foreach/foreachq: output !__

Word was: foo
Word was: bar
Word was: foobar

Word was: foo
Word was: bar
Word was: foobar
__! 047 composites foreach/foreachq - generate a shell case statement: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`foreach.m4')
define(`_case', `  $1)
    $2=" $1";;
')dnl
define(`_cat', `$1$2')dnl
case $`'1 in
foreach(`x', `(`(`a', `vara')', `(`b', `varb')', `(`c', `varc')')',
        `_cat(`_case', x)')dnl
esac
__! 047 composites foreach/foreachq - generate a shell case statement: output !__

case $1 in
  a)
    vara=" a";;
  b)
    varb=" b";;
  c)
    varc=" c";;
esac
__! 048 composites foreach/foreachq - comparison: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
define(`a', `1')define(`b', `2')define(`c', `3')
include(`foreach.m4')
include(`foreachq.m4')
foreach(`x', `(``a'', ``(b'', ``c)'')', `x
')
foreachq(`x', ```a'', ``(b'', ``c)''', `x
')dnl
__! 048 composites foreach/foreachq - comparison: output !__



1
(2)1

, x
)
a
(b
c)
__! 049 composite foreachq limitation: input( {my ($self) = @_; $self->include(['inc']); $self; }) !__
include(`foreach.m4')include(`foreachq.m4')
foreach(`name', `(`a', `b')', ` defn(`name')')
foreachq(`name', ``a', `b'', ` defn(`name')')
__! 049 composite foreachq limitation: output !__

 a b
 _arg1(`a', `b') _arg1(shift(`a', `b'))
__! 050 composite stack: input( {my ($self) = @_; $self->include(['inc']); $self }) !__
include(`stack.m4')
pushdef(`a', `1')pushdef(`a', `2')pushdef(`a', `3')
define(`show', ``$1'
')
stack_foreach(`a', `show')dnl
stack_foreach_lifo(`a', `show')dnl
__! 050 composite stack: output !__



1
2
3
3
2
1
__! 050 composite define_blind: input !__
define(`define_blind', `ifelse(`$#', `0', ``$0'',
`_$0(`$1', `$2', `$'`#', `$'`0')')')
define(`_define_blind', `define(`$1',
`ifelse(`$3', `0', ``$4'', `$2')')')
define_blind
define_blind(`foo', `arguments were $*')
foo
foo(`bar')
define(`blah', defn(`foo'))
blah
blah(`a', `b')
defn(`blah')
__! 050 composite define_blind: output !__


define_blind

foo
arguments were bar

blah
arguments were a,b
ifelse(`$#', `0', ``$0'', `arguments were $*')
__! 051 composite curry: input( {my ($self) = @_; $self->include(['inc']); $self; }) !__
include(`curry.m4')include(`stack.m4')
define(`reverse', `ifelse(`$#', `0', , `$#', `1', ``$1'',
                          `reverse(shift($@)), `$1'')')
pushdef(`a', `1')pushdef(`a', `2')pushdef(`a', `3')
stack_foreach(`a', `:curry(`reverse', `4')')
curry(`curry', `reverse', `1')(`2')(`3')
__! 051 composite curry: output !__



:1, 4:2, 4:3, 4
3, 2, 1
__! 052 composites copy/rename: input( {my ($self) = @_; $self->include(['inc']); $self; }) !__
include(`curry.m4')include(`stack.m4')
define(`rename', `copy($@)undefine(`$1')')dnl
define(`copy', `ifdef(`$2', `errprint(`$2 already defined
')m4exit(`1')',
   `stack_foreach(`$1', `curry(`pushdef', `$2')')')')dnl
pushdef(`a', `1')pushdef(`a', defn(`divnum'))pushdef(`a', `2')
copy(`a', `b')
rename(`b', `c')
a b c
popdef(`a', `c')c a
popdef(`a', `c')a c
__! 052 composites copy/rename: output !__




2 b 2
 0
1 1
__! 053 dumpdef: input !__
define(`foo', `Hello world.')
dumpdef(`foo')
dumpdef(`define')
__! 053 dumpdef: output !__



__! 054 dumpdef 2: input !__
pushdef(`f', ``$0'1')pushdef(`f', ``$0'2')
f(popdef(`f')dumpdef(`f'))
f(popdef(`f')dumpdef(`f'))
__! 054 dumpdef 2: output !__

f2
f1
__! 055 dnl: input !__
define(`foo', `Macro `foo'.')dnl A very simple macro, indeed.
foo
__! 055 dnl: output !__
Macro foo.
__! 056 dnl warning: input !__
dnl(`args are ignored, but side effects occur',
define(`foo', `like this')) while this text is ignored: undefine(`foo')
See how `foo' was defined, foo?
__! 056 dnl warning: output !__
See how foo was defined, like this?
__! 057 dnl warning at eof: input !__
m4wrap(`m4wrap(`2 hi
')0 hi dnl 1 hi')
define(`hi', `HI')
__! 057 dnl warning at eof: output !__


0 HI 2 HI
