#!perl
use strict;
use diagnostics;
use Config::AutoConf;
use POSIX qw/EXIT_SUCCESS/;

my $gnuregex = shift || die "Usage: $^X gnuregexDirectory";

chdir($gnuregex) || die "Cannot chdir to $gnuregex, $!";

my $ac = Config::AutoConf->new(logfile => 'config.log');
$ac->check_header('langinfo.h', 1);
$ac->check_header('libintl.h', 1);
$ac->define_var('_REGEX_INCLUDE_LIMITS_H', 1);
$ac->define_var('re_syntax_options', 'rpl_re_syntax_options');
$ac->define_var('re_set_syntax', 'rpl_re_set_syntax');
$ac->define_var('re_compile_pattern', 'rpl_re_compile_pattern');
$ac->define_var('re_compile_fastmap', 'rpl_re_compile_fastmap');
$ac->define_var('re_search', 'rpl_re_search');
$ac->define_var('re_search_2', 'rpl_re_search_2');
$ac->define_var('re_match', 'rpl_re_match');
$ac->define_var('re_match_2', 'rpl_re_match_2');
$ac->define_var('re_set_registers', 'rpl_re_set_registers');
$ac->define_var('re_comp', 'rpl_re_comp');
$ac->define_var('re_exec', 'rpl_re_exec');
$ac->define_var('regcomp', 'rpl_regcomp');
$ac->define_var('regexec', 'rpl_regexec');
$ac->define_var('regerror', 'rpl_regerror');
$ac->define_var('regfree', 'rpl_regfree');
$ac->write_config_h('config.h');

exit(EXIT_SUCCESS);
