package inc::MyDistMakeMaker;
use Moose;
use Config;
use Config::AutoConf;     # Just to make sure Config::AutoConf is loaded if Marpa needs it
use File::Spec;

my $INC = File::Spec->catdir(File::Spec->curdir, 'gnuregex');
my @SRC = qw/regex regex_internal regexec regcomp/;
my $OBJS = join(' ', map {File::Spec->catfile($INC, "$_\$(OBJ_EXT)")} @SRC);

extends 'Dist::Zilla::Plugin::MakeMaker::Awesome';

override _build_WriteMakefile_args => sub {
    +{
	# Add MYEXTLIB => to WriteMakefile() args
	%{ super() },
	MYEXTLIB => 'libgnuregex' . $Config{lib_ext},
        INC  => "-I$INC",
        OBJECT    => $OBJS
    }
};

override _build_MakeFile_PL_template => sub {
    my ($self) = @_;
    my $template = super();
 
    $template .= <<'TEMPLATE';
package MY;
use Config;

sub postamble {
    my $self = shift;
    my @ret = (
	$self->SUPER::postamble,
	'libgnuregex' . $Config{lib_ext} . ' :',
	"\t" . '$(PERLRUN) BuildGnuRegex.pl gnuregex',
	''
	);
    return join "\n", @ret;
}
TEMPLATE
 
    return $template;
};

  __PACKAGE__->meta->make_immutable;
