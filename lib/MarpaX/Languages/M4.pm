package MarpaX::Languages::M4;

# VERSION

# AUTHORITY

# ABSTRACT: M4 implementation in Perl

=head1 DESCRIPTION

This package is an implementation of M4, using Marpa::R2 and Moops.

=head1 SYNOPSIS

     use POSIX qw/EXIT_SUCCESS/;
     use MarpaX::Languages::M4::Impl::Default;
     use Log::Any;
     use Log::Any::Adapter;
     use Log::Any::Adapter::Callback;

     Log::Any::Adapter->set('Callback', min_level => 'trace', logging_cb => \&_logging_cb);

     my $impl = MarpaX::Languages::M4::Impl::Default->new_with_options();
     $impl->impl_parse('debugmode(`V\')m4wrap(`test\')');
     print "Value: " . $impl->impl_value . "\n";
     print "Rc: " . $impl->impl_rc . "\n";

     sub _logging_cb {
         my ($method, $self, $format, @params) = @_;
         printf STDERR  "$format\n", @args;
     }

=head1 SEE ALSO

L<Marpa::R2>, L<Moops>, <M4 POSIX|http://pubs.opengroup.org/onlinepubs/9699919799/utilities/m4.html>, <M4 GNU|https://www.gnu.org/software/m4/manual/m4.html>

=cut

1;
