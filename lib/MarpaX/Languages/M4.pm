use Moops;
use MarpaX::Languages::M4::Impl::Default;

# PODCLASSNAME

# ABSTRACT: M4 pre-processor

class MarpaX::Languages::M4 {
    extends 'MarpaX::Languages::M4::Impl::Default';

    # VERSION

    # AUTHORITY

    #
    # We are lazy and just explicitely proxy all Impl methods
    #
    method quote           (@args) { $self->impl_quote           (@args) }
    method unquote         (@args) { $self->impl_unquote         (@args) }
    method appendValue     (@args) { $self->impl_appendValue     (@args) }
    method value           (@args) { $self->impl_value           (@args) }
    method valueRef        (@args) { $self->impl_valueRef        (@args) }
    method parseIncremental(@args) { $self->impl_parseIncremental(@args) }
    method parse           (@args) { $self->impl_parse           (@args) }
    method unparsed        (@args) { $self->impl_unparsed        (@args) }
    method setEoi          (@args) { $self->impl_setEoi          (@args) }
    method eoi             (@args) { $self->impl_eoi             (@args) }
    method raiseException  (@args) { $self->impl_raiseException  (@args) }
    method line            (@args) { $self->impl_line            (@args) }
    method debugfile       (@args) { $self->impl_debugfile       (@args) }
    method rc              (@args) { $self->impl_rc              (@args) }
    method isImplException (@args) { $self->impl_isImplException (@args) }
    method macroExecute    (@args) { $self->impl_macroExecute    (@args) }
    method macroCallId     (@args) { $self->impl_macroCallId     (@args) }
}

=head1 DESCRIPTION

This package is an implementation of M4.

=head1 SYNOPSIS

     use POSIX qw/EXIT_SUCCESS/;
     use MarpaX::Languages::M4;
     use Log::Any;
     use Log::Any::Adapter;
     use Log::Any::Adapter::Callback;

     Log::Any::Adapter->set('Callback', min_level => 'trace', logging_cb => \&_logging_cb);

     my $m4 = MarpaX::Languages::M4->new_with_options();
     $m4->parse('debugmode(`V\')m4wrap(`test\')');
     print "Value: " . $m4->value . "\n";
     print "Rc: " . $m4->rc . "\n";

     sub _logging_cb {
         my ($method, $self, $format, @params) = @_;
         printf STDERR  "$format\n", @args;
     }

=cut

=head1 SEE ALSO

L<Marpa::R2>, L<Moops>, L<M4 POSIX|http://pubs.opengroup.org/onlinepubs/9699919799/utilities/m4.html>, L<M4 GNU|https://www.gnu.org/software/m4/manual/m4.html>

=cut

1;
