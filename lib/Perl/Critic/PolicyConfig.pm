##############################################################################
#      $URL$
#     $Date$
#   $Author$
# $Revision$
##############################################################################

package Perl::Critic::PolicyConfig;

use strict;
use warnings;

use Readonly;

our $VERSION = '1.082';

use Perl::Critic::Exception::AggregateConfiguration;
use Perl::Critic::Exception::Configuration::Option::Policy::ParameterValue;
use Perl::Critic::Utils qw< :characters severity_to_number >;

#-----------------------------------------------------------------------------

Readonly::Scalar my $NO_LIMIT => 'no_limit';

#-----------------------------------------------------------------------------

sub new {
    my ($class, $policy_short_name, $specification) = @_;

    my %self = $specification ? %{ $specification } : ();
    my %non_public_data;

    $non_public_data{_policy_short_name} = $policy_short_name;

    foreach my $standard_parameter (
        qw< maximum_violations_per_document severity set_themes add_themes >
    ) {
        if ( exists $self{$standard_parameter} ) {
            $non_public_data{"_$standard_parameter"} =
                delete $self{$standard_parameter};
        }
    }

    $self{_non_public_data} = \%non_public_data;


    return bless \%self, $class;
}

#-----------------------------------------------------------------------------

sub _validate_maximum_violations_per_document {
    my ($self, $errors) = @_;

    my $user_maximum_violations =
        $self->get_maximum_violations_per_document();

    if ( defined $user_maximum_violations ) {
        if (
                $user_maximum_violations =~ m/$NO_LIMIT/xmsio
            or  $user_maximum_violations eq $EMPTY
        ) {
            $user_maximum_violations = undef;
        }
        elsif ( not is_integer($user_maximum_violations) ) {
            $errors->add_exception(
                new_parameter_value_exception(
                    'maximum_violations_per_document',
                    $user_maximum_violations,
                    undef,
                    "does not look like an integer.\n"
                )
            );

            return;
        }
        elsif ( $user_maximum_violations < 0 ) {
            $errors->add_exception(
                new_parameter_value_exception(
                    'maximum_violations_per_document',
                    $user_maximum_violations,
                    undef,
                    "is not greater than or equal to zero.\n"
                )
            );

            return;
        }

        $self->set_maximum_violations_per_document(
            $user_maximum_violations
        );
    }

    return;
}

#-----------------------------------------------------------------------------

sub _get_non_public_data {
    my $self = shift;

    return $self->{_non_public_data};
}

#-----------------------------------------------------------------------------

sub get_policy_short_name {
    my $self = shift;

    return $self->_get_non_public_data()->{_policy_short_name};
}

#-----------------------------------------------------------------------------

sub get_set_themes {
    my ($self) = @_;

    return $self->_get_non_public_data()->{_set_themes};
}

#-----------------------------------------------------------------------------

sub get_add_themes {
    my ($self) = @_;

    return $self->_get_non_public_data()->{_add_themes};
}

#-----------------------------------------------------------------------------

sub get_severity {
    my ($self) = @_;

    return $self->_get_non_public_data()->{_severity};
}

#-----------------------------------------------------------------------------

sub get_maximum_violations_per_document {
    my ($self) = @_;

    return $self->_get_non_public_data()->{_maximum_violations_per_document};
}

#-----------------------------------------------------------------------------

sub get {
    my ($self, $parameter) = @_;

    return $self->{$parameter};
}

#-----------------------------------------------------------------------------

1;

__END__

#-----------------------------------------------------------------------------

=pod

=for stopwords

=head1 NAME

Perl::Critic::PolicyConfig - Configuration data for a Policy.


=head1 DESCRIPTION

A container for the configuration of a Policy.


=head1 METHODS

=over

=item C<get_policy_short_name()>

The name of the policy this configuration is for.  Primarily here for
the sake of debugging.


=item C< get_set_themes() >

The value of C<set_themes> in the user's F<.perlcriticrc>.


=item C< get_add_themes() >

The value of C<add_themes> in the user's F<.perlcriticrc>.


=item C< get_severity() >

The value of C<severity> in the user's F<.perlcriticrc>.


=item C< get_maximum_violations_per_document() >

The value of C<maximum_violations_per_document> in the user's
F<.perlcriticrc>.


=item C< get($parameter) >

Retrieve the value of the specified parameter in the user's
F<.perlcriticrc>.


=back


=head1 SEE ALSO

L<Perl::Critic::DEVELOPER/"MAKING YOUR POLICY CONFIGURABLE">


=head1 AUTHOR

Elliot Shank <perl@galumph.com>


=head1 COPYRIGHT

Copyright (c) 2008 Elliot Shank.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  The full text of this license
can be found in the LICENSE file included with this module.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab shiftround :