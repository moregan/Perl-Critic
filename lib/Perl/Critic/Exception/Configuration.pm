##############################################################################
#      $URL$
#     $Date$
#   $Author$
# $Revision$
##############################################################################

package Perl::Critic::Exception::Configuration;

use strict;
use warnings;

our $VERSION = '1.080';

#-----------------------------------------------------------------------------

use Exception::Class (
    'Perl::Critic::Exception::Configuration' => {
        isa         => 'Perl::Critic::Exception',
        description => 'A problem with Perl::Critic configuration, whether from a file or a command line or some other source.',
        fields      => [ qw{ source } ],
    },
);

#-----------------------------------------------------------------------------

1;

__END__

#-----------------------------------------------------------------------------

=pod

=for stopwords

=head1 NAME

Perl::Critic::Exception::Configuration - A problem with L<Perl::Critic> configuration

=head1 DESCRIPTION

A representation of a problem found with the configuration of
L<Perl::Critic>, whether from a F<.perlcriticrc>, another profile
file, or command line.

This is an abstract class.  It should never be instantiated.


=head1 METHODS

=over

=item C<source()>

Where the configuration information came from, if it could be determined.


=back


=head1 SEE ALSO

L<Perl::Critic::Exception::Configuration::Generic>
L<Perl::Critic::Exception::Configuration::Option>


=head1 AUTHOR

Elliot Shank <perl@galumph.com>

=head1 COPYRIGHT

Copyright (c) 2007 Elliot Shank.  All rights reserved.

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
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :