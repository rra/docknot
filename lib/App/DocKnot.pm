# Parent module for DocKnot.
#
# DocKnot provides various commands for generating documentation, web pages,
# and software releases.  This parent module provides some internal helper
# functions used to load configuration and metadata.  The normal entry point
# are the various submodules, or App::DocKnot::Command via docknot.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot 3.02;

use 5.024;
use autodie;
use warnings;

use File::BaseDir qw(config_files);
use File::ShareDir qw(module_file);
use File::Spec;
use JSON;
use Perl6::Slurp;

##############################################################################
# Helper methods
##############################################################################

# Helper routine to return the path of a file from the application data.
# These data files are installed with App::DocKnot, but each file can be
# overridden by the user via files in $HOME/.config/docknot or
# /etc/xdg/docknot (or whatever $XDG_CONFIG_DIRS is set to).
#
# We therefore try File::BaseDir first (which handles the XDG paths) and fall
# back on using File::ShareDir to locate the data.
#
# $self - The App::DocKnot object
# @path - The relative path of the file as a list of components
#
# Returns: The absolute path to the application data
#  Throws: Text exception on failure to locate the desired file
sub appdata_path {
    my ($self, @path) = @_;

    # Try XDG paths first.
    my $path = config_files('docknot', @path);

    # If that doesn't work, use the data that came with the module.
    if (!defined($path)) {
        $path = module_file('App::DocKnot', File::Spec->catfile(@path));
    }
    return $path;
}

# Helper routine that locates an application data file, interprets it as JSON,
# and returns the resulting decoded contents.  This uses the relaxed parsing
# mode, so comments and commas after data elements are supported.
#
# $self - The App::DocKnot object
# @path - The path of the file to load, as a list of components
#
# Returns: Anonymous hash or array resulting from decoding the JSON object
#  Throws: slurp or JSON exception on failure to load or decode the object
sub load_appdata_json {
    my ($self, @path) = @_;
    my $path = $self->appdata_path(@path);
    my $json = JSON->new;
    $json->relaxed;
    return $json->decode(scalar(slurp($path)));
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot docknot MERCHANTABILITY NONINFRINGEMENT sublicense JSON
submodules

=head1 NAME

App::DocKnot - Documentation and software release management

=head1 REQUIREMENTS

Perl 5.24 or later and the modules File::BaseDir, File::ShareDir, JSON,
Perl6::Slurp, and Template (part of Template Toolkit), all of which are
available from CPAN.

=head1 DESCRIPTION

DocKnot is a system for documentation and software release management.  Its
functionality is provided by various submodules, often invoked via the
B<docknot> command-line program.  For more information, see L<docknot(1)>.

This module only provides helper functions to load configuration and metadata
that are used by its various submodules.

=head1 INSTANCE METHODS

=over 4

=item appdata_path(PATH[, ...])

Return the path of a file from the application data.  The file is specified as
one or more path components.

These data files are installed with App::DocKnot, but each file can be
overridden by the user via files in F<$HOME/.config/docknot> or
F</etc/xdg/docknot> (or whatever $XDG_CONFIG_DIRS is set to).  Raises a text
exception if the desired file could not be located.

=item load_appdata_json(PATH[, ...])

Locate an application data file using the same algorithm as appdata_path(),
interpret it as JSON, and returns the resulting decoded contents.  This uses
the relaxed JSON parsing mode, so comments and commas after data elements are
supported.  The path is specified as one or more path components.

Raises a slurp or JSON exception on failure to load or decode the data file.

=back

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2013-2019 Russ Allbery <rra@cpan.org>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

=head1 SEE ALSO

L<docknot(1)>

This module is part of the App-DocKnot distribution.  The current version of
App::DocKnot is available from CPAN, or directly from its web site at
L<https://www.eyrie.org/~eagle/software/docknot/>.

=cut

# Local Variables:
# copyright-at-end-flag: t
# End:
