# Read and return DocKnot package configuration.
#
# Parses the DocKnot package configuration and provides it to other DocKnot
# commands.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Config 4.01;

use 5.024;
use autodie;
use parent qw(App::DocKnot);
use warnings;

use Carp qw(croak);
use File::BaseDir qw(config_files);
use Kwalify qw(validate);
use YAML::XS ();

##############################################################################
# Helper methods
##############################################################################

# Load a YAML file with schema checking.
#
# $path   - Path to the YAML file to load
# $schema - Name of the schema file against which to check it
#
# Returns: Contents of the file as a hash
#  Throws: YAML::XS exception on invalid file
#          Text exception on schema mismatch
sub _load_yaml_file {
    my ($self, $path, $schema) = @_;

    # Tell YAML::XS to use real booleans.  Otherwise, Kwalify is unhappy with
    # data elements set to false.
    local $YAML::XS::Boolean = 'JSON::PP';

    # Load the metadata and check it against the schema.
    my $data_ref    = YAML::XS::LoadFile($path);
    my $schema_path = $self->appdata_path('schema', $schema);
    my $schema_ref  = YAML::XS::LoadFile($schema_path);
    eval { validate($schema_ref, $data_ref) };
    if ($@) {
        my $errors = $@;
        chomp($errors);
        die "Schema validation for $path failed:\n$errors\n";
    }

    # Return the verified contents.
    return $data_ref;
}

##############################################################################
# Public Interface
##############################################################################

# Create a new App::DocKnot::Config object, which will be used for subsequent
# calls.
#
# $args  - Anonymous hash of arguments with the following keys:
#   metadata - Path to the docknot.yaml file
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid metadata directory path
sub new {
    my ($class, $args_ref) = @_;

    # Ensure we were given a valid metadata argument.
    my $metadata = $args_ref->{metadata} // 'docs/docknot.yaml';
    if (!-e $metadata) {
        croak("metadata path $metadata does not exist");
    }

    # Create and return the object.
    my $self = { metadata => $metadata };
    bless($self, $class);
    return $self;
}

# Load the DocKnot package configuration.
#
# Returns: The package configuration as a dict
#  Throws: YAML::XS exception on invalid package metadata
#          Text exception on schema mismatch for package metadata
#          Text exception on inconsistencies in the package data
sub config {
    my ($self) = @_;

    # Load the package metadata.
    my $data_ref = $self->_load_yaml_file($self->{metadata}, 'docknot.yaml');

    # build.install defaults to true.
    if (!exists($data_ref->{build}{install})) {
        $data_ref->{build}{install} = 1;
    }

    # Set a flag indicating whether the testing section was overridden.  This
    # is easier for templates to check.
    for my $section_ref ($data_ref->{readme}{sections}->@*) {
        if (lc($section_ref->{title}) eq 'testing') {
            $data_ref->{readme}{testing} = 1;
            last;
        }
    }

    # Expand the package license into license text.
    my $license       = $data_ref->{license}{name};
    my $licenses_path = $self->appdata_path('licenses.yaml');
    my $licenses_ref  = YAML::XS::LoadFile($licenses_path);
    if (!exists($licenses_ref->{$license})) {
        die "Unknown license $license\n";
    }
    $data_ref->{license}{summary} = $licenses_ref->{$license}{summary};
    $data_ref->{license}{text}    = $licenses_ref->{$license}{text};

    # Return the resulting configuration.
    return $data_ref;
}

# Load the DocKnot global configuration.
#
# Returns: The global configuration as a dict
#  Throws: YAML::XS exception on invalid configuration file
#          Text exception on schema mismatch for the configuration file
sub global_config {
    my ($self) = @_;

    # Load the global configuration file and return an empty hash if no config
    # exists.
    my $config_path = config_files('docknot', 'config.yaml');
    if (!defined($config_path)) {
        return {};
    }
    my $data_ref = $self->_load_yaml_file($config_path, 'config.yaml');

    # Return the resulting configuration.
    return $data_ref;
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT sublicense CPAN XDG Kwalify
distdir

=head1 NAME

App::DocKnot::Config - Read and return DocKnot package configuration

=head1 SYNOPSIS

    use App::DocKnot::Config;
    my $reader = App::DocKnot::Config->new({ metadata => 'docs/docknot.yaml' });
    my $config = $reader->config();
    my $global_config = $reader->global_config();

=head1 REQUIREMENTS

Perl 5.24 or later and the modules File::BaseDir, File::ShareDir, Kwalify, and
YAML::XS, all of which are available from CPAN.

=head1 DESCRIPTION

This component of DocKnot reads and returns the DocKnot global configuration
and the configuration for a package.  It takes as input a directory of
metadata and returns either the global configuration or the package
configuration as a hash.

The global configuration and additional metadata about specific licenses is
loaded via File::BaseDir and therefore uses XDG paths by default.  This means
that the configuration and supplemental license metadata is found by searching
the following paths in order:

=over 4

=item 1.

F<$XDG_CONFIG_HOME/docknot> (F<$HOME/.config/docknot> by default)

=item 2.

F<$XDG_CONFIG_DIRS/docknot> (F</etc/xdg/docknot> by default)

=item 3.

Files included in the package.

=back

The global configuration file should be named F<config.yaml> in that
directory.  The license metadata should be named F<licenses.yaml>.

Default license metadata is included with the App::DocKnot module and is used
unless more specific configuration files exist.  Be aware that, if such a
configuration file exists, the data is not merged and only the configuration
file is used.

=head1 CONFIGURATION

=head1 Global Configuration

The global configuration file (F<$HOME/.config/docknot/config.yaml> by
default) may contain the following keys:

=over 4

=item distdir

Specifies the directory into which to build and store distribution tarballs
(via the C<docknot dist> command).  Subdirectories of this directory are used
as working directories while the distribution is being built, and the final
tarballs are stored in this directory.

If this is not specified, the B<-d> option to C<docknot dist> is mandatory.

=item pgp_key

Sign distribution tarballs generated via C<docknot dist> with this PGP key.
Equivalent to the B<-p> option to C<docknot dist>.

=back

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Config object.  This should be used for all
subsequent actions.  ARGS should be a hash reference with one or more of the
following keys:

=over 4

=item metadata

The path to the metadata for a package.  Default: F<docs/docknot.yaml>
relative to the current directory.

=back

=back

=head1 INSTANCE METHODS

=over 4

=item config()

Load the metadata for the package and return it as a hash.  The possible keys
of this hash and the possible values are not yet documented.

=item global_config()

Load the global DocKnot configuration and return it as a hash.

=back

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2013-2021 Russ Allbery <rra@cpan.org>

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
DocKnot is available from CPAN, or directly from its web site at
L<https://www.eyrie.org/~eagle/software/docknot/>.

=cut

# Local Variables:
# copyright-at-end-flag: t
# End:
