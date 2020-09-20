# Update package configuration for a DocKnot version upgrade.
#
# Adjusts the DocKnot configuration data for changes in format for newer
# versions of DocKnot.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Update 3.05;

use 5.024;
use autodie;
use parent qw(App::DocKnot);
use warnings;

use Carp qw(croak);
use File::Spec;
use JSON::MaybeXS qw(JSON);
use Kwalify qw(validate);
use Perl6::Slurp;
use YAML::XS ();

# The older JSON metadata format stored text snippets in separate files in the
# file system.  This is the list of additional files to load from the metadata
# directory if they exist.  The contents of these files will be added to the
# configuration in a key of the same name.  If the key contains a slash, like
# foo/bar, it will be stored as a nested hash, as $data{foo}{bar}.
our @JSON_METADATA_FILES = qw(
  bootstrap
  build/middle
  build/suffix
  debian/summary
  packaging/extra
  support/extra
  test/prefix
  test/suffix
);

##############################################################################
# JSON helper methods
##############################################################################

# Internal helper routine to return the path of a file or directory from the
# package metadata directory.  The resulting file or directory path is not
# checked for existence.
#
# $self - The App::DocKnot::Generate object
# @path - The relative path of the file as a list of components
#
# Returns: The absolute path in the metadata directory
sub _metadata_path {
    my ($self, @path) = @_;
    return File::Spec->catdir($self->{metadata}, @path);
}

# Internal helper routine to read a file from the package metadata directory
# and return the contents.  The file is specified as a list of path
# components.
#
# $self - The App::DocKnot::Generate object
# @path - The path of the file to load, as a list of components
#
# Returns: The contents of the file as a string
#  Throws: slurp exception on failure to read the file
sub _load_metadata {
    my ($self, @path) = @_;
    return slurp('<:utf8', $self->_metadata_path(@path));
}

# Like _load_metadata, but interprets the contents of the metadata file as
# JSON and decodes it, returning the resulting object.  This uses the relaxed
# parsing mode, so comments and commas after data elements are supported.
#
# $self - The App::DocKnot::Generate object
# @path - The path of the file to load, as a list of components
#
# Returns: Anonymous hash or array resulting from decoding the JSON object
#  Throws: slurp or JSON exception on failure to load or decode the object
sub _load_metadata_json {
    my ($self, @path) = @_;
    my $data = $self->_load_metadata(@path);
    my $json = JSON->new;
    $json->relaxed;
    return $json->decode($data);
}

# Load the legacy JSON DocKnot package configuration.
#
# $self - The App::DocKnot::Config object
#
# Returns: The package configuration as a dict
#  Throws: autodie exception on failure to read metadata
#          Text exception on inconsistencies in the package data
sub _config_from_json {
    my ($self) = @_;

    # Localize $@ since we catch and ignore a lot of exceptions and don't want
    # to leak changes to $@ to the caller.
    local $@ = q{};

    # Load the package metadata from JSON.
    my $data_ref = $self->_load_metadata_json('metadata.json');

    # Load supplemental README sections.  readme.sections will contain a list
    # of sections to add to the README file.
    for my $section ($data_ref->{readme}{sections}->@*) {
        my $title = $section->{title};

        # The file containing the section data will match the title, converted
        # to lowercase and with spaces changed to dashes.
        my $file = lc($title);
        $file =~ tr{ }{-};

        # Load the section content.
        $section->{body} = $self->_load_metadata('sections', $file);
    }

    # If there are no supplemental README sections, remove that data element.
    if (!$data_ref->{readme}{sections}->@*) {
        delete($data_ref->{readme});
    }

    # If the package is marked orphaned, load the explanation.
    if ($data_ref->{orphaned}) {
        $data_ref->{orphaned} = $self->_load_metadata('orphaned');
    }

    # If the package has a quote, load the text of the quote.
    if ($data_ref->{quote}) {
        $data_ref->{quote}{text} = $self->_load_metadata('quote');
    }

    # Move the name of the license to its new metadata key.
    my $license = $data_ref->{license};
    $data_ref->{license} = { name => $license };

    # Load additional license notices if they exist.
    eval { $data_ref->{license}{notices} = $self->_load_metadata('notices') };

    # Load the standard sections.
    $data_ref->{blurb}        = $self->_load_metadata('blurb');
    $data_ref->{description}  = $self->_load_metadata('description');
    $data_ref->{requirements} = $self->_load_metadata('requirements');

    # Load optional information if it exists.
    for my $file (@JSON_METADATA_FILES) {
        my @file = split(m{/}xms, $file);
        if (scalar(@file) == 1) {
            eval { $data_ref->{$file} = $self->_load_metadata(@file) };
        } else {
            eval {
                $data_ref->{ $file[0] }{ $file[1] }
                  = $self->_load_metadata(@file);
            };
        }
    }

    # Return the resulting configuration.
    return $data_ref;
}

##############################################################################
# Public Interface
##############################################################################

# Create a new App::DocKnot::Update object, which will be used for subsequent
# calls.
#
# $class - Class of object to create
# $args  - Anonymous hash of arguments with the following keys:
#   metadata - Path to the directory containing package metadata
#   output   - Path to the output file with the converted metadata
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid metadata directory path
sub new {
    my ($class, $args_ref) = @_;

    # Ensure we were given a valid metadata argument.
    my $metadata = $args_ref->{metadata} // 'docs/metadata';
    if (!-d $metadata) {
        croak("metadata path $metadata does not exist or is not a directory");
    }

    # Create and return the object.
    my $self = {
        metadata => $metadata,
        output   => $args_ref->{output} // 'docs/docknot.yaml',
    };
    bless($self, $class);
    return $self;
}

# Update an older version of DocKnot configuration.  Currently, this only
# handles the old JSON format.
#
# $self - The App::DocKnot::Update object
#
# Raises: autodie exception on failure to read metadata
#         Text exception on inconsistencies in the package data
#         Text exception if schema checking failed on the converted config
sub update {
    my ($self) = @_;

    # Tell YAML::XS that we'll be feeding it JSON::PP booleans.
    local $YAML::XS::Boolean = 'JSON::PP';

    # Load the config.
    my $data_ref = $self->_config_from_json();

    # Add the current format version.
    $data_ref->{format} = 'v1';

    # Check the schema of the resulting file.
    my $schema_path = $self->appdata_path('schema/docknot.yaml');
    my $schema_ref  = YAML::XS::LoadFile($schema_path);
    eval { validate($schema_ref, $data_ref) };
    if ($@) {
        my $errors = $@;
        chomp($errors);
        die "Schema validation failed:\n$errors\n";
    }

    # Write the new YAML package configuration.
    YAML::XS::DumpFile($self->{output}, $data_ref);
    return;
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT sublicense CPAN XDG

=head1 NAME

App::DocKnot::Update - Update DocKnot package configuration for new formats

=head1 SYNOPSIS

    use App::DocKnot::Update;
    my $reader = App::DocKnot::Update->new(
        {
            metadata => 'docs/metadata',
            output   => 'docs/docknot.yaml',
        }
    );
    my $config = $reader->update();

=head1 REQUIREMENTS

Perl 5.24 or later and the modules File::BaseDir, File::ShareDir, JSON,
Perl6::Slurp, and YAML::XS, all of which are available from CPAN.

=head1 DESCRIPTION

This component of DocKnot updates package configuration from older versions.
Currently, its main purpose is to convert from the JSON format used prior to
DocKnot 4.0 to the current YAML syntax.

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Update object.  This should be used for all
subsequent actions.  ARGS should be a hash reference with one or more of the
following keys:

=over 4

=item metadata

The path to the directory containing the legacy JSON metadata for a package.
Default: F<docs/metadata> relative to the current directory.

=item output

The path to which to write the new YAML configuration.  Default:
F<docs/docknot.yaml> relative to the current directory.

=back

=back

=head1 INSTANCE METHODS

=over 4

=item update()

Load the legacy JSON metadata and write out the YAML equivalent.

=back

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2013-2020 Russ Allbery <rra@cpan.org>

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
