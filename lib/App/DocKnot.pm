# Implementation of the DocKnot application.
#
# This is the primary class for the DocKnot application, which supports
# generation of various documentation files based on package metadata and
# general templates.
#
# Written by Russ Allbery <rra@cpan.org>
# Copyright 2013 Russ Allbery <rra@cpan.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot 1.00;

use 5.018;
use autodie;
use warnings;

use Carp qw(croak);
use File::BaseDir qw(config_files);
use File::ShareDir qw(module_file);
use File::Spec;
use JSON;
use Perl6::Slurp;
use Template;
use Text::Wrap qw(wrap);

##############################################################################
# Generator functions
##############################################################################

# The internal helper object methods in this section are generators.  They
# return code references intended to be passed into Template Toolkit as code
# references so that they can be called inside templates, incorporating data
# from the App::DocKnot configuration or the package metadata.

# Returns code to center a line in $self->{width} characters given the text of
# the line.  The returned code will take a line of text and return that line
# with leading whitespace added as required.
#
# $self - The App::DocKnot object
#
# Returns: Code reference to a closure that uses $self->{width} for width
sub _code_for_center {
    my ($self) = @_;
    my $center = sub {
        my ($text) = @_;
        my $space = $self->{width} - length($text);
        if ($space <= 0) {
            return $text;
        } else {
            return q{ } x int($space / 2) . $text;
        }
    };
    return $center;
}

# Returns code that formats the copyright notices for the package.  The
# resulting code reference takes one parameter, the indentation level, and
# wraps the copyright notices accordingly.  They will be wrapped with a
# four-space outdent and kept within $self->{width} columns.
#
# $self           - The App::DocKnot object
# $copyrights_ref - A reference to a list of anonymous hashes, each with keys:
#   holder - The copyright holder for that copyright
#   years  - The years of that copyright
#
# Returns: Code reference to a closure taking an indent level and returning
#          the formatted copyright notice
sub _code_for_copyright {
    my ($self, $copyrights_ref) = @_;
    my $copyright = sub {
        my ($indent) = @_;
        my $prefix = q{ } x $indent;
        my $notice;
        for my $copyright (@{$copyrights_ref}) {
            my $holder = $copyright->{holder};
            my $years  = $copyright->{years};

            # Build the initial notice with the word copyright and the years.
            my $text = 'Copyright ' . $copyright->{years};
            local $Text::Wrap::columns  = $self->{width} + 1;
            local $Text::Wrap::unexpand = 0;
            $text = wrap($prefix, $prefix . q{ } x 4, $text);

            # See if the holder fits on the last line.  If so, add it there;
            # otherwise, add another line.
            my $last_length;
            if (rindex($text, "\n") == -1) {
                $last_length = length($text);
            } else {
                $last_length = length($text) - rindex($text, "\n");
            }
            if ($last_length + length($holder) < $self->{width}) {
                $text .= " $holder";
            } else {
                $text .= "\n" . $prefix . q{ } x 4 . $holder;
            }
            $notice .= $text . "\n";
        }
        chomp($notice);
        return $notice;
    };
    return $copyright;
}

# Returns code to indent each line of a paragraph by a given number of spaces.
# This is constructed as a method returning a closure so that its behavior can
# be influenced by App::DocKnot configuration in the future, but it currently
# doesn't use any configuration.
#
# $self - The App::DocKnot object
#
# Returns: Code reference to a closure
sub _code_for_indent {
    my ($self) = @_;
    my $indent = sub {
        my ($text, $space) = @_;
        my @text = split(m{\n}xms, $text);
        return join("\n", map { q{ } x $space . $_ } @text);
    };
    return $indent;
}

##############################################################################
# Helper methods
##############################################################################

# Internal helper routine to return the path of a file from the application
# data.  These data files are installed with App::DocKnot, but each file can
# be overridden by the user via files in $HOME/.config/docknot or
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
sub _appdata_path {
    my ($self, @path) = @_;

    # Try XDG paths first.
    my $path = config_files(@path);

    # If that doesn't work, use the data that came with the module.
    if (!defined($path)) {
        $path = module_file('App::DocKnot', File::Spec->catfile(@path));
    }
    return $path;
}

# Internal helper routine that locates an application data file, interprets it
# as JSON, and returns the resulting decoded contents.  This uses the relaxed
# parsing mode, so comments and commas after data elements are supported.
#
# $self - The App::DocKnot object
# @path - The path of the file to load, as a list of components
#
# Returns: Anonymous hash or array resulting from decoding the JSON object
#  Throws: slurp or JSON exception on failure to load or decode the object
sub _load_appdata_json {
    my ($self, @path) = @_;
    my $path = $self->_appdata_path(@path);
    my $json = JSON->new;
    $json->relaxed;
    return $json->decode(scalar(slurp($path)));
}

# Internal helper routine to return the path of a file or directory from the
# package metadata directory.  The resulting file or directory path is not
# checked for existence.
#
# $self - The App::DocKnot object
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
# $self - The App::DocKnot object
# @path - The path of the file to load, as a list of components
#
# Returns: The contents of the file as a string
#  Throws: slurp exception on failure to read the file
sub _load_metadata {
    my ($self, @path) = @_;
    return slurp($self->_metadata_path(@path));
}

# Like _load_metadata, but interprets the contents of the metadata file as
# JSON and decodes it, returning the resulting object.  This uses the relaxed
# parsing mode, so comments and commas after data elements are supported.
#
# $self - The App::DocKnot object
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

##############################################################################
# Public interface
##############################################################################

# Create a new App::DocKnot object, which will be used for subsequent calls.
#
# $class - Class of object to create
# $args  - Anonymous hash of arguments with the following keys:
#   width    - Line length at which to wrap output files
#   metadata - Path to the directory containing package metadata
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid metadata directory path
sub new {
    my ($class, $args_ref) = @_;

    # Ensure we were given a valid metadata argument.
    my $metadata = $args_ref->{metadata};
    if (!defined($metadata)) {
        croak('Missing metadata argument to new');
    }
    if (!-d $metadata) {
        croak("Metadata path $metadata does not exist or is not a directory");
    }

    # Create and return the object.
    my $self = {
        metadata => $metadata,
        width    => $args_ref->{width} // 74,
    };
    bless($self, $class);
    return $self;
}

# Generate a documentation file from the package metadata.  Takes the template
# to use and returns the generated documentation.
#
# $self     - The App::DocKnot object
# $template - Name of the documentation template (using Template Toolkit)
#
# Returns: The generated documentation as a string
#  Throws: autodie exception on failure to read metadata or write the output
#          Text exception on Template Toolkit failures
#          Text exception on inconsistencies in the package data
sub generate {
    my ($self, $template) = @_;

    # Load the package metadata from JSON.
    my $data_ref = $self->_load_metadata_json('metadata.json');

    # Load supplemental README sections.  readme.sections will contain a list
    # of sections to add to the README file.
    for my $section (@{ $data_ref->{readme}{sections} }) {
        my $title = $section->{title};

        # The file containing the section data will match the title, converted
        # to lowercase and with spaces changed to dashes.
        my $file = lc($title);
        $file =~ s{ [ ] }{-}xms;

        # Load the section content.
        $section->{body} = $self->_load_metadata('readme', $file);
    }

    # Expand the package license into license text.
    my $license      = $data_ref->{license};
    my $licenses_ref = $self->_load_appdata_json('licenses.json');
    if (!exists($licenses_ref->{$license})) {
        die "Unknown license $license\n";
    }
    my $license_text = slurp($self->_appdata_path('licenses', $license));
    $data_ref->{license} = { %{ $licenses_ref->{$license} } };
    $data_ref->{license}{full} = $license_text;

    # Create the variable information for the template.  Start with all
    # metadata as loaded above.
    my %vars = %{$data_ref};

    # Load the standard sections.
    $vars{blurb}        = $self->_load_metadata('blurb');
    $vars{description}  = $self->_load_metadata('description');
    $vars{requirements} = $self->_load_metadata('requirements');

    # Load Debian summary information.
    $vars{debian}{summary} = $self->_load_metadata('debian', 'summary');

    # Add code references for our defined helper functions.
    $vars{center}    = $self->_code_for_center;
    $vars{copyright} = $self->_code_for_copyright($data_ref->{copyrights});
    $vars{indent}    = $self->_code_for_indent;

    # Find the path to the relevant template.
    $template = $self->_appdata_path('templates', "${template}.tmpl");

    # Run Template Toolkit processing.
    my $tt = Template->new({ ABSOLUTE => 1 }) or die Template->error . "\n";
    my $result;
    $tt->process($template, \%vars, \$result) or die $tt->error . "\n";

    # Word-wrap the results to our width.  This requires some annoying
    # heuristics, but the alternative is to try to get the template to always
    # produce correctly-wrapped results, which is far harder.
    #
    # First, break the resulting text up into paragraphs.  (This will also
    # turn more than two consecutive newlines into just two newlines.)
    my @paragraphs = split(m{ \n(?:[ ]*\n)+ }xms, $result);

    # Add back the trailing newlines at the end of each paragraph.
    @paragraphs = map { $_ . "\n" } @paragraphs;

    # For each paragraph that looks like regular text, which means indented by
    # two or four spaces and consistently on each line, remove the indentation
    # and then add it back in while wrapping the text.
    for my $paragraph (@paragraphs) {
        my ($indent) = ($paragraph =~ m{ \A ([ ]*) \S }xms);

        # If the indent is longer than four characters, leave it alone.
        next if length($indent) > 4;

        # If this looks like a bullet list or thread commands leave it alone.
        next if $paragraph =~ m{ \A \s* [*\\] }xms;

        # If this paragraph is not consistently indented, leave it alone.
        next if $paragraph !~ m{ \A (?: \Q$indent\E \S[^\n]+ \n )+ \z }xms;

        # Strip the indent from each line.
        $paragraph =~ s{ (?: \A | (?<=\n) ) \Q$indent\E }{}xmsg;

        # Remove any existing newlines, preserving two spaces after periods.
        $paragraph =~ s{ [.] \n (\S) }{.  $1}xmsg;
        $paragraph =~ s{ \n(\S) }{ $1}xmsg;

        # Force locally correct configuration of Text::Wrap.
        local $Text::Wrap::break    = qr{\s+}xms;
        local $Text::Wrap::columns  = $self->{width} + 1;
        local $Text::Wrap::unexpand = 0;

        # Do the wrapping.  This modifies @paragraphs in place.
        $paragraph = wrap($indent, $indent, $paragraph);

        # Strip any trailing whitespace, since some gets left behind after
        # periods by Text::Wrap.
        $paragraph =~ s{ [ ]+ \n }{\n}xmsg;
    }

    # Glue the paragraphs back together and return the result.  Because the
    # last newline won't get stripped by the split above, we have to strip an
    # extra newline from the end of the file.
    $result = join("\n", @paragraphs);
    $result =~ s{ \n+ \z }{\n}xms;
    return $result;
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__
