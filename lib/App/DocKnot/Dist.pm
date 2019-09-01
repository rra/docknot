# Generate a distribution tarball for a package.
#
# This is the implementation of the docknot dist command, which determines and
# runs the commands necessary to build a distribution tarball for a given
# package.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Dist 3.01;

use 5.024;
use autodie;
use warnings;

use App::DocKnot::Config;
use Carp qw(croak);
use Cwd qw(getcwd);
use File::Copy qw(move);
use File::Path qw(remove_tree);
use IPC::Run qw(run);
use IPC::System::Simple qw(systemx);

# Base commands to run for various types of distributions.  Additional
# variations may be added depending on additional configuration parameters.
#<<<
our %COMMANDS = (
    'Autoconf' => [
        ['./bootstrap'],
        ['./configure', 'CC=clang'],
        ['make', 'warnings'],
        ['make', 'check'],
        ['make', 'clean'],
        ['./configure', 'CC=gcc'],
        ['make', 'warnings'],
        ['make', 'check'],
        ['make', 'clean'],
        ['make', 'check-cppcheck'],
        ['make', 'distcheck'],
    ],
    'ExtUtils::MakeMaker' => [
        ['perl', 'Makefile.PL'],
        ['make', 'disttest'],
        ['make', 'dist'],
    ],
    'Module::Build' => [
        ['perl', 'Build.PL'],
        ['./Build', 'disttest'],
        ['./Build', 'dist'],
    ],
    'make' => [
        ['make', 'dist'],
    ],
);
#>>>

##############################################################################
# Helper methods
##############################################################################

# Given a source directory, a prefix for tarballs and related files (such as
# signatures), and a destination directory, move all matching files from the
# source directory to the destination directory.
#
# $self        - The App::DocKnot::Dist object
# $source_path - The source directory path
# $prefix      - The tarball file prefix
# $dest_path   - The destination directory path
#
# Throws: Text exception if no files are found
#         Text exception on failure to move a file
sub _move_tarballs {
    my ($self, $source_path, $prefix, $dest_path) = @_;

    # Find all matching files.
    my $pattern = qr{ \A \Q$prefix\E - \d.* [.]tar [.][xg]z \z }xms;
    opendir(my $source, $source_path);
    my @files = grep { $_ =~ $pattern } readdir($source);
    closedir($source);

    # Move the files.
    for my $file (@files) {
        my $source_file = File::Spec->catfile($source_path, $file);
        move($source_file, $dest_path)
          or die "cannot move $source_file to $dest_path: $!\n";
    }
    return;
}

# Given a command with arguments, replace a command of "perl" with the
# configured path to Perl, if any.  Assumes that the perl configuration
# parameter is set in the object and should not be called if this is not true.
#
# $self        - The App::DocKnot::Dist object
# $command_ref - Reference to an array representing a command with arguments
#
# Returns: Reference to an array representing a command with arguments, with
#          the command replaced with the configured path to Perl if it was
#          "perl"
sub _replace_perl_path {
    my ($self, $command_ref) = @_;
    if ($command_ref->[0] ne 'perl') {
        return $command_ref;
    }
    my @command = $command_ref->@*;
    $command[0] = $self->{perl};
    return [@command];
}

##############################################################################
# Public interface
##############################################################################

# Create a new App::DocKnot::Dist object, which will be used for subsequent
# calls.
#
# $class - Class of object ot create
# $args  - Anonymous hash of arguments with the following keys:
#   distdir  - Path to the directory for distribution tarball
#   metadata - Path to the directory containing package metadata
#   perl     - Path to Perl to use (default: search the user's PATH)
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid metadata directory path
#          Text exception on missing or invalid distdir argument
sub new {
    my ($class, $args_ref) = @_;

    # Create the config reader.
    my %config_args;
    if ($args_ref->{metadata}) {
        $config_args{metadata} = $args_ref->{metadata};
    }
    my $config = App::DocKnot::Config->new(\%config_args);

    # Ensure we were given a valid distdir argument.
    my $distdir = $args_ref->{distdir};
    if (!defined($distdir)) {
        croak('distdir path not given');
    } elsif (!-d $distdir) {
        croak("distdir path $distdir does not exist or is not a directory");
    }

    # Create and return the object.
    my $self = {
        config  => $config->config(),
        distdir => $distdir,
        perl    => $args_ref->{perl},
    };
    bless($self, $class);
    return $self;
}

# Analyze a source directory and return the list of commands to run to
# generate a distribution tarball.
#
# $self - The App::DocKnot::Dist object
#
# Returns: List of commands, each of which is a list of strings representing
#          a command and its arguments
sub commands {
    my ($self)   = @_;
    my $type     = $self->{config}{build}{type};
    my @commands = map { [@$_] } $COMMANDS{$type}->@*;

    # Special-case: If a specific path to Perl was configured, use that path
    # rather than searching for perl in the user's PATH.  This is used
    # primarily by the test suite, which wants to run a Module::Build Build.PL
    # and thus has to use the same perl binary as the one running the tests.
    if (defined($self->{perl})) {
        @commands = map { $self->_replace_perl_path($_) } @commands;
    }

    # Special-case: Autoconf packages with C++ support should also attempt a
    # build with a C++ compiler.
    if ($type eq 'Autoconf' && $self->{config}{build}{cplusplus}) {
        #<<<
        my @extra = (
            ['./configure', 'CC=g++'],
            ['make', 'check'],
            ['make', 'clean'],
        );
        #>>>
        splice(@commands, 1, 0, @extra);
    }

    # Special-case: Autoconf packages with Valgrind support should also run
    # make check-valgrind.
    if ($type eq 'Autoconf' && $self->{config}{build}{valgrind}) {
        splice(@commands, -3, 0, ['make', 'check-valgrind']);
    }

    return @commands;
}

# Generate a distribution tarball.  This assumes it is run from the root
# directory of the package to release and that it is a Git repository.  It
# exports the Git repository, runs the commands to generate the tarball, and
# then removes the working tree.
#
# $self - The App::DocKnot::Dist object
#
# Throws: Text exception if any of the commands fail
sub make_distribution {
    my ($self) = @_;

    # Determine the source directory and the distribution directory name.
    my $source = getcwd() or die "cannot get current directory: $!\n";
    my $prefix = $self->{config}{distribution}{tarname};

    # If the distribution directory name already exists, remove it.  Automake
    # may have made parts of it read-only, so be forceful in the removal.
    # Note that this does not pass the safe parameter and therefore should not
    # be called on attacker-controlled directories.
    chdir($self->{distdir});
    if (-d $prefix) {
        remove_tree($prefix);
    }

    # Export the Git repository into a new directory.
    my @git = ('git', 'archive', "--remote=$source", "--prefix=${prefix}/",
        'master',);
    my @tar = qw(tar xf -);
    run(\@git, q{|}, \@tar) or die "@git | @tar failed with status $?\n";

    # Change to that directory and run the configured commands.
    chdir($prefix);
    for my $command_ref ($self->commands()) {
        systemx($command_ref->@*);
    }

    # Move the generated tarball to the parent directory.
    $self->_move_tarballs(File::Spec->curdir(), $prefix, File::Spec->updir());

    # Remove the working tree.
    chdir(File::Spec->updir());
    remove_tree($prefix, { safe => 1 });
    return;
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT sublicense JSON CPAN ARGS
distdir Automake

=head1 NAME

App::DocKnot::Dist - Prepare a distribution tarball

=head1 SYNOPSIS

    use App::DocKnot::Dist;
    my $docknot = App::DocKnot::Dist->new({ distdir => '/path/to/dist' });
    $docknot->make_distribution();

=head1 REQUIREMENTS

Perl 5.24 or later and the modules File::BaseDir, File::ShareDir, IPC::Run,
IPC::System::Simple, JSON, and Perl6::Slurp, all of which are available from
CPAN.

=head1 DESCRIPTION

This component of DocKnot generates distribution tarballs for a package.  This
is a bit of an odd inclusion in the DocKnot suite, since it's not about
generating documentation, but it uses the same configuration and metadata as
the rest of DocKnot.

Specifically, App::DocKnot::Dist exports the current branch from Git into a
separate working directory, runs the commands appropriate to create a
distribution (based on the build system configured in the package metadata),
and cleans up the working directory.

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Dist object.  This should be used for all
subsequent actions.  ARGS should be a hash reference with one or more of the
following keys:

=over 4

=item distdir

The path to the directory into which to put the distribution tarball.  This
should point to a trusted directory, not one where an attacker could have
written files (see make_distribution() below).  Required.

=item metadata

The path to the directory containing metadata for a package.  Default:
F<docs/metadata> relative to the current directory.

=item perl

The path to the Perl executable to use for build steps that require it.  Used
primarily in the test suite.  Default: The binary named C<perl> on the user's
PATH.

=back

=back

=head1 INSTANCE METHODS

=over 4

=item commands()

Return the commands that should be run to generate a distribution tarball as a
reference to an array of arrays.  Each included array is a single command.
This method is provided primarily for testing convenience and is normally just
an implementation detail of make_distribution().

=item make_distribution()

Generate a distribution tarball in the C<destdir> directory provided to new().

If C<destdir> already contains a subdirectory whose name matches the
C<tarname> of the distribution, it will be forcibly removed.  In order to
successfully remove trees that result from Automake's C<make distcheck>
failing partway through, App::DocKnot::Dist will change permissions as needed
to remove an existing directory.  For security reasons, the C<distdir>
parameter of this module should therefore only be pointed to a trusted
directory, not one where an attacker could have written files.

=back

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2019 Russ Allbery <rra@cpan.org>

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
