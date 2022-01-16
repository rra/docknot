# Release a distribution tarball for a package.
#
# This is the implementation of the docknot release command, which copies a
# release tarball (normally generated by docknot dist) into a publication
# area, archives old versions, and updates the .versions database for spin.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Release 6.01;

use 5.024;
use autodie;
use warnings;

use App::DocKnot::Config;
use App::DocKnot::Spin::Versions;
use App::DocKnot::Util qw(latest_tarball);
use Carp qw(croak);
use Path::Tiny qw(path);

##############################################################################
# Public interface
##############################################################################

# Create a new App::DocKnot::Release object, which will be used for subsequent
# calls.
#
# $args_ref - Anonymous hash of arguments with the following keys:
#   archivedir - Path to the archive directory
#   distdir    - Path to where docknot dist puts distribution tarballs
#   metadata   - Path to the package metadata
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid package metadata
#          Text exceptions on invalid global configuration
#          Text exceptions on invalid distdir argument
sub new {
    my ($class, $args_ref) = @_;

    # Create the config reader.
    my %config_args;
    if ($args_ref->{metadata}) {
        $config_args{metadata} = $args_ref->{metadata};
    }
    my $config_reader = App::DocKnot::Config->new(\%config_args);

    # Load the global and package configuration.
    my $global_config_ref = $config_reader->global_config();
    my $config_ref = $config_reader->config();

    # Ensure we were given a valid archivedir and distdir arguments if they
    # were not set in the global configuration.
    my $archivedir = $args_ref->{archivedir}
      // $global_config_ref->{archivedir};
    if (!defined($archivedir)) {
        croak('archivedir path not given');
    } elsif (!-d $archivedir) {
        croak(
            "archivedir path $archivedir does not exist or is not a directory",
        );
    }
    my $distdir = $args_ref->{distdir} // $global_config_ref->{distdir};
    if (!defined($distdir)) {
        croak('distdir path not given');
    } elsif (!-d $distdir) {
        croak("distdir path $distdir does not exist or is not a directory");
    }

    # Build an App::DocKnot::Spin::Versions object if configured with a path
    # to a versions database.
    my $versions;
    if ($global_config_ref->{versions}) {
        my $versions_path = path($global_config_ref->{versions});
        $versions = App::DocKnot::Spin::Versions->new($versions_path);
    }

    # Create and return the object.
    #<<<
    my $self = {
        archivedir   => path($archivedir),
        distdir      => path($distdir),
        package      => $config_ref->{name},
        section      => $config_ref->{distribution}{section},
        tarname      => $config_ref->{distribution}{tarname},
        version_name => $config_ref->{distribution}{version},
        versions     => $versions,
    };
    #>>>
    bless($self, $class);
    return $self;
}

# Release a new version and update .versions if so configured.
#
# Throws: Text exception on any failures
sub release {
    my ($self) = @_;
    my $tarball_ref = latest_tarball($self->{distdir}, $self->{tarname});
    if (!defined($tarball_ref)) {
        croak("no release of $self->{tarname} found in $self->{distdir}");
    }

    # Archive old versions.  This is only done if the current version in the
    # archive directory is different than the version we're about to release.
    # If it is not, we overwrite the version in the archive directory, since
    # we assume we're replacing a release.
    my $current_path = $self->{archivedir}->child($self->{section});
    my $current_ref = latest_tarball($current_path, $self->{tarname});
    if (defined($current_ref)) {
        if ($current_ref->{version} ne $tarball_ref->{version}) {
            my $old_root = $self->{archivedir}->child('ARCHIVE');
            my $old_path = $old_root->child($self->{tarname});
            $old_path->mkpath();
            for my $file ($current_ref->{files}->@*) {
                $current_path->child($file)->move($old_path->child($file));
            }
        }
    }

    # Copy the new version into place and update the symlinks.
    $current_path->mkpath();
    for my $file ($tarball_ref->{files}->@*) {
        $self->{distdir}->child($file)->copy($current_path->child($file));
        my $generic_name = $file;
        $generic_name =~ s{ \A (\Q$self->{tarname}\E) - [\d.]+ [.] }{$1.}xms;
        my $generic_path = $current_path->child($generic_name);
        $generic_path->remove();
        symlink($file, $generic_path);
    }

    # Update the .versions file.
    if ($self->{versions}) {
        my $name = $self->{version_name};
        my $version = $tarball_ref->{version};
        my $date = $tarball_ref->{date};
        $self->{versions}->update_version($name, $version, $date);
    }
    return;
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT sublicense archivedir distdir

=head1 Name

App::DocKnot::Release - Release a distribution tarball

=head1 SYNOPSIS

    use App::DocKnot::Release;
    my $docknot = App::DocKnot::Release->new();
    $docknot->release();

=head1 REQUIREMENTS

Perl 5.24 or later and the modules File::BaseDir, File::ShareDir,
Git::Repository, Path::Tiny, and YAML::XS, all of which are available from
CPAN.

=head1 DESCRIPTION

This component of DocKnot releases a distribution tarball (normally created by
C<docknot dist> or App::DocKnot::Dist), maintains a software distribution
directory, and updates a version and release date database.

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Release object.  This should be used for all
subsequent actions.  ARGS should be a hash reference with one or more of the
following keys:

=over 4

=item archivedir

The release area into which to put the distribution tarball.  The current
distribution will be put in a subdirectory named after the
C<distribution.section> key in the package configuration.  Older versions will
be moved to the F<ARCHIVE> subdirectory of I<archivedir>.  Required if not set
in the global configuration file.

=item distdir

The directory from which to get the new distribution tarball, normally
generated by C<docknot dist>.  The latest version in this directory will be
used.  Required if not set in the global configuration file.

=item metadata

The path to the metadata for the package on which to operate.  Default:
F<docs/docknot.yaml> relative to the current directory.

=back

=back

=head1 INSTANCE METHODS

=over 4

=item release()

Copy the distribution tarball (in multiple formats, with PGP signatures) into
a release area, updates symlink pointing to the latest version, and move any
old release to an archive area.

If C<versions> is set in the global configuration file, updates the
F<.versions> file found at that path with the new release version and release
date.  See L<App::DocKnot::Spin::Versions> for more information about
F<.versions> files.

=back

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2022 Russ Allbery <rra@cpan.org>

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

L<docknot(1)>, L<App::DocKnot::Config>, L<App::DocKnot::Dist>,
L<App::DocKnot::Spin::Versions>

This module is part of the App-DocKnot distribution.  The current version of
DocKnot is available from CPAN, or directly from its web site at
L<https://www.eyrie.org/~eagle/software/docknot/>.

=cut

# Local Variables:
# copyright-at-end-flag: t
# End:
