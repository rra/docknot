#!/usr/bin/perl
#
# Basic tests for App::DocKnot::Dist.
#
# Copyright 2019 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use Cwd qw(getcwd);
use File::Copy;
use File::Spec;
use File::Temp;
use IPC::Run qw(run);
use IPC::System::Simple qw(systemx);

use Test::More;

# Find the full path to the test data.
my $cwd      = getcwd() or die "$0: cannot get working directory: $!\n";
my $dataroot = File::Spec->catfile($cwd, 't', 'data', 'dist');

# Set up a temporary directory, copy all files from the data directory, and
# commit them.  We have to rename the test while we copy it to avoid having it
# picked up by the main package test suite.
my $dir = File::Temp->newdir();
chdir($dir);
systemx(qw(git init source));
chdir('source');
for my $file (qw(Build.PL MANIFEST MANIFEST.SKIP)) {
    copy(File::Spec->catfile($dataroot, $file), File::Spec->curdir())
      or die "$0: cannot copy $file: $!\n";
}
mkdir('docs');
mkdir(File::Spec->catfile('docs', 'metadata'));
my $metadata_path
  = File::Spec->catfile($dataroot, 'docs', 'metadata', 'metadata.json');
copy($metadata_path, File::Spec->catfile('docs', 'metadata'))
  or die "$0: cannot copy docs/metadata/metadata.json: $!\n";
for my $file (qw(blurb description requirements)) {
    open(my $fh, '>', File::Spec->catfile('docs', 'metadata', $file));
    close($fh);
}
mkdir('lib');
copy(File::Spec->catfile($dataroot, 'lib', 'Empty.pm'), 'lib')
  or die "$0: cannot copy lib/Empty.pm: $!\n";
mkdir('t');
my $testdir = File::Spec->catfile('t', 'api');
mkdir($testdir);
my $test_path = File::Spec->catfile($dataroot, 't', 'api', 'empty.t.in');
copy($test_path, File::Spec->catfile($testdir, 'empty.t'))
  or die "$0: cannot copy t/api/empty.t: $!\n";
systemx(qw(git add -A .));
systemx(qw(git commit -m Initial));

# Check whether we have all the necessary tools to set up the test.  This test
# relies on the external git and tar utilities, which may not be available on
# all systems.
if (!run(['git', 'archive', 'HEAD'], q{|}, ['tar', 'tf', q{-}])) {
    plan skip_all => 'git and tar not available';
} else {
    plan tests => 2;
}

# Load the module.  Change back to the starting directory for this so that
# coverage analysis works.
chdir($cwd);
require_ok('App::DocKnot::Dist');
chdir(File::Spec->catfile($dir, 'source'));

# Setup finished.  Now we can create a distribution tarball.
my $distdir = File::Spec->catfile($dir, 'dist');
mkdir($distdir);
my $dist = App::DocKnot::Dist->new({ distdir => $distdir });
$dist->make_distribution();
ok(-f File::Spec->catfile($distdir, 'Empty-1.00.tar.gz'), 'dist exists');

# Change directories so that the temporary directory can be cleaned up.
chdir($cwd);
