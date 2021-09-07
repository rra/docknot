#!/usr/bin/perl
#
# Test running spin on a tree of files.
#
# Copyright 2021 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use lib 't/lib';

use Capture::Tiny qw(capture_stdout);
use File::Copy::Recursive qw(dircopy);
use File::Spec;
use File::Temp;
use Perl6::Slurp qw(slurp);
use POSIX qw(strftime);
use Test::DocKnot::Spin qw(is_spin_output_tree);

use Test::More;

# Expected output when spinning our tree of input files.
my $EXPECTED_OUTPUT = <<'OUTPUT';
Spinning .../index.html
Updating .../names.png
Creating .../software
Spinning .../software/index.html
Creating .../software/docknot
Spinning .../software/docknot/index.html
Creating .../usefor
Spinning .../usefor/index.html
Creating .../usefor/drafts
Updating .../usefor/drafts/draft-ietf-usefor-message-id-01.txt
Updating .../usefor/drafts/draft-ietf-usefor-posted-mailed-01.txt
Updating .../usefor/drafts/draft-lindsey-usefor-signed-01.txt
Updating .../usefor/drafts/draft-ietf-usefor-useage-01.txt
OUTPUT

require_ok('App::DocKnot::Spin');

# Spin a tree of files.
my $output   = File::Temp->newdir();
my $datadir  = File::Spec->catfile('t',      'data', 'spin');
my $input    = File::Spec->catfile($datadir, 'input');
my $expected = File::Spec->catfile($datadir, 'output');
my $spin     = App::DocKnot::Spin->new({ 'style-url' => '/~eagle/styles/' });
my $stdout   = capture_stdout {
    $spin->spin_tree($input, $output->dirname);
};
my $count = is_spin_output_tree($output, $expected, 'spin_tree');
is($stdout, $EXPECTED_OUTPUT, 'Expected spin_tree output');

# Create a bogus file in the output tree.
my $bogus      = File::Spec->catfile($output->dirname, 'bogus');
my $bogus_file = File::Spec->catfile($bogus,           'some-file');
mkdir($bogus);
open(my $fh, '>', $bogus_file);
print {$fh} "Some stuff\n" or die "Cannot write to $bogus_file: $!\n";
close($fh);

# Spinning the same tree of files again should do nothing because of the
# modification timestamps.
$stdout = capture_stdout {
    $spin->spin_tree($input, $output->dirname);
};
is($stdout, q{}, 'Spinning again does nothing');

# The extra file shouldn't be deleted.
ok(-d $bogus, 'Stray file and directory not deleted');

# Reconfigure spin to enable deletion, and run it again.  The only action
# taken should be to delete the stray file.
$spin
  = App::DocKnot::Spin->new({ delete => 1, 'style-url' => '/~eagle/styles/' });
$stdout = capture_stdout {
    $spin->spin_tree($input, $output->dirname);
};
is(
    $stdout,
    "Deleting .../bogus/some-file\nDeleting .../bogus\n",
    'Spinning with delete option cleans up',
);
ok(!-e $bogus, 'Stray file and directory was deleted');

# Copy the input tree to a new temporary directory and regenerate output files
# with the new timestamps.
my $tmpdir = File::Temp->newdir();
dircopy($input, $tmpdir)
  or die "Cannot copy $input to $tmpdir: $!\n";
capture_stdout {
    $spin->spin_tree($tmpdir->dirname, $output->dirname);
};

# Now, update the .versions file at the top of the input tree to change the
# timestamp to a second into the future.  This should force regeneration of
# only the software/docknot/index.html file.
my $versions_path = File::Spec->catfile($tmpdir->dirname, '.versions');
my $versions      = slurp($versions_path);
my $new_date      = strftime('%Y-%m-%d %T', localtime(time() + 1));
$versions =~ s{ \d{4}-\d\d-\d\d [ ] [\d:]+ }{$new_date}xms;
open(my $versions_fh, '>', $versions_path);
print {$versions_fh} $versions or die "Cannot write to $versions_path: $!\n";
close($versions_fh);
$stdout = capture_stdout {
    $spin->spin_tree($tmpdir->dirname, $output->dirname);
};
is(
    $stdout,
    "Spinning .../software/docknot/index.html\n",
    'Spinning again regenerates the DocKnot page',
);

# Report the end of testing.
done_testing($count + 7);
