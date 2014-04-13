#!/usr/bin/perl
#
# Tests for the App::DocKnot module API.
#
# Written by Russ Allbery <rra@cpan.org>
# Copyright 2013
#     The Board of Trustees of the Leland Stanford Junior University
#
# See LICENSE for licensing terms.

use 5.018;
use autodie;
use warnings;

use File::Spec;
use IPC::System::Simple qw(capturex);
use Perl6::Slurp;
use Test::More tests => 4;

# Load the module.
BEGIN { use_ok('App::DocKnot') }

# Helper function to compare a string to the contents of a file, similar to
# the standard is() function, but to show the line-based unified diff between
# them if they differ.
#
# $got      - The output that we received
# $expected - The path to the file containing the expected output
# $message  - The message to use when reporting the test results
#
# Returns: undef
#  Throws: Exception on failure to read or write files or run diff
sub is_diff {
    my ($got, $expected, $message) = @_;

    # If they're equal, this is simple.
    my $data = slurp($expected);
    if ($got eq $data) {
        is($got, $data, $message);
        return;
    }

    # They're not equal.  Write out what we got so that we can run diff.
    open(my $tmp, q{>}, 'tmp');
    print {$tmp} $got or die "Cannot write to tmp: $!\n";
    close($tmp);

    # Run diff and report the results.
    my $diff = capturex([0..1], 'diff', '-u', $expected, 'tmp');
    diag($diff);

    # Remove the temporary file and report failure.
    unlink('tmp');
    ok(0, $message);
    return;
}

# We have a set of test cases in the data directory.  Each of them contains
# metadata and output directories.
my $dataroot = File::Spec->catfile('t', 'data');
opendir(my $tests, File::Spec->catfile('t', 'data'));
my @tests = File::Spec->no_upwards(readdir($tests));
closedir($tests);
@tests = grep { -d File::Spec->catfile($dataroot, $_, 'metadata') } @tests;

# For each of those cases, initialize an object from the metadata directory,
# generate file from known templates, and compare that with the corresponding
# output file.
for my $test (@tests) {
    my $metadata_path = File::Spec->catfile($dataroot, $test, 'metadata');
    my $docknot = App::DocKnot->new({ metadata => $metadata_path });
    isa_ok($docknot, 'App::DocKnot', "for $test");

    # Loop through the possible templates.
    for my $template (qw(readme thread)) {
        my $got = $docknot->generate($template);
        my $path = File::Spec->catfile($dataroot, $test, 'output', $template);
        is_diff($got, $path, "README for $test");
    }
}
