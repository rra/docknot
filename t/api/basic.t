#!/usr/bin/perl
#
# Tests for the App::DocKnot module API.
#
# Written by Russ Allbery <rra@stanford.edu>
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
use Test::More tests => 3;

# Load the module.
BEGIN { use_ok('App::DocKnot') }

# We have a set of test cases in the data directory.  Each of them contains
# metadata and output directories.
my $dataroot = File::Spec->catfile('t', 'data');
opendir(my $tests, File::Spec->catfile('t', 'data'));
my @tests = File::Spec->no_upwards(readdir($tests));
closedir($tests);
@tests = grep { -d File::Spec->catfile($dataroot, $_, 'metadata') } @tests;

# For each of those cases, initialize an object from the metadata directory,
# generate a file from the readme template, and compare that with the readme
# in the output directory.
for my $test (@tests) {
    my $path = File::Spec->catfile($dataroot, $test, 'metadata');
    my $docknot = App::DocKnot->new({ metadata => $path });
    isa_ok($docknot, 'App::DocKnot', "for $test");
    my $got = $docknot->generate('readme');
    $path = File::Spec->catfile($dataroot, $test, 'output', 'readme');
    my $expected = slurp($path);
    if ($got eq $expected) {
        is($got, $expected, "README for $test");
    } else {
        open(my $tmp, q{>}, 'tmp');
        print {$tmp} $got or die "Cannot write to tmp: $!\n";
        close($tmp);
        my $diff = '# ' . capturex([0..1], 'diff', '-u', $path, 'tmp');
        diag($diff);
        unlink('tmp');
        ok(0, "README for $test");
    }
}
