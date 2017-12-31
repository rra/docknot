#!/usr/bin/perl
#
# Tests for license metadata.
#
# Written by Russ Allbery <rra@cpan.org>
# Copyright 2017 Russ Allbery <rra@cpan.org>
#
# See LICENSE for licensing terms.

use 5.018;
use autodie;
use warnings;

use File::ShareDir qw(module_file);
use File::Spec;
use JSON;
use Perl6::Slurp;
use Test::More;

# Load the module.
BEGIN { use_ok('App::DocKnot') }

# Load the licenses.json file.
my $path = module_file('App::DocKnot', 'licenses.json');
my $json = JSON->new;
$json->relaxed;
my $licenses_ref = $json->decode(scalar(slurp($path)));

# The number of tests will be one plus two times the number of licenses.
my $num_tests = 1 + 2 * keys(%{$licenses_ref});

# Ensure that, for every license listed in this file, there is a summary and a
# corresponding file containing license text.
for my $key (sort keys(%{$licenses_ref})) {
    ok(defined($licenses_ref->{$key}{summary}), "summary for $key");
    my $license = File::Spec->catfile('licenses', $key);
    eval { $path = module_file('App::DocKnot', $license) };
    ok(!$@, "license file for $key");
}

# Check the number of tests was correct.
done_testing($num_tests);
