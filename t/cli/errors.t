#!/usr/bin/perl
#
# Tests for the App::DocKnot command dispatch error handling.
#
# Copyright 2018 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use Test::More tests => 7;

# Load the module.
BEGIN { use_ok('App::DocKnot') }

# Check an error against the expected message, removing the trailing newline
# and stripping off the leading $0 that's prepended and the colon and space
# directly following it, if any.
#
# $error     - The error to check
# $expected  - The expected error message
# $test_name - Additional test name information
#
# Returns: undef
sub is_error {
    my ($error, $expected, $test_name) = @_;
    chomp($error);
    $error =~ s{ \A \Q$0\E :? [ ]? }{}xms;
    is($error, $expected, $test_name);
    return;
}

# Create the command-line parser.
my $docknot = App::DocKnot->new();
isa_ok($docknot, 'App::DocKnot');

# Test various errors.
eval { $docknot->run('foo') };
is_error($@, 'unknown command foo', 'Unknown command');
local @ARGV = ();
eval { $docknot->run() };
is_error($@, 'no subcommand given', 'No subcommand');
eval { $docknot->run('generate', '-f', 'readme') };
is_error($@, 'generate: unknown option: f', 'Unknown option');
eval { $docknot->run('generate') };
is_error($@, 'generate: too few arguments', 'Too few arguments');
eval { $docknot->run('generate', 'a', 'b', 'c') };
is_error($@, 'generate: too many arguments', 'Too many arguments');
