#!/usr/bin/perl
#
# Test errors generated by App::DocKnot::Spin::Thread.
#
# Copyright 2021 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use Capture::Tiny qw(capture);
use File::Spec;
use File::Temp;

use Test::More tests => 2;

# Expected errors from spinning the error file.  The line numbers are still
# not entirely correct because line tracking is very complicated and still not
# entirely correct.
my $EXPECTED_ERRORS = <<'ERRORS';
errors.th:1: cannot find argument 2: Did not find opening bracket after prefix: "\s*", detected at offset 2
errors.th:3: invalid macro placeholder \2 (greater than 1)
errors.th:5: invalid macro argument count for \badcount
errors.th:9: unknown variable \=UNKNOWN
errors.th:11: unknown command or macro \unknown
errors.th:14: space in anchor "#foo bar"
errors.th:15: no package release information available
errors.th:16: no sitemap file found
errors.th:17: no package version information available
errors.th:17: cannot stat file nonexistent-file
ERRORS

require_ok('App::DocKnot::Spin::Thread');

# Spin the errors file with output captured.
my $input = File::Spec->catfile('t', 'data', 'spin', 'errors', 'errors.th');
my $spin = App::DocKnot::Spin::Thread->new();
my ($stdout, $stderr) = capture {
    $spin->spin_thread_file($input);
};

# Simplify the file name, and then check against the expected output.
$stderr =~ s{ ^ [^:]+/errors[.]th: }{errors.th:}xmsg;
$stderr =~ s{ (cannot [ ] stat [ ] file [ ]) /[^:]+/([^/:]+) : .* }{$1$2\n}xms;
is($stderr, $EXPECTED_ERRORS, 'errors are correct');
