#!/usr/bin/perl
#
# Tests for the App::DocKnot command dispatch for spin and spin-file.
#
# Copyright 2021 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use lib 't/lib';

use Capture::Tiny qw(capture_stdout);
use File::Temp;
use File::Spec;
use Test::RRA qw(is_file_contents);
use Test::DocKnot::Spin qw(is_spin_output is_spin_output_tree);

use Test::More;

# Load the module.
BEGIN { use_ok('App::DocKnot::Command') }

# Create the command-line parser.
my $docknot = App::DocKnot::Command->new();
isa_ok($docknot, 'App::DocKnot::Command');

# Create a temporary directory for test output.
my $tempdir = File::Temp->newdir();

# Spin a single file.
my $datadir  = File::Spec->catfile('t',      'data',   'spin');
my $input    = File::Spec->catfile($datadir, 'input',  'index.th');
my $expected = File::Spec->catfile($datadir, 'output', 'index.html');
my $output   = File::Spec->catfile($tempdir->dirname, 'index.html');
$docknot->run('spin-file', '-s', '/~eagle/styles', $input, $output);
is_spin_output($output, $expected, 'spin-file (output specified)');

# Spin a single file to standard output.
my $stdout = capture_stdout {
    $docknot->run('spin-file', '-s', '/~eagle/styles', $input);
};
open(my $output_fh, '>', $output);
print {$output_fh} $stdout or BAIL_OUT("Cannot write to $output: $!");
close($output_fh);
is_spin_output($output, $expected, 'spin-file (standard output)');

# Spin a tree of files.
$input    = File::Spec->catfile($datadir, 'input');
$expected = File::Spec->catfile($datadir, 'output');
$docknot->run('spin', '-s', '/~eagle/styles', $input, $tempdir->dirname);
my $count = is_spin_output_tree($tempdir->dirname, $expected, 'spin');

# Report the end of testing.
done_testing($count + 4);
