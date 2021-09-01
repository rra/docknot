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

use Cwd qw(getcwd);
use File::Compare qw(compare);
use File::Find qw(find);
use File::Spec;
use File::Temp;
use Perl6::Slurp qw(slurp);
use Test::DocKnot::Spin qw(is_spin_output);

use Test::More;

require_ok('App::DocKnot::Spin');

# Record the current working directory, since spin currently changes it.
my $cwd = getcwd();

# Spin a tree of files.
my $output   = File::Temp->newdir();
my $datadir  = File::Spec->catfile('t',      'data', 'spin');
my $input    = File::Spec->catfile($datadir, 'input');
my $expected = File::Spec->catfile($datadir, 'output');
my $spin     = App::DocKnot::Spin->new({ 'style-url' => '/~eagle/styles/' });
$spin->spin_command($input, $output->dirname);

# Holds all the files seen in the output tree.
my %seen;

# Function that compares each of the output files in the tree, called from
# File::Find on the output directory.
sub check_output {
    my $file = $_;
    return if -d $file;

    # Determine the relative path and mark it as seen.
    my $path = File::Spec->abs2rel($File::Find::name, $output);
    $seen{$path} = 1;

    # Find the corresponding expected file.
    my $expected_file
      = File::Spec->rel2abs(File::Spec->catfile($expected, $path), $cwd);

    # Compare HTML output using is_spin_output and all other files as copies.
    if ($file =~ m{ [.] html \z }xms) {
        is_spin_output($file, $expected_file, $path);
    } else {
        is(compare($file, $expected_file), 0, "$path (binary)");
    }
    return;
}

# Compare the output.
find(\&check_output, $output);
my $count = keys(%seen);

# Missing files from the output.
my @missing;

# Function that checks that every file in the expected output tree was seen in
# the generated output tree, called from File::Find on the expected directory.
sub check_files {
    my $file = $_;
    return if -d $file;

    # Determine the relative path and make sure it was in the %seen hash.
    my $path = File::Spec->abs2rel($File::Find::name, $expected);
    if ($seen{$path}) {
        delete $seen{$path};
    } else {
        push(@missing, $path);
    }
    return;
}

# Check that there aren't any missing files.
find(\&check_files, $expected);
is_deeply(\@missing, [], 'All expected files generated');

# Report the end of testing.
done_testing($count + 2);
