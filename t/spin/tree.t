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

use File::Spec;
use File::Temp;
use Test::DocKnot::Spin qw(is_spin_output_tree);

use Test::More;

require_ok('App::DocKnot::Spin');

# Spin a tree of files.
my $output   = File::Temp->newdir();
my $datadir  = File::Spec->catfile('t',      'data', 'spin');
my $input    = File::Spec->catfile($datadir, 'input');
my $expected = File::Spec->catfile($datadir, 'output');
my $spin     = App::DocKnot::Spin->new({ 'style-url' => '/~eagle/styles/' });
$spin->spin_tree($input, $output->dirname);
my $count = is_spin_output_tree($output, $expected, 'spin_tree');

# Report the end of testing.
done_testing($count + 1);
