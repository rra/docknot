#!/usr/bin/perl
#
# Test running spin on a single file.
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
use Perl6::Slurp qw(slurp);
use Test::DocKnot::Spin qw(is_spin_output);

use Test::More tests => 2;

require_ok('App::DocKnot::Spin');

# Spin a single file.
my $tempfile = File::Temp->new();
my $datadir  = File::Spec->catfile('t',      'data',   'spin');
my $input    = File::Spec->catfile($datadir, 'input',  'index.th');
my $expected = File::Spec->catfile($datadir, 'output', 'index.html');
my $spin     = App::DocKnot::Spin->new({ 'style-url' => '/~eagle/styles/' });
$spin->spin_file($input, $tempfile->filename);
is_spin_output($tempfile, $expected, 'Single file conversion');
