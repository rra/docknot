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

use Cwd qw(getcwd);
use File::Spec;
use File::Temp;
use Perl6::Slurp qw(slurp);
use Test::RRA qw(is_file_contents);

use Test::More tests => 2;

require_ok('App::DocKnot::Spin');

# Record the current working directory, since spin currently changes it.
my $cwd = getcwd();

# Spin a single file.
my $tempfile = File::Temp->new();
my $datadir  = File::Spec->catfile('t',      'data',   'spin');
my $input    = File::Spec->catfile($datadir, 'input',  'index.th');
my $expected = File::Spec->catfile($datadir, 'output', 'index.html');
my $spin     = App::DocKnot::Spin->new({ 'style-url' => '/~eagle/styles/' });
$spin->spin_command($input, $tempfile->filename);

# Go back to the previous working directory, since spin_command currently
# changes directories.  Strip timestamps before comparing results.
chdir($cwd);
my $results = slurp($tempfile);
$results =~ s{
    [ ] \d{4}-\d\d-\d\d (?: [ ] \d\d:\d\d:\d\d [ ] -0000 )?
}{ %DATE%}gxms;
$results =~ s{ spin [ ] \d+ [.] \d+ }{spin %VERSION%}xms;
is_file_contents($results, $expected, 'Single file conversion');
