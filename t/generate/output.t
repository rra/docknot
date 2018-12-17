#!/usr/bin/perl
#
# Test the generate_output method.
#
# Copyright 2016, 2018 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use lib 't/lib';

use Cwd qw(getcwd);
use File::Spec;
use File::Temp;
use Perl6::Slurp;
use Test::RRA qw(is_file_contents);

use Test::More tests => 5;

# Load the module.
BEGIN {
    use_ok('App::DocKnot');
    use_ok('App::DocKnot::Generate');
}

# Initialize the App::DocKnot object using the default metadata path.
my $metadata_path = File::Spec->catfile(getcwd(), 'docs', 'metadata');
my $docknot = App::DocKnot::Generate->new({ metadata => $metadata_path });
isa_ok($docknot, 'App::DocKnot::Generate');

# Write the README output for the DocKnot package to a temporary file.
my $tmp     = File::Temp->new();
my $tmpname = $tmp->filename;
$docknot->generate_output('readme', $tmpname);
my $output = slurp($tmp);
is_file_contents($output, 'README', 'README in package');

# Test default output destinations by creating a temporary directory and then
# generating the README file without an explicit output location.
my $tmpdir = File::Temp->newdir();
chdir($tmpdir);
$docknot->generate_output('readme');
$output = slurp('README');
is_file_contents($output, 'README', 'README in package (default filename)');

# Allow cleanup to delete our temporary directory.
chdir(File::Spec->rootdir());
