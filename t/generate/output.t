#!/usr/bin/perl
#
# Test the generate_output method.  This doubles as a test for whether the
# package metadata is consistent with the files currently in the distribution.
#
# Copyright 2016, 2018-2019 Russ Allbery <rra@cpan.org>
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

use Test::More tests => 7;

# Load the module.
BEGIN { use_ok('App::DocKnot::Generate') }

# Initialize the App::DocKnot object using the default metadata path.
my $metadata_path = File::Spec->catfile(getcwd(), 'docs', 'metadata');
my $docknot = App::DocKnot::Generate->new({ metadata => $metadata_path });
isa_ok($docknot, 'App::DocKnot::Generate');

# Save the paths to the real README and README.md files.
my $readme_path    = File::Spec->catfile(getcwd(), 'README');
my $readme_md_path = File::Spec->catfile(getcwd(), 'README.md');

# Write the README output for the DocKnot package to a temporary file.  Always
# apply a CRLF conversion layer (which should be harmless on UNIX) to ensure
# correct behavior on Windows, where we should have automatically converted
# newlines to CRLF when writing.
my $tmp     = File::Temp->new();
my $tmpname = $tmp->filename;
$docknot->generate_output('readme', $tmpname);
my $output = slurp('<:crlf', $tmp);
is_file_contents($output, 'README', 'README in package');
$docknot->generate_output('readme-md', $tmpname);
$output = slurp('<:crlf', "$tmpname");
is_file_contents($output, 'README.md', 'README.md in package');

# Test default output destinations by creating a temporary directory and then
# generating the README file without an explicit output location.
my $tmpdir = File::Temp->newdir();
chdir($tmpdir);
$docknot->generate_output('readme');
$output = slurp('README');
is_file_contents($output, $readme_path, 'README using default filename');

# Use generate_all to generate all the metadata with default output paths.
unlink('README');
$docknot->generate_all();
$output = slurp('README');
is_file_contents($output, $readme_path, 'README from generate_all');
$output = slurp('README.md');
is_file_contents($output, $readme_md_path, 'README.md from generate_all');

# Allow cleanup to delete our temporary directory.
chdir(File::Spec->rootdir());
