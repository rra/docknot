#!/usr/bin/perl
#
# Tests for the App::DocKnot command dispatch for generate.
#
# Copyright 2018 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use lib 't/lib';

use File::Temp;
use File::Spec;
use Perl6::Slurp;
use Test::RRA qw(is_file_contents);

use Test::More tests => 4;

# Load the module.
BEGIN { use_ok('App::DocKnot') }

# Create the command-line parser.
my $docknot = App::DocKnot->new();
isa_ok($docknot, 'App::DocKnot');

# Generate the package README file to a temporary file, read it into memory,
# and compare it to the actual README file.  This duplicates part of the
# generate/self.t test, but via the command-line parser.
my $tempdir       = File::Temp->newdir();
my $output_path   = File::Temp->new(DIR => $tempdir);
my $metadata_path = File::Spec->catfile('docs', 'metadata');
my @generate_args = ('-m', $metadata_path, 'readme', $output_path);
$docknot->run('generate', @generate_args);
my $output = slurp($output_path);
is_file_contents($output, 'README', 'Generated README from argument list');
unlink($output_path);

# Do the same thing again, but using arguments from @ARGV.  Be sure to
# stringify $output_path, or slurp() will try to read from the file descriptor
# instead of the path and just get end of file.
local @ARGV = ('generate', @generate_args);
$docknot->run();
$output = slurp("$output_path");
is_file_contents($output, 'README', 'Generated README from ARGV');
