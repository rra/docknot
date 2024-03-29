#!/usr/bin/perl
#
# Build script for the docknot application.
#
# Copyright 2013, 2016, 2018-2022 Russ Allbery <rra@cpan.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use Module::Build;

# Basic package configuration.
my $build = Module::Build->new(
    dist_abstract        => 'Package documentation generator',
    dist_author          => 'Russ Allbery <rra@cpan.org>',
    license              => 'mit',
    module_name          => 'App::DocKnot',
    recursive_test_files => 1,
    add_to_cleanup       => [qw(MANIFEST.bak cover_db)],

    # Module static data files.
    share_dir => { module => { 'App::DocKnot' => 'share' } },

    # Add additional package metadata.
    meta_merge => {
        'meta-spec' => { version => '2' },
        resources   => {
            bugtracker => {
                web => 'https://github.com/rra/docknot/issues',
            },
            homepage   => 'https://www.eyrie.org/~eagle/software/docknot',
            repository => {
                url  => 'https://github.com/rra/docknot.git',
                web  => 'https://github.com/rra/docknot',
                type => 'git',
            },
        },
    },

    # Other package relationships.
    configure_requires => { 'Module::Build' => 0.4004 },
    requires           => {
        'Date::Parse'            => 0,
        'File::BaseDir'          => 0,
        'File::ShareDir'         => 0,
        'Git::Repository'        => 0,
        'Image::Size'            => 0,
        'IO::Uncompress::Gunzip' => 0,
        'IO::Compress::Xz'       => 0,
        'IPC::Run'               => 0,
        'IPC::System::Simple'    => 0,
        'JSON::MaybeXS'          => '1.004000',
        'Kwalify'                => 0,
        'List::SomeUtils'        => '0.07',
        'Path::Iterator::Rule'   => 0,
        'Path::Tiny'             => '0.101',
        'Perl6::Slurp'           => 0,
        'Pod::Thread'            => '3.01',
        'Sort::Versions'         => 0,
        'Template'               => 0,
        'YAML::XS'               => '0.81',
        perl                     => '5.024',
    },
    test_requires => {
        'Capture::Tiny'         => 0,
        'File::Copy::Recursive' => 0,
    },
);

# Generate the build script.
$build->create_build_script;
