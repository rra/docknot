#!/usr/bin/perl
#
# Tests for App::DocKnot::Spin::Versions (.versions file handling).
#
# Copyright 2021-2022 Russ Allbery <rra@cpan.org>
#
# SPDX-License-Identifier: MIT

use 5.024;
use autodie;
use warnings;

use lib 't/lib';

use Path::Tiny qw(path);
use POSIX qw(tzset);

use Test::More tests => 20;

require_ok('App::DocKnot::Spin::Versions');

# All dates in the sample data are in America/Los_Angeles.  Specify this in
# the POSIX format in the hope this will also work on systems without tzinfo
# installed.
local $ENV{TZ} = 'PST8PDT,M3.2.0,M11.1.0';
tzset();

# Parse the file.
my $path = path('t', 'data', 'spin', 'input', '.versions');
my $versions = App::DocKnot::Spin::Versions->new($path);
isa_ok($versions, 'App::DocKnot::Spin::Versions');

# Check the resulting information.
is($versions->version('docknot'), '4.01', 'docknot version');
is($versions->release_date('docknot'), '2021-02-27', 'docknot release date');
is(
    $versions->latest_release('software/docknot/index.th'), 1614460092,
    'latest release for software/docknot/index.th',
);

# Unknown products or files.
is($versions->version('unknown'), undef, 'unknown version');
is($versions->release_date('unknown'), undef, 'unknown release date');
is($versions->latest_release('index.th'), 0, 'unknown file index.th');

# Check continuation handling and a line without dependencies.
my $inputdir = path('t', 'data', 'spin', 'versions');
$path = $inputdir->child('continuation');
$versions = App::DocKnot::Spin::Versions->new($path);
is($versions->version('docknot'), '4.01', 'docknot version');
is($versions->release_date('docknot'), '2021-02-27', 'docknot release date');
is($versions->version('other-package'), '1.00', 'other-package version');
is(
    $versions->release_date('other-package'), '2021-09-07',
    'other-package release date',
);
is($versions->version('third-package'), '2.00', 'third-package version');
is(
    $versions->release_date('third-package'), '2021-09-06',
    'third-package release date',
);
is(
    $versions->latest_release('software/index.th'), 1614460092,
    'latest release for software/index.th',
);

# Check that third-package overrides the timestamp for
# software/docknot/index.th.
is(
    $versions->latest_release('software/docknot/index.th'), 1630897980,
    'latest release for software/docknot/index.th',
);

# Check error handling.
$path = $inputdir->child('invalid-continuation');
eval { App::DocKnot::Spin::Versions->new($path) };
is(
    $@, "continuation without previous entry in $path\n",
    'invalid continuation',
);
$path = $inputdir->child('invalid-date');
eval { App::DocKnot::Spin::Versions->new($path) };
is($@, qq(invalid date "20-02-27" in $path\n), 'invalid date');
$path = $inputdir->child('invalid-time');
eval { App::DocKnot::Spin::Versions->new($path) };
is($@, qq(invalid time "13:08" in $path\n), 'invalid time');
$path = $inputdir->child('invalid-line');
eval { App::DocKnot::Spin::Versions->new($path) };
is($@, "invalid line 2 in $path\n", 'invalid line');
