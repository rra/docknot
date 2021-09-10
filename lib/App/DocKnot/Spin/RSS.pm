# Generate RSS and thread from a feed description file.
#
# This module generates RSS feeds and thread indexes of newly-published pages
# or change notes for a web site maintained with App::DocKnot::Spin.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Spin::RSS 4.01;

use 5.024;
use autodie;
use warnings;

use App::DocKnot::Spin::Thread;
use Cwd qw(getcwd);
use Date::Parse qw(str2time);
use File::Basename qw(fileparse);
use Perl6::Slurp qw(slurp);
use POSIX qw(strftime);

##############################################################################
# Utility functions
##############################################################################

# List intersection.
#
# $one - First list
# $two - Second list
#
# Returns: Common elements of both lists as a list
sub _intersect {
    my ($one, $two) = @_;
    my %one = map { $_ => 1 } $one->@*;
    return grep { $one{$_} } $two->@*;
}

# Construct an absolute URL from a relative URL and a base URL.  This plays
# fairly fast and loose with schemes and the like, since we don't need to be
# precise for our purposes.
#
# $url  - Relative URL
# $base - Base URL to which it is relative
#
# Returns: Absolute URL
sub _absolute_url {
    my ($url, $base) = @_;

    # If $url is already absolute, return it.
    return $url if $url =~ m{ \A [[:lower:]]+ : }xms;

    # If $url starts with /, take only the scheme and host from the base URL.
    if ($url =~ m{ \A / }xms) {
        $base =~ s{ \A ( [[:lower:]]+ :// [^/]+ ) .* }{$1}xms;
        return $base . $url;
    }

    # Otherwise, strip the last component off the base URL, and then strip
    # more trailing components off the base URL for every ../ element in the
    # relative URL.  Then glue them together.  This does not deal with the
    # case where there are more ../ elements than there are elements in the
    # base URL.
    $base =~ s{ [^/]+ \z }{}xms;
    while ($url =~ s{ \A [.][.]/+ }{}xms) {
        $base =~ s{ [^/]+ /+ \z }{}xms;
    }
    return $base . $url;
}

# Construct a relative URL from an absolute URL and a base URL.  If there is
# no base URL or if the URLs cannot be made relative to each other, return the
# relative URL unchanged.
#
# $url  - Absolute URL
# $base - URL to which it should be relative
#
# Returns: Relative URL
sub _relative_url {
    my ($url, $base) = @_;
    return $url if !$base;

    # Remove the protocol and host portion from the base URL and ensure that
    # portion matches.
    if ($base =~ s{ \A ( https?:// [^/]+ ) /* }{}xms) {
        my $host = $1;
        if ($url !~ s{ \A \Q$host\E /* }{}xms) {
            return $url;
        }
    } else {
        return $url;
    }

    # Split the base URL into path segments.  While the input URL starts with
    # a matching segment, remove it.  When we run out of matching segments,
    # the relative URL is a number of ../ strings equal to the number of
    # remaining base segments, plus the remaining input URL.
    my @base = split(m{ /+ }xms, $base);
    while ($url && @base) {
        my $segment = shift(@base);
        if ($url !~ s{ \A \Q$segment\E (?: /+ | \z ) }{}xms) {
            return ('../' x (scalar(@base) + 1)) . $url;
        }
    }
    return ('../' x scalar(@base)) . $url;
}

##############################################################################
# Parsing
##############################################################################

# Read key/value blocks in an RFC-2822-style file.
#
# $file - File to read
#
# Returns: List of hashes corresponding to the blocks in the file.
sub _read_rfc2822_file {
    my ($self, $file) = @_;
    my $key;
    my @blocks  = ({});
    my $current = $blocks[0];

    # Parse the file.  $key holds the last key seen, used to append
    # continuation values to the previous key.  $current holds the current
    # block being parsed and @blocks all blocks seen so far.
    open(my $fh, '<', $file);
    while (defined(my $line = <$fh>)) {
        if ($line =~ m{ \A \s* \z }xms) {
            if ($key) {
                $current = {};
                push(@blocks, $current);
                undef $key;
            }
        } elsif ($line =~ m{ \A (\S+): [ \t]+ ([^\n]+) \Z }xms) {
            my ($new_key, $value) = ($1, $2);
            $value =~ s{ \s+ \z }{}xms;
            $key = lc($new_key);
            $current->{$key} = $value;
        } elsif ($line =~ m{ \A (\S+): \s* \z }xms) {
            my $new_key = $1;
            $key = lc($new_key);
            $current->{$key} //= q{};
        } elsif ($line =~ m{ \A \s }xms) {
            if (!$key) {
                die "$file:$.: invalid continuation line\n";
            }
            my $value = $line;
            $value =~ s{ \A \s }{}xms;
            if ($value =~ m{ \A [.] \s* \Z }xms) {
                $value = "\n";
            }
            if ($current->{$key} && $current->{$key} !~ m{ \n\z }xms) {
                $current->{$key} .= "\n";
            }
            $current->{$key} .= $value;
        } else {
            die "$file:$.: cannot parse line\n";
        }
    }
    close($fh);

    # If the file ends in a blank line, we'll have a stray empty block.
    # Remove it.
    if (!$key) {
        pop(@blocks);
    }

    # Return the parsed blocks.
    return \@blocks;
}

# Parse a change file.  Save the metadata into the provided hash reference and
# the changes into the provided array reference.  Each element of the array
# will be a hash with keys title, date, link, and description.
#
# $file - File to read
#
# Returns: List of reference to metadata hash and reference to a list of
#          hashes of changes
sub _parse_changes {
    my ($self, $file) = @_;
    my $blocks_ref = $self->_read_rfc2822_file($file);

    # The first block is our metadata.  recent defaults to 15.
    my $metadata_ref = shift($blocks_ref->@*);
    if (!defined($metadata_ref->{recent})) {
        $metadata_ref->{recent} = 15;
    }

    # Canonicalize the data for the rest of the blocks, and check for
    # duplicate GUIDs.
    my %guids;
    my $base = $metadata_ref->{base};
    for my $block_ref ($blocks_ref->@*) {
        $block_ref->{date} = str2time($block_ref->{date})
          or die qq{cannot parse date "$block_ref->{date}"\n};

        # Relative links are relative to the base URL in the metadata.
        if ($block_ref->{link} && $base) {
            if ($block_ref->{link} eq q{/}) {
                $block_ref->{link} = $base;
            } else {
                $block_ref->{link} = $base . $block_ref->{link};
            }
        }

        # If no GUID was given, take it from the link for journal and review
        # entries, and otherwise from the date.  Then ensure it's unique.
        my $guid = $block_ref->{guid};
        if (!$guid) {
            if ($block_ref->{journal} || $block_ref->{review}) {
                $guid = $block_ref->{link};
            } else {
                $guid = $block_ref->{date};
            }
        }
        if ($guids{$guid}) {
            die "duplicate GUID for entry $guid\n";
        }
        $block_ref->{guid} = $guid;

        # Determine the tags.
        my @tags = $block_ref->{tags} ? split(q{ }, $block_ref->{tags}) : ();
        if ($block_ref->{review}) {
            push(@tags, 'review');
        }
        $block_ref->{tags} = \@tags;
    }

    # Return the results.
    return ($metadata_ref, $blocks_ref);
}

##############################################################################
# RSS output
##############################################################################

# Escape a string for XML.
sub xml_escape {
    my ($string) = @_;
    $string =~ s/&/&amp;/g;
    $string =~ s/</&lt;/g;
    $string =~ s/>/&gt;/g;
    return $string;
}

# Format a journal post into HTML for inclusion in an RSS feed.
sub rss_journal {
    my ($file) = @_;
    my $spin = App::DocKnot::Spin::Thread->new();
    my $source = slurp($file);
    my $cwd = getcwd();
    my (undef, $dir) = fileparse($file);
    chdir($dir);
    my $page = $spin->spin_thread($source);
    chdir($cwd);
    my @page = map { "$_\n" } split(m{ \n }xms, $page);
    shift @page while (@page and $page[0] !~ /<h1>/);
    shift @page;
    shift @page while (@page and $page[0] =~ /^\s*$/);
    pop @page while (@page and $page[$#page] !~ /<div class=\"date\"><p>/);
    pop @page;
    pop @page while (@page and $page[$#page] =~ /^\s*$/);
    return join ('', @page) . "\n";
}

# Format a review into HTML for inclusion in an RSS feed.  Takes the path to
# the review thread file and the metadata for this feed.
sub rss_review {
    my ($file, $metadata) = @_;
    my $dir = $file;
    $dir =~ s%/+[^/]*$%%;
    my $class = ($dir =~ /magazines/) ? 'magazines' : 'books';
    my $base = $metadata->{base} . ($metadata->{base} =~ m%/$% ? '' : '/')
        . 'reviews';
    my $spin = App::DocKnot::Spin::Thread->new();
    my $source = slurp($file);
    my $cwd = getcwd();
    my (undef, $threaddir) = fileparse($file);
    chdir($threaddir);
    my $page = $spin->spin_thread($source);
    chdir($cwd);
    my @page = map { "$_\n" } split(m{ \n }xms, $page);
    my ($title, $author);
    while (@page and $page[0] !~ /<table class=\"info\">/) {
        if ($page[0] =~ m{<h1><cite>(.*)</cite></h1>}) {
            $title = $1;
        } elsif ($page[0] =~ m{<p class="(?:author|date)">(.*)</p>}) {
            $author = $1;
        }
        shift @page;
    }
    die "$0: cannot find title and author in $file"
        unless ($title && $author);
    pop @page while (@page and $page[$#page] !~ /<p class=\"rating\">/);
    my ($buy, $ebook);
    for my $i (0 .. $#page) {
        if ($page[$i] =~ /<p class=\"ebook\">/) {
            $ebook = $i;
        }
        if ($page[$i] =~ /<p class=\"buy\">/) {
            $buy = $i;
            last;
        }
    }
    splice (@page, $buy, 2) if $buy;
    splice (@page, $ebook, 4) if $ebook;
    $page = join ('', @page);
    $page =~ s/^\s*<table[^>]+>/<table>/mg;
    $page =~ s/^\s*<tr/  <tr/mg;
    $page =~ s/^\s*<td[^>]+>/    <td>/mg;
    $page =~ s{</tr></table></div>}{</tr></table>};
    $page =~ s/<div class=\"review\">//;
    $page =~ s/<p class=\"rating\">/<p>/;
    $page =~ s{<span class="story"><span id="\S+">(.*?)</span></span>}
              {<strong>$1</strong>}sg;
    $page = "<p>Review: <cite>$title</cite>, $author</p>\n\n" . $page;
    return $page . "\n";
}

# Print out the RSS version of the changes information given a file to which
# to print it, the file name, the metadata resulting RSS file, and a reference
# to the array of entries.  Various things are still hard-coded here.  Use the
# date of the last change as <pubDate> and the current time as
# <lastBuildDate>; it's not completely clear to me that this is correct.
sub rss_output {
    my ($file, $name, $metadata, $entries) = @_;
    $name =~ s,.*/,,;
    my $format = '%a, %d %b %Y %H:%M:%S %z';
    my $date = strftime ($format, localtime);
    my $last;
    if (@$entries) {
        $last = strftime ($format, localtime $entries->[0]{date});
    } else {
        $last = $date;
    }
    my $version = '1.25';
    print $file <<"EOC";
<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>$metadata->{title}</title>
    <link>$metadata->{base}</link>
    <description>$metadata->{description}</description>
    <language>$metadata->{language}</language>
    <pubDate>$last</pubDate>
    <lastBuildDate>$date</lastBuildDate>
    <generator>spin-rss $version</generator>
EOC
    if ($metadata->{'rss-base'}) {
        print $file qq(    <atom:link href="$metadata->{'rss-base'}$name");
        print $file qq( rel="self"\n);
        print $file qq(               type="application/rss+xml" />\n);
    }
    print $file "\n";
    for my $entry (@$entries) {
        my $date = strftime ($format, localtime $entry->{date});
        my $title = xml_escape ($entry->{title});
        my $description;
        if ($entry->{description}) {
            $description = xml_escape ($entry->{description});
            $description =~ s/^/        /mg;
            $description =~ s,^(\s*),$1<p>,;
            $description =~ s,\n*\z,</p>\n,;
        } elsif ($entry->{journal}) {
            $description = rss_journal ($entry->{journal});
        } elsif ($entry->{review}) {
            $description = rss_review ($entry->{review}, $metadata);
        }
        $description =~ s{(<(?:a href|img src)=\")(?!http:)([./\w][^\"]+)\"}
                         {$1 . _absolute_url ($2, $entry->{link}) . '"'}ge;
        my $perma = ($entry->{guid} =~ /^http/) ? '' : ' isPermaLink="false"';
        print $file "    <item>\n";
        print $file "      <title>$title</title>\n";
        print $file "      <link>$entry->{link}</link>\n";
        print $file "      <description><![CDATA[\n";
        print $file "$description";
        print $file "      ]]></description>\n";
        print $file "      <pubDate>$date</pubDate>\n";
        print $file "      <guid$perma>$entry->{guid}</guid>\n";
        print $file "    </item>\n";
    }
    print $file "  </channel>\n";
    print $file "</rss>\n";
}

##############################################################################
# Thread output
##############################################################################

# Print out the thread version of the recent changes list, given a file to
# which to print it, the metadata, and a reference to the array of entries.
sub thread_output {
    my ($file, $metadata, $entries) = @_;
    if ($metadata->{'thread-prefix'}) {
        print $file $metadata->{'thread-prefix'}, "\n";
    } else {
        print $file "\\heading[Recent Changes][indent]\n\n";
        print $file "\\h1[Recent Changes]\n\n";
    }
    my $last;
    for my $entry (@$entries) {
        my $month = strftime ('%B %Y', localtime $entry->{date});
        if (not $last or $month ne $last) {
            print $file "\\h2[$month]\n\n";
            $last = $month;
        }
        my $date = strftime ('%Y-%m-%d', localtime $entry->{date});
        print $file "\\desc[$date \\entity[mdash]\n";
        print $file "      \\link[$entry->{link}]\n";
        print $file "           [$entry->{title}]][\n";
        my $description = $entry->{description};
        $description =~ s/^/    /mg;
        $description =~ s/\\/\\\\/g;
        print $file $description;
        print $file "]\n\n";
    }
    print $file "\\signature\n";
}

##############################################################################
# Index output
##############################################################################

# Translate the thread of a journal entry for inclusion in an index page.
# Also takes the full URL for the permanent link to the entry page.  Returns
# the thread.
sub index_journal {
    my ($file, $url) = @_;
    open (IN, '<', $file) or die "$0: cannot open $file: $!\n";
    local $_;
    while (<IN>) {
        last if /\\h1/;
    }
    my $text = <IN>;
    $text = '' if $text =~ /^\s*$/;
    while (<IN>) {
        last if /^\\date/;
        $text .= $_;
    }
    close IN;
    return $text;
}

# Translate the thread of a book review for inclusion into an index page.
# Also takes the full URL for the permanent link to the entry page.  Returns
# the thread.
sub index_review {
    my ($file, $url) = @_;
    open (IN, '<', $file) or die "$0: cannot open $file: $!\n";
    local $_;
    my ($title, $author);
    while (<IN>) {
        my $char = '(?:[^\]\\\\]|\\\\entity\[\S+\])';
        if (/\\(header|edited)\s*\[($char+)\]\s*$/) {
            $_ .= <IN>;
        }
        if (/\\(header|edited)\s*\[($char+)\]\s*\[($char+)\]/) {
            ($title, $author) = ($2, $3);
            $author .= ' (ed.)' if ($1 eq 'edited');
            last;
        }
    }
    unless (defined $author) {
        die "$0: cannot parse review file $file\n";
    }
    my $text;
    if ($file =~ m,/magazines/,) {
        $text = "Review: \\cite[$title], $author\n\n";
    } else {
        $text = "Review: \\cite[$title], by $author\n\n";
    }
    $text .= "\\table[][\n";
    while (<IN>) {
        last if /^\\div\(review\)\[/;
        my $char = '(?:[^\]\\\\]|\\\\entity\[\S+\])';
        if (/^\s*\\data\[($char+)\]\s*\[($char+)\]/) {
            $text .= "    \\tablerow[$1][$2]\n";
        }
    }
    $text .= "]\n\n";
    while (<IN>) {
        last if /^\\done/;
        s/\\story\[\d+\]/\\strong/g;
        s/^\\rating\s*\[([^\]]+)\]/Rating: $1 out of 10/;
        $text .= $_;
    }
    close IN;
    return $text;
}

# Print out the index version of the recent changes list, given a file to
# which to print it, the metadata, and a reference to the array of entries.
sub index_output {
    my ($file, $metadata, $entries) = @_;
    if ($metadata->{'index-prefix'}) {
        print $file $metadata->{'index-prefix'}, "\n";
    }
    my $last;
    for my $entry (@$entries) {
        my $date = strftime ('%Y-%m-%d %H:%M', localtime $entry->{date});
        my $day = $date;
        $day =~ s/ .*//;
        print $file "\\h2[$day: $entry->{title}]\n\n";
        my $text;
        if ($entry->{journal}) {
            $text = index_journal ($entry->{journal}, $entry->{link});
        } elsif ($entry->{review}) {
            $text = index_review ($entry->{review}, $entry->{link});
        }
        $text =~ s{(\\(?:link|image)\s*)\[([^\]]+)\]}
                  {"${1}[" . _absolute_url ($2, $entry->{link}) . ']'}ge;
        $text =~ s{(\\image\s*)\[([^\]]+)\]}
            {"${1}[" . _relative_url ($2, $metadata->{'index-base'}) . ']'}ge;
        print $file $text;
        print $file "\\class(footer)[$date \\entity[mdash]\n";
        print $file "    \\link[$entry->{link}]\n";
        print $file "         [Permanent link]]\n\n";
    }
    if ($metadata->{'index-suffix'}) {
        print $file $metadata->{'index-suffix'}, "\n";
    }
    print $file "\\signature\n";
}

##############################################################################
# Public interface
##############################################################################

# Create a new RSS generator object.
#
# $args - Anonymous hash of arguments with the following keys:
#   base - Base path for output files
#
# Returns: Newly created object
sub new {
    my ($class, $args_ref) = @_;

    # Canonicalize the base path to have a single trailing slash.
    my $base = $args_ref->{base};
    if ($base) {
        $base =~ s{ /* \z}{/}xms;
    }

    # Create and return the object.
    my $self = { base => $base };
    bless($self, $class);
    return $self;
}

# Generate specified output files from an .rss input file.
#
# $source - Path to the .rss file
# $base   - Optional base path for output
sub generate {
    my ($self, $source, $base) = @_;
    if ($base) {
        $base =~ s{ /* \z}{/}xms;
    }
    $base //= $self->{base};

    # Read in the changes.
    my ($metadata_ref, $changes_ref) = $self->_parse_changes($source);

    # Now, the output key tells us what files to write out.
    my @output;
    if ($metadata_ref->{output}) {
        @output = split (' ', $metadata_ref->{output});
    } else {
        @output = ('*:rss:index.rss');
    }
    for my $output (@output) {
        my ($tags, $format, $file) = split (':', $output);
        my $path;
        if ($base && $file !~ m,^/,) {
            $path = "$base$file";
        } else {
            $path = $file;
        }
        my $prettyfile = $path;
        $prettyfile = ".../$prettyfile" unless $prettyfile =~ m,^/,;
        next if (-f $path && -M $path <= -M $source);
        my @tags = split (',', $tags);
        my @interest;
        for my $change ($changes_ref->@*) {
            if ($tags eq '*' || _intersect($change->{tags}, \@tags)) {
                push (@interest, $change);
            }
        }
        if ($format eq 'thread') {
            print "Generating thread file $prettyfile\n";
            open (THREAD, '>', $path) or die "$0: cannot create $path: $!\n";
            thread_output (\*THREAD, $metadata_ref, \@interest);
            close THREAD;
        } elsif ($format eq 'rss') {
            if (@interest > $metadata_ref->{recent}) {
                splice (@interest, $metadata_ref->{recent});
            }
            print "Generating RSS file $prettyfile\n";
            open (RSS, '>', $path) or die "$0: cannot create $path: $!\n";
            rss_output (\*RSS, $file, $metadata_ref, \@interest);
            close RSS;
        } elsif ($format eq 'index') {
            if (@interest > $metadata_ref->{recent}) {
                splice (@interest, $metadata_ref->{recent});
            }
            print "Generating index file $prettyfile\n";
            open (INDEX, '>', $path) or die "$0: cannot create $path: $!\n";
            index_output (\*INDEX, $metadata_ref, \@interest);
            close INDEX;
        }
    }
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT RSS TimeDate YYYY-MM-DD
sublicense hoc rss

=head1 NAME

App::DocKnot::Spin::RSS - Generate RSS and thread from a feed description file

=head1 SYNOPSIS

    use App::DocKnot::Spin::RSS;

    my $rss = App::DocKnot::Spin::RSS->new({ base => 'path/to/tree' });
    $rss->generate('path/to/tree/.rss');

=head1 REQUIREMENTS

Perl 5.006 or later and the modules Date::Parse (part of the TimeDate
distribution) and Perl6::Slurp, both of which are available from CPAN.

=head1 DESCRIPTION

App::DocKnot::Spin::RSS reads as input a feed description file consisting of
simple key/value pairs and writes out either thread (for input to
App::DocKnot::Spin::Thread) or RSS.  The feed description consists of a
leading block of metadata and then one block per entry in the feed.  Each
block can either include the content of the entry or can reference an external
thread file, in several formats, for the content.  The feed description file
defines one or more output files in the Output field of the metadata.

Output files are only regenerated if they are older than the input feed
description file.

App::DocKnot::Spin::RSS is designed for use with App::DocKnot::Spin.  It
relies on App::DocKnot::Spin::Thread to convert thread to HTML, both for
inclusion in RSS feeds and for post-processing of generated thread files.
App::DocKnot::Spin::RSS is invoked automatically by App::DocKnot::Spin when it
encounters an F<.rss> file in a directory it is processing.

See L<INPUT LANGUAGE> for the details of the language in which F<.rss> files
are written.

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Spin::RSS object.  ARGS should be a hash reference
with one or more of the following keys, all of which are optional:

=over 4

=item base

By default, App::DocKnot::Spin::RSS output files are relative to the current
working directory.  If the C<base> argument is given, output files will be
relative to the value of C<base> instead.  Output files specified as absolute
paths will not be affected.

=back

=back

=head1 INSTANCE METHODS

=over 4

=item generate(FILE[, BASE])

Parse the input file FILE and generate the output files that it specifies.
BASE, if given, specifies the root directory for output files specified with
relative paths, and overrides any C<base> argument given to new().

=back

=head1 INPUT LANGUAGE

The input for App::DocKnot::Spin::RSS is normally a F<.rss> file in a tree
being processed by App::DocKnot::Spin.  The file consists of one or more
blocks of RFC-2822-style fields with values, each separated by a blank line.
Each field and value looks like an e-mail header field, including possible
continuation lines:

    Field: value
     continuation of value

Any line beginning with whitespace is considered a continuation of the
previous line.  If a value should contain a blank line, indicate that
blank line with a continuation line containing only a period.  For
example:

    Field: first paragraph
     .
     second paragraph

=head2 Metadata

The first block of the file sets the metadata for this set of output.  The
following fields are supported:

=over 4

=item Base

The base URL for entries in this file.  All links in subsequent blocks of
the file, if not absolute URLs, are treated as relative to this URL and
are made absolute by prepending this URL.  Always specify this key unless
all Link fields in the remainder of the file use absolute URLs.

This field value is also used as the <link> element in the RSS feed,
indicating the web site corresponding to this feed.

=item Description

The description of the feed, used only in the RSS output.  This should
always be set if there are any RSS output files.

=item Index-Base

The base URL for output files of type C<index>.  This is used to
canonicalize relative URLs and should be the URL to the directory
containing the HTML file that will result from processing the thread
output.  This should be set if there are any output files of type
C<index>; if it isn't set, relative links may be rewritten incorrectly.

=item Index-Prefix

When generating output files of type C<index>, use the value as the
initial content of the generated thread.  This field should almost always
be set if any output files of type C<index> are defined.  It will contain
such things as the \heading command, any prologue material, initial
headings, and so forth.

=item Index-Suffix

When generating output files of type C<index>, append the value to the end
of the generated thread.  The \signature command is always appended and
should not be included here.  Set this field only if there is other thread
that needs to be appended (such as closing brackets for \div commands).

=item Language

The language of the feed, used only in the RSS output.  This should always
be set if there are any RSS output files.  Use C<en-us> for US English.

=item Output

Specifies the output files for this input file in the form of a
whitespace-separated list of output specifiers.  This field must always be
set.

An output specifier is of the form I<tags>:I<type>:I<file>, where I<file>
is the output file (always a relative path), I<type> is the type of
output, and I<tags> indicates which entries to include in this file.
I<tags> is a comma-separated list of tags or the special value C<*>,
indicating all tags.

There are three types of output:

=over 4

=item index

Output thread containing all recent entries.  This output file honors the
Recent field similar to RSS output and is used to generate something akin
to a journal or blog front page: an HTML version of all recent entries.
It only supports external entries (entries with Journal or Review fields).
The Index-Base and Index-Prefix (and possibly Index-Suffix) fields should
be set.

For output for entries with simple descriptions included in the input
file, see the C<thread> output type.

=item rss

Output an RSS file.  App::DocKnot::Spin::RSS only understands the RSS 2.0
output format.  The Description, Language, RSS-Base, and Title fields should
be set to provide additional metadata for the output file.

=item thread

Output thread containing all entries in this input file.  This should only
be used for input files where all entries have their description text
inline in the input file.  Every entry will be included.  The output will
be divided into sections by month, and each entry will be in a description
list, with the title prefixed by the date.  The Thread-Prefix field should
be set.

For output that can handle entries from external files, see the C<index>
output type.

=back

=item Recent

Sets the number of recent entries to include in output files of type
C<rss> or C<index> (but not C<thread>, which will include the full
contents of the file).  If this field is not present, the default is 15.

=item RSS-Base

The base URL for RSS files generated by this file.  Each generated RSS
file should have a link back to itself, and that link will be formed by
taking the output name and prepending this field.

=item Thread-Prefix

When generating thread output from this file, use the value as the initial
content of the generated thread.  This field should almost always be set
if any output files of type C<thread> are defined.  It will contain such
things as the \heading command, any prologue material, initial headings,
and so forth.

=item Title

The title of the feed, used only in the RSS output.  This should always be
set if there are any RSS output files.

=back

=head2 Entries

After the first block, each subsequent block in the input file defines an
entry.  Entries take the following fields:

=over 4

=item Date

The date of this entry in ISO date format (YYYY-MM-DD HH:MM).  This field
is required.

=item Description

The inline contents of this entry.  One and only one of this field,
Journal, or Review should be present.  Description fields can only be used
with output types of C<rss> or C<thread>.

=item Journal

Specifies that the content of this entry should be read from an external
thread file given by the value of this field.  The contents of that file
are expected to be in the thread format used by my journal entries:
specifically, everything is ignored up to the first \h1 and after
\class(date), and the \class(date) line is stripped off (the date
information from the Date field is used instead).

One and only one of this field, Description, or Review should be present.

=item Link

The link to the page referenced by this entry.  The link is relative to
the Base field set in the input file metadata.  This field is required.

=item Review

Specifies that the content of this entry should be read from an external
thread file given by the value of this field.  The contents of that file
are expected to be in the thread format used by my book reviews.

Many transformations are applied to entries of this sort based on the
format used by my book reviews and the URL layout they use, none of which
is documented at present.  For the time being, see the source code for
what transformations are done.  This support will require modification for
use by anyone else.

One and only one of this field, Description, or Review should be present.

=item Tags

Whitespace-separated tags for this entry, used to determine whether this
entry will be included in a given output file given its output
specification.  In addition to any tags listed here, any entry with a
Review field will automatically have the C<review> tag.

Entries may have no tags, in which case they're only included in output
files with a tag specification of C<*>.

=item Title

The title of the entry.  This field is required.

=back

=head1 BUGS

The separate C<index> and C<thread> output types are a historical artifact
and should be merged in some way.  This will likely also require a way of
configuring different caps on number of included entries in different
output specifications.

The file format for review entries is not specified and involves a lot of
ad hoc heuristics that work with my book review format.

Support for additional RSS standards would be nice, although in practice
everything seems to be able to cope with RSS 2.0.

=head1 NOTES

RSS 2.0 was chosen as the output format because it supports GUIDs for
entries separate from the entry URLs and hence supports multiple entries
for the same URL, something that I needed for an RSS feed of recent
changes to my entire site.

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2008, 2010-2012, 2021 Russ Allbery <rra@cpan.org>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

=head1 SEE ALSO

L<docknot(1)>, L<App::DocKnot::Spin>, L<App::DocKnot::Spin::Thread>

This module is part of the App-DocKnot distribution.  The current version of
DocKnot is available from CPAN, or directly from its web site at
L<https://www.eyrie.org/~eagle/software/docknot/>.

=cut

# Local Variables:
# copyright-at-end-flag: t
# End:
