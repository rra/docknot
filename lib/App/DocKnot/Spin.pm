# Translate thread (an HTML macro language) into HTML.
#
# This module translates a single file or a tree of files in thread (a custom
# macro language) into HTML.  It also handles formatting some other input
# types (text and POD, for example), copying other types of files to the
# output tree, and, when run on a tree of files, creates site navigation
# links.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Spin 4.01;

use Cwd qw(getcwd realpath);
use FileHandle ();
use Getopt::Long qw(GetOptions);
use Git::Repository ();
use Image::Size qw(html_imgsize);
use IPC::System::Simple qw(systemx);
use File::Basename qw(fileparse);
use File::Copy qw(copy);
use File::Find qw(find finddepth);
use File::Spec ();
use POSIX qw(mktime strftime);
use Text::Balanced qw(extract_bracketed);

# The default list of files and/or directories to exclude from spinning.  This
# can be added to (but not removed from) with the --exclude option.  Each of
# these should be a regular expression.
my @EXCLUDES = (
    qr{ ^ [.] (?!htaccess\z) }xms,
    qr{ ^ (?:CVS|Makefile|RCS) \z }xms,
);

# The URL to the software page for all of my web page generation software,
# used to embed a link to the software that generated the page.
my $URL = 'https://www.eyrie.org/~eagle/software/web/';

use strict;
use subs qw(expand parse parse_context);
use warnings;
use vars qw($FILE @FILES $FULLPATH $ID $OUTPUT
            @RSS %SITEDESCS %SITELINKS @SITEMAP
            @STATE %VERSIONS %commands);

##############################################################################
# Output
##############################################################################

# Sends something to the output file with special handling of whitespace for
# more readable HTML output.
#
# @output - Strings to output.
sub _output {
    my ($self, @output) = @_;
    my $output = join(q{}, @output);

    # If we have saved whitespace, separate any closing tags at the start of
    # the output from the rest of the output and insert that saved space
    # between those closing tags and the rest of the output.
    #
    # The effect of this is to move whitespace between element bodies and
    # their closing tags to outside of the closing tags, which makes the HTML
    # much more readable.
    if ($self->{space}) {
        my ($close, $body) = $output =~ m{
            \A
            (\s*
             (?: </(?!body)[^>]+> \s* )*
            )
            (.*)
        }xms;
        $close .= $self->{space};

        # Collapse multiple whitespace-only lines into a single blank line.
        $close =~ s/\n\s*\n\s*\n/\n\n/g;

        # Replace the output with added whitespace and clear saved whitespace.
        $output = $close . $body;
        $self->{space} = q{};
    }

    # Remove and save any trailing whitespace.
    if ($output =~ s{ \n (\s+) \z }{\n}xms) {
        $self->{space} = $1;
    }

    # Send the results to the output file.
    print { $self->{out_fh} } $output;
}

# Warn about a problem with the current file and line.
sub _warning {
    my ($self, $problem) = @_;
    warn "spin:$FILE:$.: $problem\n";
}

##############################################################################
# Basic parsing
##############################################################################

# Escapes &, <, and > characters found in a string.
sub escape {
    my ($self, $string) = @_;
    local $_ = $string;
    s/&/&amp;/g;
    s/</&lt;/g;
    s/>/&gt;/g;
    return $_;
}

# Wrap something in paragraph markers, being careful to get newlines right.
# Special-case a paragraph consisting entirely of <span> by turning it into a
# <p> with the same class.
sub paragraph {
    my ($self, $text) = @_;
    $text =~ s/^\n(\s*\n)*//;
    $text =~ s/(\S[ \t]*)\z/$1\n/;
    if ($text =~ m%^(\s*)<span(?!.*<span)([^>]*)>(.*)</span>(\s*)\z%s) {
        my ($lead, $class, $text, $trail) = ($1, $2, $3, $4);
        return "$lead<p$class>$text</p>$trail";
    } else {
        $text =~ s/^/<p>\n/;
        $text =~ s%(\n\s*)\z%\n</p>$1%;
        return $text;
    }
}

# Opens or closes a border of a continued structure.  Either takes the name of
# the state and its start and end tags, or takes no arguments to close all
# open states.
sub border {
    my ($self, $border, $start, $end) = @_;
    my $output = '';
    if ($border) {
        if ($STATE[-1] eq 'BLOCK' || $STATE[-1][0] ne $border) {
            $output .= $start;
            push (@STATE, [ $border, $end ]);
        }
    } else {
        my $state;
        while (defined ($state = pop @STATE)) {
            last if $state eq 'BLOCK';
            $output .= $$state[1];
        }
        push (@STATE, 'BLOCK');
    }
    return $output;
}

# Marks the beginning of major block structure.  Within this structure,
# borders will only clear to the level of this structure.
sub border_start {
    my ($self) = @_;
    push (@STATE, 'BLOCK');
}

# Clears a major block structure.
sub border_clear {
    my ($self) = @_;
    my $output = border;
    pop @STATE;
    return $output;
}

# Extract some number of arguments from the front of the given string.  If the
# optional third argument is true, try to pull off a parenthesized formatting
# instruction first, returning it as the first result (or undef if it's not
# found).  If the count is -1, pull off as many arguments as we can find.
sub extract {
    my ($self, $text, $count, $format) = @_;
    my (@result, $code);
    $text =~ s/\s*//;
    if ($format && $text =~ /^\(/) {
        ($result[0], $text) = extract_bracketed ($text, '()');
        $result[0] = substr ($result[0], 1, -1);
    } else {
        $result[0] = '';
    }
    if ($count >= 0) {
        for (1..$count) {
            ($result[$_], $text) = extract_bracketed ($text, '[]');
            if ($result[$_]) {
                $result[$_] = substr ($result[$_], 1, -1);
            } else {
                warn "$0:$FILE:$.: cannot find argument $_\n";
                $result[$_] = '';
            }
        }
    } else {
        while ($text =~ /^\s*\[/) {
            my $result;
            ($result, $text) = extract_bracketed ($text, '[]');
            last unless $result;
            $result = substr ($result, 1, -1);
            push (@result, $result);
        }
    }
    unless ($format) { shift @result }
    (@result, $text);
}

# Process a macro.  Takes the number of arguments, the definition of the
# macro, a flag saying whether we're at a block level, and then the values of
# all the arguments.  Only straight substitution commands are allowed here, of
# course.
sub macro {
    my ($self, $args, $definition, $block, @args) = @_;
    $definition =~ s/\\(\d+)/($1 > $args) ? "\\$1" : $args[$1 - 1]/ge;
    return $self->parse_context($definition, $block);
}

# Expand a given command into its representation.  This function is mutually
# recursive with parse.  Takes a third argument indicating whether this is a
# top-level element (if it is, and it doesn't generate its own container, it
# may have to be wrapped in <p>).  Returns the result of expanding the
# command, a flag saying whether the command is block level, and the remaining
# text in the paragraph.
sub expand {
    my ($self, $command, $text, $block) = @_;
    if ($command eq '==') {
        my ($new, $args, $definition);
        ($new, $args, $definition, $text) = $self->extract($text, 3);
        if (defined $definition) {
            $self->{macros}{$new} = [$args, $definition];
            return ('', 1, $text);
        }
    } elsif ($command eq '=') {
        my ($variable, $value);
        ($variable, $value, $text) = $self->extract($text, 2);
        $self->{strings}{$variable} = $self->parse($value);
        return ('', 1, $text);
    } elsif ($command =~ s/^=//) {
        if (exists($self->{strings}{$command})) {
            return ($self->{strings}{$command}, 0, $text);
        } else {
            warn "$0:$FILE:$.: unknown string $command\n";
            return ('', 0, $text);
        }
    } elsif ($command eq '\\') {
        return ('\\', 0, $text);
    } elsif (exists($self->{macros}{$command})) {
        my ($args, $definition) = $self->{macros}{$command}->@*;
        my @args;
        if ($args != 0) {
            @args = $self->extract($text, $args, 0);
            $text = pop @args;
        }
        my $block = $block && ($text !~ /\S/);
        return ($self->macro($args, $definition, $block, @args), $text);
    } else {
        if (!ref $commands{$command}) {
            warn "$0:$FILE:$.: bad command $command\n";
            return ('', 1, $text);
        }
        my ($args, $handler) = @{ $commands{$command} };
        my ($blocktag, $result);
        if ($args == 0) {
            ($blocktag, $result) = $self->$handler();
        } else {
            my @args = $self->extract($text, $args, 1);
            $text = pop @args;
            my $format = shift @args;
            ($blocktag, $result) = $self->$handler($format, @args);
        }
        return ($result, $blocktag, $text);
    }
}

# Given a text string, check it for escape sequences and expand them.  This
# function is mutually recursive with expand.  Takes one flag, saying whether
# we're at the block level.  Returns the expanded text and a flag saying
# whether the result is suitable for block level.
sub parse_context {
    my ($self, $text, $block) = @_;
    if (index ($text, '\\') == -1) {
        my $output = $text;
        $output = $self->border() . $self->paragraph($output) if $block;
        return ($output, $block);
    }

    # Chop off everything up to the first backslash and save it in output.
    # Then grab the escape and figure out what to do with it.
    #
    # If we are at block level, we have to distinguish between plain text and
    # inline commands, which have to be wrapped in paragraph tags, and
    # block-level commands, which shouldn't be.  We accumulate any output that
    # has to be wrapped in a paragraph in $paragraph (and put the border
    # before it in $border).  Whenever we see a block-level command, we wrap
    # anything currently in $paragraph in a paragraph, tack it on to the
    # output, and then add on the results of the block command.  $space holds
    # leading space, which we want to add to the paragraph if we end up
    # creating a paragraph.
    #
    # $nonblock is a flag indicating that we saw some construct that wasn't
    # suitable for block level.
    my $output = '';
    my ($border, $paragraph, $space) = ('', '', '');
    my $nonblock = 0;
    while ($text ne '') {
        unless ($text =~ s/^([^\\]+|\\([\w=]+|.))//) {
            my $error = substr ($text, 0, 20);
            $error =~ s/\n.*//s;
            die "$0:$FILE:$.: unable to parse at '$error'\n";
        }
        my $command;
        if (index ($1, '\\') == -1) {
            my $string = $1;
            if ($block && $string =~ /^\s+$/ && $paragraph eq '') {
                $space .= $string;
            } elsif ($block && ($string =~ /\S/ || $paragraph ne '')) {
                $border = $self->border() if $paragraph eq '';
                $paragraph .= $space . $string;
                $space = '';
            } else {
                $output .= $string;
                $nonblock = 1;
            }
        } else {
            $command = $2;
            my ($result, $blocktag);
            my $force = $block && $paragraph eq '';
            ($result, $blocktag, $text)
              = $self->expand($command, $text, $force);
            if ($blocktag) {
                if ($block && $paragraph ne '') {
                    $output .= $border . $self->paragraph ($space . $paragraph);
                    $border = '';
                    $paragraph = '';
                } else {
                    $output .= $space;
                }
                $output .= $result;
            } elsif ($block) {
                $border = $self->border() if $paragraph eq '';
                $paragraph .= $space . $result;
                $nonblock = 1;
            } else {
                $output .= $result;
                $nonblock = 1;
            }
            $space = '';
        }
        if ($text =~ s/^\n(\s*)//) {
            if ($paragraph ne '') {
                $paragraph .= "\n$1";
            } else {
                $output .= "\n" if $text || $nonblock;
                $output .= $1;
            }
        }
    }

    # Wrap any remaining output in paragraph tags and then return the output.
    # If we were at block level, our output is always suitable for block
    # level.  Otherwise, it's suitable for block level only if all of our
    # output was from block commands.
    $output .= $border . $self->paragraph($space . $paragraph)
        unless $paragraph eq '';
    return ($output, $block || !$nonblock);
}

# A wrapper around parse_context for callers who don't care about the block
# level of the results.
sub parse {
    my ($self, @args) = @_;
    my ($output) = $self->parse_context(@args);
    return $output;
}

##############################################################################
# Data files
##############################################################################

# Read the sitemap file for a site and flesh out the @SITEMAP array and
# %SITEDESCS and %SITELINKS hashes with information from that file.
#
# @SITEMAP is an array of anonymous arrays holding the complete site map.
# Each element represents a page.  The element will contain three elements:
# the numeric indent level, the partial URL, and the description.  %SITEDESCS
# holds a map of partial URLs to descriptions, and %SITELINKS map partial URLs
# to a list of other partial URLs (previous, next, and up).
#
# The format of the sitemap file is one line per web page, with indentation
# showing the tree structure, and with each line formatted as a partial URL, a
# colon, and a page description.  If two pages at the same level aren't
# related, a line with three dashes should be put between them at the same
# indentation level.
sub _read_sitemap {
    my ($self, $map) = @_;

    # @indents holds a stack of indentation levels.  @parents is a matching
    # stack of parent URLs for each level of indentation, and @prev is a
    # matching stack of the previous page at each level of indentation.  If
    # $prev[0] is undef, there is no previous page at that level.
    my @indents = (0);
    my (@parents, @prev);
    open (MAP, $map) or return;
    local $_;
    while (<MAP>) {
        next if /^\s*\#/;
        if (/^( *)---$/) {
            my $indent = length ($1);
            while ($indents[0] > $indent) {
                shift @indents;
                shift @prev;
                shift @parents;
            }
            $prev[0] = undef;
            next;
        }
        my ($indent, $url, $desc) = /^( *)([^\s:]+):\s+(.+)$/;
        next unless defined $desc;
        $indent = length ($indent);
        if ($indent > $indents[0]) {
            unshift (@parents, $prev[0]);
            unshift (@indents, $indent);
            unshift (@prev, undef);
        }
        while ($indents[0] > $indent) {
            shift @indents;
            shift @prev;
            shift @parents;
        }
        $SITELINKS{$url} = [ $prev[0], undef, @parents ];
        $SITELINKS{$prev[0]}[1] = $url if defined $prev[0];
        $prev[0] = $url;
        $SITEDESCS{$url} = $desc;
        push (@SITEMAP, [ $indent, $url, $desc ]);
    }
    close MAP;
}

# Given a date and time in ISO format, convert it to seconds since epoch.
sub time_to_seconds {
    my ($self, $date, $time) = @_;
    my @datetime = reverse split (':', $time);
    push (@datetime, reverse split ('-', $date));
    $datetime[4]--;
    $datetime[5] -= 1900;
    $datetime[6] = 0;
    $datetime[7] = 0;
    $datetime[8] = -1;
    return mktime (@datetime);
}

# Read in the .versions file for a site and flesh out the %VERSIONS hash.  It
# contains a mapping of product name to an anonymous array of version number
# and date of the last update.  It also fleshes out the $self->{depends} hash,
# which holds a mapping of file names that use a particular version to the
# timestamp of the last change in that version.
sub _read_versions {
    my ($self, $versions) = @_;
    open (VERSIONS, $versions) or return;
    local $_;
    my $last;
    while (<VERSIONS>) {
        next if /^\s*$/;
        next if /^\s*\#/;
        my @files;
        if (/^\s/) {
            @files = split;
        } else {
            my ($product, $version, $date, $time);
            ($product, $version, $date, $time, @files) = split;
            my $timestamp;
            if ($date) {
                $time ||= '00:00:00';
                $timestamp = $self->time_to_seconds($date, $time);
            } else {
                $timestamp = 0;
            }
            $date = strftime ('%Y-%m-%d', gmtime $timestamp);
            $VERSIONS{$product} = [ $version, $date ];
            $last = $timestamp;
        }

        # Update dependency timestamps.
        for my $file (@files) {
            if (!$self->{depends}{$file} || $self->{depends}{$file} < $last) {
                $self->{depends}{$file} = $last;
            }
        }
    }
    close VERSIONS;
}

##############################################################################
# Page headers and footers
##############################################################################

# Given the partial URL to the current page and the partial URL to another
# page, generate a relative URL between the two.
sub relative {
    my ($self, $start, $end) = @_;
    my @start = split ('/', $start, -1);
    my @end = split ('/', $end, -1);
    while (@start && @end && $start[0] eq $end[0]) {
        shift @start;
        shift @end;
    }
    if (@start == 1 && @end == 1) {
        return ($end[0] ? $end[0] : './');
    } else {
        return ('../' x $#start) . join ('/', @end);
    }
}

# Given the name of the current file being processed, return the <link> tags
# for that file suitable for the <head> section.  Uses the global %SITEDESCS
# and %SITELINKS variables.  If the partial URL isn't found in those variables
# or we're at the top page, nothing is returned.
sub sitelinks {
    my ($self, $file) = @_;
    $file =~ s%^\Q$self->{source}%%;
    $file =~ s%/index\.html$%/%;

    my $output = '';
    if ($file ne '/' && $SITELINKS{$file}) {
        my @links = @{ $SITELINKS{$file} };
        my @descs = map { defined ($_) ? $SITEDESCS{$_} : '' } @links;
        @descs = map { s/\"/&quot;/g; $_ } map { $self->escape($_) } @descs;
        @links = map {
            defined ($_) ? $self->relative($file, $_) : undef
        } @links;

        # Make the HTML for the footer.
        my @types = ('previous', 'next', 'up');
        for my $i (0..2) {
            next unless defined $links[$i];
            my $link = qq(  <link rel="$types[$i]" href="$links[$i]");
            if ($descs[$i] ne '') {
                if (length ($link) + length ($descs[$i]) + 12 > 79) {
                    $link .= "\n       ";
                }
                $link .= qq( title="$descs[$i]" />\n);
            } else {
                $link .= " />\n";
            }
            $output .= $link;
        }
        my $href = $self->relative($file, '/');
        $output .= qq(  <link rel="top" href="$href" />\n);
    }
    return $output;
}

# Given the name of the current file being processed, return the HTML for the
# navigation links for that file.  Uses the global %SITEDESCS and %SITELINKS
# variables.  If the partial URL isn't found in those variables or we're at
# the top page, nothing is returned.
sub placement {
    my ($self, $file) = @_;
    $file =~ s%^\Q$self->{source}%%;
    $file =~ s%/index\.html$%/%;

    my $output = '';
    if ($file ne '/' && $SITELINKS{$file}) {
        my @links = @{ $SITELINKS{$file} };
        my @descs = map { defined ($_) ? $SITEDESCS{$_} : '' } @links;
        @descs = map { $self->escape($_) } @descs;
        @links = map {
            defined ($_) ? $self->relative($file, $_) : undef
        } @links;

        # Build the table for the navigation bar.
        $output = qq(<table class="navbar"><tr>\n);
        $output .= qq(  <td class="navleft">);
        if (defined $links[0]) {
            $output .= qq(&lt;&nbsp;<a href="$links[0]">$descs[0]</a>);
        }
        $output .= qq(</td>\n);
        if (defined $links[2]) {
            $output .= qq(  <td>\n);
            my $first = 1;
            for my $i (reverse (2 .. $#links)) {
                next unless defined $links[$i];
                $output .= '    ';
                if ($first) {
                    $first = 0;
                } else {
                    $output .= qq(&gt; );
                }
                $output .= qq(<a href="$links[$i]">$descs[$i]</a>\n);
            }
            $output .= qq(  </td>\n);
        }
        $output .= qq(  <td class="navright">);
        if (defined $links[1]) {
            $output .= qq(<a href="$links[1]">$descs[1]</a>&nbsp;&gt;);
        }
        $output .= qq(</td>\n);
        $output .= qq(</tr></table>\n\n);
    }
    return $output;
}

# Returns the page footer, which consists of the navigation links, the regular
# signature, and the last modified date.  Takes as arguments the full path to
# the source file, the name of the destination file, the CVS Id of the source
# file if known, the template to use if the modification and current dates are
# the same, and the temlate to use if they're different.  The templates will
# have the strings %MOD% and %NOW% replaced by the appropriate dates and %URL%
# with the URL to my HTML generation software..
sub footer {
    my ($self, $source, $file, $id, @templates) = @_;
    my $output = $self->placement($file);
    $output .= "<address>\n    ";

    # Figure out the modified dates.  Use the RCS/CVS Id if available,
    # otherwise use the Git repository if available.
    my $modified;
    if (defined $id) {
        my $date = (split (' ', $id))[3];
        if ($date && $date =~ m%^(\d+)[-/](\d+)[-/](\d+)%) {
            $modified = sprintf ("%d-%02d-%02d", $1, $2, $3);
        }
    } elsif ($self->{repository} && $source =~ /^\Q$self->{source}/) {
        $modified = $self->{repository}->run(
          'log', '-1', '--format=%ct', $source);
        if ($modified) {
            $modified = strftime ('%Y-%m-%d', gmtime $modified);
        }
    }
    if (!$modified) {
        $modified = strftime ('%Y-%m-%d', gmtime ((stat $source)[9]));
    }
    my $now = strftime ('%Y-%m-%d', gmtime);

    # Determine which template to use and substitute in the appropriate times.
    my $template = ($modified eq $now) ? $templates[0] : $templates[1];
    if ($template) {
        for ($template) {
            s/%MOD%/$modified/g;
            s/%NOW%/$now/g;
            s/%URL%/$URL/g;
        }
        $output .= "$template\n";
    }
    $output .= "</address>\n";
    return $output;
}

##############################################################################
# Supporting functions
##############################################################################

# Given the format argument to a command, return the class or id attribute
# that should be used preceeded by a space, or an empty string if none should
# be used.
sub format_string {
    my ($self, $format) = @_;
    if ($format) {
        if ($format =~ s/^\#//) {
            if ($format =~ /\s/) {
                warn qq($0:$FILE:$.: Space in anchor "$format"\n);
            }
            return ' id="' . $format . '"';
        } else {
            return ' class="' . $format . '"';
        }
    } else {
        return '';
    }
}

# Splits a block of text apart at paired newlines so that it can be reparsed
# in paragraphs, but combines a paragraph with the next one if it has an
# unbalanced number of open brackets.  Used by containiners like \block that
# can contain multiple paragraphs.
sub split_paragraphs {
    my ($self, $text) = @_;
    $text =~ s/^\n(\s*\n)+/\n/;
    my @paragraphs;
    while ($text && $text =~ s/^(.*?(?:\n\n+|\s*\z))//s) {
        my $paragraph = $1;
        my $open = ($paragraph =~ tr/\[//);
        my $close = ($paragraph =~ tr/\]//);
        while ($text && $open > $close) {
            $text =~ s/^(.*?(?:\n\n+|\s*\z))//s;
            my $extra = $1;
            $open += ($extra =~ tr/\[//);
            $close += ($extra =~ tr/\]//);
            $paragraph .= $extra;
        }
        push (@paragraphs, $paragraph);
    }
    return @paragraphs;
}

# A simple block element.  Takes the name of the tag, an initial string to be
# prepended verbatim, the format, and the text.  Handles splitting the
# argument on paragraph boundaries and surrounding things properly with the
# tag.
sub block {
    my ($self, $tag, $border, $format, $text) = @_;
    my $output;
    $self->border_start();
    if ($format eq 'packed') {
        $output = $self->parse($text, 0);
    } else {
        my @paragraphs = $self->split_paragraphs($text);
        $output = join('', map { $self->parse($_, 1) } @paragraphs);
    }
    $output .= $self->border_clear();
    $output = $border . "<$tag" . $self->format_string($format) . '>' . $output;
    $output =~ s%\s*\z%</$tag>%;
    $output .= "\n" unless $format eq 'packed';
    return (1, $output);
}

# A heading.  Handles formats of #something specially by adding an <a name>
# tag inside the heading tag to make it a valid target for internal links even
# in old browsers.
sub heading {
    my ($self, $level, $format, $text) = @_;
    my $output = $self->border();
    if ($format && $format =~ /^\#/) {
        my $tag = $format;
        $tag =~ s/^\#//;
        $text = qq(<a name="$tag">$text</a>);
    }
    $output .= "<h$level" . $self->format_string($format) . '>';
    $output .= $self->parse($text);
    $output =~ s/\n\z//;
    $output .= "</h$level>\n";
    return (1, $output);
}

# A simple inline element.  Takes the name of the tag, the format, and the
# body and returns the appropriate list of block level and HTML.
sub inline {
    my ($self, $tag, $format, $text) = @_;
    my $output = "<$tag" . $self->format_string($format) . '>';
    $output .= $self->parse($text) . "</$tag>";
    return (0, $output);
}

# Enclose some text in another tag.  The one special thing that we do is if
# the enclosed text is entirely enclosed in <span> or <div> tags, we pull the
# options of the <span> or <div> out and instead apply them to the parent tag.
# Takes the tag and the text to enclose.
sub enclose {
    my ($self, $tag, $text) = @_;
    my $close = $tag;
    $close =~ s/ .*//;
    if ($text =~ m%^(\s*)<span(?!.*<span)([^>]*)>(.*)</span>(\s*)\z%s) {
        my ($lead, $class, $text, $trail) = ($1, $2, $3, $4);
        return "$lead<$tag$class>$text</$close>$trail";
    } elsif ($text =~ m%^(\s*)<div(?!.*<div)([^>]*)>(.*)</div>(\s*)\z%s) {
        my ($lead, $class, $text, $trail) = ($1, $2, $3, $4);
        return "$lead<$tag$class>$text</$close>$trail";
    } else {
        return "<$tag>$text</$close>";
    }
}

##############################################################################
# Commands
##############################################################################

# Basic inline commands.
sub do_break  { my ($self, @args) = @_; return (0, '<br />') }
sub do_bold   { my ($self, @args) = @_; return $self->inline ('b', @args) }
sub do_cite   { my ($self, @args) = @_; return $self->inline ('cite', @args) }
sub do_class  { my ($self, @args) = @_; return $self->inline ('span', @args) }
sub do_code   { my ($self, @args) = @_; return $self->inline ('code', @args) }
sub do_emph   { my ($self, @args) = @_; return $self->inline ('em', @args) }
sub do_italic { my ($self, @args) = @_; return $self->inline ('i', @args) }
sub do_strike { my ($self, @args) = @_; return $self->inline ('strike', @args) }
sub do_strong { my ($self, @args) = @_; return $self->inline ('strong', @args) }
sub do_sub    { my ($self, @args) = @_; return $self->inline ('sub', @args) }
sub do_sup    { my ($self, @args) = @_; return $self->inline ('sup', @args) }
sub do_under  { my ($self, @args) = @_; return $self->inline ('u', @args) }

# Basic block commands.
sub do_div {
    my ($self, @args) = @_;
    $self->block('div', '', @args);
}
sub do_block {
    my ($self, @args) = @_;
    $self->block('blockquote', '', @args);
}
sub do_bullet {
    my ($self, @args) = @_;
    $self->block('li', $self->border('bullet', "<ul>\n", "</ul>\n\n"), @args);
}
sub do_rule {
    my ($self) = @_;
    return (1, $self->border() . "<hr />\n");
}
sub do_number {
    my ($self, @args) = @_;
    $self->block('li', $self->border('number', "<ol>\n", "</ol>\n\n"), @args);
}

# A description list entry, which takes the heading and the body as arguments.
sub do_desc {
    my ($self, $format, $heading, $text) = @_;
    my $initial = $self->border('desc', "<dl>\n", "</dl>\n\n");
    $initial .= '<dt' . $self->format_string($format) . '>'
      . $self->parse($heading) . "</dt>\n";
    return $self->block('dd', $initial, $format, $text);
}

# An HTML entity.  Check for and handle numeric entities properly, including
# special-casing [ and ] since the user may have needed to use \entity to
# express text that contains literal brackets.
sub do_entity {
    my ($self, $format, $char) = @_;
    $char = $self->parse($char);
    if ($char eq '91') {
        return (0, '[');
    } elsif ($char eq '93') {
        return (0, ']');
    } elsif ($char =~ /^\d+$/) {
        return (0, '&#' . $char . ';');
    } else {
        return (0, '&' . $char . ';');
    }
}

# Generates the page heading at the top of the document.  Takes as arguments
# the page title and the page style.  This is where the XHTML declarations
# come from.
sub do_heading {
    my ($self, $format, $title, $style) = @_;
    $title = $self->parse($title);
    $style = $self->parse($style);
    my $file = $FILE;
    $file =~ s/\.th$/.html/;
    my $output = qq(<?xml version="1.0" encoding="utf-8"?>\n);
    $output .= qq(<!DOCTYPE html\n);
    $output .= qq(    PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"\n);
    $output .= qq(    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n);
    $output .= qq(\n<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en");
    $output .= qq( lang="en">\n);
    $output .= qq(<head>\n  <title>$title</title>\n);
    $output .= qq(  <meta http-equiv="Content-Type");
    $output .= qq( content="text/html; charset=utf-8" />\n);
    if ($style) {
        $style .= '.css';
        $style = $self->{style_url} . $style if $self->{style_url};
        $output .= qq(  <link rel="stylesheet" href="$style");
        $output .= qq( type="text/css" />\n);
    }
    if (@RSS) {
        for my $rss (@RSS) {
            my ($url, $title) = @$rss;
            $output .= qq(  <link rel="alternate" type="application/rss+xml");
            $output .= qq( href="$url"\n);
            $output .= qq(        title="$title" />\n);
        }
    }
    if ($FILE ne '-') {
        $output .= $self->sitelinks($file);
    }
    $output .= "</head>\n\n";
    my $date = strftime ('%Y-%m-%d %T -0000', gmtime);
    $output .= '<!-- Spun' . ($FILE eq '-' ? '' : ' from ' . fileparse($FILE))
        . " by spin 1.80 on $date -->\n";
    $output .= "<!-- $self->{id} -->\n" if $self->{id};
    $output .= "\n<body>\n";
    if ($FILE ne '-') {
        $output .= $self->placement($file);
    }
    return (1, $output);
}

# Used to save the RCS Id for the document.  Doesn't actually output anything
# (the identifier is later used in do_heading).
sub do_id {
    my ($self, $format, $id) = @_;
    $self->{id} = $id;
    return (1, '');
}

# Include an image.  The size is added to the HTML tag automatically.  Takes
# the relative path to the image and the alt text.
sub do_image {
    my ($self, $format, $image, $text) = @_;
    $image = $self->parse($image);
    $text = $self->parse($text);
    my $size = '';
    if (-f $image) {
        $size = ' ' . lc html_imgsize ($image);
    }
    my $output = qq(<img src="$image" alt="$text"$size);
    $output .= $self->format_string($format) . " />";
    return (1, $output);
}

# Include a file.  Note that this includes a file after the current paragraph,
# not immediately at the current point, which may be a bit surprising.
# Someday, I should fix that.
sub do_include {
    my ($self, $format, $file) = @_;
    $file = $self->parse($file);
    my $fh = FileHandle->new ("< $file")
        or die "$0:$FILE:$.: cannot include $file: $!\n";
    unshift (@FILES, [$fh, $file]);
    return (1, '');
}

# A link to a URL or partial URL.
sub do_link {
    my ($self, $format, $url, $text) = @_;
    my $output = '<a href="' . $self->parse($url) . '"';
    $output .= $self->format_string($format) . '>' . $self->parse($text)
      . '</a>';
    return (0, $output);
}

# Preformatted text, the same as the HTML tag.
sub do_pre {
    my ($self, $format, $text) = @_;
    my $output = $self->border();
    $output .= '<pre' . $self->format_string($format) . '>';
    $output .= $self->parse($text);
    $output .= "</pre>\n";
    return (1, $output);
}

# Used for the leading quotes that I have on many of my pages.  Takes the
# quote, the author, and the citation; the citation may be empty.  If the
# format is "broken", adds line breaks at the end of each line.
sub do_quote {
    my ($self, $format, $quote, $author, $cite) = @_;
    my $output = border . '<blockquote class="quote">';
    $self->border_start();
    my @paragraphs = $self->split_paragraphs ($quote);
    $quote = join ('', map { $self->parse($_, 1) } @paragraphs);
    $quote .= $self->border_clear();
    if ($format && $format eq 'broken') {
        $quote =~ s%(\S *)(\n\s*(?!</p>)\S)%$1<br />$2%g;
        $quote =~ s%\n<br />%\n%g;
        $quote =~ s%<p><br />%<p>%g;
    }
    $quote =~ s/\n+$//;
    if ($format) {
        my $class = $self->format_string($format);
        $quote =~ s/<p>/<p$class>/g;
    }
    $output .= $quote;
    if ($author) {
        $author = $self->parse($author);
        my $prefix = '';
        if ($format && ($format eq 'broken' || $format eq 'short')) {
            $output .= qq(<p class="attribution">\n);
        } else {
            $output .= qq(<p class="long-attrib">\n);
            $prefix = '&mdash; ';
        }
        if ($cite) {
            $cite = $self->parse($cite);
            $output .= "    $prefix$author,\n    $cite\n";
        } else {
            $output .= "    $prefix$author\n";
        }
        $output .= "</p>";
    } else {
        $output .= "\n";
    }
    $output .= "</blockquote>\n";
    return (1, $output);
}

# Given the name of a product, return the release date of the product.
sub do_release {
    my ($self, $format, $product) = @_;
    $product = $self->parse($product);
    if ($VERSIONS{$product}) {
        my $date = $VERSIONS{$product}[1];
        $date =~ s/ .*//;
        return (0, $date);
    } else {
        warn qq($0:$FILE:$.: No release date known for "$product"\n);
        return (0, '');
    }
}

# Used to save RSS feed information for the page.  Doesn't output anything
# directly; the RSS feed information is used later in do_heading.
sub do_rss {
    my ($self, $format, $url, $title) = @_;
    $url = $self->parse($url);
    $title = $self->parse($title);
    push (@RSS, [ $url, $title ]);
    return (1, '');
}

# Used to end each page, this adds the navigation links and my standard
# address block.
sub do_signature {
    my ($self) = @_;
    my $output = $self->border();
    if ($FILE eq '-') {
        $output .= "</body>\n</html>\n";
        return (1, $output);
    }
    my $file = $FILE;
    $file =~ s/\.th$/.html/;
    my $link = '<a href="%URL%">spun</a>';
    my $source = $FILE;
    $output .= $self->footer(
        $source, $file, $self->{id},
        "Last modified and\n    $link %MOD%",
        "Last $link\n    %NOW% from thread modified %MOD%"
    );
    $output .= "</body>\n</html>\n";
    return (1, $output);
}

# Insert the formatted size in bytes, kilobytes, or megabytes of some local
# file.  We could use Number::Format here, but what we're doing is simple
# enough and doesn't seem worth the trouble of another dependency.
sub do_size {
    my ($self, $format, $file) = @_;
    $file = $self->parse($file);
    unless ($file) {
        warn "$0:$FILE:$.: empty file name in \\size\n";
        return (0, '');
    }
    my ($size) = (stat $file)[7];
    unless (defined $size) {
        warn "$0:$FILE:$.: cannot stat file $file: $!\n";
        return (0, '');
    }
    my @suffixes = qw(K M G T);
    my $suffix = '';;
    while ($size > 1024 && @suffixes) {
        $size /= 1024;
        $suffix = shift @suffixes;
    }
    $size = sprintf ('%.0f', $size) . $suffix . 'B';
    return (0, $size);
}

# Generates a HTML version of the sitemap and outputs that.
sub do_sitemap {
    my ($self) = @_;
    unless (@SITEMAP) {
        warn qq($0:$FILE:$.: No sitemap file found\n);
        return (1, '');
    }
    my $output = $self->border();
    my @indents = (0);
    for my $page (@SITEMAP) {
        my ($indent, $url, $title) = @$page;
        next if $indent == 0;
        $url =~ s,^/,,;
        if ($indent > $indents[0]) {
            $output .= (' ' x $indent) . "<ul>\n";
            unshift (@indents, $indent);
        } else {
            while ($indent < $indents[0]) {
                $output .= (' ' x $indents[0]) . "</ul>\n";
                shift @indents;
            }
        }
        $output .= ' ' x $indent;
        $output .= qq(<li><a href="$url">$title</a></li>\n);
    }
    for my $indent (@indents) {
        last if $indent <= 0;
        $output .= (' ' x $indent) . "</ul>\n";
    }
    return (1, $output);
}

# Start a table.  Takes any additional HTML attributes to set for the table
# (this is ugly, but <table> takes so many attributes for which there is no
# style sheet equivalent that it's unavoidable) and the body of the table
# (which should consist of \tablehead and \tablerow lines).
sub do_table {
    my ($self, $format, $options, $body) = @_;
    my $tag = $options ? "table $options" : 'table';
    return $self->block($tag, '', $format, $body);
}

# A heading of a table.  Takes the contents of the cells in that heading.
sub do_tablehead {
    my ($self, $format, @cells) = @_;
    my $output = '  <tr' . $self->format_string($format) . ">\n";
    for (@cells) {
        $output .= '    ';
        $output .= $self->enclose('th', $self->parse ($_) . $self->border());
        $output .= "\n";
    }
    $output .= "  </tr>\n";
    return (1, $output);
}

# A data line of a table.  Takes the contents of the cells in that row.
sub do_tablerow {
    my ($self, $format, @cells) = @_;
    my $output = '  <tr' . $self->format_string($format) . ">\n";
    for (@cells) {
        $output .= '    ';
        $output .= $self->enclose('td', $self->parse($_) . $self->border());
        $output .= "\n";
    }
    $output .= "  </tr>\n";
    return (1, $output);
}

# Given the name of a product, return the version number of that product.
sub do_version {
    my ($self, $format, $product) = @_;
    $product = $self->parse($product);
    if ($VERSIONS{$product}) {
        return (0, $VERSIONS{$product}[0]);
    } else {
        warn qq($0:$FILE:$.: No version known for "$product"\n);
        return (0, '');
    }
}

sub do_h1 { my ($self, @args) = @_; $self->heading(1, @args); }
sub do_h2 { my ($self, @args) = @_; $self->heading(2, @args); }
sub do_h3 { my ($self, @args) = @_; $self->heading(3, @args); }
sub do_h4 { my ($self, @args) = @_; $self->heading(4, @args); }
sub do_h5 { my ($self, @args) = @_; $self->heading(5, @args); }
sub do_h6 { my ($self, @args) = @_; $self->heading(6, @args); }

# The table of available commands.  First column is the number of arguments,
# second column is the handler, and the third column is whether this is its
# own top-level element or whether it needs to be wrapped in <p> tags.  A
# count of -1 means pull off as many arguments as we can find.
%commands = (block      => [  1, 'do_block'      ],
             bold       => [  1, 'do_bold'       ],
             break      => [  0, 'do_break'      ],
             bullet     => [  1, 'do_bullet'     ],
             class      => [  1, 'do_class'      ],
             cite       => [  1, 'do_cite'       ],
             code       => [  1, 'do_code'       ],
             desc       => [  2, 'do_desc'       ],
             div        => [  1, 'do_div'        ],
             emph       => [  1, 'do_emph'       ],
             entity     => [  1, 'do_entity'     ],
             heading    => [  2, 'do_heading'    ],
             h1         => [  1, 'do_h1'         ],
             h2         => [  1, 'do_h2'         ],
             h3         => [  1, 'do_h3'         ],
             h4         => [  1, 'do_h4'         ],
             h5         => [  1, 'do_h5'         ],
             h6         => [  1, 'do_h6'         ],
             id         => [  1, 'do_id'         ],
             image      => [  2, 'do_image'      ],
             include    => [  1, 'do_include'    ],
             italic     => [  1, 'do_italic'     ],
             link       => [  2, 'do_link'       ],
             number     => [  1, 'do_number'     ],
             pre        => [  1, 'do_pre'        ],
             quote      => [  3, 'do_quote'      ],
             release    => [  1, 'do_release'    ],
             rss        => [  2, 'do_rss'        ],
             rule       => [  0, 'do_rule'       ],
             signature  => [  0, 'do_signature'  ],
             sitemap    => [  0, 'do_sitemap'    ],
             size       => [  1, 'do_size'       ],
             strike     => [  1, 'do_strike'     ],
             strong     => [  1, 'do_strong'     ],
             sub        => [  1, 'do_sub'        ],
             sup        => [  1, 'do_sup'        ],
             table      => [  2, 'do_table'      ],
             tablehead  => [ -1, 'do_tablehead'  ],
             tablerow   => [ -1, 'do_tablerow'   ],
             under      => [  1, 'do_under'      ],
             version    => [  1, 'do_version'    ]);

##############################################################################
# Interface
##############################################################################

# Convert thread to HTML.  Be aware that the working directory from which this
# function is run matters a great deal, since thread may contain relative
# paths to files that the spinning process needs to access.
#
# $in_fh   - Input file handle of thread
# $in_path - Input file path, used for error reporting
# $out_fh  - Output file handle to which to write the HTML
sub _spin {
    my ($self, $in_fh, $in_path, $out_fh) = @_;
    @FILES = ([$in_fh, $in_path]);

    # Initialize object state for a new document.
    $self->{id}      = undef;
    $self->{macros}  = {};
    $self->{out_fh}  = $out_fh;
    $self->{space}   = q{};
    $self->{strings} = {};

    # Set initial block structure state.
    $self->border_start();

    # Parse the thread file a paragraph at a time (but pick up macro contents
    # that are continued across paragraphs.
    #
    # We maintain the stack of files that we're parsing in @FILES, and
    # do_include will unshift new file handle and filename pairs onto that
    # stack.  That means that the top of the stack may change any time we call
    # parse, so we have to update the input file handle and $FILE (for error
    # reporting) each time through the loop.
    local $/ = q{};
    while (@FILES) {
        ($in_fh, $FILE) = $FILES[0]->@*;
        while (defined(my $para = <$in_fh>)) {
            if ("\n" !~ m{ \015 }xms && $para =~ m{ \015 }xms) {
                $self->_warning(
                    "found CR characters; are your line endings correct?");
            }
            my $open_count  = ($para =~ tr{\[}{});
            my $close_count = ($para =~ tr{\]}{});
            while (!eof && $open_count > $close_count) {
                my $extra = <$in_fh>;
                $open_count  += ($extra =~ tr{\[}{});
                $close_count += ($extra =~ tr{\]}{});
                $para .= $extra;
            }
            my $result = $self->parse($self->escape($para), 1);
            $result =~ s{ \A (?:\s*\n)+ }{}xms;
            if ($result !~ m{ \A \s* \z }xms) {
                $self->_output($result);
            }
            ($in_fh, $FILE) = $FILES[0]->@*;
        }
        shift(@FILES);
    }

    # Close open tags and print any deferred whitespace.
    print {$out_fh} $self->border_clear(), $self->{space};

    # Clean up per-file data so that it's not carried to the next file.
    undef @RSS;
}

##############################################################################
# External converters
##############################################################################

# Given the command to run to generate the page, the file to save the output
# in, and an anonymous sub that takes three arguments, the first being the
# captured blurb, the second being the document ID if found, and the third
# being the base name of the output file, and prints out a last modified line,
# handle a call to an external converter.
sub run_converter {
    my ($self, $command, $output, $footer) = @_;
    my @page = `$command`;
    if ($? != 0) {
        $command =~ s/ .*//;
        die "$0: command failed with exit status ", ($? >> 8), "\n";
    }
    open (OUT, "> $output") or die "$0: cannot create $output: $!\n";
    my $file = $output;
    $file =~ s%.*/%%;

    # Grab the first few lines of input, looking for a blurb and Id string.
    # Give up if we encounter <body> first.  Also look for a </head> tag and
    # add the navigation link tags before it, if applicable.  Add the
    # navigation bar right at the beginning of the body.
    my ($blurb, $docid);
    local $_;
    while (defined ($_ = shift @page)) {
        if (/<!--\s*(\$Id.*?)\s*-->/) {
            $docid = $1;
        }
        if (/<!--\s*((?:Generated|Converted).*?)\s*-->/) {
            $blurb = $1;
            $blurb =~ s/ \d\d:\d\d:\d\d -0000//;
            $blurb =~ s/ \(\d{4}-\d\d-\d\d\)//;
        }
        if (m%^</head>%) {
            print OUT $self->sitelinks($output);
        }
        print OUT $_;
        if (m%<body%i) {
            print OUT $self->placement($output);
            last;
        }
    }
    warn "$0: malformed HTML output from $command\n" unless @page;

    # Snarf input and write it to output until we see </body>, which is our
    # signal to start adding things.  We just got very confused if </body> was
    # on the same line as <body>, so don't do that.
    print OUT $_ while (defined ($_ = shift @page) && !m%</body>%i);

    # Add the footer and finish with the output.
    print OUT $footer->($blurb, $docid, $file);
    print OUT $_, @page if defined;
    close OUT;
}

# A wrapper around the cl2xhtml script, used to handle .changelog pointers in
# a tree being spun.  Adds the navigation links and the signature to the
# cl2xhtml output.
sub cl2xhtml {
    my ($self, $source, $output, $options, $style) = @_;
    $style ||= $self->{style_url} . 'changelog.css';
    my $command = "cl2xhtml $options -s $style $source";
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        $blurb =~ s%cl2xhtml%\n<a href="$URL">cl2xhtml</a>% if $blurb;
        $self->footer($source, $file, $id, $blurb, $blurb);
    };
    $self->run_converter($command, $output, $footer);
}

# A wrapper around the cvs2xhtml script, used to handle .log pointers in a
# tree being spun.  Adds the navigation links and the signature to the
# cvs2xhtml output.
sub cvs2xhtml {
    my ($self, $source, $output, $options, $style) = @_;
    my $dir = $source;
    $dir =~ s%/+[^/]+$%%;
    my $name = $source;
    $name =~ s%^.*/%%;
    $options .= " -n $name" unless $options =~ /-n /;
    $style ||= $self->{style_url} . 'cvs.css';
    $options .= " -s $style";
    my $command = "(cd $dir && cvs log $name) | cvs2xhtml $options";
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        $blurb =~ s%cvs2xhtml%\n<a href="$URL">cvs2xhtml</a>% if $blurb;
        $self->footer($source, $file, $id, $blurb, $blurb);
    };
    $self->run_converter($command, $output, $footer);
}

# A wrapper around the faq2html script, used to handle .faq pointers in a tree
# being spun.  Adds the navigation links and the signature to the faq2html
# output.
sub faq2html {
    my ($self, $source, $output, $options, $style) = @_;
    $style ||= $self->{style_url} . 'faq.css';
    my $command = "faq2html $options -s $style $source";
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        $blurb =~ s%faq2html%\n<a href="$URL">faq2html</a>%;
        $self->footer($source, $file, $id, $blurb, $blurb);
    };
    $self->run_converter($command, $output, $footer);
}

# A wrapper around pod2thread and spin -f, used to handle .pod pointers in a
# tree being spun.  Adds the navigation links and the signature to the output.
sub pod2html {
    my ($self, $source, $output, $options, $style) = @_;
    $options = '-n' unless $options;
    my $styles = ($self->{style_url} ? " -s $self->{style_url}" : '');
    $style = 'pod' unless $style;
    $options .= " -s $style";
    my $command = "pod2thread $options $source | $FULLPATH spin-file$styles";
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        my $link = '<a href="%URL%">spun</a>';
        $self->footer(
            $source, $file, $id,
            "Last modified and\n    $link %MOD%",
            "Last $link\n    %NOW% from POD modified %MOD%"
        );
    };
    $self->run_converter ($command, $output, $footer);
}

##############################################################################
# Per-file operations
##############################################################################

# Given a pointer file, read the master file name and any options, returning
# them as a list with the newlines chomped off.
#
# $file - The path to the file to read
#
# Returns: List of the master file, any command-line options, and the style
#          sheet to use, as strings
#  Throws: Text exception if no master file is present in the pointer
#          autodie exception if the pointer file could not be read
sub _read_pointer {
    my ($self, $file) = @_;

    # Read the pointer file.
    open(my $pointer, '<', $file);
    my $master = <$pointer>;
    my $options = <$pointer>;
    my $style = <$pointer>;
    close($pointer);

    # Clean up the contents.
    if (!$master) {
        die "no master file specified in $file";
    }
    chomp($master);
    if (defined($options)) {
        chomp($options);
    } else {
        $options = q{};
    }
    if (defined($style)) {
        chomp($style);
    }

    # Return the details.
    return ($master, $options, $style);
}

# This routine is called by File::Find for every file in the source tree.  It
# decides what to do with each file, whether spinning it or copying it.
#
# Throws: Text exception on any processing error
#         autodie exception if files could not be accessed or written
sub _process_file {
    my ($self) = @_;
    my $file = $_;
    return if ($file eq '.');
    for my $regex ($self->{excludes}->@*) {
        if ($file =~ m{$regex}xms) {
            $File::Find::prune = 1;
            return;
        }
    }
    my $input  = $File::Find::name;
    my $output = $input;
    $output =~ s{ \A \Q$self->{source}\E }{$self->{output}}xms
      or die "$input file out of tree?\n";
    my $shortout = $output;
    $shortout =~ s{ \A \Q$self->{output}\E }{...}xms;

    # Conversion rules for pointers.  The key is the extension, the first
    # value is the name of the command for the purposes of output, and the
    # second is the name of the method to run.
    my %rules = (
        changelog => ['cl2xhtml',   'cl2xhtml'],
        faq       => ['faq2html',   'faq2html'],
        log       => ['cvs2xhtml',  'cvs2xhtml'],
        rpod      => ['pod2thread', 'pod2html'],
    );

    # Figure out what to do with the input.
    if (-d $file) {
        $self->{generated}{$output} = 1;
        if (-e $output && !-d $output) {
            die "cannot replace $output with a directory\n";
        } elsif (!-d $output) {
            print "Creating $shortout\n";
            mkdir($output, 0755);
        }
        my $rss_path = File::Spec->catfile($file, '.rss');
        if (-f $rss_path) {
            systemx('spin-rss', '-b', $file, $rss_path);
        }
    } elsif ($file =~ m{ [.] th \z }xms) {
        $output   =~ s{ [.] th \z }{.html}xms;
        $shortout =~ s{ [.] th \z }{.html}xms;
        $self->{generated}{$output} = 1;
        my $relative = $input;
        $relative =~ s{ ^ \Q$self->{source}\E / }{}xms;
        my $time = $self->{depends}{$relative} || 0;
        if (-e $output) {
            return if (-M $file >= -M $output && (stat($output))[9] >= $time);
        }
        print "Spinning $shortout\n";
        open(my $in_fh,  '<', $input);
        open(my $out_fh, '>', $output);
        $self->_spin($in_fh, $input, $out_fh);
        close($in_fh);
        close($out_fh);
    } else {
        my ($extension) = ($file =~ m{ [.] ([^.]+) \z }xms);
        if (defined($extension) && $rules{$extension}) {
            my ($name, $sub) = $rules{$extension}->@*;
            $output   =~ s{ [.] \Q$extension\E \z }{.html}xms;
            $shortout =~ s{ [.] \Q$extension\E \z }{.html}xms;
            $self->{generated}{$output} = 1;
            my ($file, $options, $style) = $self->_read_pointer($input);
            if (-e $output && -e $file) {
                return if (-M $file >= -M $output && -M $file >= -M $output);
            }
            print "Running $name for $shortout\n";
            $sub->($file, $output, $options, $style);
        } else {
            $self->{generated}{$output} = 1;
            if (!-e $output || -M $file < -M $output) {
                print "Updating $shortout\n";
                copy($file, $output)
                  or die "$0: copy of $input to $output failed: $!\n";
            }
        }
    }
}

# This routine is called by File::Find for every file in the destination tree
# in depth-first order, if the user requested file deletion of files not
# generated from the source tree.  It checks each file to see if it is in the
# $self->{generated} hash that was generated during spin processing, and if
# not, removes it.
#
# Throws: autodie exception on failure of rmdir or unlink
sub _delete_files {
    my ($self) = @_;
    return if ($_ eq '.' || $_ eq '..');
    my $file = $File::Find::name;
    return if $self->{generated}{$file};
    my $shortfile = $file;
    $shortfile =~ s{ ^ \Q$self->{output}\E }{...}xms;
    print "Deleting $shortfile\n";
    if (-d $file) {
        rmdir($file);
    } else {
        unlink($file);
    }
    return;
}

##############################################################################
# Public interface
##############################################################################

# Create a new App::DocKnot::Spin object, which will be used for subsequent
# calls.
#
# $args  - Anonymous hash of arguments with the following keys:
#   delete    - Whether to delete files missing from the source tree
#   exclude   - List of regular expressions matching file names to exclude
#   style-url - Partial URL to style sheets
#
# Returns: Newly created object
#  Throws: Text exceptions on invalid metadata directory path
sub new {
    my ($class, $args_ref) = @_;

    # Treat all exclude arguments as regular expressions and add them to the
    # global exclusion list.
    my @excludes = @EXCLUDES;
    if ($args_ref->{exclude}) {
        push(@excludes, map { qr{$_} } $args_ref->{exclude}->@*);
    }

    # Add a trailing slash to the partial URL for style sheets.
    my $style_url = $args_ref->{'style-url'} // q{};
    if ($style_url) {
        $style_url =~ s{ /* \z }{/}xms;
    }

    # Used to invoke spin as a filter.
    $FULLPATH = $0;

    # Create and return the object.
    my $self = {
        delete    => $args_ref->{delete},
        excludes  => [@excludes],
        style_url => $style_url,
    };
    bless($self, $class);
    return $self;
}

# Spin a single file of thread to HTML.
#
# $input  - Input file (if not given, assumes standard input)
# $output - Output file (if not given, assumes standard output)
#
# Raises: Text exception on processing error
sub spin_file {
    my ($self, $input, $output) = @_;
    my $cwd = getcwd();
    my ($in_fh, $out_fh);

    # Canonicalize input and output paths.
    if (defined($input)) {
        $self->{source} = realpath($input)
           or die "cannot canonicalize $input: $!\n";
        if (!-f $self->{source}) {
            die "input file $self->{source} must be a regular file";
        }
    } else {
        $self->{input} = '-';
    }
    if (defined($output)) {
        $self->{output} = realpath($output)
          or die "cannot canonicalize $output: $!\n";
        $self->{output} =~ s{ /+ \z }{}xms;
    } else {
        $self->{output} = '-';
    }

    # Open the output file.
    if ($self->{output} eq '-') {
        open($out_fh, '>&STDOUT');
    } else {
        open($out_fh, '>', $self->{output});
    }

    # When spinning a single file, the input file must not be a directory.  We
    # do the work from the directory of the file to ensure that relative file
    # references resolve properly.
    if ($self->{source} eq '-') {
        open($in_fh, '<&STDIN');
    } else {
        open($in_fh, '<', $self->{source});
        my (undef, $input_dir) = fileparse($self->{source});
        chdir($input_dir);
    }

    # Do the work.
    $self->_spin($in_fh, $self->{source}, $out_fh);

    # Clean up and restore the working directory.
    close($in_fh);
    close($out_fh);
    chdir($cwd);
    return;
}

# Spin a directory of files into a web site.
#
# $input  - The input directory
# $output - The output directory (which may not exist)
#
# Raises: Text exception on processing error
sub spin_tree {
    my ($self, $input, $output) = @_;

    # Canonicalize and check input.
    $input = realpath($input) or die "cannot canonicalize $input: $!\n";
    if (!-d $input) {
        die "input tree $input must be a directory";
    }
    $self->{source} = $input;

    # Canonicalize and check output.
    if (!-d $output) {
        print "Creating $output\n";
        mkdir($output, 0755);
    }
    $output = realpath($output) or die "cannot canonicalize $output: $!\n";
    $self->{output} = $output;

    # Read metadata from the top of the input directory.
    $self->_read_sitemap(File::Spec->catfile($input, '.sitemap'));
    $self->_read_versions(File::Spec->catfile($input, '.versions'));
    if (-d File::Spec->catdir($input, '.git')) {
        $self->{repository} = Git::Repository->new(work_tree => $input);
    }

    # Process the input tree.
    find(sub { $self->_process_file(@_) }, $input);
    if ($self->{delete}) {
        finddepth(sub { $self->_delete_files(@_) }, $output);
    }
    return;
}

1;

##############################################################################
# Documentation
##############################################################################

=for stopwords
Allbery RCS RSS XHTML YYYY-MM-DD -dhv faq2html respin respun spin-rss
cl2xhtml cvs2xhtml preformatted

=head1 NAME

spin - Translate thread, an HTML macro language, into XHTML

=head1 SYNOPSIS

spin [B<-dhv>] [B<-e> I<pattern> ...] [B<-s> I<url>] I<source> [I<output>]

=head1 REQUIREMENTS

Perl 5.005 or later and the Image::Size and Text::Balanced modules.  Also
expects to find B<faq2html>, B<cvs2xhtml>, B<cl2xhtml>, and B<pod2thread>
to convert certain types of files.  The Git::Repository module is required
to determine last change dates for thread source from Git history.

=head1 DESCRIPTION

B<spin> implements a fairly simple macro language that expands out into
XHTML, as well as serving as a tool to maintain a set of web pages,
updating a staging area with the latest versions, converting pages written
in the macro language (named "thread"), and running B<faq2html> where
directed.

When invoked with the B<-f> option, B<spin> works in filter mode, reading
thread from stdin and writing the converted output to stdout.  Some
features, such as appending a signature or navigation links, are disabled
in this mode.

If I<source> is a regular file, I<output> should be the name of the file
into which to put the output, and B<spin> will process only that one file
(which is assumed to be thread).  I<output> may be omitted to send the
output to standard output. The same features are disabled in this mode as
in filter mode.

Otherwise, each file in the directory I<source> is examined recursively.
For each one, it is either copied verbatim into the same relative path
under I<output>, used as instructions to an external program (see the
details on converters below), or converted to HTML.  The HTML output for
external programs or for converted pages is put under I<output> with the
same file name but with the extension changed to C<.html>.  Missing
directories are created.  If the B<-d> flag is given, files and
directories in the I<output> directory that do not correspond to files in
the I<source> directory will be deleted.

Files that end in C<.th> are assumed to be in thread and are turned into
HTML.  For the details of the thread language, see L<THREAD LANGUAGE>
below.

Files that end in various other extensions are taken to be instructions to
run an external converter on a file.  The first line of such a pointer
file should be the path to the source file, the second line any arguments
to the converter, and the third line the style sheet to use if not the
default.  Which converter to run is based on the extension of the file as
follows:

    .changelog  cl2xhtml
    .faq        faq2html
    .log        cvs log <file> | cvs2xhtml
    .rpod       pod2thread <file> | spin -f

All other files not beginning with a period are copied as-is, except that
files or directories named F<CVS>, F<Makefile>, or F<RCS> are ignored.  As
an exception, F<.htaccess> files are also copied over.

B<spin> looks for a file named F<.sitemap> at the top of the I<source>
directory and reads it for navigation information to generate the
navigation links at the top and bottom of each page.  The format of this
file is one line per web page, with indentation showing the tree
structure, and with each line formatted as a partial URL, a colon, and a
page description.  If two pages at the same level aren't related, a line
with three dashes should be put between them at the same indentation
level.  The partial URLs should start with / representing the top of the
hierarchy (the I<source> directory), but all generated links will be
relative.

Here's an example of a simple F<.sitemap> file:

    /personal/: Personal Information
      /personal/contact.html: Contact Information
      ---
      /personal/projects.html: Current Projects
    /links/: Links
      /links/lit.html: Other Literature
      /links/music.html: Music
      /links/sf.html: Science Fiction and Fantasy

This defines two sub-pages of the top page, /personal/ and /links/.
/personal/ has two pages under it that are not part of the same set (and
therefore shouldn't have links to each other).  /links/ has three pages
under it which are part of a set and should be linked between each other.

If F<.sitemap> is present, this navigation information will also be put
into the <head> section of the resulting HTML file as <link> tags.  Some
browsers will display this information as a navigation toolbar.

At the bottom of each page, B<spin> will add information about when the file
was last modified and generated.

B<spin> looks for a file named F<.versions> at the top of the I<source>
directory and reads it for version information.  If it is present, each
line should be of the form:

    <product>  <version>  <date>  <time>  <files>

where <product> is the name of a product with a version number, <version>
is the version, <date> and <time> specify the time of the last release (in
ISO YYYY-MM-DD HH:MM:SS format and the local time zone), and <files> is
any number of paths relative to I<source>, separated by spaces, listing
source thread files that use \version or \release for <product>.  If there
are more files than can be listed on one line, additional files can be
listed on the next and subsequent lines so long as they all begin with
whitespace (otherwise, they'll be taken to be other products).  This
information is not only used for the \version and \release commands, but
also as dependency information.  If the date of a release is newer than
the timestamp of the output from one of the files listed in <files>, that
file will be spun again even if it hasn't changed (to pick up the latest
version and release information).

B<spin> looks for a file named F<.rss> in each directory it processes.  If
one is found, B<spin> runs B<spin-rss> on that file, passing the B<-b>
option to point to the directory about to be processed.  B<spin> does this
before processing the files in that directory, so B<spin-rss> can create
or update files that will then be processed by B<spin> as normal.

If there is a directory named F<.git> at the top of the source tree,
B<spin> will assume that the source is a Git repository and will try to
use C<git log> to determine the last modification date of files.

=head1 THREAD LANGUAGE

=head2 Basic Syntax

A thread file is mostly plain ASCII text with a blank line between
paragraphs.  There is no need to explicitly mark paragraphs; paragraph
boundaries will be inferred from the blank line between them and the
appropriate <p> tags will be added to the HTML output.  There is no need
to escape any character except C<\> (which should be written as C<\\>) and
an unbalanced [ or ] (which should be written as C<\entity[91]> or
C<\entity[93]> respectively).  Escaping [ or ] is not necessary if the
brackets are balanced within the paragraph, and therefore is only rarely
needed.

Commands begin with C<\>.  For example, the command to insert a line break
(corresponding to the <br> tag in HTML) is \break.  If the command takes
arguments, they are enclosed in square brackets after the command.  If
there are multiple arguments, they are each enclosed in square brackets
and follow each other.  Any amount of whitespace (but nothing else) is
allowed between the command and the arguments, or between the arguments.
So, for example, all of the following are entirely equivalent:

    \link[index.html][Main page]
    \link  [index.html]  [Main page]

    \link[index.html]
    [Main page]

    \link
    [index.html]
    [Main page]

(\link is a command that takes two arguments.)

Commands can take multiple paragraphs of text as arguments in some cases
(for things like list items).  Commands can be arbitrarily nested.

Some commands take an additional optional argument which specifies the
class attribute for that HTML tag, for use with style sheets, or the id
attribute, for use with style sheets or as an anchor.  That argument is
enclosed in parentheses and placed before any other arguments.  If the
argument begins with C<#>, it will be taken to be an id.  Otherwise, it
will be taken as a class.  For example, a first-level heading is normally
written as:

    \h1[Heading]

(with one argument).  Either of the following will add a class attribute
of C<header> to that HTML container that can be referred to in style
sheets:

    \h1(header)[Heading]
    \h1  (header)  [Heading]

and the following would add an id attribute of C<intro> to the heading so
that it could be referred to with the anchor C<#intro>:

    \h1(#intro)[Introduction]

Note that the heading commands have special handling for id attributes;
see below for more details.

=head2 Basic Format

There are two commands that are required to occur in every document.  The
first is \heading, which must occur before any regular page text.  It
takes two arguments, the first of which is the page title (the title that
shows up in the window title bar for the browser and is the default text
for bookmarks, not anything that's displayed as part of the body of the
page) and the second of which is the style sheet to use.  If there is no
style sheet for this page, the second argument should be empty ([]).

The second required command is \signature, which must be the last command
in the file.  \signature will take care of appending the signature,
appending navigation links, closing any open blocks, and any other cleanup
that has to happen at the end of a generated HTML page.

It is also highly recommended, if you are using Subversion, CVS, or RCS
for revision control, to put \id[$Z<>Id$] as the first command in each
file.  In Subversion, you will also need to enable keyword expansion with
C<svn propset svn:keywords Id I<file>>.  B<spin> will then take care of
putting the last modified date in the footer for you based on the Id
timestamp (which may be more accurate than the last modified time of the
thread file).  If you are using Git, you don't need to include anything
special in the thread source; as long as the source directory is the
working tree of a Git repository, B<spin> will use Git to determine the
last modification date of the file.

You can include other files with the \include command, although it has a
few restrictions.  The \include command must appear either at the
beginning of the file or after a blank line and should be followed by a
blank line, and you should be careful not to include the same file
recursively.  Thread files will not be automatically respun when included
files change, so you will need touch the thread file to force it to be
respun.

=head2 Block Commands

Block commands are commands that should occur in a paragraph by
themselves, not contained in a paragraph with other text.  They indicate
high-level structural elements of the page.  Three of them were already
discussed above:

=over 4

=item \heading[<title>][<style>]

As described above, this sets the page title to <title> and the style
sheet to <style>.  If the B<-s> option was given, that base URL will be
prepended to <style> to form the URL for the style sheet; otherwise,
<style> will be used verbatim as a URL.

=item \id[$Z<>Id$]

Tells B<spin> the Subversion, CVS, or RCS revision number and time.  This
string is embedded verbatim in an HTML comment near the beginning of the
generated output as well as used for the last modified information added
by the \signature command.  For this command to behave properly, it must
be given before \heading.

=item \include[<file>]

Include <file> after the current paragraph.  If multiple files are
included in the same paragraph, they're included in reverse order, but
this behavior may change in later versions of B<spin>.  It's strongly
recommended to always put the \include command in its own paragraph.
Don't put \heading or \signature into an included file; the results won't
be correct.

=back

Here are the rest of the block commands.  Any argument of <text> can be
multiple paragraphs and contain other embedded block commands (so you can
nest a list inside another list, for example).

=over 4

=item \block[<text>]

Put text in an indented block, equivalent to <blockquote> in HTML.  Used
primarily for quotations or things like license statements embedded in
regular text.

=item \bullet[<text>]

<text> is formatted as an item in a bullet list.  This is like <li> inside
<ul> in HTML, but the surrounding list tags are inferred automatically and
handled correctly when multiple \bullet commands are used in a row.
Normally, <text> is treated like a paragraph.

If used with a class attribute of C<packed>, such as with:

    \bullet(packed)[First item]

then the <text> argument will not be treated as a paragraph and will not
be surrounded in <p> tags.  No block commands should be used inside this
type of \bullet command.  This variation will, on most browsers, not put
any additional whitespace around the line and will look better for
bulleted lists where each item is a single line.

=item \desc[<heading>][<text>]

An element in a description list, where each item has a tag <heading> and
an associated body text of <text>, like <dt> and <dd> in HTML.  As with
\bullet, the <dl> tags are inferred automatically.

=item \h1[<heading>] .. \h6[<heading>]

Level one through level six headings, just like <h1> .. <h6> in HTML.  If
given an id argument, such as:

    \h1(#anchor)[Heading]

then not only will an id attribute be added to the <h1> container but the
text of the heading will also be enclosed in an <a name> container to
ensure that C<#anchor> can be used as an anchor in a link even in older
browsers that don't understand id attributes.  This is special handling
that only works with \h1 through \h6, not with other commands.

=item \number[<text>]

<text> is formatted as an item in a numbered list, like <li> inside <ol>
in HTML.  As with \bullet and \desc, the surrounding tags are inferred
automatically.  As with \bullet, a class attribute of C<packed> will omit
the paragraph tags around <text> for better formatting with a list of
short items.  See the description under \bullet for more information.

=item \pre[<text>]

Insert <text> preformatted, preserving spacing and line breaks.  This uses
the HTML <pre> tag, and therefore is normally also shown in a fixed-width
font by the browser.

When using \pre inside indented blocks or lists, it's worth bearing in
mind how browsers show indentation with \pre.  Normally, the browser
indents text inside \pre relative to the enclosing block, so you should
only put as much whitespace before each line in \pre as those lines should
be indented relative to the enclosing text.  However B<lynx>,
unfortunately, indents relative to the left margin, so it's difficult to
use indentation that looks correct in both B<lynx> and other browsers.

=item \quote[<text>][<author>][<work>]

Used for quotes at the top of a web page.  The whole text will be enclosed
in a <blockquote> tag with class C<quote> for style sheets.  <text> may be
multiple paragraphs, and then a final paragraph will be added (with class
C<attribution>) containing the author, a comma, and the <work> inside
<cite> tags.  <work> can be omitted by passing an empty third argument.
If \quote is given a class argument of C<broken>, <text> will be treated
as a series of lines and a line break (C<< <br /> >>) will be added to the
end of each line.

=item \rss[<url>][<title>]

Indicates that this page has a corresponding RSS feed at the URL <url>.
The title of the RSS feed (particularly important if a page has more than
one feed) is given by <title>.  The feed links are included in the page
header output by \heading, so this command must be given before \heading
to be effective.

=item \rule

A horizontal rule, <hr> in HTML.

=item \sitemap

Inserts an unordered list showing the structure of the whole site,
provided that a F<.sitemap> file was found at the root of the I<source>
directory and B<spin> wasn't run as a filter or on a single file.  If
F<.sitemap> wasn't found or if B<spin> is running as a filter or on a
single file, inserts nothing.

Be aware that B<spin> doesn't know whether a file contains a \sitemap
command and hence won't know to respin a file when the F<.sitemap> file
has changed.  You will need touch the source file to force it to be
respun.

=item \table[<options>][<body>]

Creates a table.  The <options> text is added verbatim to the <table> tag
in the generated HTML, so it can be used to set various HTML attributes
like C<cellpadding> that aren't easily accessible in a portable fashion
from style sheets.  <body> is the body of the table, which should
generally consist exclusively of \tablehead and \tablerow commands.

The descriptions are somewhat hard to read, so here's a sample table:

    \table[rules="cols" borders="1"][
        \tablehead [Older Versions]     [Webauth v3]
        \tablerow  [suauthSidentSrvtab] [WebAuthKeytab]
        \tablerow  [suauthFailAction]   [WebAuthLoginURL]
        \tablerow  [suauthDebug]        [WebAuthDebug]
        \tablerow  [suauthProxyHeader]  [(use mod_headers)]
    ]

The table support is currently preliminary.  I've not yet found a good way
of expressing tables, and it's possible that the syntax will change later.

=item \tablehead[<cell>][<cell>] ...

A heading row in a table.  \tablehead takes any number of <cell>
arguments, wraps them all in a <tr> table row tag, and puts each cell
inside <th>.  If a cell should have a certain class attribute, the easiest
way to do that is to use a \class command around the <cell> text, and the
class attribute will be "lifted" up to become an attribute of the
enclosing <th> tag.

=item \tablerow[<cell>][<cell>] ...

A regular row in a table.  \tablerow takes any number of <cell> arguments,
wraps them all in a <tr> table row tag, and puts each cell inside <td>.
If a cell should have a certain class attribute, the easiest way to do
that is to use a \class command around the <cell> text, and the class
attribute will be "lifted" up to become an attribute of the enclosing <th>
tag.

=back

=head2 Inline Commands

Inline commands can be used in the middle of a paragraph intermixed with
other text.  Most of them are simple analogs to their HTML counterparts.
All of the following take a single argument (the enclosed text) and map to
simple HTML tags:

    \bold       <b></b>                 (usually use \strong)
    \cite       <cite></cite>
    \code       <code></code>
    \emph       <em></em>
    \italic     <i></i>                 (usually use \emph)
    \strike     <strike></strike>       (should use styles)
    \strong     <strong></strong>
    \sub        <sub></sub>
    \sup        <sup></sup>
    \under      <u></u>                 (should use styles)

Here are the other inline commands:

=over 4

=item \break

A forced line break, <br> in HTML.

=item \class[<text>]

Does nothing except wrap <text> in an HTML <span> tag.  The only purpose
of this command is to use it with a class argument that can be used in a
style sheet.  For example, you might write:

    \class(red)[A style sheet can make this text red.]

so that the style sheet can then refer to class C<red> and change its
color.

=item \entity[<code>]

An HTML entity with code <code>.  Basically, becomes &<code>; in the
generated HTML, or &#<code>; if <code> is entirely numeric.  About the
only time you'd need to use this is for non-ASCII characters (European
names, for example) or if you need a literal [ or ] that isn't balanced.

=item \image[<url>][<text>]

Insert an inline image.  <text> is the alt text for the image (which will
be displayed on non-graphical browsers).  Height and width tags are added
automatically assuming that <url> is a relative URL in the same tree of
files as the thread source.

=item \link[<url>][<text>]

Create a link to <url> with link text <text>.  Basically <a href=""></a>.

=item \release[<product>]

Replaced with the date portion of the version information for <product>,
taken from the F<.versions> file at the top of the source tree.  The date
will be returned in the UTC time zone, not the local time zone.

=item \size[<file>]

Replaced with the size of <file> in B, KB, MB, GB, or TB as is most
appropriate, without decimal places.  The next largest unit is used if the
value is larger than 1024.  1024 is used as the scaling factor, not 1000.

=item \version[<product>]

Replaced with the version number for <product>, taken from the
F<.versions> file at the top of the source tree.

=back

=head2 Defining New Macros

One of the important things that thread supports over HTML is the ability
to define new macros on the fly.  If there are particular constructs that
are frequently used on the page, you can define a macro at the top of that
page and then just use it repeatedly throughout the page.

A string can be defined with the command:

    \=[<string>][<value>]

where <string> is the name that will be used (can only be alphanumerics
plus underscore) and <value> is the value that string will expand into.
Any later occurrence of \=<string> in the file will be replaced with
<value>.  For example:

    \=[HOME][http://www.stanford.edu/]

will cause any later occurrences of \=HOME in the file to be replaced with
the text C<http://www.stanford.edu/>.  This can be useful for things like
URLs for links, so that all the URLs can be collected at the top of the
page for easy updating.

A new macro can be defined with the command:

    \==[<name>][<arguments>][<definition>]

where <name> is the name of the macro (again consisting only of
alphanumerics or underscore), <arguments> is the number of arguments that
it takes, and <definition> is the definition of the macro.  When the macro
is expanded, any occurrence of \1 in the definition is replaced with the
first argument, any occurrence of \2 with the second argument, and so
forth.

For example:

    \==[bolddesc] [2] [\desc[\bold[\1]][\2]]

defines a new macro \bolddesc that takes the same arguments as the regular
\desc command but always wraps the first argument, the heading, in
<strong>.

=head1 BUGS

Currently, the style sheets for B<cl2xhtml>, B<cvs2xhtml>, B<faq2html>,
and B<pod2thread> are hard-coded into this program to fit my web pages.
This makes this program awkward for others to use, since the style sheet
has to be specified in every pointer file if they're using different
names.

There is no way to configure how navigation links are added if the sitemap
support is used.

\include needs some work to make it behave as expected without requiring
that each \include be in its own paragraph.  It should be possible to
support \heading and \signature in included files without breaking the
navigation link support.

\sitemap can only be used at the top of the web site or the links would be
wrong.  It needs to do relative adjustment of the links.

The sitemap support currently only adds previous, next, up, and top links
in the header of the generated web page.  Most browsers that support this
functionality also support first and last links, and the information is
available in the sitemap file to generate those.  They should also be
included.

=head1 SEE ALSO

cl2xhtml(1), cvs2xhtml(1), faq2html(1), pod2thread(1), spin-rss(1)

The XHTML 1.0 standard at L<http://www.w3.org/TR/xhtml1/>.

Current versions of this program are available from my web tools page at
L<https://www.eyrie.org/~eagle/software/web/>, as are copies of all of the
above-mentioned programs.

=head1 AUTHOR

Russ Allbery <eagle@eyrie.org>

=head1 COPYRIGHT AND LICENSE

Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
Russ Allbery <eagle@eyrie.org>.

This program is free software; you may redistribute it and/or modify it
under the same terms as Perl itself.

=cut
