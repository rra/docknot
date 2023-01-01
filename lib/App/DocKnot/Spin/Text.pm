# Convert some particular text formats into HTML.
#
# This program is an ad hoc set of heuristics and tricks, attempting to
# convert a few text file formats that I commonly use into reasonable HTML.
# General text to XHTML conversions is impossible due to the wildly differing
# formats used by people when writing text, so this module doesn't try to
# solve the general problem.  It's good enough to turn the FAQs I maintain
# into HTML documents, which is all that I need of it.
#
# SPDX-License-Identifier: MIT

##############################################################################
# Modules and declarations
##############################################################################

package App::DocKnot::Spin::Text 7.01;

use 5.024;
use autodie;
use warnings FATAL => 'utf8';

use vars qw($INDENT @INDENT %STATE $WS);

use App::DocKnot;
use App::DocKnot::Util qw(print_fh);
use Path::Tiny qw(path);
use POSIX qw(strftime);

# Replace with the month names you want to use, if you don't want English.
our @MONTHS = qw(January February March April May June July August September
                 October November December);

##############################################################################
# Utility functions
##############################################################################

# Turns section numbers at the beginning of lines in a paragraph into links.
sub contents {
    local $_ = shift;
    s%^(\s*([\d.]+)[.\)]\s+)(.*?)([ \t]*\n)%$1<a href="#S$2">$3</a>$4%gm;
    $_;
}

# Removes an initial bullet on a paragraph, replacing it with spaces.
sub debullet { local $_ = shift; s/(\s*)[-*o](\s)/$1 $2/; $_ }

# Unescape &, <, and > characters.
sub deescape { local $_ = shift; s/&gt;/>/g; s/&lt;/</g; s/&amp;/&/g; $_ }

# Removes an initial number on a paragraph, replacing it with spaces.
sub denumber {
    local $_ = shift;
    s/^(\s*)(\d\d?[.\)])(\s)/$1 . ' ' x length ($2) . $3/e;
    $_;
}

# Remove ASCII underlining from a section heading.
sub derule { local $_ = shift; s/^[-=~]+\n//m; $_ }

# Turns *some text* into <strong>some text</strong>, while trying to be
# careful to avoid other uses of wildcards.
sub embolden {
    local $_ = shift;
    s%(^|\s)\*(\w.*?\S)\*([,.!?;\s])%$1<strong>$2</strong>$3%gs;
    $_;
}

# Escapes &, <, and > characters found in a string.
sub escape { local $_ = shift; s/&/&amp;/g; s/</&lt;/g; s/>/&gt;/g; $_ }

# Returns the length of the indentation of a line or paragraph.
sub indent { $_[0] =~ /^(\s*)/; length $1 }

# Returns the number of lines in a paragraph, not counting trailing blanks.
sub lines { local $_ = shift; s/\s+$/\n/; tr/\n// }

# Returns a nicely formatted "Last modified" string from an RCS/CVS Id.
sub modified_id {
    my $id = shift;
    my ($version, $date) = (split (' ', $id))[2,3];
    my ($year, $month, $day) = split (m%[/-]%, $date);
    $day =~ s/^0//;
    my $revision = ($version =~ /\./) ? " (revision $version)" : '';
    'Last modified '. $MONTHS[$month - 1] . ' ' . $day . ', ' . $year
        . $revision;
}

# The same, but from a UNIX timestamp.
sub modified_timestamp {
    my $timestamp = shift;
    my ($year, $month, $day) = (localtime $timestamp)[5, 4, 3];
    $year += 1900;
    'Last modified ' . $MONTHS[$month] . ' ' . $day . ', ' . $year;
}

# Remove all whitespace in a string.
sub smash { local $_ = shift; s/\s//g; $_ }

# Strip a number of characters of indentation from a line that's given by the
# second argument, returning the result.  Used to strip leading indentation
# off of <pre> text so that it isn't indented excessively just because in the
# text version it had to be indented relative to the surrounding text.
sub strip_indent {
    local $_ = shift;
    my $indent = shift;
    if (defined $indent && $indent > 0) {
        s/^ {$indent}//gm;
    }
    $_;
}

# Undoes HTML character escapes.
sub unescape { local $_ = shift; s/&amp;/&/g; s/&lt;/</g; s/&gt;/>/g; $_ }

# Remove a constant prefix at the beginning of each line of a paragraph.
sub unquote {
    my ($string, $quote) = @_;
    $string =~ s/((?:^|\n)\s*)(\Q$quote\E\s+)/$1 . ' ' x length ($2)/ge;
    $string;
}

# Replace tabs with spaces.
sub untabify {
    local $_ = shift;
    1 while s/^(.*?)(\t+)/' ' x (length ($2) * 8 - length ($1) % 8)/me;
    $_;
}

# Given a special-character-escaped URL, wrap <a href></a> to that URL around
# it.  Remove a leading mailto: in the link text.
sub url {
    my $link = shift;
    my $text = $link;
    $link = smash (unescape $link);
    $text =~ s/^(?:mailto|news)://;
    '&lt;<a href="' . $link . '">' . $text . '</a>&gt;';
}

# Looks for a URL in <URL:...> form, with or without the URL: part, and wraps
# a link around it.
sub urlize {
    my $text = shift;
    $text =~ s%&lt;(?:URL:)?([a-z]{2,}:.+?)&gt;%url ($1)%ge;
    $text;
}

# Remove whitespace at the beginning and end of a string.
sub whitechomp { local $_ = shift; s/^\s+//; s/\s+$//; $_ }

##############################################################################
# Identification functions
##############################################################################

# Expects a paragraph, returns whether it is composed entirely of bullet
# items.  Take some care to avoid returning true for paragraphs that consist
# of a single bullet entry, since we want to handle those separately to wrap
# them in paragraph tags.
sub is_allbullet {
    local $_ = shift;
    my @lines = split ("\n", $_);
    return if not $lines[0] =~ /^(\s*[-*o]\s)\S/;
    my $bullet  = $1;
    my $space   = $bullet;
    $space =~ s/[-*o]/ /;
    my $bullets = 0;
    for (@lines) {
        next if !/\S/;
        return if !/^(?:\Q$bullet\E|\Q$space\E)\S/;
        $bullets++ if /^\Q$bullet\E/;
    }
    return $bullets > 1;
}

# Expects a paragraph, returns whether every line is a numbered item with a
# simple number.
sub is_allnumbered { $_[0] =~ /^(\s*\d\d?[.\)]\s.*\n){2,}\s*$/ }

# Expects a paragraph, returns whether it's in all capital letters.
sub is_allcaps { $_[0] !~ m%[^A-Z0-9\s\"\(\),:.!/?-]% }

# Expects a paragraph, returns whether it looks like it's broken into a series
# of short lines or a series of lines without internal space.  The last line
# of the paragraph doesn't matter for this determination.
sub is_broken {
    local $_ = shift;
    s/\s+$/\n/;
    my @lines = split ("\n", $_);
    return if @lines == 1;
    pop @lines;
    return 1 if grep { length ($_) < 40 } @lines;
    my $short = grep { length ($_) < 60 } @lines;
    ($short >= int (@lines / 2) + 1) || /^(?:\s*\S+[ \t]*\n)+$/;
}

# Expects a paragraph, returns whether it's a bulletted item.
sub is_bullet { $_[0] =~ /^\s*[-*o]\s/ }

# Expects a line, returns whether it's centered (in 74 columns).  Also require
# at least 10 spaces of whitespace so that we don't catch accidentally
# centered paragraph lines by mistake.
sub is_centered {
    $_[0] =~ /^(\s+)(.+)/
        && abs (74 - length ($2) - length ($1) * 2) < 2
        && length (untabify $1) >= 8;
}

# Expects a paragraph, returns whether it looks like a content listing.
sub is_contents { $_[0] =~ /^(?:\s*[\d.]+[.\)][ \t].*\n)+\s*$/ }

# Expects a paragraph, returns whether it looks like a title and description.
# Allow for multiple titles.
sub is_description {
    $_[0] =~ /^(\s*)\S.*\n(?:\1\S.*\n)*(\s+)\S.*\n(?:\2\S.*\n)*\s*$/
        && length ($1) < length ($2);
}

# Expects a paragraph, returns whether it's a digest divider.
sub is_divider { $_[0] =~ /^-{30}\s*$/ }

# Expects a line, returns whether it's a mail/news header.
sub is_header { $_[0] =~ /^[\w-]+:\s/ }

# Expects a paragraph, returns whether it's a heading.  This is all about
# heuristics and guesses, and there are a number of other things we could
# confuse for headings, so we have to be careful.  If it's a single line and
# outdented from the baseline, it's probably a heading.  If it's at the
# baseline, check to see if it looks like a heading and either it's in all
# caps or there is a rule underneath it.  If we haven't seen a baseline, be
# more accepting about headers.  If we're inside a contents block, be even
# more careful and disallow numbered things that look like a heading unless
# they're outdented.
sub is_heading {
    local $_ = deescape (shift);
    my $indent = indent $_;
    my $nobase = !defined $STATE{baseline};
    my $outdented = defined ($STATE{baseline}) && $indent < $STATE{baseline};
    return if (!$outdented && $STATE{contents} && /^[\d.]+[.\)]\s/);
    my $even = !defined ($INDENT) || $indent <= $INDENT;
    ($outdented && lines ($_) == 1 && (/\S\s\S/ || length ($_) < 30))
        || ($even && m%^\s*[ \w\"\(\),:./&-]{0,30}[\w\"\)]\s*\n[-=~]+\s*$%)
        || ($even && m%^\s*[ A-Z0-9\"\(\),:./&-]{0,30}[A-Z0-9\"\)]\s*\n$%)
        || ($even && $nobase && m%^\s*[ \w\"\(\),:./&-]{0,33}[\w\"\)]\s*\n$%);
}

# Expects a line, returns whether it's an RCS/CVS Id string that has been
# correctly expanded.
sub is_id { $_[0] =~ /^\s*\$Id\: .*\$\s*$/ }

# Expects a paragraph, returns whether it appears to have internal whitespace.
sub is_literal { $_[0] =~ /^[ \t]*\S.*(?:[^.?!\"\)\]:*_\n]  |   |\t)\S/m }

# Expects a paragraph, returns undef if it doesn't look like a numbered
# paragraph or the number if it does.
sub is_numbered { ($_[0] =~ /^\s*(\d\d?)[.\)]\s/) ? $1 : undef }

# Expects a paragraph, returns true if the paragraph has inconsistent
# indentation.
sub is_offset {
    local $_ = shift;

    # Strip off a leading bullet or number and consider it whitespace in
    # making this check.
    s/^(\s*(?:\d\d?)[.\)]\s)/' ' x length ($1)/e;
    s/^(\s*[-*o]\s)/' ' x length ($1)/e;

    # Now, return true if the indentation isn't consistent.
    !/^(\s*)\S.*\n(\1\S.*\n)*\s*$/
}

# Expects a paragraph, returns undef if not quoted or the quote character if
# it is quoted.  Requires that the paragraph be at least two lines.
sub is_quoted { $_[0] =~ /^\s*([^\w\s\"\'])\s*.*\n(\s*\1\s*.*\n)+$/ && $1 }

# Expects a paragraph, returns whether it's a rule.
sub is_rule { $_[0] =~ /^\s*[-=][-=\s]*$/ }

# Expects a paragraph, returns whether it ends with a sentence.  As a special
# case, a URL counts as a sentence so that we don't wrap <pre> around URLs.
sub is_url;
sub is_sentence {
    local $_ = shift;
    return 1 if /\S[.?!][\)\]\"]?\s*$/;
    return 1 if /^\s*\w.*\s\S+:\s*$/;
    return 1 if is_url $_;
    0;
}

# Expects a paragraph, returns whether it's the start of a signature block,
# defined to be a paragraph whose first line is exactly "-- ".
sub is_signature { $_[0] =~ /^-- \n/ }

# Expects a paragraph, returns whether it's a simple intented URL (already
# converted into a real link.
sub is_url { $_[0] =~ m%^\s*&lt;<a href.+>\S+</a>&gt;\s*$% }

##############################################################################
# Input and output
##############################################################################

# Output some text, adding any preserved whitespace after any closing tags.
#
# @data - HTML to output
sub _output {
    my ($self, @data) = @_;
    if ($WS) {
        $data[0] =~ s{ \A (\s* (?: </(?!body)[^>]+> \s*)* )}{$1$WS}xms;
        $WS = q{};
    }
    print_fh($self->{out_fh}, $self->{output}, @data);
    return;
}

# Read a paragraph, including all of its trailing blank lines.  By default,
# lines with nothing but whitespace are paragraph dividers.
#
# $in_fh         - Input file handle
# $require_blank - If true, only completely blank lines are dividers
sub _next_paragraph {
    my ($self, $in_fh, $require_blank) = @_;
    my $paragraph = $self->{buffer} // q{};
    my $nonblank_line = $require_blank ? qr{ [^\n] }xms : qr{ \S }xms;

    my $line;
    while (defined($line = <$in_fh>) && $line =~ $nonblank_line) {
        $paragraph .= $line;
    }
    if (defined($line)) {
        $paragraph .= $line;
    }
    while (defined($line = <$in_fh>) && $line =~ m{ \A \s* \z }xms) {
        $paragraph .= $line;
    }
    $self->{buffer} = $line;
    return $paragraph;
}

# Read from the input file descriptor, skipping blank lines.
#
# $in_fh - Input file handle
#
# Returns: Next non-blank line or undef on end of file
sub _skip_blank_lines {
    my ($self, $in_fh) = @_;
    my $line;
    do {
        $line = <$in_fh>;
    } while (defined($line) && $line !~ m{ \S }xms);
    return $line;
}

# Read from the input file descriptor, skipping blank lines and rules.
#
# $line  - Buffered line
# $in_fh - Input file handle
#
# Returns: Next non-blank, non-rule line or undef on end of file
sub _skip_blank_lines_and_rules {
    my ($self, $line, $in_fh) = @_;
    while (defined($line) && ($line !~ m{ \S }xms || is_rule($line))) {
        $line = <$in_fh>;
    }
    return $line;
}

##############################################################################
# HTML constructors
##############################################################################

# Output the header of the HTML document.  We claim "transitional" XHTML 1.0
# compliance; we can't claim strict solely because we use the value attribute
# in <li> in the absence of widespread implementation of CSS Level 2.  Assume
# English output.
#
# $header_ref - Additional information from the headers of the text document
sub _output_header {
    my ($self, $header_ref) = @_;
    $self->_output(
        '<?xml version="1.0" encoding="utf-8"?>', "\n",
        '<!DOCTYPE html', "\n",
        '    PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"', "\n",
        '    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">', "\n",
        "\n",
        '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">',
        "\n",
        '<head>', "\n",
        q{  }, title($self->{title} // $header_ref->{title} // q{}), "\n",
    );
    if ($self->{style}) {
        $self->_output(q{  }, style($self->{style}), "\n");
    }
    $self->_output(
        q{  },
        '<meta http-equiv="content-type" content="text/html; charset=utf-8"',
        " />\n",
    );
    if ($self->{sitemap}) {
        if (defined($self->{output}) && defined($self->{out_path})) {
            my $page = $self->{out_path}->relative($self->{output});
            $self->_output($self->{sitemap}->links($page));
        }
    }
    $self->_output("</head>\n\n");
    if ($header_ref->{id}) {
        $self->_output(comment($header_ref->{id}), "\n");
    }

    # Add a generator comment.
    my $date = strftime('%Y-%m-%d %T -0000', gmtime());
    my $version = $App::DocKnot::VERSION;
    $self->_output(comment("Converted by DocKnot $version on $date"), "\n\n");
}

# An XML comment.
sub comment {
    my @data = @_;
    my $data = join ('', @data);
    '<!-- ' . $data . ' -->';
}

# A link to a CSS style sheet.
sub style {
    my $style = shift;
    qq(<link rel="stylesheet" href="$style" type="text/css" />);
}

# Wrap a container around data, keeping the tags on the same line.
sub container {
    my ($tag, @data) = @_;
    my $data = join ('', @data);
    $data = '<' . $tag . '>' . $data;
    $tag =~ s/ .*//;
    $data =~ s%(\s*)$%</$tag>$1%;
    $data;
}

# Output a list item.  Takes the indentation, the item, and an optional third
# argument, which if specified is the number to use for the item (using the
# value attribute, which for some reason is deprecated under HTML 4.0 without
# any viable alternative for what I use it for).
sub li {
    my ($indent, $data, $value) = @_;
    $indent = 0 unless defined $indent;
    my $output = '';
    if (@INDENT && $INDENT[0][0] eq 'li') {
        $output .= "</li>\n";
        shift @INDENT;
    }
    unshift (@INDENT, [ 'li', $indent ]);
    my $tag = defined $value ? qq(<li value="$value">\n) : "<li>\n";
    $output . $tag . $data;
}

# Wrap a container around data, preserving trailing blank lines outside and
# putting the tags on lines of their own.
sub paragraph {
    my ($tag, @data) = @_;
    my $data = join ('', @data);
    $data .= "\n" unless ($data =~ /\n$/);
    '<' . $tag . ">\n" . $data . '</' . $tag . ">\n";
}

# Multiparagraph structure is maintained based on indentation level.  The
# global variable @INDENT holds a stack of pairs of block elements and their
# corresponding indentation levels.  The possible structure elements are dl,
# dd, ul, ol, li, and blockquote.
#
# This function is used to start or end block structure elements.  It closes
# any pending open structure elements with an indent level greater than the
# indentation level given, and then closes any open structure elements with an
# indentation level equal to the one given if a new structure element is given
# and it is different than the open one.  Then, if a structure element is
# given, open a new block structure element with that indentation.
#
# One can pass attributes in for the opening tag; anything after a space will
# be stripped out for determining the close tag.
sub start {
    my ($indent, $tag, $data) = @_;
    $indent = 0 unless defined $indent;
    my $e = $tag || '';
    $e =~ s/ .*//;
    $data = '' unless $data;
    my $output = '';
    while (@INDENT) {
        last if ($INDENT[0][1] < $indent);
        last if ($tag && $INDENT[0][1] == $indent && $INDENT[0][0] eq $tag);
        last if ($INDENT[0][1] == $indent && !$tag && $INDENT[0][0] ne 'dl');
        $output .= "</$INDENT[0][0]>\n";
        shift @INDENT;
    }
    return $output unless $tag;
    if (!@INDENT || $indent > $INDENT[0][1]) {
        $output .= "<$tag>\n";
        unshift (@INDENT, [ $tag, $indent ]);
    }
    $output . $data;
}

# Handle titles, which should have newlines turned into spaces and leading and
# trailing whitespace zapped.
sub title {
    local $_ = shift;
    s/\s*\n\s*/ /g;
    s/^\s+//;
    s/\s+$//;
    '<title>' . $_ . '</title>';
}

# Various containers.
sub blockquote { paragraph ('blockquote', @_) }
sub dt         { container ('dt',         @_) }
sub h1         { container ('h1',         @_) }
sub h2         { container ('h2',         @_) }
sub h3         { container ('h3',         @_) }
sub p          { paragraph ('p',          @_) }
sub pre        { container ('pre',        @_) }

##############################################################################
# Header parsing
##############################################################################

# Parse a block of RFC 2822 headers.
#
# $line  - Buffered input line
# $in_fh - Input file handle
#
# Returns: Hash of lower-cased header names to contents, or the empty hash if
#          no headers were seen
sub _parse_rfc2822_headers {
    my ($self, $line, $in_fh) = @_;
    my %header;

    while (defined($line) && $line =~ m{ \A ([\w-]+): \s+ (.*) }xms) {
        my ($header, $content) = ($1, $2);

        # Deal with continuation lines.
        $line = <$in_fh>;
        while (defined($line) && $line =~ m{ \A \s+ \S }xms) {
            $content .= $line;
            $line = <$in_fh>;
        }

        # Save the header contents.
        chomp($content);
        $header{lc($header)} = $content;
    }

    return \%header;
}

# Check to see if the header looks like that of a FAQ.  If so, parse it.
#
# $header_ref - Hash into which to store the parse results.
# $line       - Buffered input line
# $in_fh      - Input file handle
sub _handle_faq_headers {
    my ($self, $header_ref, $line, $in_fh) = @_;

    # Skip over a leading "From " line from an mbox file.
    if (defined($line) && $line =~ m{ \A From [ ] }xms) {
        $line = <$in_fh>;
    }

    # Parse the top-level headers, if any.
    my $top_ref = $self->_parse_rfc2822_headers($line, $in_fh);

    # Skip blank lines (either initial ones or ones after headers.
    $line = $self->_skip_blank_lines($in_fh);

    # Parse the FAQ subheaders, if any.
    my $sub_ref = $self->_parse_rfc2822_headers($line, $in_fh);

    # Store the information we care about from the headers.
    $header_ref->{author} = $top_ref->{from};
    $header_ref->{original} = $sub_ref->{'original-author'};
    $header_ref->{title} = $sub_ref->{'html-title'} // $top_ref->{subject};
    return;
}

# Parse the headers of a text document.  Sets the title, heading, id, author,
# and original (author) attributes in the object if the corresponding
# information was found.
#
# $in_fh - Input file handle
#
# Returns: Hash of data from headers with the following keys:
#            author   - Author of document
#            id       - RCS Id string
#            heading  - Main document heading
#            original - Original author of document
#            title    - Document title
sub _parse_headers {
    my ($self, $in_fh) = @_;
    my %header;

    # Check for a leading RCS/CVS version identifier.  For FAQs that I'm
    # posting to Usenet using postfaq, this will always be the first line of
    # the file stored on disk.
    my $line = <$in_fh>;
    if (is_id($line)) {
        chomp($line);
        $header{id} = $line;
        $line = $self->_skip_blank_lines($in_fh);
    }

    # Check for the type of document.  First, see if it looks like a FAQ with
    # news/mail headers, and if so read those headers and the subheaders.
    # Otherwise, skip over leading blank lines and rules.
    if (!$self->{title} && (is_header($line) || $line =~ m{ \A From }xms)) {
        $self->_handle_faq_headers(\%header, $line, $in_fh);
        $line = $self->_skip_blank_lines_and_rules(q{}, $in_fh);
    } else {
        $line = $self->_skip_blank_lines_and_rules($line, $in_fh);
    }

    # See if we have a centered title at the top of the document.  If so,
    # we'll make that the document title unless we also saw a Subject header
    # or a constructor argument.  Titles shouldn't be in all caps, though.
    if (is_centered($line)) {
        $header{heading} = whitechomp($line);
        if (!defined($header{title})) {
            $header{title} = $header{heading};
            if (is_allcaps($header{title})) {
                $header{title} =~ s{ \b ([A-Z]+) \b }{\L\u$1}xmsg;
            }
        }
        $line = $self->_skip_blank_lines_and_rules(q{}, $in_fh);
    } else {
        $header{heading} = $header{title} // $self->{title};
    }

    # Buffer the next line.
    $self->{buffer} = $line;
    return \%header;
}

##############################################################################
# Document conversion
##############################################################################

# Convert a document from text to HTML.
#
# $in_fh    - Input file handle
# $in_path  - Input path
# $out_fh   - Output file handle
# $out_path - Output path
sub _convert_document {
    my ($self, $in_fh, $in_path, $out_fh, $out_path) = @_;

    # Initialize object state for a new document.
    #<<<
    $self->{buffer}   = q{};        # Buffered input line not yet converted
    $self->{in_fh}    = $in_fh;     # Input file handle
    $self->{in_path}  = $in_path;   # Path to input file
    $self->{out_fh}   = $out_fh;    # Output file handle
    $self->{out_path} = $out_path;  # Path to the output file
    #>>>

    # Parse the document headers.
    my $header_ref = $self->_parse_headers($in_fh);

    # Generate the header of the HTML file.
    $self->_output_header($header_ref);

    # Open the body of the document, print the navigation links if possible,
    # and print out the heading if we found one.
    $self->_output("<body>\n\n");
    if ($self->{sitemap} && defined($self->{output}) && defined($out_path)) {
        my $page = $out_path->relative($self->{output});
        my @navbar = $self->{sitemap}->navbar($page);
        if (@navbar) {
            $self->_output(@navbar, "\n");
        }
    }
    if ($header_ref->{heading}) {
        $self->_output(h1($header_ref->{heading}), "\n");
    }

    # If we have additional headers, print them out.  Otherwise, if we have
    # author information from a From header, print that out under the main
    # heading.
    #
    # If we have RCS/CVS Id information, add another subheading containing the
    # last modified date.  Alternately, if the modified option was set, get
    # the last modified date from the source file.  Existing subheadings that
    # look like they're just Revision or Date strings are replaced by our more
    # nicely formatted string.
    #
    # We go to some length here to avoid unnecessary <br> tags.
    #
    # Note that </strong> has to be on the end of the last line rather than
    # the beginning of the next to work around a bug in lynx.
    if ($header_ref->{heading}) {
        my ($subheading, $modified);
        if ($header_ref->{id}) {
            $modified = modified_id($header_ref->{id});
        } elsif ($self->{modified} && $in_path ne '-') {
            my $timestamp = (stat $in_path)[9];
            if ($timestamp) {
                $modified = modified_timestamp ($timestamp);
            }
        }
        $_ = $self->{buffer};
        while (defined && (/^\s*$/ || is_centered ($_) || $subheading)) {
            if (/^\s*$/) {
                do { $_ = <$in_fh> } while (defined && is_rule $_);
                if (defined && is_centered ($_)) {
                    $self->_output("\n</p>\n") if $subheading;
                    $subheading = 0;
                    next;
                } else {
                    last;
                }
            } else {
                $self->_output(qq(<p class="subheading">\n))
                  unless $subheading;
                $self->_output("<br />\n") if $subheading;
                $subheading++;
                if ($modified && (/\$Revision/ || /\$Date/)) {
                    $self->_output(q{  }, $modified);
                    undef $modified;
                } else {
                    $_ = urlize (escape (whitechomp ($_)));
                    $self->_output(q{  }, $_);
                }
            }
            do { $_ = <$in_fh> } while (defined && is_rule $_);
        }
        $self->{buffer} = $_;
        if (!defined $subheading && $header_ref->{author}) {
            $subheading++;
            $self->_output(qq(<p class="subheading">\n));
            $self->_output(q{  }, escape($header_ref->{author}));
            if ($header_ref->{original}) {
                $self->_output("<br />\n  (originally by ");
                $self->_output(escape($header_ref->{original}), ')');
            }
        }
        if ($modified) {
            $self->_output(qq(<p class="subheading">\n)) unless $subheading;
            $self->_output("<br />\n") if $subheading;
            $self->_output(q{  }, $modified);
            $subheading++;
        }
        $self->_output("\n</p>\n") if $subheading;
        $self->_output("\n");
    }

    # Scan the actual body of the text.  We don't use paragraph mode, since it
    # doesn't work with blank lines that contain whitespace; instead, we
    # cobble together our own paragraph mode that does.  Note that $_ already
    # has a non-blank line of input coming into this loop.
    my $space;
    while (defined($self->{buffer})) {
        $_ = $self->_next_paragraph($in_fh);

        # Ignore any text after a signature block.
        last if (is_signature $_);

        # If we just hit a digest divider, the next thing will likely be a
        # Subject: line that we want to turn into a section header.  Digest
        # section titles are always level 2 headers currently.
        if (is_divider $_) {
            $STATE{pre} = 0;
            $self->_output(start(-1));
            undef $INDENT;
            ($WS) = /\n(\s*)$/;
            $_ = $self->_next_paragraph($in_fh);
            s/\n(\s*)$/\n/;
            $space = $1;
            if (s/^Subject:\s+//) {
                $STATE{contents} = /\bcontents\b/i;
                $_ = escape $_;
                if (/^([\d.]+)[.\)]\s/) {
                    $self->_output(
                        h2(container(qq(a name="S$1" id="S$1"), $_))
                    );
                } else {
                    $self->_output(h2($_));
                }
                next;
            }
        }

        # Treat lines of dash-type characters as rules.
        if (is_rule $_) {
            $STATE{pre} = 0;
            ($space) = /\n(\s*)$/;
            $self->_output(start(-1), "<hr />\n");
            undef $INDENT;
            next
        }

        # Everything else needs to have special characters escaped.  We don't
        # do this earlier because if we want to allow < and > in rules, the
        # escaping would make our lives miserable.
        $_ = escape $_;

        # Do this before untabification and stashing of trailing whitespace,
        # but after escaping.  Check to see if this paragraph looks like
        # literal text.  If so, we wrap it in <pre> and output it as is.  As a
        # special exception to our normal paragraph handling, this paragraph
        # doesn't end until we find a literal blank line; this hack lets full
        # diffs be included in a FAQ without confusing the parser.
        if (is_literal $_) {
            if (/\n[ \t]+$/) { $_ .= $self->_next_paragraph($in_fh, 1) }
            $self->_output(pre(strip_indent($_, $INDENT)));
            s/\n(\n\s*)$/\n/;
            $space = $1;
            $STATE{pre} = 1;
            next;
        }

        # Not literal text, so untabify it and stash whitespace.
        $_ = untabify $_;
        s/\n(\s*)$/\n/;
        $space = $1;
        my $indent = indent $_;

        # If the paragraph has inconsistent indentation, or is indented
        # relative to the baseline *and* the last paragraph we emitted was
        # enclosed in <pre>, assume that this paragraph belongs in <pre> as
        # well.
        if ($STATE{pre}) {
            if (is_offset ($_) || (defined $INDENT && $indent > $INDENT)) {
                $self->_output(pre(strip_indent($_, $INDENT)));
                next;
            } else {
                $STATE{pre} = 0;
            }
        }

        # Check for a heading.  We distinguish between level 2 headings and
        # level 3 headings as follows: The first heading we encounter is
        # assumed to be a level 2 heading, and any further headers at that
        # same indentation level are also level 2 headings.  If we detect any
        # other headings at a greater indent, they're marked as level 3.
        if (is_heading ($_)) {
            s/^\s+//;
            $STATE{contents} = /\bcontents\b/i;
            my $h;
            if (defined $STATE{h2}) {
                if ($indent <= $STATE{h2}) { $h = \&h2 }
                else                       { $h = \&h3 }
            } else {
                $STATE{h2} = $indent;
                $h = \&h2;
            }
            if (/^([\d.]+)[.\)]\s/) {
                my $anchor = qq(a name="S$1" id="S$1");
                $self->_output(start(), $h->(container($anchor, derule($_))));
            } else {
                $self->_output(start(), $h->(derule($_)));
            }
            $INDENT = $STATE{baseline};
            next;
        }

        # A sudden change to an indentation of 0 when that's less than our
        # indentation baseline is also a sign of literal text.
        if ($INDENT && $indent == 0 && $INDENT > 0 && defined($STATE{baseline})
            && $STATE{baseline} > 0) {
            $self->_output(pre(strip_indent($_, $INDENT)));
            $STATE{pre} = 1;
            next;
        }

        # We're dealing with a normal paragraph of some sort, so go ahead and
        # turn URLs into links.  Check whether the paragraph is broken first,
        # though, and stash that information, since turning URLs into links
        # can artificially lengthen lines.
        my $broken = is_broken $_;
        $_ = urlize $_;

        # Check to see if we're in a contents section, and if so if this
        # paragraph looks like a table of contents.  If so, turn all of the
        # section headings into links and assume broken text.
        if ($STATE{contents} && is_contents $_) { $_ = contents $_ }

        # Check for paragraphs that are entirely bulletted lines, and turn
        # them into unordered lists without <p> tags.
        if (is_allbullet $_) {
            my $last;
            my @lines = split (/\n/, $_);
            for (@lines) {
                next unless /\S/;
                if (is_bullet $_) {
                    if (defined $last) {
                        $self->_output(start($INDENT, 'ul'));
                        $self->_output(li($INDENT, embolden($last)));
                    }
                    $last = debullet $_;
                    $INDENT = indent $last;
                } else {
                    $last .= "\n$_";
                }
            }
            if (defined $last) {
                $self->_output(start($INDENT, 'ul'));
                $self->_output(li($INDENT, embolden($last)));
            }
            next;
        }

        # Check for paragraphs that are entirely numbered lines, and turn them
        # into ordered lists without <p> tags.
        if (is_allnumbered $_) {
            my @lines = split (/\n/, $_);
            for (@lines) {
                next unless /\S/;
                my ($number) = /^(\d+)/;
                $_ = denumber $_;
                $INDENT = indent $_;
                $self->_output(start($INDENT, 'ol'));
                $self->_output(li($INDENT, embolden($_), $number));
            }
            next;
        }

        # Check for bulletted paragraphs and turn them into lists.
        if (is_bullet $_) {
            $_ = debullet $_;
            $INDENT = indent $_;
            $self->_output(start($INDENT, 'ul'));
            $self->_output(li($INDENT, p(embolden $_)));
            next;
        }

        # Check for paragraphs quoted with some character and turn them into
        # blockquotes provided they don't have inconsistent indentation.
        my $quote = is_quoted ($_);
        if ($quote && !$broken) {
            $_ = unquote ($_, $quote);
            $INDENT = indent $_;
            $self->_output(start($INDENT, 'blockquote', p(embolden $_)));
            next;
        }

        # Check for numbered paragraphs and turn them into lists.
        my $number = is_numbered ($_);
        if (defined $number) {
            my $contents = is_contents ($_);
            $_ = denumber $_;
            $INDENT = indent $_;
            s%(\n\s*\S)%<br />$1%g if ($broken || $contents);
            $self->_output(start($INDENT, 'ol'));
            $self->_output(li($INDENT, p(embolden($_)), $number));
            next;
        }

        # Check for things that look like description lists and handle them.
        # Note that we don't allow indented description lists, because they're
        # usually something we actually want to make <pre>.  This is another
        # fairly fragile heuristic.
        if (is_description ($_) && defined $INDENT) {
            my (@title, $body);
            ($title[0], $body) = split ("\n", $_, 2);
            my ($space) = ($title[0] =~ /^(\s*)/);
            while ($body =~ /^$space\S/) {
                my $title;
                ($title, $body) = split ("\n", $body, 2);
                push (@title, $title);
            }
            if ($indent == $INDENT || indent ($body) == $INDENT) {
                @title = map { embolden ($_) } @title;
                my $title = join ("<br />\n", @title) . "\n";
                $INDENT = indent $body;
                $body =~ s%(\n\s*\S)%<br />$1%g if is_broken $body;
                $self->_output(start($indent, 'dl', dt($title)));
                $self->_output(start($INDENT, 'dd', p(embolden $body)));
                next;
            }
        }

        # If the paragraph has inconsistent indentation, we should output it
        # in <pre>.
        if (is_offset $_) {
            $self->_output(pre(strip_indent($_, $INDENT)));
            $STATE{pre} = 1;
            next;
        }

        # A sudden indentation change also means the paragraph should be
        # blockquoted.  We render broken blockquoted text in <pre>, which may
        # not be what's wanted for things like quotes of poetry... this is
        # probably worth looking at in more detail.
        if (defined $INDENT && $indent > $INDENT) {
            if ($broken || (lines ($_) == 1 && !is_sentence $_)) {
                $self->_output(pre(strip_indent($_, $INDENT)));
                $STATE{pre} = 1;
            } else {
                $INDENT = $indent;
                $self->_output(start($INDENT, 'blockquote', p(embolden($_))));
            }
            next;
        }

        # Close multiparagraph structure if we've outdented again.
        if ($INDENT && $indent < $INDENT) { $self->_output(start($indent)) }

        # Looks like a normal paragraph.  Establish our indentation baseline
        # if we haven't already.
        if (!defined $STATE{baseline} && !$INDENT) {
            $STATE{baseline} = $indent;
        }
        $INDENT = $indent;
        s%(\n\s*\S)%<br />$1%g if $broken;
        $self->_output(p(embolden($_)));

    } continue {
        $WS = $space;
    }

    # All done.  Print out our closing tags.
    $self->_output(start(-1));
    if ($self->{sitemap} && defined($self->{output}) && defined($out_path)) {
        my $page = $out_path->relative($self->{output});
        my @navbar = $self->{sitemap}->navbar($page);
        if (@navbar) {
            $self->_output("\n", @navbar);
        }
    }
    $self->_output("\n</body>\n</html>\n");
}

##############################################################################
# Public interface
##############################################################################

# Create a new text to HTML converter.
#
# $args_ref - Anonymous hash of arguments with the following keys:
#   output    - Root of the output tree (for sitemap information)
#   modified  - Whether to get last-modified date from source file
#   sitemap   - App::DocKnot::Spin::Sitemap object
#   style     - URL to the style sheet
#   title     - Document title
#
# Returns: Newly created object
sub new {
    my ($class, $args_ref) = @_;

    # Create and return the object.
    #<<<
    my $self = {
        output   => $args_ref->{output},
        modified => $args_ref->{modified},
        sitemap  => $args_ref->{sitemap},
        style    => $args_ref->{style},
        title    => $args_ref->{title},
    };
    #<<<
    bless($self, $class);
    return $self;
}

# Convert text to HTML.
#
# $input  - Input file (if not given, assumes standard input)
# $output - Output file (if not given, assumes standard output)
sub spin_text_file {
    my ($self, $input, $output) = @_;
    my ($in_fh, $out_fh);

    # Figure out what file we're going to be processing.  We can function as a
    # filter if so desired.
    if (defined($input)) {
        $input = path($input)->realpath();
        $in_fh = $input->openr_utf8();
    } else {
        open($in_fh, '<&:raw:encoding(utf-8)', 'STDIN');
    }

    # Open the output file.
    if (defined($output)) {
        $output = path($output)->absolute();
        $out_fh = $output->openw_utf8();
    } else {
        open($out_fh, '>&:raw:encoding(utf-8)', 'STDOUT');
    }

    # Do the work.
    $self->_convert_document($in_fh, $input, $out_fh, $output);

    # Close input and output.
    close($in_fh);
    close($out_fh);
}

##############################################################################
# Module return value and documentation
##############################################################################

1;
__END__

=for stopwords
Allbery DocKnot MERCHANTABILITY NONINFRINGEMENT sublicense outdenting RCS
documentable outdented subheaders preformatted XHTML

=head1 NAME

App::DocKnot::Spin::Text - Convert some particular text formats into HTML

=head1 SYNOPSIS

    use App::DocKnot::Spin::Text;

    my $text = App::DocKnot::Spin::Text->new({style => '/styles/faq.css'});
    $text->spin_text_file('/path/to/input', '/path/to/output.html');

=head1 REQUIREMENTS

Perl 5.24 or later and the modules List::SomeUtils, Path::Tiny, and
Sort::Versions, available from CPAN.

=head1 DESCRIPTION

This is another of those odd breed of partially functional beasts, a text to
HTML converter.

This is not truly possible in general; people do too many varied things with
their text to intuit document structure from it.  This is therefore a
converter that will translate documents written the way I write.  It may or
may not work for you.  The chances that it will work for you are directly
proportional to how much your writing looks like mine.

App::DocKnot::Spin::Text understands digest separators (lines of exactly
thirty hyphens, from the minimal digest standard) and will treat a C<Subject>
header immediately after them as a section header.  Beyond that, headings must
either be outdented, underlined on the following line, or in all caps to be
recognized as section headers.  (Outdenting means that the regular text is
indented by a few spaces, but headers start in column 0, or at least in a
column farther to the left than the regular text.)

Section headers that begin with numbers (with any number of periods) will be
given C<< <a id> >> tags containing that number prepended with C<S>.  As a
special case of the parsing, any section with a header containing C<contents>
will have lines beginning with numbers turned into links to the appropriate <a
id> tags in the same document.  You can use this to turn the table of contents
of your minimal digest format FAQ into a real table of contents with links in
the HTML version.

Text with embedded whitespace more than a single space or a couple of spaces
at a sentence boundary or after a colon (and any text with literal tabs) will
be wrapped in C<< <pre> >> tags.  So will any indented text that doesn't look
like English paragraphs.  URLs surrounded by C<< <...> >> or C<< <URL:...> >>
will be turned into links.  Other URLs will not be turned into links, nor is
any effort made to turn random body text into links because it happens to look
like a link.

Bullet lists and numbered lists will be turned into the appropriate HTML
structures.  Some attempt is also made to recognize description lists, but
App::DocKnot::Spin::Text was written by someone who writes a lot of technical
documentation and therefore tends to prefer C<< <pre> >> if unsure whether
something is a description list or preformatted text.  Description lists are
therefore only going to work if the description titles aren't indented
relative to the surrounding text.

Regular indented paragraphs or paragraphs quoted with a consistent
non-alphanumeric quote character are recognized and turned into HTML block
quotes.

It's worthwhile paying attention to the headers at the top of your document so
that App::DocKnot::Spin::Text can get a few things right.  If you use RCS or
CVS, put the RCS C<Id> keyword as the first line of your document; it will be
stripped out of the resulting output and App::DocKnot::Spin::Text will use it
to determine the document revision.  This should be followed by regular
message headers and news.answers subheaders if the document is an actual FAQ,
and App::DocKnot::Spin::Text will use the C<From> and C<Subject> headers to
figure out a title and headings to use.  As a special case, an HTML-title
header in the subheaders will override any other title that
App::DocKnot::Spin::Text thinks it should use for the document.

App::DocKnot::Spin::Text expects your document to have an C<< <h1> >> title,
and will add one from the Subject header if it doesn't find one.  It will also
add subheaders (C<class="subheading">) giving the author (from the C<From>
header) and the last modified time and revision (from the RCS C<Id> string) if
there are no subheadings already.  If there's a subheading that contains RCS
identifiers, it will be replaced by a nicely formatted heading generated from
the RCS C<Id> information in the HTML output.

Text marked as C<*bold*> using the standard asterisk notation will be
surrounded by C<< <strong> >> tags, if the asterisks appear to be marking bold
text rather than serving as wildcards or some other function.

App::DocKnot::Spin::Text produces output (at least in the absence of any
lurking bugs) which complies with the XHTML 1.0 Transitional standard.  The
input and output character set is assumed to be UTF-8.

=head1 CLASS METHODS

=over 4

=item new(ARGS)

Create a new App::DocKnot::Spin::Thread object.  A single converter object can
be reused repeatedly to convert multiple files, provided that they have the
same options.  ARGS should be a hash reference with one or more of the
following keys, all of which are optional:

=over 4

=item output

The path to the root of the output tree when converting a tree of files.  This
will be used to calculate relative path names for generating inter-page links
using the provided C<sitemap> argument.  If C<sitemap> is given, this option
should also always be given.

=item modified

Add a last modified subheader to the document.  This will always be done if an
RCS C<Id> string is present in the input.  Otherwise, a last modified
subheader based on the last modification date of the input file will be added
if the input is a file and this option is set to a true value.  The default is
false.

=item sitemap

An App::DocKnot::Spin::Sitemap object.  This will be used to create inter-page
links.  For inter-page links, the C<output> argument must also be provided.

=item style

The URL to the style sheet to use.  The appropriate HTML will be added to the
C<< <head> >> section of the resulting document.

=item title

The HTML page title to use.  This will also be used as the C<< <h1> >> heading
if the document doesn't contain one, but will not override a heading found in
the document (only the HTML C<< <title> >> attribute).

=back

=back

=head1 INSTANCE METHODS

=over 4

=item spin_text_file([INPUT[, OUTPUT]])

Convert a single text file to HTML.  INPUT is the path of the input file and
OUTPUT is the path of the output file.  OUTPUT or both INPUT and OUTPUT may be
omitted, in which case standard input or standard output, respectively, will
be used.

If OUTPUT is omitted, App::DocKnot::Spin::Text will not be able to obtain
sitemap information even if a sitemap was provided, and therefore will not add
inter-page links.

=back

=head1 NOTES

I wrote this program because every other text to HTML converter that I've seen
made specific assumptions about the document format and wanted you to write
like it wanted you to write rather than like the way you wanted to write.
This program instead wants you to write like I write, which from my
perspective is an improvement.

I don't claim that this is the be-all and end-all of text to HTML converters,
as I don't believe such a beast exists.  I do believe it's pretty close to
being the be-all and end-all of text to HTML converters for text that I
personally have written, since I've written into it a lot of knowledge of the
sorts of text formatting conventions that I use.  If you happen to use the
same ones, you may be delighted with this module.  If you don't, you'll
probably be very frustrated with it.

In any case, I took to this project the perspective that whenever there was
something this program couldn't handle, I wanted to make it smarter rather
than change the input.  I've mostly been successful at that, so far.

=head1 CAVEATS

This program attempts to intuit structure from an unstructured markup format.
It therefore relies on a whole bunch of fussy heuristics, poorly-understood
assumptions, and sheer blind luck.  To fully document the boundary cases of
this program would take more time and patience than I care to invest; see the
source code if you're curious.  This is not a predictable or easily
documentable program.  Instead, it attempts to do what I mean without bugging
me about it.

There is therefore, at least currently, no way to control or adjust parameters
in this program without editing it.  I may someday add that, but I'm leery of
it, since the code complexity would start increasing exponentially if I tried
to let people tweak everything.  I've given up on more than one text to HTML
converter because it had more options than B<ls> and expected you to try to
figure out which ones should be used for a document yourself.

English month names are used for the last modification dates, and the
resulting HTML always declares that the document is in English.  This could be
made configurable if anyone wishes.

=head1 AUTHOR

Russ Allbery <rra@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 1999-2002, 2004-2005, 2008, 2010, 2013, 2021-2022 Russ Allbery
<rra@cpan.org>

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

L<docknot(1)>, L<App::DocKnot::Spin>, L<App::DocKnot::Spin::Sitemap>

This module is part of the App-DocKnot distribution.  The current version of
DocKnot is available from CPAN, or directly from its web site at
L<https://www.eyrie.org/~eagle/software/docknot/>.

=cut

# Local Variables:
# copyright-at-end-flag: t
# End:
