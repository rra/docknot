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

use 5.024;
use autodie;
use warnings;

use App::DocKnot::Spin::Sitemap;
use App::DocKnot::Spin::Versions;
use Carp qw(croak);
use Cwd qw(getcwd realpath);
use Getopt::Long qw(GetOptions);
use Git::Repository ();
use Image::Size qw(html_imgsize);
use IPC::System::Simple qw(capture systemx);
use File::Basename qw(fileparse);
use File::Copy qw(copy);
use File::Find qw(find finddepth);
use File::Spec  ();
use Pod::Thread ();
use POSIX qw(strftime);
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

# The table of available commands.  The columns are:
#
# 1. Number of arguments or -1 to consume as many arguments as it can find.
# 2. Name of the method to call with the arguments and (if wanted) format.
# 3. Whether to look for a format in parens before the arguments.
my %COMMANDS = (
    # name       args  method             want_format
    block     => [1,  '_cmd_block',       1],
    bold      => [1,  '_cmd_bold',        1],
    break     => [0,  '_cmd_break',       0],
    bullet    => [1,  '_cmd_bullet',      1],
    class     => [1,  '_cmd_class',       1],
    cite      => [1,  '_cmd_cite',        1],
    code      => [1,  '_cmd_code',        1],
    desc      => [2,  '_cmd_desc',        1],
    div       => [1,  '_cmd_div',         1],
    emph      => [1,  '_cmd_emph',        1],
    entity    => [1,  '_cmd_entity',      0],
    heading   => [2,  '_cmd_heading',     0],
    h1        => [1,  '_cmd_h1',          1],
    h2        => [1,  '_cmd_h2',          1],
    h3        => [1,  '_cmd_h3',          1],
    h4        => [1,  '_cmd_h4',          1],
    h5        => [1,  '_cmd_h5',          1],
    h6        => [1,  '_cmd_h6',          1],
    id        => [1,  '_cmd_id',          0],
    image     => [2,  '_cmd_image',       1],
    include   => [1,  '_cmd_include',     0],
    italic    => [1,  '_cmd_italic',      1],
    link      => [2,  '_cmd_link',        1],
    number    => [1,  '_cmd_number',      1],
    pre       => [1,  '_cmd_pre',         1],
    quote     => [3,  '_cmd_quote',       1],
    release   => [1,  '_cmd_release',     0],
    rss       => [2,  '_cmd_rss',         0],
    rule      => [0,  '_cmd_rule',        0],
    signature => [0,  '_cmd_signature',   0],
    sitemap   => [0,  '_cmd_sitemap',     0],
    size      => [1,  '_cmd_size',        0],
    strike    => [1,  '_cmd_strike',      1],
    strong    => [1,  '_cmd_strong',      1],
    sub       => [1,  '_cmd_sub',         1],
    sup       => [1,  '_cmd_sup',         1],
    table     => [2,  '_cmd_table',       1],
    tablehead => [-1, '_cmd_tablehead',   1],
    tablerow  => [-1, '_cmd_tablerow',    1],
    under     => [1,  '_cmd_under',       1],
    version   => [1,  '_cmd_version',     0],
    q{=}      => [2,  '_define_variable', 0],
    q{==}     => [3,  '_define_macro',    0],
    q{\\}     => [0,  '_literal',         0],
);

##############################################################################
# Output
##############################################################################

# print with error checking.  autodie unfortunately can't help us because
# print can't be prototyped and hence can't be overridden.
sub _print_checked {
    my (@args) = @_;
    print @args or croak('print failed');
    return;
}

# print with error checking and an explicit file handle.  autodie
# unfortunately can't help us because print can't be prototyped and
# hence can't be overridden.
#
# $fh   - Output file handle
# $file - File name for error reporting
# @args - Remaining arguments to print
#
# Returns: undef
#  Throws: Text exception on output failure
sub _print_fh {
    my ($fh, $file, @args) = @_;
    print {$fh} @args or croak("cannot write to $file: $!");
    return;
}

# Sends something to the output file with special handling of whitespace for
# more readable HTML output.
#
# @output - Strings to output
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
        my ($prefix, $body) = $output =~ m{
            \A
            (\s*
             (?: </(?!body)[^>]+> \s* )*
            )
            (.*)
        }xms;
        $prefix .= $self->{space};

        # Collapse multiple whitespace-only lines into a single blank line.
        $prefix =~ s{ \n\s* \n\s* \n }{\n\n}xmsg;

        # Replace the output with added whitespace and clear saved whitespace.
        $output = $prefix . $body;
        $self->{space} = q{};
    }

    # Remove and save any trailing whitespace.
    if ($output =~ s{ \n (\s+) \z }{\n}xms) {
        $self->{space} = $1;
    }

    # Send the results to the output file.
    _print_fh($self->{out_fh}, $self->{out_path}, $output);
    return;
}

# Report a fatal problem with the current file and line.
#
# $problem - Error message to report
#
# Throws: Text exception with the provided error
sub _fatal {
    my ($self, $problem) = @_;
    die "$self->{file}:$.: $problem\n";
}

# Warn about a problem with the current file and line.
#
# $problem - Warning message to report
sub _warning {
    my ($self, $problem) = @_;
    warn "$0 spin:$self->{file}:$.: $problem\n";
    return;
}

##############################################################################
# Basic parsing
##############################################################################

# Escapes &, <, and > characters for HTML output.
#
# $string - Input string
#
# Returns: Escaped string
sub _escape {
    my ($string) = @_;
    $string =~ s{ & }{&amp;}xmsg;
    $string =~ s{ < }{&lt;}xmsg;
    $string =~ s{ > }{&gt;}xmsg;
    return $string;
}

# Wrap something in paragraph markers, being careful to get newlines right.
# Special-case a paragraph consisting entirely of <span> by turning it into a
# <p> with the same class.
#
# $text - Text to wrap
#
# Returns: Text wrapped in <p> tags
sub _paragraph {
    my ($self, $text) = @_;

    # Trim leading newline and whitespace and ensure the paragraph ends with a
    # newline.
    $text =~ s{ \A \n (\s*\n)* }{}xms;
    $text =~ s{ ( \S [ \t]* ) \z }{$1\n}xms;

    # If the whole paragraph is wrapped in <span>, lift its attributes into
    # the <p> tag.  Otherwise, just add the <p> tags.
    if ($text =~ m{ \A (\s*) <span ([^>]*) > (.*) </span> (\s*) \z }xms) {
        my ($lead, $attrs, $body, $trail) = ($1, $2, $3, $4);
        return "$lead<p$attrs>$body</p>$trail";
    } else {
        $text =~ s{ \A }{<p>\n}xms;
        $text =~ s{ (\n\s*) \z }{\n</p>$1}xms;
        return $text;
    }
}

# Opens the border of a continued structure.
#
# spin, unlike HTML, does not require declaring structures like lists in
# advance of adding elements to them.  You start a bullet list by simply
# having a bullet item, and a list is started if one is not already open.
# This is the method that does that: check whether the desired structure is
# already open and, if not, open it and add it to the state stack.
#
# $border - Name of the border state to open
# $start  - The opening tag
# $end    - The closing tag
#
# Returns: Output to write to start the structure
sub _border_start {
    my ($self, $border, $start, $end) = @_;
    my $state  = $self->{state}[-1];
    my $output = q{};

    # If we're at the top-level block structure or inside a structure other
    # than ours, open the structure and add it to the state stack.
    if ($state eq 'BLOCK' || $state->[0] ne $border) {
        $output .= $start;
        push($self->{state}->@*, [$border, $end]);
    }

    return $output;
}

# Closes the border of any currently-open continued structure.  This is done,
# for example, when a new block structure is opened or a paragraph of regular
# text is seen at the same level as the structure elements.
#
# Returns: Output to write to close the structure.
sub _border_end {
    my ($self) = @_;
    my $output = q{};

    # Find all open structures up to the first general block structure.  We'll
    # pop off the block structure so put it back when we're done.
    while (defined(my $state = pop($self->{state}->@*))) {
        last if $state eq 'BLOCK';
        $output .= $state->[1];
    }
    push($self->{state}->@*, 'BLOCK');

    return $output;
}

# Marks the beginning of major block structure.  Within this structure,
# borders will only clear to the level of this structure.
sub _block_start {
    my ($self) = @_;
    push($self->{state}->@*, 'BLOCK');
    return;
}

# Clears a major block structure.
sub _block_end {
    my ($self) = @_;
    my $output = $self->_border_end();
    pop($self->{state}->@*);
    return $output;
}

# Extract some number of arguments from the front of the given string.
#
# $text        - Text to parse arguments from
# $count       - How many arguments to extract, or -1 for as many as possible
# $want_format - If true, check for a parenthesized formatting instruction
#                first and extract it if present
#
# Returns: List of the following strings:
#            $format - Format or empty string, omitted if !$want_format
#            $text   - The remaining unparsed text
#            @args   - $count arguments (undef if the argument wasn't found)
sub _extract {
    my ($self, $text, $count, $want_format) = @_;
    my $format = q{};
    my @args;

    # Extract the format string if requested.
    if ($want_format) {
        $format = extract_bracketed($text, '()') // q{};
        if ($format) {
            $format = substr($format, 1, -1);
        }
    }

    # Extract the desired number of arguments, or all arguments present if
    # $count was negative.
    if ($count >= 0) {
        for my $i (1 .. $count) {
            my $arg = extract_bracketed($text, '[]');
            if (defined($arg)) {
                $arg = substr($arg, 1, -1);
            } else {
                $self->_warning("cannot find argument $i: $@");
                $arg = q{};
            }
            push(@args, $arg);
        }
    } else {
        while (defined(my $arg = extract_bracketed($text, '[]'))) {
            push(@args, substr($arg, 1, -1));
        }
    }

    # Return the results.
    return $want_format ? ($format, $text, @args) : ($text, @args);
}

# Expand a macro invocation.
#
# $definition - Definition of the macro
# $block      - True if currently in block context
# @args       - The arguments to the macro
#
# Returns: List with the macro expansion and the block context flag
sub _macro {
    my ($self, $definition, $block, @args) = @_;

    # The function that expands a macro substitution marker.  If the number of
    # the marker is higher than the number of arguments of the macro, leave it
    # as-is.  (We will have already warned about this when defining the
    # macro.)
    my $expand = sub {
        my ($n) = @_;
        return ($n > scalar(@args)) ? "\\$n" : $args[$n - 1];
    };

    # Replace the substitution markers in the macro definition.
    $definition =~ s{ \\(\d+) }{ $expand->($1) }xmsge;

    # Now parse the result as if it were input thread and return the results.
    return $self->_parse_context($definition, $block);
}

# Expand a given command into its representation.  This function is mutually
# recursive with _parse_context and _macro.
#
# $command - Name of the command
# $text    - Input text following the command
# $block   - True if currently in block context (if so, and if the command
#            doesn't generate its own container, it will need to be wrapped
#            in <p>
#
# Returns: List with the following elements:
#            $output - Output from expanding the command
#            $block  - Whether the output is block context
#            $text   - Remaining unparsed text
sub _expand {
    my ($self, $command, $text, $block) = @_;

    # Special handling for expanding variables.  These references look like
    # \=NAME and expand to the value of the variable "NAME".
    if ($command =~ m{ \A = \w }xms) {
        my $variable = substr($command, 1);
        if (exists($self->{variable}{$variable})) {
            return ($self->{variable}{$variable}, 0, $text);
        } else {
            $self->_warning("unknown variable $variable");
            return (q{}, 0, $text);
        }
    }

    # Special handling for macros.  Macros shadow commands of the same name.
    if (exists($self->{macro}{$command})) {
        my ($args, $definition) = $self->{macro}{$command}->@*;

        # Extract the macro arguments, if any were requested.
        my @args;
        if ($args != 0) {
            ($text, @args) = $self->_extract($text, $args, 0);
        }

        # The macro runs in a block context if we're currently in block
        # context and there is no remaining non-whitespace text.  Otherwise,
        # use an inline context.
        $block &&= $text =~ m{ \A \s* \z }xms;

        # Return the macro expansion.
        return ($self->_macro($definition, $block, @args), $text);
    }

    # The normal command-handling case.  Ensure it is a valid command.
    if (!ref($COMMANDS{$command})) {
        $self->_warning("unknown command or macro $command");
        return (q{}, 1, $text);
    }

    # Dispatch the command to its handler.
    my ($args, $handler, $want_format) = $COMMANDS{$command}->@*;
    if ($want_format) {
        my ($format, $rest, @args) = $self->_extract($text, $args, 1);
        my ($blocktag, $output) = $self->$handler($format, @args);
        return ($output, $blocktag, $rest);
    } else {
        my ($rest,     @args)   = $self->_extract($text, $args);
        my ($blocktag, $output) = $self->$handler(@args);
        return ($output, $blocktag, $rest);
    }
}

# This is the heart of the input parser.  Take a string of raw input, expand
# the commands in it, and format the results as HTML.  This function is
# mutually recursive with _expand and _macro.
#
# $text  - Input text to parse
# $block - True if the parse is done in a block context
#
# Returns: List of the following values:
#            $output - HTML output corresponding to $text
#            $block  - Whether the result is suitable for block level
#
## no critic (Subroutines::ProhibitExcessComplexity)
sub _parse_context {
    my ($self, $text, $block) = @_;

    # Check if there are any commands in the input.  If not, we have a
    # paragraph of regular text.
    if (index($text, q{\\}) == -1) {
        my $output = $text;

        # If we are at block context, we need to make the text into a block
        # element, which means wrapping it in <p> tags.  Since that is a
        # top-level block construct, also close any open block structure.
        if ($block) {
            $output = $self->_border_end() . $self->_paragraph($output);
        }

        # Return the result.
        return ($output, $block);
    }

    # The output seen so far.
    my $output = q{};

    # Output required to close any open block-level constructs that we saw
    # prior to the text we're currently parsing.
    my $border = q{};

    # Output with inline context that needs to be wrapped in <p> tags.
    my $paragraph = q{};

    # Leading whitespace that should be added to a created paragraph.  This is
    # only non-empty if $paragraph is empty.
    my $space = q{};

    # Whether we saw a construct not suitable for block level.
    my $nonblock = 0;

    # We have at least one command.  Parse the text into sections of regular
    # text and commands, expand the commands, and glue the results together as
    # HTML.
    #
    # If we are at block level, we have to distinguish between plain text and
    # inline commands, which have to be wrapped in paragraph tags, and
    # block-level commands, which shouldn't be.
    while ($text ne q{}) {
        my ($string, $command);

        # Extract text before the next command, or a command name (but none of
        # its arguments).  I think it's impossible for this regex to fail to
        # match as long as $text is non-empty, but do error handling just in
        # case.
        if ($text =~ s{ \A ( [^\\]+ | \\ ([\w=]+ | .) ) }{}xms) {
            ($string, $command) = ($1, $2);
        } else {
            my $context = substr($text, 0, 20);
            $context =~ s{ \n .* }{}xms;
            $self->_fatal(qq(unable to parse near "$context"));
        }

        # If this is not a command, and we're not at the block level, just add
        # it verbatim to the output.
        #
        # if we are at the block level, pull off any leading space.  If there
        # is still remaining text, add it plus any accumulated whitespace to a
        # new paragraph.
        if (index($string, q{\\}) == -1) {
            if ($block) {
                if ($string =~ s{ \A (\s+) }{}xms) {
                    $space .= $1;
                }
                if ($paragraph ne q{} || $string ne q{}) {
                    if ($paragraph eq q{}) {
                        $border = $self->_border_end();
                    }
                    $paragraph .= $space . $string;
                    $space = q{};
                }
            } else {
                $output .= $string;
                $nonblock = 1;
            }
        }

        # Otherwise, we have a command.  Expand that command, setting block
        # context if we haven't seen any inline content so far.
        else {
            my ($result, $blocktag);
            ($result, $blocktag, $text)
              = $self->_expand($command, $text, $block && $paragraph eq q{});

            # If the result requires block context, output any pending
            # paragraph and then the result.  Otherwise, if we are already at
            # block context, start a new paragraph.  Otherwise, just append
            # the result to our output.
            if ($blocktag) {
                if ($block && $paragraph ne q{}) {
                    $output .= $border . $self->_paragraph($paragraph);
                    $border    = q{};
                    $paragraph = q{};
                } else {
                    $output .= $space;
                }
                $output .= $result;
            } elsif ($block) {
                if ($paragraph eq q{}) {
                    $border = $self->_border_end();
                }
                $paragraph .= $space . $result;
                $nonblock = 1;
            } else {
                $output .= $result;
                $nonblock = 1;
            }
            $space = q{};
        }

        # If the next bit of unparsed text starts with a newline, extract it
        # and any following whitespace now.  Add it to our paragraph if we're
        # accumulating one; otherwise, add it to the output, but only add the
        # newline if we saw inline elements or there is remaining text.  This
        # suppresses some useless black lines.
        if ($text =~ s{ \A \n (\s*) }{}xms) {
            if ($paragraph ne q{}) {
                $paragraph .= "\n$1";
            } else {
                if ($text ne q{} || $nonblock) {
                    $output .= "\n";
                }
                $output .= $1;
            }
        }
    }

    # If there is any remaining paragraph text, wrap it in tags and append it
    # to the output.  If we were at block level, our output is always suitable
    # for block level.  Otherwise, it's suitable for block level only if all
    # of our output was from block commands.
    if ($paragraph ne q{}) {
        $output .= $border . $self->_paragraph($paragraph);
    }
    return ($output, $block || !$nonblock);
}
## use critic

# A wrapper around parse_context for callers who don't care about the block
# level of the results.
#
# $text  - Input text to parse
# $block - True if the parse is done in a block context
#
# Returns: HTML output corresponding to $text
sub _parse {
    my ($self, $text, $block) = @_;
    my ($output) = $self->_parse_context($text, $block);
    return $output;
}

##############################################################################
# Supporting functions
##############################################################################

# Generate the format attributes for an HTML tag.
#
# $format - Format argument to the command
#
# Returns: String suitable for interpolating into the tag, which means it
#          starts with a space if non-empty
sub _format_attr {
    my ($self, $format) = @_;
    return q{} if !$format;

    # Formats starting with # become id tags.  Otherwise, it is a class.
    if ($format =~ s{ \A \# }{}xms) {
        if ($format =~ m{ \s }xms) {
            $self->_warning(qq(space in anchor "#$format"));
        }
        return qq{ id="$format"};
    } else {
        return qq{ class="$format"};
    }
}

# Split a block of text apart at paired newlines so that it can be reparsed as
# paragraphs, but combine a paragraph with the next one if it has an
# unbalanced number of open brackets.  Used by containiners like \block that
# can contain multiple paragraphs.
#
# $text - Text to split
#
# Returns: List of paragraphs
sub _split_paragraphs {
    my ($self, $text) = @_;
    my @paragraphs;

    # Collapse any consecutive newlines at the start to a single newline.
    $text =~ s{ \A \n (\s*\n)+ }{\n}xms;

    # Pull paragraphs off the text one by one.
    while ($text ne q{} && $text =~ s{ \A ( .*? (?: \n\n+ | \s*\z ) )}{}xms) {
        my $para        = $1;
        my $open_count  = ($para =~ tr{\[}{});
        my $close_count = ($para =~ tr{\]}{});
        while ($text ne q{} && $open_count > $close_count) {
            if ($text =~ s{ \A ( .*? (?: \n\n+ | \s*\z ) )}{}xms) {
                my $extra = $1;
                $open_count  += ($extra =~ tr{\[}{});
                $close_count += ($extra =~ tr{\]}{});
                $para .= $extra;
            } else {
                # This should be impossible.
                break;
            }
        }
        push(@paragraphs, $para);
    }

    # Return the paragraphs.
    return @paragraphs;
}

# A simple block element.  Handles splitting the argument on paragraph
# boundaries and surrounding things properly with the tag.
#
# $tag    - Name of the tag
# $border - Initial string to output before the block
# $format - Format string for the block
# $text   - Contents of the block
#
# Returns: Block context, output
sub _block {
    my ($self, $tag, $border, $format, $text) = @_;
    my $output = $border . "<$tag" . $self->_format_attr($format) . '>';
    $self->_block_start();

    # If the format is packed, the contents of the block should be treated as
    # inline rather than block and not surrounded by <p>.  This is how compact
    # bullet or number lists are done.  Otherwise, parse each containing
    # paragraph separately in block context.
    if ($format eq 'packed') {
        $output .= $self->_parse($text, 0);
    } else {
        my @paragraphs = $self->_split_paragraphs($text);
        $output .= join(q{}, map { $self->_parse($_, 1) } @paragraphs);
    }
    $output .= $self->_block_end();

    # Close the tag.  The tag may have contained attributes, which aren't
    # allowed in the closing tag.
    $tag    =~ s{ [ ] .* }{}xms;
    $output =~ s{ \s* \z }{</$tag>}xms;
    if ($format ne 'packed') {
        $output .= "\n";
    }

    return (1, $output);
}

# A simple inline element.
#
# $tag    - Name of the tag
# $format - Format string
# $text   - Contents of the element
#
# Returns: Inline context, output
sub _inline {
    my ($self, $tag, $format, $text) = @_;
    my $output = "<$tag" . $self->_format_attr($format) . '>';
    $output .= $self->_parse($text) . "</$tag>";
    return (0, $output);
}

# A heading.  Handles formats of #something specially by adding an <a name>
# tag inside the heading tag to make it a valid target for internal links even
# in old browsers.
#
# $level  - Level of the heading
# $format - Format string
# $text   - Content of the heading
#
# Returns: Block context, output
sub _heading {
    my ($self, $level, $format, $text) = @_;
    my $output = $self->_border_end();
    $text = $self->_parse($text);

    # Special handling for anchors in the format string.
    if ($format =~ m{ \A \# }xms) {
        my $tag = $format;
        $tag =~ s{ \A \# }{}xms;
        $text = qq{<a name="$tag">$text</a>};
    }

    # Build the output.
    $output .= "<h$level" . $self->_format_attr($format) . '>' . $text;
    $output =~ s{ \n \z }{}xms;
    $output .= "</h$level>\n";
    return (1, $output);
}

# Enclose some text in another tag.  If the enclosed text is entirely enclosed
# in <span> or <div> tags, we pull the options of the <span> or <div> out and
# instead apply them to the parent tag.
#
# $tag  - Name of tag
# $text - Text to enclose
sub _enclose {
    my ($self, $tag, $text) = @_;

    # Strip any attributes from the tag.
    my $close_tag = $tag;
    $close_tag =~ s{ [ ] .*}{}xms;

    # Handle <div> and <span> wrapping.
    if ($text =~ m{ \A (\s*) <span([^>]*)> (.*) </span> (\s*) \z}xms) {
        my ($lead, $class, $body, $trail) = ($1, $2, $3, $4);
        return "$lead<$tag$class>$body</$close_tag>$trail";
    } elsif ($text =~ m{ \A (\s*) <div([^>]*)> (.*) </div> (\s*) \z}xms) {
        my ($lead, $class, $body, $trail) = ($1, $2, $3, $4);
        return "$lead<$tag$class>$body</$close_tag>$trail";
    } else {
        return "<$tag>$text</$close_tag>";
    }
}

# Build te page footer, which consists of the navigation links, the regular
# signature, and the last modified date.
#
# $source    - Full path to the source file
# $out_path  - Full path to the output file
# $id        - CVS Id of the source file or undef if not known
# @templates - Two templates to use.  The first will be used if the
#              modification and current dates are the same, and the second
#              if they are different.  %MOD% and %NOW% will be replaced with
#              the appropriate dates and %URL% with the URL to the site
#              generation software.
#
# Returns: HTML output
sub _footer {
    my ($self, $source, $out_path, $id, @templates) = @_;
    my $output  = q{};
    my $in_tree = 0;
    if ($self->{source} && $source =~ m{ \A \Q$self->{source}\E }xms) {
        $in_tree = 1;
    }

    # Add the end-of-page navbar if we have sitemap information.
    if ($self->{sitemap} && $self->{output}) {
        my $page = $out_path;
        $page =~ s{ \A \Q$self->{output}\E }{}xms;
        $output .= join(q{}, $self->{sitemap}->navbar($page));
    }

    # Figure out the modification dates.  Use the RCS/CVS Id if available,
    # otherwise use the Git repository if available.
    my $modified;
    if (defined($id)) {
        my (undef, undef, $date) = split(q{ }, $id);
        if ($date && $date =~ m{ \A (\d+) [-/] (\d+) [-/] (\d+) }xms) {
            $modified = sprintf('%d-%02d-%02d', $1, $2, $3);
        }
    } elsif ($self->{repository} && $in_tree) {
        $modified
          = $self->{repository}->run('log', '-1', '--format=%ct', $source);
        if ($modified) {
            $modified = strftime('%Y-%m-%d', gmtime($modified));
        }
    }
    if (!$modified) {
        $modified = strftime('%Y-%m-%d', gmtime((stat $source)[9]));
    }
    my $now = strftime('%Y-%m-%d', gmtime());

    # Determine which template to use and substitute in the appropriate times.
    $output .= "<address>\n" . q{ } x 4;
    my $template = ($modified eq $now) ? $templates[0] : $templates[1];
    $template =~ s{ %MOD% }{$modified}xmsg;
    $template =~ s{ %NOW% }{$now}xmsg;
    $template =~ s{ %URL% }{$URL}xmsg;
    $output .= "$template\n";
    $output .= "</address>\n";

    return $output;
}

##############################################################################
# Special commands
##############################################################################

# These methods are all used, but are indirected through the above table, so
# perlcritic gets confused.
#
## no critic (Subroutines::ProhibitUnusedPrivateSubroutines)

# Define a new macro.  This is the command handler for \==.
#
# $name       - Name of the macro
# $args       - Number of arguments
# $definition - Definition of the macro
#
# Returns: Block context, empty output
sub _define_macro {
    my ($self, $name, $args, $definition) = @_;
    $args = $self->_parse($args);

    # Verify the argument count and definition.
    if ($args !~ m{ \A \d+ \z }xms) {
        $self->_warning("invalid macro argument count for $name");
    }
    for my $arg ($definition =~ m{ \\(\d+) }xmsg) {
        if ($1 > $args) {
            my $msg = "invalid macro placeholder \\$1 (greater than $args)";
            $self->_warning($msg);
        }
    }

    # Define the macro.
    $self->{macro}{$name} = [$self->_parse($args), $definition];
    return (1, q{});
}

# Define a new variable.  This is the command handler for \=.
#
# $name  - Name of the variable
# $value - Value of the variable
#
# Returns: Block context, empty output
sub _define_variable {
    my ($self, $name, $value) = @_;
    $self->{variable}{$name} = $self->_parse($value);
    return (1, q{});
}

# Literal backslash.  This is the command handler for \\.
sub _literal { return (0, q{\\}) }

##############################################################################
# Regular commands
##############################################################################

# Basic inline commands.
sub _cmd_break  { return (0, '<br />') }
sub _cmd_bold   { my ($self, @a) = @_; return $self->_inline('b',      @a) }
sub _cmd_cite   { my ($self, @a) = @_; return $self->_inline('cite',   @a) }
sub _cmd_class  { my ($self, @a) = @_; return $self->_inline('span',   @a) }
sub _cmd_code   { my ($self, @a) = @_; return $self->_inline('code',   @a) }
sub _cmd_emph   { my ($self, @a) = @_; return $self->_inline('em',     @a) }
sub _cmd_italic { my ($self, @a) = @_; return $self->_inline('i',      @a) }
sub _cmd_strike { my ($self, @a) = @_; return $self->_inline('strike', @a) }
sub _cmd_strong { my ($self, @a) = @_; return $self->_inline('strong', @a) }
sub _cmd_sub    { my ($self, @a) = @_; return $self->_inline('sub',    @a) }
sub _cmd_sup    { my ($self, @a) = @_; return $self->_inline('sup',    @a) }
sub _cmd_under  { my ($self, @a) = @_; return $self->_inline('u',      @a) }

# The headings.
sub _cmd_h1 { my ($self, @a) = @_; return $self->_heading(1, @a); }
sub _cmd_h2 { my ($self, @a) = @_; return $self->_heading(2, @a); }
sub _cmd_h3 { my ($self, @a) = @_; return $self->_heading(3, @a); }
sub _cmd_h4 { my ($self, @a) = @_; return $self->_heading(4, @a); }
sub _cmd_h5 { my ($self, @a) = @_; return $self->_heading(5, @a); }
sub _cmd_h6 { my ($self, @a) = @_; return $self->_heading(6, @a); }

# A horizontal rule.
sub _cmd_rule {
    my ($self) = @_;
    return (1, $self->_border_end() . "<hr />\n");
}

# Simple block commands.

sub _cmd_div {
    my ($self, $format, $text) = @_;
    return $self->_block('div', q{}, $format, $text);
}

sub _cmd_block {
    my ($self, $format, $text) = @_;
    return $self->_block('blockquote', q{}, $format, $text);
}

sub _cmd_bullet {
    my ($self, $format, $text) = @_;
    my $border = $self->_border_start('bullet', "<ul>\n", "</ul>\n\n");
    return $self->_block('li', $border, $format, $text);
}

sub _cmd_number {
    my ($self, $format, $text) = @_;
    my $border = $self->_border_start('number', "<ol>\n", "</ol>\n\n");
    return $self->_block('li', $border, $format, $text);
}

# A description list entry.
#
# $format  - Format string
# $heading - Initial heading
# $text    - Body text
sub _cmd_desc {
    my ($self, $format, $heading, $text) = @_;
    $heading = $self->_parse($heading);
    my $format_attr = $self->_format_attr($format);
    my $border      = $self->_border_start('desc', "<dl>\n", "</dl>\n\n");
    my $initial     = $border . "<dt$format_attr>" . $heading . "</dt>\n";
    return $self->_block('dd', $initial, $format, $text);
}

# An HTML entity.  Check for and handle numeric entities properly, including
# special-casing [ and ] since the user may have needed to use \entity to
# express text that contains literal brackets.
#
# $entity - Entity specification, an HTML name or a Unicode number
sub _cmd_entity {
    my ($self, $char) = @_;
    $char = $self->_parse($char);
    if ($char eq '91') {
        return (0, '[');
    } elsif ($char eq '93') {
        return (0, ']');
    } elsif ($char =~ m{ \A \d+ \z }xms) {
        return (0, q{&\#} . $char . q{;});
    } else {
        return (0, q{&} . $char . q{;});
    }
}

# Generates the page heading at the top of the document.  This is where the
# XHTML declarations come from.
#
# $title - Page title
# $style - Page style
sub _cmd_heading {
    my ($self, $title, $style) = @_;
    $title = $self->_parse($title);
    $style = $self->_parse($style);

    # Get the relative URL of the output page, used for sitemap information.
    my $page = $self->{out_path};
    if ($self->{output}) {
        $page =~ s{ \A \Q$self->{output}\E }{}xms;
    }

    # Build the page header.
    my $output = qq{<?xml version="1.0" encoding="utf-8"?>\n};
    $output .= qq{<!DOCTYPE html\n};
    $output .= qq{    PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"\n};
    $output .= qq{    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n};
    $output .= qq{\n<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"};
    $output .= qq{ lang="en">\n};
    $output .= qq{<head>\n  <title>$title</title>\n};
    $output .= q{  <meta http-equiv="Content-Type"};
    $output .= qq{ content="text/html; charset=utf-8" />\n};

    # Add style sheet.
    if ($style) {
        $style .= '.css';
        if ($self->{style_url}) {
            $style = $self->{style_url} . $style;
        }
        $output .= qq{  <link rel="stylesheet" href="$style"};
        $output .= qq{ type="text/css" />\n};
    }

    # Add RSS links if any.
    for my $rss ($self->{rss}->@*) {
        my ($url, $rss_title) = $rss->@*;
        $output .= q{  <link rel="alternate" type="application/rss+xml"};
        $output .= qq{ href="$url"\n};
        $output .= qq{        title="$rss_title" />\n};
    }

    # Add <link> tags based on the sitemap.
    if ($self->{sitemap}) {
        my @links = $self->{sitemap}->links($page);
        if (@links) {
            $output .= join(q{}, @links);
        }
    }

    # End of the header.
    $output .= "</head>\n\n";

    # Add some generator comments.
    my $date = strftime('%Y-%m-%d %T -0000', gmtime());
    my $from
      = $self->{file} eq q{-}
      ? q{}
      : ' from ' . fileparse($self->{file});
    $output .= "<!-- Spun$from by spin 1.80 on $date -->\n";
    if ($self->{id}) {
        $output .= "<!-- $self->{id} -->\n";
    }

    # Add the <body> tag and the navbar (if we have a sitemap).
    $output .= "\n<body>\n";
    if ($self->{sitemap}) {
        my @navbar = $self->{sitemap}->navbar($page);
        if (@navbar) {
            $output .= join(q{}, @navbar);
        }
    }

    return (1, $output);
}

# Used to save the RCS Id for the document.  Doesn't actually output anything
# (the identifier is later used in _cmd_heading).
sub _cmd_id {
    my ($self, $id) = @_;
    $self->{id} = $id;
    return (1, q{});
}

# Include an image.  The size is added to the HTML tag automatically.
#
# $format - Format string
# $image  - Path to the image (may be relative or an absolute URL)
# $alt    - Alt text of image
sub _cmd_image {
    my ($self, $format, $image, $text) = @_;
    $image = $self->_parse($image);
    $text  = $self->_parse($text);

    # Determine the size attributes of the image if possible.
    my $size = -e $image ? q{ } . lc(html_imgsize($image)) : q{};

    # Generate the tag.
    my $output = qq{<img src="$image" alt="$text"$size};
    $output .= $self->_format_attr($format) . ' />';
    return (1, $output);
}

# Include a file.  Note that this includes a file after the current paragraph,
# not immediately at the current point, which may be a bit surprising.
sub _cmd_include {
    my ($self, $file) = @_;
    $file = $self->_parse($file);
    open(my $fh, '<', $file);
    push($self->{files}->@*, [$fh, $file]);
    return (1, q{});
}

# A link to a URL or partial URL.
#
# $format - Format string
# $url    - Target URL
# $text   - Anchor text
sub _cmd_link {
    my ($self, $format, $url, $text) = @_;
    $url  = $self->_parse($url);
    $text = $self->_parse($text);
    my $format_attr = $self->_format_attr($format);
    return (0, qq{<a href="$url"$format_attr>$text</a>});
}

# Preformatted text.  This does not use _block because we don't want to split
# the contained text into paragraphs and we want to parse it all in inline
# context always.
sub _cmd_pre {
    my ($self, $format, $text) = @_;
    my $output = $self->_border_end();
    $output .= '<pre' . $self->_format_attr($format) . '>';
    $output .= $self->_parse($text);
    $output .= "</pre>\n";
    return (1, $output);
}

# Used for the leading quotes that I have on many of my pages.  If the format
# is "broken", adds line breaks at the end of each line.
#
# $format - Format string, used as the format for the main <p> tag inside the
#           <blockquote>.  Values broken and short trigger special handling,
#           such as adding line breaks or changing the attribution class.
# $quote  - Text of the quote
# $author - Author of the quote
# $cite   - Attribution of the quote
sub _cmd_quote {
    my ($self, $format, $quote, $author, $cite) = @_;
    $author = $self->_parse($author);
    $cite   = $self->_parse($cite);
    my $output = $self->_border_end() . q{<blockquote class="quote">};

    # Parse the contents of the quote in a new block context.
    $self->_block_start();
    my @paragraphs = $self->_split_paragraphs($quote);
    $quote = join(q{}, map { $self->_parse($_, 1) } @paragraphs);
    $quote .= $self->_block_end();

    # Remove trailing newlines.
    $quote =~ s{ \n+ \z }{}xms;

    # If this is a broken quote, add line breaks to each line.
    if ($format eq 'broken') {
        $quote =~ s{ ( \S [ ]* ) ( \n\s* (?!</p>)\S )}{$1<br />$2}xmsg;

        # Remove <br /> tags for blank lines or at the start.
        $quote =~ s{ \n <br [ ] /> }{\n}xmsg;
        $quote =~ s{ <p> <br [ ] /> }{<p>}xmsg;
    }

    # If there was a format, apply it to every <p> tag in the quote.
    if ($format) {
        my $format_attr = $self->_format_attr($format);
        $quote =~ s{ <p> }{<p$format_attr>}xmsg;
    }

    # Done with the quote.
    $output .= $quote;

    # Format the author and citation.
    if ($author) {
        my $prefix = q{};
        if ($format eq 'broken' || $format eq 'short') {
            $output .= qq{<p class="attribution">\n};
        } else {
            $output .= qq{<p class="long-attrib">\n};
            $prefix = '&mdash; ';
        }
        if ($cite) {
            $output .= "    $prefix$author,\n    $cite\n";
        } else {
            $output .= "    $prefix$author\n";
        }
        $output .= '</p>';
    } else {
        $output .= "\n";
    }

    # Close the HTML tag and return the output.
    $output .= "</blockquote>\n";
    return (1, $output);
}

# Given the name of a product, return the release date of the product.
sub _cmd_release {
    my ($self, $package) = @_;
    $package = $self->_parse($package);
    if (!$self->{versions}) {
        $self->_warning('no package release information available');
        return (0, q{});
    }
    my $date = $self->{versions}->release_date($package);
    if (!defined($date)) {
        $self->_warning(qq(no release date known for "$package"));
        return (0, q{});
    }
    return (0, $date);
}

# Used to save RSS feed information for the page.  Doesn't output anything
# directly; the RSS feed information is used later in _cmd_heading.
sub _cmd_rss {
    my ($self, $url, $title) = @_;
    $url   = $self->_parse($url);
    $title = $self->_parse($title);
    push($self->{rss}->@*, [$url, $title]);
    return (1, q{});
}

# Used to end each page, this adds the navigation links and my standard
# address block.
sub _cmd_signature {
    my ($self) = @_;
    my $output = $self->_border_end();

    # If we're spinning from standard input, don't add any of the standard
    # footer, just close the HTML tags.
    if ($self->{file} eq q{-}) {
        $output .= "</body>\n</html>\n";
        return (1, $output);
    }

    # Otherwise, _footer does most of the work and we just add the tags.
    my $link = '<a href="%URL%">spun</a>';
    $output .= $self->_footer(
        $self->{file}, $self->{out_path}, $self->{id},
        "Last modified and\n    $link %MOD%",
        "Last $link\n    %NOW% from thread modified %MOD%",
    );
    $output .= "</body>\n</html>\n";
    return (1, $output);
}

# Insert the formatted size in bytes, kilobytes, or megabytes of some local
# file.  We could use Number::Format here, but what we're doing is simple
# enough and doesn't seem worth the trouble of another dependency.
sub _cmd_size {
    my ($self, $file) = @_;
    $file = $self->_parse($file);

    # Get the size of the file.
    my ($size) = (stat($file))[7];
    if (!defined($size)) {
        $self->_warning("cannot stat file $file: $!");
        return (0, q{});
    }

    # Format the size using SI units.
    my @suffixes = qw(K M G T);
    my $suffix   = q{};
    while ($size > 1024 && @suffixes) {
        $size /= 1024;
        $suffix = shift(@suffixes);
    }

    # Return the result.
    return (0, sprintf('%.0f', $size) . $suffix . 'B');
}

# Generates a HTML version of the sitemap and outputs that.
sub _cmd_sitemap {
    my ($self) = @_;
    if (!$self->{sitemap}) {
        $self->_warning('no sitemap file found');
        return (1, q{});
    }
    return (1, $self->_border_end() . $self->{sitemap}->sitemap());
}

# Start a table.  Takes any additional HTML attributes to set for the table
# (this is ugly, but <table> takes so many attributes for which there is no
# style sheet equivalent that it's unavoidable) and the body of the table
# (which should consist of \tablehead and \tablerow lines).
sub _cmd_table {
    my ($self, $format, $options, $body) = @_;
    my $tag = $options ? "table $options" : 'table';
    return $self->_block($tag, q{}, $format, $body);
}

# A heading of a table.  Takes the contents of the cells in that heading.
sub _cmd_tablehead {
    my ($self, $format, @cells) = @_;
    my $output = '  <tr' . $self->_format_attr($format) . ">\n";
    for (@cells) {
        my $text = $self->_parse($_) . $self->_border_end();
        $output .= (q{ } x 4) . $self->_enclose('th', $text) . "\n";
    }
    $output .= "  </tr>\n";
    return (1, $output);
}

# A data line of a table.  Takes the contents of the cells in that row.
sub _cmd_tablerow {
    my ($self, $format, @cells) = @_;
    my $output = '  <tr' . $self->_format_attr($format) . ">\n";
    for (@cells) {
        my $text = $self->_parse($_) . $self->_border_end();
        $output .= (q{ } x 4) . $self->_enclose('td', $text) . "\n";
    }
    $output .= "  </tr>\n";
    return (1, $output);
}

# Given the name of a package, return the version number of its latest
# release.
sub _cmd_version {
    my ($self, $package) = @_;
    if (!$self->{versions}) {
        $self->_warning('no package version information available');
        return (0, q{});
    }
    my $version = $self->{versions}->version($package);
    if (!defined($version)) {
        $self->_warning(qq(no version known for "$package"));
        return (0, q{});
    }
    return (0, $version);
}

##############################################################################
# Interface
##############################################################################

# Convert thread to HTML.  Be aware that the working directory from which this
# function is run matters a great deal, since thread may contain relative
# paths to files that the spinning process needs to access.
#
# $in_fh    - Input file handle of thread
# $in_path  - Input file path, used for error reporting
# $out_fh   - Output file handle to which to write the HTML
# $out_path - Output file path, used for error reporting
sub _spin {
    my ($self, $in_fh, $in_path, $out_fh, $out_path) = @_;

    # Initialize object state for a new document.
    $self->{files}    = [[$in_fh, $in_path]];
    $self->{id}       = undef;
    $self->{macro}    = {};
    $self->{out_fh}   = $out_fh;
    $self->{out_path} = $out_path;
    $self->{rss}      = [];
    $self->{space}    = q{};
    $self->{state}    = ['BLOCK'];
    $self->{variable} = {};

    # Parse the thread file a paragraph at a time (but pick up macro contents
    # that are continued across paragraphs.
    #
    # We maintain the stack of files that we're parsing in $self->{files}, and
    # _cmd_include will unshift new file handle and filename pairs onto that
    # stack.  That means that the top of the stack may change any time we call
    # _parse, so we have to update the input file handle and $self->{file}
    # each time through the loop.
    local $/ = q{};
    while ($self->{files}->@*) {
        ($in_fh, $self->{file}) = $self->{files}[-1]->@*;
        while (defined(my $para = <$in_fh>)) {
            if ("\n" !~ m{ \015 }xms && $para =~ m{ \015 }xms) {
                my $m = 'found CR characters; are your line endings correct?';
                $self->_warning($m);
            }
            my $open_count  = ($para =~ tr{\[}{});
            my $close_count = ($para =~ tr{\]}{});
            while (!eof && $open_count > $close_count) {
                my $extra = <$in_fh>;
                $open_count  += ($extra =~ tr{\[}{});
                $close_count += ($extra =~ tr{\]}{});
                $para .= $extra;
            }
            my $result = $self->_parse(_escape($para), 1);
            $result =~ s{ \A (?:\s*\n)+ }{}xms;
            if ($result !~ m{ \A \s* \z }xms) {
                $self->_output($result);
            }
            ($in_fh, $self->{file}) = $self->{files}[-1]->@*;
        }
        pop($self->{files}->@*);

        # Close the input file handle if it was one we opened, which is all of
        # them except the last one in the stack.
        if ($self->{files}->@*) {
            close($in_fh);
        }
    }

    # Close open tags and print any deferred whitespace.
    _print_fh($out_fh, $out_path, $self->_block_end(), $self->{space});
    return;
}

##############################################################################
# External converters
##############################################################################

# Given the output from a converter, the file to save the output in, and an
# anonymous sub that takes three arguments, the first being the captured
# blurb, the second being the document ID if found, and the third being the
# base name of the output file, and prints out a last modified line, reformat
# the output of an external converter.
sub _write_converter_output {
    my ($self, $page_ref, $output, $footer) = @_;
    my $page = $output;
    $page =~ s{ \A \Q$self->{output}\E }{}xms;
    open(my $out_fh, '>', $output);

    # Grab the first few lines of input, looking for a blurb and Id string.
    # Give up if we encounter <body> first.  Also look for a </head> tag and
    # add the navigation link tags before it, if applicable.  Add the
    # navigation bar right at the beginning of the body.
    my ($blurb, $docid);
    while (defined(my $line = shift($page_ref->@*))) {
        if ($line =~ m{ <!-- \s* (\$Id.*?) \s* --> }xms) {
            $docid = $1;
        }
        if ($line =~ m{ <!-- \s* ( (?:Generated|Converted) .*? )\s* --> }xms) {
            $blurb = $1;

            # Only show the date of the output, not the time or time zone.
            $blurb =~ s{ [ ] \d\d:\d\d:\d\d [ ] -0000 }{}xms;

            # Strip the date from the converter version output.
            $blurb =~ s{ [ ] [(] \d{4}-\d\d-\d\d [)] }{}xms;
        }
        if ($self->{sitemap} && $line =~ m{ \A </head> }xmsi) {
            my @links = $self->{sitemap}->links($page);
            if (@links) {
                _print_fh($out_fh, $output, @links);
            }
        }
        _print_fh($out_fh, $output, $line);
        if ($line =~ m{ <body }xmsi) {
            if ($self->{sitemap}) {
                my @navbar = $self->{sitemap}->navbar($page);
                if (@navbar) {
                    _print_fh($out_fh, $output, @navbar);
                }
            }
            last;
        }
    }
    warn "$0 spin: malformed HTML output for $output\n" unless $page_ref->@*;

    # Snarf input and write it to output until we see </body>, which is our
    # signal to start adding things.  We just got very confused if </body> was
    # on the same line as <body>, so don't do that.
    my $line;
    while (defined($line = shift($page_ref->@*))) {
        last if $line =~ m{ </body> }xmsi;
        _print_fh($out_fh, $output, $line);
    }

    # Add the footer and finish with the output.
    _print_fh($out_fh, $output, $footer->($blurb, $docid));
    if (defined($line)) {
        _print_fh($out_fh, $output, $line, $page_ref->@*);
    }
    close($out_fh);
    return;
}

# A wrapper around the cl2xhtml script, used to handle .changelog pointers in
# a tree being spun.  Adds the navigation links and the signature to the
# cl2xhtml output.
sub _cl2xhtml {
    my ($self, $source, $output, $options, $style) = @_;
    $style ||= $self->{style_url} . 'changelog.css';
    my @page   = capture("cl2xhtml $options -s $style $source");
    my $footer = sub {
        my ($blurb, $id) = @_;
        if ($blurb) {
            $blurb =~ s{ cl2xhtml }{\n<a href="$URL">cl2xhtml</a>}xms;
        }
        $self->_footer($source, $output, $id, $blurb, $blurb);
    };
    $self->_write_converter_output(\@page, $output, $footer);
    return;
}

# A wrapper around the cvs2xhtml script, used to handle .log pointers in a
# tree being spun.  Adds the navigation links and the signature to the
# cvs2xhtml output.
sub _cvs2xhtml {
    my ($self, $source, $output, $options, $style) = @_;
    $style ||= $self->{style_url} . 'cvs.css';

    # Separate the source file into a directory and filename.
    my ($name, $dir) = fileparse($source);

    # Construct the options to cvs2xhtml.
    if ($options !~ m{ -n [ ] }xms) {
        $options .= " -n $name";
    }
    $options .= " -s $style";

    # Run the converter and write the output.
    my @page   = capture("(cd $dir && cvs log $name) | cvs2xhtml $options");
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        if ($blurb) {
            $blurb =~ s{ cvs2xhtml }{\n<a href="$URL">cvs2xhtml</a>}xms;
        }
        $self->_footer($source, $output, $id, $blurb, $blurb);
    };
    $self->_write_converter_output(\@page, $output, $footer);
    return;
}

# A wrapper around the faq2html script, used to handle .faq pointers in a tree
# being spun.  Adds the navigation links and the signature to the faq2html
# output.
sub _faq2html {
    my ($self, $source, $output, $options, $style) = @_;
    $style ||= $self->{style_url} . 'faq.css';
    my @page   = capture("faq2html $options -s $style $source");
    my $footer = sub {
        my ($blurb, $id, $file) = @_;
        if ($blurb) {
            $blurb =~ s{ faq2html }{\n<a href="$URL">faq2html</a>}xms;
        }
        $self->_footer($source, $output, $id, $blurb, $blurb);
    };
    $self->_write_converter_output(\@page, $output, $footer);
    return;
}

# A wrapper around Pod::Thread and a nested _spin invocation, used to handle
# .pod pointers in a tree being spun.  Adds the navigation links and the
# signature to the output.
sub _pod2html {
    my ($self, $source, $output, $options, $style) = @_;
    $style //= 'pod';

    # Construct the Pod::Thread formatter object.
    my %options = (style => $style);
    if ($options) {
        if ($options =~ m{ -c ( \s | \z ) }xms) {
            $options{contents} = 1;
        }
        if ($options =~ m{ -t \s '(.*)' }xms) {
            $options{title} = $1;
        }
    } else {
        $options{navbar} = 1;
    }
    my $podthread = Pod::Thread->new(%options);

    # Grab the thread output.
    my $data;
    $podthread->output_string(\$data);
    $podthread->parse_file($source);

    # Run that through spin to convert to HTML.
    my $page;
    open(my $in_fh,  '<', \$data);
    open(my $out_fh, '>', \$page);
    $self->_spin($in_fh, q{-}, $out_fh, q{-});
    close($in_fh);
    close($out_fh);

    # Push the result through _write_converter_output.
    my $file = $source;
    $file =~ s{ [.] [^.]+ \z }{.html}xms;
    my $footer = sub {
        my ($blurb, $id) = @_;
        my $link = '<a href="%URL%">spun</a>';
        $self->_footer(
            $source, $output, $id,
            "Last modified and\n    $link %MOD%",
            "Last $link\n    %NOW% from POD modified %MOD%",
        );
    };
    my @page = map { "$_\n" } split(qr{\n}xms, $page);
    $self->_write_converter_output(\@page, $output, $footer);
    return;
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
    my $master  = <$pointer>;
    my $options = <$pointer>;
    my $style   = <$pointer>;
    close($pointer);

    # Clean up the contents.
    if (!$master) {
        die "no master file specified in $file\n";
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
#
## no critic (Subroutines::ProhibitExcessComplexity)
sub _process_file {
    my ($self) = @_;
    my $file = $_;
    return if $file eq q{.};
    for my $regex ($self->{excludes}->@*) {
        if ($file =~ m{$regex}xms) {
            $File::Find::prune = 1;
            return;
        }
    }
    my $input  = $File::Find::name;
    my $output = $input;
    $output =~ s{ \A \Q$self->{source}\E }{$self->{output}}xms
      or die "input file $file out of tree\n";
    my $shortout = $output;
    $shortout =~ s{ \A \Q$self->{output}\E }{...}xms;

    # Conversion rules for pointers.  The key is the extension, the first
    # value is the name of the command for the purposes of output, and the
    # second is the name of the method to run.
    my %rules = (
        changelog => ['cl2xhtml',   '_cl2xhtml'],
        faq       => ['faq2html',   '_faq2html'],
        log       => ['cvs2xhtml',  '_cvs2xhtml'],
        rpod      => ['pod2thread', '_pod2html'],
    );

    # Figure out what to do with the input.
    if (-d $file) {
        $self->{generated}{$output} = 1;
        if (-e $output && !-d $output) {
            die "cannot replace $output with a directory\n";
        } elsif (!-d $output) {
            _print_checked("Creating $shortout\n");
            mkdir($output, 0755);
        }
        my $rss_path = File::Spec->catfile($file, '.rss');
        if (-e $rss_path) {
            systemx('spin-rss', '-b', $file, $rss_path);
        }
    } elsif ($file =~ m{ [.] th \z }xms) {
        $output   =~ s{ [.] th \z }{.html}xms;
        $shortout =~ s{ [.] th \z }{.html}xms;
        $self->{generated}{$output} = 1;
        my $relative = $input;
        $relative =~ s{ ^ \Q$self->{source}\E / }{}xms;
        my $time = 0;
        if ($self->{versions}) {
            $time = $self->{versions}->latest_release($relative);
        }
        if (-e $output) {
            return if (-M $file >= -M $output && (stat($output))[9] >= $time);
        }
        _print_checked("Spinning $shortout\n");
        open(my $in_fh,  '<', $input);
        open(my $out_fh, '>', $output);
        $self->_spin($in_fh, $input, $out_fh, $output);
        close($in_fh);
        close($out_fh);
    } else {
        my ($extension) = ($file =~ m{ [.] ([^.]+) \z }xms);
        if (defined($extension) && $rules{$extension}) {
            my ($name, $sub) = $rules{$extension}->@*;
            $output   =~ s{ [.] \Q$extension\E \z }{.html}xms;
            $shortout =~ s{ [.] \Q$extension\E \z }{.html}xms;
            $self->{generated}{$output} = 1;
            my ($source, $options, $style) = $self->_read_pointer($input);
            if (-e $output && -e $source) {
                if (-M $input >= -M $output && -M $source >= -M $output) {
                    return;
                }
            }
            _print_checked("Running $name for $shortout\n");
            $self->$sub($source, $output, $options, $style);
        } else {
            $self->{generated}{$output} = 1;
            if (!-e $output || -M $file < -M $output) {
                _print_checked("Updating $shortout\n");
                copy($file, $output)
                  or die "copy of $input to $output failed: $!\n";
            }
        }
    }
    return;
}
## use critic

# This routine is called by File::Find for every file in the destination tree
# in depth-first order, if the user requested file deletion of files not
# generated from the source tree.  It checks each file to see if it is in the
# $self->{generated} hash that was generated during spin processing, and if
# not, removes it.
#
# Throws: autodie exception on failure of rmdir or unlink
sub _delete_files {
    my ($self) = @_;
    return if $_ eq q{.};
    my $file = $File::Find::name;
    return if $self->{generated}{$file};
    my $shortfile = $file;
    $shortfile =~ s{ ^ \Q$self->{output}\E }{...}xms;
    _print_checked("Deleting $shortfile\n");
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
        push(@excludes, map { qr{$_}xms } $args_ref->{exclude}->@*);
    }

    # Add a trailing slash to the partial URL for style sheets.
    my $style_url = $args_ref->{'style-url'} // q{};
    if ($style_url) {
        $style_url =~ s{ /* \z }{/}xms;
    }

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
    my $cwd = getcwd() or die "cannot get current directory: $!\n";
    my ($in_fh, $out_fh);

    # Reset data from a previous run.
    delete $self->{repository};
    delete $self->{sitemap};
    delete $self->{versions};

    # When spinning a single file, the input file must not be a directory.  We
    # do the work from the directory of the file to ensure that relative file
    # references resolve properly.
    if (defined($input)) {
        $input = realpath($input) or die "cannot canonicalize $input: $!\n";
        if (-d $input) {
            die "input file $input must be a regular file\n";
        }
        open($in_fh, '<', $input);
        my (undef, $input_dir) = fileparse($input);
        chdir($input_dir);
    } else {
        $input = q{-};
        open($in_fh, '<&', 'STDIN');
    }

    # Open the output file.
    if (defined($output)) {
        $output = realpath($output) or die "cannot canonicalize $output: $!\n";
        $output =~ s{ /+ \z }{}xms;
        open($out_fh, '>', $output);
    } else {
        $output = q{-};
        open($out_fh, '>&', 'STDOUT');
    }

    # Do the work.
    $self->_spin($in_fh, $input, $out_fh, $output);

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

    # Reset data from a previous run.
    delete $self->{repository};
    delete $self->{sitemap};
    delete $self->{versions};

    # Canonicalize and check input.
    $input = realpath($input) or die "cannot canonicalize $input: $!\n";
    if (!-d $input) {
        die "input tree $input must be a directory\n";
    }
    $self->{source} = $input;

    # Canonicalize and check output.
    if (!-d $output) {
        _print_checked("Creating $output\n");
        mkdir($output, 0755);
    }
    $output = realpath($output) or die "cannot canonicalize $output: $!\n";
    $self->{output} = $output;

    # Read metadata from the top of the input directory.
    my $sitemap_path = File::Spec->catfile($input, '.sitemap');
    if (-e $sitemap_path) {
        $self->{sitemap} = App::DocKnot::Spin::Sitemap->new($sitemap_path);
    }
    my $versions_path = File::Spec->catfile($input, '.versions');
    if (-e $versions_path) {
        $self->{versions} = App::DocKnot::Spin::Versions->new($versions_path);
    }
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

##############################################################################
# Module return value and documentation
##############################################################################

## no critic (Documentation::RequirePackageMatchesPodName)

1;
__END__

=for stopwords
Allbery RCS RSS XHTML YYYY-MM-DD -dhv faq2html respin respun spin-rss
cl2xhtml cvs2xhtml preformatted

=head1 NAME

spin - Translate thread, an HTML macro language, into XHTML

=head1 SYNOPSIS

spin [B<-dhv>] [B<-e> I<pattern> ...] [B<-s> I<url>] I<source> [I<output>]

=head1 REQUIREMENTS

Perl 5.005 or later and the Image::Size and Text::Balanced modules.  Also
expects to find B<faq2html>, B<cvs2xhtml>, and B<cl2xhtml> to convert certain
types of files.  The Git::Repository module is required to determine last
change dates for thread source from Git history.

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

It is also highly recommended, if you are using Subversion, CVS, or RCS for
revision control, to put C<< \id[$Z<>Id$] >> as the first command in each
file.  In Subversion, you will also need to enable keyword expansion with
C<svn propset svn:keywords Id I<file>>.  B<spin> will then take care of
putting the last modified date in the footer for you based on the Id timestamp
(which may be more accurate than the last modified time of the thread file).
If you are using Git, you don't need to include anything special in the thread
source; as long as the source directory is the working tree of a Git
repository, B<spin> will use Git to determine the last modification date of
the file.

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

=item \id[<id>]

Tells B<spin> the Subversion, CVS, or RCS revision number and time.  <id>
should be the string C<< $Z<>Id$ >>, which will be expanded by Subversion,
CVS, and RCS.  This string is embedded verbatim in an HTML comment near the
beginning of the generated output as well as used for the last modified
information added by the \signature command.  For this command to behave
properly, it must be given before \heading.

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

    \=[FOO][some string]

will cause any later occurrences of C<\=FOO> in the file to be replaced with
the text C<some string>.  This can be useful for things like URLs for links,
so that all the URLs can be collected at the top of the page for easy
updating.

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
