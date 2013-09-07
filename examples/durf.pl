#!/usr/local/bin/perl
# -*-Perl-*-
eval 'exec /usr/local/bin/perl -S $0 ${1+"$@"}'
 if 0;
'di';
'ig00';
#From jimbo@rcx1.ssd.csd.harris.com Wed Jun 23 10:17:25 1993
#From: jimbo@rcx1.ssd.csd.harris.com (Jim Winters)
#Newsgroups: comp.lang.perl
#Subject: durf: disc usage report formatter
#Date: 19 Jun 93 17:53:23 GMT
#Organization: Harris Computer Systems Division

#If you are like me, you find the output of du(1) a little hard to
#interpret.  I mean, the fields are simple enough to understand, but
#it's just hard to get the big picture if you're du'ing a large tree.

#durf is a perl script that that formats the output of du differently.
#"Durf" is for "disk usage report formatter", (really just because I
#like the name).

#I'm something of a novice perl programmer, so my perl code is probably
#not the best, but I've been using this script for a few months now
#with no problems or even any desire to change it!  :-)

#I don't know how portable this will be to other systems, but I don't
#particularly expect any problems.  (I'm on a flavor of unix.)  Du on
#our system has a pretty simple format, and durf doesn't really care
#where the input comes from (if you use the '-' argument), as long as
#it's in that format.  I have noticed that durf does not work with some
#older versions of perl.

#The script has had the man page thingie (forgot the name, sorry)
#applied to it, so it is its own man page.  Also, durf has a -H option
#for help.

#Go to some (small) directory and type durf.  The output will look
#something like this (from my bin directory):


#Tot%  Blocks  Full-pathname
#    Tot% Sub% Blocks    Child

#100%    5019  .                     <--- name of top level directory
#     75%  75%   3804    m88k            <--- a child, "m88k"
#     15   15     779    .               <--- special child, "."
#      4    4     243    hcx
#      2    2     120    gcx
#      1    1      73    sun

# 75%    3804  ./m88k
#     75% 100%   3804    .

#(I've truncated the output for this article.)

#Basically durf is telling you how the space is distributed in the
#tree.  Here 75% of the total space is in the m88k subtree.  m88k also
#has 75% of the space in the subdirectory.  (Since this is the top
#level, all the percentages are the same.)  The other numbers are sizes
#in blocks.

#The special child "." is the amount of space in the directory
#exclusive of the children.

#After the list of top-level children, we see a listing for the m88k
#subtree.  Within this subtree, we see that 75% of the total space is
#in ".", which is consistent with the previous entry.  However, since
#m88k has no children, 100% of the space in THIS subtree is in ".".

#Naturally, this is all more interesting with bigger trees.

#durf requires "yagrip.pl" by jgreely@cis.ohio-state.edu, 89/11/1, so I
#have included it as well.  It needs to go in your perl library.

#I release this code for any purpose whatsoever.  Enjoy.

#
# $Header: /net/pauillac/caml/repository/cash/examples/durf.pl,v 1.1 2002/06/19 14:38:15 verlyck Exp $
#
# $Log: durf.pl,v $
# Revision 1.1  2002/06/19 14:38:15  verlyck
# Added the perl script from which was written durf.ml, with its motivation and
# documentation.
#

# durf - disk usage report formatter.
#

sub Usage {
    die &usage( @Usage ) , <<EndOfUsage;

-H      Write this message and die.
-t threshold[%]
        Only show directories at or above threshold blocks.  With "%",
        only show directories at or above threshold percent of the
        total.  Default is "1%"
du-args E.g., directory on which to operate.  Default is ".".
-       Read standard input for 'du' listing.

Like 'du', but formatted differently.
EndOfUsage
}

#
# Deal with options
#

$threshold = "1%";

# yagrip doit etre au meme endroit que durf
$_ = $ENV{'HOME'} . '/pl';
($_ = $0) =~ s|/[^/]*$|| unless -d $_; # le dir de ce programme-ci
unshift(@INC, $_);		# avec push, do merde quelquefois (??)

require "yagrip.pl";            # allows "--" to terminate options

$Option_string = "Ht:";

@Usage = ( &sname( $0 ), $Option_string, "threshold[%]", "[{du-args|-}]" );

&getopt( $Option_string ) || die &usage( @Usage );

if ( defined( $opt_H ) ) { &Usage(); }

if ( defined( $opt_t ) ) { $threshold = $opt_t; }

if ( $threshold =~ /%$/ ) {
    $threshold_is_percent = 1;
    chop( $threshold );
}

$threshold += 0;

#
# Prepare file handle
#
if ( $ARGV[0]  && $ARGV[0] eq '-' ) {
    $open_string = '-';
} else {
    $open_string = 'du ' . join( ' ', @ARGV ) . '|';
}

open( DU, $open_string ) || die "Can't open \"$open_string\".\n";

#
# Load input
#
while(<DU>) {
    chop;
    if ( /^\s*(\d+)\s+(\S.*)$/ ) {
        &load( $1, $2 );
    } else {
        print STDERR "Can't understand line:\n$_\n";
    }
}

#
# Establish list of roots
#
foreach (keys %size) {
    push( @root_list, $_ ) if ( ! $parent{$_} || ! $size{ $parent{$_} } );
}

#
# Calculate total size
#
grep( $total_size += $size{$_}, @root_list );

&print_headers();

#
# Output formatted tree for each root
#
while( $_ = pop( @root_list ) ) {
    &do_subtree( $_ );
}

exit( 0 );

sub do_subtree {
    push( @subtree_list, @_ );
    while( $_ = pop( @subtree_list ) ) {
        &do_output( $_ );
    }
}

sub sname {
    local( $name ) = @_;
    local( @name_array ) = split( '/', $name );
    pop( @name_array );
}

sub dname {
    local( $name ) = @_;
    local( @name_array ) = split( '/', $name );
    if ( scalar( @name_array ) > 1 ) {
        pop( @name_array );
        join( '/', @name_array );
    } else {
        $name;
    }
}

sub load {
    local( $size, $name ) = @_;
    $size{ $name } = $size;
    $parent = &dname( $name );
    if ( $parent ne $name ) {
        $parent{ $name } = $parent;
        @children = split( "\n", $children{ $parent } );
        push( @children, $name );
        $children{ $parent } = join( "\n", @children );
    }
}

sub do_output {
    local( $name ) = @_;

    &print_long( $size{$name}, $name );

    @children = split( "\n", $children{ $name } );

    $dot_size = $size{$name};

    $subtree_size = $dot_size;

    grep( $dot_size -= $size{$_}, @children );

    $dot_name = $name . "/.";

    push( @children, $dot_name );

    $size{$dot_name} = $dot_size;

    if ( $threshold_is_percent ) {
        @children =
            grep( ($size{$_}*100)/$total_size >= $threshold, @children );
    } else {
        @children = grep( $size{$_} >= $threshold, @children );
    }

    @children = reverse sort { $size{$a} <=> $size{$b}; } @children;

    @push_list = reverse grep( $_ ne $dot_name, @children );
    push( @subtree_list, @push_list );

    $num_components = scalar( split( '/', $name ) );

    # do short output here

    $percent_char = '%';

    foreach (@children) {
        $size = $size{$_};
        @name_array = split( '/', $_ );
        splice( @name_array, $[, $num_components );
        $_ = @name_array[$[];
        &print_short( $size, $_ );
        $percent_char = ' ';
    }
}

sub print_short {
    local( $size, $name ) = @_;
    $sub_percent = int( ($size*100)/$subtree_size );
    printf "    %3d%s %3d%s %6d    %s\n",
        int( ($size*100)/$total_size ), $percent_char,
        $sub_percent, $percent_char,
        $size,
        $name;
}

sub print_long {
    local( $size, $name ) = @_;
    print "\n";
    printf "%3d%%  %6d  %s\n",
        int( ($size*100)/$total_size ),
        $size,
        $name;
}

sub print_headers {
    print "Tot%  Blocks  Full-pathname\n";
    print "    Tot% Sub% Blocks    Child\n";
}

###############################################################

    # These next few lines are legal in both Perl and nroff.

.00;                       # finish .ig
 
'di           \" finish diversion--previous line must be blank
.nr nl 0-1    \" fake up transition to first page again
.nr % 0         \" start at page 1
'; __END__ ##### From here on it's a standard manual page #####

.TH DURF 1 "February 10, 1993"
.AT 3
.SH NAME
durf \- disk usage report formatter
.SH SYNOPSIS
.B durf [\-H] [\-t threshold[%]] [{du-args|\-}]
.SH DESCRIPTION
.I Durf
reformats the output of
.IR du\^ (1)
to make it a little easier to get an understanding of the distribution of space.
.P
The output is grouped by subtrees.
Each subtree is shown as a line like this:
.sp
.nf
\fITotal\fP%\fI   Blocks   Full-pathname\fP
.fi
.sp
which shows the position in the hierarchy and reports size
information for the subtree, followed by a group of indented lines like this:
.sp
.nf
      \fITotal\fP%\fI  Subtree\fP%\fI   Blocks   Child\fP
.fi
.sp
which reports size information for that portion of the subtree.
.I Child
is an immediate child of
.IR Full-pathname .
.IR Total %
is the percentage contribution to the total size.
.IR Subtree %
is the percentage contribution to the subtree.
.I Blocks
are disk blocks as reported by
.IR du .
The list of children is in decreasing order of size.
After the list of all the children, the next non-blank line begins the
subtree for the first child, and so on.
.P
A special fake child,
.BR . ,
is listed in each subtree (if it meets the threshold requirement).
This is simply the contribution of the directory independent of
the children.  (It is the size of the subtree less the size of the
children.)
.P
If the argument to
.I durf
is
.BR \- ,
then
.I durf
reads the standard input for the
.I du
list.
Otherwise, it runs
.IR du ,
passing it
.IR du-args .
You may pass options to
.I du
by terminating the
.I durf
options with
.BR \-\- .
.SS Options
.TP 12
\-H
Writes a usage message and dies.
.TP
\-t \fIthreshold\fP[%]
Sets a threshold for which entries
.I durf
will show.
If the threshold ends with '%', then
.I durf
will not show entries that contribute less than
.I threshold
percent to the total space.
Otherwise, the threshold is in terms of disk blocks.
The default is \fB\-t 1%\fP.
To get a complete listing, use \fB\-t 0\fP.
.SH AUTHOR
Jim Winters
.SH "SEE ALSO"
du(1).
.SH CAUTION
Believes
.IR du .
