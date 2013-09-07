(***********************************************************************)
(*                                Cash                                 *)
(*                                                                     *)
(*          Bruno Verlyck, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Cash is based on Scsh, by Olin Shivers.                            *)
(***********************************************************************)

(** The Caml Shell *)

(** Cash is a Unix shell that is embedded within Objective Caml.  It's a Caml
implementation of (an as large as possible subset of) the API of Scsh, the
Scheme Shell by Olin Shivers.  See the
{{:ftp://ftp.scsh.net/pub/scsh/0.5/scsh-manual.ps.gz}Scsh manual } of which this
manual is a mere adaptation.  ({i This is to check that Olin "did it.  \[He\]
did it all by \[him\]self" --- you should at least read his foreword,
`Acknowledgments'.  In no way prior knowledge of Scsh is necessary to use
Cash.})  *)

(** Cash is designed for writing useful standalone Unix programs and shell
scripts --- it spans a wide range of application, from ``script'' applications
usually handled with perl or sh, to more standard systems applications usually
written in C.

Cash has two components: a process notation for running programs and setting up
pipelines and redirections (not yet implemented), and a complete syscall library
for low-level access to the operating system.  This manual gives a complete
description of Cash.  (A general discussion of the design principles behind Scsh
can be found in the paper
{{:ftp://ftp.scsh.net/pub/scsh/papers/scsh-paper.ps.gz}``A Scheme Shell'' }. *)

(** {2 Copyright & source-code license} *)

(** Cash open-source and can be freely redistributed; it is distributed under
the terms of the GNU Lesser General Public License version 2.1 (see the file
[LGPL] in the distribution).  *)

(** {2 Caveats} *)

(** It is important to note what Cash is {i not}, as well as what it is.  Cash
is primarily designed for the writing of shell scripts --- programming.  It is
not a very comfortable system for interactive command use: the current release
lacks job control, command-line editing, a terse, convenient command syntax, and
it does not read in an initialisation file analogous to [login] or [.profile].

However, Cash has a version string: *)

value version : string;

(** {2 Naming conventions} *)

(** Following Scsh, we use a general naming scheme that consistently employs a
set of abbreviations.  This is intended to make it easier to remember the names
of things. *)

(** {3 Common naming conventions} *)

(** Some of the common conventions we share with Scsh are:
- [fdes] or [fd] means ``file descriptor,'' a small integer used in Unix to
  represent I/O channels.
- [call_with_...] Procedures that call their argument on some computed value are
  usually named [call_with_...], {i e.g.}, [call_with_fdes_in] {i in_channel}
  {i proc}, which calls {i proc} on {i in_channel}'s file descriptor, returning
  whatever {i proc} returns.  The abbreviated name means ``call with file
  descriptor from in_channel.''
- [with_...] Procedures that call their argument in some special dynamic context
  frequently have names of the form [with_...].  For example, [with_env] {i env}
  {i thunk}. These functions set the process environment body, execute their
  thunk, and then return after resetting the environment to its original state.
- [create_...] or [delete_...] Procedures that create (resp. delete) objects in
  the file system (files, directories, temp files, fifos, {i etc.}), begin with
  [create_...] (resp. [delete_...]).
- [low_...] These are lower-level Cash primitives that are not commonly used
  (but for implementing the higher-level ones).
- [..._info...] Data structures packaging up information about various OS
  entities frequently end in ...[_info]. Examples:
        [user_info_...], [file_info_...], [group_info_...], and [host_info_...].
*)

(** {3 Cash own naming conventions} *)

(** This paragraph is intended for users already familiar with Scsh, to help
 them to find the corresponding procedures' names while translating their
 scripts :-).  You may skip it it you wish.*)

(** We had to extend Scsh's conventions for two sorts of reasons: first, Caml
being statically typed, Scsh's polymorphic procedures are often present in 2 to
4 versions in Cash, suffixed by a type tag indicative of the type of the main
argument this procedure operates on.  The most common tags are:
- ..._in : argument is an in_channel
- ..._out: argument is an out_channel
- ..._fd : argument is a file descriptor
- ..._fn : argument is a file name
*)

(** Second, Caml is much stricter about the lexical syntax of its identifiers
than Lisp like languages, so we generally translated them this way:
- [from->to] becomes [to_of_from], staying in Caml's naming philosophy
- [predicate?] becomes [is_predicate].  Combined with the first extension, this
  gives, {i e.g.}, [file-directory?] comes in the 4 flavors
  [is_file_directory_fn], [is_file_directory_fd], [is_file_directory_in] and
  [is_file_directory_out].
- [!], the Scheme marker for side-effecting procedures, becomes [_bang] (for
  lack of a better translation).

There are several exceptions: [file-exists?] becomes the slightly more euphonic
[is_file_existing_fn], [is_file_existing_fd], {i etc.} and [file-not-exist?] is
[file_not_exists_fn], [file_not_exists_fd], {i etc.} because it doesn't yield a
bool in Cash and is no more really a predicate. [move->fdes] has the 3 versions
[move_fd_to_fdes], [move_in_channel_to_fdes] and [move_out_channel_to_fdes].
There are more around [dup]-ing procedures (see section {!Cash.unixio}).

All this generally makes identifiers even longer than the Scheme ones (sorry for
this), except for [(current-input-port)] or [(with-current-input-port ...)]
which become the more civilized [stdin] and [with_stdin ...].  *)

(** {2 A word about Unix standards} *)

(** "The wonderful thing about Unix standards is that there are so many to
choose from."  You may be totally bewildered about the multitude of various
standards that exist.  Rest assured that this nowhere in this manual will you
encounter an attempt to spell it all out for you; you could not read and
internalise such a twisted account without bleeding from the nose and ears.

However, you might keep in mind the following simple fact: of all the standards,
Posix is the least common denominator.  So when this manual repeatedly refers to
Posix, the point is ``the thing we are describing should be portable just about
anywhere.''  Cash sticks to Posix when at all possible; its major departure is
symbolic links, which aren't in Posix (see --- it really {e is} a least common
denominator). *)

(* p 15. *)

(** {1 Process notation} *)

(** Scsh has a notation for controlling Unix processes that takes the form of
s-expressions; this notation can then be embedded inside of standard Scheme
code.  This notation is not yet done for Cash.  If you want to have a feeling of
what it'll resemble to, refer to the
{{:ftp://ftp.scsh.net/pub/scsh/0.5/scsh-manual.ps.gz}Scsh manual }, chapter
2.  Thus we skip directly to the basic blocks on top of which this notation is
built (after a little advertising for the Scsh API).
 *)

(** {2 Procedures and syntax extensions} *)

(** It is a general design principle in Scsh/Cash that all functionality made
available through special syntax is also available in a straightforward
procedural form.  So there are procedural equivalents for all of the process
notation.  In this way, the programmer is not restricted by the particular
details of the syntax. *) (* XXX completer. *)

(** Having a solid procedural foundation also allows for general notational
experimentation using Camlp4 macros.  For example, the programmer can build his
own pipeline notation on top of the [fork] and [fork_with_pipe] procedures.
Chapter {!Cash.syscalls} gives the full story on all the procedures in the
syscall library.  *)

(** {2 Interfacing process output to Caml} *)

(** There is a family of procedures that can be used to capture the output of
processes as Caml data. *)

(** [run_with_...] all fork off subprocesses, collecting the process' output to
stdout in some form or another.  The subprocess runs with file descriptor 1 and
the current [stdout] channel bound to a pipe. *)

(** Value is an in_channel open on process's [stdout].
  Returns immediately after forking child. *)
value run_with_in_channel : (unit -> unit) -> in_channel;

(** Value is an out_channel open on process's [stdin].
  Returns immediately after forking child. *)
value run_with_out_channel : (unit -> unit) -> out_channel;

(** Value is name of a temp file containing process's output.
  Returns when process exits. *)
value run_with_file : (unit -> unit) -> string;

(** Value is a string containing process' output.
  Returns when eof read. *)
value run_with_string : (unit -> unit) -> string;

(** Splits process' output into a list of newline-delimited strings.  Returns
when eof read.  The delimiting newlines are not included in the strings
returned. *)
value run_with_strings : (unit -> unit) -> list string;

(** In sexp procedures below, `data', `object' and `item' should conform to some
Lisp/Scheme syntax.  See sexp.mli and atomo.mll for details on the supported
syntax.  [Sexp.simple] values can be printed with [Sexp.display]. *)

(** Reads a single object from process' stdout with [Sexp.read].
  Returns as soon as the read completes . *)
value run_with_sexp : (unit -> unit) -> Sexp.simple;

(** Repeatedly reads objects from process' stdout with [Sexp.read].
  Returns accumulated list upon eof. *)
value run_with_sexps : (unit -> unit) -> list Sexp.simple;

(** The following procedures are also of utility for generally parsing
    input streams in Cash. *)

(** Reads the channel until eof, then returns the accumulated string. *)
value string_of_in_channel : in_channel -> string;

(** Repeatedly reads data from the channel until eof, then returns the accumulated
  list of items in a schemeish form.  Note: you can read one item with
  [Sexp.read].  *)
value sexp_list_of_in_channel : in_channel -> list Sexp.simple;

(** Repeatedly reads newline-terminated strings from the channel until eof, then
  returns the accumulated list of strings. The delimiting newlines are not part
  of the returned strings. *)
value string_list_of_in_channel : in_channel -> list string;

(** Generalises these two procedures.
  It uses a reader to repeatedly read objects from a channel.
  It accumulates these objects into a list, which is returned upon eof. *)
value list_of_in_channel : (in_channel -> 'a) -> in_channel -> list 'a;

(** The [string_list_of_in_channel] and [sexp_list_of_in_channel] procedures
  are trivial to define, being merely [list_of_in_channel] curried with the
  appropriate parsers:
{[ let string_list_of_in_channel = list_of_in_channel input_line
 let sexp_list_of_in_channel = list_of_in_channel Sexp.read ]}
 *)
(** The following compositions also hold:
{[ run_with_string thunk = run_with_in_channel o string_of_in_channel
 run_with_strings thunk = run_with_in_channel o string_list_of_in_channel
 run_with_sexp thunk = run_with_in_channel o Sexp.read
 run_with_sexps thunk = run_with_in_channel o sexp_list_of_in_channel ]}
 *)

(* p 16. *)
(**

[fold_in_channel] {i ichan reader op seed} can be used to perform a variety of
iterative operations over an input stream.  It repeatedly uses {i reader} to
read an object from {i ichan}.  If the first read returns eof, then the entire
[fold_in_channel] operation returns the seed.  If the first read operation
returns some other value {i v}, then {i op} is applied to {i v} and the seed:
 {i op v seed}.
This should return a new seed value, and the reduction then loops, reading a new
value from the channel, and so forth.

For example, [list_of_in_channel] {i reader channel} could be (and in fact is)
defined as
{[        List.rev (fold_in_channel channel reader (::) \[\])
]} 
An imperative way to look at [fold_in_channel] is to say that it abstracts the
idea of a loop over a stream of values read from some channel, where the seed
value expresses the loop state. *)
value fold_in_channel : in_channel -> (in_channel -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'b;

(* p 17. *)
(** {2 More complex process operations} *)

(** The procedures in the previous section provide for the common case, where
the programmer is only interested in the output of the process.  These
procedures provide more complicated facilities for manipulating processes. *)

(** The type of a process object; it encapsulates the subprocess' process id and
exit code; it is the value passed to the {!Cash.wait} system call (which gives access
to the exit code when it is ready).  See also {!Cash.pid_of_proc} *)
type proc = Proc_3_4.proc;

(** This procedure can be used if the programmer also wishes access to the
process' pid, exit status, or other information.  It forks off a subprocess,
returning two values: a channel open on the process' stdout (and current
[stdout]), and the subprocess's process object. *)
value run_with_inchan_plus_proc : (unit -> unit) -> (in_channel * proc);
(** For example, to uncompress a tech report, reading the uncompressed data into
Cash, and also be able to track the exit status of the decompression process,
use the following:
{[  let (chan, child) =
    run_with_inchan_plus_proc (fun () -> exec_path "zcat" ["tr91-145.tex.Z"\]}) in
  let paper = string_of_in_channel chan in
  let status = wait child in
  (* ...use paper, status and child here... *) ]}
*) (** Note that you must {i first} do the [string_of_in_channel] and {i then}
do the [wait] --- the other way around may lock up when the zcat fills up its
output pipe buffer. *)

(** This procedure is the dual of the preceding: the program has to write to the
child's stdin.  It forks off a subprocess, returning two values: a channel open
on the process' stdin (and current [stdin]), and the subprocess's process
object.  (Be prepared to SIGPIPE). *)
value run_with_outchan_plus_proc : (unit -> unit) -> (out_channel * proc);

(** {2 Multiple stream capture} *)

(** The Unix view of file descriptors.  See {!Cash.gen_io} for explanations
about how file descriptors are managed by Cash. *)
type fd = int;

(** Occasionally, the programmer may want to capture multiple distinct output
streams from a process. For instance, he may wish to read the stdout and stderr
streams into two distinct strings. This is accomplished with the
[run_with_collecting] procedure. *)

(** Run processes that produce multiple output streams and return channels open
on these streams.  To avoid issues of deadlock, [run_with_collecting] doesn't use
pipes. Instead, it first runs the process with output to temp files, then
returns channels open on the temp files. For example,
{[  run_with_collecting [1; 2] (fun () -> exec_path "ls" []) ]}
runs [ls] with stdout (fd 1) and stderr (fd 2) redirected to temporary
files.
When the [ls] is done, [run_with_collecting] returns three values: the
[ls] process' exit status, and two channels open on the temporary files. The
files are deleted before [run_with_collecting] returns, so when the channels are
closed, they vanish. *)
value run_with_collecting :
  list fd -> (unit -> unit) -> (Unix.process_status * list in_channel);
(**

For example, if Kaiming has his mailbox protected, then
{[  let (status, fds) =
     run_with_collecting [1; 2]
       (fun () -> exec_path "cat" ["/usr/kmshea/mbox"]) in
   (status, List.map string_of_in_channel fds) ]}
might produce
{v - : (Unix.process_status * list string) =
(Unix.WEXITED 1, [""; "cat: /usr/kmshea/mbox: Permission denied\n"]) v}
 *)

(* p 19. *)
(* Syntax
  or (||) *)

(* p 20. *)
(* Syntax
  and (&&) *)

(** {2 Process filters} *)

(** These procedures are useful for forking off processes to filter text streams. *)

(** Returns a procedure that when called, repeatedly reads a character from the
    current [stdin], applies its first argument {i filter} to the character, and
    writes the result to the current [stdout].  The procedure returns upon
    reaching eof on [stdin]. *)
value char_filter : (char -> char) -> unit -> unit;

(** Returns a procedure that when called, repeatedly reads a string from the
    current [stdin], applies its first argument {i filter} to the string, and
    writes the result to the current [stdout].  The procedure returns upon
    reaching eof on [stdin].

    The optional [buflen] argument controls the number of characters each
    internal read operation requests; this means that [filter] will never be
    applied to a string longer than [buflen] chars.  The default [buflen] value
    is 1024. *)
value string_filter : ?buflen: int -> (string -> string) -> unit -> unit;

(** {1:syscalls System calls} *)

(** Cash aims at providing essentially complete access to the basic Unix kernel
services: processes, files, signals and so forth.  As the [Unix] module provides
a fairly good Posix interface, Cash often relies on it to give an extended
interface.  In particular, it uncovers the opaque [Unix.file_descr] type, and
all the necessary connections with the so-called `revealed' channels.  Cash adds
very few restrictions to the way [Pervasives], [Unix] and [Cash] functions
(especially I/O) may be freely intermixed. {i E.g.}, [Unix.read] on a
[Unix.file_descr] obtained by [Unix.in_channel_of_descr] still needs careful
synchonization with [Unix.lseek] and/or [seek_in/tell_in] if Pervasives I/O is
to be interleaved.  *)

(* p 21. *)
(** {2 Errors} *)

(** The [Unix] module already raises exceptions for any errno <> 0, so if any
syscall returns, it succeeded.  Note that Cash should automatically retry any
interrupted system call it defines, so they never raise [Unix.EINTR]. *)

value errno_error : Unix.error -> string -> string -> 'a;
(** Raises a [Unix] error exception for [Unix.error] argument.
  This is just for compatibility with Scsh. *)

(* p 22. *)
(* with-errno-handler: use try. *)

value unwind_protect : (unit -> 'a) -> ('b -> unit) -> 'b -> 'a;
(** [unwind_protect] {i thunk protect ed}, named after a similar functionality
of Lisp, calls thunk, then, before returning its result (be it a value or an
exception), ensures that {i protect} is applied to {i ed}.  It can be used,
{i e.g.}, to ensure that a file is closed after an action, regardless of any
exception this action may raise.  *)

(** {2:gen_io I/O} *)
(** {3 Pervasives I/O operations} *)
(** Contrarily to Scsh, when using file descriptors, Cash doesn't attempt to
bypass the underlying I/O system, which is reasonably efficient.  So to use
[Pervasives] primitives on file descriptors, you should use [in_channel_of_fd] or
[out_channel_of_fd] to get the proper channel.  [Unix read] and [write] primitives are
still available for those who {i really} want them. *)

(* p 24. *)
(** {3:withstd Channel manipulation and standard channels} *)

value close_fd_after : fd -> (fd -> 'a) -> 'a;
value close_in_after : in_channel -> (in_channel -> 'a) -> 'a;
value close_out_after : out_channel -> (out_channel -> 'a) -> 'a;
(** [close_..._after] {i channel/fd consumer} return {i (consumer channel/fd)},
but close the channel (or file descriptor) on return. *)

value with_stdin : in_channel -> (unit -> 'a) -> 'a;
value with_stdout : out_channel -> (unit -> 'a) -> 'a;
value with_stderr : out_channel -> (unit -> 'a) -> 'a;
(** These procedures install the given channel as the [stdin], [stdout], and
[stderr] channel, respectively, for the duration of a call to their 2d argument. *)

(* p 25. *)
value set_stdin : in_channel -> unit;
value set_stdout : out_channel -> unit;
value set_stderr : out_channel -> unit;
(** These procedures set the standard I/O channels to new values, the old ones being
abandoned in the great bit bucket --- no flush, no close. *)

(** NOTE: The six procedures above don't change the file descriptor associated
to their channel argument, so {i e.g.}, [stdout] may be associated to another
file descriptor than 1.  Use [(out_channel_of_fd 1)] to get a non-side-effected
channel.  So you can go fishin' in the great bit bucket...  [flush_all] will
flush those channels too.  *)

value close_in : in_channel -> bool;
value close_out : out_channel -> bool;
(**
Closing a channel or file descriptor: the 3 procedures around return true if they
closed an open channel/fd (this differs from [Pervasives.close_\{in,out\}]).  If
the channel was already closed, they return false; this is not an error. *)
value close_fd : fd -> bool;
(**
If the [fd] arg to close_fd has a channel allocated to it, the channel is
shifted to a new file descriptor created with [{in,out}_channel_of_dup_fd fd]
before closing the [fd]. The channel then has its revealed count set to zero.
This reflects the design criteria that channels are not associated with file
descriptors, but with open files.

To close a file descriptor, and any associated channel it might have, you
must instead say one of (as appropriate):
{[  close_in (in_channel_of_fd fd)
  close_out (out_channel_of_fd fd)]}
*)

(** These two procedures are used to synchronise Unix' standard I/O
  file descriptors and Caml's current I/O channels. *)

value stdchans_to_stdio : unit -> unit;
(**
This causes the standard I/O file descriptors (0, 1, and 2) to
take their values from the current standard I/O channels.  It is exactly
equivalent to the series of redirections:
{[   fdes_of_dup_in ~newfd:0 stdin;
   fdes_of_dup_out ~newfd:1 stdout;
   fdes_of_dup_out ~newfd:2 stderr ]}
Why not [move_..._to_fdes]? Because [stdout] and [stderr] might be the same channel. *)

value stdio_to_stdchans : unit -> unit;
(**
This causes the bindings of the current standard I/O channels
to be changed to channels constructed over the standard I/O file descriptors.
It is exactly equivalent to the series of assignments:
{[   set_stdin (in_channel_of_fd 0);
   set_stdout (out_channel_of_fd 1);
   set_stderr (out_channel_of_fd 2)]}

However, you are more likely to find the dynamic-extent variant,
[with_stdio_channels], below, to be of use in general programming. *)

(* p 26. *)

value with_stdio_chans : (unit -> 'a) -> 'a;
(** Binds the standard channels [stdin], [stdout], and [stderr] to be channels
  on file descriptors 0, 1, 2, and then calls its 1st argument.
  [with_stdio_chans thunk] is equivalent to:
{[  with_stdin (in_channel_of_fd 0)
    (fun () -> with_stdout (out_channel_of_fd 1)
       (fun () -> with_stderr (out_channel_of_fd 2) thunk))]} *)

(** {3 String channels} *)

(** Ocaml has no string channels, but Cash emulates them with temp files. *)

value make_string_in_channel : string -> in_channel;
(** Returns a channel that reads characters from the supplied string. *)

value make_string_out_channel : unit -> out_channel;
(** A string output channel is a channel that collects the characters given to
it into a string (well, a temp file, in fact). *)

value string_out_channel_output : ?close: bool -> out_channel -> string;
(** The accumulated string is retrieved by applying [string_out_channel_output]
 to the channel.  You can call this even on a closed channel.  However, as the
emulation maintains a hidden input channel on the temp file, you can use a {i
~close:true} argument to close both channels, and free the underlying disk
storage. This will also make the out_channel unrecognised as a string output
channel. *)

value call_with_string_out_channel : ?close: bool -> (out_channel -> unit) -> string;
(** The first arg {i procedure} value is called on a channel.  When it returns,
 [call_with_string_out_channel] returns a string containing the characters that
 were written to that channel during the execution of {i procedure}. *)

(** {3 Revealed channels and file descriptors} *)

(** The material in this section and the following one is not critical for most
applications.  You may safely skim or completely skip this section on a first
reading.

Caml doesn't specify what happens to the file descriptor when a channel is
garbage-collected: is it closed or not ?  In the following discussion, we
suppose the same behaviour as many Scheme implementations which close channels
when they collect them.  Anyway, the same arguments apply when exec'ing another
program.

Dealing with Unix file descriptors in a Caml environment is difficult.  In Unix,
open files are part of the process environment, and are referenced by small
integers called {e file descriptors}. Open file descriptors are the fundamental
way I/O redirections are passed to subprocesses, since file descriptors are
preserved across fork's and exec's.

Caml, on the other hand, uses channels for specifying I/O sources. Channels are
garbage-collected Caml objects, not integers.  When a channel becomes
unreachable, it can be collected (and the associated file descriptor may be
closed).  Because file descriptors are just integers, it's impossible to garbage
collect them --- one wouldn't be able to collect an unreachable channel on file
descriptor 3 unless there were no 3's in the system, and you could further prove
that your program would never again compute a 3. This is difficult at best.

If a Caml program only used Caml channels, and never actually used file
descriptors, this would not be a problem. But Caml code must descend to the file
descriptor level in at least two circumstances:
- when interfacing to foreign code
- when interfacing to a subprocess.
    
This causes a problem. Suppose we have a Caml channel constructed on top of file
descriptor 2. We intend to fork off a program that will inherit this file
descriptor. If we drop references to the channel, the garbage collector could
prematurely close file 2 before we fork the subprocess. The interface described
below is intended to fix this and other problems arising from the mismatch
between channels and file descriptors.

The Caml runtime maintains a list of open channels, from which one can retrieve
the Caml channel allocated for a given file descriptor. Cash imposes the further
restriction that there is at most one open channel for each open file
descriptor.  This is not enforced by the [Unix] module's functions
[{in,out}_channel_of_descr], which will happily allocate a new channel each time
they are called, each with its own buffer, but the system only knows one
position in the file --- the Caml runtime behaviour can be understood, but only
with serious insight (and the sources).  So, for [Cash.{in,out}_channel_of_fd]
to be able to give an unambiguous answer (i.e. the previously opened channel on
this fd, not anyone of those already opened, nor a new one --- except if there
was none ---), you should only use the Cash versions.  In any case, if there are
more than one channel opened on a file descriptor, these functions will signal
an error.  This is nearly the only incompatibility between [Unix] and [Cash]
(the second being that [Unix.open_file] doesn't call [set_close_on_exec]).  *)

(** The channel data structure has one Cash-specific field besides the
descriptor: {i revealed}.  When a channel is closed with [(close_{in,out}
channel)], the channel's file descriptor is closed, it is unlinked from the open
channel list, and the channel's {i descriptor} field is reset to some "no fd"
value.

When a file descriptor is closed with [(close_fd fdes)], any associated channel
is shifted to a new file descriptor created with [({in,out}_channel_of_dup_fd
fdes)].  The channel has its revealed count reset to zero (and hence becomes
eligible for closing on exec or GC). See discussion below.  To really put a
stake through a descriptor's heart without waiting for associated channels to be
closed, you must say one of {[
   close_in (in_channel_of_fd fdes)
   close_out (out_channel_of_fd fdes)
]}

The {i revealed} field is an aid to garbage collection. It is an integer
semaphore. If it is zero, the channel's file descriptor can be closed when the
channel is collected. Essentially, the {i revealed} field reflects whether or
not the channel's file descriptor has escaped to the Caml user. If the Caml user
doesn't know what file descriptor is associated with a given channel, then he
can't possibly retain an ``integer handle'' on the channel after dropping
pointers to the channel itself, so the garbage collector is free to close the
file.

Channels allocated with [open_in] and [open_out] are unrevealed channels --- {i
i.e.}, {i revealed} is initialised to 0.  No one knows the channel's file
descriptor, so the file descriptor can be closed when the channel is collected.

The functions [{in,out}_channel_of_fd] and [fd_of_{in,out}_channel] are used to
shift back and forth between file descriptors and channels.  When
[fd_of_{in,out}_channel] reveals a channel's file descriptor, it increments the
channel's {i revealed} field.  When the user is through with the file
descriptor, he can call [release_{in,out}_channel_handle] {i channel}, which
decrements the count.  The functions [call_with_fdes_{in,out}] {i channel proc}
automate this protocol.  If {i proc} throws out of the [call_with_fdes_...]
application, the exception is caught, the descriptor handle released, then the
exception is re-raised.  When the user maps a file descriptor to a channel with
[{in,out}_channel_of_fd], the channel has its revealed field incremented.

Not all file descriptors are created by requests to make channels.  Some are
inherited on process invocation via [exec(2)], and are simply part of the global
environment. Subprocesses may depend upon them, so if a channel is later
allocated for these file descriptors, is should be considered as a revealed
channel. For example, when the Caml shell's process starts up, it opens channels
on file descriptors 0, 1, and 2 for the initial values of [stdin], [stdout], and
[stderr].  These channels are initialised with {i revealed} set to 1, so that
[stdin], [stdout], and [stderr] are not closed even if the user drops the
channel.
*)(**
Unrevealed file channels have the nice property that they can be closed when all
pointers to the channel are dropped. This can happen during gc, or at an
[exec()] --- since all memory is dropped at an [exec()].  No one knows the
file descriptor associated with the channel, so the exec'd process certainly
can't refer to it.

This facility preserves the transparent may-close-on-collect property for file
channels that are used in straightforward ways, yet allows access to the
underlying Unix substrate without interference from the garbage collector. This
is critical, since shell programming absolutely requires access to the Unix file
descriptors, as their numerical values are a critical part of the process
interface.

A channel's underlying file descriptor can be shifted around with [dup(2)] when
convenient. That is, the actual file descriptor on top of which a channel is
constructed can be shifted around underneath the channel by the Cash runtime
when necessary.  This is important, because when the user is setting up file
descriptors prior to a [exec(2)], he may explicitly use a file descriptor that
has already been allocated to some channel. In this case, the Cash runtime just
shifts the channel's file descriptor to some new location with [dup], freeing up
its old descriptor.  This prevents errors from happening in the following
scenario.  Suppose we have a file open on channel {i f}.  Now we want to run a
program that reads input on file 0, writes output to file 1, errors to file 2,
and logs execution information on file 3. We want to run this program with input
from {i f}.  So we write (in an sh-like syntax,  since Cash pipeline syntax is
not fixed for now --- here, {i $f$} denotes a Caml input channel):
*) (**
[ <<run "/usr/shivers/bin/prog 1>output.txt 2>error.log 3>trace.log 0<]{i $f$}[">>]

Now, suppose by ill chance that, unbeknownst to us, when the operating system
opened {i f}'s file, it allocated descriptor 3 for it. If we blindly redirect
[trace.log] into file descriptor 3, we'll clobber {i f} !  However, the
channel-shuffling machinery saves us: when the [<<run ...>>] form tries to dup
[trace.log]'s file descriptor to 3, [dup] will notice that file descriptor 3 is
already associated with an unrevealed channel ({i i.e.}, {i f}). So, it will
first move {i f} to some other file descriptor. This keeps {i f} alive and well
so that it can subsequently be dup'd into descriptor 0 for [prog]'s stdin.

The channel-shifting machinery makes the following guarantee: a channel is only
moved when the underlying file descriptor is closed, either by a [close()] or a
[dup2()] operation. Also when explicitly asked by {!Cash.with_stdin},
[set_stdout] and consorts. Otherwise a channel/file-descriptor association is
stable.

Under normal circumstances, all this machinery just works behind the scenes to
keep things straightened out. The only time the user has to think about it is
when he starts accessing file descriptors from channels, which he should almost
never have to do. If a user starts asking what file descriptors have been
allocated to what channels, he has to take responsibility for managing this
information.
*)

(** {3 Channel-mapping machinery} *)

(** The procedures provided in this section are almost never needed.
You may safely skim or completely skip this section on a first reading.

Here are the routines for manipulating channels in Cash. The important points to
remember are:
- A channel is associated with an open file, not a particular file descriptor.
- The association between a channel and a particular file descriptor is never
  changed {i except} when requested by [set_std...] or [with_std...]
  (see {!Cash.withstd}), or when the file descriptor is explicitly closed.
  ``Closing'' includes being used as the target of a [dup2], so the set of
  procedures below that close their targets are [close_...], two-argument
  [dup_...], and [move_..._to_fdes]. If the target file descriptor of one of
  these routines has an allocated channel, the channel will be shifted to
  another freshly-allocated file descriptor, and marked as unrevealed, thus
  preserving the channel but freeing its old file descriptor.

These rules are what is necessary to ``make things work out'' with no surprises
in the general case. *)

(* p 30. *)
value in_channel_of_fd : fd -> in_channel;
value out_channel_of_fd : fd -> out_channel;
value fd_of_in_channel : in_channel -> fd;
value fd_of_out_channel : out_channel -> fd;
(** These increment the channel's revealed count. *)

external in_channel_revealed : in_channel -> int = "chan_revealed_count";
external out_channel_revealed : out_channel -> int = "chan_revealed_count";
(** Return the channel's revealed count. *)

external release_in_channel_handle : in_channel -> unit = "release_chan_handle";
external release_out_channel_handle : out_channel -> unit = "release_chan_handle";
(** Decrement the channel's revealed count. *)

(* Scsh uses the inverse order -- don't ask me why I changed it. *)
value call_with_fdes_in : (fd -> 'a) -> in_channel -> 'a;
value call_with_fdes_out : (fd -> 'a) -> out_channel -> 'a;
(** [call_with_fdes_...] {i consumer channel} calls {i consumer} on the file
 descriptor underlying {i channel}; takes care of revealed bookkeeping.
While {i consumer} is running, the {i channel}'s revealed count is incremented. *)

(** Mapping [fd] -> [fd] and [channel] -> [channel]. *)

value move_fd_to_fdes : fd -> fd -> fd;

(** [move_fd_to_fdes] {i fd target-fd}: if [fd] is a file-descriptor not equal
to {i target-fd}, dup it to {i target-fd} and close it. Returns {i target-fd}. *)

value move_in_channel_to_fdes : in_channel -> fd -> in_channel;
value move_out_channel_to_fdes : out_channel -> fd -> out_channel;
(** [move_{in,out}_channel_to_fdes] {i channel target-fd}: {i channel} is
shifted to {i target-fd}, by duping its underlying file-descriptor if necessary.
{i channel}'s original file descriptor is closed (if it was different from {i
target-fd}). Returns the channel.  This operation resets {i channel}'s revealed
count to 1. *)

(** In all cases when {i fd} or {i channel} is actually shifted, if there is a
channel already using {i target-fd}, it is first relocated to some other file
descriptor. *)

(* p 31. *)

(** {3:unixio Unix I/O} *)

(** The 9 next procedures provide the functionality of C's [dup()] and
[dup2()].  The X_of_dup_Y ones convert any fd/channel to any other kind, and
X_of_dup_X is named dup_X.  *)

value dup_fd : ?newfd: fd -> fd -> fd;
value fdes_of_dup_in : ?newfd: fd -> in_channel -> fd;
value fdes_of_dup_out : ?newfd: fd -> out_channel -> fd;

value in_channel_of_dup_fd : ?newfd: fd -> fd -> in_channel;
value dup_in : ?newfd: fd -> in_channel -> in_channel;
value in_channel_of_dup_out : ?newfd: fd -> out_channel -> in_channel;

value out_channel_of_dup_fd : ?newfd: fd -> fd -> out_channel;
value out_channel_of_dup_in : ?newfd: fd -> in_channel -> out_channel;
value dup_out : ?newfd: fd -> out_channel -> out_channel;
(** These procedures use the Unix [dup()] syscall to replicate their fd/channel
last argument.  If a [newfd] file descriptor is given, it is used as the target
of the dup operation, i.e., the operation is a [dup2()].  In this case,
procedures that return a channel (such as [dup_in]) will return one with the
revealed count set to one.  For example, [dup_in ~newfd:5 stdin] produces a new
channel with underlying file descriptor 5, whose revealed count is 1.  If
[newfd] is not specified, then the operating system chooses the file descriptor,
and any returned channel is marked as unrevealed.

If the [newfd] target is given, and some channel is already using that file
descriptor, the channel is first quietly shifted (with another [dup]) to some
other file descriptor (zeroing its revealed count).

Since Caml doesn't provide read/write channels, [{in,out}_channel_of_dup_in] can
be useful for getting an output version of an input channel, or {i vice versa}.
For example, if [p] is an input channel open on a tty, and we would like to do
output to that tty, we can simply use [out_channel_of_dup_in p] to produce an
equivalent output channel for the tty.  However, you are responsible for the
open modes of the channel when doing so. *)

(** Positioning modes for [seek_...] *)
type seek_command =
  Unix.seek_command ==
    [ SEEK_SET  (** positions are relative to the beginning of the file *)
    | SEEK_CUR  (** positions are relative to the current position *)
    | SEEK_END ](** positions are relative to the end of the file *)
;

value seek_fd : ?whence: seek_command -> fd -> int -> int;
value seek_in : ?whence: seek_command -> in_channel -> int -> int;
value seek_out : ?whence: seek_command -> out_channel -> int -> int;
(** Reposition the I/O cursor for a file descriptor or channel.  This gives the
[Unix.lseek] functionality applied to fd's and channels.  Not all such values
are seekable; this is dependent on the OS implementation.  The return value is
the resulting position of the I/O cursor in the I/O stream. *)

value tell_fd : fd -> int;
value tell_in : in_channel -> int;
value tell_out : out_channel -> int;
(** Return the position of the I/O cursor in the the I/O stream.  Not all file
descriptors or channels support cursor-reporting; this is dependent on the OS
implementation. *)

type file_perm = int;

(* p 32. *)
(** The flags to [open_file_...]. *)
type open_flag =
  Unix.open_flag ==
    [ O_RDONLY          (** Open for reading *)                 
    | O_WRONLY          (** Open for writing *)                 
    | O_RDWR            (** Open for reading and writing *)     
    | O_NONBLOCK        (** Open in non-blocking mode *)        
    | O_APPEND          (** Open for append *)                  
    | O_CREAT           (** Create if nonexistent *)            
    | O_TRUNC           (** Truncate to 0 length if existing *) 
    | O_EXCL            (** Fail if existing *)                 
    | O_NOCTTY          (** Don't make this dev a controlling tty *)
    | O_DSYNC           (** Writes complete as `Synchronised I/O data integrity completion' *)
    | O_SYNC            (** Writes complete as `Synchronised I/O file integrity completion' *)
    | O_RSYNC ]         (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)
;

value open_file_out :
  ?perms: file_perm -> string -> list open_flag -> out_channel;
value open_file_in :
  ?perms: file_perm -> string -> list open_flag -> in_channel;
(** [open_file_...] {i ~perms fname flags}: {i perms} defaults to [0o666].
  {i flags} is a list of [open_flag]s.  You must use exactly one of the
  [O_RDONLY], [O_WRONLY], or [O_RDWR] flags.

  Caml do not have input/output channels, so it's one or the other. (You can
  hack simultaneous I/O on a file by opening it R/W, taking the result input
  channel, and duping it to an output channel with [out_channel_of_dup_in].) *)

value open_fdes : ?perms: file_perm -> string -> list open_flag -> fd;
(** Same as [open_file_{in,out}], but returns a file descriptor. *)

value open_input_file : ?flags: (list open_flag) -> string -> in_channel;
value open_output_file :
  ?flags: (list open_flag) -> ?perms: file_perm -> string -> out_channel;
(** These are equivalent to [open_file_...], after adding the
  read/write mode to the [flags] argument to [O_RDONLY] or [O_WRONLY],
  respectively (so don't use them).  [Flags] defaults to [] for
  [open_input_file], and [\[O_CREAT; O_TRUNC\]] for [open_output_file].  These
  procedures are for compatibility with Scsh, and the defaults make the
  procedures backwards-compatible with their Scheme standard unary definitions.
*)

value openfile : string -> list open_flag -> file_perm -> Unix.file_descr;
(** For Cash proper operation, you must use this openfile in place of the Unix
  one; ours calls [Unix.set_close_on_exec] on the returned file_descr, to make
  all the channel-mapping machinery work smoothly. *)

(** The 6 procedures below are for compatibility with Scheme. *)

value with_input_from_file : string -> (unit -> 'a) -> 'a;
value with_output_to_file : string -> (unit -> 'a) -> 'a;
value with_errors_to_file : string -> (unit -> 'a) -> 'a;
(** The file named by the first argument is opened for input or output, an input
 or output channel connected to it is made into [stdin], [stdout] or [stderr]
 (see [with_std...] in {!Cash.withstd}) and the thunk is called.  When it
 returns, the channel is closed and the previous default is restored.  The value
 yielded by the thunk is returned.

 Note: the open functions used are [Pervasives.open_{in,out}].  If you need
 [open_file_{in,out}], it's easy enough to cook up your own wrapper with
 [unwind_protect] and [with_std...]
*)

value call_with_input_file : string -> (in_channel -> 'a) -> 'a;
value call_with_output_file : string -> (out_channel -> 'a) -> 'a;
value call_with_fdes_fn :
  ?perms: file_perm -> string -> list open_flag -> (fd -> 'a) -> 'a;
(** [call_with_...] {i .. filename .. proc} call {i proc} with a channel or fd
  opened on {i filename}.  This channel or fd is closed before returning the
  value yielded by {i proc}.  The open procedures used are
  [Pervasives.open_{in,out}] and {!Cash.open_fdes}. *)

type fdes_flags = [ FD_CLOEXEC ];
value fdes_flags_fd : fd -> list fdes_flags;
value fdes_flags_in : in_channel -> list fdes_flags;
value fdes_flags_out : out_channel -> list fdes_flags;

value set_fdes_flags_fd : fd -> list fdes_flags -> unit;
value set_fdes_flags_in : in_channel -> list fdes_flags -> unit;
value set_fdes_flags_out : out_channel -> list fdes_flags -> unit;
(** These procedures allow reading and writing of an open file's flags.  The
only such flag defined by Posix is [FD_CLOEXEC]; your Unix implementation may
provide others. 

These procedures should not be particularly useful to the programmer, as the
Cash runtime already provides automatic control of the close-on-exec property,
as long as you don't use [Unix.open_file] without using [Unix.set_close_on_exec]
immediately ({!Cash.openfile} does it for you) -- this is the second
incompatibility with [Unix] (the first being [Unix.{in,out}_channel_of_descr]).
Unrevealed channels always have their file descriptors marked close-on-exec, as
they can be closed when the Cash process execs a new program.  Whenever the user
reveals or unreveals a channel's file descriptor, the runtime automatically sets
or clears the flag for the programmer.  Programmers that manipulate this flag
should be aware of these extra, automatic operations. *)

(* p 33. *)
value fdes_status_fd : fd -> list open_flag;
value fdes_status_in : in_channel -> list open_flag;
value fdes_status_out : out_channel -> list open_flag;

value set_fdes_status_fd : fd -> list open_flag -> unit;
value set_fdes_status_in : in_channel -> list open_flag -> unit;
value set_fdes_status_out : out_channel -> list open_flag -> unit;
(** These procedures allow reading and writing of an open file's status flags
 (see table below).  Note that this file-descriptor state is shared between file
descriptors created by [dup_..] (and [fork...]) --- if you create channel {i b}
by applying [dup_...] to channel {i a}, and change {i b}'s status flags, you
will also have changed {i a}'s status flags. *)

(** Status flags for [open_file_...], [fdes_status_...] and
[set_fdes_status_...]. *)

(* The [O_ACCMODE] value is not an actual flag, but a bit mask used to select
the field for the [O_RDONLY], [O_WRONLY] and [O_RDWR] bits. *)
(**
{v
                Allowed operations              Status flag
                --------------------------------------------------------------
Open+Get+Set    These flags can be used         O_APPEND
                in open_file_...,               O_NONBLOCK
                fdes_status_... and             O_SYNC          (...SYNC aren't
                set_fdes_status_... calls       O_DSYNC         impl. in 3.04)
                                                O_RSYNC
                --------------------------------------------------------------
Open+Get        These flags can be used         O_RDONLY
                in open_file_... and            O_WRONLY
                fdes_status_... calls, but are  O_RDWR
                ignored by set_fdes_status_... 
                --------------------------------------------------------------
Open            These flags are only relevant   O_CREAT
                for open_file_... calls; they   O_EXCL
                are ignored by fdes_status_...  O_NOCTTY       (not impl. in 3.04)
                and set_fdes_status_... calls.  O_TRUNC
v}
*)

value pipe : unit -> (in_channel * out_channel);
(** Returns two channels, the read and write end-points of a Unix pipe. *)

(** [fold_input] {i f init reader source} is a folder in the sort of
  List.fold_left, but instead of folding a 'a list, you give it a {i reader}
  function (such as [read_line]), and a {i source} (as [stdin]).  The elements
  to fold are computed by applying {i read_line} to the {i source}. For
  directories, see {!Cash.fold_directory} and {!Cash.directory_files}. *)
value fold_input : ('a -> 'b -> 'a) -> 'a -> ('c -> 'b) -> 'c -> 'a;

(** Note about the [read_string...] procedures: the ones with a [?src:fd]
argument (default to fd 0, and) directly use [Unix.read].  But those with a
[?src:in_channel] argument (default to [stdin], and) use [Pervasives.input];
this is to permit seamless mixing of calls to these procedures and Pervasives
I/O operations.  For large blocks of data, it might be more efficient to use
[read_string ~src:(fd_of_in_channel chan)], since there's no buffering, but
mixing this with Pervasives may require synchronization ([seek_in]) which is to
your charge.  Similar considerations apply to write_string... *)

type error_packet =
  [ Sys__error of string
  | Unix__error of (Unix.error * string * string) ]
;

exception
  String_io_error of (error_packet * string * string * int * int * int);
(** This is the exception raised by the
read_string.../write_string... procedures.  It contains the original
error_packet, the name of the procedure, the string the operation has been
attempted on, the start index, the index upto which data has already been
read/written, the end_ index. *)

value read_string : ?src: fd -> int -> string;
value read_string_in : ?src: in_channel -> int -> string;
value read_string_bang : ?src: fd -> ?start: int -> ?end_: int -> string -> int;
value read_string_bang_in :
  ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;
(** These calls read exactly as much data as you requested, unless there is not
enough data (eof).  [read_string_bang...] {i str} reads the data into string {i
str} at the indices in the half-open interval \[{i start},{i end_}); the default
interval is the whole string: {i start} [= 0] and {i end_} [= String.length] {i
str}.  They will persistently retry on partial reads and when interrupted until
(1) error, (2) eof, or (3) the input request is completely satisfied.  Partial
reads can occur when reading from an intermittent source, such as a pipe or tty.

[read_string{,in}] returns the string read; [read_string_bang...] returns the
number of characters read. They both raise [End_of_file] at eof.  A request to
read zero bytes returns immediately, with no eof check.

The values of [start] and [end_] must specify a well-defined interval in [str],
i.e., [0 <= start <= end_ <= String.length str].

Any partially-read data is included in the error exception packet.  Error
returns on non-blocking input are considered an error.  *)

(* p 34. *)

value read_string_partial : ?src: fd -> int -> string;
value read_string_partial_in : ?src: in_channel -> int -> string;
value read_string_bang_partial : ?src: fd -> ?start: int -> ?end_: int -> string -> int;
value read_string_bang_partial_in :
  ?src: in_channel -> ?start: int -> ?end_: int -> string -> int;
(** These are atomic best-effort/forward-progress calls.  Best effort: they may
read less than you request if there is a lesser amount of data immediately
available ({i e.g.}, because you are reading from a pipe or a tty).  Forward
progress: if no data is immediately available ({i e.g.}, empty pipe), they will
block.  Therefore, if you request an {i n > 0} byte read, while you may not get
everything you asked for, you will always get something (barring eof).

There is one case in which the forward-progress guarantee is cancelled: when the
programmer explicitly sets the channel to non-blocking i/o.  In this case, if no
data is immediately available, the procedure will not block, but will
immediately return a zero-byte read.

[read_string_partial{,in}] reads the data into a freshly allocated string, which
it returns as its value.  [read_string_bang_partial...] {i str} reads the data
into string {i str} at the indices in the half-open interval \[[start],[end_]);
the default interval is the whole string: [start = 0] and [end_ = String.length
string].  It returns the number of bytes read.

The values of [start] and [end_] must specify a well-defined interval in [str],
i.e., [0 <= start <= end_ <= String.length str].

A request to read zero bytes returns immediatedly, with no eof check.

In sum, there are only three ways you can get a zero-byte read: (1) you request
one, (2) you turn on non-blocking i/o, or (3) you try to read at eof (but then
End_of_file is raised).

These are the routines to use for non-blocking input.  They are also useful when
you wish to efficiently process data in large blocks, and your algorithm is
insensitive to the block size of any particular read operation.  *)

value write_string : ?dst: fd -> ?start: int -> ?end_: int -> string -> unit;
value write_string_out : ?dst: out_channel -> ?start: int -> ?end_: int -> string -> unit;
(** These procedures write all the data requested. 
 If the procedure cannot perform the write with a single kernel call (due to
interrupts or partial writes), it will perform multiple write operations until
all the data is written or an error has occurred.  A non-blocking i/o error is
considered an error.  (Error exception packets for this syscall include the
amount of data partially transferred before the error occurred.)

In [write_string...] {i str}, the data written are the characters of the string
{i str} in the half-open interval \[{i start},{i end_}).  The default interval
is the whole string: {i start} [= 0] and {i end_} [= String.length string].  The
values of [start] and [end_] must specify a well-defined interval in [str],
i.e., [0 <= start <= end_ <= String.length str].

A zero-byte write returns immediately, with no error.

Output to buffered channels: [write-string]'s efforts end as soon as all the
data has been placed in the output buffer.  Errors and true output may not
happen until a later time, of course. *)

value write_string_partial : ?dst: fd -> ?start: int -> ?end_: int -> string -> int;
value write_string_partial_out :
  ?dst: out_channel -> ?start: int -> ?end_: int -> string -> int;
(** These routines are the atomic best-effort/forward-progress analog to
[write_string...]. They return the number of bytes written, which may be less
than you asked for.  Partial writes can occur when (1) we write off the physical
end of the media, (2) the write is interrrupted, or (3) the file descriptor is
set for non-blocking i/o.

If the file descriptor is not set up for non-blocking i/o, then a successful
return from these procedures makes a forward progress guarantee --- that is, a
partial write took place of at least one byte:
- If we are at the end of physical media, and no write takes place, an error
  exception is raised.  So a return implies we wrote {e something}.
- If the call is interrupted after a partial transfer, it returns immediately.
  But if the call is interrupted before any data transfer, then the write is
  retried.

If we request a zero-byte write, then the call immediately returns 0.  If the
file descriptor is set for non-blocking i/o, then the call may return 0 if it
was unable to immediately write anything ({i e.g.}, full pipe).  Barring these
two cases, a write either returns {i nwritten} > 0, or raises an error
exception.

Contrarily to Scsh, non-blocking i/o is also available on buffered channels.  Doing
non-blocking i/o to a buffered channel is well-defined: if a Sys_blocked_io
encapsulated exception is raised, the bufuer is full and writing one character
would block.  *)

(** The kind of things you can ask to [select]. *)
type selectable =
  [ Nothing
  | Read_in of in_channel
  | Read_fd of fd
  | Write_out of out_channel
  | Write_fd of fd
  | Except_in of in_channel
  | Except_fd of fd ]
;

value select_bang : ?timeout:float -> array selectable -> (int * int * int);
value select : ?timeout:float -> array selectable -> array selectable;
(** The [select] procedure allows a process to block and wait for events on
    multiple I/O channels. 
The {i selectable} argument is an array of [Read_in] or [Except_in] input
channels and [Read_fd] or [Except_fd] integer file descriptors, and [Write_out]
output channels and [Write_fd] integer file descriptors.  The procedure returns
an array whose elements are a subset of the array argument.  In this result
array, every [Read_in] or [Read_fd] element of is ready for input; every
[Write_out] or [Write_fd] element is ready for output; every [Except_in] or
[Except_fd] element has an exceptional condition pending.

The [select] call will block until at least one of the I/O channels passed to it
is ready for operation.  The {i timeout} value can be used to force the call to
time-out after a given number of seconds. Its default means wait indefinitely. A
zero value can be used to poll the I/O channels.

If an Unix I/O channel appears more than once in the selectable argument ---
perhaps occuring once as a Caml channel, and once as the channel's underlying
integer file descriptor --- only one of these two references may appear in the
returned vector.  Buffered I/O channels are handled specially --- if an input
channel's buffer is not empty, or an output channel's buffer is not yet full,
then these channels are immediately considered eligible for I/O without using
the actual, primitive [select] system call to check the underlying file
descriptor.  This works pretty well for buffered input channels, but is a little
problematic for buffered output channels.

The [select_bang] procedure is similar, but indicates the subset of active I/O
channels by side-effecting the argument array.  Non-active I/O channels in the
argument array are overwritten with [Nothing] values.

The call returns the number of active elements remaining in the array.  As a
convenience, the vectors passed in to [select_bang] are allowed to contain
[Nothing] values as well as integers and channels.

{i Remark:} I (Olin) have found the [select_bang] interface to be the more
useful of the two. After the system call, it allows you to check a specific I/O
channel in constant time.  *)

(** {3 Buffered I/O} *)

(** Caml channels use buffered I/O --- data is transferred to or from the OS in
blocks. Cash provides control of this mechanism: the programmer may force
saved-up output data to be transferred to the OS when he chooses, and may also
choose which I/O buffering policy to employ for a given channel (or turn
buffering off completely).

It can be useful to turn I/O buffering off in some cases, for example when an
I/O stream is to be shared by multiple subprocesses.  For this reason, Cash
allocates an unbuffered channel for file descriptor 0 at start-up time.  Because
shells frequently share stdin with subprocesses, if the shell does buffered
reads, it might ``steal'' input intended for a subprocess.  For this reason, all
shells, including sh, csh, scsh and cash, read stdin unbuffered.  Applications
that can tolerate buffered input on stdin can reset [stdin] to block buffering
for higher performance.

There are three buffering policies that may be chosen: *)

type bufpolicy =
  [ Block(** General block buffering (general default). *)
  | Line(** Line buffering (tty default). *)
  | Nobuf ](** Direct I/O --- no buffering. *)
;

(** The line buffering policy flushes output whenever a newline is output;
whenever the buffer is full; or whenever an input is read from stdin.  Line
buffering is the default for channels open on terminal devices.  {i Oops:
Pervasives I/O implementation doesn't support it, so line buffering is not
implemented.}  *)

value set_chan_buffering_in : in_channel -> ?size: int -> bufpolicy -> unit;
value set_chan_buffering_out : out_channel -> ?size: int -> bufpolicy -> unit;
(** [set_chan_buffering_...] {i channel} [~size:]{i size policy} allows the
programmer to assign a particular I/O buffering policy to a channel, and to
choose the size of the associated buffer.

The {i size} argument requests an I/O buffer of {i size} bytes.  If not given, a
reasonable default is used; if given and zero, buffering is turned off (i.e.,
[~size:0] for any policy is equivalent to {i policy} = [Nobuf]).

Implementation notes: you can't set a size lower than the actual contents of the
buffer; so you may have to flush out_channels, or only use it on new
in_channels, i.e., before I/O is performed on the channel.  You can't set a size
higher than the current standard size (4 Kb) yet.  The [Nobuf] policy is
emulated by a buffer of size 1.  With Ocaml 3.04, [set_chan_buffering_in] is
ineffective. *)

value force_output : out_channel -> unit;
(** This procedure flushes buffered output, and raises a write-error exception on error. *)
value flush_all_chans : unit -> unit;
(** This procedure flushes all open output channels with buffered data. *)

(* XXX lock. *)

(** {2 File system} *)

(** Besides the following procedures, which allow access to the computer's file
system, Cash also provides a set of procedures which manipulate file {i names}.
These string-processing procedures are documented in section {!Cash.filenames}.
*)

(* p 40. *)
type override =
  [ Don't
  | Delete
  | Query ]
;

value create_directory : ?perms: file_perm -> ?override: override -> string -> unit;
value create_fifo : ?perms: file_perm -> ?override: override -> string -> unit;
value create_hard_link : ?override: override -> string -> string -> unit;
value create_symlink : ?override: override -> string -> string -> unit;
(** These procedures create objects of various kinds in the file system.

The {i ~override} argument controls the action if there is already an object in
the file system with the new name:
- [Don't] (override): signal an error (default)
- [Query]: prompt the user
- [Delete]: delete the old object (with [delete_file] or [delete_directory,] as
  appropriate) before creating the new object.

{i Perms} defaults to [0o777] (but is masked by the current umask).

Note: currently, if you try to create a hard or symbolic link from a file to
itself, you will error out with {i ~override} [Don't], and simply delete your
file with {i ~override} [Delete]. Catching this will require some sort of
true-name procedure, which Cash (nor Scsh) currently do not have.  *)

value delete_file : string -> unit;
value delete_directory : string -> unit;
value delete_filesys_object : string -> bool;
(** These procedures delete objects from the file system.  The
[delete_filesys_object] procedure will delete an object of any type from the
file system: files, (empty) directories, symlinks, fifos, {i etc.}

If the object being deleted doesn't exist, [delete_directory] and [delete_file]
raise an error, while [delete_filesys_object] simply returns. *)

value read_symlink : string -> string;
(** Return the filename referenced by symbolic link [fname]. *)

value rename_file : ?override: override -> string -> string -> unit;
(** When using [rename_file] {i old_fname new_fname}, if you override an
existing object, then {i old_fname} and {i new_fname} must type-match --- either
both directories, or both non-directories.  This is required by the semantics of
Unix [rename()].

Note: there is an unfortunate atomicity problem with the [rename_file]
procedure: if you specify [~override:Don't], but create file {i new_fname}
sometime between [rename_file]'s existence check and the actual rename
operation, your file will be clobbered with {i old_fname}. There is no way to
fix this problem, given the semantics of Unix [rename()]; at least it is highly
unlikely to occur in practice. *)

(* p 41. *)
value set_file_mode_fn : string -> file_perm -> unit;
value set_file_mode_fd : fd -> file_perm -> unit;
value set_file_mode_in : in_channel -> file_perm -> unit;
value set_file_mode_out : out_channel -> file_perm -> unit;

value set_file_owner_fn : string -> int -> unit;
value set_file_owner_fd : fd -> int -> unit;
value set_file_owner_in : in_channel -> int -> unit;
value set_file_owner_out : out_channel -> int -> unit;

value set_file_group_fn : string -> int -> unit;
value set_file_group_fd : fd -> int -> unit;
value set_file_group_in : in_channel -> int -> unit;
value set_file_group_out : out_channel -> int -> unit;
(** These procedures set the permission bits, owner id, and group id of a file,
respectively.  The file can be specified by using a file name: [set_file_..._fn]
{i filename}, or either an integer file descriptor: [set_file_..._fd] {i fd} or
a channel: [set_file_..._{in,out}] {i channel} open on the file.  Setting file
user ownership usually requires root privileges. *)

value set_file_times : ?times: (float * float) -> string -> unit;
(** This procedure sets the access and modified times for the file to the
supplied values (see around {! Cash.date} for the Cash representation of time).
If the {i ~times} argument is not supplied, they are both taken to be the
current time.  You must provide both times or neither.  If the procedure
completes successfully, the file's time of last status-change ([ctime]) is set
to the current time. *)

value sync_file_fd : int -> unit;
value sync_file_out : out_channel -> unit;
value sync_file_system : unit -> unit;
(** Calling [sync_file_...] causes Unix to update the disk data structures for a
given file.  For [sync_file_out], any buffered data the channel may have is first
flushed.  Calling [sync_file_system] synchronises the kernel's entire file
system with the disk.

These procedures are not Posix.  Interestingly enough, [sync_file_system]
doesn't actually do what it is claimed to do.  We just threw it in for humor
value.  See the [sync(2)] man page for Unix enlightenment. *)

value truncate_file_fn : string -> int -> unit;
value truncate_file_fd : fd -> int -> unit;
value truncate_file_in : in_channel -> int -> unit;
value truncate_file_out : out_channel -> int -> unit;
(** [truncate_file_...] {i fname/file descriptor/channel len} truncates the
    specified file is to {i len} bytes in length. *)

type file_kind =
  Unix.file_kind ==
    [ S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK ]
;
type file_info =
  Unix.stats ==
    { st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int;
      st_atime : float;
      st_mtime : float;
      st_ctime : float }
;
(** What is returned by [file_info_...].  An alias for [Unix.stats]. *)

value file_info_fn : ?chase: bool -> string -> file_info;
value file_info_fd : fd -> file_info;
value file_info_in : in_channel -> file_info;
value file_info_out : out_channel -> file_info;
(** The [file_info_...] procedures return a record structure containing
everything there is to know about a file. [file_info_fn] takes a {i ~chase}
flag; if it's is true (the default), then the procedure chases symlinks and
reports on the files to which they refer. If {i ~chase} is false, then the
procedure checks the actual file itself, even if it's a symlink. *)

(* p 42. *)

(** The following procedures all return selected information about a file; they
are built on top of [file_info_...], and are called with the same arguments that
are passed to it.  *)

value file_type_fn : ?chase: bool -> string -> file_kind;
value file_type_fd : fd -> file_kind;
value file_type_in : in_channel -> file_kind;
value file_type_out : out_channel -> file_kind;
(** Return the type of fn/fd/channel. *)

value file_inode_fn : ?chase: bool -> string -> int;
value file_inode_fd : fd -> int;
value file_inode_in : in_channel -> int;
value file_inode_out : out_channel -> int;
(** Return the inode of fn/fd/channel. *)

value file_mode_fn : ?chase: bool -> string -> file_perm;
value file_mode_fd : fd -> file_perm;
value file_mode_in : in_channel -> file_perm;
value file_mode_out : out_channel -> file_perm;
(** Return the mode bits (permissions, setuid, setgid) of fn/fd/channel. *)

value file_nlinks_fn : ?chase: bool -> string -> int;
value file_nlinks_fd : fd -> int;
value file_nlinks_in : in_channel -> int;
value file_nlinks_out : out_channel -> int;
(** Return the number of hard links to this fn/fd/channel. *)

value file_owner_fn : ?chase: bool -> string -> int;
value file_owner_fd : fd -> int;
value file_owner_in : in_channel -> int;
value file_owner_out : out_channel -> int;
(** Return the owner of fn/fd/channel. *)

value file_group_fn : ?chase: bool -> string -> int;
value file_group_fd : fd -> int;
value file_group_in : in_channel -> int;
value file_group_out : out_channel -> int;
(** Return the group id of fn/fd/channel. *)

value file_size_fn : ?chase: bool -> string -> int;
value file_size_fd : fd -> int;
value file_size_in : in_channel -> int;
value file_size_out : out_channel -> int;
(** Return the size in bytes of fn/fd/channel. *)

value file_last_access_fn : ?chase: bool -> string -> float;
value file_last_access_fd : fd -> float;
value file_last_access_in : in_channel -> float;
value file_last_access_out : out_channel -> float;
(** Return the time of last access of fn/fd/channel. *)

value file_last_mod_fn : ?chase: bool -> string -> float;
value file_last_mod_fd : fd -> float;
value file_last_mod_in : in_channel -> float;
value file_last_mod_out : out_channel -> float;
(** Return the time of last modification of fn/fd/channel. *)

value file_last_status_change_fn : ?chase: bool -> string -> float;
value file_last_status_change_fd : fd -> float;
value file_last_status_change_in : in_channel -> float;
value file_last_status_change_out : out_channel -> float;
(** Return the time of last status change of fn/fd/channel. *)

(** Example
{[      (* All my files in /usr/tmp: *)
        with_cwd "/usr/tmp"
          (fun () -> List.filter (fun f -> file_owner_fn f = user_uid())
            (directory_files "."))
]} *)

(* p 43. *)

(** The following procedures are file-type predicates that test the type of a
given file.  They are applied to the same arguments to which [file_info_...] is
applied; the sole exception is [file_symlink_fn], which does not take the
optional {i chase} second argument.  For example,
 {[       is_file_directory_fn "/usr/dalbertz"          => true]} *)

value is_file_directory_fn : ?chase: bool -> string -> bool;
value is_file_directory_fd : fd -> bool;
value is_file_directory_in : in_channel -> bool;
value is_file_directory_out : out_channel -> bool;

value is_file_fifo_fn : ?chase: bool -> string -> bool;
value is_file_fifo_fd : fd -> bool;
value is_file_fifo_in : in_channel -> bool;
value is_file_fifo_out : out_channel -> bool;

value is_file_regular_fn : ?chase: bool -> string -> bool;
value is_file_regular_fd : fd -> bool;
value is_file_regular_in : in_channel -> bool;
value is_file_regular_out : out_channel -> bool;

value is_file_socket_fn : ?chase: bool -> string -> bool;
value is_file_socket_fd : fd -> bool;
value is_file_socket_in : in_channel -> bool;
value is_file_socket_out : out_channel -> bool;

value is_file_special_fn : ?chase: bool -> string -> bool;
value is_file_special_fd : fd -> bool;
value is_file_special_in : in_channel -> bool;
value is_file_special_out : out_channel -> bool;

value is_file_symlink_fn : string -> bool;
value is_file_symlink_fd : fd -> bool;
value is_file_symlink_in : in_channel -> bool;
value is_file_symlink_out : out_channel -> bool;

(** The following [is_file_not_...] procedures return this accessibility
information about a named file/file descriptor/channel. *)
type accessibility =
  [ Accessible(** Access permitted. *)
  | Unaccessible(** Can't stat --- a protected directory is blocking access. *)
  | Permission(** Permission denied. *)
  | No_directory(** Some directory doesn't exist. *)
  | Nonexistent ](** File doesn't exist. *)
;

(** A file is considered writeable if either (1) it exists and is writeable or
(2) it doesn't exist and the directory is writeable.  Since symlink permission
bits are ignored by the filesystem, these calls do not take a {i chase} flag.

Note that these procedures use the process' {e effective} user and group ids for
permission checking.  Posix defines an [access()] function that uses the
process' real uid and gids. This is handy for setuid programs that would like to
find out if the actual user has specific rights; Cash ought to provide this
functionality (but doesn't at the current time).  

There are several problems with these procedures. First, there's an atomicity
issue. In between checking permissions for a file and then trying an operation
on the file, another process could change the permissions, so a return value
from these functions guarantees nothing. Second, the code special-cases
permission checking when the uid is root --- if the file exists, root is assumed
to have the requested permission.  However, not even root can write a file that
is on a read-only file system, such as a CD ROM. In this case,
[is_file_not_writable_...] will lie, saying that root has write access, when in
fact the opening the file for write access will fail.  Finally, write permission
confounds write access and create access.  These should be disentangled.

Some of these problems could be avoided if Posix had a real-uid variant of the
[access()] call we could use, but the atomicity issue is still a problem. In the
final analysis, the only way to find out if you have the right to perform an
operation on a file is to try and open it for the desired operation. These
permission-checking functions are mostly intended for script-writing, where
loose guarantees are tolerated.  *)

value is_file_not_readable_fn : string -> accessibility;
value is_file_not_readable_fd : fd -> accessibility;
value is_file_not_readable_in : in_channel -> accessibility;
value is_file_not_readable_out : out_channel -> accessibility;

value is_file_not_writable_fn : string -> accessibility;
value is_file_not_writable_fd : fd -> accessibility;
value is_file_not_writable_in : in_channel -> accessibility;
value is_file_not_writable_out : out_channel -> accessibility;

value is_file_not_executable_fn : string -> accessibility;
value is_file_not_executable_fd : fd -> accessibility;
value is_file_not_executable_in : in_channel -> accessibility;
value is_file_not_executable_out : out_channel -> accessibility;

(* p 44. *)

(** The following [is_file_...] procedures are the logical negation of the
preceding [is_file_not_...]  procedures. Refer to them for a discussion of their
problems and limitations. *)

value is_file_readable_fn : string -> bool;
value is_file_readable_fd : fd -> bool;
value is_file_readable_in : in_channel -> bool;
value is_file_readable_out : out_channel -> bool;

value is_file_writable_fn : string -> bool;
value is_file_writable_fd : fd -> bool;
value is_file_writable_in : in_channel -> bool;
value is_file_writable_out : out_channel -> bool;

value is_file_executable_fn : string -> bool;
value is_file_executable_fd : fd -> bool;
value is_file_executable_in : in_channel -> bool;
value is_file_executable_out : out_channel -> bool;

(** What the following [file_not_exists_...] return.*)
type existing =
  [ Existing(** Exists. *)
  | Unexisting(** Doesn't exist. *)
  | Search_denied](** Some protected directory is blocking the search. *)
;

value file_not_exists_fn : ?chase: bool -> string -> existing;
value file_not_exists_fd : fd -> existing;
value file_not_exists_in : in_channel -> existing;
value file_not_exists_out : out_channel -> existing;

(** The logical negations of the preceding functions. *)

value is_file_existing_fn : ?chase: bool -> string -> bool;
value is_file_existing_fd : fd -> bool;
value is_file_existing_in : in_channel -> bool;
value is_file_existing_out : out_channel -> bool;

(* p 45. *)

(** {2 Directories, globbing and temp files} *)

(** [fold_directory] folds the file names of a directory in the same way as
  {!Cash.fold_input}, {e except} `[.]' and `[..]'. *)
value fold_directory : ('a -> string -> 'a) -> 'a -> string -> 'a;

value directory_files : ?dot_files: bool -> string -> list string;
(** [directory_files] {i dir} return the list of files in directory {i dir}.
  The {i ~dot_files} flag (default false) causes dot files to be included in the
  list.  Regardless of the value of {i ~dot_files}, the two files [.] and [..]
  are {e never} returned.

  The directory {i dir} is not prepended to each file name in the result
  list. That is, [directory_files "/etc"] returns
   {[    \["chown"; "exports"; "fstab"; ...\]]} {e not}
   {[    \["/etc/chown"; "/etc/exports"; "/etc/fstab"; ...\]]}

  To use the files in returned list, the programmer can either manually prepend
  the directory:
    {[    List.map (fun f -> file_name_as_directory dir ^ f) (directory_files dir) ]}
  or cd to the directory before using the file names:
    {[    with_cwd dir (fun () -> List.iter delete_file (directory_files ".")) ]}
  or use the [glob] procedure, defined below.

  A directory list can be generated by [run_with_strings (fun () -> exec_path
  "ls" [])], but this is unreliable, as filenames with whitespace in their
  names will be split into separate entries. Using [directory_files] is
  reliable.
*)

value glob : list string -> list string;
(** [glob \[]{i patterns}[\]] glob each pattern against the filesystem and
return the sorted list.  Duplicates are not removed. Patterns matching nothing
are not included literally. (Why bother to mention such a silly possibility?
Because that is what sh does.)  C shell \{a,b,c\} patterns are
expanded. Backslash quotes characters, turning off the special meaning of \{,
\}, *, \[, \] and ?.

Note that the rules of backslash for Caml strings and glob patterns work
together to require four backslashes in a row to specify a single literal
backslash. Fortunately, it is very rare that a backslash occurs in a Unix file
name.

A glob subpattern will not match against dot files unless the first character of
the subpattern is a literal ``[.]''.  Further, a dot subpattern will not match
the files [.] or [..] unless it is a constant pattern, as in [glob "../*/*.c"].
So a directory's dot files can be reliably generated with the simple glob
pattern [".*"]. Some examples:
{[  (* All the C and #include files in my directory. *)
    glob ["*.c"; "*.h"] ]}
{[  (* All the C files in this directory and its immediate subdirectories. *)
    glob ["*.c"; "*/*.c"] ]}
{[  (* All the C files in the lexer and parser dirs. *)
    glob ["lexer/*.c"; "parser/*.c"]
    glob ["{lexer,parser}/*.c"] ]}
{[  (* All the C files in the strange directory "{lexer,parser}". *)
    glob "\\{lexer,parser\\}/*.c"] ]}
{[  (* All the files ending in "*", e.g., ["foo*"; "bar*"] *)
     glob ["*\\*"] ]}
{[   (* All files containing the string "lexer",
      e.g., ["mylexer.c"; "lexer1.notes"] *)
     glob ["*lexer*"] ]}
{[   (* Either ["lexer"] or []. *)
     glob ["lexer"] ]}

If the first character of the pattern (after expanding braces) is a slash, the
search begins at root; otherwise, the search begins in the current working
directory.

If the last character of the pattern (after expanding braces) is a slash, then
the result matches must be directories, {i e.g.},
{[    glob ["/usr/man/man?/"]          => ["/usr/man/man1/"; "/usr/man/man2/"; ...] ]}

Globbing can sometimes be useful when we need a list of a directory's files
where each element in the list includes the pathname for the file.  Compare:
{[    directory_files "../include/*"   => ["cig.h"; "decls.h"; ...]
    glob ["../include/*"]      => ["../include/cig.h"; "../include/decls.h"; ...]
]}
*)

(* p 47. *)
value glob_quote : string -> string;
(** [glob_quote] {i str} returns a constant glob pattern that exactly matches {i
str}.  All wild-card characters in {i str} are quoted with a backslash. *)

type file_match_pattern =
  [ String_pat of string
  | Regexp_pat of Pcre.regexp
  | Predicate_pat of string -> bool ]
;

value file_match : ?dot_files: bool -> string -> list file_match_pattern -> list string;
(** [file_match] {i root \[pat}{_ 1}{i ; pat}{_ 2}{i ; ...\]} provides a more
    powerful file-matching service, at the expense of a less convenient
    notation. It is intermediate in power between most shell matching machinery
    and recursive [find(1)].

Each [String_pat] or [Regexp_pat] pattern is a regexp. The procedure searches
from {i root}, matching the first-level files against pattern {i pat}{_ 1}, the
second-level files against {i pat}{_ 2}, and so forth.  The list of files
matching the whole path pattern is returned, in sorted order.

The files [.] and [..] are never matched. Other dot files are only matched if
the {i ~dot_files} argument is [true].

A given {i pat}{_ i} pattern is matched as a regexp, so it is not forced to
match the entire file name. E.g., pattern ["t"] matches any file containing a
``t'' in its name, while pattern ["^t$"] matches only a file whose entire name
is ``[t]''.

 The {i pat}{_ i} patterns can be more general than stated above.
- A single pattern can specify multiple levels of the path by embedding [/]
  characters within the pattern. For example, the pattern ["a/b/c"] gives a
  match equivalent to the list of patterns ["a"; "b"; "c"].

- A [Predicate_pat] pattern is a procedure, which is used as a match
  predicate. It will be repeatedly called with a candidate file-name to
  test. The file-name will be the entire path accumulated. If the procedure
  raises an error condition, [file_match] will catch the error and treat it as a
  failed match. This keeps [file_match] from being blown out of the water by
  applying tests to dangling symlinks and other similar situations.

Some examples:

{[ file_match "/usr/lib" [String_pat "m$"; String_pat "^tab"]
     => ["/usr/lib/term/tab300"; "/usr/lib/term/tab300-12"; ...] ]}
{[ file_match "." [String_pat "^lex|parse|codegen$"; String_pat "\\\\.c$"]
     => ["lex/lex.c"; "lex/lexinit.c"; "lex/test.c"; "parse/actions.c";
     "parse/error.c"; "parse/test.c"; "codegen/io.c"; "codegen/walk.c"] ]}
{[ file_match "."  [String_pat "^lex|parse|codegen$/\\\\.c$")
     => (* The same. *) ]}
{[ file_match "." [Predicate_pat is_file_directory_fn]
     => (* All subdirs of the current directory. *) ]}
{[ file_match "/" [Predicate_pat is_file_directory_fn]
    => ["/bin"; "/dev"; "/etc"; "/tmp"; "/usr"]
       (* All subdirs of root. *) ]}
{[ file_match "."  [String_pat "\\\\.c"]
    => (* All the C files in my directory. *) ]}
{[ let ext extension = fun fn -> String_13.has_suffix fn extension in
 let trew _ -> true in
 file_match "." [String_pat "./\\\\.c"];
 file_match "." [String_pat ""; String_pat "\\\\.c"];
 file_match "." [Predicate_pat trew; String_pat "\\\\.c"];
 file_match "." [Predicate_pat trew; Predicate_pat (ext ".c")]
    => (* All the C files of all my immediate subdirs. *) ]}
{[ file_match "." ["lexer"]
    => ["mylexer.c"; "lexer.notes"]
       (* Compare with glob ["lexer"], above. *) ]}
    
Note that when {i root} is the current working directory (["."]), when it is
converted to directory form, it becomes [""], and doesn't show up in the result
file-names.

It is regrettable that the regexp wild card char, ``[.]'', is such an important
file name literal, as dot-file prefix and extension delimiter.
*)

(* p 49. *)
value create_temp_file : ?prefix: string -> unit -> string;
(** [create_temp_file ()] creates a new temporary file and return its name.
  The optional argument specifies the filename prefix to use, and defaults to
  ["/usr/tmp/]{i pid}["], where {i pid} is the current process' id.  The
  procedure generates a sequence of filenames that have {i ~prefix} as a common
  prefix, looking for a filename that doesn't already exist in the file
  system. When it finds one, it creates it, with permission [0o600] and returns
  the filename. (The file permission can be changed to a more permissive
  permission with [set_file_mode] after being created).

This file is guaranteed to be brand new. No other process will have it
open. This procedure does not simply return a filename that is very likely to be
unused. It returns a filename that definitely did not exist at the moment
[create_temp_file] created it.

It is not necessary for the process' pid to be a part of the filename for the
uniqueness guarantees to hold. The pid component of the default prefix simply
serves to scatter the name searches into sparse regions, so that collisions
are less likely to occur. This speeds things up, but does not affect
correctness.

Security note: doing i/o to files created this way in [/usr/tmp/] is not
necessarily secure. General users have write access to [/usr/tmp/], so even if
an attacker cannot access the new temp file, he can delete it and replace it
with one of his own. A subsequent open of this filename will then give you his
file, to which he has access rights. There are several ways to defeat this
attack,
- Use [temp_file_iterate], below, to return the file descriptor allocated when
  the file is opened. This will work if the file only needs to be opened once.
- If the file needs to be opened twice or more, create it in a protected
  directory, {i e.g.}, [$HOME].
- Ensure that [/usr/tmp] has its sticky bit set. This requires system
  administrator privileges.
*)

(* p 50. *)
value set_temp_file_template : (string * string) -> unit;
value with_temp_file_template : (string * string) -> (unit -> 'a) -> 'a;
(** The actual default prefix used by [create_temp_file] and template for
  [temp_file_iterate] can be overridden for increased security, and is
  controlled by these two procs, which modify it permanently
  ([set_temp_file_template]) or temporarily ([with_temp_file_template]).

This template is a pair of strings used as prefix and suffix for the names; it
defaults to [("/usr/tmp/]{i pid}[.", "")], where {i pid} is the current process'
process id.  File names are generated by inserting a varying string between
them. *)

value temp_file_iterate : ?template: (string * string) -> (string -> option 'a) -> 'a;
(** This procedure can be used to perform certain atomic transactions on the
 file system involving filenames. Some examples: 
- Linking a file to a fresh backup temp name.
- Creating and opening an unused, secure temp file.
- Creating an unused temporary directory.

This procedure uses {i template} to generate a series of trial file names (see
[with_temp_file_template]).

The second argument is a {i maker} procedure which is serially called on each
file name generated.  It returns one value wrapped in an option type. If it is
[None] or if {i maker} raises [Unix.Unix_error Unix.EEXIST ...],
[temp_file_iterate] will loop, generating a new file name and calling {i maker}
again. If the returned value is [Some v], the loop is terminated, returning [v].

After a number of unsuccessful trials, [temp_file_iterate] may give up and
signal an error.

Thus, if we ignore its optional {i template} argument, [create_temp_file] could
be defined as:
{[
  let create_temp_file () =
    let flags = [O_WRONLY; O_CREAT; O_EXCL] in
    temp_file_iterate
      (fun fname ->
         ignore (Io_3_2.close_fd (Io_3_2.open_fdes ~perms:0o600 fname flags)); 
         Some fname)
]}

To rename a file to a temporary name:
{[
  temp_file_iterate 
    ~template: (".#temp.", "")            (* Keep link in cwd. *)
    (fun backup -> create_hard_link old_file backup; Some backup);
  delete_file old_file
]} Recall that Cash reports syscall failure by raising an error exception, not
by returning an error code. This is critical to to this example --- the
programmer can assume that if the [temp_file_iterate] call returns, it returns
successully.  So the following [delete_file] call can be reliably invoked, safe
in the knowledge that the backup link has definitely been established.

To create a unique temporary directory:
{[
  temp_file_iterate
    ~template: ("/usr/tmp/tempdir.", "")
    (fun dir -> create_directory dir; Some dir)
]}

Similar operations can be used to generate unique symlinks and fifos, or to
return values other than the new filename ({i e.g.}, an open file descriptor or
channel).

For increased security, a user may wish to change the template to use a
directory not allowing world write access ({i e.g.}, his home directory). *)

(* p 51. *)
value temp_file_channel : unit -> (in_channel * out_channel);
(** This procedure can be used to provide an interprocess communications channel
 with arbitrary-sized buffering.  It returns two values, an input channel and an
 output channel, both open on a new temp file.  The temp file itself is deleted
 from the Unix file tree before [temp_file_channel] returns, so the file is
 essentially unnamed, and its disk storage is reclaimed as soon as the two
 channels are closed.

[Temp_file_channel] is analogous to [pipe] with two exceptions:
- If the writer process gets ahead of the reader process, it will not hang
  waiting for some small pipe buffer to drain. It will simply buffer the data on
  disk. This is good.
- If the reader process gets ahead of the writer process, it will also not hang
  waiting for data from the writer process. It will simply see and report an end
  of file. This is bad.

In order to ensure that an end-of-file returned to the reader is legitimate, the
reader and writer must serialise their i/o. The simplest way to do this is for
the reader to delay doing input until the writer has completely finished doing
output, or exited.  *)

(** {2 Processes} *)

(* p 52. *)
value exec : string -> list string -> unit;
value exec_path : string -> list string -> 'a;
value exec_with_env : string -> ?env: (list (string * string)) -> list string -> unit;
value exec_path_with_env : string -> ?env: (list (string * string)) -> list string -> 'a;
(** The [..._with_env] variants take an optional environment as 2d argument.
 The default value is taken to mean the current process' environment ({i i.e.},
 the value of the [external char **environ]).

The path-searching variants search the directories in the list [exec_path_list]
for the program.  A path-search is not performed if the program name contains a
slash character --- it is used directly. So a program with a name like
["bin/prog"] always executes the program [bin/prog] in the current working
directory. See [$PATH] and {!Cash.exec_path_list}, below.

All of these procedures flush buffered output and close unrevealed channels before
executing the new binary.  To avoid flushing buffered output, see [low_exec] below.

Note that the C [exec()] procedure allows the zeroth element of the
argument vector to be different from the file being executed, {i e.g.}
{[  char *argv[] = {"-", "-f", 0};
  exec("/bin/csh", argv, envp); ]}
The Cash [exec], [exec_path], [exec_with_env], and [exec_path_with_env]
procedures do not give this functionality --- element 0 of the arg vector is
always identical to the [prog] argument. In the rare case the user wishes to
differentiate these two items, he can use the low-level [low_exec] and
[exec_path_search] procedures.  These procedures never return under any
circumstances.  As with any other system call, if there is an error, they raise
an exception.  *)

value low_exec : string -> ?env: (list (string * string)) -> list string -> unit;
value exec_path_search : string -> list string -> string;
(** The [low_exec] procedure is the low-level interface to the system call.
 In [low_exec] {i prog ~env arglist}, the {i arglist} parameter is a list of
arguments; {i ~env}, if any, is a string -> string alist; if none, it means the
current process' environment.  The new program's [argv\[0\]] will be taken from
[List.hd] {i arglist}, {e not} from {i prog}.  [low_exec] does not flush
buffered output (see {!Cash.flush_all_chans}).

[exec_path_search] {i fname pathlist} searches the directories of {i pathlist}
looking for an occurrence of file {i fname}. If no executable file is found, it
raises [Not_found]. If {i fname} contains a slash character, the path search is
short-circuited, but the procedure still checks to ensure that the file exists
and is executable --- if not, it still raises [Not_found].  Users of this
procedure should be aware that it invites a potential race condition: between
checking the file with [exec_path_search] and executing it with [low_exec], the
file's status might change.  The only atomic way to do the search is to loop
over the candidate file names, exec'ing each one and looping when the exec
operation fails.

See [$PATH] and {!Cash.exec_path_list}, below. *)

(* p 53. *)
value exit : int -> 'a;
external low_exit : int -> 'a = "sys_exit";
(** These procedures terminate the current process with a given exit status.
 The low-level [low_exit] procedure immediately terminates the process without
 flushing buffered output. *)

value call_terminally : option (unit -> unit) -> option 'a;
(** [call_terminally] calls its thunk. When the thunk returns, the process
 exits.  Although [call_terminally] could be implemented as
        {[    fun thunk -> thunk(); exit 0 ]}
an implementation can take advantage of the fact that this procedure never
returns. For example, a Scheme runtime can start with a fresh stack and also
start with a fresh dynamic environment, where shadowed bindings are
discarded. This can allow the old stack and dynamic environment to be collected
(assuming this data is not reachable through some live continuation).

Useless to say, this behaviour is not implemented in Caml.  *)

value suspend : unit -> unit;
(** Suspend the current process with a SIGSTOP signal. *)

value fork : unit -> option proc;
value fork_child : (unit -> unit) -> proc;
value low_fork : ?child: (unit -> unit) -> unit -> option proc;
(** [fork ()] is like C [fork()]. 
 In the parent process, it returns [(Some] <the child's {e process object}>[)]
 (see {!Cash.procobj} for more information on process objects).  In the child
 process, it returns [None].

[fork_child] {i thunk} only returns in the parent process, returning the child's
{e process object}.  The child process calls {i thunk} and then exits.

[fork] and [fork_child] flush buffered output before forking, and set the child
process to non-interactive. [low_fork] does not perform this bookkeeping; it
simply forks.  *)

(* p 54. *)
value fork_with_pipe : unit -> option proc;
value fork_child_with_pipe : (unit -> unit) -> proc;
value low_fork_with_pipe : ?child:(unit -> unit) -> unit -> option proc;
(** Like [fork], [fork_child] and [low_fork], but the parent and child
  communicate via a pipe connecting the parent's [stdin] to the child's
  stdout. These procedures side-effect the parent by changing his [stdin].

In effect, [fork_...with_pipe] splice a process into the data stream immediately
upstream of the current process.  This is the basic function for creating
pipelines.  Long pipelines are built by performing a sequence of
[fork_child_with_pipe] calls.  For example, to create a background two-process
pipe [a | b], we write:
 {[   fork_child (fun () -> fork_child_with_pipe a; b ()) ]}
which returns the process object for [b]'s process.

To create a background three-process pipe [a | b | c], we write:
 {[
   fork_child
       (fun () ->
           fork_child_with_pipe a;
           fork_child_with_pipe b;
           c ());; ]}
which returns the process object for [c]'s process.

Note that these procedures affect file descriptors, not channels.  That is, the
pipe is allocated connecting the child's file descriptor 1 to the parent's file
descriptor 0.  {e Any previous Caml channel built over these affected file
descriptors is shifted to a new, unused file descriptor with [dup] before
allocating the I/O pipe.}  This means, for example, that the channels bound to
[stdin] and [stdout] in either process are not affected --- they still refer to
the same I/O sources and sinks as before.  Remember the simple Cash rule: Caml
channels are bound to I/O sources and sinks, {e not} particular file
descriptors.

If the child process wishes to rebind the current [stdout] to the pipe on file
descriptor 1, it can do this using [with_stdout] or a related function (see
{!Cash.withstd}).  Similarly, if the parent wishes to change the current [stdin]
to the pipe on file descriptor 0, it can do this using [set_stdin] or a related
function.  Here is an example showing how to set up the I/O channels on both
sides of the pipe:
{[   fork_child_with_pipe
       (fun () ->
          with_stdout (out_channel_of_fd 1)
            (fun () -> print_endline "Hello, world."));
   set_stdin (in_channel_of_fd 0);
   print_endline (read_line stdin);;  (* Read the string output by the child. *) ]}
None of this is necessary when the I/O is performed by an exec'd program in the
child or parent process, only when the pipe will be referenced by Caml code
through one of the default current I/O channels.  *)

(* XXX modifier la documentation quand ceci sera OK
 See the [(|+] {i conns} {i pf1} [...] {i pfn}[)] process form for a description
 of connection lists.

Voici la documentation de |+ (c'est ch... à refaire la prochaine fois)

A {i connect-list} is a specification of how two processes are to be wired
together by pipes.  It has the form [((]{i from1} {i from2} [...] {i to}[) ...)]
and is implicitly backquoted.  For example,

[(|+ ((1 2 0) (3 1))] {i pf1} {i pf2}[)]

runs {i pf1} and {i pf2}.  The first clause [(1 2 0)] causes {i pf1}'s stdout
(1) and stderr (2) to be connected via pipe to {i pf2}'s stdin (0).  The second
clause [(3 1)] causes {i pf1}'s file descriptor 3 to be connected to {i pf2}'s
file descriptor 1.

 *)
value fork_with_pipe_plus : list (list fd) -> option proc;
value fork_child_with_pipe_plus : (unit -> 'a) -> list (list fd) -> proc;
value low_fork_with_pipe_plus : ?child: (unit -> 'a) -> list (list fd) -> option proc;
(** Like [fork_with_pipe] {e et al.}, but the pipe connections between the child
 and parent are specified by a connection list.

A {i connect-list} is a specification of how the two processes are to be wired
together by pipes.  It has the form [((]{i from1} {i from2} [...] {i to}[)
...)].  For example, with

{[    [[1; 2; 0]; [3; 1]]]}

the first clause [\[1; 2; 0\]] causes child's stdout (1) and stderr (2) to be
connected via pipe to parent's stdin (0).  The second clause [\[3; 1\]] causes
child's file descriptor 3 to be connected to parent's file descriptor 1.

{i Note that all from's are }[out_channel]{i s, and all to's are }[in_channel]{i
s; the child produces, the parent consumes.}
*)


(** {3:procobj Process objects and process reaping} *)

(** Cash uses {e process objects} to represent Unix processes.
  They are created by the [fork] procedure, and have the following hidden
  structure:
{[ type proc = { p_id : int; p_status : Unix.process_status }
]}

The only always accessible slot in a proc record is the process' pid, the
integer id assigned by Unix to the process: to get it, use the only (low level)
exported procedure for manipulating process objects: [pid_of_proc]. *)

value pid_of_proc : proc -> int;
(** Extract the process id out of a proc object. *)

(** The type of the {i ~probe} argument to [proc_of_pid], that determines what
action to take if there is no process object indexed by the given pid in the
system. *)
type probe_pid =
  Proc_3_4.probe_pid ==
    [ Probe(** {e Signal error condition.} *)
    | Create(** Create new proc object. *)
    | Don't_probe ](** Return [None]. *)
;

value proc_of_pid : ?probe: probe_pid -> int -> option proc;
(** This procedure maps integer Unix process ids to Cash process objects.
 It is intended for use in interactive and debugging code, and is deprecated for
 use in production code.
*)

(* p 56. *)

(** Sometime after a child process terminates, Cash will perform a [wait] system
call on the child in background, caching the process' exit status in the child's
proc object.  This is called ``reaping'' the process.  Once the child has been
waited, the Unix kernel can free the storage allocated for the dead process'
exit information, so process reaping prevents the process table from becoming
cluttered with un-waited dead child processes (a.k.a. ``zombies'').  This can be
especially severe if the Cash process never waits on child processes at all; if
the process table overflows with forgotten zombies, the OS may be unable to fork
further processes.

Reaping a child process moves its exit status information from the kernel into
the Cash process, where it is cached inside the child's process object.  If the
Cash user drops all pointers to the process object, it will simply be garbage
collected.  On the other hand, if the Cash program retains a pointer to the
process object, it can use Cash's [wait] system call to synchronise with the
child and retrieve its exit status multiple times (this is not possible with
simple Unix integer pids in C --- the programmer can only wait on a pid once).

Thus, process objects allow Cash programmer to do two things not allowed in
other programming environments: 
- Subprocesses that are never waited on are still removed from the process
  table, and their associated exit status data is eventually automatically
  garbage collected.
- Subprocesses can be waited on multiple times.

However, note that once a child has exited, if the Cash programmer drops all
pointers to the child's proc object, the child's exit status will be reaped and
thrown away.  This is the intended behaviour, and it means that integer pids are
not enough to cause a process's exit status to be retained by the Cash runtime.
(This is because it is clearly impossible to GC data referenced by integers.)

As a convenience for interactive use and debugging, all procedures that take
process objects have corresponding [..._pid] versions taking integer Unix pids
as arguments, coercing them to the corresponding process objects.  Since integer
process ids are not reliable ways to keep a child's exit status from being
reaped and garbage collected, programmers are encouraged to use process objects
in production code.  *)

type autoreap_policy = Proc_3_4.autoreap_policy == [ No_autoreaping | Early | Late ];

value autoreap_policy : ?policy: autoreap_policy -> unit -> autoreap_policy;
(** The Cash programmer can choose different policies for automatic process
    reaping. 
The policy is determined by using a {i ~policy} argument whose values have the
following meaning:
- [Early] (the default):
   The child is reaped from the Unix kernel's process table into Cash as soon as
   it dies. This is done by having a signal handler for the [SIGCHLD] signal
   reap the process.  {e If a Cash program sets its own handler for the
   [SIGCHLD] signal, the handler must reap dead children by calling [wait],
   [wait_any], or [reap_zombies].}  {i Scsh only note: we deprecate
   interrupt-driven code, and hope to provide alternative tools in a future,
   multi-threaded release of Scsh}.
- [Late]:
   The child is not autoreaped until it dies {e and} the Cash program drops all
   pointers to its process object. That is, the process table is cleaned out
   during garbage collection.  {i Oops: The }[late] {i policy is not supported
   under the current release of Cash. It requires more sophisticated gc hooks
   than we can get from this release of Caml.}
- [No_autoreaping]:
   If autoreaping is turned off, process reaping is completely under control of
   the programmer, who can force outstanding zombies to be reaped by manually
   calling the [reap_zombies] procedure (see below).

Note that under any of the autoreap policies, a particular process {i p} can be
manually reaped into Cash by simply calling [wait] {i p}.  {e All} zombies
can be manually reaped with [reap_zombies].

The [autoreap_policy] procedure returns the policy's previous value.  Calling
[autoreap_policy ()] returns the current policy without no change. *)

(* p 57. *)
value reap_zombies : unit -> bool;
(** This procedure reaps all outstanding exited child processes into Cash.  It
returns true if there are no more child processes to wait on, and false if there
are outstanding processes still running or suspended. *)

(** {4 Issues with process reaping} *)

(** Reaping a process does not reveal its process group at the time of death;
this information is lost when the process reaped.  This means that a dead,
reaped process is {e not eligible} as a return value for a future
[wait_process_group] call.  This is not likely to be a problem for most code, as
programs almost never wait on exited processes by process group.  Process group
waiting is usually applied to {e stopped} processes, which are never reaped.  So
it is unlikely that this will be a problem for most programs.  *)

(* %%% Actually, this is *not* a problem if you stick with proc objects, instead
%%% of using pids, so I commented it out.

%\paragraph{Pid aliasing}
%Second, once a process has been reaped, its 16-bit process id becomes
%available to Unix for re-use.
%So it is conceivable that a long time in the future, a [fork] operation
%could produce a subprocess with the identical pid, causing [wait]
%operations on the old, dead, reaped child, and the new child to become
%confused.
%This kind of pid aliasing is intrinsic to the nature of Unix's single-use pid
%deallocation policy,
%but is very, very unlikely to happen in practice,
%given the 16-bit size of the pid space.
%Scsh will detect occurences of pid aliasing, 
%in the unlikely event that one occurs.
%When [fork] creates a proc object, it checks to see if the Scsh heap
%contains an already existing proc object with the same pid as the newly forked
%process.
%If so, an exception is raised; if not handled by the program, this will stop
%the program, either killing the process or invoking an interactive debugger. *)

(** Automatic process reaping is a useful programming convenience.  However, if
a program is careful to wait for all children, and does not wish automatic
reaping to happen, the programmer can simply turn process autoreaping off.

Programs that do not wish to use automatic process reaping should be aware that
some Cash routines create subprocesses but do not return the child's proc
object: {!Cash.run_with_in_channel}, and related procedures ([run_with_strings],
{e et al.}).  Automatic process reaping will clean the child processes created
by these procedures out of the kernel's process table.  If a program doesn't use
process reaping, it should either avoid these forms, or use [wait_any] to wait
for the children to exit. *)

(* p 58. *)
(** {3 Process waiting} *)

type process_status =
  Unix.process_status ==
    [ WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int ]
;

exception Child_not_ready;

type wait_flag =
  Unix.wait_flag ==
    [ WNOHANG(** Raise [Child_not_ready] immediately if child still active. *)
    | WUNTRACED ](** Wait for suspend as well as exit. *)
;

value wait : ?wflags: (list wait_flag) -> proc -> process_status;
value wait_pid : ?wflags: (list wait_flag) -> int -> process_status;
(** These procedures wait until a child process exits, and returns its exit
  code. The {i proc} argument to [wait] is a process object (section
{!Cash.procobj}) or, to [wait_pid], an integer process id.  

They return the child's exit status code (or suspension code, if the [WUNTRACED]
option is used, see above).  See section {!Cash.waitcodes} about querying status
values.

The {i flags} argument is a list of additional options. See above. *)

type wait_any =
  Proc_3_4.wait_any ==
    [ None_ready | No_children | Exited of (proc * process_status) ]
;

value wait_any : ?wflags: (list wait_flag) -> unit -> wait_any;
(** The optional {i flags} argument is as for [wait].
 This procedure waits for any child process to exit (or stop, if the [WUNTRACED]
 flag is used).  If one child has exited, it returns the process' process object
 and status code.  If there are no children left for which to wait,
 [No_children] is returned.  If the [WNOHANG] flag is used, and none of the
 children are immediately eligible for waiting, then [None_ready] is returned.

[wait_any] will not return a process that has been previously waited by any
other process-wait procedure ([wait], [wait_pid], [wait_any], and
[wait_process_group]).  It will return reaped processes that haven't yet been
waited.

The use of [wait_any] is deprecated.  *)

value wait_process_group : ?wflags: (list wait_flag) -> proc -> wait_any;
value wait_process_group_pgrp : ?wflags: (list wait_flag) -> int -> wait_any;
(** These procedures wait for any child whose process group is {i proc} 
 (a process object, for [wait_process_group]) or {i pgrp} (an integer process
 group id, for [wait_process_group_pgrp]).  The {i flags} argument is as for
 [wait].

Note that if the programmer wishes to wait for exited processes by process
group, the program should take care not to use process reaping (section
{!Cash.procobj}, as this loses process group information. However,
most process-group waiting is for stopped processes (to implement job control),
so this is rarely an issue, as stopped processes are not subject to reaping.  *)

(* p 59. *)

(** {3:waitcodes Analysing process status codes} *)

(** When a child process dies (or is suspended), its parent can call the [wait]
procedure to recover the exit (or suspension) status of the child.  The exit
status is a small integer that encodes information describing how the child
terminated.  The bit-level format of the exit status is not defined by Posix;
you must use pattern-match to decode it.  However, if a child terminates
normally with exit code 0, Posix does require [wait] to return an exit status
that is exactly zero.  So {i status} [= WEXITED 0] is a correct way to test for
non-error, normal termination, {i e.g.},
{[   let proc = 
     (fork_child (fun () -> exec_path "rcp" ["cash.tar.gz"; "lambda.csd.hku.hk:"])) 
   in
   if wait proc = WEXITED 0 then delete_file "cash.tar.gz"
]}
*)

(* p 60. *)

(** {2 Process state} *)

value umask : unit -> int;
value set_umask : int -> unit;
value with_umask : int -> (unit -> 'a) -> 'a;
(** The process' current umask is retrieved with [umask], and set with
[set_umask] {i perms}. Calling [with_umask] {i perms thunk} changes the umask to
{i perms} for the duration of the call to {i thunk}. If {i thunk} raises an
exception, the umask is reset to its external value.  *)

value chdir : ?dir:string -> unit -> unit;
value cwd : unit -> string;
value with_cwd : string -> (unit -> 'a) -> 'a;

(** These procedures manipulate the current working directory.  
 The cwd can be changed with [chdir] (although in most cases, [with_cwd] is
preferrable).  [chdir ()] changes the cwd to the user's home directory.
[with_cwd] {i dir thunk} calls {i thunk} with the cwd temporarily set to {i
dir}; when {i thunk} returns, or raises an exception, the cwd is returned to its
original value.  *)

value pid : unit -> int;
value parent_pid : unit -> int;
value process_group : unit -> int;
value set_process_group : ?proc: proc -> int -> unit;
value set_process_group_pid : int -> int -> unit;
(** [pid] and [parent_pid] retrieve the process id for the current process and
    its parent. 
[process_group] returns the process group of the current process.  A process'
process-group can be set with [set_process_group...]; the value {i proc} (for
[set_process_group_pid], an integer process id) specifies the affected
process. {i proc} defaults to the current process.  *)

type prio =
  Proc_state_3_5.prio ==
    [ Prio_process
    | Prio_pgrp
    | Prio_user ]
;
(** Tells to the following [priority] and [set_priority] procedures if {i ~who}
    is a process id, a process group id or a user id, respectively. *)

value set_priority : ?who: proc -> prio -> int -> unit;
value set_priority_pid : int -> prio -> int -> unit;
value priority : ?who: proc -> prio -> int;
value priority_pid : int -> prio -> int;
value nice : ?proc: proc -> int -> unit;
value nice_pid : int -> int -> unit;
(** These procedures manipulate nice values of processes.  The optional
 arguments of type [proc] default to the current process.  
The ones of type [prio] indicate how to interpret the first argument. The [int]
last arguments/results are the nice values, except for [nice...], where it is a
delta to be added to the nice value.  If you insist on using pids, there are
[..._pid] variants, where the first argument is an integer process id.  The
corresponding Posix procedures are [{set,get}priority].  *)

(* p 61. *)
value user_login_name : unit -> string;
value user_uid : unit -> int;
value user_effective_uid : unit -> int;

value user_gid : unit -> int;
value user_effective_gid : unit -> int;
value user_supplementary_gids : unit -> array int;

value set_uid : int -> unit;
value set_gid : int -> unit;
(** These routines get and set the effective and real user and group ids.    
 The [set_uid] and [set_gid] routines correspond to the Posix [setuid()] and
 [setgid()] procedures. *)

type process_times =
  Unix.process_times ==
    { tms_utime : float; tms_stime : float; tms_cutime : float; tms_cstime : float }
;
(** The process times are:
- [tms_utime]: user CPU time
- [tms_stime]: system CPU time
- [tms_cutime]: the sames for all...
- [tms_cstime]: ...descendant (terminated) processes

Note that CPU time clock resolution is not the same as the real-time clock
resolution provided by [time_plus_ticks].  That's Unix.  *)

value process_times : unit -> process_times;
(** Get the process_times of the current process. *)

value cpu_ticks_per_sec : unit -> int;
(** Returns the resolution of the CPU timer in clock ticks per second.
 This can be used to convert the times reported by [process_times] to ticks. *)

(** {2 User and group database access} *)

(** These procedures are used to access the user and group databases ({i e.g.},
the ones traditionally stored in [/etc/passwd] and [/etc/group].)  *)

(** This record gives the recorded information for a particular user. *)
type user_info =
  User_group_3_6.user_info ==
    { ui_name : string;
      ui_uid : int;
      ui_gid : int;
      ui_home_dir : string;
      ui_shell : string }
;

value user_info : int -> user_info;
value user_info_name : string -> user_info;
(** Return a user_info record for this user: an integer uid (for [user_info]),
 or a string user-name (for [user_info_name]). *)

(* p 62. *)
value username_to_uid : string -> int;
value uid_to_username : int -> string;
(** These two procedures convert integer uid's and user names to the other form. *)

(** This record gives the recorded information for a particular group. *)
type group_info =
  User_group_3_6.group_info ==
    { gi_name : string; gi_gid : int; gi_members : list string }
;

value group_info : int -> group_info;
value group_info_name : string -> group_info;
(** Return a group_info record for this group: an integer gid (for
  [group_info]), or a string group-name (for [group_info_name]). *)

value groupname_to_gid : string -> int;
value gid_to_groupname : int -> string;
(** These two procedures convert integer gid's and group names to the other form. *)

(** {2 Accessing command-line arguments} *)

value command_line_arguments : ref (option (list string));
value command_line : unit -> list string;
(** The list of strings [command_line_arguments] contains the arguments
    passed to the Cash process on the command line.
Calling [command_line ()] returns the complete [argv] string list, including the
program. So if we run a Cash program
{[        /usr/shivers/bin/myls -CF src]}
then [command_line_arguments] is
{[        ["-CF"; "src"] ]}
and [command_line ()] returns
{[        ["/usr/shivers/bin/myls"; "-CF"; "src"] ]}

{i Oops:} [command_line_arguments] {i should be a string list ref, sans option.
This is due to the way Ocaml script execution munges Sys.argv: it's done after
ocamlrun argument processing, and just before }[#use]{i -ing the script.  No way
to insert} [command_line_arguments] {i initialization here.  So}
[!command_line_arguments] {i will be }[None] {i until the first call to}
[command_line ()]{i . To reset things as they should be, use the recipe
described below and forget all this mess.  } *)

value make_command_line_arguments : unit -> list string;
(** Usage: insert the following code in front of your script:
 {[  let command_line_arguments = ref (make_command_line_arguments ()) ]}
(suppress [ref] if you don't intend to modify it.)  Then you get the intended
[command_line_arguments].  Sorry. *)

value arg : ?default: 'a -> list 'a -> int -> 'a;
value arg_star : ?default_thunk: (unit -> 'a) -> list 'a -> int -> 'a;
value argv : ?default: string -> int -> string;
(** These procedures are useful for accessing arguments from argument lists.
 [arg] {i arglist n} returns the {i n}th element of {i arglist}.
The index is 1-based.  If {i n} is too large, {i default} is returned; if no {i
default}, then an error is signaled.

[arg_star] is similar, except that the {i default-thunk} is called to generate
the default value.

[argv] {i n} is simply [arg (command_line ()) (]{i n} [+ 1)]. 
The +1 offset ensures that the two expressions:
{[   arg !command_line_arguments n;
   argv n ]}
return the same argument (assuming the user has not rebound or modified
[command_line_arguments]).  Example: 
{[  if !command_line_arguments = [] then
    fork_child
      (fun () -> 
        exec_path "xterm" ["-n"; host; "-title"; host; "-name"; "xterm_" ^ host])
  else
    let progname = file_name_nondirectory (argv 1) in
    let title = host ^ ":" ^ progname in
    fork_child
      (fun () ->
         exec_path "xterm"
           ("-n" :: title :: "-title" :: title :: "-e" :: !command_line_arguments))
]} *)

(** A subtlety: when the ocaml interpreter is used to execute a Cash program, the
program name reported in the head of the [command_line ()] list is the Cash
program, {e not} the interpreter.  For example, if we have a shell script in
file [fullecho]: {[        #!/usr/local/bin/cash
        open Cash;;
        List.iter (fun arg -> print_string arg; print_char ' ') (command_line ());;]}
and we run the program
 {[        fullecho hello world]}
the program will print out
 {[        ./fullecho hello world]}
not
 {[        /usr/local/bin/cashtop -I mydir ./fullecho hello world]}
{i The }[./] {i prepended to the name of the program is an artifact of the
interactive shell (bash, or some so) on a particular OS --- it may or may not
appear elsewhere.}

This argument line processing ensures that if a Cash program is subsequently
compiled into a standalone executable or byte-compiled to a custom executable,
or even a byte-code dynamically linked file, executable by the ocamlrun virtual
machine, its semantics will be unchanged --- the arglist processing is
invariant. In effect, the
        [/usr/local/bin/cash]
is not part of the program; it's a specification for the machine to execute the
program on, so it is not properly part of the program's argument list.
*)

(* p 64. *)

(** {2 System parameters} *)

value system_name : unit -> string;
(** Returns the name of the host on which we are executing.
 This may be a local name, such as ``solar,'' as opposed to a fully-qualified
 domain name such as ``solar.csie.ntu.edu.tw.''
*)

(** {2 Signal system} *)

value signal_process : proc -> int -> unit;
value signal_process_pid : int -> int -> unit;
value signal_process_group : proc -> int -> unit;
value signal_process_group_pgrp : int -> int -> unit;
(** These two pair of procedures send signals to a specific process, and all the
  processes in a specific process group, respectively.
The {i proc} arguments are processes, or, for [signal_..._pid], integer process
ids.  *)

(** The three kinds of interval timers. *)
type itimer =
  Unix.interval_timer ==
    [ ITIMER_REAL
       (** decrements in real time, and sends the signal [SIGALRM] when
       expired. *)
   | ITIMER_VIRTUAL
       (**  decrements in process virtual time, and sends [SIGVTALRM] when expired. *)
   | ITIMER_PROF ]
       (** (for profiling) decrements both when the process is running and when
         the system is running on behalf of the process; it sends [SIGPROF] when
         expired. *)
;

(** The status of an interval timer *)
type itimer_status =
  Unix.interval_timer_status ==
    { it_interval : float;         (** Period *)
      it_value : float             (** Current value of the timer *) 
    }
;

value itimer : ?newstat:itimer_status -> itimer -> itimer_status;
(** This is a straighforward interface to Unix.getitimer (if no {i newstat}) and
 Unix.setitimer. *)

value pause_until_interrupt : unit -> unit;
(** The name says it all. *)

value sleep : int -> unit;
value sleep_until : float -> unit;
(** The [sleep] procedure causes the process to sleep for {i secs} seconds.
 The [sleep_until] procedure causes the process to sleep until {i time} (see
 section {!Cash.time_date}). *)

(* p 69. *)

(** {2:time_date Time} *)

(** Cash's time system is fairly sophisticated, particularly with respect to its
careful treatment of time zones.  However, casual users shouldn't be
intimidated; all of the complexity is optional, and defaulting all the optional
arguments reduces the system to a simple interface. *)

(** {3 Terminology} *)

(**``UTC'' and ``UCT'' stand for ``universal coordinated time,'' which is the
official name for what is colloquially referred to as ``Greenwich Mean Time.''

Posix allows a single time zone to specify {e two} different offsets from UTC:
one standard one, and one for ``summer time.''  Summer time is frequently some
sort of daylight savings time.

The Cash time package consistently uses this terminology: we never say ``gmt''
or ``dst;'' we always say ``utc'' and ``summer time.''  *)

(** {3 Basic data types} *)

(** We have two types: {e time} and {e date}. *)
(* time-zone independent: \footnote{Physics pedants please note:
    The Scsh authors live in a Newtonian universe. We disclaim responsibility
    for calculations performed in non-ANSI standard light-cones.}. *)
(** A {e time} specifies an instant in the history of the universe.  It is location
and time-zone independent.  A time is a real value giving the number of elapsed
seconds since the Unix ``epoch'' (Midnight, January 1, 1970 UTC).  Time values
provide nearly arbitrary time resolution, limited only by the Caml floats (IEEE).

A {e date} is a name for an instant in time that is specified relative to some
location/time-zone in the world, {i e.g.}:

    Friday October 31, 1994 3:47:21 pm EST.
*)

(** Dates provide one-second resolution, and are expressed with the following
record type (a Posix tm struct): *)

type date =
  Time_3_10.date ==
    { seconds : int;(**  Seconds after the minute \[0-59\]. *)
      minute : int;(** Minutes after the hour \[0-59\]. *)
      hour : int;(** Hours since midnight \[0-23\]. *)
      month_day : int;(** Day of the month \[1-31\]. *)
      month : int;(** Months since January \[0-11\]. *)
      year : int;(** Years since 1900. *)
      tz_name : option string;(** Time-zone name: an optional string. *)
      tz_secs : option int;(** Time-zone offset: an optional integer. *)
      is_summer : option bool;(** Summer (Daylight Savings) time in effect? *)
      week_day : int;(** Days since Sunday \[0-6\]. *)
      year_day : int(** Days since Jan. 1 \[0-365\]. *)
    }
;

(** If the [tz_secs] field is given, it specifies the time-zone's offset from
UTC in seconds. If it is specified, the [tz_name] and [is_summer] fields are
ignored when using the date structure to determine a specific instant in time.

If the [tz_name] field is given, it is a time-zone string such as ["EST"] or
["HKT"] understood by the OS.  Since Posix time-zone strings can specify dual
standard/summer time-zones ({i e.g.}, "EST5EDT" specifies U.S. Eastern
Standard/Eastern Daylight Time), the value of the [is_summer] field is used to
resolve the ambiguous boundary cases. For example, on the morning of the Fall
daylight savings change-over, 1:00am--2:00am happens twice. Hence the date 1:30
am on this morning can specify two different seconds; the [is_summer] flag says
which one.

A date with [tz_name] = [tz_secs] = [None] is a date that is specified in terms
of the system's current time zone.

There is redundancy in the [date] data structure.  For example, the [year_day]
field is redundant with the [month_day] and [month] fields.  Either of these
implies the values of the [week_day] field.  The [is_summer] and [tz_name]
fields are redundant with the [tz_secs] field in terms of specifying an instant
in time.  This redundancy is provided because consumers of dates may want it
broken out in different ways.  The Cash procedures that produce date records
fill them out completely.  However, when date records produced by the programmer
are passed to Cash procedures, the redundancy is resolved by ignoring some of
the secondary fields.  This is described for each procedure below. *)

(* p 70. *)
value make_date :
  ?tzn: string -> ?tzs: int -> ?summ: bool -> ?wday: int -> ?yday: int -> int -> int ->
    int -> int -> int -> int -> date;
(** When making a [date] record, the last five elements of the record are
 optional; the first three default ({i tzn, tzs, summ}) to [None], the last two
({i wday, yday}) to 0.  This is useful when creating a [date] record to pass as
an argument to [time].  *)

(** {3 Time zones} *)

(** Several time procedures take time zones as arguments. When optional, the
time zone defaults to local time zone. Otherwise the time zone can be one of: *)

type time_zone =
  Time_3_10.time_zone ==
    [ Tz_local(** Local time. *)
    | Tz_secs of int(** Seconds of offset from UTC. For example, New York City is -18000 (-5 hours), San Francisco is -28800 (-8 hours). *)
    | Tz_name of string ](** A Posix time zone string understood by the OS ({i i.e.}, the sort of time zone assigned to the [$TZ] environment variable). *)
;

(** An integer time zone gives the number of seconds you must add to UTC to get
time in that zone. It is {e not} ``seconds west'' of UTC --- that flips the sign.

To get UTC time, use a time zone of either 0 or ["UCT0"]. *)

(** {3 Procedures} *)

value time_plus_ticks : unit -> float;
value ticks_per_sec : unit -> float;
(** The current time, with sub-second resolution.
 Sub-second resolution is not provided by Posix, but is available on many
systems. The time is returned as a float, whose integer part is the number of
elapsed seconds since the Unix epoch, and fractional part corresponds to a
number of sub-second ``ticks.'' The length of a tick may vary from
implementation to implementation; it can be determined from [ticks_per_sec ()].

The system clock is not required to report time at the full resolution given by
[ticks_per_sec ()]. For example, on BSD, time is reported at 1 micro-second
resolution, so [ticks_per_sec ()] is 1,000,000. That doesn't mean the system
clock has micro-second resolution.

If the OS does not support sub-second resolution, the fractional part is always
0, and [ticks_per_sec ()] returns 1. *)

(* p 71. *)

value date : unit -> date;
value date_of_time : ?tz: time_zone -> float -> date;
(** Simple [date ()] returns the current date, in the local time zone.

[date_of_time] {i ~tz time} converts the time to the date as specified by the
time zone {i tz}.  {i tz} defaults to local time, and is as described in the
time-zone section.  Use [date_of_time ~tz (time ())] if you need the current
date in a non-local time zone.

If the {i tz} argument is an integer, the date's [tz_name] field is a Posix time
zone of the form ``[UTC+]{i hh}[:]{i mm}[:]{i ss}''; the trailing [:]{i mm}[:]{i
ss} portion is deleted if it is zeroes.

{i The Posix facility for converting dates to times, [mktime ()], has a broken
design: it indicates an error by returning -1, which is also a legal return
value (for date 23:59:59 UCT, 12/31/1969). Cash resolves the ambiguity in a
paranoid fashion: it always reports an error if the underlying Unix facility
returns -1. We feel your pain. } *)

value time : unit -> float;
value time_of_date : date -> float;
(** Simple [time ()] returns the current time.  [time_of_date] {i date} converts
a date to a time.

Note that the input {i date} record is overconstrained.  [time] ignores {i
date}'s [week_day] and [year_day] fields.  If the date's [tz_secs] field is set,
the [tz_name] and [is_summer] fields are ignored.

If the [tz_secs] field is [None], then the time-zone is taken from the [tz_name]
field. A [None] value of [tz_name] means the system's current time zone. When
calculating with time-zones, the date's [is_summer] field is used to resolve
ambiguities:
- [Some false]: Resolve an ambiguous time in favor of non-summer time.
- [Some true]: Resolve an ambiguous time in favor of summer time.
- [None]: unspecified (mktime(3) says `the information is not available')

The [Some] bool values are useful in boundary cases during the change-over. For
example, in the Fall, when US daylight savings time changes over at 2:00 am,
1:30 am happens twice --- it names two instants in time, an hour apart.

Outside of these boundary cases, the [is_summer] flag is ignored. For example,
if the standard/summer change-overs happen in the Fall and the Spring, then the
value of [is_summer] is ignored for a January or July date. A January date would
be resolved with standard time, and a July date with summer time, regardless of
the [is_summer] value.

The [is_summer] flag is also ignored if the time zone doesn't have a summer
time --- for example, simple UTC. *)

(* p 72. *)
value string_of_date : date -> string;
value format_date : string -> date -> string;
(** [string_of_date] formats the date as a 24-character string of the form:
{v     Sun Sep 16 01:03:52 1973 v}

[format_date] {i fmt date} formats the date according to the format string {i
fmt}. The format string is copied verbatim, except that % characters indicate
conversion specifiers that are replaced by fields from the date record.  The
full set of conversion specifiers supported by [format_date] is:
- %%  Converted to the % character.
- %a  abbreviated weekday name
- %A  full weekday name
- %b  abbreviated month name
- %B  full month name
- %c  time and date using the time and date representation for the locale ([~X ~x])
- %d  day of the month as a decimal number (01-31)
- %H  hour based on a 24-hour clock as a decimal number (00-23)
- %I  hour based on a 12-hour clock as a decimal number (01-12)
- %j  day of the year as a decimal number (001-366)
- %m  month as a decimal number (01-12)
- %M  minute as a decimal number (00-59)
- %p  AM/PM designation associated with a 12-hour clock
- %S  second as a decimal number (00-61)
- %U  week number of the year; Sunday is first day of week (00-53)
- %w  weekday as a decimal number (0-6), where Sunday is 0
- %W  week number of the year; Monday is first day of week (00-53)
- %x  date using the date representation for the locale
- %X  time using the time representation for the locale
- %y  year without century (00-99)
- %Y  year with century ({i e.g.} 1990)
- %Z  time zone name or abbreviation, or no characters if no time zone is determinable
*)

(** Here, there should be a
 {[       fill_in_date : date -> date; ]}
procedure, but it isn't implemented (yet) in Scsh, so I can't just steal the
code.  Here's the spec anyway:

This procedure fills in missing, redundant slots in a date record.  In
decreasing order of priority:
- {b year, month, month_day => year_day}: If the [year], [month], and
  [month_day] fields are all defined (are all integers), the [year_day] field is
  set to the corresponding value.
- {b year, year_day => month, month_day}: If the [month] and [month_day] fields
  aren't set, but the [year] and [year_day] fields are set, then [month] and
  [month_day] are calculated.
- {b year, month, month_day, year_day => week_day}: If either of the above rules
  is able to determine what day it is, the [week_day] field is then set.
- {b tz_secs => tz_name}: If [tz_secs] is defined, but [tz_name] is not, it is
  assigned a time-zone name of the form ``[UTC+]{i hh}[:]{i mm}[:]{i ss}''; the
  trailing [:]{i mm}[:]{i ss} portion is deleted if it is zeroes.
- {b tz_name, date, is_summer => tz_secs, is_summer}: If the date information is
  provided up to second resolution, [tz_name] is also provided, and [tz_secs] is
  not set, then [tz_secs] and [is_summer] are set to their correct values.
  Summer-time ambiguities are resolved using the original value of
  [is_summer]. If the time zone doesn't have a summer time variant, then
  [is_summer] is set to [None].
- {b local time, date, is_summer => tz_name, tz_secs, is_summer}: If the date
  information is provided up to second resolution, but no time zone information
  is provided (both [tz_anme] and [tz_secs] aren't set), then we proceed as in
  the above case, except the system's current time zone is used.
 *)

(* p 74. *)

(** {2 Environment variables} *)

value getenv : string -> string;
value setenv : ?sval: string -> string -> unit;
(** These functions get and set the process environment, stored in the external
C variable [char **environ].  An environment variable {i var} is a string.  If
an environment variable is set to a string {i sval}, then the process' global
environment structure is altered with an entry of the form ["]{i var}[=]{i
sval}["].  If {i sval} is omitted, then any entry for {i var} is deleted.  *)

value alist_of_env : unit -> list (string * string);
(** The [alist_of_env] procedure converts the entire environment into
 an alist, {i e.g.},
{[
[("TERM", "vt100");
 ("SHELL", "/usr/local/bin/cash"); 
 ("PATH", "/sbin:/usr/sbin:/bin:/usr/bin");
 ("EDITOR", "emacs") ;
 ...] ]}
*)

(* p 75. *)
value setenv_from_alist : list (string * string) -> unit;
(** The alist argument is installed as the current Unix environment ({i i.e.},
 converted to a null-terminated C vector of ["]{i var}[=]{i val}["] strings
 which is assigned to the global [char **environ]).
{[ setenv_from_alist
   [("TERM", "vt100");
    ("SHELL", "/usr/local/bin/cash"); 
    ("PATH", "/sbin:/usr/sbin:/bin:/usr/bin");
    ("EDITOR", "emacs") ;
    ...] ]} *)

(** The following three functions help the programmer manipulate alist tables in
some generally useful ways. They are all defined using [=] for key comparison.
*)

value alist_delete : 'a -> list ('a * 'b) -> list ('a * 'b);
(** [alist_delete] {i key alist} deletes any entry labelled by value {i key}. *)

value alist_update : 'a -> 'b -> list ('a * 'b) -> list ('a * 'b);
(** [alist_update] {i key val alist} deletes {i key} from {i alist}, then cons
  on a [(]{i key}[, ]{i val}[)] entry. *)

value alist_compress : list ('a * bool) -> list ('a * bool);
(** Compresses {i alist} by removing shadowed entries. Example:
{[  (* Shadowed (1 . c) entry removed. *)
    alist-compress [(1, a); (2, b); (1, c); (3, d)]    => [(1, a); (2, b); (3, d)]
]} *)

value with_env : list (string * string) -> (unit -> 'a) -> 'a;
value with_total_env : list (string * string) -> (unit -> 'a) -> 'a;
(** These procedures call their last argument {i thunk} in the context of an
 altered environment. They return whatever values {i thunk} returns. Non-local
 returns restore the environment to its outer value.

In [with_env] {i env_alist_delta thunk}, the {i env_alist_delta} argument
specifies a {e modification} to the current environment --- {i thunk}'s
environment is the original environment overridden with the bindings specified
by the alist delta.

In [with_total_env] {i env_alist thunk}, the {i env_alist} argument specifies a
complete environment that is installed for {i thunk}. *)

(** Example: These three pieces of code all run the mailer with special [$TERM]
and [$EDITOR] values. 

{[      let mail_me () = exec_path "mail" ["shivers\@lcs.mit.edu"];; ]}
{[
      with_env ["TERM", "kterm"; "EDITOR", my_editor]
        (fun () -> wait (fork_child mail_me));; ]}
{[
      wait
        (fork_child
           (* Env mutation happens in the subshell. *)
           (fun () ->
              setenv ~sval:"kterm" "TERM";
              setenv ~sval:my_editor "EDITOR";
              mail_me ()));; ]}
{[
      (* In this example, we compute an alternate environment env2 as an alist, and
         install it with an explicit call to the exec_path_with_env procedure. *) 
      let env = alist_of_env () in       (* Get the current environment, *)
      let env1 = alist_update "TERM" "kterm" env in      (* and compute  *)
      let env2 = alist_update "EDITOR" my_editor env1 in (* the new env. *)
      wait
        (fork_child
           (fun () -> exec_path_with_env "mail" ~env:env2 ["shivers\@cs.cmu.edu"]));;
]}
*)

(** {3 Path lists and colon lists} *)

(** When environment variables such as [$PATH] need to encode a list of strings
(such as a list of directories to be searched), the common Unix convention is to
separate the list elements with colon delimiters ({i ...and hope the individual
list elements don't contain colons themselves.})  To convert between the
colon-separated string encoding and the list-of-strings representation, see the
[infix_splitter] function (section {!Cash.field_splitter}) and the string
library's [String.concat] function.  For example,
{[ let split = infix_splitter ~delim:(Regexp (Pcre.regexp ":")) ();;
 split "/sbin:/bin::/usr/bin"                    => ["/sbin"; "/bin"; ""; "/usr/bin"]
 String.concat ":"  ["/sbin"; "/bin"; ""; "/usr/bin"]       => "/sbin:/bin::/usr/bin"
]}

The following two functions are useful for manipulating these ordered lists,
once they have been parsed from their colon-separated form. *)

(* p 76. *)
value add_before : 'a -> 'a -> list 'a -> list 'a;
value add_after : 'a -> 'a -> list 'a -> list 'a;
(** These functions are for modifying search-path lists, where element order
 is significant. 

[add_before] {i elt before} adds {i elt} to the list immediately before the
first occurrence of {i before} in the list.  If {i before} is not in the list,
{i elt} is added to the end of the list.

[add_after] {i elt after} is similar: {i elt} is added after the last occurrence
of {i after}.  If {i after} is not found, {i elt} is added to the beginning of
the list.

The result may share structure with the original list.  Both functions use [=]
for comparing elements. *) 

(* p 77. *)

(** {3 $USER, $HOME, and $PATH} *)

(** Like sh and unlike csh, Cash has {e no} interactive dependencies on
environment variables.  It does, however, initialise certain internal values at
startup time from the initial process environment, in particular [$HOME] and
[$PATH].  Cash never uses [$USER] at all.  It computes [user_login_name ()] from
the system call [user_uid ()].  *)

value home_directory : ref string;
(** Cash accesses [$HOME] at start-up time, and stores the value in the
  global variable [home_directory]. It uses this value for [~] lookups and for
  returning to home on [chdir ()]. *)

value exec_path_list : unit -> list string;
value set_exec_path_list : list string -> unit;
value with_exec_path_list : list string -> (unit -> 'a) -> 'a;
(** Cash accesses [$PATH] at start-up time, colon-splits the path list, and
  stores the value in an unexported variable, accessible by [exec_path_list
  ()]. This list is used for [exec_path] and [exec_path_with_env] searches.  It
  can be permanently modified by [set_exec_path_list] {i new_list}, or for the
  duration of a call with [with_exec_path_list] {i new_list thunk} --- this is
  the recommended way to alter it. *)

(* p 78. *)

(** {2 Terminal device control} *)

(** Cash provides a complete set of routines for manipulating terminal devices
--- putting them in ``raw'' mode, changing and querying their special
characters, modifying their i/o speeds, and so forth.  The cash interface is
designed both for generality and portability across different Unix platforms, so
you don't have to rewrite your program each time you move to a new system.
We've also made an effort to use reasonable, Scheme-like names for the
multitudinous named constants involved, so when you are reading code, you'll
have less likelihood of getting lost in a bewildering maze of obfuscatory
constants named [ICRNL], [INPCK], [IUCLC], and [ONOCR].

This section can only lay out the basic functionality of the terminal device
interface.  For further details, see the termios(3) man page on your system, or
consult one of the standard Unix texts. *)

(** {3 Portability across OS variants} *)

(** Terminal-control software is inescapably complex, ugly, and low-level.  Unix
variants each provide their own way of controlling terminal devices, making it
difficult to provide interfaces that are portable across different Unix systems.
Cash's terminal support is based primarily upon the Posix termios interface.
Programs that can be written using only the Posix interface are likely to be
widely portable.

The bulk of the documentation that follows consists of several pages worth of
tables defining different named constants that enable and disable different
features of the terminal driver.  Some of these flags are Posix; others are
taken from the two common branches of Unix development, SVR4 and 4.3+ Berkeley.
Cash guarantees that the non-Posix constants will be defined identifiers.
- If your OS supports a particular non-Posix flag, its named constant will be
  bound to the flag's value.
- If your OS doesn't support the flag, its named constant will be present, but
  bound to -1.

This means that if you want to use SVR4 or Berkeley features in a program, your
program can portably test the values of the flags before using them --- the
flags can reliably be referenced without producing ``unbound value'' errors.

Finally, note that although Posix, SVR4, and Berkeley cover the lion's share of
the terminal-driver functionality, each operating system inevitably has
non-standard extensions.  While a particular cash implementation may provide
these extensions, they are not portable, and so are not documented here. *)

(* p 79. *)

(** {3 Miscellaneous procedures} *)

value is_tty_fd : fd -> bool;
value is_tty_in : in_channel -> bool;
value is_tty_out : out_channel -> bool;
(** Return true if the argument is a tty. *)

value tty_file_name_fd : fd -> string;
value tty_file_name_in : in_channel -> string;
value tty_file_name_out : out_channel -> string;
(** The argument must be a file descriptor or channel open on a tty.
  Return the file-name of the tty. *)

(** {3 The tty_info record type} *)

(** The primary data-structure that describes a terminal's mode is
a [tty_info] record, defined as follows: *)

type tty_info =
  { control_chars : string;             (** Magic input chars *)
    input_flags : nativeint;            (** Input processing *)
    output_flags : nativeint;           (** Output processing *)
    control_flags : nativeint;          (** Serial-line control *)
    local_flags : nativeint;            (** Line-editting UI *)
    input_speed : int;                  (** Code for input speed *)
    output_speed : int;                 (** Code for output speed *)
    min : int;                          (** Raw-mode input policy *)
    time : int                          (** Raw-mode input policy *)
  }
;
(** {4 The control-characters string} *)

(** The [control_chars] field is a character string; its characters may be
indexed by integer values taken from the record [ttychar]. *)

type tty_chars =
  { delete_char : int;          (** {[ Posix       C: ERASE        typ. del]} *)
    delete_line : int;          (** {[ Posix       C: KILL         typ. ^U ]} *)
    eof : int;                  (** {[ Posix       C: EOF          typ. ^D ]} *)
    eol : int;                  (** {[ Posix       C: EOL                  ]} *)
    interrupt : int;            (** {[ Posix       C: INTR         typ. ^C ]} *)
    quit : int;                 (** {[ Posix       C: QUIT         typ. ^\ ]} *)
    suspend : int;              (** {[ Posix       C: SUSP         typ. ^Z ]} *)
    start : int;                (** {[ Posix       C: START        typ. ^Q ]} *)
    stop : int;                 (** {[ Posix       C: STOP         typ. ^S ]} *)
    delayed_suspend : int;      (** {[ SVR4+BSD    C: DSUSP        typ. ^Y ]} *)
    delete_word : int;          (** {[ SVR4+BSD    C: WERASE       typ. ^W ]} *)
    discard : int;              (** {[ SVR4+BSD    C: DISCARD      typ. ^O ]} *)
    eol2 : int;                 (** {[ SVR4+BSD    C: EOL2                 ]} *)
    literal_next : int;         (** {[ SVR4+BSD    C: LNEXT        typ. ^V ]} *)
    reprint : int;              (** {[ BSD         C: REPRINT      typ. ^R ]} *) 
    status : int                (** {[ BSD         C: STATUS       typ. ^T ]} *)
    }
;
value ttychar : tty_chars;

(** As discussed above, only the Posix entries in [ttychar] are guaranteed to be
legal, integer indices.  A program can reliably test the OS to see if the
non-Posix characters are supported by checking the index constants.  If the
control-character function is supported by the terminal driver, then the
corresponding index will be bound to a positive integer; if it is not supported,
the index will be bound to -1. *)

value disable_tty_char : char;
(** To disable a given control-character function, set its corresponding entry
in the [control_chars] string to the special character [disable_tty_char] (and
then use a [set_tty_info_...] procedure to update the terminal's state). *)

(* p 80. *)

(** {4 The flag fields} *)

(** The [tty_info] record's [input_flags], [output_flags], [control_flags], and
[local_flags] fields are all bit sets represented as two's-complement native
integers.  Their values are composed by or'ing together values taken from the
named constants in records [ttyin], [ttyout] and [ttyc], described below.

As discussed above, only the Posix entries listed in these tables are guaranteed
to be legal, integer flag values.  A program can reliably test the OS to see if
the non-Posix flags are supported by checking the named constants.  If the
feature is supported by the terminal driver, then the corresponding flag will be
bound to an integer; if it is not supported, the flag will be bound to -1. *)

type tty_in =
  { check_parity : nativeint;           (** {[ Posix       C: INPCK   Check Parity. ]} *)
    ignore_bad_parity_chars : nativeint;(** {[ Posix       C: IGNPAR  Ignore chars with parity errors. ]} *)
    mark_parity_errors : nativeint;     (** {[ Posix       C: PARMRK  Insert chars to mark parity errors. ]} *)
    ignore_break : nativeint;           (** {[ Posix       C: IGNBRK  Ignore breaks. ]} *)
    interrupt_on_break : nativeint;     (** {[ Posix       C: BRKINT  Signal on breaks. ]} *)
    seven_bits : nativeint;             (** {[ Posix       C: ISTRIP  Strip char to seven bits. ]} *)
    cr_to_nl : nativeint;               (** {[ Posix       C: ICRNL   Map carriage-return to newline. ]} *)
    ignore_cr : nativeint;              (** {[ Posix       C: IGNCR   Ignore carriage-returns. ]} *)
    nl_to_cr : nativeint;               (** {[ Posix       C: INLCR   Map newline to carriage-return. ]} *)
    input_flow_ctl : nativeint;         (** {[ Posix       C: IXOFF   Enable input flow control. ]} *)
    output_flow_ctl : nativeint;        (** {[ Posix       C: IXON    Enable output flow control. ]} *)
    xon_any : nativeint;                (** {[ SVR4+BSD    C: IXANY   Any char restarts after stop. ]} *)
    beep_on_overflow : nativeint;       (** {[ SVR4+BSD    C: IMAXBEL Ring bell when queue full. ]} *)
    lowercase : nativeint               (** {[ SVR4        C: IUCLC   Map upper case to lower case. ]} *)
  }
;
value ttyin : tty_in;
(** These are the named flags for the [tty_info] record's {i input_flags} field.
  These flags generally control the processing of input chars.  Only the Posix
  entries are guaranteed to be <> -1. *)

type tty_out =
  { enable : nativeint;                 (** {[ Posix    C: OPOST  Enable output processing. ]} *)
    nl_to_crnl : nativeint;             (** {[ Posix    C: ONLCR  Map nl to cr-nl. ]} *)
    discard_eot : nativeint;            (** {[ Posix    C: ONOEOT Discard EOT chars. ]} *)
    expand_tabs : nativeint;            (** {[ Posix    C: OXTABS Expand tabs. ]} *)
    cr_to_nl : nativeint;               (** {[ Posix    C: OCRNL  Map cr to nl. ]} *)
    nl_does_cr : nativeint;             (** {[ Posix    C: ONLRET Nl performs cr as well. ]} *)
    no_col0_cr : nativeint;             (** {[ Posix    C: ONOCR  No cr output in column 0. ]} *)
    delay_with_fill_char : nativeint;   (** {[ Posix    C: OFILL  Send fill char to delay. ]} *)
    fill_with_del : nativeint;          (** {[ Posix    C: OFDEL  Fill char is ASCII DEL. ]} *)
    uppercase : nativeint;              (** {[ Posix    C: OLCUC  Map lower to upper case. ]} *)
    bs_delay : nativeint;               (** {[ Backspace delay: Bit-field mask ]} *)
    bs_delay0 : nativeint;              (** {[ Backspace delay: values         ]} *)
    bs_delay1 : nativeint;
    cr_delay : nativeint;               (** {[ Carriage-return delay: Bit-field mask ]} *)
    cr_delay0 : nativeint;              (** {[ Carriage-return delay: values         ]} *)
    cr_delay1 : nativeint;
    cr_delay2 : nativeint;
    cr_delay3 : nativeint;
    ff_delay : nativeint;               (** {[ Form-feed delay: Bit-field mask ]} *)
    ff_delay0 : nativeint;              (** {[ Form-feed delay: values         ]} *)
    ff_delay1 : nativeint;
    tab_delay : nativeint;              (** {[ Horizontal-tab delay: Bit-field mask ]} *)
    tab_delay0 : nativeint;             (** {[ Horizontal-tab delay: values         ]} *)
    tab_delay1 : nativeint;
    tab_delay2 : nativeint;
    tab_delayx : nativeint;
    nl_delay : nativeint;               (** {[ Newline delay: Bit-field mask ]} *)
    nl_delay0 : nativeint;              (** {[ Newline delay: values         ]} *)
    nl_delay1 : nativeint;
    vtab_delay : nativeint;             (** {[ Vertical tab delay: Bit-field mask ]} *)
    vtab_delay0 : nativeint;            (** {[ Vertical tab delay: values         ]} *)
    vtab_delay1 : nativeint;
    all_delay : nativeint               (** {[ All: Total bit-field mask ]} *)
  }
;
value ttyout  : tty_out;
(** Output-flags (before bs_delay). These are the named flags for the [tty_info]
  record's {i output_flags} field.  These flags generally control the processing
  of output chars.  Only the Posix entries are guaranteed to be <> -1.

Then, delay constants. These are the named flags for the [tty_info] record's {i
output_flags} field. These flags control the output delays associated with
printing special characters.  They are non-Posix, and have values <> -1 only on
SVR4 systems. *)

type tty_c =
  { char_size : nativeint;              (** {[ Posix    C: CSIZE      Character size mask ]} *)
    char_size5 : nativeint;             (** {[ Posix    C: CS5        5 bits. ]} *)
    char_size6 : nativeint;             (** {[ Posix    C: CS6        6 bits. ]} *)
    char_size7 : nativeint;             (** {[ Posix    C: CS7        7 bits. ]} *)
    char_size8 : nativeint;             (** {[ Posix    C: CS8        8 bits. ]} *)
    enable_parity : nativeint;          (** {[ Posix    C: PARENB     Generate and detect parity. ]} *)
    odd_parity : nativeint;             (** {[ Posix    C: PARODD     Odd parity. ]} *)
    enable_read : nativeint;            (** {[ Posix    C: CREAD      Enable reception of chars. ]} *)
    hup_on_close : nativeint;           (** {[ Posix    C: HUPCL      Hang up on last close. ]} *)
    no_modem_sync : nativeint;          (** {[ Posix    C: LOCAL      Ignore modem lines. ]} *)
    two_stop_bits : nativeint;          (** {[ Posix    C: CSTOPB     Send two stop bits. ]} *)
    ignore_flags : nativeint;           (** {[ Posix    C: CIGNORE    Ignore control flags. ]} *)
    cts_output_flow_control : nativeint;(** {[ BSD      C: CCTS_OFLOW CTS flow control of output. ]} *)
    rts_input_flow_control : nativeint; (** {[ BSD      C: CRTS_IFLOW RTS flow control of output. ]} *)
    carrier_flow_ctl : nativeint        (** {[ BSD      C: MDMBUF      ]} *)
    }
;
value ttyc : tty_c;
(** Control-flags.  These are the named flags for the [tty_info] record's {i
  control_flags} field.  These flags generally control the details of the
  terminal's serial line.  Only the Posix entries are guaranteed to be <> -1. *)

type tty_l =
  { canonical : nativeint;                (** {[ Posix        C: ICANON     Canonical input processing. ]} *)
    echo : nativeint;                     (** {[ Posix        C: ECHO       Enable echoing. ]} *)
    echo_delete_lines : nativeint;        (** {[ Posix        C: ECHOK      Echo newline after line kill. ]} *)
    echo_nl : nativeint;                  (** {[ Posix        C: ECHONL     Echo newline even if echo is off. ]} *)
    visual_delete : nativeint;            (** {[ Posix        C: ECHOE      Visually erase chars. ]} *)
    enable_signals : nativeint;           (** {[ Posix        C: ISIG       Enable ^C, ^Z signalling. ]} *)
    extended : nativeint;                 (** {[ Posix        C: IEXTEN     Enable extensions. ]} *)
    no_flush_on_interrupt : nativeint;    (** {[ Posix        C: NOFLSH     Don't flush after interrupt. ]} *)
    ttou_signal : nativeint;              (** {[ Posix        C: TOSTOP     SIGTTOU on background output. ]} *)
    echo_ctl : nativeint;                 (** {[ SVR4+BSD     C: ECHOCTL    Echo control chars as "^X". ]} *)
    flush_output : nativeint;             (** {[ SVR4+BSD     C: FLUSHO     Output is being flushed. ]} *)
    hardcopy_delete : nativeint;          (** {[ SVR4+BSD     C: ECHOPRT    Visual erase for hardcopy. ]} *)
    reprint_unread_chars : nativeint;     (** {[ SVR4+BSD     C: PENDIN     Retype pending input. ]} *)
    visual_delete_line : nativeint;       (** {[ SVR4+BSD     C: ECHOKE     Visually erase a line-kill. ]} *)
    alt_delete_word : nativeint;          (** {[ BSD          C: ALTWERASE  Alternate word erase algorithm. ]} *)
    no_kernel_status : nativeint;         (** {[ BSD          C: NOKERNINFO No kernel status on ^T. ]} *)
    case_map : nativeint                  (** {[ SVR4         C: XCASE      Canonical case presentation. ]} *)
    }
;
value ttyl : tty_l;
(** Local-flags.  These are the named flags for the [tty_info] record's {i
local_flags} field.  These flags generally control the details of the
line-editing user interface.  Only the Posix entries are guaranteed to be <> -1. *)

(** {4 The speed fields} *)

(** The [input_speed] and [output_speed] fields determine the I/O rate of the
terminal's line.  The value of these fields is an integer giving the speed in
bits-per-second.  The following speeds are supported by Posix:
{v
                  0     134      600     4800 
                 50     150     1200     9600 
                 75     200     1800    19200
                110     300     2400    38400
v}

Your OS may accept others; there's currently no provision for the special values
[EXTA] and [EXTB]. *)

(** {4 The min and time fields} *)

(** The integer [min] and [time] fields determine input blocking behaviour
during non-canonical (raw) input; otherwise, they are ignored.  See the
termios(3) man page for further details.

Be warned that Posix allows the base system call's representation of the
[tty_info] record to share storage for the [min] field and the [ttychar.eof]
element of the control-characters string, and for the [time] field and the
[ttychar/eol] element of the control-characters string.  Many implementations in
fact do this.

To stay out of trouble, set the [min] and [time] fields only if you are putting
the terminal into raw mode; set the eof and eol control-characters only if you
are putting the terminal into canonical mode.  It's ugly, but it's Unix. *)

(* p 81. *)

(** {4 Using tty-info records} *)

value make_tty_info :
  nativeint -> nativeint -> nativeint -> nativeint -> int -> int -> int -> int ->
    tty_info;
value copy_tty_info : tty_info -> tty_info;
(** These procedures make it possible to create new [tty_info] records.
  The typical method for creating a new record is to copy one retrieved by a
  call to the [tty_info] procedure, then modify the copy as desired.  Note that
  the call [make_tty_info] {i input_flags output_flags control_flags local_flags
  ispeed ospeed min time} does not take a parameter to define the new record's
  control characters.

{i Why? Because the length of the string varies from Unix to Unix.  For example,
the word-erase control character (typically control-w) is provided by most
Unixes, but not part of the Posix spec.}  Instead, it simply returns a
[tty_info] record whose control-character string has all elements initialised to
ASCII nul.  You may then install the special characters by assigning to the
string.  Similarly, the control-character string in the record produced by
[copy_tty_info] does not share structure with the string in the record being
copied, so you may mutate it freely. *)

value tty_info_fd : fd -> tty_info;
value tty_info_in : in_channel -> tty_info;
value tty_info_out : out_channel -> tty_info;
value tty_info_fn : string -> tty_info;
(** The {i fd/channel/string} parameter is an integer file descriptor, Caml
channel opened on a terminal device, or a file-name for a terminal device.  This
procedure returns a [tty_info] record describing the terminal's current mode. *)

value set_tty_info_now_fd : fd -> tty_info -> unit;
value set_tty_info_now_in : in_channel -> tty_info -> unit;
value set_tty_info_now_out : out_channel -> tty_info -> unit;
value set_tty_info_now_fn : string -> tty_info -> unit;

value set_tty_info_drain_fd : fd -> tty_info -> unit;
value set_tty_info_drain_in : in_channel -> tty_info -> unit;
value set_tty_info_drain_out : out_channel -> tty_info -> unit;
value set_tty_info_drain_fn : string -> tty_info -> unit;

value set_tty_info_flush_fd : fd -> tty_info -> unit;
value set_tty_info_flush_in : in_channel -> tty_info -> unit;
value set_tty_info_flush_out : out_channel -> tty_info -> unit;
value set_tty_info_flush_fn : string -> tty_info -> unit;
(** The {i fd/channel/string} parameter is an integer file descriptor or Caml
channel opened on a terminal device, or a file-name for a terminal device.  The
The procedure chosen determines when and how the terminal's mode is altered:
{v
   set_tty_info_now_...         Make change immediately.
   set_tty_info_drain_...       Drain output, then change.
   set_tty_info_flush_...       Drain output, flush input, then change.
v}
*)

(* p 82. *)

(** {3 Other terminal-device procedures} *)

value send_tty_break_fd : ?duration: int -> fd -> unit;
value send_tty_break_in : ?duration: int -> in_channel -> unit;
value send_tty_break_out : ?duration: int -> out_channel -> unit;
value send_tty_break_fn : ?duration: int -> string -> unit;
(** The {i fd/channel/string} parameter is an integer file descriptor or Caml
  channel opened on a terminal device, or a file-name for a terminal device.
Send a break signal to the designated terminal.  A break signal is a sequence of
continuous zeros on the terminal's transmission line.

The {i duration} argument determines the length of the break signal.  A zero
value (the default) causes a break of between 0.25 and 0.5 seconds to be sent;
other values determine a period in a manner that will depend upon local
community standards. *)

value drain_tty_fd : fd -> unit;
value drain_tty_in : in_channel -> unit;
value drain_tty_out : out_channel -> unit;
value drain_tty_fn : string -> unit;
(** The {i fd/channel/string} parameter is an integer file descriptor or Caml
channel opened on a terminal device, or a file-name for a terminal device.

This procedure waits until all the output written to the terminal device has
been transmitted to the device.  If {i channel} is an out_channel with buffered
I/O enabled, then the port's buffered characters are flushed before waiting for
the device to drain. *)

value flush_tty_input_fd : fd -> unit;
value flush_tty_input_in : in_channel -> unit;
value flush_tty_input_out : out_channel -> unit;
value flush_tty_input_fn : string -> unit;

value flush_tty_output_fd : fd -> unit;
value flush_tty_output_in : in_channel -> unit;
value flush_tty_output_out : out_channel -> unit;
value flush_tty_output_fn : string -> unit;

value flush_tty_both_fd : fd -> unit;
value flush_tty_both_in : in_channel -> unit;
value flush_tty_both_out : out_channel -> unit;
value flush_tty_both_fn : string -> unit;
(** The {i fd/channel/string} parameter is an integer file descriptor or Caml
   channel opened on a terminal device, or a file-name for a terminal device.

These procedures discard the unread input chars or unwritten output chars in the
tty's kernel buffers. *)

value start_tty_output_fd : fd -> unit;
value start_tty_output_in : in_channel -> unit;
value start_tty_output_out : out_channel -> unit;
value start_tty_output_fn : string -> unit;

value stop_tty_output_fd : fd -> unit;
value stop_tty_output_in : in_channel -> unit;
value stop_tty_output_out : out_channel -> unit;
value stop_tty_output_fn : string -> unit;

value start_tty_input_fd : fd -> unit;
value start_tty_input_in : in_channel -> unit;
value start_tty_input_out : out_channel -> unit;
value start_tty_input_fn : string -> unit;

value stop_tty_input_fd : fd -> unit;
value stop_tty_input_in : in_channel -> unit;
value stop_tty_input_out : out_channel -> unit;
value stop_tty_input_fn : string -> unit;
(** These procedures can be used to control a terminal's input and output flow.
The {i fd/channel/string} parameter is an integer file descriptor or Caml
channel opened on a terminal device, or a file-name for a terminal device.

The [stop_tty_output_...] and [start_tty_output_...] procedures suspend and
resume output from a terminal device.  The [stop_tty_input_...] and
[start_tty_input_...] procedures transmit the special STOP and START characters
to the terminal with the intention of stopping and starting terminal input flow. *)

(* p 83. *)

(** {3 Control terminals, sessions, and terminal process groups} *)

value open_control_tty_in : ?flags: (list Io_3_2.open_flag) -> string -> in_channel;
value open_control_tty_out : ?flags: (list Io_3_2.open_flag) -> string -> out_channel;
(** This procedure opens terminal device {i tty_name} as the process' control
terminal (see the [termios] man page for more information on control terminals).
The {i tty_name} argument is a file-name such as [/dev/ttya].  The {i flags}
argument is a value suitable as the last argument to the [open_file] call; it
defaults to [O_RDWR] for [open_control_tty_in], causing the terminal to be
opened for both input and output, and [O_WRONLY] for [open_control_tty_out].

The channel returned is an in_channel if the {i flags} permit it, otherwise an
out_channel.  Ocaml do not have input/output channels, so it's one or the other.
However, you can get both read and write channels open on a terminal by opening
it read/write with [open_control_tty_in], taking the result in_channel, and
duping it to an output channel with [out_channel_of_dup_in].

This procedure guarantees to make the opened terminal the process' control
terminal only if the process does not have an assigned control terminal at the
time of the call.  If the scsh process already has a control terminal, the
results are undefined.

To arrange for the process to have no control terminal prior to calling this
procedure, use the [become_session_leader] procedure. *)

value become_session_leader : unit -> int;
(** This is the C [setsid()] call.  Posix job-control has a three-level
hierarchy: session/process-group/process.  Every session has an associated
control terminal.  This procedure places the current process into a brand new
session, and disassociates the process from any previous control terminal.  You
may subsequently use [open_control_tty] to open a new control terminal.

It is an error to call this procedure if the current process is already a
process-group leader.  One way to guarantee this is not the case is only to call
this procedure after forking. *)

value tty_process_group_fd : fd -> int;
value tty_process_group_in : in_channel -> int;
value tty_process_group_out : out_channel -> int;
value tty_process_group_fn : string -> int;

value set_tty_process_group_fd : fd -> int -> int;
value set_tty_process_group_in : in_channel -> int -> int;
value set_tty_process_group_out : out_channel -> int -> int;
value set_tty_process_group_fn : string -> int -> int;
(** These eight procedures get and set the process group of a given terminal. *)

(* p 84. *)

value control_tty_file_name : unit -> string;
(** Return the file-name of the process' control tty.  On every version of Unix
of which we are aware, this is just the string ["/dev/tty"].  However, this
procedure uses the official Posix interface, so it is more portable than simply
using a constant string. *)

(** {3 Pseudo-terminals} *)

(** Cash implements an interface to Berkeley-style pseudo-terminals. *)

value fork_pty_session :
  (unit -> unit) -> (Proc_3_4.proc * in_channel * out_channel * string);
(** [fork_pty_session] {i thunk} gives a convenient high-level interface to
pseudo-terminals.  It first allocates a pty/tty pair of devices, and then forks
a child to execute procedure {i thunk}.  In the child process
- Stdio and the current I/O channels are bound to the terminal device.
- The child is placed in its own, new session (see [become_session_leader]).
- The terminal device becomes the new session's controlling terminal (see
  [open_control_tty]).
- [stderr] is unbuffered.

The [fork_pty_session] procedure returns four values: the child's process
object, two channels open on the controlling pty device, and the name of the
child's corresponding terminal device. *)

value open_pty : unit -> (in_channel * string);
(** This procedure finds a free pty/tty pair, and opens the pty device with
read/write access.  It returns a channel on the pty, and the name of the
corresponding terminal device.

The channel returned is an input channel -- Caml doesn't allow input/output
channels.  However, you can easily use [out_channel_of_dup_in] {i
pty_in_channel} to produce a matching output channel.  You may wish to turn off
I/O buffering for this output channel. *)

value tty_name_of_pty_name : string -> string;
value pty_name_of_tty_name : string -> string;
(** These two procedures map between corresponding terminal and pty controller
names. For example,
{[   tty_name_of_pty_name "/dev/ptyq3"          => "/dev/ttyq3"
   pty_name_of_tty_name "/dev/ttyrc"          => "/dev/ptyrc"
]}

{i This is rather Berkeley-specific. SVR4 ptys are rare enough that I (Olin)
have no real idea if it generalises across the Unix gap. Experts are invited to
advise. Users feel free to not worry -- the predominance of current popular Unix
systems use Berkeley ptys. } *)

(* p 85. *)

value make_pty_generator : unit -> unit -> string;
(** [make_pty_generator ()] returns a generator of candidate pty names.  Each
time the returned procedure is called, it produces a new candidate.  Software
that wishes to search through the set of available ptys can use a pty generator
to iterate over them.  After producing all the possible ptys, a generator
raises [Not_found] every time it is called.  Example:
{[
   let pg = make_pty_generator ();
   pg ();                       => "/dev/ptyp0"
   pg ();                       => "/dev/ptyp1"
...
   pg ();                       => "/dev/ptyqe"
   pg ();                       => "/dev/ptyqf"
   pg ();                       => Not_found
   pg ();                       => Not_found
...
]}
*)

(* p 91. *)

(** {1 Networking} *)

(** The Caml Shell provides a BSD-style sockets interface.  There is not an
official standard for a network interface for Cash to adopt (this is the subject
of the forthcoming Posix.8 standard).  However, Berkeley sockets are a {i de
facto} standard, being found on most Unix workstations and PC operating systems.

It is fairly straightforward to add higher-level network protocols such as smtp,
telnet, or http on top of the the basic socket-level support Cash provides.  For
those who read scheme, the Scheme Underground has also released a network
library with many of these protocols as a companion to the current release of
Scsh.  See this code for examples showing the use of the sockets interface.  *)

(** {2 Sockets} *)

(* p 92. *)
(** A socket is one end of a network connection. Three specific properties of
sockets are specified at creation time: the protocol-family, type, and
protocol. *)

type socket_domain =
  Unix.socket_domain ==
    [ PF_UNIX
    | PF_INET ]
;
type protocol_family = socket_domain;
(** The [protocol_family] specifies the protocol family to be used with the
socket. This also determines the address family of socket addresses, which are
described in more detail below.  It is the same type as [Unix.socket_domain]. *)

(** The [socket_type] specifies the style of communication. Examples that your
operating system probably provides are stream and datagram sockets.  Others
maybe available depending on your system. Cash supports the same values as
[Unix.socket_type]. *)
type socket_type =
  Unix.socket_type ==
    [ SOCK_STREAM
    | SOCK_DGRAM
    | SOCK_RAW
    | SOCK_SEQPACKET ]
;

(** The {i protocol} specifies a particular protocol to use within a protocol
family and type. Usually only one choice exists, but it's probably safest to set
this explicitly. See the protocol database routines for information on looking
up protocol constants. *)
type protocol_level =
  Network_4.protocol_level ==
    [ SOL_SOCKET ]
;

(* p 93. *)
type socket =
  { family : protocol_family; sock_in : in_channel; sock_out : out_channel }
;
(** Type of the sockets.

The [family] specifies the protocol family of the socket. The [sock_in] and
[sock_out] fields are channels that can be used for input and output,
respectively. For a stream socket, they are only usable after a connection has
been established via {!Cash.connect_socket} or {!Cash.accept_connection}. For a
datagram socket, a socket can be immediately used by {!Cash.send_message}, and
{i sock_in} can be used after {!Cash.bind_socket} has created a local
address. *)

value create_socket : ?protocol: int -> protocol_family -> socket_type -> socket;
value create_socket_pair : socket_type -> (socket * socket);
(** New sockets are typically created with [create_socket].  However,
[create_socket_pair] can also be used to create a pair of connected sockets in
the [PF_UNIX] protocol-family. *)

value close_socket : socket -> unit;
(** [close_socket] provides a convenient way to close a socket's channel. It is
preferred to explicitly closing the sock_in and sock_out because using
[close_in] or [close_out] on sockets is not currently portable across operating
systems. *)

(** {2 Socket addresses} *)

type inet_addr = Unix.inet_addr;
(** The type of Internet hosts addresses. Besides being an opaque host address,
an Internet host address can also be one of the following constants: *)

value inet_addr_any : inet_addr;
value inet_addr_loopback : inet_addr;
value inet_addr_broadcast : inet_addr;
(** The use of [inet_addr_any] is described below in {!Cash.bind_socket}.
[inet_addr_loopback] is an address that always specifies the local
machine. [inet_addr_broadcast] is used for network broadcast communications.

For information on obtaining a host's address, see the {!Cash.host_info_name}
and {!Cash.host_info_addr} functions below. *)

(** The format of a socket-address depends on the address family of the
socket. Address-family-specific routines are provided to convert
protocol-specific addresses to socket addresses. The value returned by
these routines has type sockaddr (an alias of [Unix.sockaddr]). *)
type sockaddr =
  Unix.sockaddr ==
    [ ADDR_UNIX of string
    | ADDR_INET of inet_addr and int ]
;

(* p 94. *)
value socket_address_of_unix_address : string -> sockaddr;
(** [socket_address_of_unix_address] {i pathname} returns a {i socket-address}
based on the string {i pathname}. There is a system dependent limit on the
length of {i pathname}. *)

value socket_address_of_internet_address : inet_addr -> int -> sockaddr;
(** [socket_address_of_internet_address] {i host_address service_port} returns a
{i socket_address} based on an {i host_address} and an integer {i service_port}. *)

value sockaddr_of_host_and_port : string -> int -> sockaddr;
value sockaddr_of_host_and_service : string -> string -> sockaddr;
(** At a slightly higher level of interface, you can also give an host name and
  a port number (or service name) to one of these two procedures, which
  resolve the given name(s) to make an Internet socket address. *)

value unix_address_of_socket_address : sockaddr -> string;
value internet_address_of_socket_address : sockaddr -> (inet_addr * int);
(** These routines return the address-family-specific addresses.  Be aware that
most implementations don't correctly return anything more than an empty string
for addresses in the [ADDR_UNIX] address-family. *)

(* p 91. *)
(** {2 High-level interface} *)

(** For convenience, and to avoid some of the messy details of the socket
interface, we provide a high level socket interface. These routines attempt to
make it easy to write simple clients and servers without having to think of many
of the details of initiating socket connections.  We welcome suggested
improvements to this interface, including better names, which right now are
solely descriptions of the procedure's action.  This might be fine for people
who already understand sockets, but does not help the new networking
programmer. *)

(* p 91. *)

value socket_connect : sockaddr -> socket_type -> unit;
(** [socket_connect] {i socket_address socket_type} is intended for creating
client applications.  [socket_connect] returns a [socket] which can be used for
input and output from a remote server.  *)

(* p 92. *)
value bind_listen_accept_loop_unix : string -> (socket -> sockaddr -> unit) -> unit;
value bind_listen_accept_loop_port : int -> (socket -> sockaddr -> unit) -> unit;
value bind_listen_accept_loop_service : string -> (socket -> sockaddr -> unit) -> unit;

(** [bind_listen_accept_loop_...] {i what proc} is intended for creating server
applications. {i what} tells what to connect to.  {i proc} is a procedure whose
arguments: a [socket] and a [sockaddr], are made from {i what}.

[bind_listen_accept_loop_unix] {i path} uses a path to make the [socket] in the
[PF_UNIX] protocol-family.

[bind_listen_accept_loop_port] {i port} makes the [socket] in the [PF_INET]
protocol-family.  You may use a service name instead with
[bind_listen_accept_loop_service] {i service}.

{i proc} is called with a socket and a socket address each time there is a
connection from a client application. The socket allows communications with the
client.  The socket address specifies the address of the remote client.

This procedure does not return, but loops indefinitely accepting connections
from client programs. *)

(** {2 Socket primitives} *)

(** The procedures in this section are presented in the order in which a
typical program will use them. Consult a text on network systems
programming for more information on sockets.
Some recommended ones are:
- ``Unix Network Programming'' by W. Richard Stevens
- ``An Introductory 4.3BSD Interprocess Communication Tutorial.''  (reprinted in
  UNIX Programmer's Supplementary Documents Volume 1, PS1:7)
- ``An Advanced 4.3BSD Interprocess Communication Tutorial.''  (reprinted in
  UNIX Programmer's Supplementary Documents Volume 1, PS1:8)

The last two tutorials are freely available as part of BSD. In the absence of
these, your Unix manual pages for socket might be a good starting point for
information. *)

(* p 95. *)
value connect_socket : socket -> sockaddr -> unit;
(** [connect_socket] {i socket socket-address} sets up a connection from a {i
socket} to a remote {i socket-address}. A connection has different meanings
depending on the socket type. A stream socket must be connected before use. A
datagram socket can be connected multiple times, but need not be connected at
all if the remote address is specified with each [send_message], described
below. Also, datagram sockets may be disassociated from a remote address by
connecting to a null remote address. *)

value bind_socket : socket -> sockaddr -> unit;
(** [bind_socket] {i socket socket-address} assigns a certain local {i
socket-address} to a {i socket}. Binding a socket reserves the local address. To
receive connections after binding the socket, use [listen_socket] for stream
sockets and [receive_message] for datagram sockets.

Binding an Internet socket with a host address of [inet_addr_any] indicates that
the caller does not care to specify from which local network interface
connections are received. Binding an Internet socket with a service port number
of zero indicates that the caller has no preference as to the port number
assigned.

Binding a socket in the Unix address family creates a socket special file in the
file system that must be deleted before the address can be reused. See
[delete_file]. *)

value listen_socket : socket -> int -> unit;
(** [listen_socket] {i socket backlog} allows a stream {i socket} to start
receiving connections, allowing a queue of up to {i backlog} connection
requests. Queued connections may be accepted by [accept_connection]. *)

value accept_connection : socket -> (socket * sockaddr);
(** [accept_connection] receives a connection on a {i socket}, returning
a new socket that can be used for this connection and the remote socket
address associated with the connection. *)

value socket_local_address : socket -> sockaddr;
value socket_remote_address : socket -> sockaddr;
(** Sockets can be associated with a local address or a remote address or
both. [socket_local_address] returns the local {i sockaddr} record associated
with {i socket}. [socket_remote_address] returns the remote {i sockaddr} record
associated with {i socket}. *)

type shutdown_command =
  Unix.shutdown_command ==
    [ SHUTDOWN_RECEIVE
    | SHUTDOWN_SEND
    | SHUTDOWN_ALL ]
;

(* p 96. *)
value shutdown_socket : socket -> shutdown_command -> unit;
(** [shutdown_socket] {i how_to} shuts down part of a full-duplex socket.  The
part to shut down is specified by the {i how_to} argument. *)

(** {2 Performing input and output on sockets}*)

type msg_flag =
  Unix.msg_flag ==
    [ MSG_OOB (* Out of band. *)
    | MSG_DONTROUTE
    | MSG_PEEK ]
;

value receive_message : ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);
value receive_message_bang :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
    (int * sockaddr);
value receive_message_partial :
  ?flags: (list msg_flag) -> socket -> int -> (string * sockaddr);
value receive_message_bang_partial :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> socket -> string ->
    (int * sockaddr);
value send_message :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr -> socket ->
    string -> unit;
value send_message_partial :
  ?start: int -> ?end_: int -> ?flags: (list msg_flag) -> ?sockaddr: sockaddr -> socket ->
    string -> int;

(** For most uses, standard input and output routines such as [read_string] and
[write_string] should suffice.  However, in some cases an extended interface is
required. The [receive_message] and [send_message] calls parallel the
[read_string] and [write_string] calls with a similar naming scheme.

One additional feature of these routines is that [receive_message] returns the
remote {i socket-address} and {i send-message} takes an optional remote
[socket_address]. This allows a program to know the source of input from a
datagram socket and to use a datagram socket for output without first connecting
it.

All of these procedures take an optional {i ~flags} field. This argument is a
list of [msg_flag]'s.

See [read_string_in] and [write_string] for a more detailed description of the
arguments and return values.  *)

(** {2 Socket options} *)

(** [socket_option_...] and [set_socket_option_...] allow the inspection and
modification, respectively, of several options available on sockets. The {i
protocol_level} argument specifies what protocol level is to be examined or
affected. A level of [SOL_SOCKET] specifies the highest possible level that is
available on all socket types. A specific protocol number can also be used as
provided by [protocol_info], described below.

There are several different classes of socket options: *)

(** The first class consists of boolean options which can be either true or
false. *)
type socket_bool_option =
  Unix.socket_bool_option ==
    [ SO_DEBUG
    | SO_BROADCAST
    | SO_REUSEADDR
    | SO_KEEPALIVE
    | SO_DONTROUTE
    | SO_OOBINLINE
    | SO_ACCEPTCONN ]
;

(** Value options are another category of socket options. Options of this kind
are an integer value. *)
type socket_int_option =
  Unix.socket_int_option ==
    [ SO_SNDBUF
    | SO_RCVBUF
    | SO_ERROR
    | SO_TYPE
    | SO_RCVLOWAT
    | SO_SNDLOWAT ]
;

(** A third option kind specifies how long for data to linger after a socket has
been closed. There is only one option of this kind. It is set with either [None]
to disable it or ([Some] integer) number of seconds to linger and return a value
of the same type upon inspection.
 *)
type socket_optint_option =
  Unix.socket_optint_option ==
    [ SO_LINGER ]
;

(** The fourth and final option kind of this time is a timeout option. There are
two values of this option, for sending or receiving. These are set with a real
number of microseconds resolution and return a real value upon inspection.
 *)
type socket_float_option =
  Unix.socket_float_option ==
    [ SO_RCVTIMEO
    | SO_SNDTIMEO ]
;

(* p 97. *)
value socket_option_bool : socket -> protocol_level -> socket_bool_option -> bool;
value set_socket_option_bool :
  socket -> protocol_level -> socket_bool_option -> bool -> unit;

value socket_option_int : socket -> protocol_level -> socket_int_option -> int;
value set_socket_option_int :
  socket -> protocol_level -> socket_int_option -> int -> unit;

value socket_option_optint :
  socket -> protocol_level -> socket_optint_option -> option int;
value set_socket_option_optint :
  socket -> protocol_level -> socket_optint_option -> option int -> unit;

value socket_option_float : socket -> protocol_level -> socket_float_option -> float;
value set_socket_option_float :
  socket -> protocol_level -> socket_float_option -> float -> unit;

(* p 98. *)

(** {2 Database-information entries} *)

(** [host_info_...] could fail and raise the following error for one of these
reasons.
 *)
type herror =
  [ HOST_NOT_FOUND
  | TRY_AGAIN
  | NO_RECOVERY
  | NO_DATA
  | NO_ADDRESS ]
;

exception Netdb_error of herror;

type host_info =
  Unix.host_entry ==
    { h_name : string;(** Host name. *)
      h_aliases : array string;(** Alternative names. *)
      h_addrtype : protocol_family;
      h_addr_list : array inet_addr (** Host addresses. *)
    }
;
(** [host_info_...] return a value of this type. *)

value host_info_name : string -> host_info;
value host_info_addr : sockaddr -> host_info;
(** [host_info_...] allow a program to look up a host entry based on either its
string {i name} or {i socket_address}. *)

type network_info =
  { n_name : string;(** Network name. *)
    n_aliases : array string;(** Alternative names. *)
    n_addrtype : protocol_family;
    n_net : int32(* Network number. *)
  }
;
(** [network_info_...] return a value of this type. *)

value network_info_name : string -> network_info;
value network_info_addr : sockaddr -> network_info;
(** [network_info_...] allow a program to look up a network entry based on
either its string {i name} or {i socket_address}. *)

type service_info =
  Unix.service_entry ==
    { s_name : string;(** Service name. *)
      s_aliases : array string;(** Alternative names. *)
      s_port : int;(** Port number. *)
      s_proto : string(** Protocol name. *)
    }
;
(** [service_info_...] return a value of this type. *)

value service_info_name : ?protocol: string -> string -> service_info;
value service_info_port : ?protocol: string -> int -> service_info;
(** [service_info_...] allow a program to look up a service entry based on
either its string {i name} or integer {i port}. *)

type protocol_info =
  Unix.protocol_entry == 
    { p_name : string;(** Protocol name. *)
      p_aliases : array string;(** Alternative names. *)
      p_proto : int(** Protocol number. *)
    }
;
(** [protocol_info_...] return a value of this type. *)

value protocol_info_name : string -> protocol_info;
value protocol_info_port : int -> protocol_info;
(** [protocol_info_...] allow a program to look up a protocol entry based on
either its string {i name} or integer {i port}. *)

(** {1 Strings and characters} *)

(** Strings are the basic communication medium for Unix processes, so a Unix
programming environment must have reasonable facilities for manipulating them.
Cash provides a powerful set of procedures for processing strings and
characters.  Besides the the facilities described in this chapter, Cash also
provides:
- {b Field parsing, delimited record I/O} (chapter {!Cash.record_io_and_field_parsing}).
   These procedures let you read in chunks of text delimited by selected
   characters, and parse each record into fields based on regular expressions
   (for example, splitting a string at every occurrence of colon or
   white-space).
- {b The SRFI-13 string libraries.}
   This pair of libraries contains procedures that create, fold, iterate over,
   search, compare, assemble, cut, hash, case-map, and otherwise manipulate
   strings.  They are provided by the String_13 and String_13_internals modules,
   and are also available in the default {!Cash} module.  More documentation on
   these procedures can be found at URLs
   {{:http://srfi.schemers.org/srfi-13/srfi-13.html} (html)} and
   {{:http://srfi.schemers.org/srfi-13/srfi-13.txt} (text)}.
*) (** (Oops: the SRFI-13 libraries are not implemented for now)
- {b The SRFI-14 character-set library}.
   This library provides a set-of-characters abstraction, which is frequently
   useful when searching, parsing, filtering or otherwise operating on strings
   and character data. The SRFI is provided by the Charset_14 module; its
   bindings are also available in the default {!Cash} module.
   More documentation on this library can be found at URLs
   {{:http://srfi.schemers.org/srfi-14/srfi-14.html} (html)} and
   {{:http://srfi.schemers.org/srfi-14/srfi-14.txt} (text)}.
 *)

(** {2:filenames Manipulating file names} *)

(** These procedures do not access the file-system at all; they merely operate
on file-name strings. Much of this structure is patterned after the GNU Emacs
design. Perhaps a more sophisticated system would be better, something like the
pathname abstractions of CommonLisp or MIT Scheme. However, being Unix-specific,
we can be a little less general.  *)

(** {3 Terminology} *)

(** These procedures carefully adhere to the Posix standard for file-name
resolution, which occasionally entails some slightly odd things.  This section
will describe these rules, and give some basic terminology.

A {e file-name} is either the file-system root (``/''), or a series of
slash-terminated directory components, followed by a a file component.  Root is
the only file-name that may end in slash.  Some examples:
{v
  File name            Dir components        File component
  src/des/main.c       \["src"; "des"\]        "main.c"
  /src/des/main.c      \[""; "src"; "des"\]    "main.c"
  main.c               \[\]                    "main.c" v}

Note that the relative filename {b src/des/main.c} and the absolute filename {b
/src/des/main.c} are distinguished by the presence of the root component {b ""}
in the absolute path.

Multiple embedded slashes within a path have the same meaning as a single slash.
More than two leading slashes at the beginning of a path have the same meaning
as a single leading slash --- they indicate that the file-name is an absolute one,
with the path leading from root.  However, Posix permits the OS to give special
meaning to {e two} leading slashes.  For this reason, the routines in this
section do not simplify two leading slashes to a single slash.

A file-name in {e directory form} is either a file-name terminated by a slash,
{i e.g}., ``{b /src/des/}'', or the empty string, ``''.  The empty string
corresponds to the current working directory, whose file-name is dot (``{b
.}'').  Working backwards from the append-a-slash rule, we extend the syntax of
Posix file-names to define the empty string to be a file-name form of the root
directory ``{b /}''.  (However, ``{b /}'' is also acceptable as a file-name form
for root.)  So the empty string has two interpretations: as a file-name form, it
is the file-system root; as a directory form, it is the current working
directory.  Slash is also an ambiguous form: {b /} is both a directory-form and
a file-name form.

The directory form of a file-name is very rarely used.  Almost all of the
procedures in Cash name directories by giving their file-name form (without the
trailing slash), not their directory form.  So, you say ``{b /usr/include}'',
and ``{b .}'', not ``{b /usr/include/}'' and ``''.  The sole exceptions are
[file_name_as_directory] and [directory_as_file_name], whose jobs are to convert
back-and-forth between these forms, and [file_name_directory], whose job it is
to split out the directory portion of a file-name.  However, most procedures
that expect a directory argument will coerce a file-name in directory form to
file-name form if it does not have a trailing slash.  Bear in mind that the
ambiguous case, empty string, will be interpreted in file-name form, i.e., as
root. *)

(** {3 Procedures} *)

(* p 102. *)
value is_file_name_directory : string -> bool;
value is_file_name_non_directory : string -> bool;
(** These predicates return true if the string is in directory form, or
  file-name form (see the above discussion of these two forms).  Note that they
  both return true on the ambiguous case of empty string, which is both a
  directory (current working directory), and a file name (the file-system root).

{v
File name       is_..._directory        is_..._non_directory}
"src/des"       false                   true
"src/des/"      true                    false
"/"             true                    false
"."             false                   true
""              true                    true
 v}
 *)

value file_name_as_directory : string -> string;
(** Convert a file-name to directory form.
  Basically, add a trailing slash if needed:
{[   file_name_as_directory "src/des"             =>  "src/des/"
   file_name_as_directory "src/des/"            =>  "src/des/"
]}
[.], [/], and [""] are special:
{[   file_name_as_directory "."                   =>  ""
   file_name_as_directory "/"                   =>  "/"
   file_name_as_directory ""                    =>  "/"
]}
*)

(* p 103. *)
value directory_as_file_name : string -> string;
(** Convert a directory to a simple file-name.
  Basically, kill a trailing slash if one is present:
{[   directory_as_file_name "foo/bar/"            => "foo/bar"
]}
[/] and [""] are special:
{[   directory_as_file_name "/"                   => "/"
   directory_as_file_name ""                    => "."  (* i.e., the cwd *)
]}
*)

value is_file_name_absolute : string -> bool;
(** Does {i fname} begin with a root or [~] component?
  (Recognising [~] as a home-directory specification is an extension of Posix
  rules.)
{[   is_file_name_absolute "/usr/shivers"         => true
   is_file_name_absolute "src/des"              => false
   is_file_name_absolute "~/src/des"            => true ]}
Non-obvious case:
{[   is_file_name_absolute ""                     => true (* i.e., root *) ]}
 *)

value file_name_directory : string -> string;
(** Return the directory component of {i fname} in directory form.
  If the file-name is already in directory form, return it as-is.
{[   file_name_directory "/usr/bdc"               => "/usr/"
   file_name_directory "/usr/bdc/"              => "/usr/bdc/"
   file_name_directory "bdc/.login"             => "bdc" ]}
Root has no directory component:
{[   file_name_directory "/"                      => ""
   file_name_directory ""                       => "" ]}
*)
value file_name_nondirectory : string -> string;
(** Return non-directory component of fname.
{[   file_name_nondirectory "/usr/ian"            => "ian"
   file_name_nondirectory "/usr/ian/"           => ""
   file_name_nondirectory "ian/.login"          => ".login"
   file_name_nondirectory "main.c"              => "main.c"
   file_name_nondirectory ""                    => ""
   file_name_nondirectory "/"                   => "/"
]}
*)
(* p 104. *)
value split_file_name : string -> list string;
(** Split a file-name into its components.
{[   split_file_name "src/des/main.c"             => ["src"; "des"; "main.c"]
   split_file_name "/src/des/main.c"            => [""; "src"; "des"; "main.c"]
   split_file_name "main.c"                     => ["main.c"]
   split_file_name "/"                          => [""]
]} *)

value file_name_of_path_list : ?dir: string -> list string -> string;
(** Inverse of split_file_name.
{[   file_name_of_path_list ["src"; "des"; "main.c"] => "src/des/main.c"
   file_name_of_path_list [""; "src"; "des"; "main.c"] => "/src/des/main.c"
]}
Optional {i ~dir} arg anchors relative path-lists:
{[   file_name_of_path_list ~dir:"/usr/shivers" ["src"; "des"; "main.c"]
                                                => "/usr/shivers/src/des/main.c"
]}
The optional {i ~dir} argument is usefully [(cwd ())]. *)

value file_name_extension : string -> string;
(** Return the file-name's extension.
{[   file_name_extension "main.c"                 => ".c"
   file_name_extension "main.c.old"             => ".old"
   file_name_extension "/usr/shivers"           => ""
]}
Weird cases:
{[   file_name_extension "foo."                   => "."
   file_name_extension "foo.."                  => "."
]}
Dot files are not extensions:
{[   file_name_extension ".login"                 => "" ]} *)

(* p 105. *)
value file_name_sans_extension : string -> string;
(** Return everything but the extension.
{[   file_name_sans_extension "main.c"            => "main"
   file_name_sans_extension "main.c.old"        => "main.c"
   file_name_sans_extension "/usr/shivers"      => "/usr/shivers"
]}
Weird cases:
{[   file_name_sans_extension "foo."              => "foo"
   file_name_sans_extension "foo.."             => "foo."
]}
Dot files are not extensions:
{[   file_name_sans_extension "/usr/shivers/.login" => "/usr/shivers/.login" ]}
Note that appending the results of [file_name_extension] and
[file_name_sans_extension] in all cases produces the original file-name. *)

value parse_file_name : string -> (string * string * string);
(** Let {i f} be [file_name_nondirectory] {i fname}.  This function returns the
  three values:
- [file_name_directory] {i fname}
- [file_name_sans_extension] {i f}
- [file_name_extension] {i f}

The inverse of [parse_file_name], in all cases, is [String.concat ""].
The boundary case of [/] was chosen to preserve this inverse.
*)
value replace_extension : string -> string -> string;
(** [replace_extension] {i fname} {i ext} replaces {i fname}'s extension with {i ext}.
  It is exactly equivalent to:

  [(file_name_sans_extension] {i fname} [) ^] {i ext} *)

(* p 106. *)
value simplify_file_name : string -> string;
(** Removes leading and internal occurrences of dot.
    A trailing dot is left alone, as the parent could be a symlink.  Removes
    internal and trailing double-slashes.  A leading double-slash is left alone,
    in accordance with Posix.  However, triple and more leading slashes are
    reduced to a single slash, in accordance with Posix.  Double-dots (parent
    directory) are left alone, in case they come after symlinks or appear in a
    [/../]{i machine}[/...] ``super-root'' form (which Posix permits). *)
value resolve_file_name : ?dir: string -> string -> string;
(**
{ul
 {- Do [~] expansion.}
 {- If {i ~dir} is given, convert a relative file-name to an absolute file-name,
  relative to directory {i ~dir}.}
} *)
value expand_file_name : ?dir: string -> string -> string;
(** Resolve and simplify the file-name. *)

value absolute_file_name : ?dir: string -> string -> string;
(** [absolute_file_name ~dir:]{i dir} {i fname} converts file-name {i fname}
into an absolute file name, relative to directory {i ~dir}, which defaults to
the current working directory. The file name is simplified before being
returned.

This procedure does not treat a leading tilde character specially. *)

value home_dir : ?user: string -> unit -> string;
(** Returns {i ~user}'s home directory. {i ~user} defaults to the current user.
{[   home_dir ()                                  => "/user1/lecturer/shivers"
   home_dir ~user:"ctkwan"                      => "/user0/research/ctkwan"
]}
*)
value home_file : ?user: string -> string -> string;
(** Returns file-name {i fname} relative to {i ~user}'s home directory;
{i ~user} defaults to the current user.
{[   home_file "man"                              => "/usr/shivers/man"
   home_file ~user:"fcmlau" "man"               => "/usr/fcmlau/man"
]}
*)

(** The general [substitute_env_vars] string procedure, defined in the next
section, is also frequently useful for expanding file-names. *)

(** {3 Other string manipulation facilities} *)

(* p 107. *)
value substitute_env_vars : string -> string;
(** Replace occurrences of environment variables with their values.
  An environment variable is denoted by a dollar sign followed by alphanumeric
  chars and underscores, or is surrounded by braces.
{[   substitute_env_vars "$USER/.login"           => "shivers/.login"
   substitute_env_vars "${USER}_log"            => "shivers_log" ]}
 *)

(** The four next procedures are convenience alternatives to the [String] ones. *)

value index : ?from: int -> string -> char -> option int;
(** This is like [String.index] and [String.index_from] altogether ({i ~from}
  defaults to 0), but it never raises [Not_found]: instead, it packages their
  result in an option type; (note: you can use [Env_3_11.internal_index] to get
  -1 if the char is not found).
*)
value rindex : ?from: int -> string -> char -> option int;
(** Same with [rindex] (and [Env_3_11.internal_rindex]); {i ~from} defaults to
  the length of the string. *)

value substring : string -> int -> int -> string;
(** This is like [String.sub] but uses two indices in the string, instead of
  the start position and the length to search. {i This is gratuitous Scheme
  compatibility.}  *)

value xsubstring : string -> int -> int -> string;
(** This eXtended substring accepts negative indices, meaning to count from the
  end of the string: -1 is the last char, so xsubstring s (-2) (-1) extracts the
  last char.  If you look at the indices as being between the characters, 0 is
  before the first one, and -1 after the last. *)

(** {3 Character predicates} *)

value is_letter : char -> bool;
value is_lower_case : char -> bool;
value is_upper_case : char -> bool;
value is_title_case : char -> bool;
value is_digit : char -> bool;
value is_letter_or_digit : char -> bool;
value is_graphic : char -> bool;
value is_printing : char -> bool;
value is_whitespace : char -> bool;
value is_blank : char -> bool;
value is_iso_control : char -> bool;
value is_punctuation : char -> bool;
value is_hex_digit : char -> bool;
value is_ascii : char -> bool;

(** Each of these predicates tests for membership in one of the standard
character sets provided by the SRFI-14 character-set library (module
Charset_14).  Additionally, the following redundant bindings are provided for
R5RS compatibility: *)

value is_alphabetic : char -> bool;(** == is_letter_or_digit. *)
value is_alphanumeric : char -> bool;(** == is_letter_or_digit. *)
value is_numeric : char -> bool;(** == is_digit. *)

(** {1:rdelim Reading delimited strings} *)

(** Cash provides a set of procedures that read delimited strings from input
channels.  There are procedures to read a single line of text (terminated by a
newline character), a single paragraph (terminated by a blank line), and general
delimited strings (terminated by a character belonging to an arbitrary character
set).

All of the delimited input operations described below take a [handle_delim]
parameter, which determines what the procedure does with the terminating
delimiter character.  There are three plus one possible choices for a
[handle_delim] parameter: *)

(* p 137. *)
type handle_delim =
  [ Trim
  | Peek
  | Concat ]
;

(** The fourth option is to use a ..._split version of the procedure, that
  return delimiter as second value (so the return type is not compatible with
  the standard version).

The first case, [Trim], is the standard default for all the routines described
in this section.  The last three cases allow the programmer to distinguish
between strings that are terminated by a delimiter character, and strings that
are terminated by an end-of-file.
 *)

(** Type of the second value returned by {!Cash.low_read_delimited_bang} and
  the ..._split procedures.  *)
type termination_kind =
  [ Eof (** Read terminated by end of file *)
  | Read of char (** Read terminated by this delimiter *)
  | Full_buffer ] (** Filled buffer without finding a delimiter *)
;

value read_line : ?handle_newline: handle_delim -> in_channel -> string;
(** Reads and returns one line of text; on eof, raises End_of_file.
  A line is terminated by newline or eof.

  {i handle_newline} determines what [read_line] does with the newline or EOF
  that terminates the line; it defaults to [Trim] (discard the newline).  Using
  this argument allows one to tell whether or not the last line of input in a
  file is newline terminated.
 *)
value read_line_split : in_channel -> (string * termination_kind);
(** Same as [read_line], but returns separately the line and the delimiter
   (maybe [Eof]).  [Full_buffer] can't happen. *)

(* p 138. *)
value read_paragraph : ?handle_delim: handle_delim -> in_channel -> string;
value read_paragraph_split : in_channel -> (string * string);
(** These procedures skip blank lines, then read text from a channel until a
  blank line or eof is found.  A ``blank line'' is a (possibly empty) line
  composed only of white space.  The {i ~handle_delim} parameter (or using
  [read_paragraph_split]) determines how the terminating blank line is handled.
  It is described above, and defaults to [Trim].  The [Peek] option is not
  available. *)

(** The following procedures read in strings from channels delimited by
characters belonging to a specific set.  See the
{{:http://srfi.schemers.org/srfi-14/srfi-14.html}character set library
specification} for information on character set manipulation.  *)

value read_delimited :
  ?chan: in_channel -> ?handle_delim: handle_delim -> Charset_14.any_t -> string;
value read_delimited_split :
  ?chan: in_channel -> Charset_14.any_t -> (string * termination_kind);
(** Read until we encounter one of the chars in {i charset} or eof.
    The {i ~handle_delim} parameter (or using [read_delimited_split]) determines
    how the terminating character is handled. It is described above, and
    defaults to [Trim].

    The {i char_set} argument may be a charset, a string, a character, or a
    character predicate; it is coerced to a charset.

    [Full_buffer] can't happen to [read_delimited_split]. *)

value read_delimited_bang :
  ?chan: in_channel -> ?handle_delim: handle_delim -> ?start: int -> ?end_: int ->
    Charset_14.any_t -> string -> option int;
value read_delimited_bang_split :
  ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string ->
    option (int * termination_kind);
(** Side-effecting variants of [read_delimited].

    The data is written into the string {i buf} at the indices in the half-open
    interval \[{i start,end_}); the default interval is the whole string: {i
    start} = 0 and {i end_} = ([String.length] {i buf}).  The values of {i
    start} and {i end_} must specify a well-defined interval in {i str}, i.e., 0
    <= {i start} <= {i end_} <= ([String.length] {i buf}).

    [read_delimited_bang] returns [Some] {i nbytes}, the number of bytes
    read. If the buffer filled up without a delimiter character being found,
    [None] is returned. If the channel is at eof when the read starts,
    End_of_file is raised.

    If the read is successfully terminated by reading a delimiter character ({i
    i.e.,} [read_delimited_bang] returns [Some] integer, or
    [read_delimited_bang_split] returns ({i n}, [Char] {i c}), then the {i
    ~handle_delim} parameter (or using [read_delimited_bang_split]) determines
    how the terminating character is handled.  It is described above, and
    defaults to [Trim].
 *)

value low_read_delimited_bang :
  ?chan: in_channel -> ?start: int -> ?end_: int -> Charset_14.any_t -> string ->
    handle_delim -> (termination_kind * int);
(** This low-level delimited reader uses an alternate interface.
It returns two values: {i terminator} and {i num_read}.
- {i terminator}: A [termination_kind] value describing why the read was terminated.
- {i num_read}: Number of characters read into {i buf}.

If the read is successfully terminated by reading a delimiter character, then
the {i handle_delim} parameter determines what to do with the terminating
character.  If [Peek], the character is left in the input stream where a
subsequent read operation will retrieve it; else the character is removed from
the input stream: if [Trim], it is not copied in the buffer; if [Concat], it is
put in the buffer.  In either case, the character is also the first value
returned by the procedure call.

Invariants:
- {i terminator} = [Full_buffer] => {i num_read = end_ - start}.
- {i terminator} = [Eof] and {i num_read} = 0 => at EOF.
*)

(* p 139. *)
value skip_char_set : ?chan: in_channel -> Charset_14.any_t -> int;
(** [skip_char_set] {i skip_chars} skips characters occurring in the set {i
  skip_chars}, and returns the number of characters skipped.  The {i skip_chars}
  argument may be a charset, a string, a character, or a character predicate; it
  is coerced to a charset. *)

(** {1:record_io_and_field_parsing Record I/O and field parsing} *)

(** Unix programs frequently process streams of records, where each record is
delimited by a newline, and records are broken into fields with other delimiters
(for example, the colon character in [/etc/passwd]).  Cash has procedures that
allow the programmer to easily do this kind of processing.  Cash's field parsers
can also be used to parse other kinds of delimited strings, such as
colon-separated [$PATH] lists. *)

(* XX pas d'awk, donc pas de sous-section {2 Record I/O and field parsing} *)

(** The procedures in this section are used to read records from I/O streams and
parse them into fields.  A record is defined as text terminated by some
delimiter (usually a newline).  A record can be split into fields by using
regular expressions in one of several ways: to {e match} fields, to {e separate}
fields, or to {e terminate} fields.  The field parsers can be applied to
arbitrary strings (one common use is splitting environment variables such as
[$PATH] at colons into its component elements).

The general delimited-input procedures described in chapter {!Cash.rdelim} are
also useful for reading simple records, such as single lines, paragraphs of
text, or strings terminated by specific characters.
*)

(* p 141. *)

(** {2 Reading records} *)

value record_reader :
  ?delims: Charset_14.any_t -> ?elide_delims: bool ->
    ?handle_delim: Delim_7.handle_delim -> unit -> in_channel -> string;

value record_reader_split :
  ?delims: Charset_14.any_t -> ?elide_delims: bool -> unit -> in_channel ->
    (string * string);
(** [record_reader] {i ~delims ~elide_delims ~handle_delim} [()] returns a
 procedure that reads records from a channel.

A record is a sequence of characters terminated by one of the characters in {i
delims} or eof.  The {i delims} set defaults to the set \{'\n'\}.  It may be a
charset, string, character, or character predicate, and is coerced to a charset.

If {i elide_delims} is [true], then a contiguous sequence of delimiter chars are
taken as a single record delimiter.  If {i elide_delims} is [false] (the
default), then a delimiter char coming immediately after a delimiter char
produces an empty-string record. The reader consumes the delimiting char(s)
before returning from a read.

The {i handle_delim} argument (or using [record_reader_split]) controls what is
done with the record's terminating delimiter.  It has the same meaning as for
the procedures of chapter {!Cash.rdelim}, except there is no Peek option:
- [Trim]: Delimiters are trimmed (the default).
- [Concat]:  The record and its delimiter are returned as a single string.

When using [record_reader_split], the reader returns delimiter string as a
second argument. If record is terminated by EOF, then the null string is
returned as this second argument.

The reader procedure returned takes one argument, the channel from which to
read. It returns a string or raises [End_of_file].

To emphasize that these procedures are normally used to make a reader procedure,
they take a unit argument after the optionals, which is not necessary from a
strict typing point of view.  It's easier to write:
{[    let read = record_reader ~handle_delim:Concat () ]}
than be forced to use:
{[    let read = record_reader ?delims:None ?elide_delims:None ~handle_delim:Concat ]}

Moreover, [record_reader] does a non-trivial amount of work to make a faster
reader procedure; it is not efficient to use [ record_reader () channel ] in a
tight loop --- this would be even less noticeable if one could write
[record_reader channel]. *)

(** {2:field_splitter Parsing fields} *)

(** Another handle_delim type reserved for field spliting. *)
type handle_field_delim =
  [ Trim_f      (** Delimiters are thrown away after parsing (default). *)
  | Split_f     (** Delimiters are appended to the field preceding them. *)
  | Concat_f ]  (** Delimiters are returned as separate elements in the field list. *)
;

(** The many ways to specify how to match fields or delimiters. *)
type delim_matcher =
  [ Match_proc of string -> int -> (int * int)
      (** A function of a string, searching from an int position, that returns
      the offsets of the next match, {i i.e}, the indices of its first
      character, and of the first character following it. *) 
  | String of string (** A litteral string. *)
  | Charset of Charset_14.any_t (** A Charset. *)
  | Regexp of Pcre.regexp (** A compiled regexp. *)
  | Pattern of string ] (** The string denotation of a regexp. *)
;

value field_splitter :
  ?field: delim_matcher -> ?num_fields: int -> unit -> ?start: int -> string ->
    list string;
value infix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;
value suffix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;
value sloppy_suffix_splitter :
  ?delim: delim_matcher -> ?num_fields: int -> ?handle_delim: handle_field_delim ->
    unit -> ?start: int -> string -> list string;
(** These functions return a parser function that can be used as follows:
{[    parser ~start string            => string list ]}

The returned parsers split strings into fields defined by regular expressions.
You can parse by specifying a pattern that {e separates} fields, a pattern that
{e terminates} fields, or a pattern that {e matches} fields: 
- [field_splitter]: matches fields
- [infix_splitter]: separates fields
- [suffix_splitter]: terminates fields
- [sloppy_suffix_splitter]: terminates fields

These parser generators are controlled by a range of options, so that you can
precisely specify what kind of parsing you want. However, these options default
to reasonable values for general use.

Defaults:
- {i ~num_fields}: none (as many fields as possible)
- {i ~handle_delim}: [Trim_f] (discard delimiter chars)
- {i ~field, ~delim}: bindings are explicitly available...
*)

(* p 142. *)
value default_field_matcher : delim_matcher;
(** The default value of {i ~field} arg to [field_splitter]: "\S+"
  (non-white-space). *)

value default_infix_matcher : delim_matcher;
(** The default value of {i ~delim} arg to [infix_splitter]: "\s+" (white space) *)

value default_suffix_matcher : delim_matcher;
(** The default value of {i ~delim} arg to [suffix_splitter] and
  [sloppy_suffix_splitter]: "\s+|\z" (white space or eos) *) 

(** These defaults mean: break the string at white space, discarding the white
  space, and parse as many fields as possible.

The {i delim} parameter is a regular expression matching the text that occurs
between fields.  In the separator case, it defaults to a pattern matching white
space; in the terminator case, it defaults to white space or end-of-string.

The {i field} parameter is a regular expression used to match fields. It
defaults to non-white-space.

The {i delim} patterns may be given as a matching procedure, a litteral string,
a charset, an un-compiled regexp pattern, which are all coerced to compiled
regular expressions.  So the following expressions are all equivalent, each
producing a function that splits strings apart at colons:
{[
    infix_splitter ~delim:(String ":") ();
    infix_splitter ~delim:(Charset_14.of_string ":") ();
    infix_splitter ~delim:(Regexp (Pcre.regexp ":")) ();
    infix_splitter ~delim:(Pattern ":") ();
    infix_splitter
      ~delim:(Match_proc
         (fun s pos -> let i = String.index_from s pos ':' in i, i + 1))
      ();;
]}

The {i handle_delim} determines what to do with delimiters.  See
{!Cash.handle_field_delim}.

The {i num_fields} argument used to create the parser specifies how many fields
to parse.  If unspecified, the procedure parses them all.  If a positive integer
[n], exactly that many fields are parsed; it is an error if there are more or
fewer than [n] fields in the record. If {i num_fields} is a negative integer or
zero, then [|n|] fields are parsed, and the remainder of the string is returned
in the last element of the field list; it is an error if fewer than [|n|] fields
can be parsed.

The field parser produced is a procedure that can be employed as follows:
{[     parse ~start string            => string list ]}
The optional {i start} argument (default 0) specifies where in the string to
begin the parse. It is an error if {i start} [> String.length] {i string}.
*)(**
The parsers returned by the four parser generators implement different kinds of
field parsing:
{ul {- [field_splitter]: The regular expression specifies the actual field.}
{- [suffix_splitter]: Delimiters are interpreted as element {e terminators}. 
 If vertical-bar is the the delimiter, then the string [""] is the empty record
 [()], ["foo|"] produces a one-field record [("foo")], and ["foo"] is an error.
 [("foo")], and ["foo"] is an error.

 The syntax of suffix-delimited records is:

 {i <record>} [::=] {i ""} (empty record) [|] {i <element> <delim> <record>}

 It is an error if a non-empty record does not end with a delimiter.  To make
 the last delimiter optional, make sure the delimiter regexp matches the
 end-of-string (["\z"]). }
{- [infix_splitter]: Delimiters are interpreted as element {e separators}. If
 comma is the delimiter, then the string ["foo,"] produces a two-field record
 [("foo" "")].

 The syntax of infix-delimited records is:

 {i <record>} [::=] {i ""} (forced to be empty record) [|] {i <real-infix-record>}

 {i <real-infix-record>} [::=]  {i <element> <delim> <real-infix-record>} [|] {i <element>}

 Note that separator semantics doesn't really allow for empty records --- the
 straightforward grammar ({i i.e.}, {i <real-infix-record>}) parses an empty
 string as a singleton list whose one field is the empty string, [("")], not as
 the empty record [()]. This is unfortunate, since it means that infix string
 parsing doesn't make [^] and [@] isomorphic. For example, with
{[ let split = infix_splitter ~delim:(String ":") (); ]}
{[
 split (x ^ ":" ^ y) ]}
 doesn't always equal
{[ (split x) @ (split y) ]}
 It fails when {i x} or {i y} are the empty string.  Terminator semantics {e
 does} preserve a similar isomorphism.

 However, separator semantics is frequently what other Unix software uses, so to
 parse their strings, we need to use it. For example, Unix [$PATH] lists have
 separator semantics. The path list ["/bin:"] is broken up into [("/bin" "")],
 not [("/bin")].  Comma-separated lists should also be parsed this way. }
{- [sloppy_suffix_splitter]: The same as the [suffix] case, except that the
 parser will skip an initial delimiter string if the string begins with one
 instead of parsing an initial empty field. This can be used, for example, to
 field-split a sequence of English text at white-space boundaries, where the
 string may begin or end with white space, by using regexp:
{[ "\s+|\z" ]}
 (But you would be better off using [field_splitter] in this case.)
}
} 

The next table shows how the different parser grammars split apart the same
strings.  Having to choose between the different grammars requires you to decide
what you want, but at least you can be precise about what you are parsing. Take
fifteen seconds and think it out. Say what you mean; mean what you say.

{v
Record            : suffix          :|$ suffix        : infix           non-: field
""                []                []                []                []
":"               [""]              [""]              [""; ""]          []
"foo:"            ["foo"]           ["foo    "]       ["foo"; ""]       ["foo"]
":foo"            error             [""; "foo"]       [""; "foo"]       ["foo"]
"foo:bar"         error             ["foo"; "bar"]    ["foo"; "bar"]    ["foo"; "bar"]
v}
*)

(** {3 Field readers} *)

(* p 145. *)
value field_reader :
  ?field_parser: (?start: int -> string -> list string) ->
    ?rec_reader: (in_channel -> string) -> unit -> in_channel -> (string * list string);
(** This utility returns a procedure that reads records with field structure
  from a channel. The reader is used as follows:
{[ reader channel           => (raw-record, parsed-record) ]}

When the reader is applied to an input channel, it reads a record using {i
rec_reader}.  This record is parsed with {i field_parser}. These two values ---
the record, and its parsed representation --- are returned as a pair from the
reader.

When called at eof, the reader raises [End_of_file].

For example, if channel [p] is open on [/etc/passwd], then
 {[ let field_parser = infix_splitter ~delim:(String ":") ~num_fields:7 () in
 let parse = field_reader ~field_parser () in
 parse p;; ]}
returns two values:
{[
("dalbertz:mx3Uaqq0:107:22:David Albertz:/users/dalbertz:/bin/csh",
["dalbertz"; "mx3Uaqq0"; "107"; "22"; "David Albertz"; "/users/dalbertz"; "/bin/csh"])
]} 

The {i rec_reader} defaults to [read_line]. 
*)

value default_field_parser : ?start: int -> string -> list string;
(** The  default value of the {i ~field_parser} argument to [field_reader]:
  it is [field_splitter ()], a parser that picks out sequences of
  non-white-space strings.
*)

value gen_field_reader :
  ('record -> 'fields) -> ('chan -> 'record) -> 'chan -> ('record * 'fields);
(** Although the record reader typically returns a string, and the field-parser
 typically takes a string argument, this is not required. The record reader can
 produce, and the field-parser consume, values of any type. However, the types
 of defaults arguments to [field_reader] constrain its type.  So you can use
 this alternate version; its standard use is to be partially applied to 2
 arguments, returning a reader like [field_reader].  See examples below.
*)

(** Some examples of field_reader:
{[    (* /etc/passwd reader. *)
    let passwd_reader =
      field_reader ~field_parser:(infix_splitter ~delim:(String ":") ~num_fields:7 ()) ()
      (* wandy:3xuncWdpKhR.:73:22:Wandy Saetan:/usr/wandy:/bin/csh. *)
    ;;
    (* Two ls -l output readers. *)
    let ls_long_reader =
      field_reader ~field_parser:(infix_splitter ~delim:(Pattern "\s+") ~num_fields:8 ()) ()
      (* -rw-r--r--  1 shivers    22880 Sep 24 12:45 scsh.scm *)
    ;;
    let ls_long_with_blanks_in_filenames_reader =
      field_reader ~field_parser:(infix_splitter ~delim:(Pattern "\s+") ~num_fields:(-7) ()) ()
      (* -rw-r--r--  1 shivers        8 Sep 24 12:45 who am I *)
    ;;
    (* Internet hostname reader. *)
    let hostname_reader =
      field_reader ~field_parser:(field_splitter ~field:(Pattern "[^.]+") ()) ()
      (* stat.sinica.edu.tw *)
    ;;
    (* Internet IP address reader. *)
    let numeric_IP_address_reader =
      field_reader ~field_parser:(field_splitter ~field:(Pattern "[^.]+") ~num_fields:4 ()) ()
      (* 18.24.0.241 *)
    ;;
    (* Line of integers. *)
    let parse_num = field_splitter ~field:(Pattern "[-+]?\d+") ();;
    let line_of_ints_reader =
      let field_parser s = List.map int_of_string (parse_num s) in
      gen_field_reader field_parser read_line
      (* 18 24 0 241 *)
    ;;
    (* Same as above. *)
    let another_line_of_ints_reader =
      let read = field_reader ~field_parser:parse_num () in
      fun chan -> let (record, fields) = (read chan) in (record, List.map int_of_string fields)
      (* Yale beat harvard 26 to 7. *)
    ;;
]}
*)

(** {3 Forward-progress guarantees and empty-string matches} *)

(** A loop that pulls text off a string by repeatedly matching a regexp against
that string can conceivably get stuck in an infinite loop if the regexp matches
the empty string. For example, the regexps ["\A"], ["\z"], [".*"], and
["foo|[^f]*"] can all match the empty string.

The routines in this package that iterate through strings with regular
expressions are careful to handle this empty-string case.  If a regexp matches
the empty string, the next search starts, not from the end of the match (which
in the empty string case is also the beginning --- that's the problem), but from
the next character over.  This is the correct behaviour. Regexps match the
longest possible string at a given location, so if the regexp matched the empty
string at location {i i}, then it is guaranteed it could not have matched a longer
pattern starting with character {i i}. So we can safely begin our search for the
next match at char {i i + 1}.

With this provision, every iteration through the loop makes some forward
progress, and the loop is guaranteed to terminate.

This has the effect you want with field parsing. For example, if you split a
string with the empty pattern, you will explode the string into its individual
characters:
{[ (suffix_splitter ~delim:(String "") ()) "foo"               => [""; "f"; "o"; "o"] ]}

However, even though this boundary case is handled correctly, we don't recommend
using it. Say what you mean --- just use a field splitter:
{[ (field_splitter ~field:(Pattern ".") ()) "foo"               => ["f"; "o"; "o"] ]}
*)

(** {1 Running Cash} *)

(** There are several different ways to invoke cash.  You can run it as an
interactive Ocaml system, with a standard read-eval-print interaction loop.

Cash can also be invoked as the interpreter for a shell script by putting a
``[#!/usr/local/bin/cash]'' line at the top of the shell script.

Descending a level, it is also possible to compile to byte- or native code, with
or without -custom for byte-code executables.

This chapter will cover these various ways of invoking cash programs, from
bigger/faster to smaller/slower methods. *) 

(** {2:true_exec Making true executables} *)

(** You just use cash as a library.  Standard linking is as follow:

[ocamlc -custom] {i other options} [unix.cma pcre.cma cash.cma] {i other files}

[ocamlopt] {i other options} [unix.cmxa pcre.cmxa cash.cmxa] {i other files}

This gives fast startup, plus faster execution with ocamlopt.  With ocamlc -g,
and the proper environment variable OCAMLRUNPARAM, you get backtraces.  You can
use ocamldebug. *)

(** {2:byte_exec Making bytecode-only executables} *)

(** You don't link in the runtime system:

[ocamlc] {i other options, no -custom} [unix.cma pcre.cma cash.cma] {i other files}

Same as byte-code executable of the preceding section {!Cash.true_exec}, but the
bytecode executable is smaller, the startup time negligibly longer.  This works
only if none of the libraries used has been compiled with the -custom flag.
*)

(** {2 Cash scripts} *)

(** Caml scripts (so does Cash) use a toplevel to compile on the fly, then
execute source scripts.  You just put an [#!/usr/local/bin/cash] as the first
line of the source.  This gives you minimal footprint, but you have longer
startup-time because of the compilation to byte-code prior to execution.  As
toplevels don't know how to dump backtraces on unhandled exceptions, you can't
get one this way (see {!Cash.true_exec} and {!Cash.byte_exec}).

If you're concerned by startup time and disk usage, but don't care of using 2
files for a script, you can make a compromise by doing the following:
You first compile you source to a .cmo
  [ocamlc] {i options} [-c myscript.ml]
but you don't link it.  Then you write a wrapper in a file named, say, myscript:
{[
  #!/usr/local/bin/cash
  #load "myscript.cmo";;
]}
and you make it executable with:
  [chmod +x myscript]

Then you don't get type errors afterwards, don't pay for compilation at each
execution of the script, and link no byte-code caml library, as the toplevel
already has them in -- unix.cma and especially cash.cma aren't that small.
*) (**
Now we get at OS limits concerning [#!] lines:

First, the standard cash toplevel is itself (in the OS terminology) an ocamlrun
script (ocamlrun being termed ``interpreter''), as its first line is a [#!] line
too.  But generally, Unix/Linux OSes don't allow such ``scripts'' to be used
themselves in another [#!] line.  The solution to overcome this limit is easy:
make a cash toplevel with -custom, so, by including the ocamlrun executable, it
becomes a true executable to the OS' eyes.

The second limit is that some Unices truncate the [#!] line to some ever too
short size, and/or limit the number of arguments that can be added there
(generally no less than one after the ``interpreter'' name) to an ever too small
number, and do so either silently, or with an imprecise error code (./myscript:
No such file or directory, {i e.g.}, though ./myscript does exist).  And you
sometimes need those -unsafe or -w flags to your script.

There we use a solution from Scsh: the toplevel itself is named cashtop, and we
use a so-called ``trampoline'' to start it, named cash (there are versions with
Camlp4 revised syntax, resp. cashrtop and cashr).  Cash is a true executable, so
a custom toplevel isn't necessary anymore (but can be used to regain some dozens
of ms of startup time).  It doesn't support cashtop options, but use the second
line of the script to gather them.  So if your script starts like this: {[
  #!/usr/local/bin/cash
  cashtop arguments ...
  !#
]}
i.e. the 3d line is ``[!#\n]'', the second line will be parsed as described below,
to make the arguments to cashtop. *)

(** {4 Secondary argument syntax} *)

(** Cash uses a very simple grammar to encode the extra arguments on the second line
of the cash script.  The only special characters are space, tab, newline, and
backslash.

- Each space character terminates an argument.  This means that two spaces in a
  row introduce an empty-string argument.
- The tab character is not permitted (unless you quote it with the backslash
  character described below).  This is to prevent the insidious bug where you
  believe you have six space characters, but you really have a tab character,
  and {e vice-versa}.
- The newline character terminates an argument, like the space character, and
  also terminates the argument sequence.  This means that an empty line parses
  to the singleton list whose one element is the empty string: ([""]).  The
  grammar doesn't admit the empty list.
- The backslash character is the escape character.
    It escapes backslash, space, tab, turning off their special functions, and
    allowing them to be included in arguments.  The Ansi C escape sequences
    ([\b], [\n], [\r] and [\t]) are also supported; these also produce
    argument-constituents -- [\n] doesn't act like a terminating newline.
    Backslash followed by other chars is not allowed (so we can extend the
    escape-code space later if we like).

You have to construct these line-two argument lines carefully.  In particular,
beware of trailing spaces at the end of the line---they'll give you extra
trailing empty-string arguments.  Here's an example:
{[
#!/bin/interpreter \
foo bar  quux\ yow\end{verbatim}
]}
would produce the arguments
 [\["foo"; "bar"; ""; "quux yow"\]]
*)

(** {2 Cash switches} *)

(** The cash trampoline takes command-line switches in this format:
{[
    $ ./cash -help
    Usage: cash [switches] [--] [scriptfile] [arguments]
    switches are:
      -v            tell about syscalls; and don't unlink -c tempfile
      -c <code>     execute code. Several -c allowed. Omit <scriptfile>
      -sfd <num>    like -c, but code is read on file descriptor num
      --            end my switches
      -help  display this list of options
]}

In the tradition of sh -c, or sed -e, you can give a program text as one or more
arguments (that are strictly concatenated):
{[
    $ cash -c 'print_endline "hello world";;'
    hello world
]}
or even:
{[
    $ /cash -c 'print_endline "hello' -c ' world";;'
    hello world
]}
if you understand enough the quoting syntax of your shell.

You can read a program text from a file descriptor too, by using the -sfd
switch.  For example, to read a script from standard input, use -sfd 0.  You can
use -sfd several times, and mix with -c: all the fragments are concatenated in
the order of the switches.

*)
