/***********************************************************************)
(*                                Cash                                 *)
(*                                                                     *)
(*          Bruno Verlyck, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Cash is based on Scsh, by Olin Shivers.                            *)
(***********************************************************************/

#include <unistd.h>
#include <string.h>
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "otherlibs/unix/unixsupport.h"

extern char ** environ;

static char ** last_alloced_env = 0;

void free_last_env ()
{
  if (last_alloced_env != 0) {
    char ** p;

    for (p = last_alloced_env; *p != NULL; p++) {
      stat_free (*p);
    }
  }
  stat_free (last_alloced_env);
  last_alloced_env = NULL;
}

CAMLprim value cash_set_environ_of_list (value env)
{
  value p;
  char ** newenv;
  int len = 1;                          /* 1 for final NULL */

  free_last_env ();
  for (p = env; p != Val_emptylist; p = Field (p, 1)) {
    ++len;
  }

  newenv = stat_alloc (len * sizeof (char *));
  last_alloced_env = environ = newenv;

  for (p = env; p != Val_emptylist; p = Field (p, 1)) {
    value s = Field (p, 0);
    char * t = stat_alloc (1 + string_length (s));

    strcpy (t, String_val (s));
    *newenv++ = t;
  }
  *newenv = NULL;

  return Val_unit;
}

CAMLprim value cash_set_environ (value env)
{
  char ** newenv;
  int len = Wosize_val (env);

  free_last_env ();
  newenv = stat_alloc ((len + 1) * sizeof (char *)); /* +1 for final NULL */
  last_alloced_env = environ = newenv;

  newenv[len] = NULL;
  while (len-- > 0) {
    value s = Field (env, len);
    char * t = stat_alloc (1 + string_length (s));

    strcpy (t, String_val (s));
    *newenv++ = t;
  }

  return Val_unit;
}

CAMLprim value cpu_clock_ticks_per_sec ()
{
#ifdef _SC_CLK_TCK
  static long clock_tick = 0;

  if (clock_tick == 0)
    clock_tick = sysconf (_SC_CLK_TCK); /* POSIX.1, POSIX.2 */
  return Val_int (clock_tick);
#else
#include <time.h>
#ifdef CLK_TCK
  return Val_int (CLK_TCK);
#else
  return Val_int (60);
#endif
#endif
}

#include "byterun/io.h"
#include "byterun/fail.h"
#include "byterun/alloc.h"
#include <time.h>

CAMLprim value cash_tzname (value isdst)
{
  return copy_string (tzname[Int_val(isdst)]);
}

CAMLprim value cash_timezone ()
{
  return Val_int (timezone);
}

/* external tzset : unit -> unit = "tzset" doesn't always work */
CAMLprim value cash_tzset ()
{
  tzset ();
  return Val_unit;
}

#include <errno.h>

/* Oops
** There's a fundamental problem with the Posix mktime() function used below
** -- it's error return value (-1) is also a valid return value, for date
**     11:59:00 UTC, 12/31/1969
**
** 1. We choose to err on the paranoid side. If mktime() returns -1, it is
**    considered an error.
** 2. If we return an error, we try to return a useful errno value, if we can.
**
** Who designed this interface?
*/

CAMLprim value cash_mktime (value t, value summer)
{
  struct tm tm;
  time_t clock;
  value clkval = Val_unit;

  Begin_roots1(clkval);
    tm.tm_sec = Int_val(Field(t, 0));
    tm.tm_min = Int_val(Field(t, 1));
    tm.tm_hour = Int_val(Field(t, 2));
    tm.tm_mday = Int_val(Field(t, 3));
    tm.tm_mon = Int_val(Field(t, 4));
    tm.tm_year = Int_val(Field(t, 5));
    tm.tm_wday = Int_val(0);
    tm.tm_yday = Int_val(0);
    tm.tm_isdst = Int_val(summer);
    errno = 0;
    clock = mktime (&tm);
    if (clock == (time_t) -1)
      {
        if (!errno)
          errno = ERANGE;
        uerror ("Cash.time", Nothing);
      }
    clkval = copy_double ((double) clock);
  End_roots ();
  return clkval;
}

CAMLprim value cash_strftime (value buflen, value fmt, value t, value summer)
{
  int len = Int_val (buflen);
  char * s = stat_alloc (len);
  struct tm tm;
  unsigned int rlen;
  value res;

  tm.tm_sec = Int_val(Field(t, 0));
  tm.tm_min = Int_val(Field(t, 1));
  tm.tm_hour = Int_val(Field(t, 2));
  tm.tm_mday = Int_val(Field(t, 3));
  tm.tm_mon = Int_val(Field(t, 4));
  tm.tm_year = Int_val(Field(t, 5));
  tm.tm_wday = Int_val(Field(t, 9));
  tm.tm_yday = Int_val(Field(t, 10));
  tm.tm_isdst = Int_val(summer);
  rlen = strftime (s, len, String_val (fmt), &tm);
  if (rlen > len) failwith ("format_date: pb");
  s[rlen] = '\0';                       /* in case rlen = 0 */
  res = copy_string (s);
  stat_free (s);
  return res;
}

#include <stdio.h>

extern struct channel * all_opened_channels;

/* Must test fd >= 0 before calling in_use. */
static struct channel * in_use (int fd)
{
  struct channel * channel, * chan;

  for (channel = all_opened_channels; channel != NULL; channel = channel->next)
    if (channel->fd == fd)
      {                                 /* Check for several channels on the same fd */
        for (chan = channel->next; chan != NULL; chan = chan->next)
          if (chan->fd == fd)
            {
              char buf[150];
              sprintf (buf, "Several channels use file descriptor %d; %s",
                       fd, "use {in,out}channel_of_fd instead of Unix.descr_of_{in,out}_channel");
              invalid_argument (buf);
            };
        return channel;
      }
  return NULL;
}

extern value unix_set_close_on_exec(value fd);
extern value unix_clear_close_on_exec(value fd);

CAMLprim value io_move_chan (value new_fd, struct channel * channel, value niew_revealed)
{
  int nfd = Int_val (new_fd);
  int new_revealed = Int_val (niew_revealed);

  if (nfd < 0) invalid_argument ("move_chan");
  if (channel->fd != nfd) {
    if (in_use (nfd)) return Val_false;
    channel->fd = nfd;
  }
  /* Unreveal the port by shifting the revealed count
     over to the old-revealed count. */
  channel->old_revealed = channel->revealed;
  channel->revealed = new_revealed;
  if (new_revealed == 0)
    unix_set_close_on_exec (new_fd);
  return Val_true;
}

/* with_port */
CAMLprim value io_push_chan (value channel, struct channel * new_low_chan)
{
  CAMLparam1 (channel);
  struct channel * old_low_chan = Channel (channel);

  old_low_chan->refcount--;
  Channel (channel) = new_low_chan;
  new_low_chan->refcount++;
  CAMLreturn ((value) old_low_chan);
}

extern void finalize_channel(value vchan);

/* set_port */
CAMLprim value io_set_chan (value channel, struct channel * new_low_chan)
{
  struct channel * old_low_chan =
    (struct channel *)io_push_chan (channel, new_low_chan);

  /* One could use xx_channel_of_fd if not closed. */
  if (old_low_chan->refcount == 0
      /* However, there's little chance if the fd is unrevealed; it must have been shifted */
      && old_low_chan->revealed == 0)   /* since channel is (std{in,out,err}) */
                                        /* Note: no flush, no close... XXX */
    finalize_channel (channel);         /* will --refcount, should do no harm */

  return Val_unit;
}

#define None_val Val_int(0)
#define Some_tag 0

CAMLprim value maybe_fdes2chan (value fd)
{
  CAMLparam1 (fd);
  CAMLlocal1 (res);
  struct channel * chan = in_use (Int_val (fd));

  if (chan == NULL) CAMLreturn (None_val);
  res = alloc_small (1, Some_tag);
  Field (res, 0) = (value) chan;

  CAMLreturn (res);
}

CAMLprim value channel_of_chan (value chan)
{
  return (value) Channel (chan);
}

extern value alloc_channel(struct channel * chan);

CAMLprim value out_channel_of_channel (struct channel * channel)
{
  if (channel->max != NULL) invalid_argument ("ochan_of_low_chan");
  return alloc_channel (channel);
}

CAMLprim value in_channel_of_channel (struct channel * channel)
{
  if (channel->max == NULL) invalid_argument ("ichan_of_low_chan");
  return alloc_channel (channel);
}

CAMLprim value chan_revealed_count (value chan)
{
  struct channel * channel = Channel (chan);

  return Val_int (channel->revealed);
}

CAMLprim value release_chan_handle (value chan)
{
  struct channel * channel = Channel (chan);
  int rev = channel->revealed;

  if (rev == 0)
    channel->old_revealed--;
  else {
    if (--channel->revealed == 0)       /* ; We just became unrevealed, so */
      unix_set_close_on_exec (channel->fd); /* ; the fd can be closed on exec. */
  }
  return Val_unit;
}

CAMLprim value increment_revealed_count (struct channel * channel)
{
  channel->revealed++;
  if (channel->revealed == 1)           /* ; We just became revealed, so */
    unix_clear_close_on_exec (channel->fd); /* ; don't close on exec (). */
  return Val_unit;
}

CAMLprim value channel_of_fd (value chan, value revealed)
{
  struct channel * channel = Channel (chan);

  channel->revealed = Val_int (revealed);
  if (channel->revealed == 0)
    unix_set_close_on_exec (channel->fd);
  return chan;
}

extern value caml_open_descriptor_in (value fd);
extern value caml_open_descriptor_out (value fd);

CAMLprim value in_channel_of_fd (value fd, value revealed)
{
  return channel_of_fd (caml_open_descriptor_in (Val_int (fd)), revealed);
}

CAMLprim value out_channel_of_fd (value fd, value revealed)
{
  return channel_of_fd (caml_open_descriptor_out (Val_int (fd)), revealed);
}

CAMLprim value cash_charset_skip_to (value charset, value buf, value from, value end)
{
  unsigned char * cs = String_val (charset), * str = String_val (buf);
  int i = Int_val (from), upto = Int_val (end);

  while (i < upto) {
    if (cs [str [i]]) break;
    i++;
  }
  return Val_int (i);
}

CAMLprim value cash_charset_skip (value charset, value buf, value from, value end)
{
  unsigned char * cs = String_val (charset), * str = String_val (buf);
  int i = Int_val (from), upto = Int_val (end);

  while (i < upto) {
    if (cs [str [i]] == 0) break;
    i++;
  }
  return Val_int (i);
}

CAMLextern int do_read(int fd, char * p, unsigned int n);

static long read_delimited_scan(struct channel * channel, unsigned char * cs)
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        memmove(channel->buff, channel->curr, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no delimiter was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = do_read(channel->fd, channel->max, channel->end - channel->max);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (cs[(unsigned char) *p++] == 0);
  /* Found a delimiter. Return the length of the line, delimiter included. */
  return (p - channel->curr);
}

CAMLprim value cash_read_delimited_scan(value vchannel, value delims)
{
  struct channel * channel = Channel(vchannel);
  unsigned char * cs = String_val (delims);
  long res;

  Lock(channel);
  res = read_delimited_scan(channel, cs);
  Unlock(channel);
  return Val_long(res);
}

/* XX rendre la taille précédente ? */
/* On pourrait faire realloc si refcount = 1 donc ne pas se limiter à IO_BUFFER_SIZE */
/* mais ça saute en ocaml-3.04 si new_size < IO_BUFFER_SIZE */
CAMLprim value cash_set_buffer_size (value chan, value size)
{
  struct channel * channel = Channel (chan);
  int new_size = Int_val (size);

  if (new_size == 0) new_size = IO_BUFFER_SIZE;
  /* Check new_size is not over used space */
  if ((unsigned) new_size > IO_BUFFER_SIZE
      || new_size < (channel->max == NULL
                     ? channel->curr - channel->buff /* in_channel */
                     : channel->max - channel->curr)) /* out_channel */
    invalid_argument ("set_chan_buffering");
  /* Shift unread chars at the beginning */
  if (channel->max != NULL && channel->curr != channel->buff)
    memmove(channel->buff, channel->curr, channel->max - channel->curr);

  channel->end = &channel->buff[new_size];
  return Val_unit;
}

CAMLprim value cash_input_buf_empty (value chan)
{
  struct channel * channel = Channel (chan);

  return Val_int (channel->curr >= channel->max);
}

CAMLprim value cash_output_buf_full (value chan)
{
  struct channel * channel = Channel (chan);

  return Val_int (channel->curr >= channel->end);
}

CAMLprim value cash_sync ()
{
  sync ();
  return Val_unit;
}

#include <netdb.h>

static int h_errno_values[] = {
  HOST_NOT_FOUND, TRY_AGAIN, NO_RECOVERY, NO_DATA, NO_ADDRESS
};

CAMLprim value get_h_errno (value syscall)
{
  int i, herr;

  for (herr = h_errno, i = 0; i < (sizeof (h_errno_values) / sizeof (int)); i++)
    {
      if (herr == h_errno_values[i])
        return Int_val (i);
    }
  if (errno)
    uerror (String_val (syscall), Nothing);
  raise_not_found ();
}

#include <sys/socket.h>

#ifndef SO_DEBUG
#define SO_DEBUG (-1)
#endif
#ifndef SO_BROADCAST
#define SO_BROADCAST (-1)
#endif
#ifndef SO_REUSEADDR
#define SO_REUSEADDR (-1)
#endif
#ifndef SO_KEEPALIVE
#define SO_KEEPALIVE (-1)
#endif
#ifndef SO_DONTROUTE
#define SO_DONTROUTE (-1)
#endif
#ifndef SO_OOBINLINE
#define SO_OOBINLINE (-1)
#endif
#ifndef SO_ACCEPTCONN
#define SO_ACCEPTCONN (-1)
#endif
#ifndef SO_SNDBUF
#define SO_SNDBUF (-1)
#endif
#ifndef SO_RCVBUF
#define SO_RCVBUF (-1)
#endif
#ifndef SO_ERROR
#define SO_ERROR (-1)
#endif
#ifndef SO_TYPE
#define SO_TYPE (-1)
#endif
#ifndef SO_RCVLOWAT
#define SO_RCVLOWAT (-1)
#endif
#ifndef SO_SNDLOWAT
#define SO_SNDLOWAT (-1)
#endif
#ifndef SO_LINGER
#define SO_LINGER (-1)
#endif
#ifndef SO_RCVTIMEO
#define SO_RCVTIMEO (-1)
#endif
#ifndef SO_SNDTIMEO
#define SO_SNDTIMEO (-1)
#endif

static int sockopt_levels[] = { SOL_SOCKET };

static int sockopt_bool[] = {
  SO_DEBUG, SO_BROADCAST, SO_REUSEADDR, SO_KEEPALIVE,
  SO_DONTROUTE, SO_OOBINLINE, SO_ACCEPTCONN };

static int sockopt_int[] = {
  SO_SNDBUF, SO_RCVBUF, SO_ERROR, SO_TYPE, SO_RCVLOWAT, SO_SNDLOWAT };

static int sockopt_optint[] = { SO_LINGER };

static int sockopt_float[] = { SO_RCVTIMEO, SO_SNDTIMEO };


extern value getsockopt_int(int * sockopt, value socket, int level,
                            value option);
extern value setsockopt_int(int * sockopt, value socket, int level,
                            value option, value status);
extern value getsockopt_optint(int * sockopt, value socket, int level,
                               value option);
extern value setsockopt_optint(int * sockopt, value socket, int level,
                               value option, value status);
extern value getsockopt_float(int * sockopt, value socket, int level,
                              value option);
extern value setsockopt_float(int * sockopt, value socket, int level,
                              value option, value status);

CAMLprim value cash_getsockopt_bool (value socket, value level, value option)
{
  return getsockopt_int (sockopt_bool, socket, sockopt_levels[Int_val(level)], option);
}

CAMLprim value cash_setsockopt_bool (value socket, value level, value option, value status)
{
  return setsockopt_int (sockopt_bool, socket, sockopt_levels[Int_val(level)], option, status);
}

CAMLprim value cash_getsockopt_int (value socket, value level, value option)
{
  return getsockopt_int (sockopt_int, socket, sockopt_levels[Int_val(level)], option);
}

CAMLprim value cash_setsockopt_int (value socket, value level, value option, value status)
{
  return setsockopt_int (sockopt_int, socket, sockopt_levels[Int_val(level)], option, status);
}

CAMLprim value cash_getsockopt_optint (value socket, value level, value option)
{
  return getsockopt_optint (sockopt_optint, socket, sockopt_levels[Int_val(level)], option);
}

CAMLprim value cash_setsockopt_optint (value socket, value level, value option, value status)
{
  return setsockopt_optint (sockopt_optint, socket, sockopt_levels[Int_val(level)], option, status);
}

CAMLprim value cash_getsockopt_float (value socket, value level, value option)
{
  return getsockopt_float (sockopt_float, socket, sockopt_levels[Int_val(level)], option);
}

CAMLprim value cash_setsockopt_float (value socket, value level, value option, value status)
{
  return setsockopt_float (sockopt_float, socket, sockopt_levels[Int_val(level)], option, status);
}

#include <termios.h>

/* Stolen from otherlibs/unix/termios.c */
static struct speedtable_entry {
  speed_t speed;
  int baud;
} speedtable[] = {
  {B50,      50},
  {B75,      75},
  {B110,     110},
  {B134,     134},
  {B150,     150},
  {B300,     300},
  {B600,     600},
  {B1200,    1200},
  {B1800,    1800},
  {B2400,    2400},
  {B4800,    4800},
  {B9600,    9600},
  {B19200,   19200},
  {B38400,   38400},
#ifdef B57600
  {B57600,   57600},
#endif
#ifdef B115200
  {B115200,  115200},
#endif
#ifdef B230400
  {B230400,  230400},
#endif
  {B0,       0}
};

#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))

static value get_baud (speed_t speed)
{
  struct speedtable_entry * sp;

  for (sp = speedtable; sp < &speedtable[NSPEEDS]; sp++)
    {
      if (speed == sp->speed)
        return Val_int (sp->baud);
    }
  failwith ("tty_info: unrecognized baud rate");
}

static speed_t get_speed (value baud_)
{
  struct speedtable_entry * sp;
  int baud = Int_val (baud_);

  for (sp = speedtable; sp < &speedtable[NSPEEDS]; sp++)
    {
      if (baud == sp->baud)
        return sp->speed;
    }
  failwith ("set_tty_info: unrecognized baud rate");
}

CAMLprim value cash_tty_info (value fd)
{
  CAMLparam0 ();
  CAMLlocal5 (control_chars, input_flags, output_flags, control_flags, local_flags);
  struct termios tty_info;
  value res;

  if (tcgetattr(Int_val(fd), &tty_info) == -1)
    uerror("tty_info", Nothing);
  control_chars = alloc_string (NCCS);
  memmove (String_val (control_chars), tty_info.c_cc, NCCS);
  input_flags = copy_nativeint (tty_info.c_iflag);
  output_flags = copy_nativeint (tty_info.c_oflag);
  control_flags = copy_nativeint (tty_info.c_cflag);
  local_flags = copy_nativeint (tty_info.c_lflag);
  res = alloc_small (9, 0);
  Field (res, 0) = control_chars;
  Field (res, 1) = input_flags;
  Field (res, 2) = output_flags;
  Field (res, 3) = control_flags;
  Field (res, 4) = local_flags;
  Field (res, 5) = get_baud (cfgetispeed (&tty_info));
  Field (res, 6) = get_baud (cfgetospeed (&tty_info));
  Field (res, 7) = Val_int (tty_info.c_cc[VMIN]);
  Field (res, 8) = Val_int (tty_info.c_cc[VTIME]);
  return res;
}

/* Stolen from otherlibs/unix/termios.c */
static int when_flag_table[] = {
  TCSANOW, TCSADRAIN, TCSAFLUSH
};

CAMLprim value cash_set_ttyinfo (value how, value fd, value info)
{
  struct termios tty_info;

  /* Posix mandates initializing tty_info with tcgetattr. */
  if (tcgetattr(Int_val(fd), &tty_info) == 0) {
    memmove (tty_info.c_cc, String_val (Field (info, 0)), NCCS);
    tty_info.c_iflag = Nativeint_val (Field (info, 1));
    tty_info.c_oflag = Nativeint_val (Field (info, 2));
    tty_info.c_cflag = Nativeint_val (Field (info, 3));
    tty_info.c_lflag = Nativeint_val (Field (info, 4));
    if (cfsetispeed (&tty_info, get_speed (Field (info, 5))) == 0
        && cfsetospeed (&tty_info, get_speed (Field (info, 6))) == 0)
      {
        /* This first clause of this conditional test will hopefully resolve the
        ** branch at compile time.  However, since VMIN/VEOF and VTIME/VEOL are
        ** allowed by POSIX to collide, we have to check.  If they do collide,
        ** we set EOF & EOL in canonical mode, and MIN & TIME in raw mode.
        ** Ah, Unix.
        */
        if ((VMIN != VEOF && VTIME != VEOL) || !(tty_info.c_lflag & ICANON)) {
          tty_info.c_cc[VMIN] = Field (info, 7);
          tty_info.c_cc[VTIME] = Field (info, 8);;
        }
        if (tcsetattr (Int_val (fd), when_flag_table[Int_val (how)], &tty_info) == 0)
          return Val_unit;
      }
  }
  uerror("set_tty_info", Nothing);
}


#ifndef VDSUSP
#define VDSUSP (-1)
#endif
#ifndef VWERASE
#define VWERASE (-1)
#endif
#ifndef VDISCARD
#define VDISCARD (-1)
#endif
#ifndef VEOL2
#define VEOL2 (-1)
#endif
#ifndef VLNEXT
#define VLNEXT (-1)
#endif
#ifndef VREPRINT
#define VREPRINT (-1)
#endif
#ifndef VSTATUS
#define VSTATUS (-1)
#endif
#ifndef _POSIX_VDISABLE
#ifdef __CYGWIN__			/* Bletch! */
#define _POSIX_VDISABLE '\0'
#endif
#endif

static char ttychars[] =
{
  _POSIX_VDISABLE,                      /* 1 */
  VERASE,
  VKILL,
  VEOF,
  VEOL,                                 /* 5 */
  VINTR,
  VQUIT,
  VSUSP,
  VSTART,
  VSTOP,                                /* 10 */
  VDSUSP,
  VWERASE,
  VDISCARD,
  VEOL2,
  VLNEXT,                               /* 15 */
  VREPRINT,
  VSTATUS,                              /* 17 */
};

CAMLprim value cash_tty_chars ()
{
  value res;

  res = alloc_string (sizeof (ttychars));
  memmove (String_val (res), ttychars, sizeof (ttychars));
  return res;
}

static value make_native_array (tcflag_t * flags, int n)
{
  CAMLparam0 ();
  CAMLlocal1 (res);
  int i;

  res = alloc (n, 0);
  for (i = 0; i < n; i++)
    {
      Store_field (res, i, copy_nativeint (*flags++));
    }
  CAMLreturn (res);
}

/* Non-Posix c_iflag's */
#ifndef IXANY
#define IXANY (-1)
#endif
#ifndef IMAXBEL
#define IMAXBEL (-1)
#endif
#ifndef IUCLC
#define IUCLC (-1)
#endif

static tcflag_t input_flags[] =
{
  INPCK,                                /* 1 */
  IGNPAR,
  PARMRK,
  IGNBRK,
  BRKINT,                               /* 5 */
  ISTRIP,
  ICRNL,
  IGNCR,
  INLCR,
  IXOFF,                                /* 10 */
  IXON,
  IXANY,
  IMAXBEL,
  IUCLC,                                /* 14 */
};

#define NIFLAGS (sizeof (input_flags) / sizeof (input_flags[0]))

/* Non-Posix c_oflag's */
#ifndef ONLCR
#define ONLCR (-1)
#endif
#ifndef ONOEOT
#define ONOEOT (-1)
#endif
#ifndef OXTABS
#define OXTABS (-1)
#endif
#ifndef OCRNL
#define OCRNL (-1)
#endif
#ifndef ONLRET
#define ONLRET (-1)
#endif
#ifndef ONOCR
#define ONOCR (-1)
#endif
#ifndef OFILL
#define OFILL (-1)
#endif
#ifndef OFDEL
#define OFDEL (-1)
#endif
#ifndef OLCUC
#define OLCUC (-1)
#endif
#ifndef ONLCR
#define ONLCR (-1)
#endif
#ifndef ONOEOT
#define ONOEOT (-1)
#endif
#ifndef OXTABS
#define OXTABS (-1)
#endif
#ifndef OCRNL
#define OCRNL (-1)
#endif
#ifndef ONLRET
#define ONLRET (-1)
#endif
#ifndef ONOCR
#define ONOCR (-1)
#endif
#ifndef OFILL
#define OFILL (-1)
#endif
#ifndef OFDEL
#define OFDEL (-1)
#endif
#ifndef OLCUC
#define OLCUC (-1)
#endif
#ifndef BSDLY
#define BSDLY (-1)
#endif
#ifndef BS0
#define BS0 (-1)
#endif
#ifndef BS1
#define BS1 (-1)
#endif
#ifndef CRDLY
#define CRDLY (-1)
#endif
#ifndef CR0
#define CR0 (-1)
#endif
#ifndef CR1
#define CR1 (-1)
#endif
#ifndef CR2
#define CR2 (-1)
#endif
#ifndef CR3
#define CR3 (-1)
#endif
#ifndef FFDLY
#define FFDLY (-1)
#endif
#ifndef FF0
#define FF0 (-1)
#endif
#ifndef FF1
#define FF1 (-1)
#endif
#ifndef TABDLY
#define TABDLY (-1)
#endif
#ifndef TAB0
#define TAB0 (-1)
#endif
#ifndef TAB1
#define TAB1 (-1)
#endif
#ifndef TAB2
#define TAB2 (-1)
#endif
#ifndef TAB3
#define TAB3 (-1)
#endif
#ifndef NLDLY
#define NLDLY (-1)
#endif
#ifndef NL0
#define NL0 (-1)
#endif
#ifndef NL1
#define NL1 (-1)
#endif
#ifndef VTDLY
#define VTDLY (-1)
#endif
#ifndef VT0
#define VT0 (-1)
#endif
#ifndef VT1
#define VT1 (-1)
#endif

static tcflag_t output_flags[] =
{
  OPOST,                                /* 1 */
  ONLCR,
  ONOEOT,
  OXTABS,
  OCRNL,                                /* 5 */
  ONLRET,
  ONOCR,
  OFILL,
  OFDEL,
  OLCUC,                                /* 10 */
  BSDLY,
  BS0,
  BS1,
  CRDLY,
  CR0,                                  /* 15 */
  CR1,
  CR2,
  CR3,
  FFDLY,
  FF0,                                  /* 20 */
  FF1,
  TABDLY,
  TAB0,
  TAB1,
  TAB2,                                 /* 25 */
  TAB3,
  NLDLY,
  NL0,
  NL1,
  VTDLY,                                /* 30 */
  VT0,
  VT1,
  BSDLY | CRDLY | FFDLY | TABDLY | NLDLY | VTDLY, /* 33 */
};

#define NOFLAGS (sizeof (output_flags) / sizeof (output_flags[0]))

#ifndef CIGNORE
#define CIGNORE (-1)
#endif
#ifndef CCTS_OFLOW
#define CCTS_OFLOW (-1)
#endif
#ifndef CRTS_IFLOW
#define CRTS_IFLOW (-1)
#endif
#ifndef MDMBUF
#define MDMBUF (-1)
#endif

static tcflag_t control_flags[] =
{
  CSIZE,                                /* 1 */
  CS5,
  CS6,
  CS7,
  CS8,                                  /* 5 */
  PARENB,
  PARODD,
  CREAD,
  HUPCL,
  CLOCAL,                               /* 10 */
  CSTOPB,
  CIGNORE,
  CCTS_OFLOW,
  CRTS_IFLOW,
  MDMBUF,                               /* 15 */
};

#define NCFLAGS (sizeof (control_flags) / sizeof (control_flags[0]))

#ifndef ECHOCTL
#define ECHOCTL         (-1)
#endif
#ifndef FLUSHO
#define FLUSHO          (-1)
#endif
#ifndef ECHOPRT
#define ECHOPRT         (-1)
#endif
#ifndef PENDIN
#define PENDIN          (-1)
#endif
#ifndef ECHOKE
#define ECHOKE          (-1)
#endif
#ifndef ALTWERASE
#define ALTWERASE       (-1)
#endif
#ifndef NOKERNINFO
#define NOKERNINFO      (-1)
#endif
#ifndef XCASE
#define XCASE           (-1)
#endif

static tcflag_t local_flags[] =
{
  ICANON,                               /* 1 */
  ECHO,
  ECHOK,
  ECHONL,
  ECHOE,                                /* 5 */
  ISIG,
  IEXTEN,
  NOFLSH,
  TOSTOP,
  ECHOCTL,                              /* 10 */
  FLUSHO,
  ECHOPRT,
  PENDIN,
  ECHOKE,
  ALTWERASE,                            /* 15 */
  NOKERNINFO,
  XCASE,
};

#define NLFLAGS (sizeof (local_flags) / sizeof (local_flags[0]))

CAMLprim value cash_input_flags ()
{
  return (make_native_array (input_flags, NIFLAGS));
}

CAMLprim value cash_output_flags ()
{
  return (make_native_array (output_flags, NOFLAGS));
}

CAMLprim value cash_control_flags ()
{
  return (make_native_array (control_flags, NCFLAGS));
}

CAMLprim value cash_local_flags ()
{
  return (make_native_array (local_flags, NLFLAGS));
}

CAMLprim value cash_set_control_tty (value fd)
{
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(__hpux)
  /* 4.3+BSD way to acquire control tty. !CIBAUD rules out SunOS.
  ** This code stolen from Steven's *Advanced Prog. in the Unix Env.*
  */
  if (ioctl(Int_val (fd), TIOCSCTTY, (char *) 0) < 0) ) {
    int e = errno;
    close(fd);
    errno = e;
    uerror ("open_control_tty", Nothing)
  }
#endif
  return fd;
}


/* Local Variables: */
/* indent-tabs-mode: nil */
/* End: */
