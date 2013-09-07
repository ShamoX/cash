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
#include <errno.h>
#include <sys/resource.h>
#include "caml/mlvalues.h"
#include "otherlibs/unix/unixsupport.h"

CAMLprim value unix_getpgrp (value unit)
{
  return Val_int (getpgrp ());
}

CAMLprim value unix_setpgid (value pid, value pgid)
{
  if (setpgid (Int_val (pid), Int_val (pgid)) == -1)
    uerror ("setpgid", Nothing);
  return Val_unit;
}

#include <byterun/fail.h>

#ifdef PRIO_PROCESS

static int priorities [] = { PRIO_PROCESS, PRIO_PGRP, PRIO_USER };

CAMLprim value unix_priority (value which, value who)
{
  int r;

  errno = 0;
  r = getpriority (priorities [Int_val (which)] , Int_val (who));
  if (r == -1 && errno != 0) uerror ("priority", Nothing);
  return Val_int (r);
}

CAMLprim value unix_setpriority (value which, value who, value priority)
{
  if (setpriority (Int_val (which) , Int_val (who), Int_val (priority)) == -1)
    uerror ("setpriority", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_priority (value which, value who)
{
  invalid_argument ("priority not implemented");
}

CAMLprim value unix_setpriority (value which, value who, value priority)
{
  invalid_argument ("set_priority not implemented");
}

#endif

#include "byterun/alloc.h"
#include "caml/memory.h"

#define Pair_tag 0

/* 2d arg. list is the initial value on entry. */
static value convert_flags_to_list (int flags, value list, int * table, int table_size)
{
  int i = table_size;
  CAMLparam1 (list);
  CAMLlocal1 (tail);

  while (--i >= 0 && flags != 0)
    {
      if (flags & table[i])
        {
          flags &= ~table[i];
          tail = list;
          list = alloc_small (2, Pair_tag);
          Field (list, 0) = Val_int (i);
          Field (list, 1) = tail;
        }
    }
  CAMLreturn (list);
}

#include <fcntl.h>

static int fdes_flags[] = { FD_CLOEXEC };
CAMLprim value read_fdes_flags (value fd)
{
  int r;

  r = fcntl (Int_val (fd), F_GETFD, 0);
  if (r == -1) uerror ("fdes_flags", Nothing);
  return convert_flags_to_list (r, Val_emptylist, fdes_flags,
                                sizeof (fdes_flags) / sizeof (fdes_flags[0]));
}

CAMLprim value write_fdes_flags (value fd, value flags)
{
  if ((fcntl (Int_val (fd), F_SETFD, convert_flag_list (flags, fdes_flags))) == -1)
    uerror ("set_fdes_flags", Nothing);
  return Val_unit;
}

#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif

/* From open.c (so these funcs should probably go there) */
static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC, O_EXCL, 
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC
};

#include <assert.h>

CAMLprim value read_file_flags (value fd)
{
  int r, openmode;
  CAMLparam0 ();
  CAMLlocal1 (res);

  r = fcntl (Int_val (fd), F_GETFL, 0);
  if (r == -1) uerror ("fdes_status", Nothing);
  res = alloc_small (2, Pair_tag);
  openmode = r & O_ACCMODE;
  Field (res, 0) =
    openmode == O_RDONLY ? Val_int (0) :
    openmode == O_WRONLY ? Val_int (1) :
    openmode == O_RDWR ? Val_int (2) :
    (assert (0), Val_int (0));
  Field (res, 1) = Val_emptylist;
  res = convert_flags_to_list (r & ~O_ACCMODE, res, open_flag_table,
                               sizeof (open_flag_table) / sizeof (open_flag_table[0]));
  CAMLreturn (res);
}

CAMLprim value write_file_flags (value fd, value flags)
{
  if ((fcntl (Int_val (fd), F_SETFL, convert_flag_list (flags, open_flag_table))) == -1)
    uerror ("set_file_flags", Nothing);
  return Val_unit;
}

CAMLprim value sync_file (value fd)
{
  if (fsync (Int_val (fd)) == -1)
    uerror ("sync_file", Nothing);
  return Val_unit;
}

#include <netdb.h>
#include <string.h>
#include "caml/memory.h"
#include <byterun/alloc.h>
#include <byterun/signals.h>
#include "otherlibs/unix/socketaddr.h"

static value alloc_net_entry(struct netent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit, netnum = Val_unit;

  Begin_roots3 (name, aliases, netnum);
    name = copy_string((char *)(entry->n_name));
    aliases = copy_string_array((const char**)entry->n_aliases);
    netnum = Int32_val(entry->n_net);
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = entry->n_addrtype == PF_UNIX ? Val_int(0) : Val_int(1);
    Field(res, 3) = netnum;
  End_roots();
  return res;
}

#include "sys.h"

#ifdef HAS_GETNET

CAMLprim value unix_getnetbyname (value name)
{
  char netname[256];
  struct netent * entry;

  strncpy(netname, String_val(name), sizeof(netname) - 1);
  netname[sizeof(netname) - 1] = 0;
  enter_blocking_section();
  entry = getnetbyname(netname);
  leave_blocking_section();
  if (entry == (struct netent *) NULL) raise_not_found();
  return alloc_net_entry(entry);
}

CAMLprim value unix_getnetbyaddr(value a)
{
  uint32 adr;
  struct netent * entry;

  adr = GET_INET_ADDR(a);
  enter_blocking_section();
  entry = getnetbyaddr(adr, AF_INET);
  leave_blocking_section();
  if (entry == (struct netent *) NULL) raise_not_found();
  return alloc_net_entry(entry);
}

#else

CAMLprim value unix_getnetbyname (value which, value who)
{
  invalid_argument ("getnetbyname not implemented");
}

CAMLprim value unix_getnetbyaddr (value which, value who)
{
  invalid_argument ("getnetbyaddr not implemented");
}

#endif


CAMLprim value unix_isatty (value fd)
{
  return isatty (Int_val (fd)) ? Val_true : Val_false;
}

CAMLprim value unix_tty_file_name (value fd)
{
  char * tty = ttyname (Int_val (fd));

  if (tty == NULL) uerror ("tty_file_name", Nothing);
  return copy_string (tty);
}

CAMLprim value unix_tcgetpgrp (value fd)
{
  pid_t res;
  
  res = tcgetpgrp (Int_val (fd));
  if (res == -1)
    uerror ("tty_process_group", Nothing);
  return Val_int (res);
}

CAMLprim value unix_tcsetpgrp (value fd, value pgrp)
{
  if ((tcsetpgrp (Int_val (fd), Int_val (pgrp))) == -1)
    uerror ("set_tty_process_group", Nothing);
  return Val_unit;
}

#include <stdio.h>

CAMLprim value unix_control_tty_file_name ()
{
  char * tty = ctermid (NULL);

  if (tty == NULL) uerror ("tty_file_name", Nothing);
  return copy_string (tty);
}


/* Local Variables: */
/* indent-tabs-mode: nil */
/* End: */
