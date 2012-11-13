/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* Replacement <selinux/selinux.h> for platforms that lack it.
   Copyright (C) 2008-2012 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef _GL_SELINUX_SELINUX_H
# define _GL_SELINUX_SELINUX_H

# if __GNUC__ >= 3
#pragma GCC system_header
# endif


# if HAVE_SELINUX_SELINUX_H

#include_next 

# else

#  include <sys/types.h>
#  include <errno.h>

/* The definition of _GL_UNUSED_PARAMETER is copied here.  */
/* _GL_UNUSED_PARAMETER is a marker that can be appended to function parameter
   declarations for parameters that are not used.  This helps to reduce
   warnings, such as from GCC -Wunused-parameter.  The syntax is as follows:
       type param _GL_UNUSED_PARAMETER
   or more generally
       param_decl _GL_UNUSED_PARAMETER
   For example:
       int param _GL_UNUSED_PARAMETER
       int *(*param)(void) _GL_UNUSED_PARAMETER
   Other possible, but obscure and discouraged syntaxes:
       int _GL_UNUSED_PARAMETER *(*param)(void)
       _GL_UNUSED_PARAMETER int *(*param)(void)
 */
#ifndef _GL_UNUSED_PARAMETER
# if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
#  define _GL_UNUSED_PARAMETER __attribute__ ((__unused__))
# else
#  define _GL_UNUSED_PARAMETER
# endif
#endif

#  if !GNULIB_defined_security_types

typedef unsigned short security_class_t;
#   define security_context_t char*
#   define is_selinux_enabled() 0

static inline int getcon (security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline void freecon (security_context_t con _GL_UNUSED_PARAMETER) {}


static inline int getfscreatecon (security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int setfscreatecon (security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int matchpathcon (char const *file _GL_UNUSED_PARAMETER,
                                mode_t m _GL_UNUSED_PARAMETER,
                                security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int getfilecon (char const *file _GL_UNUSED_PARAMETER,
                              security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int lgetfilecon (char const *file _GL_UNUSED_PARAMETER,
                               security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int fgetfilecon (int fd,
                               security_context_t *con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int setfilecon (char const *file _GL_UNUSED_PARAMETER,
                              security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int lsetfilecon (char const *file _GL_UNUSED_PARAMETER,
                               security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int fsetfilecon (int fd _GL_UNUSED_PARAMETER,
                               security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }

static inline int security_check_context
    (security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int security_check_context_raw
    (security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int setexeccon (security_context_t con _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int security_compute_create
    (security_context_t scon _GL_UNUSED_PARAMETER,
     security_context_t tcon _GL_UNUSED_PARAMETER,
     security_class_t tclass _GL_UNUSED_PARAMETER,
     security_context_t *newcon _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }
static inline int matchpathcon_init_prefix
    (char const *path _GL_UNUSED_PARAMETER,
     char const *prefix _GL_UNUSED_PARAMETER)
  { errno = ENOTSUP; return -1; }

#   define GNULIB_defined_security_types 1
#  endif

# endif
#endif /* _GL_SELINUX_SELINUX_H */
