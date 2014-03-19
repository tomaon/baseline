// =============================================================================
// Copyright 2014 AONO Tomohiko
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License version 2.1 as published by the Free Software Foundation.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
// =============================================================================

#ifndef BASELINE_NIF_H
#define BASELINE_NIF_H


#include "erl_nif.h"


#define UNUSED(p) (void)(p)


extern int baseline_set_char(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_double(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_int(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_long(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_uint(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_ulong(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);

#if SIZEOF_LONG != 8
extern int baseline_set_int64(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
extern int baseline_set_uint64(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
#endif


typedef struct _baseline_opt_t baseline_opt_t;

struct _baseline_opt_t {
  const char *name;
  const void *value;
  const unsigned int size;
  int (*func)(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
};

extern int baseline_set_proplists(ErlNifEnv *env, ERL_NIF_TERM term,
                                  baseline_opt_t table[], unsigned size);

#endif // BASELINE_NIF_H
