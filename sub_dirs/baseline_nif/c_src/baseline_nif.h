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

extern void *baseline_alloc_resource(ErlNifEnv *env, unsigned size);
extern void baseline_release_resource(void **resource);

extern int baseline_get_atom(ErlNifEnv *env, ERL_NIF_TERM term, char *buf, unsigned size);
extern int baseline_get_atom_length(ErlNifEnv *env, ERL_NIF_TERM term, unsigned *len);
extern int baseline_get_resource(ErlNifEnv *env, ERL_NIF_TERM term, void **objp);
extern int baseline_get_string(ErlNifEnv *env, ERL_NIF_TERM term, char *buf, unsigned size);

extern ERL_NIF_TERM baseline_make_atom(ErlNifEnv *env, const char *name);
extern ERL_NIF_TERM baseline_make_resource(ErlNifEnv *env, void *obj);
extern ERL_NIF_TERM baseline_make_string(ErlNifEnv *env, const char *string);

extern ERL_NIF_TERM baseline_make_ok(ErlNifEnv *env, const ERL_NIF_TERM result);
extern ERL_NIF_TERM baseline_make_error(ErlNifEnv *env, const ERL_NIF_TERM reason);

extern int baseline_set_bool(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size);
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
