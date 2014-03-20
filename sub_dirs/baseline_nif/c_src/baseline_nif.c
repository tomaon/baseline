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

#include <ctype.h>
#include <string.h>

#include "baseline_nif.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ErlNifCharEncoding get_encoding() {
  return ERL_NIF_LATIN1;
}

void *baseline_alloc_resource(ErlNifEnv *env, unsigned size) {
  ErlNifResourceType *type = (ErlNifResourceType *)enif_priv_data(env);
  return enif_alloc_resource(type, size);
}

void baseline_release_resource(void **resource) {
  enif_release_resource(*resource);
  *resource = NULL;
}


int baseline_get_atom(ErlNifEnv *env, ERL_NIF_TERM term, char *buf, unsigned size) {
  return enif_get_atom(env, term, buf, size, get_encoding());
}

int baseline_get_atom_length(ErlNifEnv *env, ERL_NIF_TERM term, unsigned *len) {
  return enif_get_atom_length(env, term, len, get_encoding());
}

int baseline_get_resource(ErlNifEnv *env, ERL_NIF_TERM term, void **objp) {
  ErlNifResourceType *type = (ErlNifResourceType *)enif_priv_data(env);
  return enif_get_resource(env, term, type, objp);
}

int baseline_get_string(ErlNifEnv *env, ERL_NIF_TERM term, char *buf, unsigned size) {
  return enif_get_string(env, term, buf, size, get_encoding());
}


ERL_NIF_TERM baseline_make_atom(ErlNifEnv *env, const char *name) {
  ERL_NIF_TERM atom;
  return enif_make_existing_atom(env, name, &atom, get_encoding()) ? atom : enif_make_atom(env, name);
}

ERL_NIF_TERM baseline_make_resource(ErlNifEnv *env, void *obj) {
  return enif_make_resource(env, obj);
}

ERL_NIF_TERM baseline_make_string(ErlNifEnv *env, const char *string) {
  return enif_make_string(env, string, get_encoding());
}


ERL_NIF_TERM baseline_make_ok(ErlNifEnv *env, const ERL_NIF_TERM result) {
  return enif_make_tuple2(env, baseline_make_atom(env, "ok"), result);
}

ERL_NIF_TERM baseline_make_error(ErlNifEnv *env, const ERL_NIF_TERM reason) {
  return enif_make_tuple2(env, baseline_make_atom(env, "error"), reason);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

int baseline_set_bool(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  char buf[6];

  if (baseline_set_char(env, term, buf, sizeof(buf))) {

    for (char *p = buf; *p; ++p) *p = tolower(*p);

    *(char *)value = 0 == strcmp("true", buf) ? 1 : 0;

    return 1;
  }

  return 0;
}

int baseline_set_char(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  unsigned len = 0;

  if (enif_is_atom(env, term) && baseline_get_atom_length(env, term, &len) && len < size) {
    return baseline_get_atom(env, term, (char *)value, size);
  } else if (enif_is_list(env, term) && enif_get_list_length(env, term, &len) && len < size) {
    return baseline_get_string(env, term, (char *)value, size);
  }

  return 0;
}

int baseline_set_double(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_double(env, term, (double *)value) : 0;
}

int baseline_set_int(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_int(env, term, (int *)value) : 0;
}

int baseline_set_long(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_long(env, term, (long *)value) : 0;
}

int baseline_set_uint(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_uint(env, term, (unsigned int *)value) : 0;
}

int baseline_set_ulong(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_ulong(env, term, (unsigned long *)value) : 0;
}

#if SIZEOF_LONG != 8
int baseline_set_int64(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_int64(env, term, (ErlNifSInt64 *)value) : 0;
}

int baseline_set_uint64(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  UNUSED(size);

  return enif_is_number(env, term) ? enif_get_uint64(env, term, (ErlNifUInt64 *)value) : 0;
}
#endif


int baseline_set_proplists(ErlNifEnv *env, ERL_NIF_TERM term,
                           baseline_opt_t table[], unsigned size) {

  for (ERL_NIF_TERM head, tail; enif_get_list_cell(env, term, &head, &tail); term = tail) {

    int arity = 0;
    const ERL_NIF_TERM *array = NULL;

    if (enif_is_tuple(env, head) &&
        enif_get_tuple(env, head, &arity, &array) && 2 == arity) {

      unsigned len = 0;

      if (enif_is_atom(env, array[0]) && baseline_get_atom_length(env, array[0], &len)) {

        char name[len+1];

        baseline_get_atom(env, array[0], name, sizeof(name));

        for (size_t i = 0; i < size; i++) {
          if (0 == strcmp(table[i].name, name)) {
            if (table[i].func(env, array[1], (void *)table[i].value, table[i].size)) {
              break;
            }
          }
        }

      } // atom

    } // tuple

  } // for

  return 1;
}
