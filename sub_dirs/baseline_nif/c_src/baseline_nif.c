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

#include <string.h>

#include "baseline_nif.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

int baseline_set_char(ErlNifEnv *env, ERL_NIF_TERM term, void *value, unsigned size) {

  unsigned int len = 0;
  const ErlNifCharEncoding encode = ERL_NIF_LATIN1;

  if (enif_is_atom(env, term) && enif_get_atom_length(env, term, &len, encode) && len < size) {
    return enif_get_atom(env, term, (char *)value, size, encode);
  } else if (enif_is_list(env, term) && enif_get_list_length(env, term, &len) && len < size) {
    return enif_get_string(env, term, (char *)value, size, encode);
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

      if (enif_is_atom(env, array[0]) &&
          enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1)) {

        char name[len+1];

        enif_get_atom(env, array[0], name, sizeof(name), ERL_NIF_LATIN1);

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

  return 0;
}
