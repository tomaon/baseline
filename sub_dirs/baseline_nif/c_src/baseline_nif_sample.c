// =============================================================================
// =============================================================================

#include <stdio.h>
#include <errno.h>

#include "baseline_nif.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

typedef struct _nif_resource_t nif_resource_t;

struct _nif_resource_t {
  int id;
  int i;
  unsigned int ui;
  long l;
  unsigned long ul;
  char b;
  char a[10];
  char s[10];
};

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static nif_resource_t *alloc_resource(ErlNifEnv *env) {
  return (nif_resource_t *)baseline_alloc_resource(env, sizeof(nif_resource_t));
}

static void init_resource(nif_resource_t *resource, int id) {
  resource->id = id;
  resource->i = 0;
  resource->ui = 0;
  resource->l = 0;
  resource->ul = 0;
  resource->b = 0;
  resource->a[0] = '\0';
  resource->s[0] = '\0';
}

static int get_resource(ErlNifEnv *env, ERL_NIF_TERM term, nif_resource_t **objp) {
  return baseline_get_resource(env, term, (void **)objp);
}

static void release_resource(nif_resource_t **resource) {
  baseline_release_resource((void **)resource);
}


static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *name) {
  return baseline_make_atom(env, name);
}

static ERL_NIF_TERM make_resource(ErlNifEnv *env, void *obj) {
  return baseline_make_resource(env, obj);
}

static ERL_NIF_TERM make_string(ErlNifEnv *env, const char *string) {
  return baseline_make_string(env, string);
}


static ERL_NIF_TERM make_ok(ErlNifEnv *env) {
  return make_atom(env, "ok");
}

static ERL_NIF_TERM make_ok2(ErlNifEnv *env, const ERL_NIF_TERM result) {
  return baseline_make_ok(env, result);
}

static ERL_NIF_TERM make_error(ErlNifEnv *env, const char *reason) {
  return baseline_make_error(env, make_string(env, reason));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (1 == argc &&
      enif_is_number(env, argv[0])) {

    int id = 0;
    if (enif_get_int(env, argv[0], &id) && 0 < id) {

      nif_resource_t *resource = alloc_resource(env);

      if (NULL != resource) {

        init_resource(resource, id);

        return make_ok2(env, make_resource(env, resource));

      } else {
        return make_error(env, "enomem");
      }
    }
  }

  return make_error(env, "badarg");
}

static ERL_NIF_TERM delete_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (1 == argc &&
      enif_is_binary(env, argv[0])) {

    nif_resource_t *resource = NULL;

    if (get_resource(env, argv[0], &resource)) {

      release_resource(&resource);

      return make_ok(env);
    }
  }

  return make_error(env, "badarg");
}


static ERL_NIF_TERM get_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (1 == argc &&
      enif_is_binary(env, argv[0])) {

    nif_resource_t *resource = NULL;

    if (get_resource(env, argv[0], &resource)) {

      ERL_NIF_TERM arr[] = {
        enif_make_int(env, resource->i),
        enif_make_uint(env, resource->ui),
        enif_make_long(env, resource->l),
        enif_make_ulong(env, resource->ul),
        enif_make_int(env, resource->b),
        make_atom(env, resource->a),
        make_string(env, resource->s),
      };

      unsigned cnt = sizeof(arr) / sizeof(arr[0]);

      return enif_make_tuple2(env,
                              make_ok(env),
                              enif_make_tuple_from_array(env, arr, cnt));
    }
  }

  return make_error(env, "badarg");
}

static ERL_NIF_TERM set_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {

  if (2 == argc &&
      enif_is_binary(env, argv[0]) && enif_is_list(env, argv[1])) {

    nif_resource_t *resource = NULL;

    if (get_resource(env, argv[0], &resource)) {

      baseline_opt_t TABLE[] = {
        { "i", &resource->i, sizeof(resource->i), baseline_set_int },
        { "ui", &resource->ui, sizeof(resource->ui), baseline_set_uint },
        { "l", &resource->l, sizeof(resource->l), baseline_set_long },
        { "ul", &resource->ul, sizeof(resource->ul), baseline_set_ulong },
        { "b", &resource->b, sizeof(resource->b), baseline_set_bool },
        { "a", &resource->a, sizeof(resource->a), baseline_set_char },
        { "s", &resource->s, sizeof(resource->s), baseline_set_char },
      };

      size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

      if (baseline_set_proplists(env, argv[1], TABLE, TABLE_SIZE)) {
        return make_ok(env);
      }
    }
  }

  return make_error(env, "badarg");
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void dtor(ErlNifEnv *env, void *obj) {

  UNUSED(env), UNUSED(obj);

  //nif_resource_t *resource = (nif_resource_t *)obj;
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ErlNifFunc funcs[] = {
  {"new_nif", 1, new_nif},
  {"delete_nif", 1, delete_nif},
  {"get_nif", 1, get_nif},
  {"set_nif", 2, set_nif},
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {

  if (!enif_is_empty_list(env, load_info)) {
    return EINVAL;
  }

  const char* module_str = "baseline_nif";
  const char* name = "nif_resource_t";
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE;
  ErlNifResourceFlags* tried = NULL;

  ErlNifResourceType *type =
    enif_open_resource_type(env, module_str, name, dtor, flags, tried);

  if (NULL == type) {
    return ENOMEM;
  }

  *priv_data = type;

  return 0;
}

static int upgrade(ErlNifEnv *env,
                   void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {

  UNUSED(env), UNUSED(load_info);

  *priv_data = *old_priv_data;

  return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {
  UNUSED(env), UNUSED(priv_data);
}

/*
 */
ERL_NIF_INIT(baseline_nif_sample, funcs, load, NULL, upgrade, unload);
