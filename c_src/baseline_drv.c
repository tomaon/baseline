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

#include <errno.h>
#include <string.h>
#include <unistd.h> // sleep

#include "ei.h"
#include "erl_interface.h"
#include "erl_driver.h"

#include "baseline_drv.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

struct _driver_data {
  //
  ErlDrvPort port;
  //
  char encoding[32]; // TODO
};

typedef struct _driver_data driver_data_t;

typedef void (*driver_func_t)(driver_data_t *data,
                              char *buf, int *index, ei_x_buff *x);

struct _driver_option_t {
  const char *name;
  const void *value;
};

typedef struct _driver_option_t driver_option_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void encode_error(ei_x_buff *x, const char *error) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_atom(x, error);
}

static void encode_error1(ei_x_buff *x, const char *error, const char *arg1) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, error);
  ei_x_encode_atom(x, arg1);
}

static void encode_ok(ei_x_buff *x) {
  ei_x_encode_atom(x, "ok");
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void control_set_options(driver_data_t *data,
                                char *buf, int *index, ei_x_buff *x) {

  const driver_option_t TABLE[] = {
    { "encoding", data->encoding },
  };

  const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  int arity, found = 0;
  ei_term term;
  char atom[MAXATOMLEN_UTF8] = "unknown";

  ei_decode_list_header(buf, index, &arity);

  for (int size = arity, i = 0; !found && i < size; i++) {

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      found = -1;

      ei_decode_atom(buf, index, atom);
      ei_decode_ei_term(buf, index, &term);

      for (size_t j = 0; found && j < TABLE_SIZE; j++) {

        if (0 == strcmp(TABLE[j].name, atom)) {

          switch (term.ei_type) { // erl_interface-*/include/ei.h
          case ERL_STRING_EXT: {
            char p[term.size];
            ei_decode_string(buf, index, p);
            strcpy((char *)TABLE[j].value, p);
            found = 0;
          } break;
          case ERL_BINARY_EXT: {
            char p[term.size+1];
            long len;
            ei_decode_binary(buf, index, p, &len); p[len] = '\0';
            strcpy((char *)TABLE[j].value, p);
            found = 0;
          } break;
          case ERL_INTEGER_EXT:    case ERL_SMALL_INTEGER_EXT: {
            *((long *)TABLE[j].value) = term.value.i_val;
            found = 0;
          } break;
          case ERL_FLOAT_EXT:      case NEW_FLOAT_EXT: {
            *((double *)TABLE[j].value) = term.value.d_val;
            found = 0;
          } break;
          case ERL_ATOM_EXT:       case ERL_ATOM_UTF8_EXT:
          case ERL_SMALL_ATOM_EXT: case ERL_SMALL_ATOM_UTF8_EXT: {
            strcpy((char *)TABLE[j].value, term.value.atom_name);
            found = 0;
          } break;
          case ERL_REFERENCE_EXT:  case ERL_NEW_REFERENCE_EXT: {
            memcpy((erlang_ref *)TABLE[j].value, &term.value.ref, sizeof(erlang_ref));
            found = 0;
          } break;
          case ERL_PORT_EXT: {
            memcpy((erlang_port *)TABLE[j].value, &term.value.port, sizeof(erlang_port));
            found = 0;
          } break;
          case ERL_PID_EXT: {
            memcpy((erlang_pid *)TABLE[j].value, &term.value.pid, sizeof(erlang_pid));
            found = 0;
          } break;
          default:
            // LIST, (SMALL|LARGE)_TUPLE, NIL, ...
            break;
          }
        }
      } // for, j < TABLE_SIZE
    }
  } // for, i < size

  !found ? encode_ok(x) : encode_error1(x, "badarg", atom);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void async_invoke(void *p) {

  unsigned long seconds = (unsigned long)p;

  if (0 < seconds) {
    sleep(seconds);
  }

  // !TERMINATE ? ready_async : async_free
}

static void async_free(void *p) {
  UNUSED(p);
}

static void output_echo(driver_data_t *data,
                        char *buf, int *index, ei_x_buff *x) {
  int arity;

  ei_decode_list_header(buf, index, &arity);

  if (3 == arity) {

    unsigned long size;
    ei_decode_ulong(buf, index, &size);

    if (0 < size) {

      char text[size];
      ei_decode_string(buf, index, text);

      driver_enq(data->port, text, size); // -> flush/1, MUST: ErlDrvPDL

    } else {
      ei_skip_term(buf, index);
    }

    unsigned long wait;
    ei_decode_ulong(buf, index, &wait);

#ifdef USE_DRIVER_ASYNC_PORT_KEY
    unsigned int key = driver_async_port_key(data->port);
#else
    unsigned int key = *(unsigned int *)data->port;
#endif

    driver_async(data->port, &key, async_invoke, (void *)wait, async_free);

    ei_x_encode_atom(x, "ack");

  } else {
    encode_error(x, "badarg");
  }
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static int init(void) {

  if (0) {
    ErlDrvSysInfo sys_info;
    driver_system_info(&sys_info, sizeof(sys_info));
    printf("driver_major_version: %d\r\n", sys_info.driver_major_version);
    printf("driver_minor_version: %d\r\n", sys_info.driver_minor_version);
    printf("erts_version        : %s\r\n", sys_info.erts_version);
    printf("otp_release         : %s\r\n", sys_info.otp_release);
    printf("thread_support      : %d\r\n", sys_info.thread_support);
    printf("smp_support         : %d\r\n", sys_info.smp_support);
    printf("async_threads       : %d\r\n", sys_info.async_threads);
    printf("scheduler_threads   : %d\r\n", sys_info.scheduler_threads);
    printf("nif_major_version   : %d\r\n", sys_info.nif_major_version);
    printf("nif_minor_version   : %d\r\n", sys_info.nif_minor_version);
  }

  return 0;
}

static ErlDrvData start(ErlDrvPort port, char *command) {

  UNUSED(command); // = driver_name

  void *ptr = driver_alloc(sizeof(driver_data_t));

  if (NULL != ptr) {

    driver_data_t *data = (driver_data_t *)ptr;

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    data->port = port;
    strcpy(data->encoding, "utf8");

    return (ErlDrvData)data;
  }

  errno = ENOMEM;
  return ERL_DRV_ERROR_ERRNO;
}

static void stop(ErlDrvData drv_data) {

  driver_data_t *data = (driver_data_t *)drv_data;

  if (NULL != data) {
    driver_free(data);
  }
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

  UNUSED(len);

  static const driver_func_t TABLE[] = {
    output_echo,
  };

  static const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version, arity;
  ei_decode_version(buf, &index, &version); // version = ERL_VERSION_MAGIC (131)
  ei_decode_tuple_header(buf, &index, &arity);

  if (2 == arity) {

    ei_x_buff x2;
    ei_x_new(&x2);

    unsigned long command;
    ei_decode_ulong(buf, &index, &command);

    driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

    if (NULL != func) {
      func(data, buf, &index, &x2);
    } else {
      encode_error(&x2, "enosys");
    }

    ei_x_append(&x, &x2);

    ei_x_free(&x2);

  } else {
    encode_error(&x, "badarg");
  }

  driver_output(data->port, x.buff, x.index);

  ei_x_free(&x);
}

static void finish(void) {
}

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len,
                            char **rbuf, ErlDrvSizeT rlen) {
  UNUSED(len);

  static const driver_func_t TABLE[] = {
    control_set_options,
  };

  static const size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version;
  ei_decode_version(buf, &index, &version); // version = ERL_VERSION_MAGIC (131)

  driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

  if (NULL != func) {
    func(data, buf, &index, &x);
  } else {
    encode_error(&x, "enosys");
  }

  ErlDrvSizeT result = x.index;

  if (rlen >= result) {

    memcpy(*rbuf, x.buff, result);

  } else {

    ErlDrvBinary *binary = driver_alloc_binary(result);

    if (NULL != binary) {

      memcpy(&binary->orig_bytes[0], x.buff, result);

      *rbuf = (char *)binary;

    } else {

      ei_x_new_with_version(&x); // reset
      encode_error(&x, "nomem");

      memcpy(*rbuf, x.buff, result = x.index);
    }
  }

  ei_x_free(&x);

  return result;
}

static void ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data) {

  UNUSED(thread_data);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  ErlIOVec ev;
  ErlDrvSizeT size = driver_peekqv(data->port, &ev);

  if (0 < size) {

    char buf[size];
    ErlDrvSizeT len = driver_vec_to_buf(&ev, buf, size);

    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "ok");
    ei_x_encode_string_len(&x, buf, len);

    driver_deq(data->port, size);

  } else {
    ei_x_encode_atom(&x, "ok");
  }

  driver_output(data->port, x.buff, x.index);

  ei_x_free(&x);
}

static void flush(ErlDrvData drv_data) {

  driver_data_t *data = (driver_data_t *)drv_data;

  ErlDrvSizeT size = driver_sizeq(data->port);

  if (0 < size) {
    driver_deq(data->port, size);
  }
}

static ErlDrvSSizeT call(ErlDrvData drv_data, unsigned int command,
                         char *buf, ErlDrvSizeT len,
                         char **rbuf, ErlDrvSizeT rlen, unsigned int *flags) {
  UNUSED(flags); // ?

  return control(drv_data, command, buf, len, rbuf, rlen); // > rlen -> badarg
}

static ErlDrvEntry driver_entry = {
  .init = init,
  .start = start,
  .stop = stop,
  .output = output,
  .ready_input = NULL,
  .ready_output = NULL,
  .driver_name = "baseline_drv",
  .finish = finish,
  .handle = NULL,                               // reserved
  .control = control,
  .timeout = NULL,
  .outputv = NULL,                              // NULL -> output
  .ready_async = ready_async,                   // NULL -> async_free
  .flush = flush,
  .call = call,
  .event = NULL,                                // "Intentionally left undocumented"
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags= ERL_DRV_FLAG_USE_PORT_LOCKING,
  .handle2 = NULL,                              // reserved
  .process_exit = NULL,
  .stop_select = NULL,
};

/*
 */
DRIVER_INIT(driver) {
  return &driver_entry;
}
