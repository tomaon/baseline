#include <string.h> // memcpy
#include <unistd.h> // sleep

#include "erl_driver.h"
#include "erl_interface.h"

#include "baseline_drv_sample.h"

struct _driver_data {
  char *buf;
  int len;
};

typedef struct _driver_data driver_data_t;


static void encode_error(ei_x_buff *x, const char *error) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom_len(x, "error", 5);
  ei_x_encode_atom(x, error);
}

static void encode_ulong(ei_x_buff *x, unsigned long value) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom_len(x, "ok", 2);
  ei_x_encode_ulong(x, value);
}


static void foo(ei_x_buff *x, const char *buf, int *index) {
  unsigned long value;
  ei_decode_ulong(buf, index, &value);
  encode_ulong(x, value + 1);
}

static void bar(ei_x_buff *x, const char *buf, int *index) {
  unsigned long value;
  ei_decode_ulong(buf, index, &value);
  encode_ulong(x, value * 2);
}


static void async_invoke(void *ptr) {

  driver_data_t *data = (driver_data_t *) ptr;
  char *buf = data->buf;
  int len = data->len, index = 0;

  int version, arity;
  erlang_pid pid;
  erlang_ref ref;
  long operation;
  ei_decode_version(buf, &index, &version);   // [{Pid, Ref}, Operation, Data, []]
  ei_decode_list_header(buf, &index, &arity);
  ei_decode_tuple_header(buf, &index, &arity);
  ei_decode_pid(buf, &index, &pid);
  ei_decode_ref(buf, &index, &ref);
  ei_decode_long(buf, &index, &operation);

  ei_x_buff x;
  ei_x_new_with_version(&x);                  // [{Pid, Ref}, Reply, []]
  ei_x_encode_list_header(&x, 2);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_pid(&x, &pid);
  ei_x_encode_ref(&x, &ref);
  {
    switch (operation) {
    case 0: foo(&x, buf, &index); break;
    case 1: bar(&x, buf, &index); break;
    default: encode_error(&x, "enosys"); break;
    }
    sleep(1);
  }
  ei_x_encode_empty_list(&x);

  if (x.index > len) {
    data->buf = driver_realloc(data->buf, x.index);
  }
  memcpy(data->buf, x.buff, data->len = x.index);

  ei_x_free(&x);
}


static int init(void) {

  if (0) {
    ErlDrvSysInfo sys_info;
    driver_system_info(&sys_info, sizeof(sys_info));
    printf("driver_major_version   : %d\n", sys_info.driver_major_version);
    printf("driver_minor_version   : %d\n", sys_info.driver_minor_version);
    printf("erts_version           : %s\n", sys_info.erts_version);
    printf("otp_release            : %s\n", sys_info.otp_release);
    printf("thread_support         : %d\n", sys_info.thread_support);
    printf("smp_support            : %d\n", sys_info.smp_support);
    printf("async_threads          : %d\n", sys_info.async_threads);
    printf("scheduler_threads      : %d\n", sys_info.scheduler_threads);
    printf("nif_major_version      : %d\n", sys_info.nif_major_version);
    printf("nif_minor_version      : %d\n", sys_info.nif_minor_version);
    printf("dirty_scheduler_support: %d\n", sys_info.dirty_scheduler_support);
  }

  return 0;
}

static ErlDrvData start(ErlDrvPort port, char *command) {

  UNUSED(command); // = driver_name

  return (ErlDrvData) port;
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

  ErlDrvPort port = (ErlDrvPort) drv_data;

  void *ptr = driver_alloc(sizeof(char) * len * 2); // TODO
  memcpy(ptr, buf, len);

  driver_data_t *data = driver_alloc(sizeof(driver_data_t));
  data->buf = ptr;
  data->len = len;

  driver_async(port, NULL, async_invoke, data, NULL);
}

static void ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data) {

  ErlDrvPort port = (ErlDrvPort) drv_data;
  driver_data_t *data = (driver_data_t *) thread_data;

  driver_output(port, data->buf, data->len);

  driver_free(data->buf);
  driver_free(data);
}


static ErlDrvEntry drv_entry = {
  .init = init,                                    // erl_ddll:load_driver/2
  .start = start,                                  // erlang:open_port/2
  // .stop = NULL,                                 // erlang:port_close/1
  .output = output,                                // erlang:port_command/2,3
  // .ready_input = NULL,                          // signal
  // .ready_output = NULL,                         // signal
  .driver_name = "baseline_drv_sample",
  // .finish = NULL,                               // erl_ddll:unload_driver/1
  // .control = NULL,                              // erlang:port_control/3
  // .timeout = NULL,                              // driver_set_timer/2
  // .outputv = NULL,                              // => output
  .ready_async = ready_async,                      // driver_async/5
  // .flush = NULL,                                // before 'stop'
  // .call = NULL,                                 // erlang:port_call/3
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  // .driver_flags= 0,                             // TODO
  // .process_exit = NULL,                         // driver_monitor_process/3
  // .stop_select = NULL,                          // driver_select/4
  //. emergency_close = NULL,                      // ?
};

DRIVER_INIT(baseline_drv_sample) {
  return &drv_entry;
}
