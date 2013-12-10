// =============================================================================
// Copyright 2013 AONO Tomohiko
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// =============================================================================

#include <limits.h> // PATH_MAX
#include <string.h> // memcpy

#include "ei.h"
#include "erl_interface.h"
#include "erl_driver.h"

#include "eroonga.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ErlDrvBinary *bindup(void *src, ErlDrvSizeT size) {

  ErlDrvBinary *dest = (ErlDrvBinary *)driver_alloc_binary(size);

  memcpy(&dest->orig_bytes[0], src, size);

  return src;
}

static void encode_error0(ei_x_buff *x, const char *error) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_atom(x, error);
}

static void encode_error1(ei_x_buff *x, const char *error, const char *arg1) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, error);
  ei_x_encode_string(x, arg1);
}

static void output_bool(ErlDrvPort port, int value) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, "ok");
  ei_x_encode_boolean(&x, value);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

static void output_error0(ErlDrvPort port, const char *error) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  encode_error0(&x, error);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

static void output_error1(ErlDrvPort port, const char *error, const char *arg1) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  encode_error1(&x, error, arg1);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

static void output_ok(ErlDrvPort port) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ei_x_encode_atom(&x, "ok");

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

struct _driver_data {
  //
  ErlDrvPort port;
  //
  void *driver;
  //
  char path[PATH_MAX+1];
};

typedef struct _driver_data driver_data_t;

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {

  UNUSED(drv_data); UNUSED(buf); UNUSED(len); UNUSED(rlen);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;

  ei_x_new_with_version(&x);

  switch (command) {
  case 1:
    output_ok(data->port);
    break;

  default:
    {
      encode_error0(&x, "badarg");
    }
    break;
  }

  ErlDrvSizeT result = x.index;
  *rbuf = (char *)bindup(x.buff, result);

  ei_x_free(&x);

  return result;
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

  UNUSED(len);

  driver_data_t *data = (driver_data_t *)drv_data;

  int result = 0;
  int index = 0, version, arity;

  ei_decode_version(buf, &index, &version);
  ei_decode_tuple_header(buf, &index, &arity);

  if (2 == arity) {

    if(result) {
      output_bool(data->port, result);
    } else {
      output_error1(data->port, "badmatch", "cmd");
    }

    return;
  }

  output_error0(data->port, "badarg");
}

static ErlDrvData start(ErlDrvPort port, char *command) {

  UNUSED(command);

  if (NULL != port && GRN_SUCCESS == grn_init()) {

    void *ptr = driver_alloc(sizeof(driver_data_t));

    if (NULL != ptr) {

      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

      driver_data_t *data = (driver_data_t *)ptr;

      data->port = port;

      return (ErlDrvData)data;
    }
  }

  return ERL_DRV_ERROR_GENERAL;
}

static void stop(ErlDrvData drv_data) {

  void *ptr = drv_data;

  if (NULL != drv_data) {

    grn_fin();

    driver_free(ptr);
  }
}

static ErlDrvEntry driver_entry = {
  //.init = NULL,
  .start = start,
  .stop = stop,
  .output = output,
  //.ready_input = NULL,
  //.ready_output = NULL,
  .driver_name = (char *)("lib" APP "_drv"),
  //.finish = NULL,
  //.handle = NULL,
  .control = control,
  //.timeout = NULL,
  //.outputv = NULL,
  //.ready_async = NULL,
  //.flush = NULL,
  //.call = NULL,
  //.event = NULL,
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags= ERL_DRV_FLAG_USE_PORT_LOCKING,
  //.handle2 = NULL,
  //.process_exit = NULL,
  //.stop_select = NULL,
};

/*
 */
DRIVER_INIT(driver) {
  return &driver_entry;
}
