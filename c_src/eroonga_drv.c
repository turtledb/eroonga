// =============================================================================
// Copyright 2013 AONO Tomohiko
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

#include <errno.h>  // ENOMEM
#include <limits.h> // PATH_MAX
#include <string.h> // memcpy

#include "ei.h"
#include "erl_interface.h"
#include "erl_driver.h"

#include "eroonga.h"

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

struct _driver_data {
  ErlDrvPort port;
  grn_ctx *ctx;
};

typedef struct _driver_data driver_data_t;

typedef void (*driver_func_t)(grn_ctx *ctx,
                              int argc, const char *argv[], ei_x_buff *x);

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
  ei_x_encode_string(x, arg1);
}

static void encode_ok(ei_x_buff *x) {
  ei_x_encode_atom(x, "ok");
}

static void encode_errbuf(ei_x_buff *x, grn_ctx *ctx) {
  char buf[8];
  sprintf(buf, "GRN%04d", ctx->rc);
  encode_error1(x, buf, ctx->errbuf);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static void eroonga_decode_row(grn_ctx *ctx,
                               grn_id id, grn_obj *column, grn_obj *value, ei_x_buff *x) {

  int n = grn_column_name(ctx, column, NULL, 0);
  char name[n];

  grn_column_name(ctx, column, name, n);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_binary(x,  name, n);

  GRN_BULK_REWIND(value);

  switch (grn_obj_get_range(ctx, column)) {
    //GRN_DB_BOOL: GRN_BOOL_VALUE
    //GRN_DB_INT8: GRN_INT8_VALUE
    //GRN_DB_UINT8: GRN_UINT8_VALUE
    //GRN_DB_INT16: GRN_INT16_VALUE
    //GRN_DB_UINT16: GRN_UINT16_VALUE
    //GRN_DB_INT32: GRN_INT32_VALUE
  case GRN_DB_UINT32:
    GRN_UINT32_INIT(value, 0);
    grn_obj_get_value(ctx, column, id, value);
    ei_x_encode_ulong(x, GRN_UINT32_VALUE(value));
    break;
    //GRN_DB_INT64: GRN_INT64_VALUE
    //GRN_DB_UINT64: GRN_UINT64_VALUE
    //GRN_DB_FLOAT: GRN_FLOAT_VALUE
    //GRN_DB_TIME: GRN_TIME_VALUE
  case GRN_DB_SHORT_TEXT:
  case GRN_DB_TEXT:
  case GRN_DB_LONG_TEXT:
    GRN_TEXT_INIT(value, 0);
    grn_obj_get_value(ctx, column, id, value);
    ei_x_encode_binary(x, GRN_TEXT_VALUE(value), GRN_TEXT_LEN(value));
    break;
    //GRN_DB_TOKYO_GEO_POINT: GRN_GEO_POINT_VALUE
    //GRN_DB_WGS84_GEO_POINT: GRN_GEO_POINT_VALUE
  default:
    ei_x_encode_binary(x,  "?", 1);
    break;
  }
}

static void eroonga_decode_rows(grn_ctx *ctx, grn_obj *table,
                                int n, grn_obj *columns[], grn_obj *expr, ei_x_buff *x) {

  grn_obj *tmp = grn_table_select(ctx, table, expr, NULL, GRN_OP_OR);

  if (NULL != tmp) {

    grn_table_cursor *tc =
      grn_table_cursor_open(ctx, tmp, NULL, 0, NULL, 0, 0, -1, GRN_CURSOR_ASCENDING);

    if (NULL != tc) {

      void *key;
      grn_id id;
      grn_obj value;
      int i;

      ei_x_encode_tuple_header(x, 2);
      ei_x_encode_atom(x, "ok");

      while (GRN_ID_NIL != grn_table_cursor_next(ctx, tc)) {

        if (sizeof(grn_id) == grn_table_cursor_get_key(ctx, tc, &key)) {

          id = *(grn_id *)key;

          ei_x_encode_list_header(x, 1);

          ei_x_encode_tuple_header(x, n);
          for (i = 0; i < n; i++) {
            eroonga_decode_row(ctx, id, columns[i], &value, x);
          }
        }
      }

      ei_x_encode_empty_list(x);

      grn_obj_unlink(ctx, &value); // TODO

      grn_table_cursor_close(ctx, tc);

    } else {
      encode_errbuf(x, ctx);
    }

    grn_obj_unlink(ctx, tmp);

  } else {
    encode_errbuf(x, ctx);
  }
}

static void eroonga_get_columns(grn_ctx *ctx,
                                grn_hash *hash, grn_obj *objs[], int size) {

  grn_hash_cursor *hc = grn_hash_cursor_open(ctx, hash, NULL, 0, NULL, 0, 0, -1, 0);

  if (NULL != hc) {

    void *key;
    int i;

    for (i = 0; i < size && GRN_ID_NIL != grn_hash_cursor_next(ctx, hc); i++) {

      if (sizeof(grn_id) == grn_hash_cursor_get_key(ctx, hc, &key)) {
        objs[i] = grn_ctx_at(ctx, *(grn_id *)key);
      }
    }

    grn_hash_cursor_close(ctx, hc);
  }
}

// ---

static void eroonga_db_open(grn_ctx *ctx,
                            int argc, const char *argv[], ei_x_buff *x) {
  if (1 == argc) {

    const char *path = argv[0];

    grn_obj *database = NULL;
    (void)GRN_DB_OPEN_OR_CREATE(ctx, path, NULL, database);

    if (NULL != database) {
      encode_ok(x);
    } else {
      encode_errbuf(x, ctx);
    }

  } else {
    encode_error(x, "badarg");
  }
}

static void eroonga_table_select(grn_ctx *ctx,
                                 int argc, const char *argv[], ei_x_buff *x) {
  if (2 == argc) {

    const char *name = argv[0];
    const char *str = argv[1];

    grn_obj *table = grn_ctx_get(ctx, name, strlen(name));

    if (NULL != table) {

      grn_hash *hash =
        grn_hash_create(ctx, NULL, sizeof(grn_id), 0, GRN_OBJ_TABLE_HASH_KEY);

      if (NULL != hash) {

        const int n = grn_table_columns(ctx, table, NULL, 0, (grn_obj *)hash);

        if (0 < n) {

          grn_obj *columns[n];
          eroonga_get_columns(ctx, hash, columns, n);

          grn_obj *expr, *var;
          grn_expr_flags flags =
            GRN_EXPR_SYNTAX_SCRIPT | GRN_EXPR_ALLOW_PRAGMA |GRN_EXPR_ALLOW_COLUMN;

          GRN_EXPR_CREATE_FOR_QUERY(ctx, table, expr, var);

          if (GRN_SUCCESS == grn_expr_parse(ctx, expr, str, strlen(str),
                                            NULL, GRN_OP_MATCH, GRN_OP_AND, flags)) {

            eroonga_decode_rows(ctx, table, n, columns, expr, x);

            grn_obj_unlink(ctx, var);
            grn_obj_unlink(ctx, expr);

          } else {
            encode_errbuf(x, ctx);
          }

          for (int i = 0; i < n; i++) {
            grn_obj_unlink(ctx, columns[i]);
          }
        }

        grn_hash_close(ctx, hash);

      } else {
        encode_errbuf(x, ctx);
      }

      grn_obj_unlink(ctx, table);

    } else {
      encode_error1(x, "notfound", name);
    }

  } else {
    encode_error(x, "badarg");
  }
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
static void exec(driver_func_t func, ErlDrvSizeT size,
                 driver_data_t *data, char *buf, int *index, ei_x_buff *x) {
  int arity;

  ei_decode_list_header(buf, index, &arity);

  if (0 < arity) {

    const char *argv[arity];
    char tmp[size], *p = tmp, *end = tmp + sizeof(tmp);
    ei_term term;
    long len;
    int i;

    for (i = 0; i < arity; i++) {

      ei_decode_ei_term(buf, index, &term);

      if (p + term.size >= end) break; // TODO

      switch (term.ei_type) {
      case ERL_BINARY_EXT:
        len = term.size;
        ei_decode_binary(buf, index, (void *)(argv[i] = p), &len);
        *(p += len) = '\0'; p += 1;
        break;
      default:
        encode_error(x, "badarg");
        goto next;
      }
    }

  next:

    if (i >= arity) {
      func(data->ctx, arity, argv, x);
    } else {
      encode_error(x, "badarg");
    }

  } else {
    encode_error(x, "badarg");
  }
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  UNUSED(rlen);

  static driver_func_t TABLE[] = {
    /* ERN_CONTROL_DB_OPEN */     eroonga_db_open,
  };

  static size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version;
  ei_decode_version(buf, &index, &version);

  driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

  if (NULL != func) {
    exec(func, len, data, buf, &index, &x);
  } else {
    encode_error(&x, "not_implemented");
  }

  ErlDrvBinary *binary = driver_alloc_binary(x.index); // NULL?, TODO

  ErlDrvSizeT result = x.index;
  memcpy(&binary->orig_bytes[0], x.buff, result);

  *rbuf = (char *)binary;

  ei_x_free(&x);

  return result;
}

static int init() {
  return GRN_SUCCESS == grn_init() ? 0 : -1;
}

static void finish() {
  grn_fin();
}

static ErlDrvData start(ErlDrvPort port, char *command) {

  UNUSED(command);

  void *ptr = driver_alloc(sizeof(driver_data_t));

  if (NULL != ptr) {

    int flags = 0; // GRN_CTX_USE_QL ?
    grn_ctx *ctx = grn_ctx_open(flags);

    if (NULL != ctx) {

      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

      driver_data_t *data = (driver_data_t *)ptr;
      data->port = port;
      data->ctx = ctx;

      return (ErlDrvData)data;
    }
  }

  errno = ENOMEM;
  return ERL_DRV_ERROR_ERRNO;
}

static void stop(ErlDrvData drv_data) {

  driver_data_t *data = (driver_data_t *)drv_data;

  grn_ctx *ctx = data->ctx;

  if (NULL != ctx) {
    grn_ctx_close(ctx);
  }

  driver_free(data);
}

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

  static driver_func_t TABLE[] = {
    /* ERN_OUTPUT_TABLE_SELECT */ eroonga_table_select,
  };

  static size_t TABLE_SIZE = sizeof(TABLE) / sizeof(TABLE[0]);

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;
  ei_x_new_with_version(&x);

  int index = 0, version, arity;
  ei_decode_version(buf, &index, &version);
  ei_decode_tuple_header(buf, &index, &arity);

  if (3 == arity) {

    erlang_ref ref;
    ei_decode_ref(buf, &index, &ref);

    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_ref(&x, &ref);

    unsigned long command;
    ei_decode_ulong(buf, &index, &command);

    driver_func_t func = TABLE_SIZE > command ? TABLE[command] : NULL;

    if (NULL != func) {
      exec(func, len, data, buf, &index, &x);
    } else {
      encode_error(&x, "not_implemented");
    }

  } else {
    // no-ref => exit?, TODO
  }

  driver_output(data->port, x.buff, x.index);

  ei_x_free(&x);
}

static ErlDrvEntry driver_entry = {
  .init = init,
  .start = start,
  .stop = stop,
  .output = output,
  .ready_input = NULL,
  .ready_output = NULL,
  .driver_name = (char *)("lib" APP "_drv"),
  .finish = finish,
  .handle = NULL,
  .control = control,
  .timeout = NULL,
  .outputv = NULL,
  .ready_async = NULL,
  .flush = NULL,
  .call = NULL,
  .event = NULL,
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags= ERL_DRV_FLAG_USE_PORT_LOCKING,
  .handle2 = NULL,
  .process_exit = NULL,
  .stop_select = NULL,
};

/*
 */
DRIVER_INIT(driver) {
  return &driver_entry;
}
