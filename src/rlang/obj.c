#include "rlang.h"

#define PRECIOUS_DICT_INIT_SIZE 256

static
struct r_dict* p_precious_dict = NULL;

#include "decl/obj-decl.h"


void r_preserve(r_obj* x) {
  KEEP(x);

  r_obj* stack = r_dict_get0(p_precious_dict, x);
  if (!stack) {
    stack = KEEP(new_precious_stack(x));
    r_dict_put(p_precious_dict, x, stack);
    FREE(1);
  }

  push_precious(stack);
  FREE(1);
}

void r_unpreserve(r_obj* x) {
  r_obj* stack = r_dict_get0(p_precious_dict, x);
  if (!stack) {
    r_abort("Can't unpreserve `x` because it was not being preserved.");
  }

  int n = pop_precious(stack);

  if (n < 0) {
    r_stop_internal("r_unpreserve", "`n` unexpectedly < 0.");
  }
  if (n == 0) {
    r_dict_del(p_precious_dict, x);
  }
}


static
r_obj* new_precious_stack(r_obj* x) {
  r_obj* stack = KEEP(r_alloc_list(2));

  // Store (0) protection count and (1) element to protect
  r_list_poke(stack, 0, r_int(0));
  r_list_poke(stack, 1, x);

  FREE(1);
  return stack;
}

static
int push_precious(r_obj* stack) {
  r_obj* n = r_list_get(stack, 0);
  int* p_n = r_int_begin(n);
  return ++(*p_n);
}

static
int pop_precious(r_obj* stack) {
  r_obj* n = r_list_get(stack, 0);
  int* p_n = r_int_begin(n);
  return --(*p_n);
}

// For unit tests
struct r_dict* rlang__precious_dict() {
  return p_precious_dict;
}


enum r_type r_chr_as_r_type(r_obj* type) {
  if (!r_is_string(type)) {
    r_abort("`type` must be a character string.");
  }
  return r_c_str_as_r_type(r_chr_get_c_string(type, 0));
}

const char* obj_address_formatter = "%p";

r_obj* r_obj_address(r_obj* x) {
  static char buf[1000];
  snprintf(buf, 1000, obj_address_formatter, (void*) x);
  return Rf_mkChar(buf);
}

void r_init_library_obj(r_obj* ns) {
  p_precious_dict = r_new_dict(PRECIOUS_DICT_INIT_SIZE);
  KEEP(p_precious_dict->shelter);
  r_env_poke(ns,
             r_sym(".__rlang_lib_precious_dict__."),
             p_precious_dict->shelter);
  FREE(1);

  const char* null_addr = r_str_c_string(r_obj_address(r_null));
  if (null_addr[0] != '0' || null_addr[1] != 'x') {
    obj_address_formatter = "0x%p";
  }
}
