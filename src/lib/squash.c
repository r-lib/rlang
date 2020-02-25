#include "rlang.h"
#include "export.h"

// Initialised at load time
static bool (*rlang_is_splice_box)(sexp*) = NULL;
static sexp* (*rlang_unbox)(sexp*) = NULL;

// The vector to splice might be boxed in a sentinel wrapper
static sexp* maybe_unbox(sexp* x, bool (*is_spliceable)(sexp*)) {
  if (is_spliceable(x) && rlang_is_splice_box(x)) {
    return r_vec_coerce(rlang_unbox(x), r_type_list);
  } else {
    return x;
  }
}


typedef struct {
  r_ssize size;
  bool named;
  bool warned;
  bool recursive;
} squash_info_t;

static squash_info_t squash_info_init(bool recursive) {
  squash_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  info.recursive = recursive;
  return info;
}


// Atomic squashing ---------------------------------------------------

static r_ssize atom_squash(enum r_type kind, squash_info_t info,
                            sexp* outer, sexp* out, r_ssize count,
                            bool (*is_spliceable)(sexp*), int depth) {
  if (r_typeof(outer) != VECSXP) {
    r_abort("Only lists can be spliced");
  }

  sexp* inner;
  sexp* out_names = KEEP(r_names(out));
  r_ssize n_outer = r_length(outer);
  r_ssize n_inner;

  for (r_ssize i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = r_vec_length(maybe_unbox(inner, is_spliceable));

    if (depth != 0 && is_spliceable(inner)) {
      inner = PROTECT(maybe_unbox(inner, is_spliceable));
      count = atom_squash(kind, info, inner, out, count, is_spliceable, depth - 1);
      UNPROTECT(1);
    } else if (n_inner) {
      r_vec_poke_coerce_n(out, count, inner, 0, n_inner);

      if (info.named) {
        sexp* nms = r_names(inner);
        if (r_typeof(nms) == r_type_character) {
          r_vec_poke_n(out_names, count, nms, 0, n_inner);
        } else if (n_inner == 1 && r_has_name_at(outer, i)) {
          SET_STRING_ELT(out_names, count, STRING_ELT(r_names(outer), i));
        }
      }

      count += n_inner;
    }
  }

  FREE(1);
  return count;
}


// List squashing -----------------------------------------------------

static r_ssize list_squash(squash_info_t info, sexp* outer,
                            sexp* out, r_ssize count,
                            bool (*is_spliceable)(sexp*), int depth) {
  if (r_typeof(outer) != VECSXP) {
    r_abort("Only lists can be spliced");
  }

  sexp* inner;
  sexp* out_names = KEEP(r_names(out));
  r_ssize n_outer = r_length(outer);

  for (r_ssize i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);

    if (depth != 0 && is_spliceable(inner)) {
      inner = PROTECT(maybe_unbox(inner, is_spliceable));
      count = list_squash(info, inner, out, count, is_spliceable, depth - 1);
      UNPROTECT(1);
    } else {
      SET_VECTOR_ELT(out, count, inner);

      if (info.named && r_typeof(r_names(outer)) == r_type_character) {
        sexp* name = STRING_ELT(r_names(outer), i);
        SET_STRING_ELT(out_names, count, name);
      }

      count += 1;
    }
  }

  FREE(1);
  return count;
}


// First pass --------------------------------------------------------

static void squash_warn_names(void) {
  Rf_warningcall(r_null, "Outer names are only allowed for unnamed scalar atomic inputs");
}

static void update_info_outer(squash_info_t* info, sexp* outer, r_ssize i) {
  if (!info->warned && info->recursive && r_has_name_at(outer, i)) {
    squash_warn_names();
    info->warned = true;
  }
}
static void update_info_inner(squash_info_t* info, sexp* outer, r_ssize i, sexp* inner) {
  r_ssize n_inner = info->recursive ? 1 : r_vec_length(inner);
  info->size += n_inner;

  // Return early if possible
  if (info->named && info->warned) {
    return;
  }

  bool named = r_typeof(r_names(inner)) == r_type_character;
  bool recursive = info->recursive;

  bool copy_outer = recursive || n_inner == 1;
  bool copy_inner = !recursive;

  if (named && copy_inner) {
    info->named = true;
  }

  if (r_has_name_at(outer, i)) {
    if (!recursive && (n_inner != 1 || named) && !info->warned) {
      squash_warn_names();
      info->warned = true;
    }
    if (copy_outer) {
      info->named = true;
    }
  }
}

static void squash_info(squash_info_t* info, sexp* outer,
                        bool (*is_spliceable)(sexp*), int depth) {
  if (r_typeof(outer) != r_type_list) {
    r_abort("Only lists can be spliced");
  }

  sexp* inner;
  r_ssize n_outer = r_length(outer);

  for (r_ssize i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);

    if (depth != 0 && is_spliceable(inner)) {
      update_info_outer(info, outer, i);
      inner = PROTECT(maybe_unbox(inner, is_spliceable));
      squash_info(info, inner, is_spliceable, depth - 1);
      UNPROTECT(1);
    } else if (info->recursive || r_vec_length(inner)) {
      update_info_inner(info, outer, i, inner);
    }
  }
}

static sexp* squash(enum r_type kind, sexp* dots, bool (*is_spliceable)(sexp*), int depth) {
  bool recursive = kind == VECSXP;

  squash_info_t info = squash_info_init(recursive);
  squash_info(&info, dots, is_spliceable, depth);

  sexp* out = KEEP(r_new_vector(kind, info.size));
  if (info.named) {
    sexp* nms = KEEP(r_new_vector(r_type_character, info.size));
    r_poke_names(out, nms);
    FREE(1);
  }

  if (recursive) {
    list_squash(info, dots, out, 0, is_spliceable, depth);
  } else {
    atom_squash(kind, info, dots, out, 0, is_spliceable, depth);
  }

  FREE(1);
  return out;
}


// Predicates --------------------------------------------------------

typedef bool (*is_spliceable_t)(sexp*);

static bool is_spliced_bare(sexp* x) {
  if (!r_is_object(x)) {
    return r_typeof(x) == r_type_list;
  } else {
    return rlang_is_splice_box(x);
  }
}

static is_spliceable_t predicate_pointer(sexp* x) {
  switch (r_typeof(x)) {
  case VECSXP:
    if (Rf_inherits(x, "fn_pointer") && r_length(x) == 1) {
      sexp* ptr = VECTOR_ELT(x, 0);
      if (r_typeof(ptr) == EXTPTRSXP) {
        return (is_spliceable_t) R_ExternalPtrAddrFn(ptr);
      }
    }
    break;

  case EXTPTRSXP:
    return (is_spliceable_t) R_ExternalPtrAddrFn(x);

  default:
    break;
  }

  r_abort("`predicate` must be a closure or function pointer");
  return NULL;
}

static is_spliceable_t predicate_internal(sexp* x) {
  static sexp* is_spliced_clo = NULL;
  if (!is_spliced_clo) {
    is_spliced_clo = rlang_ns_get("is_spliced");
  }

  static sexp* is_spliceable_clo = NULL;
  if (!is_spliceable_clo) {
    is_spliceable_clo = rlang_ns_get("is_spliced_bare");
  }

  if (x == is_spliced_clo) {
    return rlang_is_splice_box;
  }
  if (x == is_spliceable_clo) {
    return &is_spliced_bare;
  }
  return NULL;
}

// Emulate closure behaviour with global variable.
static sexp* clo_spliceable = NULL;

static bool is_spliceable_closure(sexp* x) {
  if (!clo_spliceable) {
    r_abort("Internal error while splicing");
  }
  SETCADR(clo_spliceable, x);

  sexp* out = r_eval(clo_spliceable, R_GlobalEnv);
  return r_lgl_get(out, 0);
}


// Export ------------------------------------------------------------

sexp* r_squash_if(sexp* dots, enum r_type kind, bool (*is_spliceable)(sexp*), int depth) {
  switch (kind) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return squash(kind, dots, is_spliceable, depth);
  default:
    r_abort("Splicing is not implemented for this type");
    return r_null;
  }
}
sexp* rlang_squash_closure(sexp* dots, enum r_type kind, sexp* pred, int depth) {
  sexp* prev_pred = clo_spliceable;
  clo_spliceable = KEEP(Rf_lang2(pred, Rf_list2(r_null, r_null)));

  sexp* out = r_squash_if(dots, kind, &is_spliceable_closure, depth);

  clo_spliceable = prev_pred;
  FREE(1);

  return out;
}
sexp* rlang_squash(sexp* dots, sexp* type, sexp* pred, sexp* depth_) {
  enum r_type kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));
  int depth = Rf_asInteger(depth_);

  is_spliceable_t is_spliceable;

  switch (r_typeof(pred)) {
  case r_type_closure:
    is_spliceable = predicate_internal(pred);
    if (is_spliceable) {
      return r_squash_if(dots, kind, is_spliceable, depth);
    } // else fallthrough
  case r_type_builtin:
  case r_type_special:
    return rlang_squash_closure(dots, kind, pred, depth);
  default:
    is_spliceable = predicate_pointer(pred);
    return r_squash_if(dots, kind, is_spliceable, depth);
  }
}


void r_init_library_squash() {
  rlang_is_splice_box = (bool (*)(sexp*)) r_peek_c_callable("rlang", "rlang_is_splice_box");
  rlang_unbox = (sexp* (*)(sexp*)) r_peek_c_callable("rlang", "rlang_unbox");
}
