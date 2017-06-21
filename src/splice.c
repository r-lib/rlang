#include "rlang.h"
#include "export.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
  bool recursive;
} squash_info_t;

squash_info_t squash_info_init(bool recursive) {
  squash_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  info.recursive = recursive;
  return info;
}


// Atomic squashing ---------------------------------------------------

static
R_len_t atom_squash(SEXPTYPE kind, squash_info_t info,
                    SEXP outer, SEXP out, R_len_t count,
                    bool (*is_spliceable)(SEXP), int depth) {
  if (TYPEOF(outer) != VECSXP)
    Rf_errorcall(R_NilValue, "Only lists can be spliced");

  SEXP inner;
  SEXP out_names = names(out);
  R_len_t n_outer = r_length(outer);
  R_len_t n_inner;

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = vec_length(inner);

    if (depth != 0 && is_spliceable(inner)) {
      count = atom_squash(kind, info, inner, out, count, is_spliceable, depth - 1);
    } else if (n_inner) {
      vec_copy_coerce_n(inner, n_inner, out, count, 0);

      if (info.named) {
        SEXP nms = names(inner);
        if (is_character(nms))
          vec_copy_n(nms, n_inner, out_names, count, 0);
        else if (n_inner == 1 && has_name_at(outer, i))
          SET_STRING_ELT(out_names, count, STRING_ELT(names(outer), i));
      }

      count += n_inner;
    }
  }

  return count;
}


// List squashing -----------------------------------------------------

R_len_t list_squash(squash_info_t info, SEXP outer,
                    SEXP out, R_len_t count,
                    bool (*is_spliceable)(SEXP), int depth) {
  if (TYPEOF(outer) != VECSXP)
    Rf_errorcall(R_NilValue, "Only lists can be spliced");

  SEXP inner;
  SEXP out_names = names(out);
  R_len_t n_outer = r_length(outer);

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);

    if (depth != 0 && is_spliceable(inner)) {
      count = list_squash(info, inner, out, count, is_spliceable, depth - 1);
    } else {
      SET_VECTOR_ELT(out, count, inner);

      if (info.named && is_character(names(outer))) {
        SEXP name = STRING_ELT(names(outer), i);
        SET_STRING_ELT(out_names, count, name);
      }

      count += 1;
    }
  }

  return count;
}


// First pass --------------------------------------------------------

static
void squash_warn_names(void) {
  Rf_warningcall(R_NilValue, "Outer names are only allowed for unnamed scalar atomic inputs");
}

static
void update_info_outer(squash_info_t* info, SEXP outer, R_len_t i) {
  if (!info->warned && info->recursive && has_name_at(outer, i)) {
    squash_warn_names();
    info->warned = true;
  }
}
static
void update_info_inner(squash_info_t* info, SEXP outer, R_len_t i, SEXP inner) {
  R_len_t n_inner = info->recursive ? 1 : vec_length(inner);
  info->size += n_inner;

  // Return early if possible
  if (info->named && info->warned)
    return;

  bool named = is_character(names(inner));
  bool recursive = info->recursive;

  bool copy_outer = recursive || n_inner == 1;
  bool copy_inner = !recursive;

  if (named && copy_inner)
    info->named = true;

  if (has_name_at(outer, i)) {
    if (!recursive && (n_inner != 1 || named) && !info->warned) {
      squash_warn_names();
      info->warned = true;
    }
    if (copy_outer)
      info->named = true;
  }
}

static
void squash_info(squash_info_t* info, SEXP outer,
                 bool (*is_spliceable)(SEXP), int depth) {
  SEXP inner;
  R_len_t n_inner;
  R_len_t n_outer = r_length(outer);

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = info->recursive ? 1 : vec_length(inner);

    if (depth != 0 && is_spliceable(inner)) {
      update_info_outer(info, outer, i);
      squash_info(info, inner, is_spliceable, depth - 1);
    } else if (n_inner) {
      update_info_inner(info, outer, i, inner);
    }
  }
}

static
SEXP squash(SEXPTYPE kind, SEXP dots, bool (*is_spliceable)(SEXP), int depth) {
  bool recursive = kind == VECSXP;

  squash_info_t info = squash_info_init(recursive);
  squash_info(&info, dots, is_spliceable, depth);

  SEXP out = KEEP(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  if (recursive)
    list_squash(info, dots, out, 0, is_spliceable, depth);
  else
    atom_squash(kind, info, dots, out, 0, is_spliceable, depth);

  FREE(1);
  return out;
}


// Predicates --------------------------------------------------------

typedef bool (*is_spliceable_t)(SEXP);

bool is_spliced_bare(SEXP x) {
  return is_list(x) && (!OBJECT(x) || Rf_inherits(x, "spliced"));
}
bool is_spliced(SEXP x) {
  return is_list(x) && Rf_inherits(x, "spliced");
}

static
is_spliceable_t predicate_pointer(SEXP x) {
  switch (TYPEOF(x)) {
  case VECSXP:
    if (Rf_inherits(x, "fn_pointer") && r_length(x) == 1) {
      SEXP ptr = VECTOR_ELT(x, 0);
      if (TYPEOF(ptr) == EXTPTRSXP)
        return (is_spliceable_t) R_ExternalPtrAddrFn(ptr);
    }
    break;

  case EXTPTRSXP:
    return (is_spliceable_t) R_ExternalPtrAddrFn(x);

  default:
    break;
  }

  Rf_errorcall(R_NilValue, "`predicate` must be a closure or function pointer");
  return NULL;
}

is_spliceable_t predicate_internal(SEXP x) {
  static SEXP is_spliced_clo = NULL;
  if (!is_spliced_clo)
    is_spliced_clo = rlang_obj("is_spliced");

  static SEXP is_spliceable_clo = NULL;
  if (!is_spliceable_clo)
    is_spliceable_clo = rlang_obj("is_spliced_bare");

  if (x == is_spliced_clo)
    return &is_spliced;
  if (x == is_spliceable_clo)
    return &is_spliced_bare;
  return NULL;
}

// For unit tests:
bool is_clevel_spliceable(SEXP x) {
  return Rf_inherits(x, "foo");
}

// Emulate closure behaviour with global variable.
SEXP clo_spliceable = NULL;
bool is_spliceable_closure(SEXP x) {
  if (!clo_spliceable)
    Rf_error("Internal error while splicing");
  SETCADR(clo_spliceable, x);

  SEXP out = Rf_eval(clo_spliceable, R_GlobalEnv);
  return r_as_bool(out);
}


// Export ------------------------------------------------------------

SEXP rlang_squash_if(SEXP dots, SEXPTYPE kind, bool (*is_spliceable)(SEXP), int depth) {
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
    Rf_errorcall(R_NilValue, "Splicing is not implemented for this type");
    return R_NilValue;
  }
}
SEXP rlang_squash_closure(SEXP dots, SEXPTYPE kind, SEXP pred, int depth) {
  SEXP prev_pred = clo_spliceable;
  clo_spliceable = KEEP(Rf_lang2(pred, Rf_list2(R_NilValue, R_NilValue)));

  SEXP out = rlang_squash_if(dots, kind, &is_spliceable_closure, depth);

  clo_spliceable = prev_pred;
  FREE(1);

  return out;
}
SEXP rlang_squash(SEXP dots, SEXP type, SEXP pred, SEXP depth_) {
  SEXPTYPE kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));
  int depth = Rf_asInteger(depth_);

  is_spliceable_t is_spliceable;

  if (TYPEOF(pred) == CLOSXP) {
    is_spliceable = predicate_internal(pred);
    if (is_spliceable)
      return rlang_squash_if(dots, kind, is_spliceable, depth);
    else
      return rlang_squash_closure(dots, kind, pred, depth);
  }

  is_spliceable = predicate_pointer(pred);
  return rlang_squash_if(dots, kind, is_spliceable, depth);
}
