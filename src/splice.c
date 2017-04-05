#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
  bool recursive;
} splice_info_t;

splice_info_t splice_info_init(bool recursive) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  info.recursive = recursive;
  return info;
}

static
void splice_warn_names(void) {
  Rf_warningcall(R_NilValue, "Outer names are only allowed for unnamed scalar atomic inputs");
}

// Atomic splicing ---------------------------------------------------

// In particular, this returns 1 for environments
R_len_t vec_length(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return Rf_length(x);
  case NILSXP:
    return 0;
  default:
    return 1;
  }
}

static
void update_info_outer(splice_info_t* info, SEXP outer, R_len_t i) {
  if (!info->warned && info->recursive && has_name_at(outer, i)) {
    splice_warn_names();
    info->warned = true;
  }
}
static
void update_info_inner(splice_info_t* info, SEXP outer, R_len_t i, SEXP inner) {
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
      splice_warn_names();
      info->warned = true;
    }
    if (copy_outer)
      info->named = true;
  }
}

static
void splice_info(splice_info_t* info, SEXP outer,
                 bool (*is_spliceable)(SEXP), int depth) {
  SEXP inner;
  R_len_t n_inner;
  R_len_t n_outer = Rf_length(outer);

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = info->recursive ? 1 : vec_length(inner);

    if (depth != 0 && is_spliceable(inner)) {
      update_info_outer(info, outer, i);
      splice_info(info, inner, is_spliceable, depth - 1);
    } else if (n_inner) {
      update_info_inner(info, outer, i, inner);
    }
  }
}

static
void splice_copy_names_n(bool recursive, bool spliced,
                         SEXP src, R_len_t n,
                         SEXP outer_src, R_len_t i,
                         SEXP dest_nms, R_len_t count) {
  SEXP nms = names(src);
  bool copy_inner = recursive ? spliced : true;
  if (copy_inner && is_character(nms)) {
    vec_copy_n(nms, n, dest_nms, count, 0);
    return;
  }

  bool copy_outer = recursive ? (!spliced) : (n == 1);
  if (copy_outer && has_name_at(outer_src, i))
    SET_STRING_ELT(dest_nms, count, STRING_ELT(names(outer_src), i));
}

static
R_len_t atom_splice_list(SEXPTYPE kind, splice_info_t info,
                         SEXP outer, SEXP out, R_len_t count,
                         bool spliced, bool (*is_spliceable)(SEXP),
                         int depth) {
  if (TYPEOF(outer) != VECSXP)
    Rf_errorcall(R_NilValue, "Only lists can be spliced");

  SEXP inner;
  SEXP out_names = names(out);
  R_len_t n_outer = Rf_length(outer);
  R_len_t n_inner;

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);

    if (depth != 0 && is_spliceable(inner)) {
      count = atom_splice_list(kind, info, inner, out, count, true, is_spliceable, depth - 1);
      continue;
    }

    n_inner = vec_length(inner);
    if (n_inner) {
      vec_copy_coerce_n(inner, n_inner, out, count, 0);
      if (info.named)
        splice_copy_names_n(info.recursive, spliced, inner, n_inner, outer, i, out_names, count);

      count += n_inner;
      continue;
    }
  }

  return count;
}

static
SEXP atom_splice(SEXPTYPE kind, SEXP dots,
                 bool (*is_spliceable)(SEXP), int depth) {
  splice_info_t info = splice_info_init(false);
  splice_info(&info, dots, is_spliceable, depth);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  atom_splice_list(kind, info, dots, out, 0, false, is_spliceable, depth);

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

R_len_t list_splice_impl(splice_info_t info, SEXP outer,
                         SEXP out, R_len_t count, bool spliced,
                         bool (*is_spliceable)(SEXP), int depth) {
  if (TYPEOF(outer) != VECSXP)
    Rf_errorcall(R_NilValue, "Only lists can be spliced");

  SEXP inner;
  SEXP out_names = names(out);
  R_len_t n_outer = Rf_length(outer);

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);

    if (depth != 0 && is_spliceable(inner)) {
      count = list_splice_impl(info, inner, out, count, spliced, is_spliceable, depth - 1);
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

SEXP list_splice(SEXP dots, bool (*is_spliceable)(SEXP), int depth) {
  splice_info_t info = splice_info_init(true);
  splice_info(&info, dots, is_spliceable, depth);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, info.size));

  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  list_splice_impl(info, dots, out, 0, false, is_spliceable, depth);

  UNPROTECT(1);
  return out;
}


// Export ------------------------------------------------------------

bool is_spliceable_atomic_implicit(SEXP x) {
  if (OBJECT(x) && !Rf_inherits(x, "spliced"))
    Rf_errorcall(R_NilValue, "Cannot splice S3 objects");

  return is_list(x);
}
bool is_spliceable_atomic_explicit(SEXP x) {
  if (OBJECT(x) && !Rf_inherits(x, "spliced"))
    Rf_errorcall(R_NilValue, "Cannot splice S3 objects");

  return is_list(x) && Rf_inherits(x, "spliced");
}

bool is_spliceable_recursive_list(SEXP x) {
  return is_list(x);
}
bool is_spliceable_recursive_implicit(SEXP x) {
  return is_list(x) && (!OBJECT(x) || Rf_inherits(x, "spliced"));
}
bool is_spliceable_recursive_explicit(SEXP x) {
  return is_list(x) && Rf_inherits(x, "spliced");
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
  return as_bool(out);
}


SEXP rlang_splice_if(SEXP dots, SEXPTYPE kind, bool (*is_spliceable)(SEXP), int depth) {
  switch (kind) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return atom_splice(kind, dots, is_spliceable, depth);
  case VECSXP:
    return list_splice(dots, is_spliceable, depth);
  default:
    Rf_errorcall(R_NilValue, "Splicing is not implemented for this type");
    return R_NilValue;
  }
}
SEXP rlang_splice_closure(SEXP dots, SEXPTYPE kind, SEXP pred, int depth) {
  SEXP prev_pred = clo_spliceable;
  clo_spliceable = PROTECT(Rf_lang2(pred, Rf_list2(R_NilValue, R_NilValue)));

  SEXP out = rlang_splice_if(dots, kind, &is_spliceable_closure, depth);

  clo_spliceable = prev_pred;
  UNPROTECT(1);

  return out;
}
SEXP rlang_splice(SEXP dots, SEXP type, SEXP pred, SEXP depth_) {
  SEXPTYPE kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));
  int depth = Rf_asInteger(depth_);

  bool (*is_spliceable)(SEXP);
  switch (TYPEOF(pred)) {
  case LGLSXP:
    switch (kind) {
    case VECSXP:
      if (as_bool(pred))
        is_spliceable = &is_spliceable_recursive_implicit;
      else
        is_spliceable = &is_spliceable_recursive_explicit;
      break;
    default:
      if (as_bool(pred))
        is_spliceable = &is_spliceable_atomic_implicit;
      else
        is_spliceable = &is_spliceable_atomic_explicit;
      break;
    }
    break;
  case CLOSXP:
    return rlang_splice_closure(dots, kind, pred, depth);
  case EXTPTRSXP:
    is_spliceable = (bool (*)(SEXP)) R_ExternalPtrAddr(pred);
    break;
  default:
    Rf_errorcall(R_NilValue, "`predicate` must be a closure");
  }

  return rlang_splice_if(dots, kind, is_spliceable, depth);
}
