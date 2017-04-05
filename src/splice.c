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
void update_info(splice_info_t* info,
                 SEXP outer, R_len_t i, SEXP inner,
                 R_len_t n_inner, bool spliced) {
  info->size += n_inner;

  // Return early if possible
  if (info->named && info->warned)
    return;

  bool named = is_character(names(inner));
  bool recursive = info->recursive;

  bool copy_outer = recursive ? (!spliced) : (n_inner == 1);
  bool copy_inner = recursive ? spliced : true;

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
void atom_splice_info(splice_info_t* info, SEXP outer,
                      int depth, bool spliced,
                      bool (*is_spliceable)(SEXP)) {
  SEXP inner;
  R_len_t n_inner;
  R_len_t n_outer = Rf_length(outer);

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = vec_length(inner);

    if (depth != 0 && is_spliceable(inner))
      atom_splice_info(info, inner, --depth, true, is_spliceable);
    else if (n_inner)
      update_info(info, outer, i, inner, n_inner, spliced);
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
                         int depth, bool spliced,
                         bool (*is_spliceable)(SEXP)) {
  SEXP inner;
  SEXP out_names = names(out);
  R_len_t n_outer = Rf_length(outer);
  R_len_t n_inner;

  for (R_len_t i = 0; i != n_outer; ++i) {
    inner = VECTOR_ELT(outer, i);
    n_inner = vec_length(inner);

    if (depth != 0 && is_spliceable(inner)) {
      count = atom_splice_list(kind, info, inner, out, count, --depth, true, is_spliceable);
    } else if (n_inner) {
      vec_copy_coerce_n(inner, n_inner, out, count, 0);
      if (info.named)
        splice_copy_names_n(info.recursive, spliced, inner, n_inner, outer, i, out_names, count);
    }

    count += n_inner;
  }

  return count;
}

static
SEXP atom_splice(SEXPTYPE kind, SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info = splice_info_init(false);
  atom_splice_info(&info, dots, 1, false, is_spliceable);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  atom_splice_list(kind, info, dots, out, 0, 1, false, is_spliceable);

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info = splice_info_init(true);

  R_len_t i = 0;
  SEXP x;

  while (i != Rf_length(dots)) {
    x = VECTOR_ELT(dots, i);

    if (is_spliceable(x)) {
      info.size += Rf_length(x);
      info.named = info.named || is_character(names(x));
      if (has_name_at(dots, i) && !info.warned) {
        splice_warn_names();
        info.warned = true;
      }
    } else {
      info.named = info.named || has_name_at(dots, i);
      info.size += 1;
    }

    ++i;
  }

  return info;
}

SEXP list_splice(SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info = list_splice_info(dots, is_spliceable);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, info.size));

  if (info.named) {
    set_names(out, Rf_allocVector(STRSXP, info.size));
  }
  SEXP out_names = names(out);

  R_len_t i = 0;
  R_len_t count = 0;
  SEXP x;

  while (count != info.size) {
    x = VECTOR_ELT(dots, i);

    if (is_spliceable(x)) {
      R_len_t n = Rf_length(x);
      vec_copy_n(x, n, out, count, 0);

      if (info.named && is_character(names(x)))
        vec_copy_n(names(x), n, out_names, count, 0);

      count += n;
    } else {
      SET_VECTOR_ELT(out, count, x);

      if (info.named && is_character(names(dots))) {
        SEXP name = STRING_ELT(names(dots), i);
        SET_STRING_ELT(out_names, count, name);
      }

      count += 1;
    }

    ++i;
  }

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

bool is_spliceable_recursive_full(SEXP x) {
  return is_vector(x);
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


SEXP rlang_splice_if(SEXP dots, SEXPTYPE kind, bool (*is_spliceable)(SEXP)) {
  switch (kind) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return atom_splice(kind, dots, is_spliceable);
  case VECSXP:
    return list_splice(dots, is_spliceable);
  default:
    Rf_errorcall(R_NilValue, "Splicing is not implemented for this type");
    return R_NilValue;
  }
}
SEXP rlang_splice_closure(SEXP dots, SEXPTYPE kind, SEXP pred) {
  SEXP prev_pred = clo_spliceable;
  clo_spliceable = PROTECT(Rf_lang2(pred, Rf_list2(R_NilValue, R_NilValue)));

  SEXP out = rlang_splice_if(dots, kind, &is_spliceable_closure);

  clo_spliceable = prev_pred;
  UNPROTECT(1);

  return out;
}
SEXP rlang_splice(SEXP dots, SEXP type, SEXP pred) {
  SEXPTYPE kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));

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
    return rlang_splice_closure(dots, kind, pred);
  case EXTPTRSXP:
    is_spliceable = (bool (*)(SEXP)) R_ExternalPtrAddr(pred);
    break;
  default:
    Rf_errorcall(R_NilValue, "`predicate` must be a closure");
  }

  return rlang_splice_if(dots, kind, is_spliceable);
}
