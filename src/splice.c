#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
} splice_info_t;

splice_info_t splice_info_init() {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  return info;
}

void splice_warn_names(void) {
  Rf_warningcall(R_NilValue, "Outer names are only allowed for unnamed scalar atomic inputs");
}

// Atomic splicing ---------------------------------------------------

// This returns 1 for environments
R_len_t storage_length(SEXPTYPE kind, SEXP x) {
  bool store_null = (kind == VECSXP);

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
    return store_null ? 1 : 0;
  default:
    return 1;
  }
}

static
void update_info(splice_info_t* info, SEXPTYPE kind,
                 SEXP outer, R_len_t i, SEXP inner,
                 bool merge_names) {
  R_len_t n = storage_length(kind, inner);
  info->size += n;

  if (!info->named || !merge_names) {
    bool named = is_character(names(inner));
    if (named)
      info->named = true;

    if (has_name_at(outer, i)) {
      if ((named || n != 1) && !info->warned) {
        splice_warn_names();
        info->warned = true;
      }
      if (n == 1)
        info->named = true;
    }
  }
}

static
void atom_splice_info_list(splice_info_t* info, SEXPTYPE kind, SEXP outer,
                           bool merge_names) {
  R_len_t i = 0;
  SEXP inner;

  while (i != Rf_length(outer)) {
    inner = VECTOR_ELT(outer, i);

    if (storage_length(kind, inner))
      update_info(info, kind, outer, i, inner, merge_names);

    ++i;
  }
}

static
void atom_splice_info(splice_info_t* info, SEXPTYPE kind,
                      SEXP outer, bool merge_names,
                      bool (*is_spliceable)(SEXP)) {
  R_len_t i = 0;
  SEXP inner;

  while (i != Rf_length(outer)) {
    inner = VECTOR_ELT(outer, i);

    if (is_spliceable(inner))
      atom_splice_info_list(info, kind, inner, merge_names);
    else if (storage_length(kind, inner))
      update_info(info, kind, outer, i, inner, merge_names);

    ++i;
  }
}

R_len_t atom_splice_list(SEXP outer, SEXP out, R_len_t count,
                         bool named, bool merge_names, bool recurse,
                         bool (*is_spliceable)(SEXP)) {
  R_len_t i = 0;
  SEXP x;
  SEXP out_names = names(out);

  while (i != Rf_length(outer)) {
    x = VECTOR_ELT(outer, i);

    if (is_spliceable(x) && recurse) {
      count = atom_splice_list(x, out, count, named, merge_names, false, is_spliceable);
    } else if (storage_length(TYPEOF(out), x)) {
      R_len_t n = Rf_length(x);
      vec_copy_coerce_n(x, n, out, count, 0);

      if (named) {
        if (is_character(names(x)))
          vec_copy_n(names(x), n, out_names, count, 0);
        else if (merge_names && Rf_length(x) == 1 && has_name_at(outer, i))
          SET_STRING_ELT(out_names, count, STRING_ELT(names(outer), i));
      }

      count += n;
    }

    ++i;
  }

  return count;
}

SEXP atom_splice(SEXPTYPE kind, SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info = splice_info_init();
  atom_splice_info(&info, kind, dots, true, is_spliceable);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  atom_splice_list(dots, out, 0, info.named, true, true, is_spliceable);

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info = splice_info_init();

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

bool is_atomic_spliceable(SEXP x) {
  if (OBJECT(x) && !Rf_inherits(x, "spliced"))
    Rf_errorcall(R_NilValue, "Cannot splice S3 objects");

  if (is_list(x))
    return true;

  if (!is_vector(x) && !is_null(x)) {
    Rf_errorcall(R_NilValue,
                 "Cannot splice objects of type `%s` within vector",
                 kind_c_str(TYPEOF(x)));
  }

  return false;
}
bool is_explicitly_spliceable(SEXP x) {
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
    if (as_bool(pred))
      is_spliceable = &is_atomic_spliceable;
    else
      is_spliceable = &is_explicitly_spliceable;
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
