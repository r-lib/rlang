#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
} splice_info_t;

void splice_warn_names(void) {
  Rf_warningcall(R_NilValue, "Outer names are only allowed for unnamed scalar atomic inputs");
}

// Atomic splicing ---------------------------------------------------

void atom_splice_check_names(splice_info_t* info, SEXP inner, SEXP outer, R_len_t i) {
  if (has_name_at(outer, i)) {
    if (is_scalar_atomic(inner)) {
      info->named = true;
      if (!info->warned && is_character(names(inner)) && !is_empty(inner)) {
        splice_warn_names();
        info->warned = true;
      }
    } else if (!info->warned && !is_empty(inner)) {
      splice_warn_names();
      info->warned = true;
    }
  }
  if (is_atomic(inner) && is_character(names(inner)))
    info->named = true;
}

void atom_splice_info_list(splice_info_t* info, SEXPTYPE kind, SEXP outer) {
  R_len_t i = 0;
  SEXP x;

  while (i != Rf_length(outer)) {
    x = VECTOR_ELT(outer, i);
    atom_splice_check_names(info, x, outer, i);

    if (is_atomic(x)) {
      info->size += Rf_length(x);
    } else if (x != R_NilValue) {
      Rf_errorcall(R_NilValue,
                   "Cannot splice objects of type `%s` within a `%s`",
                   kind_c_str(TYPEOF(x)), kind_c_str(kind));
    }
    ++i;
  }
}

void atom_splice_info(splice_info_t* info, SEXPTYPE kind,
                      SEXP dots, bool (*is_spliceable)(SEXP)) {
  R_len_t i = 0;
  SEXP x;

  while (i != Rf_length(dots)) {
    x = VECTOR_ELT(dots, i);
    atom_splice_check_names(info, x, dots, i);

    if (is_list(x)) {
      if (!is_spliceable(x))
        Rf_errorcall(R_NilValue, "List cannot be spliced in");
      atom_splice_info_list(info, kind, x);
    } else if (!(is_vector(x) || is_null(x))) {
      Rf_errorcall(R_NilValue,
                   "Cannot splice objects of type `%s` within a `%s`",
                   kind_c_str(TYPEOF(x)), kind_c_str(kind));
    }  else {
      info->size += Rf_length(x);
    }

    ++i;
  }
}

R_len_t atom_splice_list(SEXP outer, SEXP out, R_len_t count,
                         bool named, bool recurse) {
  R_len_t i = 0;
  SEXP x;
  SEXP out_names = names(out);

  while (i != Rf_length(outer)) {
    x = VECTOR_ELT(outer, i);

    if (is_atomic(x)) {
      R_len_t n = Rf_length(x);
      vec_copy_coerce_n(x, n, out, count, 0);

      if (named) {
        if (is_character(names(x)))
          vec_copy_n(names(x), n, out_names, count, 0);
        else if (Rf_length(x) == 1 && has_name_at(outer, i))
          SET_STRING_ELT(out_names, count, STRING_ELT(names(outer), i));
      }

      count += n;
    } else if (is_list(x) && recurse) {
      count = atom_splice_list(x, out, count, named, false);
    }

    ++i;
  }

  return count;
}

SEXP atom_splice(SEXPTYPE kind, SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  atom_splice_info(&info, kind, dots, is_spliceable);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  atom_splice_list(dots, out, 0, info.named, true);

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool (*is_spliceable)(SEXP)) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;

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

bool is_implicitly_spliceable(SEXP x) {
  return is_list(x) && (Rf_inherits(x, "spliced") || !is_object(x));
}
bool is_explicitly_spliceable(SEXP x) {
  return is_list(x) && Rf_inherits(x, "spliced");
}

SEXP rlang_splice(SEXP dots, SEXP type, SEXP bare) {
  SEXPTYPE kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));

  bool (*is_spliceable)(SEXP);
  if (as_bool(bare))
    is_spliceable = &is_implicitly_spliceable;
  else
    is_spliceable = &is_explicitly_spliceable;

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
