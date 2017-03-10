#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
} splice_info_t;

void splice_names(SEXP outer, SEXP inner, SEXP out,
                  R_len_t i, R_len_t count) {
  SEXP out_names = names(out);

  if (is_character(names(inner))) {
    vec_copy_n(names(inner), Rf_length(inner), out_names, count, 0);

  } else if (Rf_length(inner) == 1 && has_name_at(outer, i)) {
    SET_STRING_ELT(out_names, count, STRING_ELT(names(outer), i));
  }
}


// Atomic splicing ---------------------------------------------------

void atm_splice_check_names(splice_info_t* info, SEXP inner, SEXP outer, R_len_t i) {
  if (has_name_at(outer, i)) {
    if (is_scalar_atomic(inner)) {
      info->named = true;
      if (!info->warned && is_character(names(inner))) {
        Rf_warning("Outer names are only allowed for unnamed scalar atomic inputs");
        info->warned = true;
      }
    } else if (!info->warned) {
      Rf_warning("Outer names are only allowed for unnamed scalar atomic inputs");
      info->warned = true;
    }
  }
  if (is_atomic(inner) && is_character(names(inner)))
    info->named = true;
}

void atm_splice_info_list(splice_info_t* info, SEXPTYPE kind, SEXP outer) {
  R_len_t i = 0;
  SEXP inner;

  while (i != Rf_length(outer)) {
    inner = VECTOR_ELT(outer, i);
    atm_splice_check_names(info, inner, outer, i);

    if (is_atomic(inner))
      info->size += Rf_length(inner);
    else
      Rf_error("Cannot splice a `%s` within a `%s`",
               Rf_type2str(TYPEOF(inner)), Rf_type2str(kind));
    i++;
  }
}

void atm_splice_info(splice_info_t* info, SEXPTYPE kind, SEXP dots, bool bare) {
  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);
    atm_splice_check_names(info, cur, dots, i);

    if (is_list(cur)) {
      bool is_spliced = Rf_inherits(cur, "spliced");
      if (!bare && !is_spliced)
        Rf_error("Bare lists cannot be spliced");
      if (is_object(cur) && !is_spliced)
        Rf_error("Objects cannot be spliced");
      atm_splice_info_list(info, kind, cur);
    } else {
      info->size += Rf_length(cur);
    }

    ++i;
  }
}

R_len_t atm_splice_list(SEXP x, SEXP out, R_len_t count, bool named) {
  R_len_t i = 0;
  R_len_t size = Rf_length(x);
  SEXP cur;

  while (i != size) {
    cur = VECTOR_ELT(x, i);
    R_len_t n = Rf_length(cur);

    vec_copy_coerce_n(cur, n, out, count, 0);

    if (named)
      splice_names(x, cur, out, i, count);

    count += n;
    i++;
  }

  return count;
}

SEXP atm_splice(SEXPTYPE kind, SEXP dots, bool bare) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  atm_splice_info(&info, kind, dots, bare);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  R_len_t i = 0;
  R_len_t count = 0;
  SEXP cur;

  while (count != info.size) {
    cur = VECTOR_ELT(dots, i);

    if (is_atomic(cur)) {
      R_len_t n = Rf_length(cur);
      vec_copy_coerce_n(cur, n, out, count, 0);

      if (info.named)
        splice_names(dots, cur, out, i, count);

      count += n;
    } else if (is_list(cur)) {
      count = atm_splice_list(cur, out, count, info.named);
    }

    ++i;
  }

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool bare) {
  splice_info_t info;
  info.size = 0;
  info.named = is_character(names(dots));

  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);

    if (is_list(cur) && (Rf_inherits(cur, "spliced") || (bare && !is_object(cur)))) {
      info.size += Rf_length(cur);
      info.named = info.named || is_character(names(cur));
    } else {
      info.size += 1;
    }

    ++i;
  }

  return info;
}

SEXP list_splice(SEXP dots, bool bare) {
  splice_info_t info = list_splice_info(dots, bare);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, info.size));

  if (info.named) {
    set_names(out, Rf_allocVector(STRSXP, info.size));
  }
  SEXP out_names = names(out);

  R_len_t i = 0;
  R_len_t count = 0;
  SEXP cur;

  while (count != info.size) {
    cur = VECTOR_ELT(dots, i);

    if (is_list(cur) && (Rf_inherits(cur, "spliced") || (bare && !is_object(cur)))) {
      R_len_t n = Rf_length(cur);
      vec_copy_n(cur, n, out, count, 0);

      if (info.named)
        splice_names(dots, cur, out, i, count);

      count += n;
    } else {
      SET_VECTOR_ELT(out, count, cur);

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

SEXP rlang_splice(SEXP dots, SEXP type, SEXP bare) {
  bool splice_bare = *(LOGICAL(bare));
  SEXPTYPE kind = Rf_str2type(CHAR(STRING_ELT(type, 0)));

  switch (kind) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return atm_splice(kind, dots, splice_bare);
  case VECSXP:
    return list_splice(dots, splice_bare);
  default:
    Rf_error("Splicing is not implemented for this type");
    return R_NilValue;
  }
}
