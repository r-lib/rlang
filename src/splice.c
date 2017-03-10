#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
} splice_info_t;

void splice_names(SEXP outer, SEXP inner, SEXP out,
                  R_len_t i, R_len_t count,
                  bool* warned) {
  SEXP out_names = names(out);

  if (is_character(names(inner))) {
    vec_copy_n(names(inner), Rf_length(inner), out_names, count, 0);

    // Warn if outer names also present
    if (!(*warned) && has_name_at(outer, i)) {
      Rf_warning("Conflicting outer and inner names while splicing");
      *warned = true;
    }
  } else if (Rf_length(inner) == 1 && has_name_at(outer, i)) {
    SET_STRING_ELT(out_names, count, STRING_ELT(names(outer), i));
  }
}


// Atomic splicing ---------------------------------------------------

splice_info_t atm_splice_info_list(SEXPTYPE kind, SEXP x,
                                   splice_info_t info) {
  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(x)) {
    cur = VECTOR_ELT(x, i);
    info.named = info.named || is_character(names(cur));

    if (is_atomic(cur))
      info.size += Rf_length(cur);
    else
      Rf_error("Cannot splice a `%s` within a `%s`",
               Rf_type2str(TYPEOF(cur)), Rf_type2str(kind));
    i++;
  }

  return info;
}

splice_info_t atm_splice_info(SEXPTYPE kind, SEXP dots, bool bare) {
  splice_info_t info;
  info.size = 0;
  info.named = is_character(names(dots));

  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);
    info.named = info.named || is_character(names(cur));

    if (is_list(cur)) {
      bool is_spliced = Rf_inherits(cur, "spliced");
      if (!bare && !is_spliced)
        Rf_error("Bare lists cannot be spliced");
      if (is_object(cur) && !is_spliced)
        Rf_error("Objects cannot be spliced");
      info = atm_splice_info_list(kind, cur, info);
    } else {
      info.size += Rf_length(cur);
    }

    ++i;
  }

  return info;
}

R_len_t atm_splice_list(SEXP x, SEXP out, R_len_t count,
                        bool named, bool* warned) {
  R_len_t i = 0;
  R_len_t size = Rf_length(x);
  SEXP cur;

  while (i != size) {
    cur = VECTOR_ELT(x, i);
    R_len_t n = Rf_length(cur);

    vec_copy_coerce_n(cur, n, out, count, 0);

    if (named)
      splice_names(x, cur, out, i, count, warned);

    count += n;
    i++;
  }

  return count;
}

SEXP atm_splice(SEXPTYPE kind, SEXP dots, bool bare) {
  splice_info_t info = atm_splice_info(kind, dots, bare);
  SEXP out = PROTECT(Rf_allocVector(kind, info.size));

  if (info.named) {
    set_names(out, Rf_allocVector(STRSXP, info.size));
  }

  bool warned = false;
  R_len_t i = 0;
  R_len_t count = 0;
  SEXP cur;

  while (count != info.size) {
    cur = VECTOR_ELT(dots, i);

    if (is_atomic(cur)) {
      R_len_t n = Rf_length(cur);
      vec_copy_coerce_n(cur, n, out, count, 0);

      if (info.named)
        splice_names(dots, cur, out, i, count, &warned);

      count += n;
    } else if (is_list(cur)) {
      // Lists are valid since already checked during first pass
      if (info.named && !warned && has_name_at(dots, i))
        Rf_warning("Outer names of spliced lists are ignored");
      count = atm_splice_list(cur, out, count, info.named, &warned);
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

  bool warned = false;
  R_len_t i = 0;
  R_len_t count = 0;
  SEXP cur;

  while (count != info.size) {
    cur = VECTOR_ELT(dots, i);

    if (is_list(cur) && (Rf_inherits(cur, "spliced") || (bare && !is_object(cur)))) {
      R_len_t n = Rf_length(cur);
      vec_copy_n(cur, n, out, count, 0);

      if (info.named)
        splice_names(dots, cur, out, i, count, &warned);

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
