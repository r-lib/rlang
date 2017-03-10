#include "utils.h"
#include "vector.h"


struct splice_info_t {
  R_len_t size;
  bool named;
  splice_info_t() : size(0), named(false)
  { }
};

void splice_names(SEXP outer, SEXP inner, SEXP out,
                  R_len_t i, R_len_t count,
                  bool* warned) {
  SEXP out_names = names(out);

  if (is_character(names(inner))) {
    vec_copy_n<STRSXP>(names(inner), Rf_length(inner), out_names, count);

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

template <SEXPTYPE Kind>
splice_info_t& atm_splice_info_list(SEXP x, splice_info_t& info) {
  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(x)) {
    cur = VECTOR_ELT(x, i);
    info.named = info.named || is_character(names(cur));

    if (TYPEOF(cur) == Kind)
      info.size += Rf_length(cur);
    else
      Rf_error("Cannot splice a `TODO` within a `TODO`");
    i++;
  }

  return info;
}

template <SEXPTYPE Kind>
splice_info_t atm_splice_info(SEXP dots, bool bare) {
  splice_info_t info;
  info.named = is_character(names(dots));

  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);
    info.named = info.named || is_character(names(cur));

    switch (TYPEOF(cur)) {
    case Kind: {
      info.size += Rf_length(cur);
      break;
    }
    case VECSXP: {
      bool is_spliced = Rf_inherits(cur, "spliced");
      if (!bare && !is_spliced)
        Rf_error("Bare lists cannot be spliced");
      if (is_object(cur) && !is_spliced)
        Rf_error("Objects cannot be spliced");
      info = atm_splice_info_list<Kind>(cur, info);
      break;
    }
    default:
      Rf_error("Cannot splice a `TODO` within a `TODO`");
    }

    ++i;
  }

  return info;
}

template <SEXPTYPE Kind>
R_len_t atm_splice_list(SEXP x, SEXP out, R_len_t count,
                        bool named, bool* warned) {
  R_len_t i = 0;
  R_len_t size = Rf_length(x);
  SEXP cur;

  while (i != size) {
    cur = VECTOR_ELT(x, i);
    R_len_t n = Rf_length(cur);

    if (TYPEOF(cur) == Kind) {
      vec_copy_n<Kind>(cur, n, out, count);

      if (named)
        splice_names(x, cur, out, i, count, warned);
    }

    count += n;
    i++;
  }

  return count;
}

template <SEXPTYPE Kind>
SEXP atm_splice(SEXP dots, bool bare) {
  splice_info_t info = atm_splice_info<Kind>(dots, bare);
  SEXP out = PROTECT(Rf_allocVector(Kind, info.size));

  if (info.named) {
    set_names(out, Rf_allocVector(STRSXP, info.size));
  }

  bool warned = false;
  R_len_t i = 0;
  R_len_t count = 0;
  SEXP cur;

  while (count != info.size) {
    cur = VECTOR_ELT(dots, i);

    switch (TYPEOF(cur)) {
    case Kind: {
      R_len_t n = Rf_length(cur);
      vec_copy_n<Kind>(cur, n, out, count);

      if (info.named)
        splice_names(dots, cur, out, i, count, &warned);

      count += n;
      break;
    }
    case VECSXP: {
      // Lists are valid since already checked during first pass
      count = atm_splice_list<Kind>(cur, out, count, info.named, &warned);
      break;
    }
    default:
      break;
    }

    ++i;
  }

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool bare) {
  splice_info_t info;
  info.named = is_character(names(dots));

  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);

    switch (TYPEOF(cur)) {
    case VECSXP: {
      if (Rf_inherits(cur, "spliced") || (bare && !is_object(cur))) {
        info.size += Rf_length(cur);
        info.named = info.named || is_character(names(cur));
      } else {
        info.size += 1;
      }
      break;
    }
    default: {
      info.size += 1;
      break;
    }}

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

    switch (TYPEOF(cur)) {
    case VECSXP: {
      if (Rf_inherits(cur, "spliced") || (bare && !is_object(cur))) {
        R_len_t n = Rf_length(cur);
        vec_copy_n<VECSXP>(cur, n, out, count);

        if (info.named)
          splice_names(dots, cur, out, i, count, &warned);

        count += n;
        break;
      } // else fallthrough
    }
    default: {
      SET_VECTOR_ELT(out, count, cur);
      if (info.named && is_character(names(dots))) {
        SEXP name = STRING_ELT(names(dots), i);
        SET_STRING_ELT(out_names, count, name);
      }
      count += 1;
      break;
    }}

    ++i;
  }

  UNPROTECT(1);
  return out;
}


// Export ------------------------------------------------------------

extern "C"
SEXP rlang_splice(SEXP dots, SEXP type, SEXP bare) {
  bool splice_bare = *(LOGICAL(bare));

  switch (Rf_str2type(CHAR(STRING_ELT(type, 0)))) {
  case LGLSXP:
    return atm_splice<LGLSXP>(dots, splice_bare);
  case INTSXP:
    return atm_splice<INTSXP>(dots, splice_bare);
  case REALSXP:
    return atm_splice<REALSXP>(dots, splice_bare);
  case CPLXSXP:
    return atm_splice<CPLXSXP>(dots, splice_bare);
  case STRSXP:
    return atm_splice<STRSXP>(dots, splice_bare);
  case RAWSXP:
    return atm_splice<RAWSXP>(dots, splice_bare);
  case VECSXP:
    return list_splice(dots, splice_bare);
  default:
    Rf_error("Splicing is not implemented for this type");
    return R_NilValue;
  }
}
