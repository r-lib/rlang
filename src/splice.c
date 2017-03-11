#include "utils.h"
#include "vector.h"


typedef struct {
  R_len_t size;
  bool named;
  bool warned;
} splice_info_t;

void splice_warn_names(void) {
  Rf_warning("Outer names are only allowed for unnamed scalar atomic inputs");
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
  SEXP inner;

  while (i != Rf_length(outer)) {
    inner = VECTOR_ELT(outer, i);
    atom_splice_check_names(info, inner, outer, i);

    if (is_atomic(inner)) {
      info->size += Rf_length(inner);
    } else if (inner != R_NilValue) {
      Rf_error("Cannot splice objects of type `%s` within a `%s`",
               kind_c_str(TYPEOF(inner)), kind_c_str(kind));
    }
    i++;
  }
}

void atom_splice_info(splice_info_t* info, SEXPTYPE kind,
                      SEXP dots, bool bare) {
  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);
    atom_splice_check_names(info, cur, dots, i);

    if (is_list(cur)) {
      bool is_spliced = Rf_inherits(cur, "spliced");
      if (!bare && !is_spliced)
        Rf_error("Bare lists cannot be spliced");
      if (is_object(cur) && !is_spliced)
        Rf_error("Objects cannot be spliced");
      atom_splice_info_list(info, kind, cur);
    } else {
      info->size += Rf_length(cur);
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
    } else if (x != R_NilValue) {
      Rf_error("Cannot splice objects of type `%s` within a `%s`",
               kind_c_str(TYPEOF(x)), kind_c_str(TYPEOF(outer)));
    }

    ++i;
  }

  return count;
}

SEXP atom_splice(SEXPTYPE kind, SEXP dots, bool bare) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;
  atom_splice_info(&info, kind, dots, bare);

  SEXP out = PROTECT(Rf_allocVector(kind, info.size));
  if (info.named)
    set_names(out, Rf_allocVector(STRSXP, info.size));

  atom_splice_list(dots, out, 0, info.named, true);

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

splice_info_t list_splice_info(SEXP dots, bool bare) {
  splice_info_t info;
  info.size = 0;
  info.named = false;
  info.warned = false;

  R_len_t i = 0;
  SEXP cur;

  while (i != Rf_length(dots)) {
    cur = VECTOR_ELT(dots, i);

    if (is_list(cur) && (Rf_inherits(cur, "spliced") || (bare && !is_object(cur)))) {
      info.size += Rf_length(cur);
      info.named = info.named || is_character(names(cur));
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

SEXP list_splice(SEXP dots, bool bare) {
  splice_info_t info = list_splice_info(dots, bare);
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

    if (is_list(x) && (Rf_inherits(x, "spliced") || (bare && !is_object(x)))) {
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
    return atom_splice(kind, dots, splice_bare);
  case VECSXP:
    return list_splice(dots, splice_bare);
  default:
    Rf_error("Splicing is not implemented for this type");
    return R_NilValue;
  }
}
