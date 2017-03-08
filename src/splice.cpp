#include "rlang.h"

using namespace rlang;


// Typed splicing ----------------------------------------------------

template <sexp_e Kind>
r::size_t splice_size_list(sexp* x) {
  r::size_t i = 0;
  r::size_t count = 0;
  sexp* cur;

  while (i != sxp::length(x)) {
    cur = list_get(x, i);
    if (sxp::kind(cur) == Kind)
      count = count + sxp::length(cur);
    else
      r::abort("Cannot splice a `TODO` within a `TODO`");
    i++;
  }

  return count;
}

template <sexp_e Kind>
r::size_t splice_size(sexp* dots, bool bare) {
  r::size_t count = 0;
  r::size_t i = 0;
  sexp* cur;

  while (i != sxp::length(dots)) {
    cur = list_get(dots, i);
    switch (sxp::kind(cur)) {
    case Kind: {
      count += sxp::length(cur);
      break;
    }
    case r::list_t: {
      bool is_spliced = sxp::inherits(cur, "spliced");
      if (!bare && !is_spliced)
        r::abort("Bare lists cannot be spliced");
      if (sxp::is_object(cur) && !is_spliced)
        r::abort("Objects cannot be spliced");
      count += splice_size_list<Kind>(cur);
      break;
    }
    default: r::abort("Cannot splice a `TODO` within a `TODO`");
    }

    ++i;
  }

  return count;
}

template <sexp_e Kind>
void splice_list(sexp* x, sexp* out, r::size_t* count) {
  r::size_t i = 0;
  r::size_t size = sxp::length(x);
  sexp* cur;
  while (i != size) {
    cur = list_get(x, i);

    if (sxp::kind(cur) == Kind) {
      r::size_t n = sxp::length(cur);
      vec_copy_n<Kind>(cur, n, out, *count);
      *count += n;
    } else {
      r::abort("Internal error: Incompatible type");
    }

    i++;
  }
}

template <sexp_e Kind>
sexp* splice(sexp* dots, bool bare) {
  r::size_t size = splice_size<Kind>(dots, bare);
  sexp* out = PROTECT(vec_alloc(Kind, size));

  r::size_t i = 0;
  r::size_t count = 0;
  sexp* cur;
  while (count != size) {
    cur = list_get(dots, i);

    switch (sxp::kind(cur)) {
    case Kind: {
      r::size_t n = sxp::length(cur);
      vec_copy_n<Kind>(cur, n, out, count);
      count += n;
      break;
    }
    case r::list_t: {
      // Lists are valid since already checked during first pass
      splice_list<Kind>(cur, out, &count);
      break;
    }
    default: {
      r::abort("todo error");
    }}

    ++i;
  }

  UNPROTECT(1);
  return out;
}


// List splicing -----------------------------------------------------

template <>
r::size_t splice_size<r::list_t>(sexp* dots, bool bare) {
  r::size_t count = 0;
  r::size_t i = 0;
  sexp* cur;

  while (i != sxp::length(dots)) {
    cur = list_get(dots, i);
    switch (sxp::kind(cur)) {
    case r::list_t: {
      if (sxp::inherits(cur, "spliced") || (bare && !sxp::is_object(cur)))
        count += sxp::length(cur);
      else
        count += 1;
      break;
    }
    default: {
      count += 1;
      break;
    }}

    ++i;
  }

  return count;
}

template <>
sexp* splice<r::list_t>(sexp* dots, bool bare) {
  r::size_t size = splice_size<r::list_t>(dots, bare);
  sexp* out = PROTECT(vec_alloc(r::list_t, size));

  r::size_t i = 0;
  r::size_t count = 0;
  sexp* cur;
  while (count != size) {
    cur = list_get(dots, i);

    switch (sxp::kind(cur)) {
    case r::list_t: {
      if (sxp::inherits(cur, "spliced") || (bare && !sxp::is_object(cur))) {
        r::size_t n = sxp::length(cur);
        vec_copy_n<r::list_t>(cur, n, out, count);
        count += n;
        break;
      } // else fallthrough
    }
    default: {
      list_set(out, count, cur);
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
sexp* rlang_splice(sexp* dots, sexp* type, sexp* bare) {
  bool splice_bare = as_bool(bare);

  switch (sxp::kind(str_pointer(type))) {
  case r::lgl_t: return splice<r::lgl_t>(dots, splice_bare);
  case r::int_t: return splice<r::int_t>(dots, splice_bare);
  case r::dbl_t: return splice<r::dbl_t>(dots, splice_bare);
  case r::cpl_t: return splice<r::cpl_t>(dots, splice_bare);
  case r::chr_t: return splice<r::chr_t>(dots, splice_bare);
  case r::raw_t: return splice<r::raw_t>(dots, splice_bare);
  case r::list_t: return splice<r::list_t>(dots, splice_bare);
  default:
    r::abort("Splicing is not implemented for this type");
    return r::null;
  }
}
