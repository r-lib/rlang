#include "rlang.h"

using namespace rlang;


struct splice_info_t {
  r::size_t size;
  bool named;
  splice_info_t() : size(0), named(false)
  { }
};


// Typed splicing ----------------------------------------------------

template <sexp_e Kind>
splice_info_t& splice_info_list(sexp* x, splice_info_t& info) {
  r::size_t i = 0;
  sexp* cur;

  while (i != sxp::length(x)) {
    cur = list::get(x, i);
    info.named = info.named || !sxp::is_null(vec::names(cur));

    if (sxp::kind(cur) == Kind)
      info.size += sxp::length(cur);
    else
      r::abort("Cannot splice a `TODO` within a `TODO`");
    i++;
  }

  return info;
}

template <sexp_e Kind>
splice_info_t splice_info(sexp* dots, bool bare) {
  splice_info_t info;
  info.named = !sxp::is_null(vec::names(dots));

  r::size_t i = 0;
  sexp* cur;

  while (i != sxp::length(dots)) {
    cur = list::get(dots, i);
    info.named = info.named || !sxp::is_null(vec::names(cur));

    switch (sxp::kind(cur)) {
    case Kind: {
      info.size += sxp::length(cur);
      break;
    }
    case r::list_t: {
      bool is_spliced = sxp::inherits(cur, "spliced");
      if (!bare && !is_spliced)
        r::abort("Bare lists cannot be spliced");
      if (sxp::is_object(cur) && !is_spliced)
        r::abort("Objects cannot be spliced");
      info = splice_info_list<Kind>(cur, info);
      break;
    }
    default:
      r::abort("Cannot splice a `TODO` within a `TODO`");
    }

    ++i;
  }

  return info;
}

void splice_names(sexp* outer, sexp* inner, sexp* out,
                  r::size_t i, r::size_t count,
                  bool* warned) {
  sexp* out_names = vec::names(out);

  if (!sxp::is_null(vec::names(inner))) {
    vec::copy_n<r::character_t>(vec::names(inner),
                                sxp::length(inner),
                                out_names, count);
    // Warn if outer names also present
    if (!(*warned) && vec::has_name_at(outer, i)) {
      r::warn("Conflicting outer and inner names while splicing");
      *warned = true;
    }
  } else if (sxp::length(inner) == 1 && vec::has_name_at(outer, i)) {
    chr::set(out_names, count, chr::get(vec::names(outer), i));
  }
}

template <sexp_e Kind>
r::size_t splice_list(sexp* x, sexp* out, r::size_t count,
                      bool named, bool* warned) {
  r::size_t i = 0;
  r::size_t size = sxp::length(x);
  sexp* cur;

  while (i != size) {
    cur = list::get(x, i);
    r::size_t n = sxp::length(cur);

    if (sxp::kind(cur) == Kind) {
      vec::copy_n<Kind>(cur, n, out, count);

      if (named)
        splice_names(x, cur, out, i, count, warned);
    }

    count += n;
    i++;
  }

  return count;
}

template <sexp_e Kind>
sexp* splice(sexp* dots, bool bare) {
  splice_info_t info = splice_info<Kind>(dots, bare);
  sexp* out = PROTECT(vec::alloc(Kind, info.size));

  if (info.named) {
    vec::set_names(out, vec::alloc(r::character_t, info.size));
  }

  bool warned = false;
  r::size_t i = 0;
  r::size_t count = 0;
  sexp* cur;

  while (count != info.size) {
    cur = list::get(dots, i);

    switch (sxp::kind(cur)) {
    case Kind: {
      r::size_t n = sxp::length(cur);
      vec::copy_n<Kind>(cur, n, out, count);

      if (info.named)
        splice_names(dots, cur, out, i, count, &warned);

      count += n;
      break;
    }
    case r::list_t: {
      // Lists are valid since already checked during first pass
      count = splice_list<Kind>(cur, out, count, info.named, &warned);
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

template <>
splice_info_t splice_info<r::list_t>(sexp* dots, bool bare) {
  splice_info_t info;

  r::size_t i = 0;
  sexp* cur;

  while (i != sxp::length(dots)) {
    cur = list::get(dots, i);
    switch (sxp::kind(cur)) {
    case r::list_t: {
      if (sxp::inherits(cur, "spliced") || (bare && !sxp::is_object(cur)))
        info.size += sxp::length(cur);
      else
        info.size += 1;
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

template <>
sexp* splice<r::list_t>(sexp* dots, bool bare) {
  splice_info_t info = splice_info<r::list_t>(dots, bare);
  sexp* out = PROTECT(vec::alloc(r::list_t, info.size));

  r::size_t i = 0;
  r::size_t count = 0;
  sexp* cur;

  while (count != info.size) {
    cur = list::get(dots, i);

    switch (sxp::kind(cur)) {
    case r::list_t: {
      if (sxp::inherits(cur, "spliced") || (bare && !sxp::is_object(cur))) {
        r::size_t n = sxp::length(cur);
        vec::copy_n<r::list_t>(cur, n, out, count);
        count += n;
        break;
      } // else fallthrough
    }
    default: {
      list::set(out, count, cur);
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
  bool splice_bare = lgl::as_bool(bare);

  switch (sxp::kind(chr::as_c_string(type))) {
  case r::logical_t: return splice<r::logical_t>(dots, splice_bare);
  case r::integer_t: return splice<r::integer_t>(dots, splice_bare);
  case r::double_t: return splice<r::double_t>(dots, splice_bare);
  case r::complex_t: return splice<r::complex_t>(dots, splice_bare);
  case r::character_t: return splice<r::character_t>(dots, splice_bare);
  case r::bytes_t: return splice<r::bytes_t>(dots, splice_bare);
  case r::list_t: return splice<r::list_t>(dots, splice_bare);
  default:
    r::abort("Splicing is not implemented for this type");
    return r::null;
  }
}
