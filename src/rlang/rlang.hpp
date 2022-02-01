#ifndef RLANG_RLANG_HPP
#define RLANG_RLANG_HPP

#include <cmath>
#include <exception>
using std::isfinite;

// Include Rinternals.h with C++ linkage to avoid rlang including it while
// having C linkage, which causes issues with the GHA Mac machine
#define R_NO_REMAP
#include <Rinternals.h>

extern "C" {
#include <rlang.h>
}

static inline
r_no_return
void rcc_abort(const char* fn) {
 try {
  throw;
 } catch (const std::exception& err) {
   r_abort(err.what());
 } catch (...) {
   r_obj* call = KEEP(r_call(r_sym(fn)));
   (r_stop_internal)("", -1, call, "Caught unknown C++ exception.");
 }
}

#endif
