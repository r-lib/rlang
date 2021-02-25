#include <cmath>
using std::isfinite;

extern "C" {
// The altrep header uses `class` as a normal symbol
#define class rlang_class

#include <rlang.h>

#undef class
}


__attribute__((noreturn))
void rcc_abort(const char* fn) {
 try {
  throw;
 } catch (const std::exception& err) {
   r_abort(err.what());
 } catch (...) {
   r_stop_internal(fn, "Caught unknown C++ exception.");
 }
}
