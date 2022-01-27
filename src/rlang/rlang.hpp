#include <cmath>
using std::isfinite;

extern "C" {
#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wkeyword-macro"
#endif

#define class rlang_class

#ifdef __clang__
# pragma clang diagnostic pop
#endif

#include <rlang.h>

#undef class
}

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
