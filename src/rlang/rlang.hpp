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
