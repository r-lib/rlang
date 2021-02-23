extern "C" {
#include <rlang.h>
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
