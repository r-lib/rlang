#define R_NO_REMAP
#include <Rinternals.h>

const char* rlang_version = "1.0.5";

/**
 * This file records the expected package version in the shared
 * library (or DLL) of the package. This is useful to check that users
 * have properly installed your package. Installation issues where the
 * package is updated but the DLL isn't are common on Windows in
 * particular. To automatically check that the native library of the
 * package was properly installed:
 *
 * - Register the function below as a C callable under the name
 *   "ffi_linked_version".
 *
 * - Call `rlang::check_linked_version(pkg_name)` from your
 *   `.onLoad()` hook. If you don't depend on rlang copy the
 *   compat-linked-version.R file from the rlang repository to your R
 *   folder. Find it at
 *   <https://github.com/r-lib/rlang/tree/master/R/compat-linked-version.R>
 */

// [[ register() ]]
SEXP rlang_linked_version() {
  return Rf_mkString(rlang_version);
}
