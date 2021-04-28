#include <rlang.h>
#include "file.h"

#ifdef _WIN32
#include <windows.h>
#endif

// This is needed to support wide character paths on windows.
// `path` is a CHARSXP containing the file path.
FILE* r_fopen(r_obj* path, const char* mode) {
  FILE* out;
  const void* vmax = vmaxget();

#ifdef _WIN32
  const char* path_c = Rf_translateCharUTF8(path);

  // First convert the mode to the wide equivalent.
  // Only usage is 2 characters ("rb") so max 8 bytes + 2 byte null.
  wchar_t mode_w[10];
  MultiByteToWideChar(CP_UTF8, 0, mode, -1, mode_w, 9);

  // Then convert the path
  size_t len = MultiByteToWideChar(CP_UTF8, 0, path_c, -1, NULL, 0);
  if (len <= 0) {
    r_abort("Can't convert file to Unicode: %s.", path_c);
  }

  wchar_t* buf = (wchar_t*)R_alloc(len, sizeof(wchar_t));
  if (buf == NULL) {
    r_abort("Can't allocate buffer of size %ll.", len);
  }

  MultiByteToWideChar(CP_UTF8, 0, path_c, -1, buf, len);
  out = _wfopen(buf, mode_w);
#else
  out = fopen(Rf_translateChar(path), mode);
#endif

  vmaxset(vmax);
  return out;
}
