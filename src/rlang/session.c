#include "rlang.h"

r_obj* eval_with_x(r_obj* call, r_obj* x);


static r_obj* is_installed_call = NULL;

bool r_is_installed(const char* pkg) {
  r_obj* installed = eval_with_x(is_installed_call, KEEP(r_chr(pkg)));
  bool out = *r_lgl_begin(installed);

  FREE(1);
  return out;
}


static r_obj* has_colour_call = NULL;

bool r_has_colour() {
  if (!r_is_installed("crayon")) {
    return false;
  }

  return *r_lgl_begin(r_eval(has_colour_call, r_envs.base));
}


void r_init_library_session() {
  is_installed_call = r_parse("requireNamespace(x, quietly = TRUE)");
  r_preserve(is_installed_call);

  has_colour_call = r_parse("crayon::has_color()");
  r_preserve(has_colour_call);
}


#ifdef _WIN32

# include    <windows.h>
# include    <tlhelp32.h>

r_obj* r_getppid() {
  DWORD pid = GetCurrentProcessId();
  HANDLE handle = NULL;
  PROCESSENTRY32W pe = { 0 };

  pe.dwSize = sizeof(PROCESSENTRY32W);
  handle = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (handle == INVALID_HANDLE_VALUE) {
    r_abort("Can't query parent pid.");
  }

  if (Process32FirstW(handle, &pe)) {
    do {
      if (pe.th32ProcessID == pid) {
        DWORD ppid = pe.th32ParentProcessID;
        CloseHandle(handle);
        return r_int(ppid);
      }
    } while (Process32NextW(handle, &pe));
  }

  /* Should not get here */
  CloseHandle(handle);
  r_stop_internal("Can't find my own process.");
  return r_null;
}

#else

# include <unistd.h>

r_obj* r_getppid() {
  return r_int(getppid());
}

#endif
