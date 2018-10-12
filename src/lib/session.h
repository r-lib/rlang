#ifndef RLANG_SESSION_H
#define RLANG_SESSION_H


static inline bool r_runs_gnu() {
#ifdef NO_GNUR
  return false;
#else
  return true;
#endif
}

bool r_is_installed(const char* pkg);
bool r_has_colour();


#endif
