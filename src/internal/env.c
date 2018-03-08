#include <rlang.h>

#define FRAME_LOCK_MASK (1 << 14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~FRAME_LOCK_MASK))

// Should only be used in development tools
sexp* rlang_env_unlock(sexp* env) {
  UNLOCK_FRAME(env);
  return FRAME_IS_LOCKED(env) == 0 ? r_shared_true : r_shared_false;
}
