
rlang_current_frame <- function() NULL
body <- as.call(list(base::sys.frame, -1L))
body(rlang_current_frame) <- body
rlang_current_frame <- as.call(list(rlang_current_frame))

rlang_sys_frame <- as.call(list(base::sys.frame, 0L))
rlang_sys_call <- as.call(list(base::sys.call, 0L))
