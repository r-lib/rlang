# internal error is thrown with OOB dyn-lof access

    Code
      err(lof_arr_push_back(lof, 0, 42L), "Location 0 does not exist")
    Output
      <error/rlang_error>
      Error in `lof_arr_push_back()`:
      ! Location 0 does not exist.
      i In file 'rlang/dyn-list-of.c' at line 167.
      i This is an internal error that was detected in the rlang package.
        Please report it at <https://github.com/r-lib/rlang/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.
    Code
      err(lof_arr_push_back(lof, 10, 42L), "Location 10 does not exist")
    Output
      <error/rlang_error>
      Error in `lof_arr_push_back()`:
      ! Location 10 does not exist.
      i In file 'rlang/dyn-list-of.c' at line 167.
      i This is an internal error that was detected in the rlang package.
        Please report it at <https://github.com/r-lib/rlang/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# re-encoding fails purposefully with any bytes

    Code
      r_obj_encode_utf8(bytes)
    Condition <simpleError>
      Error in `r_obj_encode_utf8()`:
      ! translating strings with "bytes" encoding is not allowed

---

    Code
      (expect_error(r_obj_encode_utf8(c(enc, bytes))))
    Output
      <simpleError in r_obj_encode_utf8(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

---

    Code
      (expect_error(r_obj_encode_utf8(c(enc, bytes))))
    Output
      <simpleError in r_obj_encode_utf8(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

