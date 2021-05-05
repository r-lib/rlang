# re-encoding fails purposefully with any bytes

    Code
      (expect_error(r_obj_encode_utf8(bytes)))
    Output
      <simpleError in r_obj_encode_utf8(bytes): translating strings with "bytes" encoding is not allowed>

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

---

    Code
      (expect_error(r_obj_encode_utf8(c(enc, bytes))))
    Output
      <simpleError in r_obj_encode_utf8(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

