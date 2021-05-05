# normalisation fails purposefully with any bytes

    Code
      (expect_error(r_normalise_encoding(bytes)))
    Output
      <simpleError in r_normalise_encoding(bytes): translating strings with "bytes" encoding is not allowed>

---

    Code
      (expect_error(r_normalise_encoding(c(enc, bytes))))
    Output
      <simpleError in r_normalise_encoding(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

---

    Code
      (expect_error(r_normalise_encoding(c(enc, bytes))))
    Output
      <simpleError in r_normalise_encoding(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

---

    Code
      (expect_error(r_normalise_encoding(c(enc, bytes))))
    Output
      <simpleError in r_normalise_encoding(c(enc, bytes)): translating strings with "bytes" encoding is not allowed>

