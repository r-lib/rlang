# format.rlib_bytes() works with vectors

    Code
      print(as_bytes(c(NA, 1, 2^13, 2^20, NaN, 2^15)))
    Output
      [1]     NA B      1 B  8.19 kB  1.05 MB    NaN B 32.77 kB

# print method disambiguates edge cases

    Code
      print(bytes2())
    Output
      <rlib:bytes>
      [1] (empty)

---

    Code
      print(bytes2(NA, NA))
    Output
      <rlib:bytes>
      [1] NA B NA B

