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
      [1] NA NA

