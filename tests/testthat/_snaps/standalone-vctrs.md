# vec_ptype2() implements base coercions

    Code
      vec_ptype2(lgl(), chr())
    Condition
      Error:
      ! Can't combine types <logical> and <character>.

---

    Code
      vec_ptype2(factor("a"), lgl())
    Condition
      Error:
      ! Unimplemented class <factor>.

# vec_ptype_common() works

    Code
      vec_ptype_common(list(lgl(), dbl(), ""))
    Condition
      Error:
      ! Can't combine types <double> and <character>.

# lossy casts throw

    Code
      vec_cast(1.5, 2L)
    Condition
      Error:
      ! Can't convert <double> to <integer>.

