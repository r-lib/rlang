# signallers work

    Code
      inform(c("Header.", i = "Bullet."))
    Message
      Header.
      i Bullet.

---

    Code
      inform(c("Header.", i = "Bullet."))
    Message
      Header.
      Bullet.

---

    Code
      warn(c("Header.", i = "Bullet."))
    Condition
      Warning:
      Header.
      i Bullet.

---

    Code
      warn(c("Header.", i = "Bullet."))
    Condition
      Warning:
      Header.
      Bullet.

---

    Code
      abort(c("Header.", i = "Bullet."))
    Condition
      Error:
      ! Header.
      i Bullet.

---

    Code
      abort(c("Header.", i = "Bullet."))
    Condition
      Error:
      ! Header.
      Bullet.

# unknown functions throw

    Code
      .rlang_compat("foo")
    Condition
      Error in `.rlang_compat()`:
      ! Internal error in rlang shims: Unknown function `foo()`.

