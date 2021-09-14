# signallers work

    Code
      inform(c("Header.", i = "Bullet."))
    Message <rlang_message>
      Header.
      i Bullet.

---

    Code
      inform(c("Header.", i = "Bullet."))
    Message <simpleMessage>
      Header.
      Bullet.

---

    Code
      warn(c("Header.", i = "Bullet."))
    Warning <rlang_warning>
      Header.
      i Bullet.

---

    Code
      warn(c("Header.", i = "Bullet."))
    Warning <simpleWarning>
      Header.
      Bullet.

---

    Code
      abort(c("Header.", i = "Bullet."))
    Error <rlang_error>
      Header.
      i Bullet.

---

    Code
      abort(c("Header.", i = "Bullet."))
    Error <simpleError>
      Header.
      Bullet.

# unknown functions throw

    Code
      .rlang_compat("foo")
    Error <simpleError>
      Internal error in rlang shims: Unknown function `foo()`.

