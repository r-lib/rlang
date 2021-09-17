# cnd_type_header() formats condition classes

    Code
      cnd_type_header(error_cnd())
    Output
      [1] "<error/rlang_error>"
    Code
      cnd_type_header(warning_cnd())
    Output
      [1] "<warning/rlang_warning>"
    Code
      cnd_type_header(message_cnd())
    Output
      [1] "<message/rlang_message>"
    Code
      cnd_type_header(error_cnd(class = "foobar"))
    Output
      [1] "<error/foobar>"

# can format warnings and other conditions

    <warning/rlang_warning>
    Warning in `quux()`: Header.
    i Bullet.
    Backtrace:
     1. foo()
     2. bar()

---

    <message/rlang_message>
    Message in `quux()`: 
      Header.
      i Bullet.
    Caused by warning in `quux()`: 
      Header.
      i Bullet.
    Backtrace:
     1. foo()
     2. bar()

---

    <condition/foobar>
    Condition in `quux()`: Header.
    i Bullet.
    Backtrace:
     1. foo()
     2. bar()

