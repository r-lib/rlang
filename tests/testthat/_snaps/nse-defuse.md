# auto-named expressions can be unique-repaired

    Code
      expect_equal(dots_names(1, foo = 1, 1, foo = 2), c("1...1", "foo", "1...3",
        "foo"))
    Message <message>
      New names:
      * 1 -> 1...1
      * 1 -> 1...3
    Code
      expect_equal(dots_names(bar, foo = 1, bar, foo = 2), c("bar...1", "foo",
        "bar...3", "foo"))
    Message <message>
      New names:
      * `bar` -> `bar...1`
      * `bar` -> `bar...3`

