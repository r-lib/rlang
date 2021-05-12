# can style strings with cli [plain]

    Code
      style_emph("foo")
    Output
      [1] "_foo_"
    Code
      style_strong("foo")
    Output
      [1] "*foo*"
    Code
      style_code("foo")
    Output
      [1] "`foo`"
    Code
      style_q("foo")
    Output
      [1] "foo"
    Code
      style_pkg("foo")
    Output
      [1] "foo"
    Code
      style_fn("foo")
    Output
      [1] "`foo()`"
    Code
      style_arg("foo")
    Output
      [1] "`foo`"
    Code
      style_kbd("foo")
    Output
      [1] "[foo]"
    Code
      style_key("foo")
    Output
      [1] "[foo]"
    Code
      style_file("foo")
    Output
      [1] "foo"
    Code
      style_path("foo")
    Output
      [1] "foo"
    Code
      style_email("foo")
    Output
      [1] "foo"
    Code
      style_url("foo")
    Output
      [1] "<foo>"
    Code
      style_var("foo")
    Output
      [1] "`foo`"
    Code
      style_envvar("foo")
    Output
      [1] "`foo`"
    Code
      style_field("foo")
    Output
      [1] "foo"
    Code
      style_cls("foo")
    Output
      [1] "<foo>"
    Code
      style_cls(c("foo", "bar"))
    Output
      [1] "<foo/bar>"

# can style strings with cli [ansi]

    Code
      style_emph("foo")
    Output
      [1] "\033[1m\033[22m\033[3m\033[3mfoo\033[3m\033[23m"
    Code
      style_strong("foo")
    Output
      [1] "\033[1m\033[22m\033[1m\033[1mfoo\033[1m\033[22m"
    Code
      style_code("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m\033[30m\033[47m`foo`\033[47m\033[30m\033[49m\033[39m"
    Code
      style_q("foo")
    Output
      [1] "\033[1m\033[22mfoo"
    Code
      style_pkg("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34mfoo\033[34m\033[39m"
    Code
      style_fn("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m\033[30m\033[47m`foo()`\033[47m\033[30m\033[49m\033[39m"
    Code
      style_arg("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m\033[30m\033[47m`foo`\033[47m\033[30m\033[49m\033[39m"
    Code
      style_kbd("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34m[foo]\033[34m\033[39m"
    Code
      style_key("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34m[foo]\033[34m\033[39m"
    Code
      style_file("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34mfoo\033[34m\033[39m"
    Code
      style_path("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34mfoo\033[34m\033[39m"
    Code
      style_email("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34mfoo\033[34m\033[39m"
    Code
      style_url("foo")
    Output
      [1] "\033[1m\033[22m\033[3m\033[34m\033[3m\033[34m<foo>\033[34m\033[3m\033[39m\033[23m"
    Code
      style_var("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m\033[30m\033[47m`foo`\033[47m\033[30m\033[49m\033[39m"
    Code
      style_envvar("foo")
    Output
      [1] "\033[1m\033[22m\033[30m\033[47m\033[30m\033[47m`foo`\033[47m\033[30m\033[49m\033[39m"
    Code
      style_field("foo")
    Output
      [1] "\033[1m\033[22m\033[32m\033[32mfoo\033[32m\033[39m"
    Code
      style_cls("foo")
    Output
      [1] "\033[1m\033[22m\033[34m\033[34m<foo>\033[34m\033[39m"
    Code
      style_cls(c("foo", "bar"))
    Output
      [1] "\033[1m\033[22m\033[34m\033[34m<foo/bar>\033[34m\033[39m"

