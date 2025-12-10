# Format bullets for error messages

`format_error_bullets()` takes a character vector and returns a single
string (or an empty vector if the input is empty). The elements of the
input vector are assembled as a list of bullets, depending on their
names:

- Unnamed elements are unindented. They act as titles or subtitles.

- Elements named `"*"` are bulleted with a cyan "bullet" symbol.

- Elements named `"i"` are bulleted with a blue "info" symbol.

- Elements named `"x"` are bulleted with a red "cross" symbol.

- Elements named `"v"` are bulleted with a green "tick" symbol.

- Elements named `"!"` are bulleted with a yellow "warning" symbol.

- Elements named `">"` are bulleted with an "arrow" symbol.

- Elements named `" "` start with an indented line break.

For convenience, if the vector is fully unnamed, the elements are
formatted as "\*" bullets.

The bullet formatting for errors follows the idea that sentences in
error messages are best kept short and simple. The best way to present
the information is in the
[`cnd_body()`](https://rlang.r-lib.org/reference/cnd_message.md) method
of an error condition as a bullet list of simple sentences containing a
single clause. The info and cross symbols of the bullets provide hints
on how to interpret the bullet relative to the general error issue,
which should be supplied as
[`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md).

## Usage

``` r
format_error_bullets(x)
```

## Arguments

- x:

  A named character vector of messages. Named elements are prefixed with
  the corresponding bullet. Elements named with a single space `" "`
  trigger a line break from the previous bullet.

## Examples

``` r
# All bullets
writeLines(format_error_bullets(c("foo", "bar")))
#> • foo
#> • bar

# This is equivalent to
writeLines(format_error_bullets(set_names(c("foo", "bar"), "*")))
#> • foo
#> • bar

# Supply named elements to format info, cross, and tick bullets
writeLines(format_error_bullets(c(i = "foo", x = "bar", v = "baz", "*" = "quux")))
#> ℹ foo
#> ✖ bar
#> ✔ baz
#> • quux

# An unnamed element breaks the line
writeLines(format_error_bullets(c(i = "foo\nbar")))
#> ℹ foo
#> bar

# A " " element breaks the line within a bullet (with indentation)
writeLines(format_error_bullets(c(i = "foo", " " = "bar")))
#> ℹ foo
#>   bar
```
