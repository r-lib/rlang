# Names and numbers of symbols bound in an environment

`env_names()` returns object names from an enviroment `env` as a
character vector. All names are returned, even those starting with a
dot. `env_length()` returns the number of bindings.

## Usage

``` r
env_names(env)

env_length(env)
```

## Arguments

- env:

  An environment.

## Value

A character vector of object names.

## Names of symbols and objects

Technically, objects are bound to symbols rather than strings, since the
R interpreter evaluates symbols (see
[`is_expression()`](https://rlang.r-lib.org/dev/reference/is_expression.md)
for a discussion of symbolic objects versus literal objects). However it
is often more convenient to work with strings. In rlang terminology, the
string corresponding to a symbol is called the *name* of the symbol (or
by extension the name of an object bound to a symbol).

## Encoding

There are deep encoding issues when you convert a string to symbol and
vice versa. Symbols are *always* in the native encoding. If that
encoding (let's say latin1) cannot support some characters, these
characters are serialised to ASCII. That's why you sometimes see strings
looking like `<U+1234>`, especially if you're running Windows (as R
doesn't support UTF-8 as native encoding on that platform).

To alleviate some of the encoding pain, `env_names()` always returns a
UTF-8 character vector (which is fine even on Windows) with ASCII
unicode points translated back to UTF-8.

## Examples

``` r
env <- env(a = 1, b = 2)
env_names(env)
#> [1] "a" "b"
```
