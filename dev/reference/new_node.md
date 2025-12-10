# Helpers for pairlist and language nodes

**Important**: These functions are for expert R programmers only. You
should only use them if you feel comfortable manipulating low level R
data structures at the C level. We export them at the R level in order
to make it easy to prototype C code. They don't perform any type
checking and can crash R very easily (try to take the CAR of an integer
vector — save any important objects beforehand!).

## Usage

``` r
new_node(car, cdr = NULL)

node_car(x)

node_cdr(x)

node_caar(x)

node_cadr(x)

node_cdar(x)

node_cddr(x)

node_poke_car(x, newcar)

node_poke_cdr(x, newcdr)

node_poke_caar(x, newcar)

node_poke_cadr(x, newcar)

node_poke_cdar(x, newcdr)

node_poke_cddr(x, newcdr)

node_tag(x)

node_poke_tag(x, newtag)
```

## Arguments

- car, newcar, cdr, newcdr:

  The new CAR or CDR for the node. These can be any R objects.

- x:

  A language or pairlist node. Note that these functions are barebones
  and do not perform any type checking.

- newtag:

  The new tag for the node. This should be a symbol.

## Value

Setters like `node_poke_car()` invisibly return `x` modified in place.
Getters return the requested node component.

## See also

[`duplicate()`](https://rlang.r-lib.org/dev/reference/duplicate.md) for
creating copy-safe objects and
[`base::pairlist()`](https://rdrr.io/r/base/list.html) for an easier way
of creating a linked list of nodes.
