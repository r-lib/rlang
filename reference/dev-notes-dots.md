# Development notes - `dots.R`

Development notes - `dots.R`

## `.__error_call__.` flag in dots collectors

Dots collectors like
[`dots_list()`](https://rlang.r-lib.org/reference/list2.md) are a little
tricky because they may error out in different situations. Do we want to
forward the context, i.e. set the call flag to the calling environment?
Collectors throw errors in these cases:

1.  While checking their own parameters, in which case the relevant
    context is the collector itself and we don't forward.

2.  While collecting the dots, during evaluation of the supplied
    arguments. In this case forwarding or not is irrelevant because
    expressions in `...` are evaluated in their own environment which is
    not connected to the collector's context.

3.  While collecting the dots, during argument constraints checks such
    as determined by the `.homonyms` argument. In this case we want to
    forward the context because the caller of the dots collector is the
    one who determines the constraints for its users.
