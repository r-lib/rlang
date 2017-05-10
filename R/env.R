#' Create a new environment
#'
#' @description
#'
#' These functions create new environments.
#'
#' * `env()` always creates a child of the current environment.
#'
#' * `child_env()` lets you specify a parent (see section on
#'   inheritance).
#'
#' * `new_environment()` creates a child of the empty environment. It
#'   is useful e.g. for using environments as containers of data
#'   rather than as part of a scope hierarchy.
#'
#' @section Environments as objects:
#'
#' Environments are containers of uniquely named objects. Their most
#' common use is to provide a scope for the evaluation of R
#' expressions. Not all languages have first class environments,
#' i.e. can manipulate scope as regular objects. Reification of scope
#' is one of the most powerful feature of R as it allows you to change
#' what objects a function or expression sees when it is evaluated.
#'
#' Environments also constitute a data structure in their own
#' right. They are [dictionaries][dictionary] of uniquely named
#' objects, subsettable by name and modifiable by reference. This
#' latter property (see section on reference semantics) is especially
#' useful for creating mutable OO systems (cf the [R6
#' package](https://github.com/wch/R6) and the [ggproto
#' system](http://ggplot2.tidyverse.org/articles/extending-ggplot2.html)
#' for extending ggplot2).
#'
#' @section Inheritance:
#'
#' All R environments (except the [empty environment][empty_env]) are
#' defined with a parent environment. An environment and its
#' grandparents thus form a linear hierarchy that is the basis for
#' [lexical
#' scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)) in
#' R. When R evaluates an expression, it looks up symbols in a given
#' environment. If it cannot find these symbols there, it keeps
#' looking them up in parent environments. This way, objects defined
#' in child environments have precedence over objects defined in
#' parent environments.
#'
#' The ability of overriding specific definitions is used in the
#' tidyeval framework to create powerful domain-specific grammars. A
#' common use of overscoping is to put data frame columns in
#' scope. See [as_overscope()] for technical details.
#'
#' @section Reference semantics:
#'
#' Unlike regular objects such as vectors, environments are an
#' [uncopyable][is_copyable()] object type. This means that if you
#' have multiple references to a given environment (by assigning the
#' environment to another symbol with `<-` or passing the environment
#' as argument to a function), modifying the bindings of one of those
#' references changes all other references as well.
#'
#' @param ...,data Named values. The dots have [explicit splicing
#'   semantics][dots_list].
#' @param .parent A parent environment. Can be an object supported by
#'   [as_env()].
#' @seealso `scoped_env`, [env_has()], [env_bind()].
#' @export
#' @examples
#' # env() creates a new environment which has the current environment
#' # as parent
#' env <- env(a = 1, b = "foo")
#' env$b
#' identical(env_parent(env), get_env())
#'
#'
#' # child_env() lets you specify a parent:
#' child <- child_env(env, c = "bar")
#' identical(env_parent(child), env)
#'
#' # This child environment owns `c` but inherits `a` and `b` from `env`:
#' env_has(child, c("a", "b", "c", "d"))
#' env_has(child, c("a", "b", "c", "d"), inherit = TRUE)
#'
#' # `parent` is passed to as_env() to provide handy shortcuts. Pass a
#' # string to create a child of a package environment:
#' child_env("rlang")
#' env_parent(child_env("rlang"))
#'
#' # Or `NULL` to create a child of the empty environment:
#' child_env(NULL)
#' env_parent(child_env(NULL))
#'
#' # The base package environment is often a good default choice for a
#' # parent environment because it contains all standard base
#' # functions. Also note that it will never inherit from other loaded
#' # package environments since R keeps the base package at the tail
#' # of the search path:
#' base_child <- child_env("base")
#' env_has(base_child, c("lapply", "("), inherit = TRUE)
#'
#' # On the other hand, a child of the empty environment doesn't even
#' # see a definition for `(`
#' empty_child <- child_env(NULL)
#' env_has(empty_child, c("lapply", "("), inherit = TRUE)
#'
#' # Note that all other package environments inherit from base_env()
#' # as well:
#' rlang_child <- child_env("rlang")
#' env_has(rlang_child, "env", inherit = TRUE)     # rlang function
#' env_has(rlang_child, "lapply", inherit = TRUE)  # base function
#'
#'
#' # Both env() and child_env() take dots with explicit splicing:
#' objs <- list(b = "foo", c = "bar")
#' env <- env(a = 1, !!! objs)
#' env$c
#'
#' # You can also unquote names with the definition operator `:=`
#' var <- "a"
#' env <- env(!!var := "A")
#' env$a
#'
#'
#' # Use new_environment() to create containers with the empty
#' # environment as parent:
#' env <- new_environment()
#' env_parent(env)
#'
#' # Like other new_ constructors, it takes an object rather than dots:
#' new_environment(list(a = "foo", b = "bar"))
env <- function(...) {
  env <- new.env(parent = caller_env())
  env_bind_impl(env, dots_list(...))
}
#' @rdname env
#' @export
child_env <- function(.parent, ...) {
  env <- new.env(parent = as_env(.parent))
  env_bind_impl(env, dots_list(...))
}
#' @rdname env
#' @export
new_environment <- function(data = list()) {
  env <- new.env(parent = empty_env())
  env_bind_impl(env, data)
}

#' Coerce to an environment
#'
#' `as_env()` coerces named vectors (including lists) to an
#' environment. It first checks that `x` is a dictionary (see
#' [is_dictionaryish()]). If supplied an unnamed string, it returns the
#' corresponding package environment (see [pkg_env()]).
#'
#' If `x` is an environment and `parent` is not `NULL`, the
#' environment is duplicated before being set a new parent. The return
#' value is therefore a different environment than `x`.
#'
#' @param x An object to coerce.
#' @param parent A parent environment, [empty_env()] by default. This
#'   argument is only used when `x` is data actually coerced to an
#'   environment (as opposed to data representing an environment, like
#'   `NULL` representing the empty environment).
#' @export
#' @examples
#' # Coerce a named vector to an environment:
#' env <- as_env(mtcars)
#'
#' # By default it gets the empty environment as parent:
#' identical(env_parent(env), empty_env())
#'
#'
#' # With strings it is a handy shortcut for pkg_env():
#' as_env("base")
#' as_env("rlang")
#'
#' # With NULL it returns the empty environment:
#' as_env(NULL)
as_env <- function(x, parent = NULL) {
  coerce_type(x, "an environment",
    NULL = {
      empty_env()
    },
    environment = {
      x
    },
    string = {
      if (length(x) > 1 || is_named(x)) {
        return(as_env_(x, parent))
      }
      pkg_env(x)
    },
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw = ,
    list = {
      as_env_(x, parent)
    }
  )
}
as_env_ <- function(x, parent = NULL) {
  stopifnot(is_dictionaryish(x))
  if (is_atomic(x)) {
    x <- as_list(x)
  }
  list2env(x, parent = parent %||% empty_env())
}

#' Get parent environments
#'
#' @description
#'
#' - `env_parent()` returns the parent environment of `env` if called
#'   with `n = 1`, the grandparent with `n = 2`, etc.
#'
#' - `env_tail()` searches through the parents and returns the one
#'   which has [empty_env()] as parent.
#'
#' - `env_parents()` returns the list of all parents, including the
#'   empty environment.
#'
#' See the section on _inheritance_ in [env()]'s documentation.
#'
#' @inheritParams get_env
#' @param n The number of generations to go up.
#' @return An environment for `env_parent()` and `env_tail()`, a list
#'   of environments for `env_parents()`.
#' @export
#' @examples
#' # Get the parent environment with env_parent():
#' env_parent(global_env())
#'
#' # Or the tail environment with env_tail():
#' env_tail(global_env())
#'
#' # By default, env_parent() returns the parent environment of the
#' # current evaluation frame. If called at top-level (the global
#' # frame), the following two expressions are equivalent:
#' env_parent()
#' env_parent(base_env())
#'
#' # This default is more handy when called within a function. In this
#' # case, the enclosure environment of the function is returned
#' # (since it is the parent of the evaluation frame):
#' enclos_env <- env()
#' fn <- set_env(function() env_parent(), enclos_env)
#' identical(enclos_env, fn())
env_parent <- function(env = caller_env(), n = 1) {
  env_ <- get_env(env)

  while (n > 0) {
    if (is_empty_env(env_)) {
      return(env_)
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}
#' @rdname env_parent
#' @export
env_tail <- function(env = caller_env()) {
  env_ <- get_env(env)
  next_env <- parent.env(env_)

  while(!is_empty_env(next_env)) {
    env_ <- next_env
    next_env <- parent.env(next_env)
  }

  env_
}
#' @rdname env_parent
#' @export
env_parents <- function(env = caller_env()) {
  out <- list_len(env_depth(env))

  i <- 1L
  while(!is_empty_env(env)) {
    env <- env_parent(env)
    out[[i]] <- env
    i <- i + 1L
  }

  out
}

#' Depth of an environment chain
#'
#' This function returns the number of environments between `env` and
#' the [empty environment][empty_env()], including `env`. The depth of
#' `env` is also the number of parents of `env` (since the empty
#' environment counts as a parent).
#'
#' @inheritParams get_env
#' @return An integer.
#' @seealso The section on inheritance in [env()] documentation.
#' @export
#' @examples
#' env_depth(empty_env())
#' env_depth(pkg_env("rlang"))
env_depth <- function(env) {
  env_ <- get_env(env)

  n <- 0L
  while(!is_empty_env(env_)) {
    env_ <- env_parent(env_)
    n <- n + 1L
  }

  n
}
`_empty_env` <- emptyenv()
is_empty_env <- function(env) {
  is_reference(env, `_empty_env`)
}

#' Get or set the environment of an object
#'
#' These functions dispatch internally with methods for functions,
#' formulas and frames. If called with a missing argument, the
#' environment of the current evaluation frame (see [ctxt_stack()]) is
#' returned. If you call `get_env()` with an environment, it acts as
#' the identity function and the environment is simply returned (this
#' helps simplifying code when writing generic functions for
#' environments).
#'
#' @param env An environment or an object bundling an environment,
#'   e.g. a formula, [quosure] or [closure][is_closure].
#' @param default The default environment in case `env` does not wrap
#'   an environment. If `NULL` and no environment could be extracted,
#'   an error is issued.
#' @export
#' @examples
#' # Get the environment of frame objects. If no argument is supplied,
#' # the current frame is used:
#' fn <- function() {
#'   list(
#'     get_env(call_frame()),
#'     get_env()
#'   )
#' }
#' fn()
#'
#' # Environment of closure functions:
#' get_env(fn)
#'
#' # Or of quosures or formulas:
#' get_env(~foo)
#' get_env(quo(foo))
#'
#'
#' # Provide a default in case the object doesn't bundle an environment.
#' # Let's create an unevaluated formula:
#' f <- quote(~foo)
#'
#' # The following line would fail if run because unevaluated formulas
#' # don't bundle an environment (they didn't have the chance to
#' # record one yet):
#' # get_env(f)
#'
#' # It is often useful to provide a default when you're writing
#' # functions accepting formulas as input:
#' default <- env()
#' identical(get_env(f, default), default)
get_env <- function(env = caller_env(), default = NULL) {
  out <- switch_type(env,
    environment = env,
    definition = ,
    formula = attr(env, ".Environment"),
    primitive = base_env(),
    closure = environment(env),
    list = switch_class(env, frame = env$env)
  )

  out <- out %||% default

  if (is_null(out)) {
    type <- friendly_type(type_of(env))
    abort(paste0("Can't extract an environment from ", type))
  } else {
    out
  }
}
#' @rdname get_env
#' @param new_env An environment to replace `env` with. Can be an
#'   object handled by `get_env()`.
#' @export
#' @examples
#'
#' # set_env() can be used to set the enclosure of functions and
#' # formulas. Let's create a function with a particular environment:
#' env <- child_env("base")
#' fn <- set_env(function() NULL, env)
#'
#' # That function now has `env` as enclosure:
#' identical(get_env(fn), env)
#' identical(get_env(fn), get_env())
#'
#' # set_env() does not work by side effect. Setting a new environment
#' # for fn has no effect on the original function:
#' other_env <- child_env(NULL)
#' set_env(fn, other_env)
#' identical(get_env(fn), other_env)
#'
#' # Since set_env() returns a new function with a different
#' # environment, you'll need to reassign the result:
#' fn <- set_env(fn, other_env)
#' identical(get_env(fn), other_env)
set_env <- function(env, new_env = caller_env()) {
  switch_type(env,
    definition = ,
    formula = ,
    closure = {
      environment(env) <- get_env(new_env)
      env
    },
    environment = get_env(new_env),
    abort(paste0(
      "Can't set environment for ", friendly_type(type_of(env)), ""
    ))
  )
}

mut_parent_env <- function(env, new_env) {
  env_ <- get_env(env)
  parent.env(env_) <- get_env(new_env)
  env
}
`env_parent<-` <- function(x, value) {
  env_ <- get_env(x)
  parent.env(env_) <- get_env(value)
  x
}


#' Bind symbols to objects in an environment
#'
#' @description
#'
#' These functions create bindings in an environment. The bindings are
#' supplied through `...` as pairs of names and values or expressions.
#' `env_bind()` is equivalent to evaluating a `<-` expression within
#' the given environment. This function should take care of the
#' majority of use cases but the other variants can be useful for
#' specific problems.
#'
#' - `env_bind()` takes named _values_. The arguments are evaluated
#'   once (with [explicit splicing][dots_list]) and bound in `.env`.
#'   `env_bind()` is equivalent to [base::assign()].
#'
#' - `env_bind_fns()` takes named _functions_ and creates active
#'   bindings in `.env`. This is equivalent to
#'   [base::makeActiveBinding()]. An active binding executes a
#'   function each time it is evaluated. `env_bind_fns()` takes dots
#'   with [implicit splicing][dots_splice], so that you can supply
#'   both named functions and named lists of functions.
#'
#'   If these functions are [closures][is_closure] they are lexically
#'   scoped in the environment that they bundle. These functions can
#'   thus refer to symbols from this enclosure that are not actually
#'   in scope in the dynamic environment where the active bindings are
#'   invoked. This allows creative solutions to difficult problems
#'   (see the implementations of `dplyr::do()` methods for an
#'   example).
#'
#' - `env_bind_exprs()` takes named _expressions_. This is equivalent
#'   to [base::delayedAssign()]. The arguments are captured with
#'   [exprs()] (and thus support call-splicing and unquoting) and
#'   assigned to symbols in `.env`. These expressions are not
#'   evaluated immediately but lazily. Once a symbol is evaluated, the
#'   corresponding expression is evaluated in turn and its value is
#'   bound to the symbol (the expressions are thus evaluated only
#'   once, if at all).
#'
#' @section Side effects:
#'
#' Since environments have reference semantics (see relevant section
#' in [env()] documentation), modifying the bindings of an environment
#' produces effects in all other references to that environment. In
#' other words, `env_bind()` and its variants have side effects.
#'
#' As they are called primarily for their side effects, these
#' functions follow the convention of returning their input invisibly.
#'
#' @param .env An environment or an object bundling an environment,
#'   e.g. a formula, [quosure] or [closure][is_closure]. This argument
#'   is passed to [get_env()].
#' @param ... Pairs of names and expressions, values or
#'   functions. These dots support splicing (with varying semantics,
#'   see above) and name unquoting.
#' @return The input object `.env`, with its associated environment
#'   modified in place, invisibly.
#' @export
#' @examples
#' # env_bind() is a programmatic way of assigning values to symbols
#' # with `<-`. We can add bindings in the current environment:
#' env_bind(get_env(), foo = "bar")
#' foo
#'
#' # Or modify those bindings:
#' bar <- "bar"
#' env_bind(get_env(), bar = "BAR")
#' bar
#'
#' # It is most useful to change other environments:
#' my_env <- env()
#' env_bind(my_env, foo = "foo")
#' my_env$foo
#'
#' # A useful feature is to splice lists of named values:
#' vals <- list(a = 10, b = 20)
#' env_bind(my_env, !!! vals, c = 30)
#' my_env$b
#' my_env$c
#'
#' # You can also unquote a variable referring to a symbol or a string
#' # as binding name:
#' var <- "baz"
#' env_bind(my_env, !!var := "BAZ")
#' my_env$baz
#'
#'
#' # env_bind() and its variants are generic over formulas, quosures
#' # and closures. To illustrate this, let's create a closure function
#' # referring to undefined bindings:
#' fn <- function() list(a, b)
#' fn <- set_env(fn, child_env("base"))
#'
#' # This would fail if run since `a` etc are not defined in the
#' # enclosure of fn() (a child of the base environment):
#' # fn()
#'
#' # Let's define those symbols:
#' env_bind(fn, a = "a", b = "b")
#'
#' # fn() now sees the objects:
#' fn()
env_bind <- function(.env, ...) {
  invisible(env_bind_impl(.env, dots_list(...)))
}
env_bind_impl <- function(env, data) {
  stopifnot(is_vector(data))
  stopifnot(!length(data) || is_named(data))

  nms <- names(data)
  env_ <- get_env(env)

  for (i in seq_along(data)) {
    nm <- nms[[i]]
    base::assign(nm, data[[nm]], envir = env_)
  }

  env
}
#' @rdname env_bind
#' @param .eval_env The environment where the expressions will be
#'   evaluated when the symbols are forced.
#' @export
#' @examples
#'
#' # env_bind_exprs() assigns expressions lazily:
#' env <- env()
#' env_bind_exprs(env, name = cat("forced!\n"))
#' env$name
#' env$name
#'
#' # You can unquote expressions. Note that quosures are not
#' # supported, only raw expressions:
#' expr <- quote(message("forced!"))
#' env_bind_exprs(env, name = !! expr)
#' env$name
env_bind_exprs <- function(.env, ..., .eval_env = caller_env()) {
  exprs <- exprs(...)
  stopifnot(is_named(exprs))

  nms <- names(exprs)
  env_ <- get_env(.env)

  for (i in seq_along(exprs)) {
    do.call("delayedAssign", list(
      x = nms[[i]],
      value = exprs[[i]],
      eval.env = .eval_env,
      assign.env = env_
    ))
  }

  invisible(.env)
}
#' @rdname env_bind
#' @export
#' @examples
#'
#' # You can create active bindings with env_bind_fns()
#' # Let's create some bindings in the lexical enclosure of `fn`:
#' counter <- 0
#'
#' # And now a function that increments the counter and returns a
#' # string with the count:
#' fn <- function() {
#'   counter <<- counter + 1
#'   paste("my counter:", counter)
#' }
#'
#' # Now we create an active binding in a child of the current
#' # environment:
#' env <- env()
#' env_bind_fns(env, symbol = fn)
#'
#' # `fn` is executed each time `symbol` is evaluated or retrieved:
#' env$symbol
#' env$symbol
#' eval_bare(quote(symbol), env)
#' eval_bare(quote(symbol), env)
env_bind_fns <- function(.env, ...) {
  fns <- dots_splice(...)
  stopifnot(is_named(fns) && every(fns, is_function))

  nms <- names(fns)
  env_ <- get_env(.env)

  for (i in seq_along(fns)) {
    makeActiveBinding(nms[[i]], fns[[i]], env_)
  }

  invisible(.env)
}

#' Overscope bindings by defining symbols deeper in a scope
#'
#' `env_bury()` is like [env_bind()] but it creates the bindings in a
#' new child environment. This makes sure the new bindings have
#' precedence over old ones, without altering existing environments.
#' Unlike `env_bind()`, this function does not have side effects and
#' returns a new environment (or object wrapping that environment).
#'
#' @inheritParams env_bind
#' @return A copy of `.env` enclosing the new environment containing
#'   bindings to `...` arguments.
#' @seealso [env_bind()], [env_unbind()]
#' @export
#' @examples
#' orig_env <- env(a = 10)
#' fn <- set_env(function() a, orig_env)
#'
#' # fn() currently sees `a` as the value `10`:
#' fn()
#'
#' # env_bury() will bury the current scope of fn() behind a new
#' # environment:
#' fn <- env_bury(fn, a = 1000)
#' fn()
#'
#' # Even though the symbol `a` is still defined deeper in the scope:
#' orig_env$a
env_bury <- function(.env, ...) {
  env_ <- get_env(.env)
  env_ <- child_env(env_, ...)
  set_env(.env, env_)
}

#' Remove bindings from an environment
#'
#' `env_unbind()` is the complement of [env_bind()]. Like `env_has()`,
#' it ignores the parent environments of `env` by default. Set
#' `inherit` to `TRUE` to track down bindings in parent environments.
#'
#' @inheritParams get_env
#' @param nms A character vector containing the names of the bindings
#'   to remove.
#' @param inherit Whether to look for bindings in the parent
#'   environments.
#' @return The input object `env` with its associated environment
#'   modified in place, invisibly.
#' @export
#' @examples
#' data <- set_names(as_list(letters), letters)
#' env_bind(environment(), !!! data)
#' env_has(environment(), letters)
#'
#' # env_unbind() removes bindings:
#' env_unbind(environment(), letters)
#' env_has(environment(), letters)
#'
#' # With inherit = TRUE, it removes bindings in parent environments
#' # as well:
#' parent <- child_env(NULL, foo = "a")
#' env <- child_env(parent, foo = "b")
#' env_unbind(env, "foo", inherit = TRUE)
#' env_has(env, "foo", inherit = TRUE)
env_unbind <- function(env = caller_env(), nms, inherit = FALSE) {
  env_ <- get_env(env)

  if (inherit) {
    while(any(env_has(env_, nms, inherit = TRUE))) {
      rm(list = nms, envir = env, inherits = TRUE)
    }
  } else {
    rm(list = nms, envir = env)
  }

  invisible(env)
}

#' Does an environment have or see bindings?
#'
#' `env_has()` is a vectorised predicate that queries whether an
#' environment owns bindings personally (with `inherit` set to
#' `FALSE`, the default), or sees them in its own environment or in
#' any of its parents (with `inherit = TRUE`).
#'
#' @inheritParams env_unbind
#' @return A logical vector as long as `nms`.
#' @export
#' @examples
#' parent <- child_env(NULL, foo = "foo")
#' env <- child_env(parent, bar = "bar")
#'
#' # env does not own `foo` but sees it in its parent environment:
#' env_has(env, "foo")
#' env_has(env, "foo", inherit = TRUE)
env_has <- function(env = caller_env(), nms, inherit = FALSE) {
  map_lgl(nms, exists, envir = get_env(env), inherits = inherit)
}

#' Get an object from an environment
#'
#' `env_get()` extracts an object from an enviroment `env`. By
#' default, it does not look in the parent environments.
#'
#' @inheritParams get_env
#' @inheritParams env_has
#' @param nm The name of a binding.
#' @return An object if it exists. Otherwise, throws an error.
#' @export
#' @examples
#' parent <- child_env(NULL, foo = "foo")
#' env <- child_env(parent, bar = "bar")
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_get(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_get(env, "foo", inherit = TRUE)
env_get <- function(env = caller_env(), nm, inherit = FALSE) {
  get(nm, envir = get_env(env), inherits = inherit)
}

#' Names of symbols bound in an environment
#'
#' `env_names()` returns object names from an enviroment `env` as a
#' character vector. All names are returned, even those starting with
#' a dot.
#'
#' @section Names of symbols and objects:
#'
#' Technically, objects are bound to symbols rather than strings,
#' since the R interpreter evaluates symbols (see [is_expr()] for a
#' discussion of symbolic objects versus literal objects). However it
#' is often more convenient to work with strings. In rlang
#' terminology, the string corresponding to a symbol is called the
#' _name_ of the symbol (or by extension the name of an object bound
#' to a symbol).
#'
#' @section Encoding:
#'
#' There are deep encoding issues when you convert a string to symbol
#' and vice versa. Symbols are _always_ in the native encoding (see
#' [set_chr_encoding()]). If that encoding (let's say latin1) cannot
#' support some characters, these characters are serialised to
#' ASCII. That's why you sometimes see strings looking like
#' `<U+1234>`, especially if you're running Windows (as R doesn't
#' support UTF-8 as native encoding on that platform).
#'
#' To alleviate some of the encoding pain, `env_names()` always
#' returns a UTF-8 character vector (which is fine even on Windows)
#' with unicode points unserialised.
#'
#' @inheritParams get_env
#' @return A character vector of object names.
#' @export
#' @examples
#' env <- env(a = 1, b = 2)
#' env_names(env)
env_names <- function(env) {
  nms <- names(get_env(env))
  .Call(rlang_unescape_character, nms)
}

#' Clone an environment
#'
#' This creates a new environment containing exactly the same objects,
#' optionally with a new parent.
#'
#' @inheritParams get_env
#' @param parent The parent of the cloned environment.
#' @export
#' @examples
#' env <- env(!!! mtcars)
#' clone <- env_clone(env)
#' identical(env, clone)
#' identical(env$cyl, clone$cyl)
env_clone <- function(env, parent = env_parent(env)) {
  env <- get_env(env)
  list2env(as.list(env, all.names = TRUE), parent = parent)
}

#' Does environment inherit from another environment?
#'
#' This returns `TRUE` if `x` has `ancestor` among its parents.
#'
#' @inheritParams get_env
#' @param ancestor Another environment from which `x` might inherit.
#' @export
env_inherits <- function(env, ancestor) {
  env <- get_env(env)
  stopifnot(is_env(ancestor) && is_env(env))

  while(!is_empty_env(env_parent(env))) {
    env <- env_parent(env)
    if (is_reference(env, ancestor)) {
      return(TRUE)
    }
  }

  is_empty_env(env)
}


#' Scoped environments
#'
#' @description
#'
#' Scoped environments are named environments which form a
#' parent-child hierarchy called the search path. They define what
#' objects you can see (are in scope) from your workspace. They
#' typically are package environments, i.e. special environments
#' containing all exported functions from a package (and whose parent
#' environment is the package namespace, which also contains
#' unexported functions). Package environments are attached to the
#' search path with [base::library()]. Note however that any
#' environment can be attached to the search path, for example with
#' the unrecommended [base::attach()] base function which transforms
#' vectors to scoped environments.
#'
#' - You can list all scoped environments with `scoped_names()`. Unlike
#'   [base::search()], it also mentions the empty environment that
#'   terminates the search path (it is given the name `"NULL"`).
#'
#' - `scoped_envs()` returns all environments on the search path,
#'   including the empty environment.
#'
#' - `pkg_env()` takes a package name and returns the scoped
#'   environment of packages if they are attached to the search path,
#'   and throws an error otherwise.
#'
#' - `is_scoped()` allows you to check whether a named environment is
#'   on the search path.
#'
#' @section Search path:
#'
#' The search path is a chain of scoped environments where newly
#' attached environments are the childs of earlier ones. However, the
#' global environment, where everything you define at top-level ends
#' up, is pinned as the head of that linked chain. Likewise, the base
#' package environment is pinned as the tail of the chain. You can
#' retrieve those environments with `global_env()` and `base_env()`
#' respectively. The global environment is also the environment of the
#' very first evaluation frame on the stack, see [global_frame()] and
#' [ctxt_stack()].
#'
#' @param nm The name of an environment attached to the search
#'   path. Call [base::search()] to see what is currently on the path.
#' @export
#' @examples
#' # List the names of scoped environments:
#' nms <- scoped_names()
#' nms
#'
#' # The global environment is always the first in the chain:
#' scoped_env(nms[[1]])
#'
#' # And the scoped environment of the base package is always the last:
#' scoped_env(nms[[length(nms)]])
#'
#' # These two environments have their own shortcuts:
#' global_env()
#' base_env()
#'
#' # Packages appear in the search path with a special name. Use
#' # pkg_env_name() to create that name:
#' pkg_env_name("rlang")
#' scoped_env(pkg_env_name("rlang"))
#'
#' # Alternatively, get the scoped environment of a package with
#' # pkg_env():
#' pkg_env("utils")
scoped_env <- function(nm) {
  if (identical(nm, "NULL")) {
    return(empty_env())
  }
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}
#' @rdname scoped_env
#' @param pkg The name of a package.
#' @export
pkg_env <- function(pkg) {
  pkg_name <- pkg_env_name(pkg)
  scoped_env(pkg_name)
}
#' @rdname scoped_env
#' @export
pkg_env_name <- function(pkg) {
  paste0("package:", pkg)
}

#' @rdname scoped_env
#' @export
scoped_names <- function() {
  c(search(), "NULL")
}
#' @rdname scoped_env
#' @export
scoped_envs <- function() {
  envs <- c(.GlobalEnv, env_parents(.GlobalEnv))
  set_names(envs, scoped_names())
}
#' @rdname scoped_env
#' @export
is_scoped <- function(nm) {
  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% scoped_names()
}

#' @rdname scoped_env
#' @export
base_env <- baseenv
#' @rdname scoped_env
#' @export
global_env <- globalenv

#' Get the empty environment
#'
#' The empty environment is the only one that does not have a parent.
#' It is always used as the tail of a scope chain such as the search
#' path (see [scoped_names()]).
#'
#' @export
#' @examples
#' # Create environments with nothing in scope:
#' child_env(empty_env())
empty_env <- emptyenv

#' Get the namespace of a package
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the `imports`
#' environments, which contain all the functions imported from other
#' packages.
#'
#' @param pkg The name of a package. If `NULL`, the surrounding
#'   namespace is returned, or an error is issued if not called within
#'   a namespace. If a function, the enclosure of that function is
#'   checked.
#' @seealso [pkg_env()]
#' @export
ns_env <- function(pkg = NULL) {
  if (is_null(pkg)) {
    bottom <- topenv(caller_env())
    if (!isNamespace(bottom)) abort("not in a namespace")
    bottom
  } else if (is_function(pkg)) {
    env <- env_parent(pkg)
    if (isNamespace(env)) {
      env
    } else {
      NULL
    }
  } else {
    asNamespace(pkg)
  }
}
#' @rdname ns_env
#' @export
ns_imports_env <- function(pkg = NULL) {
  env_parent(ns_env(pkg))
}
#' @rdname ns_env
#' @export
ns_env_name <- function(pkg = NULL) {
  if (is_null(pkg)) {
    pkg <- with_env(caller_env(), ns_env())
  } else if (is_function(pkg)) {
    pkg <- get_env(pkg)
  }
  unname(getNamespaceName(pkg))
}

#' Is a package installed in the library?
#'
#' This checks that a package is installed with minimal side effects.
#' If installed, the package will be loaded but not attached.
#'
#' @param pkg The name of a package.
#' @return `TRUE` if the package is installed, `FALSE` otherwise.
#' @export
#' @examples
#' is_installed("utils")
#' is_installed("ggplot5")
is_installed <- function(pkg) {
  is_true(requireNamespace(pkg, quietly = TRUE))
}


env_type <- function(env) {
  if (is_reference(env, global_env())) {
    "global"
  } else if (is_reference(env, empty_env())) {
    "empty"
  } else if (is_reference(env, base_env())) {
    "base"
  } else if (is_frame_env(env)) {
    "frame"
  } else {
    "local"
  }
}
friendly_env_type <- function(type) {
  switch(type,
    global = "the global environment",
    empty = "the empty environment",
    base = "the base environment",
    frame = "a frame environment",
    local = "a local environment",
    abort("Internal error: unknown environment type")
  )
}

env_format <- function(env) {
  type <- env_type(env)

  if (type %in% c("frame", "local")) {
    addr <- sxp_address(get_env(env))
    type <- paste(type, addr)
  }

  type
}
