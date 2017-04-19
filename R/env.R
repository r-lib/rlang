#' Get or create an environment.
#'
#' Environments are objects that create a scope for evaluation of R
#' code. Reification of scope is one of the most powerful feature of
#' the R language: it allows you to change what objects a function or
#' expression sees when it is evaluated. In R, scope is hierarchical:
#' each environment is defined with a parent environment. An
#' environment and its grandparents form together a linear
#' hierarchy. All objects within the grandparents are in scope unless
#' they are eclipsed by synonyms (other bindings with the same names)
#' in child environments.
#'
#' `get_env()` dispatches internally. Methods are provided for
#' functions, formulas and frames. If called with a missing argument,
#' the environment of the current evaluation frame (see
#' [ctxt_stack()]) is returned. If you call `get_env()` with an
#' environment, it acts as the identity function and the environment
#' is simply returned (this helps simplifying code when writing
#' generic functions).
#'
#' `env()` and `child_env()` create new environments. While `env()`
#' always creates a child of the current environment, `child_env()`
#' lets you choose a parent environment. `env_parent()` returns the
#' parent environment of `env` if called with `n = 1`, the grandparent
#' with `n = 2`, etc. `env_tail()` searches through the parents and
#' returns the one which has [empty_env()] as parent.
#'
#' @param env An environment or an object bundling an environment:
#'   either a [quosure] or a [closure][is_closure]. If
#'   missing, the environment of the current evaluation frame is
#'   returned.
#' @param parent A parent environment. Can be an object with a S3
#'   method for `as_env()`.
#' @param default The default environment in case `env` does not wrap
#'   an environment. If `NULL` and no environment could be extracted,
#'   an error is issued.
#' @param ...,data Uniquely named bindings. See [is_dictionaryish()].
#'   Dots have [explicit-splicing semantics][dots_list].
#' @param n The number of generations to go through.
#' @seealso `scoped_env`, [env_has()], [env_assign()].
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
#' # env() creates a new environment which has the current environment
#' # as parent. It takes dots with explicit splicing:
#' env(a = 1, b = "foo")
#' env(a = 1, splice(list(b = "foo", c = "bar")))
#'
#' # child_env() creates by default an environment whose parent is the
#' # empty environment. Here we return a new environment that has
#' # the evaluation environment (or frame environment) of a function
#' # as parent:
#' fn <- function() {
#'   my_object <- "A"
#'   child_env(get_env())
#' }
#' frame_env <- fn()
#'
#' # The new environment is empty:
#' env_has(frame_env, "my_object")
#'
#' # But sees objects defined inside fn() by inheriting from its
#' # parent:
#' env_has(frame_env, "my_object", inherit = TRUE)
#'
#'
#' # Create a new environment with a particular scope by setting a
#' # parent. When inheriting from the empty environment (the default),
#' # the environment will have no object in scope at all:
#' env <- child_env()
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # The base package environment is often a good default choice for a
#' # parent environment because it contains all standard base
#' # functions. Also note that it will never inherit from other loaded
#' # package environments since R keeps the base package at the tail
#' # of the search path:
#' env <- child_env(base_env())
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # Note that all other package environments inherit from base_env()
#' # as well:
#' env <- child_env(pkg_env("rlang"))
#' env_has(env, "env_has", inherit = TRUE)
#' env_has(env, "lapply", inherit = TRUE)
#'
#'
#' # The parent argument of child_env() is passed to as_env() to provide
#' # handy shortcuts:
#' env <- child_env("rlang")
#' identical(env_parent(env), pkg_env("rlang"))
#'
#'
#' # Get the parent environment with env_parent():
#' env_parent(global_env())
#'
#' # Or the tail environment with env_tail():
#' env_tail(global_env())
#'
#'
#' # By default, env_parent() returns the parent environment of the
#' # current evaluation frame. If called at top-level (the global
#' # frame), the following two expressions are equivalent:
#' env_parent()
#' env_parent(global_env())
#'
#' # This default is more handy when called within a function. In this
#' # case, the enclosure environment of the function is returned
#' # (since it is the parent of the evaluation frame):
#' enclos_env <- child_env(pkg_env("rlang"))
#' fn <- with_env(enclos_env, function() env_parent())
#' identical(enclos_env, fn())
env <- function(...) {
  env <- new.env(parent = caller_env())
  env_bind(env, dots_list(...))
}
#' @rdname env
#' @export
child_env <- function(parent = NULL, data = list()) {
  env <- new.env(parent = as_env(parent))
  env_bind(env, data)
}

#' @rdname env
#' @export
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

#' @rdname env
#' @export
env_parent <- function(env = caller_env(), n = 1) {
  env_ <- get_env(env)

  while (n > 0) {
    if (identical(env_, empty_env())) {
      return(env_)
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}

#' @rdname env
#' @export
env_tail <- function(env = caller_env()) {
  env_ <- get_env(env)
  next_env <- parent.env(env_)

  while(!identical(next_env, empty_env())) {
    env_ <- next_env
    next_env <- parent.env(next_env)
  }

  env_
}


#' Coerce to an environment.
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
#' @param parent A parent environment, [empty_env()] by default. Can
#'   be ignored with a warning for methods where it does not make
#'   sense to change the parent. Note that existing environments keep
#'   their original parent, only the parents of newly created
#'   environments are set to `parent`.
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
      if (!is_null(parent)) {
        warn("`parent` ignored for empty environment")
      }
      empty_env()
    },
    environment = {
      x
    },
    string = {
      if (length(x) > 1 || is_named(x)) {
        return(as_env_(x, parent))
      }
      if (!is_null(parent)) {
        warn("`parent` ignored for named environments")
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


#' Set an environment.
#'
#' `set_env()` does not work by side effect. The input is copied
#' before being assigned an environment, and left unchanged.
#'
#' @param env An environment or an object with a S3 method for
#'   `set_env()`.
#' @param new_env An environment to replace `env` with. Can be an
#'   object with an S method for `get_env()`.
#' @export
#' @examples
#' # Create a function with a given environment:
#' env <- child_env(base_env())
#' fn <- with_env(env, function() NULL)
#' identical(get_env(fn), env)
#'
#' # set_env() does not work by side effect. Setting a new environment
#' # for fn has no effect on the original function:
#' other_env <- child_env()
#' set_env(fn, other_env)
#' identical(get_env(fn), other_env)
#'
#' # set_env() returns a new function with a different environment, so
#' # you need to assign the returned function to the `fn` name:
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


#' Assign objects to an environment.
#'
#' These functions create bindings in the specified environment. The
#' bindings are supplied as pairs of names and values, either directly
#' (`env_assign()`), in dots (`env_define()`), or from a dictionary
#' (`env_bind()`). See [is_dictionaryish()] for the definition of a
#' dictionary.
#'
#' These functions operate by side effect. For example, if you assign
#' bindings to a closure function, the environment of the function is
#' modified in place.
#'
#' @inheritParams get_env
#' @param nm The name of the binding.
#' @param x The value of the binding.
#' @param ... Pairs of unique names and R objects used to define new
#'   bindings.
#' @return The input object `env`, with its associated environment
#'   modified in place.
#' @export
#' @examples
#' # Create a function that uses undefined bindings:
#' fn <- with_env(child_env("base"), function() list(a, b, c, d, e))
#'
#' # This would throw a scoping error if run:
#' # fn()
#'
#' data <- set_names(letters, letters)
#' env_bind(fn, data)
#'
#' # fn() now sees the objects
#' fn()
#'
#' # Redefine new bindings:
#' fn <- env_assign(fn, "a", "1")
#' fn <- env_define(fn, b = "2", c = "3")
#' fn()
env_assign <- function(env = caller_env(), nm, x) {
  env_ <- get_env(env)
  base::assign(nm, x, envir = env_)
  env
}
#' @rdname env_assign
#' @export
env_bind <- function(env = caller_env(), data = list()) {
  stopifnot(is_dictionaryish(data))
  nms <- names(data)

  env_ <- get_env(env)
  for (i in seq_along(data)) {
    base::assign(nms[[i]], data[[i]], envir = env_)
  }

  env
}
#' @rdname env_assign
#' @export
env_define <- function(env = caller_env(), ...) {
  env_bind(env, list(...))
}

#' Assign a promise to an environment.
#'
#' These functions let you create a promise in an environment. Such
#' promises behave just like lazily evaluated arguments. They are
#' evaluated whenever they are touched by code, but not when they are
#' passed as arguments.
#'
#' @inheritParams env_assign
#' @param expr An expression to capture for
#'   `env_assign_promise()`, or a captured expression (either
#'   quoted or a formula) for the standard evaluation version
#'   `env_assign_promise_()`. This expression is used to create a
#'   promise in `env`.
#' @param eval_env The environment where the promise will be evaluated
#'   when the promise gets forced. If `expr` is a formula, its
#'   environment is used instead. If not a formula and `eval_env` is
#'   not supplied, the promise is evaluated in the environment where
#'   `env_assign_promise()` (or the underscore version) was called.
#' @seealso [env_assign()], [env_assign_active()]
#' @export
#' @examples
#' env <- child_env()
#' env_assign_promise(env, "name", cat("forced!\n"))
#' env$name
#'
#' # Use the standard evaluation version with quoted expressions:
#' f <- ~message("forced!")
#' env_assign_promise_(env, "name2", f)
#' env$name2
env_assign_promise <- function(env = caller_env(), nm, expr,
                               eval_env = caller_env()) {
  f <- as_quosure(substitute(expr), eval_env)
  env_assign_promise_(env, nm, f)
}
#' @rdname env_assign_promise
#' @export
env_assign_promise_ <- function(env = caller_env(), nm, expr,
                                eval_env = caller_env()) {
  args <- list(
    x = nm,
    value = get_expr(expr),
    eval.env = get_env(expr, eval_env),
    assign.env = get_env(env)
  )
  do.call("delayedAssign", args)
}

#' Assign an active binding to an environment.
#'
#' While the expression assigned with [env_assign_promise()] is
#' evaluated only once, the function assigned by `env_assign_active()`
#' is evaluated each time the binding is accessed in `env`.
#'
#' @inheritParams env_assign
#' @param fn A function that will be executed each time the binding
#'   designated by `nm` is accessed in `env`. As all closures, this
#'   function is lexically scoped and can rely on data that are not in
#'   scope for expressions evaluated in `env`. This allows creative
#'   solutions to difficult problems.
#' @seealso [env_assign_promise()]
#' @export
#' @examples
#' # Some bindings for the lexical enclosure of `fn`:
#' data <- "foo"
#' counter <- 0
#'
#' # Create an active binding in a new environment:
#' env <- child_env()
#' env_assign_active(env, "symbol", function() {
#'   counter <<- counter + 1
#'   paste(data, counter)
#' })
#'
#' # `fn` is executed each time `symbol` is accessed from `env`:
#' env$symbol
#' env$symbol
#' eval_bare(quote(symbol), env)
#' eval_bare(quote(symbol), env)
env_assign_active <- function(env = caller_env(), nm, fn) {
  makeActiveBinding(nm, fn, env)
}

#' Bury bindings and define objects in new scope.
#'
#' `env_bury()` is like `env_bind()` but it creates the bindings in a
#' new child environment. Note that this function does not modify its
#' inputs.
#'
#' @inheritParams env_bind
#' @return An object associated with the new environment.
#' @export
#' @examples
#' scope <- child_env(base_env(), list(a = 10))
#' fn <- with_env(scope, function() a)
#'
#' # fn() sees a = 10:
#' fn()
#'
#' # env_bury() will bury the current scope of fn() behind a new
#' # environment:
#' fn <- env_bury(fn, list(a = 1000))
#' fn()
env_bury <- function(env = caller_env(), data = list()) {
  env_ <- get_env(env)
  env_ <- new.env(parent = env_)

  env_bind(env_, data)
  set_env(env, env_)
}

#' Remove bindings from an environment.
#'
#' `env_unbind()` is the complement of [env_bind()]. Like `env_has()`,
#' it ignores the parent environments of `env` by default. Set
#' `inherit` to `TRUE` to track down bindings in parent environments.
#'
#' @inheritParams env_assign
#' @param nms A character vector containing the names of the bindings
#'   to remove.
#' @param inherit Whether to look for bindings in the parent
#'   environments.
#' @return The input object `env`, with its associated
#'   environment modified in place.
#' @export
#' @examples
#' data <- set_names(letters, letters)
#' env_bind(environment(), data)
#' env_has(environment(), letters)
#'
#' # env_unbind() removes bindings:
#' env_unbind(environment(), letters)
#' env_has(environment(), letters)
#'
#' # With inherit = TRUE, it removes bindings in parent environments
#' # as well:
#' parent <- child_env(empty_env(), list(foo = "a"))
#' env <- child_env(parent, list(foo = "b"))
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
  env
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
#' parent <- child_env(empty_env(), list(foo = "foo"))
#' env <- child_env(parent, list(bar = "bar"))
#'
#' # env does not own `foo` but sees it in its parent environment:
#' env_has(env, "foo")
#' env_has(env, "foo", inherit = TRUE)
env_has <- function(env = caller_env(), nms, inherit = FALSE) {
  map_lgl(nms, exists, envir = get_env(env), inherits = inherit)
}

#' Get an object from an environment.
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
#' parent <- child_env(empty_env(), list(foo = "foo"))
#' env <- child_env(parent, list(bar = "bar"))
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_get(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_get(env, "foo", inherit = TRUE)
env_get <- function(env = caller_env(), nm, inherit = FALSE) {
  get(nm, envir = get_env(env), inherits = inherit)
}

#' List object names in an environment.
#'
#' `env_names()` returns object names from an enviroment `env` as a character
#' vector.
#' All names are returned, even those starting with a dot.
#'
#' @inheritParams env
#' @return A character vector of object names.
#' @export
#' @examples
#' env_names()
#' @useDynLib rlang rlang_unescape_character
env_names <- function(env = caller_env()) {
  env_ <- get_env(env)
  x <- names(env_)
  .Call(rlang_unescape_character, x)
}

#' Clone an environment.
#'
#' This creates a new environment containing exactly the same objects,
#' optionally with a new parent.
#'
#' @inheritParams env
#' @param parent The parent of the cloned environment.
#' @export
#' @examples
#' env <- child_env(data = mtcars)
#' clone <- env_clone(env)
#' identical(env$cyl, clone$cyl)
env_clone <- function(env, parent = env_parent(env)) {
  env <- get_env(env)
  list2env(as.list(env, all.names = TRUE), parent = parent)
}

#' Does environment inherit from another environment?
#'
#' This returns `TRUE` if `x` has `ancestor` among its parents.
#'
#' @inheritParams env
#' @param ancestor Another environment from which `x` might inherit.
#' @export
env_inherits <- function(env, ancestor) {
  env <- get_env(env)
  stopifnot(is_env(ancestor) && is_env(env))

  while(!identical(env_parent(env), empty_env())) {
    env <- env_parent(env)
    if (identical(env, ancestor)) {
      return(TRUE)
    }
  }

  identical(env, empty_env())
}


#' Scope environments
#'
#' Scope environments are named environments which form a parent-child
#' hierarchy called the search path. They define what objects you can
#' see (are in scope) from your workspace. They typically are package
#' environments, i.e. special environments containing all exported
#' functions from a package (and whose parent environment is the
#' package namespace, which also contains unexported
#' functions). Package environments are attached to the search path
#' with [base::library()]. Note however that any environment can be
#' attached to the search path, for example with the unrecommended
#' [base::attach()] base function which transforms vectors to scoped
#' environments.
#'
#' Scope environments form a chain with newly attached environments as
#' the childs of earlier ones. However, the global environment, where
#' everything you define at top-level ends up, is pinned as the head
#' of the linked list. Likewise, the base package environment is
#' always the tail of the chain. You can obtain those environments
#' with `global_env()` and `base_env()` respectively. The global
#' environment is always the environment of the very first evaluation
#' frame on the stack, see [global_frame()] and [ctxt_stack()].
#'
#' You can list all scoped environments with `scoped_names()`. With
#' `is_scoped()` you can check whether a named environment is on the
#' search path. `pkg_env()` returns the scope environment of packages
#' if they are attached to the search path, and throws an error
#' otherwise.
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
  search()
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

#' Get the empty environment.
#'
#' The empty environment is the only one that does not have a parent.
#' It is always used as the tail of a scope chain such as the search
#' path (see [scoped_names()]).
#'
#' @export
#' @examples
#' # Create environments with nothing in scope (the default):
#' child_env(parent = empty_env())
empty_env <- emptyenv

#' Get the namespace of a package.
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the `imports`
#' environments, which contain all the functions imported from other
#' packages.
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


#' Evaluate an expression within a given environment.
#'
#' These functions evaluate `expr` within a given environment (`env`
#' for `with_env()`, or the child of the current environment for
#' `locally`). They rely on [eval_bare()] which features a lighter
#' evaluation mechanism than base R [base::eval()], and which also has
#' some subtle implications when evaluting stack sensitive functions
#' (see help for [eval_bare()]).
#'
#' `locally()` is equivalent to the base function
#' [base::local()] but it produces a much cleaner
#' evaluation stack, and has stack-consistent semantics. It is thus
#' more suited for experimenting with the R language.
#'
#' @inheritParams eval_bare
#' @param env An environment within which to evaluate `expr`. Can be
#'   an object with an [get_env()] method.
#' @export
#' @examples
#' # with_env() is handy to create formulas with a given environment:
#' env <- child_env("rlang")
#' f <- with_env(env, ~new_formula())
#' identical(f_env(f), env)
#'
#' # Or functions with a given enclosure:
#' fn <- with_env(env, function() NULL)
#' identical(get_env(fn), env)
#'
#'
#' # Unlike eval() it doesn't create duplicates on the evaluation
#' # stack. You can thus use it e.g. to create non-local returns:
#' fn <- function() {
#'   g(get_env())
#'   "normal return"
#' }
#' g <- function(env) {
#'   with_env(env, return("early return"))
#' }
#' fn()
#'
#'
#' # Since env is passed to as_env(), it can be any object with an
#' # as_env() method. For strings, the pkg_env() is returned:
#' with_env("base", ~mtcars)
#'
#' # This can be handy to put dictionaries in scope:
#' with_env(mtcars, cyl)
with_env <- function(env, expr) {
  .Call(rlang_eval, substitute(expr), as_env(env, caller_env()))
}

#' @rdname with_env
#' @export
locally <- function(expr) {
  .Call(rlang_eval, substitute(expr), child_env(caller_env()))
}


env_type <- function(env) {
  if (identical(env, global_env())) {
    "global"
  } else if (identical(env, empty_env())) {
    "empty"
  } else if (identical(env, base_env())) {
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
