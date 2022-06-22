#' Evaluate an expression with quosures and pronoun support
#'
#' @description
#' `eval_tidy()` is a variant of [base::eval()] that powers the tidy
#' evaluation framework. Like `eval()` it accepts user data as
#' argument. Whereas `eval()` simply transforms the data to an
#' environment, `eval_tidy()` transforms it to a [data
#' mask][topic-data-mask] with [as_data_mask()]. Evaluating in a data
#' mask enables the following features:
#'
#' - [Quosures][topic-quosure]. Quosures are expressions bundled with
#'   an environment. If `data` is supplied, objects in the data mask
#'   always have precedence over the quosure environment, i.e. the
#'   data masks the environment.
#'
#' - [Pronouns][.data]. If `data` is supplied, the `.env` and `.data`
#'   pronouns are installed in the data mask. `.env` is a reference to
#'   the calling environment and `.data` refers to the `data`
#'   argument. These pronouns are an escape hatch for the [data mask
#'   ambiguity][topic-data-mask-ambiguity] problem.
#'
#' @param expr An [expression][topic-defuse] or
#'   [quosure][topic-quosure] to evaluate.
#' @param data A data frame, or named list or vector. Alternatively, a
#'   data mask created with [as_data_mask()] or
#'   [new_data_mask()]. Objects in `data` have priority over those in
#'   `env`. See the section about data masking.
#' @param env The environment in which to evaluate `expr`. This
#'   environment is not applicable for quosures because they have
#'   their own environments.
#'
#' @section When should eval_tidy() be used instead of eval()?:
#'
#' `base::eval()` is sufficient for simple evaluation. Use
#' `eval_tidy()` when you'd like to support expressions referring to
#' the `.data` pronoun, or when you need to support quosures.
#'
#' If you're evaluating an expression captured with
#' [injection][topic-inject] support, it is recommended to use
#' `eval_tidy()` because users may inject quosures.
#'
#' Note that unwrapping a quosure with [quo_get_expr()] does not
#' guarantee that there is no quosures inside the expression. Quosures
#' might be unquoted anywhere in the expression tree. For instance,
#' the following does not work reliably in the presence of nested
#' quosures:
#'
#' ```
#' my_quoting_fn <- function(x) {
#'   x <- enquo(x)
#'   expr <- quo_get_expr(x)
#'   env <- quo_get_env(x)
#'   eval(expr, env)
#' }
#'
#' # Works:
#' my_quoting_fn(toupper(letters))
#'
#' # Fails because of a nested quosure:
#' my_quoting_fn(toupper(!!quo(letters)))
#' ```
#'
#'
#' @section Stack semantics of `eval_tidy()`:
#'
#' `eval_tidy()` always evaluates in a data mask, even when `data` is
#' `NULL`. Because of this, it has different stack semantics than
#' [base::eval()]:
#'
#' - Lexical side effects, such as assignment with `<-`, occur in the
#'   mask rather than `env`.
#'
#' - Functions that require the evaluation environment to correspond
#'   to a frame on the call stack do not work. This is why `return()`
#'   called from a quosure does not work.
#'
#' - The mask environment creates a new branch in the tree
#'   representation of backtraces (which you can visualise in a
#'   [browser()] session with `lobstr::cst()`).
#'
#' See also [eval_bare()] for more information about these differences.
#'
#'
#' @seealso
#' - `r link("topic_data_mask")`.
#' - `r link("topic_quosure")`.
#' - `r link("topic_defuse")`.
#' - [new_data_mask()] and [as_data_mask()] for manually creating data masks.
#'
#' @examples
#'
#' # With simple defused expressions eval_tidy() works the same way as
#' # eval():
#' fruit <- "apple"
#' vegetable <- "potato"
#' expr <- quote(paste(fruit, vegetable, sep = " or "))
#' expr
#'
#' eval(expr)
#' eval_tidy(expr)
#'
#' # Both accept a data mask as argument:
#' data <- list(fruit = "banana", vegetable = "carrot")
#' eval(expr, data)
#' eval_tidy(expr, data)
#'
#' # The main difference is that eval_tidy() supports quosures:
#' with_data <- function(data, expr) {
#'   quo <- enquo(expr)
#'   eval_tidy(quo, data)
#' }
#' with_data(NULL, fruit)
#' with_data(data, fruit)
#'
#' # eval_tidy() installs the `.data` and `.env` pronouns to allow
#' # users to be explicit about variable references:
#' with_data(data, .data$fruit)
#' with_data(data, .env$fruit)
#' @export
eval_tidy <- function(expr, data = NULL, env = caller_env()) {
  .External2(ffi_eval_tidy, expr, data, env)
}

tilde_eval <- function(...) {
  .External2(
    ffi_tilde_eval,
    sys.call(),     # Quosure env
    environment(),  # Unwind-protect env
    parent.frame()  # Lexical env
  )
}

# Helps work around roxygen loading issues
#' @export
length.rlang_fake_data_pronoun <- function(...) 0L
#' @export
names.rlang_fake_data_pronoun <- function(...) NULL
#' @export
`$.rlang_fake_data_pronoun` <- function(...) NULL
#' @export
`[[.rlang_fake_data_pronoun` <- function(...) NULL
#' @export
print.rlang_fake_data_pronoun <- function(...) cat_line("<pronoun>")

#' `.data` and `.env` pronouns
#'
#' @description
#'
#' The `.data` and `.env` pronouns make it explicit where to find
#' objects when programming with [data-masked][topic-data-mask]
#' functions.
#'
#' ```
#' m <- 10
#' mtcars %>% mutate(disp = .data$disp * .env$m)
#' ```
#'
#' * `.data` retrieves data-variables from the data frame.
#' * `.env` retrieves env-variables from the environment.
#'
#' Because the lookup is explicit, there is no ambiguity between both
#' kinds of variables. Compare:
#'
#' ```
#' disp <- 10
#' mtcars %>% mutate(disp = .data$disp * .env$disp)
#' mtcars %>% mutate(disp = disp * disp)
#' ```
#'
#' Note that `.data` is only a pronoun, it is not a real data
#' frame. This means that you can't take its names or map a function
#' over the contents of `.data`. Similarly, `.env` is not an actual R
#' environment. For instance, it doesn't have a parent and the
#' subsetting operators behave differently.
#'
#'
#' @section `.data` versus the magrittr pronoun `.`:
#'
#' In a [magrittr pipeline](https://magrittr.tidyverse.org/), `.data`
#' is not necessarily interchangeable with the magrittr pronoun `.`.
#' With grouped data frames in particular, `.data` represents the
#' current group slice whereas the pronoun `.` represents the whole
#' data frame. Always prefer using `.data` in data-masked context.
#'
#'
#' @section Where does `.data` live?:
#'
#' The `.data` pronoun is automatically created for you by
#' data-masking functions using the [tidy eval framework][eval_tidy].
#' You don't need to import `rlang::.data` or use `library(rlang)` to
#' work with this pronoun.
#'
#' However, the `.data` object exported from rlang is useful to import
#' in your package namespace to avoid a `R CMD check` note when
#' referring to objects from the data mask. R does not have any way of
#' knowing about the presence or absence of `.data` in a particular
#' scope so you need to import it explicitly or equivalently declare
#' it with `utils::globalVariables(".data")`.
#'
#' Note that `rlang::.data` is a "fake" pronoun. Do not refer to
#' `rlang::.data` with the `rlang::` qualifier in data masking
#' code. Use the unqualified `.data` symbol that is automatically put
#' in scope by data-masking functions.
#'
#' @name dot-data
#' @aliases tidyeval-data
#' @format NULL
#' @usage NULL
#' @export
.data <- structure(list(), class = "rlang_fake_data_pronoun")
#' @rdname dot-data
#' @format NULL
#' @usage NULL
#' @export
.env <- .data


#' Create a data mask
#'
#' @description
#'
#' A [data mask][topic-data-mask] is an environment (or possibly
#' multiple environments forming an ancestry) containing user-supplied
#' objects. Objects in the mask have precedence over objects in the
#' environment (i.e. they mask those objects). Many R functions
#' evaluate quoted expressions in a data mask so these expressions can
#' refer to objects within the user data.
#'
#' These functions let you construct a tidy eval data mask manually.
#' They are meant for developers of tidy eval interfaces rather than
#' for end users.
#'
#'
#' @section Why build a data mask?:
#'
#' Most of the time you can just call [eval_tidy()] with a list or a
#' data frame and the data mask will be constructed automatically.
#' There are three main use cases for manual creation of data masks:
#'
#' * When [eval_tidy()] is called with the same data in a tight loop.
#'   Because there is some overhead to creating tidy eval data masks,
#'   constructing the mask once and reusing it for subsequent
#'   evaluations may improve performance.
#'
#' * When several expressions should be evaluated in the exact same
#'   environment because a quoted expression might create new objects
#'   that can be referred in other quoted expressions evaluated at a
#'   later time. One example of this is `tibble::lst()` where new
#'   columns can refer to previous ones.
#'
#' * When your data mask requires special features. For instance the
#'   data frame columns in dplyr data masks are implemented with
#'   [active bindings][base::delayedAssign].
#'
#'
#' @section Building your own data mask:
#'
#' Unlike [base::eval()] which takes any kind of environments as data
#' mask, [eval_tidy()] has specific requirements in order to support
#' [quosures][nse-defuse]. For this reason you can't supply bare
#' environments.
#'
#' There are two ways of constructing an rlang data mask manually:
#'
#' * `as_data_mask()` transforms a list or data frame to a data mask.
#'   It automatically installs the data pronoun [`.data`][.data].
#'
#' * `new_data_mask()` is a bare bones data mask constructor for
#'   environments. You can supply a bottom and a top environment in
#'   case your data mask comprises multiple environments (see section
#'   below).
#'
#'   Unlike `as_data_mask()` it does not install the `.data` pronoun
#'   so you need to provide one yourself. You can provide a pronoun
#'   constructed with `as_data_pronoun()` or your own pronoun class.
#'
#'   `as_data_pronoun()` will create a pronoun from a list, an
#'   environment, or an rlang data mask. In the latter case, the whole
#'   ancestry is looked up from the bottom to the top of the mask.
#'   Functions stored in the mask are bypassed by the pronoun.
#'
#' Once you have built a data mask, simply pass it to [eval_tidy()] as
#' the `data` argument. You can repeat this as many times as
#' needed. Note that any objects created there (perhaps because of a
#' call to `<-`) will persist in subsequent evaluations.
#'
#'
#' @section Top and bottom of data mask:
#'
#' In some cases you'll need several levels in your data mask. One
#' good reason is when you include functions in the mask. It's a good
#' idea to keep data objects one level lower than function objects, so
#' that the former cannot override the definitions of the latter (see
#' examples).
#'
#' In that case, set up all your environments and keep track of the
#' bottom child and the top parent. You'll need to pass both to
#' `new_data_mask()`.
#'
#' Note that the parent of the top environment is completely
#' undetermined, you shouldn't expect it to remain the same at all
#' times. This parent is replaced during evaluation by [eval_tidy()]
#' to one of the following environments:
#'
#' * The default environment passed as the `env` argument of `eval_tidy()`.
#' * The environment of the current quosure being evaluated, if applicable.
#'
#' Consequently, all masking data should be contained between the
#' bottom and top environment of the data mask.
#'
#' @param data A data frame or named vector of masking data.
#' @return A data mask that you can supply to [eval_tidy()].
#'
#' @export
#' @examples
#' # Evaluating in a tidy evaluation environment enables all tidy
#' # features:
#' mask <- as_data_mask(mtcars)
#' eval_tidy(quo(letters), mask)
#'
#' # You can install new pronouns in the mask:
#' mask$.pronoun <- as_data_pronoun(list(foo = "bar", baz = "bam"))
#' eval_tidy(quo(.pronoun$foo), mask)
#'
#' # In some cases the data mask can leak to the user, for example if
#' # a function or formula is created in the data mask environment:
#' cyl <- "user variable from the context"
#' fn <- eval_tidy(quote(function() cyl), mask)
#' fn()
#'
#' # If new objects are created in the mask, they persist in the
#' # subsequent calls:
#' eval_tidy(quote(new <- cyl + am), mask)
#' eval_tidy(quote(new * 2), mask)
#'
#'
#' # In some cases your data mask is a whole chain of environments
#' # rather than a single environment. You'll have to use
#' # `new_data_mask()` and let it know about the bottom of the mask
#' # (the last child of the environment chain) and the topmost parent.
#'
#' # A common situation where you'll want a multiple-environment mask
#' # is when you include functions in your mask. In that case you'll
#' # put functions in the top environment and data in the bottom. This
#' # will prevent the data from overwriting the functions.
#' top <- new_environment(list(`+` = base::paste, c = base::paste))
#'
#' # Let's add a middle environment just for sport:
#' middle <- env(top)
#'
#' # And finally the bottom environment containing data:
#' bottom <- env(middle, a = "a", b = "b", c = "c")
#'
#' # We can now create a mask by supplying the top and bottom
#' # environments:
#' mask <- new_data_mask(bottom, top = top)
#'
#' # This data mask can be passed to eval_tidy() instead of a list or
#' # data frame:
#' eval_tidy(quote(a + b + c), data = mask)
#'
#' # Note how the function `c()` and the object `c` are looked up
#' # properly because of the multi-level structure:
#' eval_tidy(quote(c(a, b, c)), data = mask)
#'
#' # new_data_mask() does not create data pronouns, but
#' # data pronouns can be added manually:
#' mask$.fns <- as_data_pronoun(top)
#'
#' # The `.data` pronoun should generally be created from the
#' # mask. This will ensure data is looked up throughout the whole
#' # ancestry. Only non-function objects are looked up from this
#' # pronoun:
#' mask$.data <- as_data_pronoun(mask)
#' mask$.data$c
#'
#' # Now we can reference values with the pronouns:
#' eval_tidy(quote(c(.data$a, .data$b, .data$c)), data = mask)
as_data_mask <- function(data) {
  .Call(ffi_as_data_mask, data)
}
#' @rdname as_data_mask
#' @export
as_data_pronoun <- function(data) {
  .Call(ffi_as_data_pronoun, data)
}

#' @rdname as_data_mask
#' @param bottom The environment containing masking objects if the
#'   data mask is one environment deep. The bottom environment if the
#'   data mask comprises multiple environment.
#'
#'   If you haven't supplied `top`, this __must__ be an environment
#'   that you own, i.e. that you have created yourself.
#' @param top The last environment of the data mask. If the data mask
#'   is only one environment deep, `top` should be the same as
#'   `bottom`.
#'
#'   This __must__ be an environment that you own, i.e. that you have
#'   created yourself. The parent of `top` will be changed by the tidy
#'   eval engine and should be considered undetermined. Never make
#'   assumption about the parent of `top`.
#' @export
new_data_mask <- function(bottom, top = bottom) {
  .Call(ffi_new_data_mask, bottom, top)
}

#' @export
`$.rlang_data_pronoun` <- function(x, nm) {
  data_pronoun_get(x, nm, call = I(call("$", quote(.data), sym(nm))))
}
#' @export
`[[.rlang_data_pronoun` <- function(x, i, ...) {
  data_pronoun_get(x, i, call = I(call("[[", quote(.data), substitute(i))))
}
data_pronoun_get <- function(x, nm, call) {
  if (!is_string(nm)) {
    abort(
      sprintf("Must subset the data pronoun with a string, not %s.", obj_type_friendly(nm)),
      call = call
    )
  }
  mask <- .subset2(x, 1)
  .Call(ffi_data_pronoun_get, mask, sym(nm), call)
}
abort_data_pronoun <- function(nm, call) {
  msg <- sprintf("Column `%s` not found in `.data`.", as_string(nm))
  abort(msg, "rlang_error_data_pronoun_not_found", call = call)
}

#' @export
`$.rlang_ctxt_pronoun` <- function(x, nm) {
  ctxt_pronoun_get(x, nm)
}
#' @export
`[[.rlang_ctxt_pronoun` <- function(x, i, ...) {
  ctxt_pronoun_get(x, i)
}
ctxt_pronoun_get <- function(x, nm, call) {
  if (!is_string(nm)) {
    abort(
      sprintf("Must subset the context pronoun with a string, not %s.", obj_type_friendly(nm)),
      call = call
    )
  }
  eval_bare(sym(nm), x)
}

#' @export
`$<-.rlang_data_pronoun` <- function(x, i, value) {
  abort(
    "Can't modify the data pronoun.",
    call = I(call("<-", call("$", quote(.data), substitute(i)), sym("...")))
  )
}
#' @export
`[[<-.rlang_data_pronoun` <- function(x, i, value) {
  abort(
    "Can't modify the data pronoun.",
    call = I(call("<-", call("[[", quote(.data), substitute(i)), sym("...")))
  )
}
#' @export
`$<-.rlang_ctxt_pronoun` <- function(x, i, value) {
  abort(
    "Can't modify the context pronoun.",
    call = I(call("<-", call("$", quote(.env), substitute(i)), sym("...")))
  )
}
#' @export
`[[<-.rlang_ctxt_pronoun` <- function(x, i, value) {
  abort(
    "Can't modify the context pronoun.",
    call = I(call("<-", call("[[", quote(.env), substitute(i)), sym("...")))
  )
}

#' @export
`[.rlang_data_pronoun` <- function(x, i, ...) {
  abort(
    "`[` is not supported by the `.data` pronoun, use `[[` or $ instead.",
    call = I(call2("[", quote(.data), !!!enexprs(i, ...)))
  )
}
#' @export
`[.rlang_ctxt_pronoun` <- function(x, i, ...) {
  abort(
    "`[` is not supported by the `.env` pronoun, use `[[` or $ instead.",
    call = I(call2("[", quote(.env), !!!enexprs(i, ...)))
  )
}

#' @export
names.rlang_data_pronoun <- function(x) {
  chr()
}
#' @export
dimnames.rlang_data_pronoun <- function(x) {
  list(chr(), chr())
}
#' @export
length.rlang_data_pronoun <- function(x) {
  0L
}

#' @export
names.rlang_ctxt_pronoun <- function(x) {
  chr()
}
#' @export
length.rlang_ctxt_pronoun <- function(x) {
  0L
}

#' @export
print.rlang_data_pronoun <- function(x, ...) {
  cat_line("<pronoun>")
  invisible(x)
}
#' @importFrom utils str
#' @export
str.rlang_data_pronoun <- function(object, ...) {
  cat_line("<pronoun>")
}

# Used for deparsing
is_data_pronoun <- function(x) {
  is_call(x, c("[[", "$"), n = 2L) && identical(node_cadr(x), dot_data_sym)
}
data_pronoun_name <- function(x) {
  if (is_call(x, "$")) {
    arg <- node_cadr(node_cdr(x))
    if (is_symbol(arg)) {
      return(as_string(arg))
    } else {
      return(NULL)
    }
  }

  if (is_call(x, "[[")) {
    arg <- node_cadr(node_cdr(x))
    if (is_string(arg)) {
      return(arg)
    } else {
      return(NULL)
    }
  }
}

#' @export
`$.rlang_fake_data_pronoun` <- function(x, nm, call = caller_env()) {
  stop_fake_data_subset(call)
}
#' @export
`[[.rlang_fake_data_pronoun` <- function(x, i, ..., call = caller_env()) {
  stop_fake_data_subset(call)
}
stop_fake_data_subset <- function(call) {
  abort(
    sprintf("Can't subset %s outside of a data mask context.", format_arg(".data")),
    call = mask_top(call, inherit = TRUE)
  )
}

is_data_mask <- function(x) {
  is_environment(x) && env_has(x, ".__rlang_data_mask__.")
}

mask_top <- function(env, inherit = FALSE) {
  top <- quo_mask_top(env, inherit = inherit)
  if (!identical(top, env)) {
    top
  } else {
    data_mask_top(env, inherit = inherit)
  }
}
data_mask_top <- function(env, recursive = FALSE, inherit = FALSE) {
  while (env_has(env, ".__tidyeval_data_mask__.", inherit = inherit)) {
    env <- env_parent(env_get(env, ".top_env", inherit = inherit))
    if (!recursive) {
      return(env)
    }
  }

  env
}
quo_mask_top <- function(env, recursive = FALSE, inherit = FALSE) {
  while (env_has(env, ".__tidyeval_quosure_mask__.", inherit = inherit)) {
    env <- env_parent(env)
    if (!recursive) {
      return(env)
    }
  }

  env
}
