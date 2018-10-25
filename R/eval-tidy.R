#' Evaluate an expression with quosures and pronoun support
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' `eval_tidy()` is a variant of [base::eval()] that powers the tidy
#' evaluation framework. Like `eval()` it accepts user data as
#' argument. Whereas `eval()` simply transforms the data to an
#' environment, `eval_tidy()` transforms it to a **data mask** with
#' [as_data_mask()]. Evaluating in a data mask enables the following
#' features:
#'
#' - [Quosures][quotation]. Quosures are expressions bundled with an
#'   environment. If `data` is supplied, objects in the data mask
#'   always have precedence over the quosure environment, i.e. the
#'   data masks the environment.
#'
#' - [Pronouns][.data]. If `data` is supplied, the `.env` and `.data`
#'   pronouns are installed in the data mask. `.env` is a reference to
#'   the calling environment and `.data` refers to the `data` argument.
#'   These pronouns lets you be explicit about where to find
#'   values and throw errors if you try to access non-existent values.
#'
#'
#' @param expr An expression or quosure to evaluate.
#' @param data A data frame, or named list or vector. Alternatively, a
#'   data mask created with [as_data_mask()] or
#'   [new_data_mask()]. Objects in `data` have priority over those in
#'   `env`. See the section about data masking.
#'
#' @param env The environment in which to evaluate `expr`. This
#'   environment is not applicable for quosures because they have
#'   their own environments.
#' @seealso [quasiquotation] for the second leg of the tidy evaluation
#'   framework.
#'
#'
#' @section Data masking:
#'
#' Data masking refers to how columns or objects inside `data` have
#' priority over objects defined in `env` (or in the quosure
#' environment, if applicable). If there is a column `var` in `data`
#' and an object `var` in `env`, and `expr` refers to `var`, the
#' column has priority:
#'
#' ```
#' var <- "this one?"
#' data <- data.frame(var = rep("Or that one?", 3))
#'
#' within <- function(data, expr) {
#'   eval_tidy(enquo(expr), data)
#' }
#'
#' within(data, toupper(var))
#' #> [1] "OR THAT ONE?" "OR THAT ONE?" "OR THAT ONE?"
#' ```
#'
#' Because the columns or objects in `data` are always found first,
#' before objects from `env`, we say that the data "masks" the
#' environment.
#'
#'
#' @section When should eval_tidy() be used instead of eval()?:
#'
#' `base::eval()` is sufficient for simple evaluation. Use
#' `eval_tidy()` when you'd like to support expressions referring to
#' the `.data` pronoun, or when you need to support quosures.
#'
#' If you're evaluating an expression captured with quasiquotation
#' support, it is recommended to use `eval_tidy()` because users will
#' likely unquote quosures.
#'
#' Note that unwrapping a quosure with [quo_get_expr()] does not
#' guarantee that there is no quosures inside the expression. Quosures
#' might be unquoted anywhere. For instance, the following does not
#' work reliably in the presence of nested quosures:
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
#' @section Life cycle:
#'
#' **rlang 0.3.0**
#'
#' Passing an environment to `data` is deprecated. Please construct an
#' rlang data mask with [new_data_mask()].
#'
#'
#' @examples
#'
#' # With simple quoted expressions eval_tidy() works the same way as
#' # eval():
#' apple <- "apple"
#' kiwi <- "kiwi"
#' expr <- quote(paste(apple, kiwi))
#' expr
#'
#' eval(expr)
#' eval_tidy(expr)
#'
#' # Both accept a data mask as argument:
#' data <- list(apple = "CARROT", kiwi = "TOMATO")
#' eval(expr, data)
#' eval_tidy(expr, data)
#'
#'
#' # In addition eval_tidy() has support for quosures:
#' with_data <- function(data, expr) {
#'   quo <- enquo(expr)
#'   eval_tidy(quo, data)
#' }
#' with_data(NULL, apple)
#' with_data(data, apple)
#' with_data(data, list(apple, kiwi))
#'
#' # Secondly eval_tidy() installs handy pronouns that allow users to
#' # be explicit about where to find symbols:
#' with_data(data, .data$apple)
#' with_data(data, .env$apple)
#'
#'
#' # Note that instead of using `.env` it is often equivalent and may
#' # be preferred to unquote a value. There are two differences. First
#' # unquoting happens earlier, when the quosure is created. Secondly,
#' # subsetting `.env` with the `$` operator may be brittle because
#' # `$` does not look through the parents of the environment.
#' #
#' # For instance using `.env$name` in a magrittr pipeline is an
#' # instance where this poses problem, because the magrittr pipe
#' # currently (as of v1.5.0) evaluates its operands in a *child* of
#' # the current environment (this child environment is where it
#' # defines the pronoun `.`).
#' \dontrun{
#'   data %>% with_data(!!kiwi)     # "kiwi"
#'   data %>% with_data(.env$kiwi)  # NULL
#' }
#' @export
eval_tidy <- function(expr, data = NULL, env = caller_env()) {
  .Call(rlang_eval_tidy, expr, data, env)
}

# Helps work around roxygen loading issues
#' @export
length.rlang_fake_data_pronoun <- function(...) NULL
#' @export
names.rlang_fake_data_pronoun <- function(...) NULL
#' @export
`$.rlang_fake_data_pronoun` <- function(...) NULL
#' @export
`[[.rlang_fake_data_pronoun` <- function(...) NULL
#' @export
print.rlang_fake_data_pronoun <- function(...) cat_line("<pronoun>")

#' Data pronoun for tidy evaluation
#'
#' @description
#'
#' This pronoun allows you to be explicit when you refer to an object
#' inside the data. Referring to the `.data` pronoun rather than to
#' the original data frame has several advantages:
#'
#' * It makes it easy to refer to column names stored as strings. If
#'   `var` contains the column `"height"`, the pronoun will subset that
#'   column:
#'
#'     ```
#'     var <- "height"
#'     dplyr::summarise(df, mean(.data[[var]]))
#'     ```
#'
#'   The index variable `var` is [unquoted][quasiquotation], which
#'   ensures a column named `var` in the data frame cannot mask it.
#'   This makes the pronoun safe to use in functions and packages.
#'
#' * Sometimes a computation is not about the whole data but about a
#'   subset. For example if you supply a grouped data frame to a dplyr
#'   verb, the `.data` pronoun contains the group subset.
#'
#' * It lets dplyr know that you're referring to a column from the
#'   data which is helpful to generate correct queries when the source
#'   is a database.
#'
#' The `.data` object exported here is useful to import in your
#' package namespace to avoid a `R CMD check` note when referring to
#' objects from the data mask.
#'
#' @name tidyeval-data
#' @format NULL
#' @export
.data <- structure(list(), class = "rlang_fake_data_pronoun")


#' Create a data mask
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' A data mask is an environment (or possibly multiple environments
#' forming an ancestry) containing user-supplied objects. Objects in
#' the mask have precedence over objects in the environment (i.e. they
#' mask those objects). Many R functions evaluate quoted expressions
#' in a data mask so these expressions can refer to objects within the
#' user data.
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
#' [quosures][quotation]. For this reason you can't supply bare
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
#' @section Life cycle:
#'
#' **rlang 0.3.0**
#'
#' Passing environments to `as_data_mask()` is soft-deprecated. Please
#' build a data mask with `new_data_mask()`.
#'
#' The `parent` argument no longer has any effect. The parent of the
#' data mask is determined from either:
#'
#'   * The `env` argument of `eval_tidy()`
#'   * Quosure environments when applicable
#'
#' **rlang 0.2.0**
#'
#' In early versions of rlang data masks were called overscopes. We
#' think data mask is a more natural name in R. It makes reference to
#' masking in the search path which occurs through the same mechanism
#' (in technical terms, lexical scoping with hierarchically nested
#' environments). We say that objects from user data mask objects
#' in the current environment.
#'
#' Following this change in terminology, `as_overscope()` and
#' `new_overscope()` were soft-deprecated in rlang 0.2.0 in favour of
#' `as_data_mask()` and `new_data_mask()`.
#'
#' @param data A data frame or named vector of masking data.
#' @param parent Soft-deprecated. This argument no longer has any effect.
#'   The parent of the data mask is determined from either:
#'   * The `env` argument of `eval_tidy()`
#'   * Quosure environments when applicable
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
#' # Now we can reference the values with the pronouns:
#' eval_tidy(quote(c(.data$a, .data$b, .data$c)), data = mask)
as_data_mask <- function(data, parent = NULL) {
  if (!is_null(parent)) {
    warn_deprecated(paste_line(
      "The `parent` argument of `as_data_mask()` is deprecated.",
      "The parent of the data mask is determined from either:",
      "",
      "  * The `env` argument of `eval_tidy()`",
      "  * Quosure environments when applicable"
    ))
  }
  .Call(rlang_as_data_mask, data)
}
#' @rdname as_data_mask
#' @export
as_data_pronoun <- function(data) {
  .Call(rlang_as_data_pronoun, data)
}

#' @rdname as_data_mask
#' @param bottom The environment containing masking objects if the
#'   data mask is one environment deep. The bottom environment if the
#'   data mask comprises multiple environment.
#' @param top The last environment of the data mask. If the data mask
#'   is only one environment deep, `top` should be the same as
#'   `bottom`.
#' @export
new_data_mask <- function(bottom, top = bottom, parent = NULL) {
  if (!is_null(parent)) {
    warn_deprecated(paste_line(
      "The `parent` argument of `new_data_mask()` is deprecated.",
      "The parent of the data mask is determined from either:",
      "",
      "  * The `env` argument of `eval_tidy()`",
      "  * Quosure environments when applicable"
    ))
  }
  .Call(rlang_new_data_mask, bottom, top)
}

#' @export
`$.rlang_data_pronoun` <- function(x, nm) {
  data_pronoun_get(x, nm)
}
#' @export
`[[.rlang_data_pronoun` <- function(x, i, ...) {
  if (!is_string(i)) {
    abort("Must subset the data pronoun with a string")
  }
  data_pronoun_get(x, i)
}
data_pronoun_get <- function(x, nm) {
  mask <- .subset2(x, 1)
  .Call(rlang_data_pronoun_get, mask, sym(nm))
}
abort_data_pronoun <- function(nm) {
  msg <- sprintf("Column `%s` not found in `.data`", as_string(nm))
  abort(msg, "rlang_data_pronoun_not_found")
}

#' @export
`$<-.rlang_data_pronoun` <- function(x, i, value) {
  abort("Can't modify the data pronoun")
}
#' @export
`[[<-.rlang_data_pronoun` <- function(x, i, value) {
  abort("Can't modify the data pronoun")
}

#' @export
`[.rlang_data_pronoun` <- function(x, i, ...) {
  abort("`[` is not supported by .data pronoun, use `[[` or $ instead")
}
#' @export
names.rlang_data_pronoun <- function(x) {
  warn_deprecated("Taking the `names()` of the `.data` pronoun is deprecated")
  env <- .subset2(x, 1)
  if (is_data_mask(env)) {
    env <- env_parent(env)
  }
  env_names(env)
}
#' @export
length.rlang_data_pronoun <- function(x) {
  warn_deprecated("Taking the `length()` of the `.data` pronoun is deprecated")
  env <- .subset2(x, 1)
  if (is_data_mask(env)) {
    env <- env_parent(env)
  }
  env_length(env)
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

is_data_mask <- function(x) {
  is_environment(x) && env_has(x, ".__rlang_data_mask__.")
}
