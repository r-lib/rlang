#' Evaluate an expression with quosures and pronoun support
#'
#' @description
#'
#' `eval_tidy()` is a variant of [base::eval()] that powers the tidy
#' evaluation framework. Like `eval()` it accepts user data as
#' argument. If supplied, it evaluates its input `expr` in a [data
#' mask][as_data_mask]. In additon `eval_tidy()` supports:
#'
#' - [Quosures][quotation]. The expression wrapped in the quosure
#'   evaluates in its original context (masked by `data` if supplied).
#'
#' - [Pronouns][.data]. If `data` is supplied, the `.env` and `.data`
#'   pronouns are installed in the data mask. `.env` is a reference to
#'   the calling environment and `.data` refers to the `data` argument.
#'   These pronouns lets you be explicit about where to find
#'   values and throw errors if you try to access non-existent values.
#'
#'
#' @section Life cycle:
#'
#' `eval_tidy()` is stable.
#'
#' @param expr An expression to evaluate.
#' @param data A data frame, or named list or vector. Alternatively, a
#'   data mask created with [as_data_mask()] or [new_data_mask()].
#' @param env The environment in which to evaluate `expr`. This
#'   environment is always ignored when evaluating quosures. Quosures
#'   are evaluated in their own environment.
#' @seealso [quasiquotation] for the second leg of the tidy evaluation
#'   framework.
#' @export
#' @examples
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
#' # Secondly eval_tidy() installs handy pronouns that allows users to
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
#' @name eval_tidy
eval_tidy <- function(expr, data = NULL, env = caller_env()) {
  .Call(rlang_eval_tidy, expr, data, environment())
}

#' Data pronoun for tidy evaluation
#'
#' @description
#'
#' This pronoun allows you to be explicit when you refer to an object
#' inside the data. Referring to the `.data` pronoun rather than to
#' the original data frame has several advantages:
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
#' @export
.data <- NULL
delayedAssign(".data", as_data_pronoun(list()))


#' Create a data mask
#'
#' @description
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
#' for end users. Most of the time you can just call [eval_tidy()]
#' with user data and the data mask will be constructed automatically.
#' There are three main use cases for manual creation of data masks:
#'
#' * When [eval_tidy()] is called with the same data in a tight loop.
#'   Tidy eval data masks are a bit expensive to build so it is best
#'   to construct it once and reuse it the other times for optimal
#'   performance.
#'
#' * When several expressions should be evaluated in the same
#'   environment because a quoted expression might create new objects
#'   that can be referred in other quoted expressions evaluated at a
#'   later time.
#'
#' * When your data mask requires special features. For instance the
#'   data frame columns in dplyr data masks are implemented with
#'   [active bindings][base::delayedAssign].
#'
#'
#' @section Building your own data mask:
#'
#' Creating a data mask for [base::eval()] is a simple matter of
#' creating an environment containing masking objects that has the
#' user context as parent. `eval()` automates this task when you
#' supply data as second argument. However a tidy eval data mask also
#' needs to enable support of [quosures][quotation] and [data
#' pronouns][tidyeval-data]. These functions allow manual construction
#' of tidy eval data masks:
#'
#' * `as_data_mask()` transforms a data frame, named vector or
#'   environment to a data mask. If an environment, its ancestry is
#'   ignored. It automatically installs a data pronoun.
#'
#' * `new_data_mask()` is a bare bones data mask constructor for
#'   environments. You can supply a bottom and a top environment in
#'   case your data mask comprises multiple environments.
#'
#'   Unlike `as_data_mask()` it does not install the `.data` pronoun
#'   so you need to provide one yourself. You can provide a pronoun
#'   constructed with `as_data_pronoun()` or your own pronoun class.
#'
#' - `as_data_pronoun()` constructs a tidy eval data pronoun that
#'   gives more useful error messages than regular data frames or
#'   lists, i.e. when an object does not exist or if an user tries to
#'   overwrite an object.
#'
#' To use a a data mask, just supply it to [eval_tidy()] as `data`
#' argument. You can repeat this as many times as needed. Note that
#' any objects created there (perhaps because of a call to `<-`) will
#' persist in subsequent evaluations:
#'
#'
#' @section Life cycle:
#'
#' All these functions are now stable.
#'
#' In early versions of rlang data masks were called overscopes. We
#' think data mask is a more natural name in R. It makes reference to
#' masking in the search path which occurs through the same mechanism
#' (in technical terms, lexical scoping with hierarchically nested
#' environments). We say that that objects from user data mask objects
#' in the current environment.
#'
#' Following this change in terminology, `as_data_mask()` and
#' `new_overscope()` were soft-deprecated in rlang 0.2.0 in favour of
#' `as_data_mask()` and `new_data_mask()`.
#'
#' @param data A data frame or named vector of masking data.
#' @param parent The parent environment of the data mask.
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
as_data_mask <- function(data, parent = base_env()) {
  .Call(rlang_as_data_mask, data, parent)
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
new_data_mask <- function(bottom, top = bottom, parent = base_env()) {
  .Call(rlang_new_data_mask, bottom, top, parent)
}


#' @export
`$.rlang_data_pronoun` <- function(x, name) {
  src <- .subset2(x, "src")
  if (!has_binding(src, name)) {
    abort(sprintf(.subset2(x, "lookup_msg"), name), "rlang_data_pronoun_not_found")
  }
  src[[name]]
}
#' @export
`[[.rlang_data_pronoun` <- function(x, i, ...) {
  if (!is_string(i)) {
    abort("Must subset the data pronoun with a string")
  }
  src <- .subset2(x, "src")
  if (!has_binding(src, i)) {
    abort(sprintf(.subset2(x, "lookup_msg"), i), "rlang_data_pronoun_not_found")
  }
  src[[i, ...]]
}

#' @export
`$<-.rlang_data_pronoun` <- function(x, i, value) {
  dict <- unclass_data_pronoun(x)

  if (dict$read_only) {
    abort("Can't modify the data pronoun")
  }

  dict$src[[i]] <- value
  set_attrs(dict, class = class(x))
}
#' @export
`[[<-.rlang_data_pronoun` <- function(x, i, value) {
  dict <- unclass_data_pronoun(x)

  if (dict$read_only) {
    abort("Can't modify the data pronoun")
  }
  if (!is_string(i)) {
    abort("Must subset the data pronoun with a string")
  }

  dict$src[[i]] <- value
  set_attrs(dict, class = class(x))
}

#' @export
names.rlang_data_pronoun <- function(x) {
  names(unclass(x)$src)
}
#' @export
length.rlang_data_pronoun <- function(x) {
  length(unclass(x)$src)
}

has_binding <- function(x, name) {
  if (is_environment(x)) {
    env_has(x, name)
  } else {
    has_name(x, name)
  }
}

#' @export
print.rlang_data_pronoun <- function(x, ...) {
  src <- unclass_data_pronoun(x)$src
  objs <- glue_countable(length(src), "object")
  cat(paste0("<pronoun>\n", objs, "\n"))
  invisible(x)
}
#' @importFrom utils str
#' @export
str.rlang_data_pronoun <- function(object, ...) {
  str(unclass_data_pronoun(object)$src, ...)
}

glue_countable <- function(n, str) {
  if (n == 1) {
    paste0(n, " ", str)
  } else {
    paste0(n, " ", str, "s")
  }
}
# Unclassing before print() or str() is necessary because default
# methods index objects with integers
unclass_data_pronoun <- function(x) {
  i <- match("rlang_data_pronoun", class(x))
  class(x) <- class(x)[-i]
  x
}
