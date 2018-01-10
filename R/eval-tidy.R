#' Evaluate an expression tidily
#'
#' @description
#'
#' `eval_tidy()` is a variant of [base::eval()] and [eval_bare()] that
#' powers the [tidy evaluation
#' framework](http://rlang.tidyverse.org/articles/tidy-evaluation.html).
#' It evaluates `expr` in an [overscope][as_data_mask] where the
#' special definitions enabling tidy evaluation are installed. This
#' enables the following features:
#'
#' - Overscoped data. You can supply a data frame or list of named
#'   vectors to the `data` argument. The data contained in this list
#'   has precedence over the objects in the contextual environment.
#'   This is similar to how [base::eval()] accepts a list instead of
#'   an environment.
#'
#' - Self-evaluation of quosures. Within the overscope, quosures act
#'   like promises. When a quosure within an expression is evaluated,
#'   it automatically invokes the quoted expression in the captured
#'   environment (chained to the overscope). Note that quosures do not
#'   always get evaluated because of lazy semantics, e.g. `TRUE ||
#'   ~never_called`.
#'
#' - Pronouns. `eval_tidy()` installs the `.env` and `.data`
#'   pronouns. `.env` contains a reference to the calling environment,
#'   while `.data` refers to the `data` argument. These pronouns lets
#'   you be explicit about where to find values and throw errors if
#'   you try to access non-existent values.
#'
#' @param expr An expression.
#' @param data A list (or data frame). This is passed to the
#'   [as_dictionary()] coercer, a generic used to transform an object
#'   to a proper data source. If you want to make `eval_tidy()` work
#'   for your own objects, you can define a method for this generic.
#' @param env The lexical environment in which to evaluate `expr`.
#' @seealso [quo()], [quasiquotation]
#' @export
#' @examples
#' # Like base::eval() and eval_bare(), eval_tidy() evaluates quoted
#' # expressions:
#' expr <- expr(1 + 2 + 3)
#' eval_tidy(expr)
#'
#' # Like base::eval(), it lets you supply overscoping data:
#' foo <- 1
#' bar <- 2
#' expr <- quote(list(foo, bar))
#' eval_tidy(expr, list(foo = 100))
#'
#' # The main difference is that quosures self-evaluate within
#' # eval_tidy():
#' quo <- quo(1 + 2 + 3)
#' eval(quo)
#' eval_tidy(quo)
#'
#' # Quosures also self-evaluate deep in an expression not just when
#' # directly supplied to eval_tidy():
#' expr <- expr(list(list(list(!! quo))))
#' eval(expr)
#' eval_tidy(expr)
#'
#' # Self-evaluation of quosures is powerful because they
#' # automatically capture their enclosing environment:
#' foo <- function(x) {
#'   y <- 10
#'   quo(x + y)
#' }
#' f <- foo(1)
#'
#' # This quosure refers to `x` and `y` from `foo()`'s evaluation
#' # frame. That's evaluated consistently by eval_tidy():
#' f
#' eval_tidy(f)
#'
#'
#' # Finally, eval_tidy() installs handy pronouns that allows users to
#' # be explicit about where to find symbols. If you supply data,
#' # eval_tidy() will look there first:
#' cyl <- 10
#' eval_tidy(quo(cyl), mtcars)
#'
#' # To avoid ambiguity and be explicit, you can use the `.env` and
#' # `.data` pronouns:
#' eval_tidy(quo(.data$cyl), mtcars)
#' eval_tidy(quo(.env$cyl), mtcars)
#'
#' # Note that instead of using `.env` it is often equivalent and may be
#' # preferred to unquote a value. There are two differences. First unquoting
#' # happens earlier, when the quosure is created. Secondly, subsetting `.env`
#' # with the `$` operator may be brittle because `$` does not look through
#' # the parents of the environment. Using `.env$name` in a magrittr pipeline
#' # is an instance where this poses problem, because the magrittr pipe
#' # currently (as of v1.5.0) evaluates its operands in a *child* of the
#' # current environment (this child environment is where it defines the
#' # pronoun `.`).
#'
#' eval_tidy(quo(!! cyl), mtcars)  # 10
#' \dontrun{
#'   mtcars %>% eval_tidy(quo(!! cyl), .)  # 10
#'   mtcars %>% eval_tidy(quo(.env$cyl), .)  # NULL
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
#' @section Constructing a data mask:
#'
#' Creating a data mask for [base::eval()] is a simple matter of
#' creating an environment containing masking objects that has the
#' user context as parent. `eval()` automates this task when you
#' supply data as second argument. However a tidy eval data mask also
#' needs to enable support of [quosures][quosure] and [data
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
#'
#' @section Evaluating in a data mask:
#'
#' To use a a data mask:
#'
#' * Supply it to [eval_tidy()] as `data` argument. You can repeat
#'   this as many times as needed.
#'
#' * When you are done evaluating use `data_mask_clean()` to empty the
#'   data mask. [eval_tidy()] automatically calls it only if it
#'   created the data mask. If you created the mask it is your
#'   responsibility to clean it.
#'
#'
#' @section Life cycle:
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
#' # This is why when you are done evaluating expressions it may be
#' # necessary to clean up the data in the mask:
#' data_mask_clean(mask)
#' fn()
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
#' @rdname as_data_mask
#' @param mask A data mask as created by `as_data_mask()` or
#'   `new_data_mask()`.
#' @export
data_mask_clean <- function(mask) {
  invisible(.Call(rlang_data_mask_clean, mask))
}


#' @export
`$.rlang_data_pronoun` <- function(x, name) {
  src <- .subset2(x, "src")
  if (!has_binding(src, name)) {
    abort(sprintf(.subset2(x, "lookup_msg"), name))
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
    abort(sprintf(.subset2(x, "lookup_msg"), i))
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
