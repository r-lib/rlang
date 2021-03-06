% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn.R
\name{as_closure}
\alias{as_closure}
\title{Transform to a closure}
\usage{
as_closure(x, env = caller_env())
}
\arguments{
\item{x}{A function or formula.

If a \strong{function}, it is used as is.

If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a function
with up to two arguments: \code{.x} (single argument) or \code{.x} and \code{.y}
(two arguments). The \code{.} placeholder can be used instead of \code{.x}.
This allows you to create very compact anonymous functions (lambdas) with up
to two inputs. Functions created from formulas have a special
class. Use \code{is_lambda()} to test for it.

If a \strong{string}, the function is looked up in \code{env}. Note that
this interface is strictly for user convenience because of the
scoping issues involved. Package developers should avoid
supplying functions by name and instead supply them by value.}

\item{env}{Environment in which to fetch the function in case \code{x}
is a string.}
}
\description{
\code{as_closure()} is like \code{\link[=as_function]{as_function()}} but also wraps primitive
functions inside closures. Some special control flow primitives
like \code{if}, \code{for}, or \code{break} can't be wrapped and will cause an
error.
}
\examples{
# Primitive functions are regularised as closures
as_closure(list)
as_closure("list")

# Operators have `.x` and `.y` as arguments, just like lambda
# functions created with the formula syntax:
as_closure(`+`)
as_closure(`~`)

}
\keyword{internal}
