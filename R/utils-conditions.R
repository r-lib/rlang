
abort_coercion <- function(x, to_type) {
  x_type <- friendly_type_of(x)
  if (!inherits(to_type, "AsIs")) {
    to_type <- as_friendly_type(to_type)
  }
  abort(paste0("Can't convert ", x_type, " to ", to_type))
}
