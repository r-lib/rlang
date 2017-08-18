
future_rlang_ver <- function() {
  version <- pkg_ver("rlang")

  version <- ver_trim(version, 3)
  version <- ver_bump(version, "minor")
  version[[3]] <- 0

  version
}
past_rlang_ver <- function() {
  version <- pkg_ver("rlang")

  version <- ver_trim(version, 3)
  if (version[[3]] == 0) {
    version <- unbump(version, "minor")
  } else {
    version[[3]] <- 0
  }

  version
}
