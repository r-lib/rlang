get_lang_strings <- function() {
  lang_strings <- c(
    de = "Gl\u00fcck",
    cn = "\u5e78\u798f",
    ru = "\u0441\u0447\u0430\u0441\u0442\u044c\u0435",
    ko = "\ud589\ubcf5"
  )

  native_lang_strings <- enc2native(lang_strings)

  same <- (lang_strings == native_lang_strings)

  list(
    same = lang_strings[same],
    different = lang_strings[!same]
  )
}

get_native_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$same) == 0) testthat::skip("No native language string available")
  lang_strings$same[[1L]]
}

get_alien_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$different) == 0) testthat::skip("No alien language string available")
  lang_strings$different[[1L]]
}

local_utf8_test <- function(frame = caller_env()) {
  reporter <- get_reporter()
  old <- reporter$unicode
  defer(reporter$unicode <- old, envir = frame)
  reporter$unicode <- FALSE
}

with_non_utf8_locale <- function(code) {
  local_utf8_test()
  old_locale <- poke_locale_non_utf8()
  on.exit(poke_ctype_locale(old_locale), add = TRUE)
  code
}

poke_locale_non_utf8 <- function() {
  if (.Platform$OS.type == "windows") {
    return(NULL)
  }

  tryCatch(
    poke_ctype_locale("en_US.ISO8859-1"),
    warning = function(...) {
      testthat::skip("Cannot set latin-1 locale")
    }
  )
}

with_latin1_locale <- function(expr) {
  local_utf8_test()
  old_locale <- suppressMessages(poke_latin1_locale())
  on.exit(poke_ctype_locale(old_locale))
  expr
}


poke_utf8_locale <- function() {
  if (.Platform$OS.type == "windows") {
    warn("UTF-8 is not supported on Windows")
  } else {
    inform("Locale codeset is now UTF-8")
    poke_ctype_locale("en_US.UTF-8")
  }
}
poke_latin1_locale <- function() {
  if (.Platform$OS.type == "windows") {
    locale <- "English_United States.1252"
  } else {
    locale <- "en_US.ISO8859-1"
  }
  inform("Locale codeset is now latin1")
  poke_ctype_locale(locale)
}
poke_mbcs_locale <- function() {
  if (.Platform$OS.type == "windows") {
    locale <- "English_United States.932"
  } else {
    locale <- "ja_JP.SJIS"
  }
  inform("Locale codeset is now of non-UTF-8 MBCS type")
  poke_ctype_locale(locale)
}
poke_ctype_locale <- function(x) {
  if (is_null(x)) {
    return(x)
  }
  # Workaround bug in Sys.setlocale()
  old <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", locale = x)
  invisible(old)
}

encodings <- function(bytes = FALSE) {
  string <- "\u00B0C"

  utf8 <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  out <- list(utf8 = utf8, unknown = unknown, latin1 = latin1)

  if (bytes) {
    out <- list2(!!! out, bytes = encoding_bytes())
  }

  out
}

# On some plaftorms iconv doesn't create unicode markers when encoding
# fails. Instead it transliterates to "?" characters.
skip_if_no_utf8_marker <- function() {
  skip <- tryCatch(
    expr = {
      out <- iconv("幸福", from = "UTF-8", to = "ISO8859-1")
      !is_string(out) || !grepl("<", out)
    },
    error = function(...) {
      TRUE
    }
  )

  if (skip) {
    skip("No UTF-8 marker with this version of libiconv.")
  }
}
