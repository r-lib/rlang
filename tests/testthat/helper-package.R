local_package <- function(
  path = NULL,
  name = "templatepackage",
  env = caller_env()
) {
  if (is.null(path)) {
    path <- withr::local_tempdir(.local_envir = env)
    desc <- desc::desc("!new")
    desc$set("Package", name)
    desc$set("Title", "A test template package")
    desc$write(file = file.path(path, "DESCRIPTION"))
  }

  pkgload::load_all(path, quiet = TRUE)
  defer(pkgload::unload(name), envir = env)

  path
}
