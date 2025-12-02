
engine_load <- function(path = file.path("C++", "build", "libengine_v4.dylib")) {
  path <- normalizePath(path, mustWork = TRUE)
  if (!is.loaded("engine_poll_R")) {
    dyn.load(path)
  }
  invisible(TRUE)
}
