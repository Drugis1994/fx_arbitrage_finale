library(testthat)

project_root <- normalizePath(file.path(testthat::test_path("..", "..", "..")), mustWork = TRUE)
proj_file <- function(...) file.path(project_root, ...)
