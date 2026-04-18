project_root <- normalizePath(
  Sys.getenv("R_PORT_PROJECT_ROOT", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

local_library <- file.path(project_root, "rlib")
if (dir.exists(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}
