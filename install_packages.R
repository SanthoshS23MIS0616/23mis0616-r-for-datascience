Sys.setenv(R_PORT_PROJECT_ROOT = normalizePath(getwd(), winslash = "/", mustWork = FALSE))
source(file.path(getwd(), "R", "bootstrap.R"))
dir.create(file.path(project_root, "rlib"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(project_root, "rlib"), .libPaths()))
packages <- c("plumber", "nnet", "e1071", "randomForest")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, lib = .libPaths()[1], repos = "https://cloud.r-project.org")
}
still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop(sprintf("Missing required R packages after installation: %s", paste(still_missing, collapse = ", ")))
}
cat("R dependencies ready in", .libPaths()[1], "\n")
