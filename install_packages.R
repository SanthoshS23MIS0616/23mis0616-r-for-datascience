Sys.setenv(R_PORT_PROJECT_ROOT = normalizePath(getwd(), winslash = "/", mustWork = FALSE))
source(file.path(getwd(), "R", "bootstrap.R"))
dir.create(file.path(project_root, "rlib"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(project_root, "rlib"), .libPaths()))
packages <- c("plumber", "randomForest", "ggplot2", "plotly")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, lib = .libPaths()[1], repos = "https://cloud.r-project.org")
}
cat("R dependencies ready in", .libPaths()[1], "\n")
