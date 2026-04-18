Sys.setenv(R_PORT_PROJECT_ROOT = normalizePath(getwd(), winslash = "/", mustWork = FALSE))
source(file.path(getwd(), "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))

if (!file.exists(MODEL_BUNDLE_PATH)) {
  source(file.path(project_root, "R", "training.R"))
  message("Training R models because no bundle was found yet...")
  train_and_save()
}

port_value <- suppressWarnings(as.integer(Sys.getenv("PORT", unset = "8000")))
if (is.na(port_value) || port_value <= 0) {
  port_value <- 8000L
}
message(sprintf("Starting plumber on port %s", port_value))

router <- plumber::plumb(file.path(project_root, "plumber.R"))
router <- plumber::pr_static(router, "/static", STATIC_DIR)
router <- plumber::pr_static(router, "/reports", REPORT_DIR)
plumber::pr_run(router, host = "0.0.0.0", port = port_value, quiet = FALSE)