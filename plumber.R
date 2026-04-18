source(file.path(getwd(), "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "graphs.R"))
source(file.path(project_root, "R", "inference.R"))

runtime_bundle <- load_bundle()
runtime_context <- build_runtime_context(runtime_bundle)

#* @get /
function(res) {
  res$body <- paste(readLines(file.path(STATIC_DIR, "index_r.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  res
}

#* @get /api/health
function() list(status = "ok", ready = file.exists(MODEL_BUNDLE_PATH))

#* @get /api/metadata
function() {
  c(runtime_context$metadata$dataset_summary, list(crop_count = runtime_context$metadata$dataset_summary$crop_count, training_report = runtime_context$metadata$training_report, xai_assets = runtime_context$metadata$xai_assets))
}

#* @get /api/graphs
function() build_graph_payload()

#* @post /api/soil-estimate
function(req, res) {
  payload <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  tryCatch(estimate_soil_profile(runtime_context, payload), error = function(err) { res$status <- 500; list(detail = conditionMessage(err)) })
}

#* @post /api/predict
function(req, res) {
  payload <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  top_k <- payload$top_k %||% TOP_K_DEFAULT
  payload$top_k <- NULL
  tryCatch(predict_crop(runtime_context, payload, top_k = as.integer(top_k)), error = function(err) { res$status <- 500; list(detail = conditionMessage(err)) })
}

#* @post /api/train
function() {
  source(file.path(project_root, "R", "training.R"))
  report <- train_and_save()
  runtime_bundle <<- load_bundle()
  runtime_context <<- build_runtime_context(runtime_bundle)
  report
}
