source(file.path(project_root, "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "crop_rules.R"))

build_graph_payload <- function() {
  recommendation_df <- read.csv(CROP_RECOMMENDATION_DATASET_PATH, stringsAsFactors = FALSE)
  master_df <- read.csv(PROJECT_MASTER_DATASET_PATH, stringsAsFactors = FALSE)
  set.seed(RANDOM_STATE)
  scatter_sample <- recommendation_df[sample.int(nrow(recommendation_df), min(800, nrow(recommendation_df))), , drop = FALSE]
  line_df <- aggregate(target_yield_t_ha ~ crop_year, data = master_df, FUN = mean)
  line_df <- line_df[order(line_df$crop_year), , drop = FALSE]
  bar_base <- aggregate(target_yield_t_ha ~ crop, data = master_df, FUN = mean)
  price_base <- aggregate(price_per_ton ~ crop, data = master_df, FUN = mean)
  bar_df <- merge(bar_base, price_base, by = "crop")
  bar_df$estimated_profit_rs_per_ha <- mapply(function(crop_name, yield, price_per_ton) {
    crop_rules <- CROP_DATABASE[[tolower(trimws(crop_name))]]
    base_cost <- if (is.null(crop_rules)) 60000 else as.numeric(crop_rules$base_cost_rs_per_ha) * 0.75
    (yield * (price_per_ton / 1000) * 1000) - base_cost
  }, bar_df$crop, bar_df$target_yield_t_ha, bar_df$price_per_ton)
  bar_df <- head(bar_df[order(bar_df$estimated_profit_rs_per_ha, decreasing = TRUE), , drop = FALSE], 10)
  corr_df <- master_df[, c("nitrogen", "phosphorous", "potassium", "temperature_c", "humidity", "ph", "rainfall_mm", "moisture", "target_yield_t_ha"), drop = FALSE]
  corr_matrix <- stats::cor(corr_df, use = "pairwise.complete.obs")
  box_sample <- master_df[sample.int(nrow(master_df), min(1500, nrow(master_df))), c("crop", "target_yield_t_ha"), drop = FALSE]
  list(
    scatter = list(x = scatter_sample$nitrogen, y = scatter_sample$phosphorous, crop = scatter_sample$crop, moisture = scatter_sample$moisture, title = "Nitrogen vs Phosphorous"),
    line = list(x = line_df$crop_year, y = round(line_df$target_yield_t_ha, 3), title = "Average Yield Trend by Crop Year"),
    bar = list(x = bar_df$crop, y = round(bar_df$estimated_profit_rs_per_ha, 2), title = "Estimated Average Profit by Crop"),
    heatmap = list(z = unname(corr_matrix), x = colnames(corr_matrix), y = rownames(corr_matrix), title = "Correlation Matrix"),
    boxplot = list(crop = box_sample$crop, yield = round(box_sample$target_yield_t_ha, 3), title = "Yield Distribution by Crop")
  )
}
