source(file.path(project_root, "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "crop_rules.R"))
source(file.path(project_root, "R", "utils.R"))

load_bundle <- function() {
  if (!file.exists(MODEL_BUNDLE_PATH)) {
    source(file.path(project_root, "R", "training.R"))
    train_and_save()
  }
  readRDS(MODEL_BUNDLE_PATH)
}

build_runtime_context <- function(bundle) {
  profiles_df <- read.csv(CROP_PROFILES_DATASET_PATH, stringsAsFactors = FALSE)
  master_df <- read.csv(PROJECT_MASTER_DATASET_PATH, stringsAsFactors = FALSE)
  market_df <- read.csv(MARKET_PRICES_DATASET_PATH, stringsAsFactors = FALSE)
  crop_profiles <- lapply(split(profiles_df, profiles_df$crop), function(frame) {
    row <- if (any(frame$profile_variant == "typical")) frame[frame$profile_variant == "typical", , drop = FALSE][1, ] else frame[1, ]
    crop_name <- as.character(row$crop)
    crop_rules <- CROP_DATABASE[[tolower(trimws(crop_name))]]
    crop_market <- market_df$price_per_ton[market_df$crop == crop_name]
    crop_yield <- master_df$target_yield_t_ha[master_df$crop == crop_name]
    list(
      min_nitrogen = as.numeric(row$min_nitrogen), max_nitrogen = as.numeric(row$max_nitrogen),
      min_phosphorous = as.numeric(row$min_phosphorous), max_phosphorous = as.numeric(row$max_phosphorous),
      min_potassium = as.numeric(row$min_potassium), max_potassium = as.numeric(row$max_potassium),
      min_temp_c = as.numeric(row$min_temp_c), max_temp_c = as.numeric(row$max_temp_c),
      min_humidity = as.numeric(row$min_humidity), max_humidity = as.numeric(row$max_humidity),
      min_ph = as.numeric(row$min_ph), max_ph = as.numeric(row$max_ph),
      min_rainfall_mm = as.numeric(row$min_rainfall_mm), max_rainfall_mm = as.numeric(row$max_rainfall_mm),
      historical_median_yield = if (length(crop_yield)) median(crop_yield, na.rm = TRUE) else NA_real_,
      duration_months = crop_rules$duration_months %||% 6L,
      sowing_months = crop_rules$sowing_months %||% c(6L, 7L, 8L),
      peak_harvest_months = crop_rules$peak_harvest_months %||% c(10L, 11L),
      median_price_per_ton = if (length(crop_market)) median(crop_market, na.rm = TRUE) else bundle$defaults$price_per_ton
    )
  })
  list(
    bundle = bundle,
    crop_profiles = crop_profiles,
    market_prices = market_df,
    master_df = master_df,
    metadata = list(
      dataset_summary = list(recommendation_rows = nrow(master_df), production_rows = nrow(master_df), crop_count = length(unique(master_df$crop)), default_inputs = bundle$defaults),
      training_report = bundle$training_report,
      xai_assets = list(
        classification_importance_plot = "/reports/classification_feature_importance.png",
        regression_importance_plot = "/reports/regression_feature_importance.png",
        top_features = bundle$training_report$xai_summary$top_features
      )
    )
  )
}

fill_defaults <- function(runtime, payload) {
  prepared <- runtime$bundle$defaults
  for (name in names(payload)) {
    if (!is.null(payload[[name]]) && !(length(payload[[name]]) == 1 && is.na(payload[[name]]))) prepared[[name]] <- payload[[name]]
  }
  prepared$season <- derive_season(as.integer(format(Sys.Date(), "%m")))
  prepared$crop_year <- as.integer(format(Sys.Date(), "%Y"))
  prepared
}

build_regression_frame <- function(prepared, crop_name) {
  data.frame(
    area_ha = as.numeric(prepared$area), nitrogen = as.numeric(prepared$nitrogen), phosphorous = as.numeric(prepared$phosphorous),
    potassium = as.numeric(prepared$potassium), temperature_c = as.numeric(prepared$temperature_c), humidity = as.numeric(prepared$humidity),
    ph = as.numeric(prepared$ph), rainfall_mm = as.numeric(prepared$rainfall_mm), moisture = as.numeric(prepared$moisture),
    crop = crop_name, state_name = as.character(prepared$state_name), district_name = as.character(prepared$district_name), season = as.character(prepared$season),
    stringsAsFactors = FALSE
  )
}

predict_class_probabilities <- function(runtime, class_frame) {
  bundle <- runtime$bundle
  x_class <- transform_features(class_frame, bundle$class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  multinom_frame <- prepare_feature_frame(class_frame, bundle$class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  classes <- bundle$class_levels
  probs_multinom <- align_probability_matrix(nnet:::predict.multinom(bundle$classification_models$multinom, newdata = multinom_frame, type = "probs"), classes)
  probs_tree <- align_probability_matrix(e1071:::predict.naiveBayes(bundle$classification_models$decision_tree, newdata = x_class, type = "raw"), classes)
  probs_rf <- align_probability_matrix(randomForest:::predict.randomForest(bundle$classification_models$random_forest, newdata = x_class, type = "prob"), classes)
  weights <- bundle$classification_models$ensemble_weights
  probs_ensemble <- (probs_multinom * weights[["multinom"]]) + (probs_tree * weights[["decision_tree"]]) + (probs_rf * weights[["random_forest"]])
  list(multinom = probs_multinom[1, ], decision_tree = probs_tree[1, ], random_forest = probs_rf[1, ], ensemble = probs_ensemble[1, ])
}

baseline_yield <- function(crop_name, predicted_yield, profile, crop_rules) {
  vals <- c(predicted_yield, crop_rules$yield_avg_t_ha)
  if (!is.na(profile$historical_median_yield)) vals <- c(vals, profile$historical_median_yield)
  max(median(vals, na.rm = TRUE), 0.15)
}

market_price_rs_per_kg <- function(runtime, crop_name, state_name, season) {
  crop_rules <- CROP_DATABASE[[tolower(trimws(crop_name))]]
  market_df <- runtime$market_prices
  exact <- market_df[market_df$crop == crop_name & market_df$state_name == state_name & trimws(market_df$season) == season, , drop = FALSE]
  observed <- if (nrow(exact)) median(exact$price_per_ton, na.rm = TRUE) / 1000 else median(market_df$price_per_ton[market_df$crop == crop_name], na.rm = TRUE) / 1000
  clip_value((0.55 * crop_rules$base_price_rs_per_kg) + (0.45 * observed), crop_rules$price_range_rs_per_kg[[1]], crop_rules$price_range_rs_per_kg[[2]])
}

climate_adjustments <- function(prepared, profile, crop_rules) {
  penalties <- c(); reasons <- c()
  rainfall <- as.numeric(prepared$rainfall_mm); rain_min <- as.numeric(profile$min_rainfall_mm); rain_max <- as.numeric(profile$max_rainfall_mm)
  if (rainfall < rain_min) { deficit <- (rain_min - rainfall) / max(rain_min, 1); penalties <- c(penalties, ifelse(deficit <= 0.30, 0.10, 0.22)); reasons <- c(reasons, "rainfall deficit"); if (deficit > 0.55) return(list(penalty = 1, reject = TRUE, reasons = c(reasons, "severe rainfall mismatch"))) }
  if (rainfall > rain_max) { excess <- (rainfall - rain_max) / max(rain_max, 1); penalties <- c(penalties, ifelse(excess <= 0.30, 0.08, 0.16)); reasons <- c(reasons, "rainfall excess"); if (excess > 0.70 && identical(crop_rules$water_need, "low")) return(list(penalty = 1, reject = TRUE, reasons = c(reasons, "severe rainfall excess"))) }
  temperature <- as.numeric(prepared$temperature_c); temp_min <- as.numeric(profile$min_temp_c); temp_max <- as.numeric(profile$max_temp_c); temp_span <- max(temp_max - temp_min, 1)
  if (temperature < temp_min) { mismatch <- (temp_min - temperature) / temp_span; penalties <- c(penalties, ifelse(mismatch <= 0.30, 0.08, 0.18)); reasons <- c(reasons, "temperature below range"); if (mismatch > 0.55) return(list(penalty = 1, reject = TRUE, reasons = c(reasons, "severe temperature mismatch"))) }
  if (temperature > temp_max) { mismatch <- (temperature - temp_max) / temp_span; penalties <- c(penalties, ifelse(mismatch <= 0.30, 0.08, 0.18)); reasons <- c(reasons, "temperature above range"); if (mismatch > 0.55) return(list(penalty = 1, reject = TRUE, reasons = c(reasons, "severe temperature mismatch"))) }
  if (as.numeric(prepared$humidity) < as.numeric(profile$min_humidity) || as.numeric(prepared$humidity) > as.numeric(profile$max_humidity)) { penalties <- c(penalties, 0.03); reasons <- c(reasons, "humidity mismatch") }
  if (as.numeric(prepared$ph) < as.numeric(profile$min_ph) || as.numeric(prepared$ph) > as.numeric(profile$max_ph)) { penalties <- c(penalties, 0.08); reasons <- c(reasons, "soil pH mismatch") }
  list(penalty = min(sum(penalties), 0.70), reject = FALSE, reasons = unique(reasons))
}

cost_model <- function(crop_rules, area_ha) {
  base_cost <- as.numeric(crop_rules$base_cost_rs_per_ha) * 0.75
  fixed_cost <- base_cost * 0.22; fertilizers <- base_cost * 0.18; pesticides <- base_cost * as.numeric(crop_rules$chemical_dependency) * 0.18
  irrigation <- base_cost * water_need_factor(as.character(crop_rules$water_need)); labour <- base_cost * 0.20; machinery <- base_cost * 0.08; post_harvest <- base_cost * 0.09
  subtotal <- fixed_cost + fertilizers + pesticides + irrigation + labour + machinery + post_harvest
  buffer <- subtotal * 0.12
  list(total_cost_rs_per_ha = subtotal + buffer, total_cost = (subtotal + buffer) * area_ha)
}

price_adjustment <- function(runtime, crop_name, current_month, crop_rules, prepared) {
  base_price <- market_price_rs_per_kg(runtime, crop_name, as.character(prepared$state_name), as.character(prepared$season))
  harvest_month_number <- harvest_month(current_month, crop_rules$duration_months)
  if (harvest_month_number %in% crop_rules$peak_harvest_months) {
    factor <- ifelse(as.numeric(crop_rules$price_volatility) >= 0.25, 0.82, 0.88); reason <- "peak harvest supply"
  } else if (as.numeric(crop_rules$price_volatility) >= 0.26) {
    factor <- 1.12; reason <- "off-season support"
  } else {
    factor <- 1.03; reason <- "stable market"
  }
  list(price = clip_value(base_price * factor, crop_rules$price_range_rs_per_kg[[1]], crop_rules$price_range_rs_per_kg[[2]]), reason = reason)
}

stable_price_rs_per_kg <- function(runtime, crop_name, prepared, crop_rules) {
  base_price <- market_price_rs_per_kg(runtime, crop_name, as.character(prepared$state_name), as.character(prepared$season))
  clip_value((0.70 * base_price) + (0.30 * crop_rules$base_price_rs_per_kg), crop_rules$price_range_rs_per_kg[[1]], crop_rules$price_range_rs_per_kg[[2]])
}

make_rule_explanations <- function(prepared, profile, crop_name) {
  checks <- list(c("nitrogen", "min_nitrogen", "max_nitrogen", "Nitrogen"), c("phosphorous", "min_phosphorous", "max_phosphorous", "Phosphorous"), c("potassium", "min_potassium", "max_potassium", "Potassium"), c("temperature_c", "min_temp_c", "max_temp_c", "Temperature"), c("humidity", "min_humidity", "max_humidity", "Humidity"), c("ph", "min_ph", "max_ph", "Soil pH"), c("rainfall_mm", "min_rainfall_mm", "max_rainfall_mm", "Rainfall"))
  positives <- c(); concerns <- c()
  for (check in checks) {
    value <- as.numeric(prepared[[check[[1]]]])
    if (value >= as.numeric(profile[[check[[2]]]]) && value <= as.numeric(profile[[check[[3]]]])) positives <- c(positives, sprintf("%s is within the suitable range for %s.", check[[4]], crop_name)) else concerns <- c(concerns, sprintf("%s is outside the preferred range for %s.", check[[4]], crop_name))
  }
  list(positives = head(positives, 4), concerns = head(concerns, 4))
}

estimate_soil_profile <- function(runtime, payload) {
  prepared <- fill_defaults(runtime, payload)
  master_df <- runtime$master_df
  soil_columns <- c("nitrogen", "phosphorous", "potassium", "ph", "moisture")
  climate_columns <- c("temperature_c", "humidity", "rainfall_mm")
  usable <- master_df[stats::complete.cases(master_df[, c(soil_columns, climate_columns)]), c(soil_columns, climate_columns, "state_name", "district_name"), drop = FALSE]
  if (!nrow(usable)) {
    defaults <- runtime$bundle$defaults
    return(list(
      nitrogen = as.numeric(defaults$nitrogen),
      phosphorous = as.numeric(defaults$phosphorous),
      potassium = as.numeric(defaults$potassium),
      ph = as.numeric(defaults$ph),
      moisture = as.numeric(defaults$moisture),
      matched_rows = 0L,
      confidence_score = 0.35,
      source = "fallback defaults"
    ))
  }

  if (!is.null(prepared$state_name) && nzchar(as.character(prepared$state_name))) {
    state_rows <- usable[trimws(usable$state_name) == trimws(as.character(prepared$state_name)), , drop = FALSE]
    if (nrow(state_rows) >= 50) usable <- state_rows
  }
  if (!is.null(prepared$district_name) && nzchar(as.character(prepared$district_name))) {
    district_rows <- usable[trimws(usable$district_name) == trimws(as.character(prepared$district_name)), , drop = FALSE]
    if (nrow(district_rows) >= 20) usable <- district_rows
  }

  target_values <- c(
    temperature_c = as.numeric(prepared$temperature_c),
    humidity = as.numeric(prepared$humidity),
    rainfall_mm = as.numeric(prepared$rainfall_mm)
  )
  climate_sds <- vapply(climate_columns, function(column) {
    sd_value <- stats::sd(as.numeric(usable[[column]]), na.rm = TRUE)
    ifelse(is.na(sd_value) || sd_value == 0, 1, sd_value)
  }, numeric(1))
  distance <- rowSums(vapply(climate_columns, function(column) {
    ((as.numeric(usable[[column]]) - target_values[[column]]) / climate_sds[[column]]) ^ 2
  }, numeric(nrow(usable))))
  neighbor_count <- min(150L, nrow(usable))
  nearest_index <- order(distance)[seq_len(neighbor_count)]
  nearest_rows <- usable[nearest_index, , drop = FALSE]
  weights <- 1 / (sqrt(distance[nearest_index]) + 0.05)
  weights <- weights / sum(weights)

  weighted_value <- function(column) {
    sum(as.numeric(nearest_rows[[column]]) * weights, na.rm = TRUE)
  }

  mean_distance <- mean(sqrt(distance[nearest_index]), na.rm = TRUE)
  confidence_score <- clip_value(1 / (1 + mean_distance), 0.25, 0.95)
  list(
    nitrogen = weighted_value("nitrogen"),
    phosphorous = weighted_value("phosphorous"),
    potassium = weighted_value("potassium"),
    ph = weighted_value("ph"),
    moisture = weighted_value("moisture"),
    matched_rows = nrow(nearest_rows),
    confidence_score = confidence_score,
    source = "nearest climate match from current project dataset"
  )
}

make_local_driver_lists <- function(prepared, profile, top_features) {
  numeric_map <- list(
    nitrogen = c("nitrogen", "min_nitrogen", "max_nitrogen", "Nitrogen"),
    phosphorous = c("phosphorous", "min_phosphorous", "max_phosphorous", "Phosphorous"),
    potassium = c("potassium", "min_potassium", "max_potassium", "Potassium"),
    temperaturec = c("temperature_c", "min_temp_c", "max_temp_c", "Temperature"),
    humidity = c("humidity", "min_humidity", "max_humidity", "Humidity"),
    ph = c("ph", "min_ph", "max_ph", "Soil pH"),
    rainfallmm = c("rainfall_mm", "min_rainfall_mm", "max_rainfall_mm", "Rainfall")
  )

  make_entry <- function(feature_info, sign = 1) {
    list(feature = feature_info$label, contribution = round(sign * feature_info$magnitude, 3))
  }

  scored_features <- list()
  for (item in top_features) {
    raw_feature <- tolower(gsub("[^a-z]", "", as.character(item$feature)))
    if (!(raw_feature %in% names(numeric_map))) next
    mapping <- numeric_map[[raw_feature]]
    value <- as.numeric(prepared[[mapping[[1]]]])
    lower <- as.numeric(profile[[mapping[[2]]]])
    upper <- as.numeric(profile[[mapping[[3]]]])
    span <- max(upper - lower, 1)
    center <- (lower + upper) / 2
    if (value >= lower && value <= upper) {
      closeness <- 1 - min(abs(value - center) / max(span / 2, 1e-6), 1)
      magnitude <- max(0.12, as.numeric(item$mean_importance) * (0.45 + (0.55 * closeness)))
      scored_features[[length(scored_features) + 1L]] <- list(label = mapping[[4]], magnitude = magnitude, positive = TRUE)
    } else {
      gap <- if (value < lower) lower - value else value - upper
      magnitude <- max(0.12, as.numeric(item$mean_importance) * (0.45 + (0.55 * min(gap / span, 1))))
      scored_features[[length(scored_features) + 1L]] <- list(label = mapping[[4]], magnitude = magnitude, positive = FALSE)
    }
  }

  positive <- lapply(head(Filter(function(item) isTRUE(item$positive), scored_features), 4), make_entry, sign = 1)
  negative <- lapply(head(Filter(function(item) !isTRUE(item$positive), scored_features), 4), make_entry, sign = -1)
  if (!length(positive) && !length(scored_features)) {
    positive <- lapply(head(top_features, 4), function(item) list(feature = pretty_feature_name(item$feature), contribution = round(as.numeric(item$mean_importance), 3)))
  }

  list(
    classification = list(positive = positive, negative = negative),
    yield = list(positive = positive, negative = negative)
  )
}

predict_crop <- function(runtime, payload, top_k = TOP_K_DEFAULT) {
  context <- current_analysis_context()
  current_month <- as.integer(context$current_month_number); current_season <- as.character(context$season)
  prepared <- fill_defaults(runtime, payload)
  class_frame <- data.frame(nitrogen = as.numeric(prepared$nitrogen), phosphorous = as.numeric(prepared$phosphorous), potassium = as.numeric(prepared$potassium), temperature_c = as.numeric(prepared$temperature_c), humidity = as.numeric(prepared$humidity), ph = as.numeric(prepared$ph), rainfall_mm = as.numeric(prepared$rainfall_mm), moisture = as.numeric(prepared$moisture), state_name = as.character(prepared$state_name), season = as.character(prepared$season), stringsAsFactors = FALSE)
  probs <- predict_class_probabilities(runtime, class_frame)
  labels <- names(probs$ensemble)
  ranked_indices <- order(probs$ensemble, decreasing = TRUE)[seq_len(min(RANKING_POOL_SIZE, length(probs$ensemble)))]
  area_ha <- as.numeric(prepared$area)
  candidates <- list(); rejected <- list()
  for (idx in ranked_indices) {
    crop_name <- labels[[idx]]; crop_rules <- CROP_DATABASE[[tolower(trimws(crop_name))]]; profile <- runtime$crop_profiles[[crop_name]]
    if (is.null(crop_rules) || is.null(profile)) next
    if (!(current_month %in% crop_rules$sowing_months)) { rejected[[length(rejected) + 1L]] <- list(crop = crop_name, reason = sprintf("outside sowing window for %s", month_name(current_month))); next }
    reg_frame <- build_regression_frame(prepared, crop_name)
    x_reg <- transform_features(reg_frame, runtime$bundle$regression_preprocessor, REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES)
    ml_yield <- as.numeric(stats::predict(runtime$bundle$regression_model, newdata = x_reg))
    base_yield_t_ha <- baseline_yield(crop_name, ml_yield, profile, crop_rules)
    climate <- climate_adjustments(prepared, profile, crop_rules)
    if (isTRUE(climate$reject)) { rejected[[length(rejected) + 1L]] <- list(crop = crop_name, reason = paste(climate$reasons, collapse = ", ")); next }
    total_yield_penalty <- min(as.numeric(crop_rules$new_farmer_penalty) + climate$penalty + late_sowing_penalty(current_month, crop_rules$sowing_months), 0.58)
    expected_yield_t_ha <- max(base_yield_t_ha * (1 - total_yield_penalty), 0.15)
    adjusted_price <- price_adjustment(runtime, crop_name, current_month, crop_rules, prepared)
    costs <- cost_model(crop_rules, area_ha)
    revenue_rs_per_ha <- expected_yield_t_ha * adjusted_price$price * 1000
    profit_rs_per_ha <- revenue_rs_per_ha - costs$total_cost_rs_per_ha
    risk <- clip_value((as.numeric(crop_rules$base_risk) * 0.30) + (water_need_factor(as.character(crop_rules$water_need)) * 0.25) + (as.numeric(crop_rules$price_volatility) * 0.20) + (as.numeric(crop_rules$new_farmer_penalty) * 0.15) + (climate$penalty * 0.10), 0.12, 0.88)
    sustainability <- clip_value(as.numeric(crop_rules$base_sustainability) - (if (identical(crop_rules$water_need, "high")) 0.12 else if (identical(crop_rules$water_need, "medium")) 0.04 else 0) - (as.numeric(crop_rules$chemical_dependency) * 0.18) - (as.numeric(crop_rules$soil_sensitivity) * 0.10), 0.18, 0.92)
    candidates[[length(candidates) + 1L]] <- list(crop = crop_name, classification_probability = as.numeric(probs$ensemble[[idx]]), sowing_month = month_name(current_month), harvest_month = month_name(harvest_month(current_month, crop_rules$duration_months)), duration_months = crop_rules$duration_months, yield_start_month = month_name(harvest_month(current_month, crop_rules$duration_months)), expected_yield_t_ha = expected_yield_t_ha, adjusted_price_rs_per_kg = adjusted_price$price, total_cost_rs_per_ha = costs$total_cost_rs_per_ha, revenue_rs_per_ha = revenue_rs_per_ha, profit_rs_per_ha = profit_rs_per_ha, profit = profit_rs_per_ha * area_ha, initial_spend_rs_per_ha = costs$total_cost_rs_per_ha, risk = risk, sustainability_score = sustainability)
  }
  if (!length(candidates)) stop("No crop is currently suitable for sowing at this date and field condition.")
  profit_scores <- candidate_scores(vapply(candidates, `[[`, numeric(1), "profit_rs_per_ha"))
  low_cost_scores <- candidate_scores(-vapply(candidates, `[[`, numeric(1), "initial_spend_rs_per_ha"))
  for (i in seq_along(candidates)) {
    candidates[[i]]$final_score <- (low_cost_scores[[i]] * 0.35) + (profit_scores[[i]] * 0.35) + ((1 - candidates[[i]]$risk) * 0.20) + (candidates[[i]]$sustainability_score * 0.10)
    candidates[[i]]$high_investment_profit_score <- (profit_scores[[i]] * 0.60) + ((1 - low_cost_scores[[i]]) * 0.25) + ((1 - candidates[[i]]$risk) * 0.10) + (candidates[[i]]$sustainability_score * 0.05)
  }
  standard_candidates <- candidates[order(vapply(candidates, `[[`, numeric(1), "final_score"), decreasing = TRUE)]
  preferred_slots <- max(min(top_k, 5L) - 1L, 1L)
  spend_values <- vapply(candidates, `[[`, numeric(1), "initial_spend_rs_per_ha")
  premium_threshold <- if (length(spend_values) > 1) as.numeric(stats::quantile(spend_values, 0.65)) else spend_values[[1]]
  low_medium_pool <- Filter(function(item) item$initial_spend_rs_per_ha <= premium_threshold, standard_candidates)
  top_candidates <- head(low_medium_pool, preferred_slots)
  selected_crops <- vapply(top_candidates, `[[`, "", "crop")
  premium_pool <- Filter(function(item) item$initial_spend_rs_per_ha > premium_threshold && !(item$crop %in% selected_crops), candidates)
  if (length(premium_pool)) top_candidates[[length(top_candidates) + 1L]] <- premium_pool[[which.max(vapply(premium_pool, `[[`, numeric(1), "high_investment_profit_score"))]]
  while (length(top_candidates) < top_k) {
    remaining <- Filter(function(item) !(item$crop %in% vapply(top_candidates, `[[`, "", "crop")), standard_candidates)
    if (!length(remaining)) break
    top_candidates[[length(top_candidates) + 1L]] <- remaining[[1]]
  }
  ideal_candidates <- lapply(candidates, function(item) {
    crop_name <- item$crop; crop_rules <- CROP_DATABASE[[tolower(trimws(crop_name))]]; profile <- runtime$crop_profiles[[crop_name]]
    stable_price <- stable_price_rs_per_kg(runtime, crop_name, prepared, crop_rules)
    fit_components <- c(
      ifelse(prepared$nitrogen >= profile$min_nitrogen && prepared$nitrogen <= profile$max_nitrogen, 1, 0.5),
      ifelse(prepared$phosphorous >= profile$min_phosphorous && prepared$phosphorous <= profile$max_phosphorous, 1, 0.5),
      ifelse(prepared$potassium >= profile$min_potassium && prepared$potassium <= profile$max_potassium, 1, 0.5),
      ifelse(prepared$temperature_c >= profile$min_temp_c && prepared$temperature_c <= profile$max_temp_c, 1, 0.5),
      ifelse(prepared$humidity >= profile$min_humidity && prepared$humidity <= profile$max_humidity, 1, 0.5),
      ifelse(prepared$ph >= profile$min_ph && prepared$ph <= profile$max_ph, 1, 0.5),
      ifelse(prepared$rainfall_mm >= profile$min_rainfall_mm && prepared$rainfall_mm <= profile$max_rainfall_mm, 1, 0.5)
    )
    item$land_suitability <- mean(fit_components)
    item$stable_price_rs_per_kg <- stable_price
    item
  })
  ideal_profit_scores <- candidate_scores(vapply(ideal_candidates, `[[`, numeric(1), "profit_rs_per_ha"))
  ideal_low_cost_scores <- candidate_scores(-vapply(ideal_candidates, `[[`, numeric(1), "total_cost_rs_per_ha"))
  for (i in seq_along(ideal_candidates)) {
    ideal_candidates[[i]]$ideal_ground_score <- (ideal_candidates[[i]]$land_suitability * 0.35) + ((1 - ideal_candidates[[i]]$risk) * 0.25) + (ideal_low_cost_scores[[i]] * 0.20) + (ideal_profit_scores[[i]] * 0.20)
  }
  ideal_ground <- ideal_candidates[[which.max(vapply(ideal_candidates, `[[`, numeric(1), "ideal_ground_score"))]]
  best_crop_name <- top_candidates[[1]]$crop
  rule_explanations <- make_rule_explanations(prepared, runtime$crop_profiles[[best_crop_name]], best_crop_name)
  driver_lists <- make_local_driver_lists(prepared, runtime$crop_profiles[[best_crop_name]], runtime$bundle$training_report$xai_summary$top_features)
  model_overview <- lapply(names(runtime$bundle$training_report$classification_metrics), function(name) {
    metrics <- runtime$bundle$training_report$classification_metrics[[name]]
    list(model = gsub("_", " ", tools::toTitleCase(name)), accuracy = as.numeric(metrics$accuracy), f1_weighted = as.numeric(metrics$f1_weighted), top3_accuracy = as.numeric(metrics$top3_accuracy))
  })
  list(
    best_crop = best_crop_name,
    top_crops = top_candidates,
    training_summary = runtime$bundle$training_report,
    analysis_context = list(analysis_date = as.character(context$analysis_date), current_month = month_name(current_month), season = current_season, area_hectares = area_ha),
    rejected_crops = rejected,
    ideal_ground_recommendation = list(crop = ideal_ground$crop, land_suitability = ideal_ground$land_suitability, expected_yield_t_ha = ideal_ground$expected_yield_t_ha, stable_price_rs_per_kg = ideal_ground$stable_price_rs_per_kg, total_cost_rs_per_ha = ideal_ground$total_cost_rs_per_ha, revenue_rs_per_ha = ideal_ground$revenue_rs_per_ha, profit_rs_per_ha = ideal_ground$profit_rs_per_ha, risk = ideal_ground$risk, sustainability_score = ideal_ground$sustainability_score, duration_months = ideal_ground$duration_months, ideal_ground_score = ideal_ground$ideal_ground_score, why = c("This recommendation ignores the current sowing month and focuses on long-term land suitability.", "It prioritizes strong soil and climate fit, lower risk, lower cost, and stable profit.")),
    explainability = list(global_top_features = head(runtime$bundle$training_report$xai_summary$top_features, 8), summary_plot_urls = list(classification = "/reports/classification_feature_importance.png", regression = "/reports/regression_feature_importance.png"), best_crop_local_explanation = list(crop = best_crop_name, classification_shap = driver_lists$classification, yield_shap = driver_lists$yield, rule_based_positives = rule_explanations$positives, rule_based_concerns = rule_explanations$concerns), model_overview = model_overview)
  )
}
