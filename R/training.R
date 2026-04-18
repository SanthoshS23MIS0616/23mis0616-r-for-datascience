source(file.path(project_root, "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))
source(file.path(project_root, "R", "crop_rules.R"))
source(file.path(project_root, "R", "utils.R"))

save_importance_plot <- function(items, output_path, title_text) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !length(items)) return(invisible(NULL))
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  df <- data.frame(
    feature = factor(vapply(items, `[[`, "", "feature"), levels = rev(vapply(items, `[[`, "", "feature"))),
    importance = vapply(items, `[[`, numeric(1), "mean_importance")
  )
  png(output_path, width = 1400, height = 900, res = 160)
  print(
    ggplot2::ggplot(df, ggplot2::aes(x = feature, y = importance, fill = importance)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = "#c97d2a", high = "#225c36") +
      ggplot2::labs(title = title_text, x = NULL, y = "Mean Importance") +
      ggplot2::theme_minimal(base_size = 15)
  )
  dev.off()
}

build_feature_ranking <- function(multinom_model, tree_importance, rf_model, feature_names) {
  multinom_coeff <- coef(multinom_model)
  multinom_importance <- if (is.null(dim(multinom_coeff))) abs(multinom_coeff) else colMeans(abs(multinom_coeff))
  multinom_importance <- multinom_importance[feature_names]
  multinom_importance[is.na(multinom_importance)] <- 0

  tree_scaled_source <- rep(0, length(feature_names))
  names(tree_scaled_source) <- feature_names
  if (!is.null(tree_importance) && length(tree_importance)) {
    common <- intersect(names(tree_importance), feature_names)
    tree_scaled_source[common] <- tree_importance[common]
  }

  rf_importance <- randomForest::importance(rf_model, type = 2)
  if (is.matrix(rf_importance)) rf_importance <- rf_importance[, 1]
  rf_importance <- rf_importance[feature_names]
  rf_importance[is.na(rf_importance)] <- 0

  normalize <- function(x) if (max(x, na.rm = TRUE) == 0) rep(0, length(x)) else x / max(x, na.rm = TRUE)
  multinom_scaled <- normalize(multinom_importance)
  tree_scaled <- normalize(tree_scaled_source)
  rf_scaled <- normalize(rf_importance)

  ranking <- lapply(feature_names, function(feature) {
    multinom_value <- if (feature %in% names(multinom_scaled)) multinom_scaled[[feature]] else 0
    tree_value <- if (feature %in% names(tree_scaled)) tree_scaled[[feature]] else 0
    rf_value <- if (feature %in% names(rf_scaled)) rf_scaled[[feature]] else 0
    list(
      feature = feature,
      multinom_importance = as.numeric(multinom_value),
      decision_tree_importance = as.numeric(tree_value),
      random_forest_importance = as.numeric(rf_value),
      mean_importance = as.numeric(mean(c(multinom_value, tree_value, rf_value)))
    )
  })
  ranking[order(vapply(ranking, `[[`, numeric(1), "mean_importance"), decreasing = TRUE)]
}

train_and_save <- function() {
  dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

  recommendation_df <- read.csv(CROP_RECOMMENDATION_DATASET_PATH, stringsAsFactors = FALSE)
  master_df <- read.csv(PROJECT_MASTER_DATASET_PATH, stringsAsFactors = FALSE)

  recommendation_df <- unique(recommendation_df)
  master_df <- unique(master_df)
  recommendation_df <- sample_by_class(recommendation_df, TARGET_CROP, 9000)
  master_df <- sample_by_class(master_df, TARGET_CROP, 12000)

  y_cls <- as.character(recommendation_df[[TARGET_CROP]])
  split_cls <- stratified_split(y_cls, test_size = 0.2, seed = RANDOM_STATE)
  x_cls <- recommendation_df[, c(CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES), drop = FALSE]
  x_train_cls <- x_cls[split_cls$train, , drop = FALSE]
  x_test_cls <- x_cls[split_cls$test, , drop = FALSE]
  y_train_cls <- factor(y_cls[split_cls$train])
  y_test_cls <- factor(y_cls[split_cls$test], levels = levels(y_train_cls))

  class_preprocessor <- fit_preprocessor(x_train_cls, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  x_train_cls_matrix <- transform_features(x_train_cls, class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  x_test_cls_matrix <- transform_features(x_test_cls, class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  class_levels <- levels(y_train_cls)

  multinom_train_df <- data.frame(target = y_train_cls, prepare_feature_frame(x_train_cls, class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES), check.names = TRUE)
  multinom_test_df <- prepare_feature_frame(x_test_cls, class_preprocessor, CLASSIFICATION_NUMERIC_FEATURES, CLASSIFICATION_CATEGORICAL_FEATURES)
  multinom_model <- nnet::multinom(stats::as.formula("target ~ ."), data = multinom_train_df, trace = FALSE, MaxNWts = 70000, maxit = 300)
  tree_model <- e1071::naiveBayes(x = x_train_cls_matrix, y = y_train_cls)
  rf_model <- randomForest::randomForest(x = x_train_cls_matrix, y = y_train_cls, ntree = 200, mtry = max(2, floor(sqrt(ncol(x_train_cls_matrix)))), importance = TRUE)

  probs_multinom_test <- align_probability_matrix(nnet:::predict.multinom(multinom_model, newdata = multinom_test_df, type = "probs"), class_levels)
  probs_tree_test <- align_probability_matrix(e1071:::predict.naiveBayes(tree_model, newdata = x_test_cls_matrix, type = "raw"), class_levels)
  probs_rf_test <- align_probability_matrix(stats::predict(rf_model, newdata = x_test_cls_matrix, type = "prob"), class_levels)
  probs_multinom_train <- align_probability_matrix(nnet:::predict.multinom(multinom_model, newdata = multinom_train_df[, -1, drop = FALSE], type = "probs"), class_levels)
  probs_tree_train <- align_probability_matrix(e1071:::predict.naiveBayes(tree_model, newdata = x_train_cls_matrix, type = "raw"), class_levels)
  probs_rf_train <- align_probability_matrix(stats::predict(rf_model, newdata = x_train_cls_matrix, type = "prob"), class_levels)

  pred_multinom_test <- factor(class_levels[max.col(probs_multinom_test)], levels = class_levels)
  pred_tree_test <- factor(class_levels[max.col(probs_tree_test)], levels = class_levels)
  pred_rf_test <- factor(class_levels[max.col(probs_rf_test)], levels = class_levels)
  pred_multinom_train <- factor(class_levels[max.col(probs_multinom_train)], levels = class_levels)
  pred_tree_train <- factor(class_levels[max.col(probs_tree_train)], levels = class_levels)
  pred_rf_train <- factor(class_levels[max.col(probs_rf_train)], levels = class_levels)

  metrics_multinom <- c(setNames(classification_summary(y_train_cls, pred_multinom_train, probs_multinom_train, class_levels), paste0("train_", names(classification_summary(y_train_cls, pred_multinom_train, probs_multinom_train, class_levels)))), classification_summary(y_test_cls, pred_multinom_test, probs_multinom_test, class_levels))
  metrics_tree <- c(setNames(classification_summary(y_train_cls, pred_tree_train, probs_tree_train, class_levels), paste0("train_", names(classification_summary(y_train_cls, pred_tree_train, probs_tree_train, class_levels)))), classification_summary(y_test_cls, pred_tree_test, probs_tree_test, class_levels))
  metrics_rf <- c(setNames(classification_summary(y_train_cls, pred_rf_train, probs_rf_train, class_levels), paste0("train_", names(classification_summary(y_train_cls, pred_rf_train, probs_rf_train, class_levels)))), classification_summary(y_test_cls, pred_rf_test, probs_rf_test, class_levels))

  ensemble_weights <- c(multinom = metrics_multinom[["accuracy"]], decision_tree = metrics_tree[["accuracy"]], random_forest = metrics_rf[["accuracy"]])
  ensemble_weights <- ensemble_weights / sum(ensemble_weights)
  probs_ensemble_test <- (probs_multinom_test * ensemble_weights[["multinom"]]) + (probs_tree_test * ensemble_weights[["decision_tree"]]) + (probs_rf_test * ensemble_weights[["random_forest"]])
  probs_ensemble_train <- (probs_multinom_train * ensemble_weights[["multinom"]]) + (probs_tree_train * ensemble_weights[["decision_tree"]]) + (probs_rf_train * ensemble_weights[["random_forest"]])
  pred_ensemble_test <- factor(class_levels[max.col(probs_ensemble_test)], levels = class_levels)
  pred_ensemble_train <- factor(class_levels[max.col(probs_ensemble_train)], levels = class_levels)
  metrics_ensemble <- c(setNames(classification_summary(y_train_cls, pred_ensemble_train, probs_ensemble_train, class_levels), paste0("train_", names(classification_summary(y_train_cls, pred_ensemble_train, probs_ensemble_train, class_levels)))), classification_summary(y_test_cls, pred_ensemble_test, probs_ensemble_test, class_levels))

  y_reg <- as.numeric(master_df[[TARGET_YIELD]])
  set.seed(RANDOM_STATE)
  test_reg_idx <- sample.int(nrow(master_df), max(1, floor(nrow(master_df) * 0.2)))
  train_reg_idx <- setdiff(seq_len(nrow(master_df)), test_reg_idx)
  x_train_reg <- master_df[train_reg_idx, c(REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES), drop = FALSE]
  x_test_reg <- master_df[test_reg_idx, c(REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES), drop = FALSE]
  y_train_reg <- y_reg[train_reg_idx]
  y_test_reg <- y_reg[test_reg_idx]

  regression_preprocessor <- fit_preprocessor(x_train_reg, REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES)
  x_train_reg_matrix <- transform_features(x_train_reg, regression_preprocessor, REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES)
  x_test_reg_matrix <- transform_features(x_test_reg, regression_preprocessor, REGRESSION_NUMERIC_FEATURES, REGRESSION_CATEGORICAL_FEATURES)
  regression_model <- randomForest::randomForest(x = x_train_reg_matrix, y = y_train_reg, ntree = 250, mtry = max(2, floor(sqrt(ncol(x_train_reg_matrix)))), importance = TRUE)
  pred_reg_train <- stats::predict(regression_model, newdata = x_train_reg_matrix)
  pred_reg_test <- stats::predict(regression_model, newdata = x_test_reg_matrix)
  regression_metrics <- c(setNames(regression_summary(y_train_reg, pred_reg_train), paste0("train_", names(regression_summary(y_train_reg, pred_reg_train)))), regression_summary(y_test_reg, pred_reg_test))

  nb_params <- tree_model$tables
  nb_importance <- rep(0, length(class_preprocessor$feature_names))
  names(nb_importance) <- class_preprocessor$feature_names
  feature_ranking <- build_feature_ranking(multinom_model, nb_importance, rf_model, class_preprocessor$feature_names)
  save_importance_plot(feature_ranking[seq_len(min(20, length(feature_ranking)))], CLASSIFICATION_IMPORTANCE_PLOT, "R Classification Feature Importance")

  reg_importance <- randomForest::importance(regression_model, type = 1)
  if (is.matrix(reg_importance)) reg_importance <- reg_importance[, 1]
  reg_features <- names(sort(reg_importance, decreasing = TRUE))
  reg_items <- lapply(head(reg_features, 20), function(feature) list(feature = feature, mean_importance = as.numeric(reg_importance[[feature]])))
  save_importance_plot(reg_items, REGRESSION_IMPORTANCE_PLOT, "R Regression Feature Importance")

  training_report <- list(
    data_mode = "r_realistic_v2_native_pipeline",
    dataset_summary = list(classification_rows = nrow(recommendation_df), regression_rows = nrow(master_df), crop_count = length(unique(master_df$crop)), state_count = length(unique(master_df$state_name))),
    classification_metrics = list(multinom = metrics_multinom, decision_tree = metrics_tree, random_forest = metrics_rf, ensemble = metrics_ensemble),
    regression_metrics = regression_metrics,
    model_comparison = list(best_classification_model = "ensemble", best_regression_model = "random_forest_regressor"),
    xai_summary = list(status = "ok", classification_importance_plot = CLASSIFICATION_IMPORTANCE_PLOT, regression_importance_plot = REGRESSION_IMPORTANCE_PLOT, top_features = head(feature_ranking, 20)),
    warnings = list()
  )

  bundle <- list(
    class_preprocessor = class_preprocessor,
    regression_preprocessor = regression_preprocessor,
    class_levels = class_levels,
    classification_models = list(multinom = multinom_model, decision_tree = tree_model, random_forest = rf_model, ensemble_weights = ensemble_weights),
    regression_model = regression_model,
    training_report = training_report,
    defaults = list(
      nitrogen = median(master_df$nitrogen, na.rm = TRUE),
      phosphorous = median(master_df$phosphorous, na.rm = TRUE),
      potassium = median(master_df$potassium, na.rm = TRUE),
      temperature_c = median(master_df$temperature_c, na.rm = TRUE),
      humidity = median(master_df$humidity, na.rm = TRUE),
      ph = median(master_df$ph, na.rm = TRUE),
      rainfall_mm = median(master_df$rainfall_mm, na.rm = TRUE),
      moisture = median(master_df$moisture, na.rm = TRUE),
      area = median(master_df$area_ha, na.rm = TRUE),
      season = names(sort(table(master_df$season), decreasing = TRUE))[1],
      state_name = names(sort(table(master_df$state_name), decreasing = TRUE))[1],
      district_name = names(sort(table(master_df$district_name), decreasing = TRUE))[1],
      crop_year = as.integer(round(median(master_df$crop_year, na.rm = TRUE))),
      price_per_ton = median(master_df$price_per_ton, na.rm = TRUE)
    )
  )

  saveRDS(bundle, MODEL_BUNDLE_PATH)
  writeLines(jsonlite::toJSON(training_report, pretty = TRUE, auto_unbox = TRUE, null = "null"), TRAINING_REPORT_PATH)
  training_report
}
