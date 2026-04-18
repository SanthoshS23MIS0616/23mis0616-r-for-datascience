source(file.path(project_root, "R", "bootstrap.R"))
source(file.path(project_root, "R", "config.R"))

clip_value <- function(value, minimum, maximum) {
  as.numeric(max(min(value, maximum), minimum))
}

candidate_scores <- function(values) {
  low <- min(values, na.rm = TRUE)
  high <- max(values, na.rm = TRUE)
  if (!is.finite(low) || !is.finite(high) || high <= low) {
    return(rep(1, length(values)))
  }
  (values - low) / (high - low)
}

compute_mode <- function(values) {
  values <- values[!is.na(values) & nzchar(as.character(values))]
  if (!length(values)) return(NA_character_)
  names(sort(table(as.character(values)), decreasing = TRUE))[1]
}

sample_by_class <- function(df, label_column, max_rows) {
  if (nrow(df) <= max_rows) return(df)
  proportions <- prop.table(table(df[[label_column]]))
  desired <- pmax(round(proportions * max_rows), 1)
  parts <- lapply(split(df, df[[label_column]]), function(frame) {
    label <- as.character(frame[[label_column]][1])
    take <- min(nrow(frame), desired[[label]])
    frame[sample.int(nrow(frame), take), , drop = FALSE]
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  if (nrow(out) > max_rows) {
    out <- out[sample.int(nrow(out), max_rows), , drop = FALSE]
  }
  out
}

stratified_split <- function(y, test_size = 0.2, seed = RANDOM_STATE) {
  set.seed(seed)
  groups <- split(seq_along(y), y)
  test_indices <- unlist(lapply(groups, function(idx) {
    sample(idx, max(1, floor(length(idx) * test_size)))
  }), use.names = FALSE)
  list(train = setdiff(seq_along(y), test_indices), test = sort(test_indices))
}

weighted_f1_score <- function(y_true, y_pred) {
  labels <- sort(unique(c(as.character(y_true), as.character(y_pred))))
  total <- length(y_true)
  weighted <- sapply(labels, function(label) {
    tp <- sum(y_true == label & y_pred == label)
    fp <- sum(y_true != label & y_pred == label)
    fn <- sum(y_true == label & y_pred != label)
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    f1 <- if ((precision + recall) == 0) 0 else (2 * precision * recall) / (precision + recall)
    f1 * (sum(y_true == label) / total)
  })
  sum(weighted)
}

top_k_accuracy <- function(y_true, probs, classes, k = 3L) {
  probs <- as.matrix(probs)
  class_idx <- match(as.character(y_true), classes)
  hits <- vapply(seq_len(nrow(probs)), function(i) {
    ranking <- order(probs[i, ], decreasing = TRUE)
    class_idx[i] %in% ranking[seq_len(min(k, ncol(probs)))]
  }, logical(1))
  mean(hits)
}

classification_summary <- function(y_true, y_pred, probs, classes) {
  list(
    accuracy = mean(as.character(y_true) == as.character(y_pred)),
    f1_weighted = weighted_f1_score(as.character(y_true), as.character(y_pred)),
    top3_accuracy = top_k_accuracy(y_true, probs, classes, 3L)
  )
}

regression_summary <- function(y_true, y_pred) {
  rmse <- sqrt(mean((y_true - y_pred) ^ 2))
  sst <- sum((y_true - mean(y_true)) ^ 2)
  sse <- sum((y_true - y_pred) ^ 2)
  r2 <- if (sst == 0) 0 else 1 - (sse / sst)
  list(rmse = rmse, r2 = r2)
}

fit_preprocessor <- function(df, numeric_features, categorical_features) {
  numeric_medians <- setNames(lapply(numeric_features, function(feature) median(as.numeric(df[[feature]]), na.rm = TRUE)), numeric_features)
  numeric_means <- setNames(lapply(numeric_features, function(feature) {
    values <- as.numeric(df[[feature]])
    values[is.na(values)] <- numeric_medians[[feature]]
    mean(values, na.rm = TRUE)
  }), numeric_features)
  numeric_sds <- setNames(lapply(numeric_features, function(feature) {
    values <- as.numeric(df[[feature]])
    values[is.na(values)] <- numeric_medians[[feature]]
    sd_value <- stats::sd(values, na.rm = TRUE)
    ifelse(is.na(sd_value) || sd_value == 0, 1, sd_value)
  }), numeric_features)
  categorical_modes <- setNames(lapply(categorical_features, function(feature) compute_mode(df[[feature]])), categorical_features)
  categorical_levels <- setNames(lapply(categorical_features, function(feature) {
    values <- as.character(df[[feature]])
    values[is.na(values) | !nzchar(values)] <- categorical_modes[[feature]]
    sort(unique(values))
  }), categorical_features)
  pre <- list(
    numeric_medians = numeric_medians,
    numeric_means = numeric_means,
    numeric_sds = numeric_sds,
    categorical_modes = categorical_modes,
    categorical_levels = categorical_levels
  )
  pre$feature_names <- colnames(transform_features(df, pre, numeric_features, categorical_features, fit_mode = TRUE))
  pre
}

prepare_feature_frame <- function(df, preprocessor, numeric_features, categorical_features) {
  working <- df[, c(numeric_features, categorical_features), drop = FALSE]
  for (feature in numeric_features) {
    values <- as.numeric(working[[feature]])
    values[is.na(values)] <- preprocessor$numeric_medians[[feature]]
    working[[feature]] <- (values - preprocessor$numeric_means[[feature]]) / preprocessor$numeric_sds[[feature]]
  }
  for (feature in categorical_features) {
    values <- as.character(working[[feature]])
    values[is.na(values) | !nzchar(values)] <- preprocessor$categorical_modes[[feature]]
    levels_used <- preprocessor$categorical_levels[[feature]]
    values[!(values %in% levels_used)] <- preprocessor$categorical_modes[[feature]]
    working[[feature]] <- factor(values, levels = levels_used)
  }
  working
}

transform_features <- function(df, preprocessor, numeric_features, categorical_features, fit_mode = FALSE) {
  working <- prepare_feature_frame(df, preprocessor, numeric_features, categorical_features)
  mm <- stats::model.matrix(~ . - 1, data = working)
  if (!fit_mode) {
    missing_cols <- setdiff(preprocessor$feature_names, colnames(mm))
    if (length(missing_cols)) {
      zeros <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
      colnames(zeros) <- missing_cols
      mm <- cbind(mm, zeros)
    }
    mm <- mm[, preprocessor$feature_names, drop = FALSE]
  }
  as.data.frame(mm)
}

align_probability_matrix <- function(prob_matrix, classes) {
  if (is.vector(prob_matrix) && !is.list(prob_matrix)) {
    original_names <- names(prob_matrix)
    prob_matrix <- matrix(prob_matrix, nrow = 1)
    if (!is.null(original_names)) colnames(prob_matrix) <- original_names
  } else {
    prob_matrix <- as.matrix(prob_matrix)
  }
  if (is.null(colnames(prob_matrix)) && ncol(prob_matrix) == length(classes)) {
    colnames(prob_matrix) <- classes
  }
  aligned <- matrix(0, nrow = nrow(prob_matrix), ncol = length(classes), dimnames = list(NULL, classes))
  common <- intersect(colnames(prob_matrix), classes)
  aligned[, common] <- prob_matrix[, common, drop = FALSE]
  aligned
}

pretty_feature_name <- function(name) {
  cleaned <- gsub("^.*?__", "", name)
  cleaned <- gsub("_", " ", cleaned)
  tools::toTitleCase(trimws(cleaned))
}
