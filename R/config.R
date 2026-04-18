source(file.path(project_root, "R", "bootstrap.R"))

DATA_DIR <- file.path(project_root, "data", "realistic_v2")
MODEL_DIR <- file.path(project_root, "models", "realistic_v2")
REPORT_DIR <- file.path(project_root, "reports")
STATIC_DIR <- file.path(project_root, "static")

CROP_RECOMMENDATION_DATASET_PATH <- file.path(DATA_DIR, "crop_recommendation_realistic.csv")
CROP_PROFILES_DATASET_PATH <- file.path(DATA_DIR, "crop_profiles_ranges.csv")
MARKET_PRICES_DATASET_PATH <- file.path(DATA_DIR, "market_prices_realistic.csv")
PROJECT_MASTER_DATASET_PATH <- file.path(DATA_DIR, "project_master_clean.csv")

MODEL_BUNDLE_PATH <- file.path(MODEL_DIR, "bundle.rds")
TRAINING_REPORT_PATH <- file.path(REPORT_DIR, "training_metrics.json")
CLASSIFICATION_IMPORTANCE_PLOT <- file.path(REPORT_DIR, "classification_feature_importance.png")
REGRESSION_IMPORTANCE_PLOT <- file.path(REPORT_DIR, "regression_feature_importance.png")

CLASSIFICATION_NUMERIC_FEATURES <- c("nitrogen", "phosphorous", "potassium", "temperature_c", "humidity", "ph", "rainfall_mm", "moisture")
CLASSIFICATION_CATEGORICAL_FEATURES <- c("state_name", "season")

REGRESSION_NUMERIC_FEATURES <- c("area_ha", "nitrogen", "phosphorous", "potassium", "temperature_c", "humidity", "ph", "rainfall_mm", "moisture")
REGRESSION_CATEGORICAL_FEATURES <- c("crop", "state_name", "district_name", "season")

TARGET_CROP <- "crop"
TARGET_YIELD <- "target_yield_t_ha"
RANDOM_STATE <- 42L
TOP_K_DEFAULT <- 5L
RANKING_POOL_SIZE <- 13L
