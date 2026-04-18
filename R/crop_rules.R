source(file.path(project_root, "R", "bootstrap.R"))

MONTH_NAMES <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

CROP_DATABASE <- list(
  apple = list(duration_months = 18L, yield_avg_t_ha = 11.0, water_need = "medium", price_range_rs_per_kg = c(28.0, 42.0), base_price_rs_per_kg = 34.0, base_cost_rs_per_ha = 210000.0, sowing_months = c(7L, 8L), peak_harvest_months = c(8L, 9L, 10L), new_farmer_penalty = 0.38, base_risk = 0.58, base_sustainability = 0.54, price_volatility = 0.32, chemical_dependency = 0.42, soil_sensitivity = 0.34),
  banana = list(duration_months = 11L, yield_avg_t_ha = 28.0, water_need = "high", price_range_rs_per_kg = c(11.0, 18.0), base_price_rs_per_kg = 14.0, base_cost_rs_per_ha = 185000.0, sowing_months = 1:12, peak_harvest_months = c(9L, 10L, 11L), new_farmer_penalty = 0.34, base_risk = 0.46, base_sustainability = 0.48, price_volatility = 0.24, chemical_dependency = 0.36, soil_sensitivity = 0.26),
  blackgram = list(duration_months = 4L, yield_avg_t_ha = 0.9, water_need = "low", price_range_rs_per_kg = c(58.0, 76.0), base_price_rs_per_kg = 66.0, base_cost_rs_per_ha = 38000.0, sowing_months = c(2L, 3L, 4L, 7L, 8L), peak_harvest_months = c(8L, 9L, 10L), new_farmer_penalty = 0.22, base_risk = 0.31, base_sustainability = 0.78, price_volatility = 0.18, chemical_dependency = 0.14, soil_sensitivity = 0.18),
  coconut = list(duration_months = 15L, yield_avg_t_ha = 8.0, water_need = "high", price_range_rs_per_kg = c(45.0, 70.0), base_price_rs_per_kg = 54.0, base_cost_rs_per_ha = 170000.0, sowing_months = 1:12, peak_harvest_months = c(9L, 10L, 11L), new_farmer_penalty = 0.37, base_risk = 0.55, base_sustainability = 0.49, price_volatility = 0.28, chemical_dependency = 0.30, soil_sensitivity = 0.32),
  coffee = list(duration_months = 18L, yield_avg_t_ha = 1.5, water_need = "medium", price_range_rs_per_kg = c(180.0, 260.0), base_price_rs_per_kg = 210.0, base_cost_rs_per_ha = 155000.0, sowing_months = c(6L, 7L, 8L), peak_harvest_months = c(11L, 12L, 1L), new_farmer_penalty = 0.39, base_risk = 0.62, base_sustainability = 0.58, price_volatility = 0.30, chemical_dependency = 0.34, soil_sensitivity = 0.32),
  grapes = list(duration_months = 14L, yield_avg_t_ha = 16.0, water_need = "medium", price_range_rs_per_kg = c(26.0, 42.0), base_price_rs_per_kg = 31.0, base_cost_rs_per_ha = 200000.0, sowing_months = c(10L, 11L, 12L), peak_harvest_months = c(2L, 3L, 4L), new_farmer_penalty = 0.36, base_risk = 0.57, base_sustainability = 0.50, price_volatility = 0.26, chemical_dependency = 0.38, soil_sensitivity = 0.28),
  jute = list(duration_months = 5L, yield_avg_t_ha = 2.3, water_need = "medium", price_range_rs_per_kg = c(42.0, 58.0), base_price_rs_per_kg = 50.0, base_cost_rs_per_ha = 62000.0, sowing_months = c(3L, 4L, 5L), peak_harvest_months = c(8L, 9L, 10L), new_farmer_penalty = 0.24, base_risk = 0.38, base_sustainability = 0.66, price_volatility = 0.20, chemical_dependency = 0.16, soil_sensitivity = 0.20),
  lentil = list(duration_months = 5L, yield_avg_t_ha = 1.1, water_need = "low", price_range_rs_per_kg = c(62.0, 84.0), base_price_rs_per_kg = 72.0, base_cost_rs_per_ha = 41000.0, sowing_months = c(10L, 11L), peak_harvest_months = c(2L, 3L), new_farmer_penalty = 0.23, base_risk = 0.33, base_sustainability = 0.79, price_volatility = 0.19, chemical_dependency = 0.13, soil_sensitivity = 0.18),
  maize = list(duration_months = 4L, yield_avg_t_ha = 3.9, water_need = "medium", price_range_rs_per_kg = c(18.0, 26.0), base_price_rs_per_kg = 22.0, base_cost_rs_per_ha = 52000.0, sowing_months = c(2L, 3L, 4L, 6L, 7L), peak_harvest_months = c(8L, 9L, 10L), new_farmer_penalty = 0.24, base_risk = 0.36, base_sustainability = 0.64, price_volatility = 0.17, chemical_dependency = 0.20, soil_sensitivity = 0.20),
  mango = list(duration_months = 18L, yield_avg_t_ha = 9.0, water_need = "medium", price_range_rs_per_kg = c(18.0, 32.0), base_price_rs_per_kg = 24.0, base_cost_rs_per_ha = 165000.0, sowing_months = c(6L, 7L, 8L), peak_harvest_months = c(4L, 5L, 6L), new_farmer_penalty = 0.38, base_risk = 0.55, base_sustainability = 0.56, price_volatility = 0.25, chemical_dependency = 0.31, soil_sensitivity = 0.28),
  orange = list(duration_months = 16L, yield_avg_t_ha = 11.0, water_need = "medium", price_range_rs_per_kg = c(18.0, 30.0), base_price_rs_per_kg = 24.0, base_cost_rs_per_ha = 172000.0, sowing_months = c(6L, 7L, 8L), peak_harvest_months = c(11L, 12L, 1L), new_farmer_penalty = 0.36, base_risk = 0.52, base_sustainability = 0.55, price_volatility = 0.24, chemical_dependency = 0.30, soil_sensitivity = 0.27),
  papaya = list(duration_months = 10L, yield_avg_t_ha = 22.0, water_need = "medium", price_range_rs_per_kg = c(11.0, 20.0), base_price_rs_per_kg = 15.0, base_cost_rs_per_ha = 145000.0, sowing_months = 1:12, peak_harvest_months = c(6L, 7L, 8L), new_farmer_penalty = 0.33, base_risk = 0.43, base_sustainability = 0.57, price_volatility = 0.22, chemical_dependency = 0.28, soil_sensitivity = 0.24),
  rice = list(duration_months = 5L, yield_avg_t_ha = 3.6, water_need = "high", price_range_rs_per_kg = c(20.0, 30.0), base_price_rs_per_kg = 24.0, base_cost_rs_per_ha = 65000.0, sowing_months = c(6L, 7L, 8L, 11L, 12L), peak_harvest_months = c(10L, 11L, 12L), new_farmer_penalty = 0.26, base_risk = 0.40, base_sustainability = 0.44, price_volatility = 0.16, chemical_dependency = 0.24, soil_sensitivity = 0.20)
)

derive_season <- function(month) {
  if (month %in% c(6L, 7L, 8L, 9L, 10L)) return("Kharif")
  if (month %in% c(11L, 12L, 1L, 2L)) return("Rabi")
  "Summer"
}

month_name <- function(month) MONTH_NAMES[(((as.integer(month) - 1L) %% 12L) + 1L)]
harvest_month <- function(sowing_month, duration_months) (((as.integer(sowing_month) - 1L + as.integer(duration_months) - 1L) %% 12L) + 1L)

late_sowing_penalty <- function(month, sowing_months) {
  if (length(sowing_months) <= 1L) return(0)
  if (month == tail(sowing_months, 1)) return(0.25)
  if (month != sowing_months[[1]]) return(0.12)
  0
}

water_need_factor <- function(level) switch(as.character(level), low = 0.12, medium = 0.22, high = 0.34, 0.22)

current_analysis_context <- function() {
  today <- Sys.Date()
  month <- as.integer(format(today, "%m"))
  list(
    analysis_date = as.character(today),
    current_month_number = month,
    current_month = month_name(month),
    season = derive_season(month),
    year = as.integer(format(today, "%Y"))
  )
}
