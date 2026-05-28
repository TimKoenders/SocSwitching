# ================================================================
# 01_vote_share_change_models.R
# Aggregate vote-share validation models
#
# Goal:
#   Estimate how demand-side salience changes and supply-side
#   social-democratic position changes predict changes in party-family
#   vote shares.
#
# Outcomes:
#   1) Social democratic
#   2) Far left
#   3) Green
#   4) Mainstream right
#   5) Far right
#
# Models:
#   OLS with country fixed effects
#   Election-clustered standard errors
#
# Interpretation:
#   Coefficients are percentage-point changes in party-family vote share
#   associated with a one-standard-deviation increase in the contextual
#   predictor, net of country fixed effects.
# ================================================================

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(fixest)
  library(broom)
  library(modelsummary)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

path_vote_shares <- here(
  "data", "processed",
  "parlgov_supply_vote_shares.rds"
)

path_salience <- here(
  "data", "processed",
  "eb_salience_election_model_input.rds"
)

path_supply_all <- here(
  "data", "analysis",
  "sd_election_supply_investment_state_libcons_all_operationalisations.rds"
)

output_dir <- here(
  "data", "analysis", "models",
  "aggregate_vote_share_change_models"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------

if (!file.exists(path_vote_shares)) {
  stop("Vote-share file not found: ", path_vote_shares)
}

if (!file.exists(path_salience)) {
  stop("Salience file not found: ", path_salience)
}

if (!file.exists(path_supply_all)) {
  stop("All-operationalisations supply file not found: ", path_supply_all)
}

vote_shares <- readRDS(path_vote_shares)
salience_context_raw <- readRDS(path_salience)
supply_context_all <- readRDS(path_supply_all)

stopifnot(is.data.frame(vote_shares), nrow(vote_shares) > 0)
stopifnot(is.data.frame(salience_context_raw), nrow(salience_context_raw) > 0)
stopifnot(is.data.frame(supply_context_all), nrow(supply_context_all) > 0)

# ------------------------------------------------
# 3. Settings
# ------------------------------------------------

primary_operationalisation <- "marpor_complete"

party_family_levels <- c(
  "Social democratic",
  "Far left",
  "Green",
  "Mainstream right",
  "Far right"
)

demand_predictors <- c(
  "eb_immigration_move_tminus1_to_t_z",
  "eb_environment_climate_move_tminus1_to_t_z",
  "eb_unemployment_move_tminus1_to_t_z"
)

supply_predictors <- c(
  "sd_investmentconsumption_move_std",
  "sd_stateconomy_move_std",
  "sd_libcons_move_std"
)

all_predictors <- c(
  demand_predictors,
  supply_predictors
)

coef_labels <- c(
  "eb_immigration_move_tminus1_to_t_z" =
    "Immigration salience",
  "eb_environment_climate_move_tminus1_to_t_z" =
    "Environment salience",
  "eb_unemployment_move_tminus1_to_t_z" =
    "Unemployment salience",
  "sd_investmentconsumption_move_std" =
    "SD social investment position",
  "sd_stateconomy_move_std" =
    "SD economic position",
  "sd_libcons_move_std" =
    "SD cultural position"
)

# ------------------------------------------------
# 4. Prepare vote-share outcomes
# ------------------------------------------------

vote_share_model_data <- vote_shares %>%
  dplyr::mutate(
    party_family = dplyr::case_when(
      competitor == "social_democratic" ~ "Social democratic",
      competitor == "far_left" ~ "Far left",
      competitor == "green" ~ "Green",
      competitor == "far_right" ~ "Far right",
      competitor %in% c("liberal", "con") ~ "Mainstream right",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(party_family)) %>%
  dplyr::group_by(
    country,
    country_name,
    elec_id,
    election_date,
    year,
    month,
    party_family
  ) %>%
  dplyr::summarise(
    vote_share = sum(vote_share, na.rm = TRUE),
    vote_share_lag = sum(vote_share_lag, na.rm = TRUE),
    vote_share_change = sum(vote_share_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    year_num = as.integer(year),
    party_family = factor(
      party_family,
      levels = party_family_levels
    )
  ) %>%
  dplyr::filter(
    !is.na(vote_share_change)
  )

cat("\n================================================\n")
cat("Vote-share outcome support\n")
cat("================================================\n")
print(
  vote_share_model_data %>%
    dplyr::count(party_family, sort = FALSE),
  n = Inf
)

# ------------------------------------------------
# 5. Prepare demand-side salience context
# ------------------------------------------------

missing_salience_vars <- setdiff(
  c("elec_id", demand_predictors),
  names(salience_context_raw)
)

if (length(missing_salience_vars) > 0) {
  stop(
    "Missing salience variables: ",
    paste(missing_salience_vars, collapse = ", ")
  )
}

salience_context <- salience_context_raw %>%
  dplyr::select(
    elec_id,
    dplyr::all_of(demand_predictors)
  ) %>%
  dplyr::distinct(elec_id, .keep_all = TRUE)

# ------------------------------------------------
# 6. Prepare supply-side position context
# ------------------------------------------------

if (!"operationalisation" %in% names(supply_context_all)) {
  stop("Supply file does not contain an operationalisation column.")
}

if (!primary_operationalisation %in% unique(supply_context_all$operationalisation)) {
  stop(
    "Supply file does not contain operationalisation: ",
    primary_operationalisation
  )
}

missing_supply_vars <- setdiff(
  c("elec_id", "iso2c_file", "operationalisation", supply_predictors),
  names(supply_context_all)
)

if (length(missing_supply_vars) > 0) {
  stop(
    "Missing supply variables: ",
    paste(missing_supply_vars, collapse = ", ")
  )
}

supply_context <- supply_context_all %>%
  dplyr::filter(
    operationalisation == primary_operationalisation
  ) %>%
  dplyr::select(
    elec_id,
    iso2c_file,
    operationalisation,
    dplyr::all_of(supply_predictors)
  ) %>%
  dplyr::distinct(elec_id, .keep_all = TRUE)

# ------------------------------------------------
# 7. Merge model data
# ------------------------------------------------

model_data <- vote_share_model_data %>%
  dplyr::left_join(
    salience_context,
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    supply_context,
    by = "elec_id"
  ) %>%
  dplyr::mutate(
    country = as.character(country),
    elec_id = as.character(elec_id),
    election_cluster = factor(elec_id)
  )

missing_predictor_summary <- model_data %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(all_predictors),
      ~ sum(is.na(.x))
    )
  )

cat("\n================================================\n")
cat("Missing predictor values after merge\n")
cat("================================================\n")
print(missing_predictor_summary, width = Inf)

model_data_complete <- model_data %>%
  dplyr::filter(
    !dplyr::if_any(dplyr::all_of(all_predictors), is.na),
    !is.na(vote_share_change),
    !is.na(country),
    !is.na(elec_id)
  )

# ------------------------------------------------
# 8. Diagnostics
# ------------------------------------------------

diagnostics <- model_data_complete %>%
  dplyr::group_by(party_family) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year_num, na.rm = TRUE),
    max_year = max(year_num, na.rm = TRUE),
    mean_vote_share = mean(vote_share, na.rm = TRUE),
    mean_vote_share_change = mean(vote_share_change, na.rm = TRUE),
    sd_vote_share_change = stats::sd(vote_share_change, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n================================================\n")
cat("Estimation-sample diagnostics\n")
cat("================================================\n")
print(diagnostics, n = Inf, width = Inf)

readr::write_csv(
  diagnostics,
  file.path(output_dir, "vote_share_change_model_diagnostics.csv")
)

readr::write_csv(
  model_data_complete,
  file.path(output_dir, "vote_share_change_model_data.csv")
)

saveRDS(
  model_data_complete,
  file.path(output_dir, "vote_share_change_model_data.rds")
)

# ------------------------------------------------
# 9. Estimate separate OLS models by party family
# ------------------------------------------------

estimate_family_model <- function(family_name, data) {
  df <- data %>%
    dplyr::filter(party_family == family_name)
  
  if (nrow(df) == 0) {
    stop("No rows for party family: ", family_name)
  }
  
  fixest::feols(
    vote_share_change ~
      eb_immigration_move_tminus1_to_t_z +
      eb_environment_climate_move_tminus1_to_t_z +
      eb_unemployment_move_tminus1_to_t_z +
      sd_investmentconsumption_move_std +
      sd_stateconomy_move_std +
      sd_libcons_move_std |
      country,
    data = df,
    cluster = ~ elec_id
  )
}

models <- purrr::map(
  party_family_levels,
  estimate_family_model,
  data = model_data_complete
)

names(models) <- party_family_levels

saveRDS(
  models,
  file.path(output_dir, "vote_share_change_ols_country_fe_election_clustered_models.rds")
)

# ------------------------------------------------
# 10. Print model summaries
# ------------------------------------------------

cat("\n================================================\n")
cat("Model summaries\n")
cat("================================================\n")

print(
  fixest::etable(
    models,
    dict = coef_labels,
    se.below = TRUE,
    fitstat = ~ n + r2 + wr2
  )
)

# ------------------------------------------------
# 11. Export model tables
# ------------------------------------------------

modelsummary::modelsummary(
  models,
  coef_map = coef_labels,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|RMSE",
  output = file.path(output_dir, "vote_share_change_models.tex")
)

modelsummary::modelsummary(
  models,
  coef_map = coef_labels,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|RMSE",
  output = file.path(output_dir, "vote_share_change_models.html")
)

# ------------------------------------------------
# 12. Tidy coefficient output
# ------------------------------------------------

tidy_results <- purrr::imap_dfr(
  models,
  function(model, family_name) {
    broom::tidy(
      model,
      conf.int = TRUE
    ) %>%
      dplyr::mutate(
        party_family = family_name,
        .before = 1
      )
  }
) %>%
  dplyr::filter(term %in% all_predictors) %>%
  dplyr::mutate(
    predictor_label = dplyr::recode(term, !!!coef_labels),
    estimate_pp = estimate,
    se_pp = std.error,
    conf.low_pp = conf.low,
    conf.high_pp = conf.high
  ) %>%
  dplyr::select(
    party_family,
    term,
    predictor_label,
    estimate_pp,
    se_pp,
    statistic,
    p.value,
    conf.low_pp,
    conf.high_pp
  )

cat("\n================================================\n")
cat("Tidy coefficient results\n")
cat("================================================\n")
print(tidy_results, n = Inf, width = Inf)

readr::write_csv(
  tidy_results,
  file.path(output_dir, "vote_share_change_models_tidy.csv")
)

# ------------------------------------------------
# 13. Wide coefficient table
# ------------------------------------------------

wide_results <- tidy_results %>%
  dplyr::select(
    party_family,
    predictor_label,
    estimate_pp,
    se_pp,
    p.value
  ) %>%
  dplyr::mutate(
    estimate_formatted = dplyr::case_when(
      p.value < 0.001 ~ sprintf("%.3f***", estimate_pp),
      p.value < 0.01  ~ sprintf("%.3f**", estimate_pp),
      p.value < 0.05  ~ sprintf("%.3f*", estimate_pp),
      p.value < 0.1   ~ sprintf("%.3f+", estimate_pp),
      TRUE            ~ sprintf("%.3f", estimate_pp)
    ),
    se_formatted = paste0("(", sprintf("%.3f", se_pp), ")"),
    cell = paste0(estimate_formatted, "\n", se_formatted)
  ) %>%
  dplyr::select(
    predictor_label,
    party_family,
    cell
  ) %>%
  tidyr::pivot_wider(
    names_from = party_family,
    values_from = cell
  )

readr::write_csv(
  wide_results,
  file.path(output_dir, "vote_share_change_models_wide_print_table.csv")
)

# ------------------------------------------------
# 14. Stacked model as robustness/compact alternative
# ------------------------------------------------

stacked_model <- fixest::feols(
  vote_share_change ~
    party_family *
    (
      eb_immigration_move_tminus1_to_t_z +
        eb_environment_climate_move_tminus1_to_t_z +
        eb_unemployment_move_tminus1_to_t_z +
        sd_investmentconsumption_move_std +
        sd_stateconomy_move_std +
        sd_libcons_move_std
    ) |
    country,
  data = model_data_complete,
  cluster = ~ elec_id
)

saveRDS(
  stacked_model,
  file.path(output_dir, "vote_share_change_stacked_ols_country_fe_election_clustered_model.rds")
)

stacked_tidy <- broom::tidy(
  stacked_model,
  conf.int = TRUE
)

readr::write_csv(
  stacked_tidy,
  file.path(output_dir, "vote_share_change_stacked_model_tidy.csv")
)

modelsummary::modelsummary(
  list("Stacked model" = stacked_model),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|RMSE",
  output = file.path(output_dir, "vote_share_change_stacked_model.tex")
)

# ------------------------------------------------
# 15. Save metadata
# ------------------------------------------------

metadata <- list(
  outcome = "vote_share_change",
  model = "OLS with country fixed effects and election-clustered standard errors",
  demand_predictors = demand_predictors,
  supply_predictors = supply_predictors,
  supply_operationalisation = primary_operationalisation,
  party_family_levels = party_family_levels,
  vote_share_source = path_vote_shares,
  salience_source = path_salience,
  supply_source = path_supply_all,
  output_dir = output_dir
)

saveRDS(
  metadata,
  file.path(output_dir, "vote_share_change_model_metadata.rds")
)

cat("\n================================================\n")
cat("Files written\n")
cat("================================================\n")
cat(file.path(output_dir, "vote_share_change_model_diagnostics.csv"), "\n")
cat(file.path(output_dir, "vote_share_change_model_data.csv"), "\n")
cat(file.path(output_dir, "vote_share_change_ols_country_fe_election_clustered_models.rds"), "\n")
cat(file.path(output_dir, "vote_share_change_models_tidy.csv"), "\n")
cat(file.path(output_dir, "vote_share_change_models_wide_print_table.csv"), "\n")
cat(file.path(output_dir, "vote_share_change_models.tex"), "\n")
cat(file.path(output_dir, "vote_share_change_models.html"), "\n")
cat(file.path(output_dir, "vote_share_change_stacked_ols_country_fe_election_clustered_model.rds"), "\n")
cat(file.path(output_dir, "vote_share_change_stacked_model_tidy.csv"), "\n")
cat(file.path(output_dir, "vote_share_change_model_metadata.rds"), "\n")

cat("\nScript completed successfully.\n")