# ================================================================
# 07_demand_salience_models_controls.R
# Mixed conditional logit models for social-democratic voter exchanges
# with election-specific party-family choice sets, CPDS controls,
# and lagged Parlgov party-family vote-share controls
#
# Goal:
#   Estimate outward, inward, and net social-democratic voter exchanges
#   while allowing available alternatives to vary across elections.
#
# Main predictors:
#   Election-to-election changes in issue salience, based on the
#   nearest Eurobarometer wave before each election.
#
# Controls:
#   Individual level:
#     gender
#     age_group
#
#   CPDS election/country-year controls:
#     cpds_vturn_z
#     cpds_left_incumbent
#     cpds_effpar_ele_z
#     cpds_dis_gall_z
#     cpds_realgdpgr_lag1_z
#     cpds_unemp_lag1_z
#     cpds_outlays_lag1_z
#     cpds_openc_lag1_z
#     cpds_ud_lag1_z
#     cpds_postfisc_gini_lag1_z
#
#   Parlgov party-family vote-share controls:
#     parlgov_sd_vote_share_lag_z
#     parlgov_alt_family_vote_share_lag_z
#
# Specification:
#   The three salience-change variables enter the model jointly.
#   CPDS and Parlgov controls enter through alternative-specific
#   interactions. AMEs are computed only for salience predictors.
#
# Net effects:
#   Net = s_nonSD * AME_inward - s_SD * AME_outward
# ================================================================

options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(stringr)
  library(mclogit)
  library(tictoc)
})

# ------------------------------------------------
# 0. Switches
# ------------------------------------------------

run_delta_method_se <- TRUE
delta_step <- 1e-5

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching"

analysis_dir <- file.path(project_dir, "data", "analysis")
input_dir <- file.path(analysis_dir, "building_analysis_data")

output_dir <- file.path(
  analysis_dir,
  "models",
  "sd_restricted_choice_set_mixed_conditional_logit_country_re_salience_change_controls"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_outward <- file.path(
  input_dir,
  "df_analysis_outward_social_democratic_cpds_controls.rds"
)

path_inward <- file.path(
  input_dir,
  "df_analysis_inward_social_democratic_cpds_controls.rds"
)

path_eb_salience <- file.path(
  project_dir,
  "data",
  "processed",
  "eb_salience_election_model_input.rds"
)

path_parlgov_vote_shares <- file.path(
  project_dir,
  "data",
  "processed",
  "parlgov_supply_vote_shares.rds"
)

# ------------------------------------------------
# 2. Load and merge data
# ------------------------------------------------

tictoc::tic("Load data")

if (!file.exists(path_outward)) {
  stop("Outward modelling data not found: ", path_outward)
}

if (!file.exists(path_inward)) {
  stop("Inward modelling data not found: ", path_inward)
}

if (!file.exists(path_eb_salience)) {
  stop("Eurobarometer salience file not found: ", path_eb_salience)
}

if (!file.exists(path_parlgov_vote_shares)) {
  stop("Parlgov vote-share file not found: ", path_parlgov_vote_shares)
}

df_out <- readRDS(path_outward)
df_in  <- readRDS(path_inward)

eb_salience <- readRDS(path_eb_salience)
parlgov_vote_shares <- readRDS(path_parlgov_vote_shares)

salience_predictors <- c(
  "eb_immigration_move_tminus1_to_t_z",
  "eb_environment_climate_move_tminus1_to_t_z",
  "eb_unemployment_move_tminus1_to_t_z"
)

cpds_control_predictors <- c(
  "cpds_vturn_z",
  "cpds_left_incumbent",
  "cpds_effpar_ele_z",
  "cpds_dis_gall_z",
  "cpds_realgdpgr_lag1_z",
  "cpds_unemp_lag1_z",
  "cpds_outlays_lag1_z",
  "cpds_openc_lag1_z",
  "cpds_ud_lag1_z",
  "cpds_postfisc_gini_lag1_z"
)

vote_share_control_predictors <- c(
  "parlgov_sd_vote_share_lag_z",
  "parlgov_alt_family_vote_share_lag_z"
)

choice_level_predictors <- c(
  salience_predictors,
  cpds_control_predictors,
  "parlgov_sd_vote_share_lag_z"
)

alternative_level_predictors <- c(
  "parlgov_alt_family_vote_share_lag_z"
)

model_predictors <- c(
  choice_level_predictors,
  alternative_level_predictors
)

salience_specification <- "joint_salience_change_block_nearest_prior_eb_wave"
cpds_control_specification <- "extended_cpds_controls"
vote_share_control_specification <- "lagged_parlgov_party_family_vote_shares"
net_share_specification <- "fixed_original_sample_risk_set_shares"

missing_salience_vars <- setdiff(salience_predictors, names(eb_salience))

if (length(missing_salience_vars) > 0) {
  stop(
    "Missing salience-change variables in eb_salience_election_model_input.rds: ",
    paste(missing_salience_vars, collapse = ", ")
  )
}

missing_cpds_out <- setdiff(cpds_control_predictors, names(df_out))
missing_cpds_in  <- setdiff(cpds_control_predictors, names(df_in))

if (length(missing_cpds_out) > 0) {
  stop(
    "Missing CPDS control variables in outward data: ",
    paste(missing_cpds_out, collapse = ", ")
  )
}

if (length(missing_cpds_in) > 0) {
  stop(
    "Missing CPDS control variables in inward data: ",
    paste(missing_cpds_in, collapse = ", ")
  )
}

required_vote_share_vars <- c(
  "elec_id",
  "competitor",
  "vote_share_lag",
  "sd_vote_share_lag"
)

missing_vote_share_vars <- setdiff(required_vote_share_vars, names(parlgov_vote_shares))

if (length(missing_vote_share_vars) > 0) {
  stop(
    "Missing required variables in parlgov_supply_vote_shares.rds: ",
    paste(missing_vote_share_vars, collapse = ", ")
  )
}

eb_salience <- eb_salience %>%
  dplyr::select(
    elec_id,
    dplyr::all_of(salience_predictors)
  ) %>%
  dplyr::distinct(elec_id, .keep_all = TRUE)

df_out <- df_out %>%
  dplyr::select(-dplyr::any_of(salience_predictors)) %>%
  dplyr::left_join(
    eb_salience,
    by = "elec_id"
  )

df_in <- df_in %>%
  dplyr::select(-dplyr::any_of(salience_predictors)) %>%
  dplyr::left_join(
    eb_salience,
    by = "elec_id"
  )

tictoc::toc()

stopifnot(is.data.frame(df_out), nrow(df_out) > 0)
stopifnot(is.data.frame(df_in), nrow(df_in) > 0)

# ------------------------------------------------
# 2b. Prepare lagged Parlgov party-family vote-share controls
# ------------------------------------------------

standardise <- function(x) {
  as.numeric(scale(x))
}

parlgov_vote_share_controls_long_raw <- parlgov_vote_shares %>%
  dplyr::filter(
    competitor %in% c(
      "social_democratic",
      "far_left",
      "green",
      "liberal",
      "con",
      "far_right"
    )
  ) %>%
  dplyr::mutate(
    elec_id = dplyr::case_when(
      stringr::str_detect(elec_id, "^DK-2022-11$") ~ "DNK-2022-11",
      TRUE ~ elec_id
    )
  )

parlgov_mainstream_right <- parlgov_vote_share_controls_long_raw %>%
  dplyr::filter(competitor %in% c("liberal", "con")) %>%
  dplyr::group_by(elec_id) %>%
  dplyr::summarise(
    vote_share_lag = dplyr::if_else(
      all(is.na(vote_share_lag)),
      NA_real_,
      sum(vote_share_lag, na.rm = TRUE)
    ),
    sd_vote_share_lag = dplyr::first(sd_vote_share_lag),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    competitor = "mainstream_right"
  )

parlgov_vote_share_controls_long <- parlgov_vote_share_controls_long_raw %>%
  dplyr::filter(
    competitor %in% c(
      "social_democratic",
      "far_left",
      "green",
      "far_right"
    )
  ) %>%
  dplyr::select(
    elec_id,
    competitor,
    vote_share_lag,
    sd_vote_share_lag
  ) %>%
  dplyr::bind_rows(
    parlgov_mainstream_right %>%
      dplyr::select(
        elec_id,
        competitor,
        vote_share_lag,
        sd_vote_share_lag
      )
  ) %>%
  dplyr::mutate(
    parlgov_alt_family_vote_share_lag = as.numeric(vote_share_lag),
    parlgov_sd_vote_share_lag = as.numeric(sd_vote_share_lag)
  ) %>%
  dplyr::select(
    elec_id,
    competitor,
    parlgov_alt_family_vote_share_lag,
    parlgov_sd_vote_share_lag
  )

parlgov_sd_vote_share_controls <- parlgov_vote_share_controls_long %>%
  dplyr::select(
    elec_id,
    parlgov_sd_vote_share_lag
  ) %>%
  dplyr::distinct(elec_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    parlgov_sd_vote_share_lag_z = standardise(parlgov_sd_vote_share_lag)
  )

parlgov_vote_share_controls_long <- parlgov_vote_share_controls_long %>%
  dplyr::mutate(
    parlgov_alt_family_vote_share_lag_z =
      standardise(parlgov_alt_family_vote_share_lag)
  ) %>%
  dplyr::select(
    elec_id,
    competitor,
    parlgov_alt_family_vote_share_lag,
    parlgov_alt_family_vote_share_lag_z
  )

df_out <- df_out %>%
  dplyr::left_join(
    parlgov_sd_vote_share_controls,
    by = "elec_id"
  )

df_in <- df_in %>%
  dplyr::left_join(
    parlgov_sd_vote_share_controls,
    by = "elec_id"
  )

cat("\nOutward outcome support:\n")
print(df_out %>% dplyr::count(outcome, sort = TRUE), n = Inf)

cat("\nInward outcome support:\n")
print(df_in %>% dplyr::count(outcome, sort = TRUE), n = Inf)

cat("\nMissing salience-change values in outward data:\n")
print(
  df_out %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(salience_predictors),
        ~ sum(is.na(.x))
      )
    ),
  width = Inf
)

cat("\nMissing salience-change values in inward data:\n")
print(
  df_in %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(salience_predictors),
        ~ sum(is.na(.x))
      )
    ),
  width = Inf
)

cat("\nMissing CPDS control values in outward data:\n")
print(
  df_out %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cpds_control_predictors),
        ~ sum(is.na(.x))
      )
    ),
  width = Inf
)

cat("\nMissing CPDS control values in inward data:\n")
print(
  df_in %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cpds_control_predictors),
        ~ sum(is.na(.x))
      )
    ),
  width = Inf
)

cat("\nMissing lagged Parlgov SD vote-share values in outward data:\n")
print(
  df_out %>%
    dplyr::summarise(
      parlgov_sd_vote_share_lag_z = sum(is.na(parlgov_sd_vote_share_lag_z))
    ),
  width = Inf
)

cat("\nMissing lagged Parlgov SD vote-share values in inward data:\n")
print(
  df_in %>%
    dplyr::summarise(
      parlgov_sd_vote_share_lag_z = sum(is.na(parlgov_sd_vote_share_lag_z))
    ),
  width = Inf
)

if ("cpds_match_status" %in% names(df_out)) {
  cat("\nOutward elections by CPDS match status:\n")
  print(
    df_out %>%
      dplyr::distinct(elec_id, cpds_match_status) %>%
      dplyr::count(cpds_match_status, name = "n_elections"),
    n = Inf,
    width = Inf
  )
}

if ("cpds_match_status" %in% names(df_in)) {
  cat("\nInward elections by CPDS match status:\n")
  print(
    df_in %>%
      dplyr::distinct(elec_id, cpds_match_status) %>%
      dplyr::count(cpds_match_status, name = "n_elections"),
    n = Inf,
    width = Inf
  )
}

# ------------------------------------------------
# 3. Predictor and model settings
# ------------------------------------------------

predictor_specs <- tibble::tibble(
  predictor = c(
    "eb_immigration_move_tminus1_to_t_z",
    "eb_environment_climate_move_tminus1_to_t_z",
    "eb_unemployment_move_tminus1_to_t_z"
  ),
  predictor_label = c(
    "Change in immigration salience",
    "Change in environmental salience",
    "Change in unemployment salience"
  ),
  file_stub = c(
    "immigration_salience_change",
    "environmental_salience_change",
    "unemployment_salience_change"
  )
)

control_specs <- tibble::tibble(
  predictor = c(
    cpds_control_predictors,
    vote_share_control_predictors
  ),
  predictor_label = c(
    "Turnout",
    "Left incumbency",
    "Effective number of electoral parties",
    "Gallagher disproportionality",
    "Lagged real GDP growth",
    "Lagged unemployment",
    "Lagged government outlays",
    "Lagged trade openness",
    "Lagged union density",
    "Lagged post-fisc Gini",
    "Lagged social-democratic vote share",
    "Lagged alternative-family vote share"
  ),
  file_stub = c(
    "turnout",
    "left_incumbency",
    "effective_number_parties",
    "gallagher_disproportionality",
    "lagged_real_gdp_growth",
    "lagged_unemployment",
    "lagged_government_outlays",
    "lagged_trade_openness",
    "lagged_union_density",
    "lagged_postfisc_gini",
    "lagged_social_democratic_vote_share",
    "lagged_alternative_family_vote_share"
  )
)

all_predictor_specs <- dplyr::bind_rows(
  predictor_specs %>%
    dplyr::mutate(predictor_type = "salience_predictor"),
  control_specs %>%
    dplyr::mutate(predictor_type = "control")
)

flow_specs <- tibble::tibble(
  flow = c("outward", "inward"),
  flow_label = c("Outward switching", "Inward switching"),
  reference_alt = c("retention", "not_to_sd")
)

outward_alt_levels <- c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)

inward_alt_levels <- c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)

alt_label_map <- c(
  retention = "Retention",
  to_far_left = "To far left",
  to_green = "To green",
  to_mainstream_right = "To mainstream right",
  to_far_right = "To far right",
  to_non = "To non-voting",
  not_to_sd = "Not to SD",
  from_far_left = "From far left",
  from_green = "From green",
  from_mainstream_right = "From mainstream right",
  from_far_right = "From far right",
  from_non = "From non-voting"
)

alt_competitor_map <- tibble::tibble(
  alt = c(
    "retention",
    "to_far_left",
    "to_green",
    "to_mainstream_right",
    "to_far_right",
    "to_non",
    "not_to_sd",
    "from_far_left",
    "from_green",
    "from_mainstream_right",
    "from_far_right",
    "from_non"
  ),
  competitor = c(
    "social_democratic",
    "far_left",
    "green",
    "mainstream_right",
    "far_right",
    NA_character_,
    "social_democratic",
    "far_left",
    "green",
    "mainstream_right",
    "far_right",
    NA_character_
  )
)

net_alt_map <- tibble::tibble(
  actor = c("far_left", "green", "mainstream_right", "far_right", "non"),
  actor_label = c("Far left", "Green", "Mainstream right", "Far right", "Non-voting"),
  outward_alt = c("to_far_left", "to_green", "to_mainstream_right", "to_far_right", "to_non"),
  inward_alt = c("from_far_left", "from_green", "from_mainstream_right", "from_far_right", "from_non")
)

required_id_vars <- c(
  "iso2c_file",
  "elec_id",
  "outcome",
  "weights",
  "gender",
  "age_group"
)

missing_id_out <- setdiff(required_id_vars, names(df_out))
missing_id_in  <- setdiff(required_id_vars, names(df_in))

if (length(missing_id_out) > 0) {
  stop("Missing required variables in outward data: ", paste(missing_id_out, collapse = ", "))
}

if (length(missing_id_in) > 0) {
  stop("Missing required variables in inward data: ", paste(missing_id_in, collapse = ", "))
}

# ------------------------------------------------
# 4. Helpers
# ------------------------------------------------

add_row_id <- function(df) {
  if ("respondent_election_id" %in% names(df)) {
    df %>%
      dplyr::mutate(choice_id = as.character(respondent_election_id))
  } else if (all(c("respondent_id", "elec_id") %in% names(df))) {
    df %>%
      dplyr::mutate(
        choice_id = paste(iso2c_file, elec_id, respondent_id, sep = "__")
      )
  } else {
    df %>%
      dplyr::mutate(
        choice_id = paste(iso2c_file, elec_id, dplyr::row_number(), sep = "__")
      )
  }
}

get_available_alternatives <- function(df, alt_levels) {
  df %>%
    dplyr::filter(outcome %in% alt_levels) %>%
    dplyr::distinct(iso2c_file, elec_id, outcome) %>%
    dplyr::rename(alt = outcome) %>%
    dplyr::mutate(alt = as.character(alt))
}

make_block_formula <- function(model_predictors) {
  stats::as.formula(
    paste(
      "cbind(chosen, choice_set_id) ~",
      paste(
        c(
          "alt",
          paste0("alt:", model_predictors),
          "alt:gender",
          "alt:age_group"
        ),
        collapse = " + "
      )
    )
  )
}

make_prediction_formula <- function(model_predictors) {
  stats::as.formula(
    paste(
      "~",
      paste(
        c(
          "alt",
          paste0("alt:", model_predictors),
          "alt:gender",
          "alt:age_group"
        ),
        collapse = " + "
      )
    )
  )
}

detect_predictor_in_term <- function(term, predictors) {
  hits <- predictors[stringr::str_detect(term, stringr::fixed(predictors))]
  if (length(hits) == 0) {
    NA_character_
  } else {
    hits[1]
  }
}

prepare_restricted_choice_data <- function(
    df,
    salience_predictors,
    cpds_control_predictors,
    vote_share_control_predictors,
    choice_level_predictors,
    alternative_level_predictors,
    model_predictors,
    parlgov_vote_share_controls_long,
    flow,
    flow_label,
    alt_levels,
    reference_alt
) {
  
  cat("\nPreparing restricted-choice-set data\n")
  cat("Flow:", flow_label, "\n")
  cat("Salience predictors:", paste(salience_predictors, collapse = ", "), "\n")
  cat("CPDS controls:", paste(cpds_control_predictors, collapse = ", "), "\n")
  cat("Vote-share controls:", paste(vote_share_control_predictors, collapse = ", "), "\n")
  cat("Reference alternative:", reference_alt, "\n")
  
  alt_levels_model <- c(setdiff(alt_levels, reference_alt), reference_alt)
  
  df_base <- df %>%
    add_row_id() %>%
    dplyr::filter(outcome %in% alt_levels) %>%
    dplyr::mutate(
      outcome = as.character(outcome),
      dplyr::across(dplyr::all_of(choice_level_predictors), as.numeric),
      gender = as.numeric(gender),
      age_group = factor(age_group, levels = c("18-34", "35-54", "55+")),
      weights = dplyr::if_else(is.na(weights), 1, as.numeric(weights)),
      country_id = factor(iso2c_file),
      election_id = factor(paste(iso2c_file, elec_id, sep = "__")),
      iso2c_file = as.character(iso2c_file),
      elec_id = as.character(elec_id)
    ) %>%
    dplyr::filter(
      !is.na(outcome),
      !is.na(choice_id),
      !is.na(country_id),
      !is.na(election_id),
      !dplyr::if_any(dplyr::all_of(choice_level_predictors), is.na),
      !is.na(gender),
      !is.na(age_group),
      !is.na(weights),
      weights > 0
    ) %>%
    droplevels()
  
  available_alts <- get_available_alternatives(
    df = df_base,
    alt_levels = alt_levels
  )
  
  df_long <- df_base %>%
    dplyr::select(
      choice_id,
      iso2c_file,
      elec_id,
      country_id,
      election_id,
      outcome,
      dplyr::all_of(choice_level_predictors),
      gender,
      age_group,
      weights,
      dplyr::any_of(c(
        "cpds_match_status",
        "cpds_multiple_election_year_warning"
      ))
    ) %>%
    dplyr::left_join(
      available_alts,
      by = c("iso2c_file", "elec_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      alt = factor(as.character(alt), levels = alt_levels_model),
      outcome = factor(as.character(outcome), levels = alt_levels_model),
      chosen = as.integer(outcome == alt),
      alt_label = dplyr::recode(as.character(alt), !!!alt_label_map),
      reference_alt = reference_alt,
      flow = flow,
      flow_label = flow_label,
      model_block = "salience_change_controls"
    ) %>%
    dplyr::left_join(
      alt_competitor_map,
      by = c("alt" = "alt")
    ) %>%
    dplyr::left_join(
      parlgov_vote_share_controls_long,
      by = c("elec_id", "competitor")
    ) %>%
    dplyr::mutate(
      parlgov_alt_family_vote_share_lag_z = dplyr::case_when(
        as.character(alt) %in% c("to_non", "from_non") ~ 0,
        TRUE ~ parlgov_alt_family_vote_share_lag_z
      )
    ) %>%
    dplyr::filter(
      !dplyr::if_any(dplyr::all_of(alternative_level_predictors), is.na)
    ) %>%
    dplyr::group_by(choice_id) %>%
    dplyr::filter(sum(chosen, na.rm = TRUE) == 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      choice_set_id = as.integer(factor(choice_id)),
      chosen = as.numeric(chosen)
    ) %>%
    droplevels()
  
  choice_set_summary <- df_long %>%
    dplyr::group_by(choice_id) %>%
    dplyr::summarise(
      n_available_alternatives = dplyr::n_distinct(alt),
      chosen_n = sum(chosen),
      .groups = "drop"
    )
  
  if (any(choice_set_summary$chosen_n != 1)) {
    stop("At least one choice set does not have exactly one chosen alternative.")
  }
  
  predictor_summary <- df_long %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(model_predictors),
        list(
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ stats::sd(.x, na.rm = TRUE)
        )
      )
    )
  
  diagnostics <- tibble::tibble(
    model_block = "salience_change_controls",
    flow = flow,
    flow_label = flow_label,
    n_long_rows = nrow(df_long),
    n_respondent_elections = dplyr::n_distinct(df_long$choice_id),
    n_elections = dplyr::n_distinct(df_long$election_id),
    n_countries = dplyr::n_distinct(df_long$country_id),
    mean_available_alternatives = mean(choice_set_summary$n_available_alternatives),
    min_available_alternatives = min(choice_set_summary$n_available_alternatives),
    max_available_alternatives = max(choice_set_summary$n_available_alternatives),
    weighted_n_choices = sum(df_long %>% dplyr::distinct(choice_id, weights) %>% dplyr::pull(weights), na.rm = TRUE),
    reference_alt = reference_alt,
    internal_reference_last = TRUE,
    salience_specification = salience_specification,
    cpds_control_specification = cpds_control_specification,
    vote_share_control_specification = vote_share_control_specification
  ) %>%
    dplyr::bind_cols(predictor_summary)
  
  cat("\nPrepared data diagnostics:\n")
  print(diagnostics, width = Inf)
  
  cat("\nInternal alternative order:\n")
  print(levels(df_long$alt))
  
  if ("cpds_match_status" %in% names(df_long)) {
    cat("\nPrepared data by CPDS match status:\n")
    print(
      df_long %>%
        dplyr::distinct(choice_id, cpds_match_status) %>%
        dplyr::count(cpds_match_status, name = "n_choice_sets"),
      n = Inf,
      width = Inf
    )
  }
  
  list(
    data = df_long,
    diagnostics = diagnostics,
    available_alts = available_alts,
    alt_levels_model = alt_levels_model
  )
}

fit_restricted_choice_mixed_clogit <- function(
    df_long,
    salience_predictors,
    cpds_control_predictors,
    vote_share_control_predictors,
    model_predictors,
    flow,
    flow_label,
    reference_alt
) {
  
  cat("\n================================================\n")
  cat("Fitting joint salience-change restricted-choice-set mixed conditional logit with controls\n")
  cat("Flow:", flow_label, "\n")
  cat("Salience predictors:", paste(salience_predictors, collapse = ", "), "\n")
  cat("CPDS controls:", paste(cpds_control_predictors, collapse = ", "), "\n")
  cat("Vote-share controls:", paste(vote_share_control_predictors, collapse = ", "), "\n")
  cat("Reference alternative:", reference_alt, "\n")
  cat("Random effects: alternative-specific country intercepts\n")
  cat("================================================\n")
  
  model_formula <- make_block_formula(model_predictors)
  
  tictoc::tic(paste(flow, "salience_change_controls", sep = "__"))
  
  fit <- mclogit::mclogit(
    model_formula,
    data = df_long,
    weights = weights,
    random = ~ 0 + alt | country_id,
    method = "PQL"
  )
  
  elapsed <- tictoc::toc(quiet = TRUE)
  
  cat("\nFinished model\n")
  cat("Elapsed seconds:", round(elapsed$toc - elapsed$tic, 2), "\n")
  
  coef_raw <- as.data.frame(summary(fit)$coefficients)
  
  coef_table <- coef_raw %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble()
  
  if ("Estimate" %in% names(coef_table)) {
    coef_table <- coef_table %>% dplyr::rename(estimate = Estimate)
  }
  
  if ("Std. Error" %in% names(coef_table)) {
    coef_table <- coef_table %>% dplyr::rename(std.error = `Std. Error`)
  }
  
  if ("z value" %in% names(coef_table)) {
    coef_table <- coef_table %>% dplyr::rename(statistic = `z value`)
  }
  
  if ("Pr(>|z|)" %in% names(coef_table)) {
    coef_table <- coef_table %>% dplyr::rename(p.value = `Pr(>|z|)`)
  }
  
  if (!"conf.low" %in% names(coef_table) &&
      all(c("estimate", "std.error") %in% names(coef_table))) {
    coef_table <- coef_table %>%
      dplyr::mutate(
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
  }
  
  coef_table <- coef_table %>%
    dplyr::mutate(
      predictor = purrr::map_chr(
        term,
        detect_predictor_in_term,
        predictors = all_predictor_specs$predictor
      )
    ) %>%
    dplyr::left_join(
      all_predictor_specs,
      by = "predictor"
    ) %>%
    dplyr::mutate(
      model_block = "salience_change_controls",
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = "restricted_choice_set_mixed_conditional_logit_country_re",
      random_effect = "alternative_specific_country_intercepts",
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification,
      .before = 1
    )
  
  diagnostics <- tibble::tibble(
    model_block = "salience_change_controls",
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt,
    n_long_rows = nrow(df_long),
    n_respondent_elections = dplyr::n_distinct(df_long$choice_id),
    n_elections = dplyr::n_distinct(df_long$election_id),
    n_countries = dplyr::n_distinct(df_long$country_id),
    weighted_n_rows = sum(df_long$weights, na.rm = TRUE),
    elapsed_seconds = elapsed$toc - elapsed$tic,
    model = "restricted_choice_set_mixed_conditional_logit_country_re",
    random_effect = "alternative_specific_country_intercepts",
    salience_specification = salience_specification,
    cpds_control_specification = cpds_control_specification,
    vote_share_control_specification = vote_share_control_specification
  )
  
  cat("\nCoefficient table, salience-change terms:\n")
  print(
    coef_table %>%
      dplyr::filter(predictor %in% salience_predictors),
    n = Inf,
    width = Inf
  )
  
  cat("\nCoefficient table, control terms:\n")
  print(
    coef_table %>%
      dplyr::filter(predictor %in% c(cpds_control_predictors, vote_share_control_predictors)),
    n = Inf,
    width = Inf
  )
  
  list(
    fit = fit,
    coefficients = coef_table,
    diagnostics = diagnostics
  )
}

make_prediction_matrix <- function(fit, df_long, model_predictors) {
  beta_names <- names(stats::coef(fit))
  
  X <- stats::model.matrix(
    make_prediction_formula(model_predictors),
    data = df_long
  )
  
  missing_cols <- setdiff(beta_names, colnames(X))
  
  if (length(missing_cols) > 0) {
    missing_matrix <- matrix(
      0,
      nrow = nrow(X),
      ncol = length(missing_cols),
      dimnames = list(NULL, missing_cols)
    )
    X <- cbind(X, missing_matrix)
  }
  
  X[, beta_names, drop = FALSE]
}

make_positive_definite <- function(V, eps = 1e-8) {
  V <- as.matrix(V)
  V <- (V + t(V)) / 2
  eigen_decomp <- eigen(V, symmetric = TRUE)
  eigen_decomp$values[eigen_decomp$values < eps] <- eps
  V_pd <- eigen_decomp$vectors %*%
    diag(eigen_decomp$values, nrow = length(eigen_decomp$values)) %*%
    t(eigen_decomp$vectors)
  dimnames(V_pd) <- dimnames(V)
  V_pd
}

compute_choice_probabilities_from_matrix <- function(X, beta, choice_id) {
  beta <- beta[colnames(X)]
  eta <- as.numeric(X %*% beta)
  
  eta_centered <- eta - ave(
    eta,
    choice_id,
    FUN = function(x) max(x, na.rm = TRUE)
  )
  
  exp_eta <- exp(eta_centered)
  
  exp_eta / ave(
    exp_eta,
    choice_id,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
}

compute_salience_ames_for_predictor_from_beta <- function(
    beta,
    fit,
    df_long,
    model_predictors,
    predictor,
    predictor_label,
    file_stub
) {
  df0 <- df_long
  df1 <- df_long
  df1[[predictor]] <- df1[[predictor]] + 1
  
  X0 <- make_prediction_matrix(
    fit = fit,
    df_long = df0,
    model_predictors = model_predictors
  )
  
  X1 <- make_prediction_matrix(
    fit = fit,
    df_long = df1,
    model_predictors = model_predictors
  )
  
  p0 <- compute_choice_probabilities_from_matrix(
    X = X0,
    beta = beta,
    choice_id = df0$choice_id
  )
  
  p1 <- compute_choice_probabilities_from_matrix(
    X = X1,
    beta = beta,
    choice_id = df1$choice_id
  )
  
  df_long %>%
    dplyr::mutate(diff = p1 - p0) %>%
    dplyr::group_by(alt) %>%
    dplyr::summarise(
      estimate = stats::weighted.mean(diff, w = weights, na.rm = TRUE),
      n_choice_rows = dplyr::n(),
      weighted_n = sum(weights, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub,
      component = "salience_change",
      .before = 1
    )
}

compute_ame_delta_se_for_predictor <- function(
    fit,
    df_long,
    model_predictors,
    predictor,
    predictor_label,
    file_stub,
    delta_step = 1e-5
) {
  beta_hat <- stats::coef(fit)
  V <- stats::vcov(fit)
  
  common_names <- intersect(names(beta_hat), colnames(V))
  
  if (length(common_names) == 0) {
    stop("No overlap between coefficient names and vcov column names.")
  }
  
  beta_hat <- beta_hat[common_names]
  V <- V[common_names, common_names, drop = FALSE]
  V <- make_positive_definite(V)
  
  point <- compute_salience_ames_for_predictor_from_beta(
    beta = beta_hat,
    fit = fit,
    df_long = df_long,
    model_predictors = model_predictors,
    predictor = predictor,
    predictor_label = predictor_label,
    file_stub = file_stub
  )
  
  alt_levels_now <- as.character(point$alt)
  
  gradient <- matrix(
    NA_real_,
    nrow = length(alt_levels_now),
    ncol = length(beta_hat),
    dimnames = list(alt_levels_now, names(beta_hat))
  )
  
  for (k in seq_along(beta_hat)) {
    h <- delta_step * max(abs(beta_hat[k]), 1)
    
    beta_plus <- beta_hat
    beta_minus <- beta_hat
    
    beta_plus[k] <- beta_plus[k] + h
    beta_minus[k] <- beta_minus[k] - h
    
    ame_plus <- compute_salience_ames_for_predictor_from_beta(
      beta = beta_plus,
      fit = fit,
      df_long = df_long,
      model_predictors = model_predictors,
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub
    ) %>%
      dplyr::select(alt, estimate_plus = estimate)
    
    ame_minus <- compute_salience_ames_for_predictor_from_beta(
      beta = beta_minus,
      fit = fit,
      df_long = df_long,
      model_predictors = model_predictors,
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub
    ) %>%
      dplyr::select(alt, estimate_minus = estimate)
    
    grad_now <- point %>%
      dplyr::select(alt) %>%
      dplyr::left_join(ame_plus, by = "alt") %>%
      dplyr::left_join(ame_minus, by = "alt") %>%
      dplyr::mutate(gradient = (estimate_plus - estimate_minus) / (2 * h))
    
    gradient[as.character(grad_now$alt), k] <- grad_now$gradient
  }
  
  se <- purrr::map_dbl(
    seq_len(nrow(gradient)),
    function(r) {
      g <- gradient[r, ]
      sqrt(as.numeric(t(g) %*% V %*% g))
    }
  )
  
  point %>%
    dplyr::mutate(
      std.error = se,
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(-abs(statistic)),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      uncertainty = "direct_numerical_delta_method"
    )
}

compute_salience_ames_for_predictor <- function(model_obj, df_long, predictor, predictor_label, file_stub) {
  fit <- model_obj$fit
  
  if (isTRUE(run_delta_method_se)) {
    compute_ame_delta_se_for_predictor(
      fit = fit,
      df_long = df_long,
      model_predictors = model_predictors,
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub,
      delta_step = delta_step
    )
  } else {
    beta_hat <- stats::coef(fit)
    
    compute_salience_ames_for_predictor_from_beta(
      beta = beta_hat,
      fit = fit,
      df_long = df_long,
      model_predictors = model_predictors,
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub
    ) %>%
      dplyr::mutate(
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        uncertainty = "not_calculated"
      )
  }
}

compute_salience_ames_all_predictors <- function(model_obj, df_long) {
  purrr::pmap_dfr(
    predictor_specs,
    function(predictor, predictor_label, file_stub) {
      compute_salience_ames_for_predictor(
        model_obj = model_obj,
        df_long = df_long,
        predictor = predictor,
        predictor_label = predictor_label,
        file_stub = file_stub
      )
    }
  )
}

get_weighted_risk_set_n <- function(df_long) {
  df_long %>%
    dplyr::distinct(choice_id, weights) %>%
    dplyr::summarise(
      weighted_n = sum(weights, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::pull(weighted_n)
}

compute_risk_set_shares <- function(df_out_long, df_in_long) {
  
  n_sd <- get_weighted_risk_set_n(df_out_long)
  n_non_sd <- get_weighted_risk_set_n(df_in_long)
  n_total <- n_sd + n_non_sd
  
  tibble::tibble(
    s_sd = n_sd / n_total,
    s_non_sd = n_non_sd / n_total,
    weighted_n_sd = n_sd,
    weighted_n_non_sd = n_non_sd,
    weighted_n_total = n_total
  )
}

compute_net_effects_from_ames <- function(
    outward_ames,
    inward_ames,
    shares,
    source
) {
  join_vars <- c(
    "predictor",
    "predictor_label",
    "file_stub",
    "actor",
    "actor_label"
  )
  
  outward_clean <- outward_ames %>%
    dplyr::mutate(outward_alt = as.character(alt)) %>%
    dplyr::filter(outward_alt %in% net_alt_map$outward_alt) %>%
    dplyr::left_join(net_alt_map, by = "outward_alt") %>%
    dplyr::select(
      predictor,
      predictor_label,
      file_stub,
      actor,
      actor_label,
      outward_alt,
      outward_ame = estimate,
      outward_se = std.error
    )
  
  inward_clean <- inward_ames %>%
    dplyr::mutate(inward_alt = as.character(alt)) %>%
    dplyr::filter(inward_alt %in% net_alt_map$inward_alt) %>%
    dplyr::left_join(net_alt_map, by = "inward_alt") %>%
    dplyr::select(
      predictor,
      predictor_label,
      file_stub,
      actor,
      actor_label,
      inward_alt,
      inward_ame = estimate,
      inward_se = std.error
    )
  
  outward_clean %>%
    dplyr::inner_join(
      inward_clean,
      by = join_vars
    ) %>%
    dplyr::mutate(
      flow = "net",
      flow_label = "Net effect",
      s_sd = shares$s_sd,
      s_non_sd = shares$s_non_sd,
      weighted_n_sd = shares$weighted_n_sd,
      weighted_n_non_sd = shares$weighted_n_non_sd,
      weighted_n_total = shares$weighted_n_total,
      estimate = s_non_sd * inward_ame - s_sd * outward_ame,
      std.error = sqrt(
        (s_non_sd^2 * inward_se^2) +
          (s_sd^2 * outward_se^2)
      ),
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(-abs(statistic)),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      uncertainty = dplyr::if_else(
        isTRUE(run_delta_method_se),
        "ame_delta_se_independent_net_combination",
        "not_calculated"
      ),
      effect_type = "net_effect",
      source = source,
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification,
      net_share_specification = net_share_specification,
      .before = 1
    )
}

run_block_model <- function(flow, flow_label, reference_alt) {
  
  if (!flow %in% c("outward", "inward")) {
    stop("Unknown flow: ", flow)
  }
  
  df_now <- if (flow == "outward") df_out else df_in
  alt_levels_now <- if (flow == "outward") outward_alt_levels else inward_alt_levels
  
  prepared <- prepare_restricted_choice_data(
    df = df_now,
    salience_predictors = salience_predictors,
    cpds_control_predictors = cpds_control_predictors,
    vote_share_control_predictors = vote_share_control_predictors,
    choice_level_predictors = choice_level_predictors,
    alternative_level_predictors = alternative_level_predictors,
    model_predictors = model_predictors,
    parlgov_vote_share_controls_long = parlgov_vote_share_controls_long,
    flow = flow,
    flow_label = flow_label,
    alt_levels = alt_levels_now,
    reference_alt = reference_alt
  )
  
  model_obj <- fit_restricted_choice_mixed_clogit(
    df_long = prepared$data,
    salience_predictors = salience_predictors,
    cpds_control_predictors = cpds_control_predictors,
    vote_share_control_predictors = vote_share_control_predictors,
    model_predictors = model_predictors,
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt
  )
  
  ames <- compute_salience_ames_all_predictors(
    model_obj = model_obj,
    df_long = prepared$data
  ) %>%
    dplyr::mutate(
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = "restricted_choice_set_mixed_conditional_logit_country_re",
      random_effect = "alternative_specific_country_intercepts",
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification,
      .before = 1
    )
  
  cat("\nAverage marginal effects of a one-standard-deviation increase in each salience change, conditional on other salience changes and controls:\n")
  print(ames, n = Inf, width = Inf)
  
  saveRDS(
    list(
      model_block = "salience_change_controls",
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = model_obj,
      data = prepared$data,
      data_diagnostics = prepared$diagnostics,
      available_alternatives = prepared$available_alts,
      alt_levels_model = prepared$alt_levels_model,
      ames = ames,
      salience_predictors = salience_predictors,
      cpds_control_predictors = cpds_control_predictors,
      vote_share_control_predictors = vote_share_control_predictors,
      choice_level_predictors = choice_level_predictors,
      alternative_level_predictors = alternative_level_predictors,
      model_predictors = model_predictors,
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification
    ),
    file.path(output_dir, paste0("model_salience_change_controls_", flow, ".rds"))
  )
  
  list(
    model_block = "salience_change_controls",
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt,
    model = model_obj,
    data = prepared$data,
    data_diagnostics = prepared$diagnostics,
    coefficients = model_obj$coefficients,
    diagnostics = model_obj$diagnostics,
    available_alternatives = prepared$available_alts,
    alt_levels_model = prepared$alt_levels_model,
    ames = ames
  )
}

# ------------------------------------------------
# 5. Run outward and inward block models
# ------------------------------------------------

tictoc::tic("Run joint salience-change restricted-choice-set mixed conditional logit models with controls")

all_results <- list()

all_results[["salience_change_controls__outward"]] <- run_block_model(
  flow = "outward",
  flow_label = "Outward switching",
  reference_alt = "retention"
)

all_results[["salience_change_controls__inward"]] <- run_block_model(
  flow = "inward",
  flow_label = "Inward switching",
  reference_alt = "not_to_sd"
)

tictoc::toc()

# ------------------------------------------------
# 6. Combine and save model outputs
# ------------------------------------------------

combined_data_diagnostics <- purrr::map_dfr(
  all_results,
  "data_diagnostics",
  .id = "model_id"
)

combined_model_diagnostics <- purrr::map_dfr(
  all_results,
  "diagnostics",
  .id = "model_id"
)

combined_coefficients <- purrr::map_dfr(
  all_results,
  "coefficients",
  .id = "model_id"
)

combined_ames <- purrr::map_dfr(
  all_results,
  "ames",
  .id = "model_id"
)

combined_available_alternatives <- purrr::imap_dfr(
  all_results,
  function(x, nm) {
    x$available_alternatives %>%
      dplyr::mutate(
        model_id = nm,
        model_block = "salience_change_controls",
        flow = x$flow,
        flow_label = x$flow_label,
        .before = 1
      )
  }
)

shares_point <- compute_risk_set_shares(
  df_out_long = all_results[["salience_change_controls__outward"]]$data,
  df_in_long = all_results[["salience_change_controls__inward"]]$data
)

point_net_effects <- compute_net_effects_from_ames(
  outward_ames = all_results[["salience_change_controls__outward"]]$ames,
  inward_ames = all_results[["salience_change_controls__inward"]]$ames,
  shares = shares_point,
  source = "point_estimate"
)

cat("\n================================================\n")
cat("Combined AMEs\n")
cat("================================================\n")
print(combined_ames, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Combined salience-change coefficients\n")
cat("================================================\n")
print(
  combined_coefficients %>%
    dplyr::filter(predictor %in% salience_predictors),
  n = Inf,
  width = Inf
)

cat("\n================================================\n")
cat("Combined control coefficients\n")
cat("================================================\n")
print(
  combined_coefficients %>%
    dplyr::filter(predictor %in% c(cpds_control_predictors, vote_share_control_predictors)),
  n = Inf,
  width = Inf
)

cat("\n================================================\n")
cat("Point-estimate net effects\n")
cat("================================================\n")
print(point_net_effects, n = Inf, width = Inf)

readr::write_csv(
  combined_data_diagnostics,
  file.path(output_dir, "restricted_choice_set_data_diagnostics.csv")
)

readr::write_csv(
  combined_model_diagnostics,
  file.path(output_dir, "restricted_choice_set_model_diagnostics.csv")
)

readr::write_csv(
  combined_coefficients,
  file.path(output_dir, "restricted_choice_set_coefficients.csv")
)

readr::write_csv(
  combined_ames,
  file.path(output_dir, "restricted_choice_set_ames.csv")
)

readr::write_csv(
  combined_available_alternatives,
  file.path(output_dir, "restricted_choice_set_available_alternatives.csv")
)

readr::write_csv(
  point_net_effects,
  file.path(output_dir, "restricted_choice_set_net_effects_point_estimates.csv")
)

saveRDS(
  point_net_effects,
  file.path(output_dir, "restricted_choice_set_net_effects_point_estimates.rds")
)

saveRDS(
  list(
    predictor_specs = predictor_specs,
    control_specs = control_specs,
    all_predictor_specs = all_predictor_specs,
    flow_specs = flow_specs,
    outward_alt_levels = outward_alt_levels,
    inward_alt_levels = inward_alt_levels,
    alt_competitor_map = alt_competitor_map,
    net_alt_map = net_alt_map,
    salience_predictors = salience_predictors,
    cpds_control_predictors = cpds_control_predictors,
    vote_share_control_predictors = vote_share_control_predictors,
    choice_level_predictors = choice_level_predictors,
    alternative_level_predictors = alternative_level_predictors,
    model_predictors = model_predictors,
    salience_specification = salience_specification,
    cpds_control_specification = cpds_control_specification,
    vote_share_control_specification = vote_share_control_specification,
    net_share_specification = net_share_specification,
    all_results = all_results,
    combined_data_diagnostics = combined_data_diagnostics,
    combined_model_diagnostics = combined_model_diagnostics,
    combined_coefficients = combined_coefficients,
    combined_ames_point = combined_ames,
    point_net_effects = point_net_effects,
    risk_set_shares = shares_point
  ),
  file.path(output_dir, "all_restricted_choice_set_mixed_conditional_logit_country_re_results.rds")
)

# ------------------------------------------------
# 7. Direct delta-method uncertainty outputs
# ------------------------------------------------

ames_with_delta_ci <- combined_ames
net_effects_with_delta_ci <- point_net_effects

cat("\n================================================\n")
cat("AMEs with direct delta-method uncertainty\n")
cat("================================================\n")
print(ames_with_delta_ci, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Net effects with propagated AME uncertainty\n")
cat("================================================\n")
print(net_effects_with_delta_ci, n = Inf, width = Inf)

readr::write_csv(
  ames_with_delta_ci,
  file.path(output_dir, "restricted_choice_set_ames_with_delta_ci.csv")
)

readr::write_csv(
  net_effects_with_delta_ci,
  file.path(output_dir, "restricted_choice_set_net_effects_with_delta_ci.csv")
)

saveRDS(
  ames_with_delta_ci,
  file.path(output_dir, "restricted_choice_set_ames_with_delta_ci.rds")
)

saveRDS(
  net_effects_with_delta_ci,
  file.path(output_dir, "restricted_choice_set_net_effects_with_delta_ci.rds")
)

final_results <- list(
  predictor_specs = predictor_specs,
  control_specs = control_specs,
  all_predictor_specs = all_predictor_specs,
  flow_specs = flow_specs,
  outward_alt_levels = outward_alt_levels,
  inward_alt_levels = inward_alt_levels,
  alt_competitor_map = alt_competitor_map,
  net_alt_map = net_alt_map,
  salience_predictors = salience_predictors,
  cpds_control_predictors = cpds_control_predictors,
  vote_share_control_predictors = vote_share_control_predictors,
  choice_level_predictors = choice_level_predictors,
  alternative_level_predictors = alternative_level_predictors,
  model_predictors = model_predictors,
  salience_specification = salience_specification,
  cpds_control_specification = cpds_control_specification,
  vote_share_control_specification = vote_share_control_specification,
  net_share_specification = net_share_specification,
  all_results = all_results,
  combined_data_diagnostics = combined_data_diagnostics,
  combined_model_diagnostics = combined_model_diagnostics,
  combined_coefficients = combined_coefficients,
  combined_ames_point = combined_ames,
  point_net_effects = point_net_effects,
  risk_set_shares = shares_point,
  delta_method_se_was_run = isTRUE(run_delta_method_se),
  delta_step = delta_step,
  ames_with_delta_ci = ames_with_delta_ci,
  net_effects_with_delta_ci = net_effects_with_delta_ci
)

saveRDS(
  final_results,
  file.path(output_dir, "final_salience_change_controls_model_results.rds")
)

cat("\nControlled salience model outputs saved to:\n")
cat(output_dir, "\n")

# ------------------------------------------------
# 8. Final estimation-sample composition
# ------------------------------------------------

summarise_estimation_sample <- function(result_obj) {
  
  df_long <- result_obj$data
  
  respondent_level <- df_long %>%
    dplyr::distinct(
      choice_id,
      iso2c_file,
      elec_id,
      outcome,
      weights,
      dplyr::any_of(c(
        "cpds_match_status",
        "cpds_multiple_election_year_warning"
      ))
    )
  
  sample_summary <- respondent_level %>%
    dplyr::summarise(
      model_block = result_obj$model_block,
      flow = result_obj$flow,
      flow_label = result_obj$flow_label,
      n_respondents = dplyr::n_distinct(choice_id),
      n_respondent_alternative_combinations = nrow(df_long),
      n_elections = dplyr::n_distinct(elec_id),
      n_countries = dplyr::n_distinct(iso2c_file),
      mean_available_alternatives =
        n_respondent_alternative_combinations / n_respondents,
      weighted_n_respondents = sum(weights, na.rm = TRUE),
      reference_alt = result_obj$reference_alt,
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification
    )
  
  outcome_summary <- respondent_level %>%
    dplyr::count(outcome, name = "n_respondents") %>%
    dplyr::mutate(
      model_block = result_obj$model_block,
      flow = result_obj$flow,
      flow_label = result_obj$flow_label,
      share = n_respondents / sum(n_respondents),
      salience_specification = salience_specification,
      cpds_control_specification = cpds_control_specification,
      vote_share_control_specification = vote_share_control_specification,
      .before = 1
    )
  
  cpds_match_summary <- if ("cpds_match_status" %in% names(respondent_level)) {
    respondent_level %>%
      dplyr::count(cpds_match_status, name = "n_respondents") %>%
      dplyr::mutate(
        model_block = result_obj$model_block,
        flow = result_obj$flow,
        flow_label = result_obj$flow_label,
        share = n_respondents / sum(n_respondents),
        .before = 1
      )
  } else {
    tibble::tibble()
  }
  
  list(
    sample_summary = sample_summary,
    outcome_summary = outcome_summary,
    cpds_match_summary = cpds_match_summary
  )
}

estimation_sample_objects <- purrr::map(
  all_results,
  summarise_estimation_sample
)

estimation_sample_summary <- purrr::map_dfr(
  estimation_sample_objects,
  "sample_summary",
  .id = "model_id"
)

estimation_outcome_summary <- purrr::map_dfr(
  estimation_sample_objects,
  "outcome_summary",
  .id = "model_id"
)

estimation_cpds_match_summary <- purrr::map_dfr(
  estimation_sample_objects,
  "cpds_match_summary",
  .id = "model_id"
)

cat("\n================================================\n")
cat("Final estimation-sample summary\n")
cat("================================================\n")
print(estimation_sample_summary, width = Inf)

cat("\n================================================\n")
cat("Final estimation-sample outcome composition\n")
cat("================================================\n")
print(estimation_outcome_summary, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Final estimation-sample CPDS match composition\n")
cat("================================================\n")
print(estimation_cpds_match_summary, n = Inf, width = Inf)

readr::write_csv(
  estimation_sample_summary,
  file.path(output_dir, "estimation_sample_summary.csv")
)

readr::write_csv(
  estimation_outcome_summary,
  file.path(output_dir, "estimation_outcome_summary.csv")
)

readr::write_csv(
  estimation_cpds_match_summary,
  file.path(output_dir, "estimation_cpds_match_summary.csv")
)

saveRDS(
  estimation_sample_summary,
  file.path(output_dir, "estimation_sample_summary.rds")
)

saveRDS(
  estimation_outcome_summary,
  file.path(output_dir, "estimation_outcome_summary.rds")
)

saveRDS(
  estimation_cpds_match_summary,
  file.path(output_dir, "estimation_cpds_match_summary.rds")
)

cat("\nScript completed successfully.\n")