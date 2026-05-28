# ================================================================
# 01_build_analysis_data.R
# Merge dependent-variable datasets with updated supply-side
# position indicators and demand-side explanatory variables
#
# Social-democratic vote-switching project
#
# Final output:
#   1) df_analysis_inward_social_democratic.rds
#      Respondent-level inward SD analysis data
#
#   2) df_analysis_outward_social_democratic.rds
#      Respondent-level outward SD analysis data
#
#   3) df_analysis_inward_social_democratic_long_choice_set.rds
#      Long inward SD choice-set data for election-specific softmax
#
#   4) df_analysis_outward_social_democratic_long_choice_set.rds
#      Long outward SD choice-set data for election-specific softmax
#
#   5) choice_set_social_democratic_analysis.rds
#      Election-alternative choice-set metadata used in this script
#
# Inputs:
#   data/analysis/df_inward_social_democratic_multinom_analysis.rds
#   data/analysis/df_outward_social_democratic_multinom_analysis.rds
#   data/processed/ess_cultural_context_election_left.rds
#   data/processed/ess_cultural_context_election_right.rds
#   data/processed/eb_salience_election_model_input.rds
#   data/processed/choice_set_social_democratic.rds
# ================================================================

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(lubridate)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching"

analysis_dir <- file.path(project_dir, "data", "analysis")
processed_dir <- file.path(project_dir, "data", "processed")

output_dir <- file.path(analysis_dir, "building_analysis_data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_inward_supply <- file.path(
  analysis_dir,
  "df_inward_social_democratic_multinom_analysis.rds"
)

path_outward_supply <- file.path(
  analysis_dir,
  "df_outward_social_democratic_multinom_analysis.rds"
)

path_ess_left <- file.path(
  processed_dir,
  "ess_cultural_context_election_left.rds"
)

path_ess_right <- file.path(
  processed_dir,
  "ess_cultural_context_election_right.rds"
)

path_eb_salience <- file.path(
  processed_dir,
  "eb_salience_election_model_input.rds"
)

path_choice_set <- file.path(
  processed_dir,
  "choice_set_social_democratic.rds"
)

# ------------------------------------------------
# 2. Helper functions
# ------------------------------------------------

scale_z <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric((x - m) / s)
}

print_sample_window <- function(df, name) {
  cat("\nSample window:", name, "\n")
  
  df %>%
    dplyr::summarise(
      n_transitions = dplyr::n(),
      n_elections = dplyr::n_distinct(elec_id),
      n_countries = dplyr::n_distinct(iso2c_file),
      first_year = suppressWarnings(min(year, na.rm = TRUE)),
      last_year = suppressWarnings(max(year, na.rm = TRUE))
    ) %>%
    print(width = Inf)
}

coverage_sample_summary <- function(df, condition, name) {
  df %>%
    dplyr::filter({{ condition }}) %>%
    dplyr::summarise(
      sample = name,
      n_transitions = dplyr::n(),
      n_elections = dplyr::n_distinct(elec_id),
      n_countries = dplyr::n_distinct(iso2c_file),
      first_year = suppressWarnings(min(year, na.rm = TRUE)),
      last_year = suppressWarnings(max(year, na.rm = TRUE))
    )
}

prefix_except_keys <- function(df, prefix, keys = "elec_id") {
  df %>%
    dplyr::rename_with(
      .fn = ~ paste0(prefix, .x),
      .cols = -dplyr::all_of(keys)
    )
}

check_unique_key <- function(df, key, name) {
  dup <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(key))) %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup) > 0) {
    cat("\nDuplicate keys in", name, ":\n")
    print(dup, n = Inf, width = Inf)
    stop(name, " contains duplicate keys.")
  }
}

check_one_row_per_respondent_election <- function(df, name) {
  dup <- df %>%
    dplyr::count(iso2c_file, elec_id, id) %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup) > 0) {
    cat("\nDuplicate respondent-election rows in", name, ":\n")
    print(dup, n = Inf, width = Inf)
    stop(name, " is not unique by iso2c_file, elec_id, id.")
  }
}

# ------------------------------------------------
# 3. Load supply-side analysis datasets
# ------------------------------------------------

df_inward_sd <- readRDS(path_inward_supply)
df_outward_sd <- readRDS(path_outward_supply)

stopifnot(is.data.frame(df_inward_sd), nrow(df_inward_sd) > 0)
stopifnot(is.data.frame(df_outward_sd), nrow(df_outward_sd) > 0)

cat("\nSupply-side datasets loaded\n")
print_sample_window(df_inward_sd, "inward social-democratic, supply file")
print_sample_window(df_outward_sd, "outward social-democratic, supply file")

# ------------------------------------------------
# 4. Standardise core identifiers
# ------------------------------------------------

df_inward_sd <- df_inward_sd %>%
  dplyr::mutate(
    year = as.integer(year),
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    id = as.character(id),
    origin_bloc_detailed = as.character(origin_bloc_detailed),
    destination_bloc_detailed = as.character(destination_bloc_detailed)
  )

df_outward_sd <- df_outward_sd %>%
  dplyr::mutate(
    year = as.integer(year),
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    id = as.character(id),
    origin_bloc_detailed = as.character(origin_bloc_detailed),
    destination_bloc_detailed = as.character(destination_bloc_detailed)
  )

check_one_row_per_respondent_election(df_inward_sd, "df_inward_sd")
check_one_row_per_respondent_election(df_outward_sd, "df_outward_sd")

# ------------------------------------------------
# 5. Reconstruct final inward and outward outcomes
# ------------------------------------------------

df_inward_sd <- df_inward_sd %>%
  dplyr::mutate(
    inward_sd_outcome = dplyr::case_when(
      destination_bloc_detailed != "social_democratic" ~ "not_to_sd",
      origin_bloc_detailed == "far_left" &
        destination_bloc_detailed == "social_democratic" ~ "from_far_left",
      origin_bloc_detailed == "green" &
        destination_bloc_detailed == "social_democratic" ~ "from_green",
      origin_bloc_detailed == "mainstream_right" &
        destination_bloc_detailed == "social_democratic" ~ "from_mainstream_right",
      origin_bloc_detailed == "far_right" &
        destination_bloc_detailed == "social_democratic" ~ "from_far_right",
      origin_bloc_detailed == "non" &
        destination_bloc_detailed == "social_democratic" ~ "from_non",
      TRUE ~ NA_character_
    ),
    inward_sd_outcome = factor(
      inward_sd_outcome,
      levels = c(
        "not_to_sd",
        "from_far_left",
        "from_green",
        "from_mainstream_right",
        "from_far_right",
        "from_non"
      )
    )
  )

df_outward_sd <- df_outward_sd %>%
  dplyr::mutate(
    outward_sd_outcome = dplyr::case_when(
      destination_bloc_detailed == "social_democratic" ~ "retention",
      destination_bloc_detailed == "far_left" ~ "to_far_left",
      destination_bloc_detailed == "green" ~ "to_green",
      destination_bloc_detailed == "mainstream_right" ~ "to_mainstream_right",
      destination_bloc_detailed == "far_right" ~ "to_far_right",
      destination_bloc_detailed == "non" ~ "to_non",
      TRUE ~ NA_character_
    ),
    outward_sd_outcome = factor(
      outward_sd_outcome,
      levels = c(
        "retention",
        "to_far_left",
        "to_green",
        "to_mainstream_right",
        "to_far_right",
        "to_non"
      )
    )
  )

# ------------------------------------------------
# 6. Keep original outcome but add final outcome alias
# ------------------------------------------------

df_inward_sd <- df_inward_sd %>%
  dplyr::rename(
    source_script_outcome = outcome
  ) %>%
  dplyr::mutate(
    outcome = inward_sd_outcome
  )

df_outward_sd <- df_outward_sd %>%
  dplyr::rename(
    source_script_outcome = outcome
  ) %>%
  dplyr::mutate(
    outcome = outward_sd_outcome
  )

# ------------------------------------------------
# 7. Validate final outcome construction
# ------------------------------------------------

if (any(is.na(df_inward_sd$inward_sd_outcome))) {
  cat("\nRows with missing inward outcome:\n")
  df_inward_sd %>%
    dplyr::filter(is.na(inward_sd_outcome)) %>%
    dplyr::count(origin_bloc_detailed, destination_bloc_detailed) %>%
    print(n = Inf)
  
  stop("Inward outcome contains missing values after recoding.")
}

if (any(is.na(df_outward_sd$outward_sd_outcome))) {
  cat("\nRows with missing outward outcome:\n")
  df_outward_sd %>%
    dplyr::filter(is.na(outward_sd_outcome)) %>%
    dplyr::count(origin_bloc_detailed, destination_bloc_detailed) %>%
    print(n = Inf)
  
  stop("Outward outcome contains missing values after recoding.")
}

stopifnot(all(df_inward_sd$origin_bloc_detailed != "social_democratic"))
stopifnot(all(df_outward_sd$origin_bloc_detailed == "social_democratic"))

stopifnot(all(levels(df_inward_sd$inward_sd_outcome) == c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)))

stopifnot(all(levels(df_outward_sd$outward_sd_outcome) == c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)))

cat("\nFinal inward outcome support before demand-side merge:\n")
df_inward_sd %>%
  dplyr::count(inward_sd_outcome, sort = TRUE) %>%
  print(n = Inf)

cat("\nFinal outward outcome support before demand-side merge:\n")
df_outward_sd %>%
  dplyr::count(outward_sd_outcome, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------
# 8. Load demand-side contextual data and choice-set metadata
# ------------------------------------------------

ess_left_raw <- readRDS(path_ess_left)
ess_right_raw <- readRDS(path_ess_right)
eb_salience_raw <- readRDS(path_eb_salience)
choice_set_social_democratic <- readRDS(path_choice_set)

stopifnot(is.data.frame(ess_left_raw), nrow(ess_left_raw) > 0)
stopifnot(is.data.frame(ess_right_raw), nrow(ess_right_raw) > 0)
stopifnot(is.data.frame(eb_salience_raw), nrow(eb_salience_raw) > 0)
stopifnot(is.data.frame(choice_set_social_democratic), nrow(choice_set_social_democratic) > 0)

choice_set_social_democratic <- choice_set_social_democratic %>%
  dplyr::mutate(
    year = as.integer(year),
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    model_direction = as.character(model_direction),
    alternative = as.character(alternative),
    alternative_available = as.logical(alternative_available),
    alternative_id = as.integer(alternative_id)
  )

check_unique_key(
  choice_set_social_democratic,
  c("iso2c_file", "elec_id", "year", "model_direction", "alternative"),
  "Social-democratic choice set"
)

cat("\nDemand-side datasets and choice-set metadata loaded\n")

# ------------------------------------------------
# 9. Prepare ESS demand-side cultural context
# ------------------------------------------------

ess_left <- ess_left_raw %>%
  dplyr::mutate(
    elec_id = as.character(elec_id)
  ) %>%
  dplyr::select(
    -dplyr::any_of(c("cntry"))
  ) %>%
  dplyr::rename(
    ess_left_country = country,
    ess_left_year = year,
    ess_left_month = month,
    ess_left_election_date = election_date,
    ess_left_essround = essround,
    ess_left_fieldwork_start = fieldwork_start,
    ess_left_fieldwork_end = fieldwork_end,
    ess_left_match_id = ess_match_id,
    ess_left_n_resp = n_resp,
    ess_left_gal_tan_mean = gal_tan_mean,
    ess_left_gal_tan_median = gal_tan_median,
    ess_left_gal_tan_sd = gal_tan_sd,
    ess_left_gal_tan_mad = gal_tan_mad,
    ess_left_gal_tan_pol = gal_tan_pol,
    ess_left_gal_tan_mean_lag = gal_tan_mean_lag,
    ess_left_gal_tan_median_lag = gal_tan_median_lag,
    ess_left_gal_tan_sd_lag = gal_tan_sd_lag,
    ess_left_gal_tan_mad_lag = gal_tan_mad_lag,
    ess_left_gal_tan_pol_lag = gal_tan_pol_lag,
    ess_left_gal_tan_mean_shift = gal_tan_mean_shift,
    ess_left_gal_tan_median_shift = gal_tan_median_shift,
    ess_left_gal_tan_sd_shift = gal_tan_sd_shift,
    ess_left_gal_tan_mad_shift = gal_tan_mad_shift,
    ess_left_gal_tan_pol_shift = gal_tan_pol_shift,
    ess_left_gal_tan_mean_z = gal_tan_mean_z,
    ess_left_gal_tan_median_z = gal_tan_median_z,
    ess_left_gal_tan_sd_z = gal_tan_sd_z,
    ess_left_gal_tan_mad_z = gal_tan_mad_z,
    ess_left_gal_tan_pol_z = gal_tan_pol_z,
    ess_left_gal_tan_mean_shift_z = gal_tan_mean_shift_z,
    ess_left_gal_tan_median_shift_z = gal_tan_median_shift_z,
    ess_left_gal_tan_sd_shift_z = gal_tan_sd_shift_z,
    ess_left_gal_tan_mad_shift_z = gal_tan_mad_shift_z,
    ess_left_gal_tan_pol_shift_z = gal_tan_pol_shift_z
  )

ess_right <- ess_right_raw %>%
  dplyr::mutate(
    elec_id = as.character(elec_id)
  ) %>%
  dplyr::select(
    -dplyr::any_of(c("cntry"))
  ) %>%
  dplyr::rename(
    ess_right_country = country,
    ess_right_year = year,
    ess_right_month = month,
    ess_right_election_date = election_date,
    ess_right_essround = essround,
    ess_right_fieldwork_start = fieldwork_start,
    ess_right_fieldwork_end = fieldwork_end,
    ess_right_match_id = ess_match_id,
    ess_right_n_resp = n_resp,
    ess_right_gal_tan_mean = gal_tan_mean,
    ess_right_gal_tan_median = gal_tan_median,
    ess_right_gal_tan_sd = gal_tan_sd,
    ess_right_gal_tan_mad = gal_tan_mad,
    ess_right_gal_tan_pol = gal_tan_pol,
    ess_right_gal_tan_mean_lag = gal_tan_mean_lag,
    ess_right_gal_tan_median_lag = gal_tan_median_lag,
    ess_right_gal_tan_sd_lag = gal_tan_sd_lag,
    ess_right_gal_tan_mad_lag = gal_tan_mad_lag,
    ess_right_gal_tan_pol_lag = gal_tan_pol_lag,
    ess_right_gal_tan_mean_shift = gal_tan_mean_shift,
    ess_right_gal_tan_median_shift = gal_tan_median_shift,
    ess_right_gal_tan_sd_shift = gal_tan_sd_shift,
    ess_right_gal_tan_mad_shift = gal_tan_mad_shift,
    ess_right_gal_tan_pol_shift = gal_tan_pol_shift,
    ess_right_gal_tan_mean_z = gal_tan_mean_z,
    ess_right_gal_tan_median_z = gal_tan_median_z,
    ess_right_gal_tan_sd_z = gal_tan_sd_z,
    ess_right_gal_tan_mad_z = gal_tan_mad_z,
    ess_right_gal_tan_pol_z = gal_tan_pol_z,
    ess_right_gal_tan_mean_shift_z = gal_tan_mean_shift_z,
    ess_right_gal_tan_median_shift_z = gal_tan_median_shift_z,
    ess_right_gal_tan_sd_shift_z = gal_tan_sd_shift_z,
    ess_right_gal_tan_mad_shift_z = gal_tan_mad_shift_z,
    ess_right_gal_tan_pol_shift_z = gal_tan_pol_shift_z
  )

check_unique_key(ess_left, "elec_id", "ESS left election context")
check_unique_key(ess_right, "elec_id", "ESS right election context")

# ------------------------------------------------
# 10. Prepare Eurobarometer demand-side salience context
# ------------------------------------------------
# Note: 01_prepare_demand_salience.R already prefixes all Eurobarometer
# variables with eb_, except elec_id, country, and election_date. Do not
# prefix the variables again here.

eb_salience <- eb_salience_raw %>%
  dplyr::mutate(
    elec_id = as.character(elec_id)
  ) %>%
  dplyr::rename(
    eb_country = country,
    eb_election_date = election_date
  )

check_unique_key(eb_salience, "elec_id", "Eurobarometer salience election context")

# ------------------------------------------------
# 11. Merge demand-side measures into final respondent-level datasets
# ------------------------------------------------

df_analysis_inward_social_democratic <- df_inward_sd %>%
  dplyr::left_join(
    ess_left,
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    ess_right,
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    eb_salience,
    by = "elec_id"
  ) %>%
  dplyr::mutate(
    model_direction = "inward_social_democratic",
    inward_sd_outcome_id = as.integer(inward_sd_outcome) - 1L
  )

df_analysis_outward_social_democratic <- df_outward_sd %>%
  dplyr::left_join(
    ess_left,
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    ess_right,
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    eb_salience,
    by = "elec_id"
  ) %>%
  dplyr::mutate(
    model_direction = "outward_social_democratic",
    outward_sd_outcome_id = as.integer(outward_sd_outcome) - 1L
  )

# ------------------------------------------------
# 12. Construct long choice-set analysis datasets
# ------------------------------------------------

choice_set_inward <- choice_set_social_democratic %>%
  dplyr::filter(model_direction == "inward_social_democratic") %>%
  dplyr::select(
    iso2c_file,
    elec_id,
    choice_set_year = year,
    model_direction,
    alternative,
    alternative_id,
    alternative_available
  ) %>%
  dplyr::mutate(
    choice_set_year_from_elec_id = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
    choice_set_year = dplyr::coalesce(
      as.integer(choice_set_year),
      choice_set_year_from_elec_id
    ),
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    model_direction = as.character(model_direction),
    alternative = as.character(alternative),
    alternative_id = as.integer(alternative_id),
    alternative_available = as.logical(alternative_available)
  )

choice_set_outward <- choice_set_social_democratic %>%
  dplyr::filter(model_direction == "outward_social_democratic") %>%
  dplyr::select(
    iso2c_file,
    elec_id,
    choice_set_year = year,
    model_direction,
    alternative,
    alternative_id,
    alternative_available
  ) %>%
  dplyr::mutate(
    choice_set_year_from_elec_id = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
    choice_set_year = dplyr::coalesce(
      as.integer(choice_set_year),
      choice_set_year_from_elec_id
    ),
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    model_direction = as.character(model_direction),
    alternative = as.character(alternative),
    alternative_id = as.integer(alternative_id),
    alternative_available = as.logical(alternative_available)
  )

expected_inward_alternatives <- c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)

expected_outward_alternatives <- c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)

stopifnot(all(sort(unique(choice_set_inward$alternative)) == sort(expected_inward_alternatives)))
stopifnot(all(sort(unique(choice_set_outward$alternative)) == sort(expected_outward_alternatives)))

check_unique_key(
  choice_set_inward,
  c("iso2c_file", "elec_id", "model_direction", "alternative"),
  "Inward social-democratic choice set"
)

check_unique_key(
  choice_set_outward,
  c("iso2c_file", "elec_id", "model_direction", "alternative"),
  "Outward social-democratic choice set"
)

missing_choice_set_inward <- df_analysis_inward_social_democratic %>%
  dplyr::distinct(iso2c_file, elec_id, model_direction) %>%
  dplyr::anti_join(
    choice_set_inward %>%
      dplyr::distinct(iso2c_file, elec_id, model_direction),
    by = c("iso2c_file", "elec_id", "model_direction")
  )

missing_choice_set_outward <- df_analysis_outward_social_democratic %>%
  dplyr::distinct(iso2c_file, elec_id, model_direction) %>%
  dplyr::anti_join(
    choice_set_outward %>%
      dplyr::distinct(iso2c_file, elec_id, model_direction),
    by = c("iso2c_file", "elec_id", "model_direction")
  )

cat("\nInward elections missing choice-set metadata:\n")
print(missing_choice_set_inward, n = Inf, width = Inf)

cat("\nOutward elections missing choice-set metadata:\n")
print(missing_choice_set_outward, n = Inf, width = Inf)

stopifnot(nrow(missing_choice_set_inward) == 0)
stopifnot(nrow(missing_choice_set_outward) == 0)

df_analysis_inward_social_democratic_long_choice_set <- df_analysis_inward_social_democratic %>%
  dplyr::mutate(
    analysis_year_from_elec_id = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
    analysis_year_for_check = dplyr::coalesce(
      as.integer(year),
      analysis_year_from_elec_id
    )
  ) %>%
  dplyr::left_join(
    choice_set_inward,
    by = c("iso2c_file", "elec_id", "model_direction"),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    choice_set_year_matches = analysis_year_for_check == choice_set_year,
    chosen = as.character(inward_sd_outcome) == as.character(alternative),
    chosen_int = as.integer(chosen),
    outcome_available = dplyr::case_when(
      as.character(inward_sd_outcome) == "not_to_sd" ~ TRUE,
      as.character(inward_sd_outcome) == "from_far_left" ~ available_far_left,
      as.character(inward_sd_outcome) == "from_green" ~ available_green,
      as.character(inward_sd_outcome) == "from_mainstream_right" ~ available_mainstream_right,
      as.character(inward_sd_outcome) == "from_far_right" ~ available_far_right,
      as.character(inward_sd_outcome) == "from_non" ~ available_non,
      TRUE ~ NA
    )
  ) %>%
  dplyr::filter(alternative_available == TRUE)

df_analysis_outward_social_democratic_long_choice_set <- df_analysis_outward_social_democratic %>%
  dplyr::mutate(
    analysis_year_from_elec_id = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
    analysis_year_for_check = dplyr::coalesce(
      as.integer(year),
      analysis_year_from_elec_id
    )
  ) %>%
  dplyr::left_join(
    choice_set_outward,
    by = c("iso2c_file", "elec_id", "model_direction"),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    choice_set_year_matches = analysis_year_for_check == choice_set_year,
    chosen = as.character(outward_sd_outcome) == as.character(alternative),
    chosen_int = as.integer(chosen),
    outcome_available = dplyr::case_when(
      as.character(outward_sd_outcome) == "retention" ~ TRUE,
      as.character(outward_sd_outcome) == "to_far_left" ~ available_far_left,
      as.character(outward_sd_outcome) == "to_green" ~ available_green,
      as.character(outward_sd_outcome) == "to_mainstream_right" ~ available_mainstream_right,
      as.character(outward_sd_outcome) == "to_far_right" ~ available_far_right,
      as.character(outward_sd_outcome) == "to_non" ~ available_non,
      TRUE ~ NA
    )
  ) %>%
  dplyr::filter(alternative_available == TRUE)

cat("\nChoice-set year consistency, inward:\n")
df_analysis_inward_social_democratic_long_choice_set %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    analysis_year_for_check,
    choice_set_year,
    choice_set_year_matches
  ) %>%
  dplyr::count(choice_set_year_matches) %>%
  print(n = Inf, width = Inf)

cat("\nChoice-set year consistency, outward:\n")
df_analysis_outward_social_democratic_long_choice_set %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    analysis_year_for_check,
    choice_set_year,
    choice_set_year_matches
  ) %>%
  dplyr::count(choice_set_year_matches) %>%
  print(n = Inf, width = Inf)

bad_choice_set_year_inward <- df_analysis_inward_social_democratic_long_choice_set %>%
  dplyr::filter(is.na(choice_set_year_matches) | choice_set_year_matches == FALSE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    analysis_year_for_check,
    choice_set_year,
    choice_set_year_matches
  ) %>%
  dplyr::arrange(iso2c_file, elec_id)

bad_choice_set_year_outward <- df_analysis_outward_social_democratic_long_choice_set %>%
  dplyr::filter(is.na(choice_set_year_matches) | choice_set_year_matches == FALSE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    analysis_year_for_check,
    choice_set_year,
    choice_set_year_matches
  ) %>%
  dplyr::arrange(iso2c_file, elec_id)

if (nrow(bad_choice_set_year_inward) > 0) {
  cat("\nInward choice-set year mismatches:\n")
  print(bad_choice_set_year_inward, n = Inf, width = Inf)
  stop("Inward choice-set year mismatch.")
}

if (nrow(bad_choice_set_year_outward) > 0) {
  cat("\nOutward choice-set year mismatches:\n")
  print(bad_choice_set_year_outward, n = Inf, width = Inf)
  stop("Outward choice-set year mismatch.")
}

cat("\nLong choice-set data constructed successfully\n")

# ------------------------------------------------
# 13. Choice-set validation
# ------------------------------------------------

cat("\nChoice-set size distribution, inward:\n")
df_analysis_inward_social_democratic_long_choice_set %>%
  dplyr::distinct(source_file, iso2c_file, elec_id, year, id, alternative) %>%
  dplyr::count(source_file, iso2c_file, elec_id, year, id, name = "n_available_alternatives") %>%
  dplyr::count(n_available_alternatives) %>%
  print(n = Inf, width = Inf)

cat("\nChoice-set size distribution, outward:\n")
df_analysis_outward_social_democratic_long_choice_set %>%
  dplyr::distinct(source_file, iso2c_file, elec_id, year, id, alternative) %>%
  dplyr::count(source_file, iso2c_file, elec_id, year, id, name = "n_available_alternatives") %>%
  dplyr::count(n_available_alternatives) %>%
  print(n = Inf, width = Inf)

inward_choice_validation <- df_analysis_inward_social_democratic_long_choice_set %>%
  dplyr::group_by(source_file, iso2c_file, elec_id, id) %>%
  dplyr::summarise(
    n_chosen = sum(chosen, na.rm = TRUE),
    n_available_alternatives = dplyr::n(),
    outcome_available = dplyr::first(outcome_available),
    .groups = "drop"
  )

outward_choice_validation <- df_analysis_outward_social_democratic_long_choice_set %>%
  dplyr::group_by(source_file, iso2c_file, elec_id, id) %>%
  dplyr::summarise(
    n_chosen = sum(chosen, na.rm = TRUE),
    n_available_alternatives = dplyr::n(),
    outcome_available = dplyr::first(outcome_available),
    .groups = "drop"
  )

cat("\nInward long choice-set validation:\n")
inward_choice_validation %>%
  dplyr::count(n_chosen, outcome_available) %>%
  print(n = Inf, width = Inf)

cat("\nOutward long choice-set validation:\n")
outward_choice_validation %>%
  dplyr::count(n_chosen, outcome_available) %>%
  print(n = Inf, width = Inf)

bad_inward_choice_rows <- inward_choice_validation %>%
  dplyr::filter(n_chosen != 1 | outcome_available != TRUE)

bad_outward_choice_rows <- outward_choice_validation %>%
  dplyr::filter(n_chosen != 1 | outcome_available != TRUE)

if (nrow(bad_inward_choice_rows) > 0) {
  cat("\nInward respondent-elections with invalid long choice-set structure:\n")
  print(bad_inward_choice_rows, n = Inf, width = Inf)
  stop("Invalid inward long choice-set structure.")
}

if (nrow(bad_outward_choice_rows) > 0) {
  cat("\nOutward respondent-elections with invalid long choice-set structure:\n")
  print(bad_outward_choice_rows, n = Inf, width = Inf)
  stop("Invalid outward long choice-set structure.")
}

stopifnot(
  nrow(inward_choice_validation) ==
    df_analysis_inward_social_democratic %>%
    dplyr::distinct(source_file, iso2c_file, elec_id, id) %>%
    nrow()
)

stopifnot(
  nrow(outward_choice_validation) ==
    df_analysis_outward_social_democratic %>%
    dplyr::distinct(source_file, iso2c_file, elec_id, id) %>%
    nrow()
)

cat("\nAvailable alternatives by model direction and alternative:\n")
choice_set_social_democratic %>%
  dplyr::count(model_direction, alternative, alternative_available) %>%
  dplyr::arrange(model_direction, alternative, alternative_available) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 14. Merge diagnostics
# ------------------------------------------------

cat("\nInward sample after full merge:\n")
print_sample_window(df_analysis_inward_social_democratic, "inward social-democratic")

cat("\nOutward sample after full merge:\n")
print_sample_window(df_analysis_outward_social_democratic, "outward social-democratic")

cat("\nInward outcome support after full merge:\n")
df_analysis_inward_social_democratic %>%
  dplyr::count(inward_sd_outcome, sort = TRUE) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nOutward outcome support after full merge:\n")
df_analysis_outward_social_democratic %>%
  dplyr::count(outward_sd_outcome, sort = TRUE) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nSupply-side coverage, inward:\n")
df_analysis_inward_social_democratic %>%
  dplyr::summarise(
    n = dplyr::n(),
    sd_investmentconsumption_nonmissing = sum(!is.na(sd_investmentconsumption_std)),
    sd_stateconomy_nonmissing = sum(!is.na(sd_stateconomy_std)),
    sd_libcons_nonmissing = sum(!is.na(sd_libcons_std)),
    sd_investmentconsumption_move_nonmissing = sum(!is.na(sd_investmentconsumption_move_std)),
    sd_stateconomy_move_nonmissing = sum(!is.na(sd_stateconomy_move_std)),
    sd_libcons_move_nonmissing = sum(!is.na(sd_libcons_move_std)),
    enp_nonmissing = sum(!is.na(enp_z)),
    basic_controls_nonmissing = sum(!is.na(gender) & !is.na(age_group)),
    supplementary_controls_nonmissing = sum(!is.na(lrself_z) & !is.na(satdem_z))
  ) %>%
  print(width = Inf)

cat("\nSupply-side coverage, outward:\n")
df_analysis_outward_social_democratic %>%
  dplyr::summarise(
    n = dplyr::n(),
    sd_investmentconsumption_nonmissing = sum(!is.na(sd_investmentconsumption_std)),
    sd_stateconomy_nonmissing = sum(!is.na(sd_stateconomy_std)),
    sd_libcons_nonmissing = sum(!is.na(sd_libcons_std)),
    sd_investmentconsumption_move_nonmissing = sum(!is.na(sd_investmentconsumption_move_std)),
    sd_stateconomy_move_nonmissing = sum(!is.na(sd_stateconomy_move_std)),
    sd_libcons_move_nonmissing = sum(!is.na(sd_libcons_move_std)),
    enp_nonmissing = sum(!is.na(enp_z)),
    basic_controls_nonmissing = sum(!is.na(gender) & !is.na(age_group)),
    supplementary_controls_nonmissing = sum(!is.na(lrself_z) & !is.na(satdem_z))
  ) %>%
  print(width = Inf)

cat("\nESS and Eurobarometer merge coverage, inward:\n")
df_analysis_inward_social_democratic %>%
  dplyr::summarise(
    n = dplyr::n(),
    ess_left_nonmissing = sum(!is.na(ess_left_gal_tan_mean_z)),
    ess_right_nonmissing = sum(!is.na(ess_right_gal_tan_mean_z)),
    eb_immigration_nonmissing = sum(!is.na(eb_immigration)),
    eb_unemployment_nonmissing = sum(!is.na(eb_unemployment)),
    eb_environment_nonmissing = sum(!is.na(eb_environment_climate)),
    eb_immigration_change_nonmissing = sum(!is.na(eb_immigration_move_tminus1_to_t_z)),
    eb_unemployment_change_nonmissing = sum(!is.na(eb_unemployment_move_tminus1_to_t_z)),
    eb_environment_change_nonmissing = sum(!is.na(eb_environment_climate_move_tminus1_to_t_z))
  ) %>%
  print(width = Inf)

cat("\nESS and Eurobarometer merge coverage, outward:\n")
df_analysis_outward_social_democratic %>%
  dplyr::summarise(
    n = dplyr::n(),
    ess_left_nonmissing = sum(!is.na(ess_left_gal_tan_mean_z)),
    ess_right_nonmissing = sum(!is.na(ess_right_gal_tan_mean_z)),
    eb_immigration_nonmissing = sum(!is.na(eb_immigration)),
    eb_unemployment_nonmissing = sum(!is.na(eb_unemployment)),
    eb_environment_nonmissing = sum(!is.na(eb_environment_climate)),
    eb_immigration_change_nonmissing = sum(!is.na(eb_immigration_move_tminus1_to_t_z)),
    eb_unemployment_change_nonmissing = sum(!is.na(eb_unemployment_move_tminus1_to_t_z)),
    eb_environment_change_nonmissing = sum(!is.na(eb_environment_climate_move_tminus1_to_t_z))
  ) %>%
  print(width = Inf)

cat("\nStructural party-system availability, inward:\n")
df_analysis_inward_social_democratic %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  dplyr::count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

cat("\nStructural party-system availability, outward:\n")
df_analysis_outward_social_democratic %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  dplyr::count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 15. Demand-side unmatched election diagnostics
# ------------------------------------------------

make_unmatched_demand_diagnostic <- function(df) {
  df %>%
    dplyr::group_by(elec_id) %>%
    dplyr::summarise(
      iso2c_file = dplyr::first(stats::na.omit(iso2c_file)),
      year = {
        x <- stats::na.omit(year)
        if (length(x) == 0) NA_integer_ else as.integer(x[1])
      },
      has_ess_left = any(!is.na(ess_left_gal_tan_mean_z)),
      has_ess_right = any(!is.na(ess_right_gal_tan_mean_z)),
      has_eb = any(!is.na(eb_immigration_move_tminus1_to_t_z)),
      .groups = "drop"
    ) %>%
    dplyr::filter(!has_ess_left | !has_ess_right | !has_eb) %>%
    dplyr::arrange(iso2c_file, year, elec_id)
}

unmatched_demand_inward <- make_unmatched_demand_diagnostic(
  df_analysis_inward_social_democratic
)

unmatched_demand_outward <- make_unmatched_demand_diagnostic(
  df_analysis_outward_social_democratic
)

cat("\nInward elections missing ESS-left, ESS-right, or EB context:\n")
print(unmatched_demand_inward, n = Inf, width = Inf)

cat("\nOutward elections missing ESS-left, ESS-right, or EB context:\n")
print(unmatched_demand_outward, n = Inf, width = Inf)

# ------------------------------------------------
# 16. Combined sample-composition diagnostics
# ------------------------------------------------

sd_analysis_all <- dplyr::bind_rows(
  df_analysis_outward_social_democratic %>%
    dplyr::mutate(flow_model = "outward_sd"),
  df_analysis_inward_social_democratic %>%
    dplyr::mutate(flow_model = "inward_sd")
)

sample_composition_summary <- dplyr::bind_rows(
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(outcome),
    "Primary SD transition universe"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_std) &
      !is.na(sd_stateconomy_std) &
      !is.na(sd_libcons_std),
    "MARPOR level position specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std),
    "MARPOR change position specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std) &
      !is.na(enp_z) &
      !is.na(gender) &
      !is.na(age_group),
    "MARPOR change position specification with ENP and basic controls"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std) &
      !is.na(enp_z) &
      !is.na(gender) &
      !is.na(age_group) &
      !is.na(lrself_z) &
      !is.na(satdem_z),
    "Supplementary MARPOR change position specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(ess_left_gal_tan_mean_z),
    "ESS demand-side cultural level specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(ess_left_gal_tan_mean_shift_z) &
      !is.na(ess_left_gal_tan_pol_shift_z),
    "ESS demand-side cultural change/polarization specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(eb_immigration) &
      !is.na(eb_unemployment) &
      !is.na(eb_environment_climate),
    "Eurobarometer issue-salience level specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(eb_immigration_move_tminus1_to_t_z) &
      !is.na(eb_unemployment_move_tminus1_to_t_z) &
      !is.na(eb_environment_climate_move_tminus1_to_t_z),
    "Eurobarometer issue-salience change specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std) &
      !is.na(ess_left_gal_tan_mean_shift_z) &
      !is.na(ess_left_gal_tan_pol_shift_z) &
      !is.na(eb_immigration_move_tminus1_to_t_z) &
      !is.na(eb_unemployment_move_tminus1_to_t_z) &
      !is.na(eb_environment_climate_move_tminus1_to_t_z) &
      !is.na(enp_z) &
      !is.na(gender) &
      !is.na(age_group),
    "Combined main specification with supply, demand, ENP, and basic controls"
  )
)

cat("\nSample-composition summary:\n")
print(sample_composition_summary, width = Inf)

choice_set_sample_summary <- dplyr::bind_rows(
  df_analysis_outward_social_democratic_long_choice_set %>%
    dplyr::mutate(flow_model = "outward_sd"),
  df_analysis_inward_social_democratic_long_choice_set %>%
    dplyr::mutate(flow_model = "inward_sd")
) %>%
  dplyr::group_by(flow_model) %>%
  dplyr::summarise(
    n_long_rows = dplyr::n(),
    n_respondent_elections = dplyr::n_distinct(
      paste(iso2c_file, elec_id, id, sep = "___")
    ),
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(iso2c_file),
    mean_available_alternatives = n_long_rows / n_respondent_elections,
    .groups = "drop"
  )

cat("\nLong choice-set sample summary:\n")
print(choice_set_sample_summary, width = Inf)

# ------------------------------------------------
# 17. Validation checks after full merge
# ------------------------------------------------

stopifnot(nrow(df_analysis_inward_social_democratic) == nrow(df_inward_sd))
stopifnot(nrow(df_analysis_outward_social_democratic) == nrow(df_outward_sd))

stopifnot(all(df_analysis_inward_social_democratic$origin_bloc_detailed != "social_democratic"))
stopifnot(all(df_analysis_outward_social_democratic$origin_bloc_detailed == "social_democratic"))

stopifnot(all(!is.na(df_analysis_inward_social_democratic$inward_sd_outcome)))
stopifnot(all(!is.na(df_analysis_outward_social_democratic$outward_sd_outcome)))

stopifnot(all(levels(df_analysis_inward_social_democratic$inward_sd_outcome) == c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)))

stopifnot(all(levels(df_analysis_outward_social_democratic$outward_sd_outcome) == c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)))

stopifnot(all(!is.na(df_analysis_inward_social_democratic$available_social_democratic)))
stopifnot(all(!is.na(df_analysis_outward_social_democratic$available_social_democratic)))

stopifnot(all(df_analysis_inward_social_democratic$available_social_democratic == TRUE))
stopifnot(all(df_analysis_outward_social_democratic$available_social_democratic == TRUE))

stopifnot(all(df_analysis_inward_social_democratic_long_choice_set$alternative_available == TRUE))
stopifnot(all(df_analysis_outward_social_democratic_long_choice_set$alternative_available == TRUE))

stopifnot(all(df_analysis_inward_social_democratic_long_choice_set$chosen_int %in% c(0L, 1L)))
stopifnot(all(df_analysis_outward_social_democratic_long_choice_set$chosen_int %in% c(0L, 1L)))

# ------------------------------------------------
# 18. Save final analysis datasets and diagnostics
# ------------------------------------------------

saveRDS(
  df_analysis_inward_social_democratic,
  file.path(output_dir, "df_analysis_inward_social_democratic.rds")
)

saveRDS(
  df_analysis_outward_social_democratic,
  file.path(output_dir, "df_analysis_outward_social_democratic.rds")
)

saveRDS(
  df_analysis_inward_social_democratic_long_choice_set,
  file.path(output_dir, "df_analysis_inward_social_democratic_long_choice_set.rds")
)

saveRDS(
  df_analysis_outward_social_democratic_long_choice_set,
  file.path(output_dir, "df_analysis_outward_social_democratic_long_choice_set.rds")
)

saveRDS(
  choice_set_social_democratic,
  file.path(output_dir, "choice_set_social_democratic_analysis.rds")
)

saveRDS(
  sample_composition_summary,
  file.path(output_dir, "sample_composition_summary_social_democratic.rds")
)

saveRDS(
  choice_set_sample_summary,
  file.path(output_dir, "choice_set_sample_summary_social_democratic.rds")
)

saveRDS(
  unmatched_demand_inward,
  file.path(output_dir, "unmatched_demand_inward_social_democratic.rds")
)

saveRDS(
  unmatched_demand_outward,
  file.path(output_dir, "unmatched_demand_outward_social_democratic.rds")
)

cat("\nSaved files:\n")
cat(file.path(output_dir, "df_analysis_inward_social_democratic.rds"), "\n")
cat(file.path(output_dir, "df_analysis_outward_social_democratic.rds"), "\n")
cat(file.path(output_dir, "df_analysis_inward_social_democratic_long_choice_set.rds"), "\n")
cat(file.path(output_dir, "df_analysis_outward_social_democratic_long_choice_set.rds"), "\n")
cat(file.path(output_dir, "choice_set_social_democratic_analysis.rds"), "\n")
cat(file.path(output_dir, "sample_composition_summary_social_democratic.rds"), "\n")
cat(file.path(output_dir, "choice_set_sample_summary_social_democratic.rds"), "\n")
cat(file.path(output_dir, "unmatched_demand_inward_social_democratic.rds"), "\n")
cat(file.path(output_dir, "unmatched_demand_outward_social_democratic.rds"), "\n")

cat("\nScript completed successfully\n")
