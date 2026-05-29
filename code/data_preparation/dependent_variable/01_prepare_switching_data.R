# ================================================================
# 01_prepare_switching_data.R
# Load, classify, validate, and save harmonized master dataset
# Social-democratic vote-switching project
#
# Final outputs:
#   1) data/processed/df_all_classified_social_democratic.rds
#   2) data/processed/df_all_classified_social_democratic.RData
#   3) data/processed/party_system_availability_social_democratic.rds
#
# Purpose:
#   This script prepares the master respondent-party dataset and
#   constructs election-level party-system availability indicators.
#   These indicators allow later multinomial models to account for
#   party-system heterogeneity by distinguishing unavailable party
#   families from available but electorally unused alternatives.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

path_micro <- file.path(
  project_dir,
  "data",
  "micro",
  "all_countries_df_long_valid_both.RData"
)

path_populist <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "external", "The PopuList 3.0.csv")

output_dir <- file.path(project_dir, "data", "processed")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------

load(path_micro)

populist <- read.csv(
  path_populist,
  sep = ";",
  stringsAsFactors = FALSE
)

stopifnot(is.data.frame(df_all), nrow(df_all) > 0)
stopifnot(is.data.frame(populist), nrow(populist) > 0)

cat("\nData loaded successfully\n")
print(dim(df_all))

cat("\nCountries:\n")
print(sort(unique(df_all$iso2c_file)))

cat("\nOverall coverage:\n")
df_all %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_countries = dplyr::n_distinct(iso2c_file),
    n_elections = dplyr::n_distinct(elec_id),
    n_respondent_elections = dplyr::n_distinct(
      paste(iso2c_file, elec_id, id, sep = "___")
    ),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  ) %>%
  print(width = Inf)

# ------------------------------------------------
# 3. Helper functions
# ------------------------------------------------

clean_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\(.*?\\)", "") %>%
    stringr::str_replace_all("[^a-z0-9 ]", " ") %>%
    stringr::str_squish()
}

first_nonmissing <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

first_nonmissing_integer <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) == 0) NA_integer_ else as.integer(x[1])
}

# ------------------------------------------------
# 4. Country and PopuList lookup tables
# ------------------------------------------------

country_lookup <- tibble::tribble(
  ~iso2c_file, ~country_name,
  "AT", "Austria",
  "AU", "Australia",
  "BE", "Belgium",
  "BG", "Bulgaria",
  "CA", "Canada",
  "CH", "Switzerland",
  "CZ", "Czech Republic",
  "DE", "Germany",
  "DK", "Denmark",
  "EE", "Estonia",
  "ES", "Spain",
  "FI", "Finland",
  "FR", "France",
  "GB", "United Kingdom",
  "GR", "Greece",
  "HU", "Hungary",
  "IE", "Ireland",
  "IL", "Israel",
  "IS", "Iceland",
  "IT", "Italy",
  "LT", "Lithuania",
  "LV", "Latvia",
  "NL", "Netherlands",
  "NO", "Norway",
  "NZ", "New Zealand",
  "PL", "Poland",
  "PT", "Portugal",
  "RO", "Romania",
  "SE", "Sweden",
  "SI", "Slovenia",
  "SK", "Slovakia"
)

populist_lookup <- populist %>%
  dplyr::transmute(
    country_name = trimws(country_name),
    party_name_clean = clean_name(party_name_english),
    far_right_populist = farright == 1,
    far_left_populist = farleft == 1
  ) %>%
  dplyr::distinct(country_name, party_name_clean, .keep_all = TRUE)

manual_match <- tibble::tribble(
  ~iso2c_file, ~party_label_best, ~party_name_clean_manual,
  "AT", "Austrian Freedom Party", "freedom party of austria",
  "AT", "Freedom Party of Austria", "freedom party of austria",
  "AT", "Alliance for the Future of Austria", "alliance for the future of austria",
  "AU", "One Nation", "one nation",
  "BE", "Flemish Bloc", "flemish interest",
  "CZ", "Tomio Okamura's Dawn of Direct Democracy", "dawn",
  "DK", "Denmark Democrats - Inger Stojberg", "denmark democrats inger stojberg",
  "DK", "Hard Line", "hard line",
  "FI", "True Finns", "finns party",
  "FR", "National Front", "national front rally",
  "FR", "National Rally", "national front rally",
  "FR", "Debout la Republique", "republic arise france arise",
  "FR", "France Arise", "republic arise france arise",
  "IT", "Casapound Italia", "casapound italia",
  "IT", "Fiamma Tricolore", "social movement tricolour flame",
  "IT", "Lega delle Leghe", "northern league",
  "IT", "Northern League", "northern league",
  "NL", "Party of Freedom", "party for freedom",
  "SE", "New Democracy", "new democracy"
)

# ------------------------------------------------
# 5. Party classification
# ------------------------------------------------

df_all <- df_all %>%
  dplyr::select(
    -dplyr::any_of(c(
      "country_name",
      "party_label_best",
      "party_name_clean",
      "party_name_clean_manual",
      "party_name_clean_match",
      "far_right_populist",
      "far_left_populist",
      "far_right",
      "far_left",
      "mainstream_right",
      "social_democratic",
      "green",
      "left",
      "other_left",
      "non_voter",
      "bloc",
      "party_bloc_detailed",
      "switch_from",
      "switch_to",
      "stay",
      "switch_from_bloc",
      "switch_to_bloc",
      "switch_from_bloc_detailed",
      "switch_to_bloc_detailed"
    ))
  ) %>%
  dplyr::left_join(
    country_lookup,
    by = "iso2c_file"
  ) %>%
  dplyr::mutate(
    party_label_best = dplyr::coalesce(
      as.character(party_name_map),
      as.character(peid),
      as.character(peid_map),
      as.character(partyabbrev_map)
    ),
    party_name_clean = clean_name(party_label_best)
  ) %>%
  dplyr::left_join(
    manual_match,
    by = c("iso2c_file", "party_label_best")
  ) %>%
  dplyr::mutate(
    party_name_clean_match = dplyr::coalesce(
      party_name_clean_manual,
      party_name_clean
    )
  ) %>%
  dplyr::left_join(
    populist_lookup,
    by = c(
      "country_name" = "country_name",
      "party_name_clean_match" = "party_name_clean"
    )
  ) %>%
  dplyr::mutate(
    far_right_populist = dplyr::coalesce(far_right_populist, FALSE),
    far_left_populist = dplyr::coalesce(far_left_populist, FALSE),
    
    far_right = far_right_populist,
    far_left = far_left_populist,
    
    social_democratic = parfam_harmonized == "soc" &
      far_right == FALSE &
      far_left == FALSE,
    
    mainstream_right = parfam_harmonized %in% c("con", "lib", "chr") &
      far_right == FALSE &
      far_left == FALSE,
    
    green = parfam_harmonized %in% c("eco", "gre") &
      far_right == FALSE &
      far_left == FALSE,
    
    left = parfam_harmonized %in% c("lef", "com") &
      far_right == FALSE &
      far_left == FALSE,
    
    other_left = (
      parfam_harmonized %in% c("lef", "com", "eco", "gre") |
        far_left == TRUE
    ) &
      social_democratic == FALSE &
      far_right == FALSE,
    
    non_voter = parfam_final == "non",
    
    party_bloc_detailed = dplyr::case_when(
      far_right ~ "far_right",
      far_left ~ "far_left",
      social_democratic ~ "social_democratic",
      mainstream_right ~ "mainstream_right",
      green ~ "green",
      left ~ "left",
      non_voter ~ "non",
      TRUE ~ "other"
    ),
    
    bloc = dplyr::case_when(
      far_right ~ "far_right",
      social_democratic ~ "social_democratic",
      mainstream_right ~ "mainstream_right",
      other_left ~ "other_left",
      non_voter ~ "non",
      TRUE ~ "other"
    )
  )

cat("\nParty classification completed\n")

# ------------------------------------------------
# 6. Structural party-system availability by election
# ------------------------------------------------

party_system_availability_social_democratic <- df_all %>%
  dplyr::filter(!is.na(elec_id)) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    source_file,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_right,
    far_left,
    social_democratic,
    mainstream_right,
    green,
    left,
    other_left,
    non_voter,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::group_by(iso2c_file, elec_id, year) %>%
  dplyr::summarise(
    source_file = first_nonmissing(source_file),
    
    available_far_left = any(party_bloc_detailed == "far_left", na.rm = TRUE),
    available_green = any(party_bloc_detailed == "green", na.rm = TRUE),
    available_social_democratic = any(
      party_bloc_detailed == "social_democratic",
      na.rm = TRUE
    ),
    available_mainstream_right = any(
      party_bloc_detailed == "mainstream_right",
      na.rm = TRUE
    ),
    available_far_right = any(party_bloc_detailed == "far_right", na.rm = TRUE),
    available_left = any(party_bloc_detailed == "left", na.rm = TRUE),
    available_other = any(party_bloc_detailed == "other", na.rm = TRUE),
    available_non = TRUE,
    
    available_other_left_pooled = any(bloc == "other_left", na.rm = TRUE),
    
    n_party_rows = dplyr::n(),
    n_parties = dplyr::n_distinct(party_label_best),
    n_detailed_party_blocs = dplyr::n_distinct(party_bloc_detailed),
    n_pooled_blocs = dplyr::n_distinct(bloc),
    
    party_families_available = paste(
      sort(unique(stats::na.omit(party_bloc_detailed))),
      collapse = "; "
    ),
    pooled_blocs_available = paste(
      sort(unique(stats::na.omit(bloc))),
      collapse = "; "
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(iso2c_file, year, elec_id)

cat("\nParty-system availability summary:\n")
party_system_availability_social_democratic %>%
  dplyr::summarise(
    n_elections = dplyr::n(),
    far_left_available = sum(available_far_left),
    green_available = sum(available_green),
    social_democratic_available = sum(available_social_democratic),
    mainstream_right_available = sum(available_mainstream_right),
    far_right_available = sum(available_far_right),
    left_available = sum(available_left),
    other_available = sum(available_other),
    non_available = sum(available_non),
    other_left_pooled_available = sum(available_other_left_pooled)
  ) %>%
  print(width = Inf)

cat("\nElection-level party-system availability:\n")
party_system_availability_social_democratic %>%
  dplyr::select(
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_social_democratic,
    available_mainstream_right,
    available_far_right,
    available_left,
    available_other,
    available_non,
    n_parties,
    party_families_available
  ) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available social-democratic party:\n")
party_system_availability_social_democratic %>%
  dplyr::filter(!available_social_democratic) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available far-left party:\n")
party_system_availability_social_democratic %>%
  dplyr::filter(!available_far_left) %>%
  dplyr::select(iso2c_file, elec_id, year, party_families_available) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available green party:\n")
party_system_availability_social_democratic %>%
  dplyr::filter(!available_green) %>%
  dplyr::select(iso2c_file, elec_id, year, party_families_available) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available far-right party:\n")
party_system_availability_social_democratic %>%
  dplyr::filter(!available_far_right) %>%
  dplyr::select(iso2c_file, elec_id, year, party_families_available) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 7. Reconstruct and validate transition labels
# ------------------------------------------------

df_all <- df_all %>%
  dplyr::group_by(iso2c_file, elec_id, id) %>%
  dplyr::mutate(
    switch_to = parfam_final[voted_now %in% TRUE][1],
    switch_from = parfam_final[voted_lag %in% TRUE][1],
    switch_to_bloc = bloc[voted_now %in% TRUE][1],
    switch_from_bloc = bloc[voted_lag %in% TRUE][1],
    switch_to_bloc_detailed = party_bloc_detailed[voted_now %in% TRUE][1],
    switch_from_bloc_detailed = party_bloc_detailed[voted_lag %in% TRUE][1],
    stay = switch_to == switch_from
  ) %>%
  dplyr::ungroup()

match_check <- df_all %>%
  dplyr::group_by(iso2c_file, elec_id, id) %>%
  dplyr::summarise(
    now_matches_check = sum(voted_now %in% TRUE, na.rm = TRUE),
    lag_matches_check = sum(voted_lag %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nVote-match validation:\n")
print(match_check %>% dplyr::count(now_matches_check, lag_matches_check))

stopifnot(all(match_check$now_matches_check == 1))
stopifnot(all(match_check$lag_matches_check == 1))

logical_switch_label_cases <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    source_file,
    elec_id,
    id,
    switch_from,
    switch_to,
    switch_from_bloc,
    switch_to_bloc,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed,
    stay
  ) %>%
  dplyr::filter(
    as.character(switch_from) %in% c("TRUE", "FALSE") |
      as.character(switch_to) %in% c("TRUE", "FALSE") |
      as.character(switch_from_bloc) %in% c("TRUE", "FALSE") |
      as.character(switch_to_bloc) %in% c("TRUE", "FALSE") |
      as.character(switch_from_bloc_detailed) %in% c("TRUE", "FALSE") |
      as.character(switch_to_bloc_detailed) %in% c("TRUE", "FALSE")
  )

cat("\nLogical switch-label cases:\n")
print(logical_switch_label_cases, n = Inf)

stopifnot(nrow(logical_switch_label_cases) == 0)

cat("\nOriginal party-family transition table:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(iso2c_file, elec_id, id, switch_from, switch_to) %>%
  dplyr::count(switch_from, switch_to) %>%
  print(n = Inf)

cat("\nPooled bloc transition table:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc,
    switch_to_bloc
  ) %>%
  dplyr::count(switch_from_bloc, switch_to_bloc) %>%
  print(n = Inf)

cat("\nDetailed bloc transition table:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed
  ) %>%
  dplyr::count(switch_from_bloc_detailed, switch_to_bloc_detailed) %>%
  print(n = Inf)

cat("\nSocial-democratic switching summary, detailed blocs:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed
  ) %>%
  dplyr::summarise(
    n_respondent_elections = dplyr::n(),
    previous_social_democratic_share = mean(
      switch_from_bloc_detailed == "social_democratic",
      na.rm = TRUE
    ),
    current_social_democratic_share = mean(
      switch_to_bloc_detailed == "social_democratic",
      na.rm = TRUE
    ),
    social_democratic_retention_share = mean(
      switch_from_bloc_detailed == "social_democratic" &
        switch_to_bloc_detailed == "social_democratic",
      na.rm = TRUE
    ),
    social_democratic_outward_share = mean(
      switch_from_bloc_detailed == "social_democratic" &
        switch_to_bloc_detailed != "social_democratic",
      na.rm = TRUE
    ),
    social_democratic_inward_share = mean(
      switch_from_bloc_detailed != "social_democratic" &
        switch_to_bloc_detailed == "social_democratic",
      na.rm = TRUE
    )
  ) %>%
  print(width = Inf)

cat("\nOutward switching from social democracy, detailed blocs:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed
  ) %>%
  dplyr::filter(switch_from_bloc_detailed == "social_democratic") %>%
  dplyr::count(switch_to_bloc_detailed) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nInward switching to social democracy, detailed blocs:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed
  ) %>%
  dplyr::filter(switch_to_bloc_detailed == "social_democratic") %>%
  dplyr::count(switch_from_bloc_detailed) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nOutward switching from social democracy, pooled blocs:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc,
    switch_to_bloc
  ) %>%
  dplyr::filter(switch_from_bloc == "social_democratic") %>%
  dplyr::count(switch_to_bloc) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nInward switching to social democracy, pooled blocs:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc,
    switch_to_bloc
  ) %>%
  dplyr::filter(switch_to_bloc == "social_democratic") %>%
  dplyr::count(switch_from_bloc) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

# ------------------------------------------------
# 8. Party classification diagnostics
# ------------------------------------------------

cat("\nParty classification summary, detailed blocs:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_right_populist,
    far_left_populist,
    far_right,
    far_left,
    social_democratic,
    mainstream_right,
    green,
    left,
    other_left,
    non_voter,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::count(party_bloc_detailed) %>%
  print(n = Inf)

cat("\nParty classification summary, pooled blocs:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_right_populist,
    far_left_populist,
    far_right,
    far_left,
    social_democratic,
    mainstream_right,
    green,
    left,
    other_left,
    non_voter,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::count(bloc) %>%
  print(n = Inf)

cat("\nFar-left parties identified by PopuList:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_left_populist,
    far_left,
    other_left,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(far_left == TRUE) %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

cat("\nFar-right parties identified by PopuList:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_right_populist,
    far_right,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(far_right == TRUE) %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

cat("\nSocial-democratic parties identified:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    social_democratic,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(social_democratic == TRUE) %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

cat("\nGreen parties identified:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    green,
    other_left,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(green == TRUE) %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

cat("\nOther-left parties identified, pooled robustness bloc:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_left,
    green,
    left,
    other_left,
    social_democratic,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(other_left == TRUE) %>%
  dplyr::arrange(iso2c_file, party_bloc_detailed, party_label_best) %>%
  print(n = Inf)

cat("\nMainstream-right parties identified:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    mainstream_right,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(mainstream_right == TRUE) %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

cat("\nParties classified as other:\n")
df_all %>%
  dplyr::distinct(
    iso2c_file,
    country_name,
    party_label_best,
    parfam_harmonized,
    parfam_final,
    far_right,
    far_left,
    social_democratic,
    mainstream_right,
    green,
    left,
    other_left,
    non_voter,
    party_bloc_detailed,
    bloc
  ) %>%
  dplyr::filter(party_bloc_detailed == "other") %>%
  dplyr::arrange(iso2c_file, party_label_best) %>%
  print(n = Inf)

# ------------------------------------------------
# 9. Covariate missingness
# ------------------------------------------------

cat("\nCovariate missingness, respondent-election level:\n")

df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(iso2c_file, elec_id, id, age, gender, lrself, satdem) %>%
  dplyr::summarise(
    n_respondent_elections = dplyr::n(),
    age_missing = sum(is.na(age)),
    gender_missing = sum(is.na(gender)),
    lrself_missing = sum(is.na(lrself)),
    satdem_missing = sum(is.na(satdem)),
    age_missing_share = mean(is.na(age)),
    gender_missing_share = mean(is.na(gender)),
    lrself_missing_share = mean(is.na(lrself)),
    satdem_missing_share = mean(is.na(satdem))
  ) %>%
  print(width = Inf)

fully_missing_covariates <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    source_file,
    elec_id,
    year,
    id,
    age,
    gender,
    lrself,
    satdem
  ) %>%
  dplyr::group_by(iso2c_file, source_file, elec_id, year) %>%
  dplyr::summarise(
    n_respondents = dplyr::n(),
    age_all_missing = all(is.na(age)),
    gender_all_missing = all(is.na(gender)),
    lrself_all_missing = all(is.na(lrself)),
    satdem_all_missing = all(is.na(satdem)),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    age_all_missing |
      gender_all_missing |
      lrself_all_missing |
      satdem_all_missing
  ) %>%
  dplyr::arrange(iso2c_file, year, elec_id)

cat("\nElections where at least one covariate is fully missing:\n")
print(fully_missing_covariates, n = Inf, width = Inf)

party_system_availability_social_democratic %>%
  count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  )

# ------------------------------------------------
# 10. Merge party-system availability into respondent-level data
#     and prepare choice-set objects for later multinomial models
# ------------------------------------------------

availability_cols <- c(
  "available_far_left",
  "available_green",
  "available_social_democratic",
  "available_mainstream_right",
  "available_far_right",
  "available_left",
  "available_other",
  "available_non",
  "available_other_left_pooled",
  "n_party_rows",
  "n_parties",
  "n_detailed_party_blocs",
  "n_pooled_blocs",
  "party_families_available",
  "pooled_blocs_available"
)

party_system_availability_for_merge <- party_system_availability_social_democratic %>%
  dplyr::select(
    iso2c_file,
    elec_id,
    year,
    dplyr::all_of(availability_cols)
  )

availability_duplicate_check <- party_system_availability_for_merge %>%
  dplyr::count(iso2c_file, elec_id, year) %>%
  dplyr::filter(n > 1)

if (nrow(availability_duplicate_check) > 0) {
  cat("\nDuplicate election keys in party-system availability table:\n")
  print(availability_duplicate_check, n = Inf, width = Inf)
  stop("Party-system availability table is not unique by iso2c_file, elec_id, year.")
}

n_before_availability_merge <- nrow(df_all)

df_all <- df_all %>%
  dplyr::left_join(
    party_system_availability_for_merge,
    by = c("iso2c_file", "elec_id", "year")
  )

stopifnot(nrow(df_all) == n_before_availability_merge)

availability_merge_check <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_social_democratic,
    available_mainstream_right,
    available_far_right,
    available_non
  ) %>%
  dplyr::filter(
    is.na(available_far_left) |
      is.na(available_green) |
      is.na(available_social_democratic) |
      is.na(available_mainstream_right) |
      is.na(available_far_right) |
      is.na(available_non)
  )

cat("\nRespondent-level party-system availability merge check:\n")
print(availability_merge_check, n = Inf, width = Inf)

stopifnot(nrow(availability_merge_check) == 0)

cat("\nParty-system availability attached to respondent-level data:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_social_democratic,
    available_mainstream_right,
    available_far_right,
    available_non
  ) %>%
  dplyr::count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 11. Prepare election-level multinomial choice sets
# ------------------------------------------------

choice_set_outward_social_democratic <- party_system_availability_social_democratic %>%
  dplyr::transmute(
    iso2c_file,
    elec_id,
    year,
    model_direction = "outward_social_democratic",
    
    retention = TRUE,
    to_far_left = available_far_left,
    to_green = available_green,
    to_mainstream_right = available_mainstream_right,
    to_far_right = available_far_right,
    to_non = available_non
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      retention,
      to_far_left,
      to_green,
      to_mainstream_right,
      to_far_right,
      to_non
    ),
    names_to = "alternative",
    values_to = "alternative_available"
  ) %>%
  dplyr::mutate(
    alternative = factor(
      alternative,
      levels = c(
        "retention",
        "to_far_left",
        "to_green",
        "to_mainstream_right",
        "to_far_right",
        "to_non"
      )
    ),
    alternative_id = as.integer(alternative) - 1L
  )

choice_set_inward_social_democratic <- party_system_availability_social_democratic %>%
  dplyr::transmute(
    iso2c_file,
    elec_id,
    year,
    model_direction = "inward_social_democratic",
    
    not_to_sd = TRUE,
    from_far_left = available_far_left,
    from_green = available_green,
    from_mainstream_right = available_mainstream_right,
    from_far_right = available_far_right,
    from_non = available_non
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      not_to_sd,
      from_far_left,
      from_green,
      from_mainstream_right,
      from_far_right,
      from_non
    ),
    names_to = "alternative",
    values_to = "alternative_available"
  ) %>%
  dplyr::mutate(
    alternative = factor(
      alternative,
      levels = c(
        "not_to_sd",
        "from_far_left",
        "from_green",
        "from_mainstream_right",
        "from_far_right",
        "from_non"
      )
    ),
    alternative_id = as.integer(alternative) - 1L
  )

choice_set_social_democratic <- dplyr::bind_rows(
  choice_set_outward_social_democratic,
  choice_set_inward_social_democratic
) %>%
  dplyr::arrange(
    model_direction,
    iso2c_file,
    year,
    elec_id,
    alternative_id
  )

cat("\nSocial-democratic multinomial choice-set availability:\n")
choice_set_social_democratic %>%
  dplyr::count(model_direction, alternative, alternative_available) %>%
  print(n = Inf, width = Inf)

cat("\nNumber of available alternatives by election and model direction:\n")
choice_set_social_democratic %>%
  dplyr::group_by(model_direction, iso2c_file, elec_id, year) %>%
  dplyr::summarise(
    n_available_alternatives = sum(alternative_available),
    available_alternatives = paste(
      as.character(alternative[alternative_available]),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  dplyr::count(model_direction, n_available_alternatives) %>%
  dplyr::arrange(model_direction, n_available_alternatives) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 12. Respondent-election level transition data with choice-set metadata
# ------------------------------------------------

df_respondent_election_social_democratic <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    source_file,
    elec_id,
    year,
    id,
    
    switch_from,
    switch_to,
    switch_from_bloc,
    switch_to_bloc,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed,
    stay,
    
    age,
    gender,
    lrself,
    satdem,
    
    available_far_left,
    available_green,
    available_social_democratic,
    available_mainstream_right,
    available_far_right,
    available_left,
    available_other,
    available_non,
    available_other_left_pooled,
    n_parties,
    n_detailed_party_blocs,
    n_pooled_blocs,
    party_families_available,
    pooled_blocs_available
  ) %>%
  dplyr::mutate(
    outward_sd_outcome = dplyr::case_when(
      switch_from_bloc_detailed != "social_democratic" ~ NA_character_,
      switch_to_bloc_detailed == "social_democratic" ~ "retention",
      switch_to_bloc_detailed == "far_left" ~ "to_far_left",
      switch_to_bloc_detailed == "green" ~ "to_green",
      switch_to_bloc_detailed == "mainstream_right" ~ "to_mainstream_right",
      switch_to_bloc_detailed == "far_right" ~ "to_far_right",
      switch_to_bloc_detailed == "non" ~ "to_non",
      TRUE ~ NA_character_
    ),
    inward_sd_outcome = dplyr::case_when(
      switch_from_bloc_detailed == "social_democratic" ~ NA_character_,
      switch_to_bloc_detailed != "social_democratic" ~ "not_to_sd",
      switch_from_bloc_detailed == "far_left" ~ "from_far_left",
      switch_from_bloc_detailed == "green" ~ "from_green",
      switch_from_bloc_detailed == "mainstream_right" ~ "from_mainstream_right",
      switch_from_bloc_detailed == "far_right" ~ "from_far_right",
      switch_from_bloc_detailed == "non" ~ "from_non",
      TRUE ~ NA_character_
    )
  )

cat("\nRespondent-election transition data with party-system availability:\n")
df_respondent_election_social_democratic %>%
  dplyr::summarise(
    n_respondent_elections = dplyr::n(),
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(iso2c_file),
    outward_sd_cases = sum(!is.na(outward_sd_outcome)),
    inward_sd_cases = sum(!is.na(inward_sd_outcome))
  ) %>%
  print(width = Inf)

cat("\nOutward social-democratic outcome support in respondent-election file:\n")
df_respondent_election_social_democratic %>%
  dplyr::filter(!is.na(outward_sd_outcome)) %>%
  dplyr::count(outward_sd_outcome, sort = TRUE) %>%
  print(n = Inf)

cat("\nInward social-democratic outcome support in respondent-election file:\n")
df_respondent_election_social_democratic %>%
  dplyr::filter(!is.na(inward_sd_outcome)) %>%
  dplyr::count(inward_sd_outcome, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------
# 13. Save master dataset, party-system availability,
#     and choice-set objects
# ------------------------------------------------

saveRDS(
  df_all,
  file.path(output_dir, "df_all_classified_social_democratic.rds")
)

save(
  df_all,
  file = file.path(output_dir, "df_all_classified_social_democratic.RData")
)

saveRDS(
  party_system_availability_social_democratic,
  file.path(output_dir, "party_system_availability_social_democratic.rds")
)

saveRDS(
  choice_set_social_democratic,
  file.path(output_dir, "choice_set_social_democratic.rds")
)

saveRDS(
  df_respondent_election_social_democratic,
  file.path(output_dir, "df_respondent_election_social_democratic.rds")
)

cat("\nSaved files:\n")
cat(file.path(output_dir, "df_all_classified_social_democratic.rds"), "\n")
cat(file.path(output_dir, "df_all_classified_social_democratic.RData"), "\n")
cat(file.path(output_dir, "party_system_availability_social_democratic.rds"), "\n")
cat(file.path(output_dir, "choice_set_social_democratic.rds"), "\n")
cat(file.path(output_dir, "df_respondent_election_social_democratic.rds"), "\n")

cat("\nScript completed successfully\n")