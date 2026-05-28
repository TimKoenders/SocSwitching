# ================================================================
# 32_append_country_files.R
# Append all harmonized country files present in data/micro
# Keeps non-voting category
# Harmonizes key covariates before appending:
#   age     <- age
#   gender  <- gender / male
#   lrself  <- lrself / lr_self
#   satdem  <- satdem / stfdem
# Drops alternatives: age_group, male, lr_self, stfdem
# Removes election_date and election_date_lag
# Places age, gender, lrself, satdem after weights
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

data_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_file <- file.path(data_dir, "all_countries_df_long_valid_both.RData")

print_header <- function(x) {
  cat("\n========================================\n")
  cat(x, "\n")
  cat("========================================\n")
}

harmonize_key_covariates <- function(df) {
  
  if (!"age" %in% names(df)) {
    df$age <- NA_real_
  }
  
  if (!"gender" %in% names(df)) {
    df$gender <- NA_character_
  }
  
  if ("male" %in% names(df)) {
    df$gender <- dplyr::coalesce(
      as.character(df$gender),
      as.character(df$male)
    )
  }
  
  if (!"lrself" %in% names(df)) {
    df$lrself <- NA_real_
  }
  
  if ("lr_self" %in% names(df)) {
    df$lrself <- dplyr::coalesce(
      suppressWarnings(as.numeric(df$lrself)),
      suppressWarnings(as.numeric(df$lr_self))
    )
  }
  
  if (!"satdem" %in% names(df)) {
    df$satdem <- NA_real_
  }
  
  if ("stfdem" %in% names(df)) {
    df$satdem <- dplyr::coalesce(
      suppressWarnings(as.numeric(df$satdem)),
      suppressWarnings(as.numeric(df$stfdem))
    )
  }
  
  df %>%
    dplyr::mutate(
      age = suppressWarnings(as.numeric(age)),
      gender = as.character(gender),
      lrself = suppressWarnings(as.numeric(lrself)),
      satdem = suppressWarnings(as.numeric(satdem))
    ) %>%
    dplyr::select(
      -dplyr::any_of(c("age_group", "male", "lr_self", "stfdem", "election_date", "election_date_lag"))
    )
}

reorder_final_columns <- function(df) {
  df %>%
    dplyr::select(
      dplyr::any_of(c("iso2c", "countryname", "year", "edate", "edate_lag",
                      "elec_id", "elec_id_lag", "id", "weights",
                      "age", "gender", "lrself", "satdem")),
      dplyr::everything()
    )
}

# ------------------------------------------------
# 1. Detect all base country files
# ------------------------------------------------
print_header("Detect available country files")

country_files <- list.files(
  path = data_dir,
  pattern = "^[a-z]{2}.*_df_long_valid_both\\.RData$",
  full.names = TRUE
)

country_files <- country_files[
  !basename(country_files) %in% c(
    "at_df_long_valid_both.RData",
    "all_countries_df_long_valid_both.RData"
  )
]

country_file_index <- tibble(
  path = country_files,
  file = basename(country_files),
  iso2c_file = str_to_upper(str_extract(basename(country_files), "^[a-z]{2}"))
) %>%
  arrange(iso2c_file, file)

print(country_file_index, n = Inf)

stopifnot(nrow(country_file_index) > 0)

# ------------------------------------------------
# 2. Load one base file safely
# ------------------------------------------------
load_country_file <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  
  df_name <- intersect(
    loaded,
    c("df_long_full", "df_long_valid_now", "df_long_valid_lag", "df_long_valid_both")
  )
  
  if (length(df_name) != 1) {
    stop("Could not uniquely identify df_long object in: ", path)
  }
  
  df <- get(df_name, envir = env)
  
  if (!is.data.frame(df)) {
    stop("Loaded object is not a data frame in: ", path)
  }
  
  if (!"parfam_final" %in% names(df)) {
    if ("parfam_harmonized" %in% names(df)) {
      df$parfam_final <- df$parfam_harmonized
    } else if ("family" %in% names(df)) {
      df$parfam_final <- df$family
    } else {
      df$parfam_final <- NA_character_
    }
  }
  
  if (!"peid_map" %in% names(df)) {
    if ("parlgov_id_1" %in% names(df)) {
      df$peid_map <- ifelse(
        df$parfam_final == "non",
        "non",
        as.character(df$parlgov_id_1)
      )
    } else {
      df$peid_map <- ifelse(df$parfam_final == "non", "non", NA_character_)
    }
  } else {
    df$peid_map <- ifelse(
      df$parfam_final == "non",
      "non",
      as.character(df$peid_map)
    )
  }
  
  if (!"party_name_map" %in% names(df) && "party_name" %in% names(df)) {
    df$party_name_map <- df$party_name
  }
  
  if (!"partyabbrev_map" %in% names(df)) {
    if ("partyabbrev" %in% names(df)) {
      df$partyabbrev_map <- df$partyabbrev
    } else {
      df$partyabbrev_map <- NA_character_
    }
  }
  
  if (!"alt" %in% names(df) && "stack" %in% names(df)) {
    df$alt <- df$stack
  }
  
  if ("region" %in% names(df)) {
    df$region <- as.character(df$region)
  }
  
  df <- harmonize_key_covariates(df)
  
  char_vars <- intersect(
    c(
      "iso2c", "countryname", "elec_id", "elec_id_lag", "id",
      "peid", "peid_map", "party_name", "party_name_map",
      "partyabbrev", "partyabbrev_map",
      "family", "parfam", "parfam_harmonized", "parfam_final",
      "switch_to", "switch_from", "gender", "region"
    ),
    names(df)
  )
  
  num_vars <- intersect(
    c(
      "year", "weights", "age", "lrself", "satdem",
      "stack", "alt", "parlgov_id_1",
      "part", "l_part", "vote", "l_vote",
      "vote_raw", "l_vote_raw",
      "vote_share", "vote_share_lag",
      "turnout", "turnout_lag",
      "citizenship",
      "now_matches_n", "lag_matches_n"
    ),
    names(df)
  )
  
  date_vars <- intersect(c("edate", "edate_lag"), names(df))
  
  logi_vars <- intersect(
    c(
      "voted_now", "voted_lag",
      "now_matches", "lag_matches",
      "stay", "valid_now", "valid_lag", "valid_both"
    ),
    names(df)
  )
  
  df %>%
    mutate(
      across(all_of(char_vars), as.character),
      across(all_of(num_vars), as.numeric),
      across(all_of(date_vars), as.Date),
      across(all_of(logi_vars), as.logical),
      source_file = basename(path),
      iso2c_file = str_to_upper(str_extract(basename(path), "^[a-z]{2}"))
    ) %>%
    dplyr::select(-dplyr::any_of(c("election_date", "election_date_lag"))) %>%
    reorder_final_columns()
}

# ------------------------------------------------
# 3a. Append base cases
# ------------------------------------------------
print_header("Load and append base files")

df_all <- map_dfr(country_files, load_country_file)

cat("\nDimensions of appended base data:\n")
print(dim(df_all))

cat("\nFiles appended as base files:\n")
print(sort(unique(df_all$source_file)))

# ------------------------------------------------
# 3b. Add manually constructed country-year files
# ------------------------------------------------
print_header("Add manually constructed country-year files")

add_manual_country_year <- function(df_all, file_path, iso2c_file, label) {
  
  if (!file.exists(file_path)) {
    cat("\n", label, " manual file not found:\n", sep = "")
    cat(file_path, "\n")
    return(df_all)
  }
  
  if (basename(file_path) %in% unique(df_all$source_file)) {
    cat("\n", label, " already included as base file, skipping:\n", sep = "")
    cat(basename(file_path), "\n")
    return(df_all)
  }
  
  env_tmp <- new.env(parent = emptyenv())
  loaded_objects <- load(file_path, envir = env_tmp)
  
  object_name <- intersect(
    loaded_objects,
    c("df_long_valid_both", "df_long_valid_both_manual")
  )
  
  if (length(object_name) != 1) {
    stop("Could not uniquely identify object in: ", file_path)
  }
  
  df_manual <- get(object_name, envir = env_tmp)
  
  if (!is.data.frame(df_manual)) {
    stop("Loaded manual object is not a data frame in: ", file_path)
  }
  
  required_vars <- c(
    "respondent_election_uid", "id", "peid",
    "party_name", "family", "parlgov_id_1",
    "stack", "edate", "edate_lag",
    "parfam", "parfam_harmonized", "parfam_final"
  )
  
  for (v in required_vars) {
    if (!v %in% names(df_manual)) {
      df_manual[[v]] <- NA
    }
  }
  
  if (all(is.na(df_manual$parfam)) && "family" %in% names(df_manual)) {
    df_manual$parfam <- df_manual$family
  }
  
  if (all(is.na(df_manual$parfam_harmonized)) && "family" %in% names(df_manual)) {
    df_manual$parfam_harmonized <- df_manual$family
  }
  
  if (all(is.na(df_manual$parfam_final))) {
    df_manual$parfam_final <- df_manual$parfam_harmonized
  }
  
  df_manual <- harmonize_key_covariates(df_manual)
  
  df_manual <- df_manual %>%
    mutate(
      respondent_election_uid = as.character(respondent_election_uid),
      id = as.character(id),
      peid = as.character(peid),
      party_name = as.character(party_name),
      family = as.character(family),
      parfam = as.character(parfam),
      parfam_harmonized = as.character(parfam_harmonized),
      parfam_final = as.character(parfam_final),
      gender = as.character(gender),
      age = as.numeric(age),
      lrself = as.numeric(lrself),
      satdem = as.numeric(satdem),
      stack = as.numeric(stack),
      parlgov_id_1 = as.numeric(parlgov_id_1),
      edate = as.Date(edate),
      edate_lag = as.Date(edate_lag),
      peid_map = if_else(
        parfam_final == "non",
        "non",
        as.character(parlgov_id_1)
      ),
      party_name_map = as.character(party_name),
      partyabbrev_map = if ("partyabbrev" %in% names(.)) {
        as.character(partyabbrev)
      } else {
        NA_character_
      },
      peid = if_else(
        parfam_final == "non",
        "non",
        as.character(peid_map)
      ),
      alt = as.numeric(stack),
      source_file = basename(file_path),
      iso2c_file = iso2c_file
    ) %>%
    dplyr::select(-dplyr::any_of(c("election_date", "election_date_lag")))
  
  extra_manual_vars <- setdiff(names(df_manual), names(df_all))
  
  if (length(extra_manual_vars) > 0) {
    cat("\nDropping extra manual variables before append:\n")
    print(extra_manual_vars)
  }
  
  df_manual <- df_manual %>%
    dplyr::select(-dplyr::any_of(extra_manual_vars))
  
  missing_in_manual <- setdiff(names(df_all), names(df_manual))
  df_manual[missing_in_manual] <- NA
  df_manual <- df_manual[, names(df_all)]
  
  for (v in names(df_all)) {
    target_class <- class(df_all[[v]])[1]
    
    if (target_class == "character") {
      df_manual[[v]] <- as.character(df_manual[[v]])
    } else if (target_class == "numeric") {
      df_manual[[v]] <- as.numeric(df_manual[[v]])
    } else if (target_class == "logical") {
      df_manual[[v]] <- as.logical(df_manual[[v]])
    } else if (target_class == "Date") {
      df_manual[[v]] <- as.Date(df_manual[[v]])
    }
  }
  
  df_all <- bind_rows(df_all, df_manual) %>%
    reorder_final_columns()
  
  cat("\n", label, " added:\n", sep = "")
  cat("Rows:", nrow(df_manual), "\n")
  cat("Respondents:", dplyr::n_distinct(df_manual$id), "\n")
  cat("Alternatives:", dplyr::n_distinct(df_manual$stack), "\n")
  
  df_all
}

manual_files <- tibble::tribble(
  ~label,              ~iso2c_file, ~file_name,
  "Austria 2019",      "AT",        "at_2019_df_long_valid_both.RData",
  "Austria 2024",      "AT",        "at_2024_df_long_valid_both.RData",
  "Switzerland 2023",  "CH",        "ch_2023_df_long_valid_both.RData",
  "Denmark 2022",      "DK",        "dk_2022_df_long_valid_both.RData",
  "France 2022",       "FR",        "fr_2022_df_long_valid_both.RData",
  "New Zealand 2023",  "NZ",        "nz_2023_df_long_valid_both.RData",
  "Poland 2023",       "PL",        "pl_2023_df_long_valid_both.RData",
  "Portugal 2022",     "PT",        "pt_2022_df_long_valid_both.RData",
  "Portugal 2024",     "PT",        "pt_2024_df_long_valid_both.RData",
  "Sweden 2022",       "SE",        "se_2022_df_long_valid_both.RData",
  "Slovenia 2022",     "SI",        "si_2022_df_long_valid_both.RData",
  "Slovakia 2023",     "SK",        "sk_2023_df_long_valid_both.RData"
)

for (i in seq_len(nrow(manual_files))) {
  df_all <- add_manual_country_year(
    df_all     = df_all,
    file_path  = file.path(data_dir, "manual", manual_files$file_name[i]),
    iso2c_file = manual_files$iso2c_file[i],
    label      = manual_files$label[i]
  )
}

# ------------------------------------------------
# 4. Basic checks and compact diagnostics
# ------------------------------------------------
print_header("Basic checks and compact diagnostics")

df_all <- df_all %>%
  dplyr::mutate(
    party_name = dplyr::if_else(
      parfam_final == "non" & is.na(party_name),
      "non-voters",
      as.character(party_name)
    ),
    family = dplyr::if_else(
      parfam_final == "non" & is.na(family),
      "non",
      as.character(family)
    ),
    peid_map = dplyr::case_when(
      parfam_final == "non" ~ "non",
      !is.na(peid_map) ~ as.character(peid_map),
      !is.na(parlgov_id_1) ~ as.character(parlgov_id_1),
      !is.na(party_name) ~ as.character(party_name),
      TRUE ~ paste0("stack_", stack)
    ),
    gender = as.character(gender),
    age = as.numeric(age),
    lrself = as.numeric(lrself),
    satdem = as.numeric(satdem)
  ) %>%
  dplyr::select(
    -dplyr::any_of(c("age_group", "male", "lr_self", "stfdem", "election_date", "election_date_lag"))
  ) %>%
  reorder_final_columns()

required_vars <- c(
  "iso2c_file", "source_file", "elec_id", "year", "edate",
  "id", "weights", "age", "gender", "lrself", "satdem",
  "stack", "alt", "vote", "l_vote",
  "voted_now", "voted_lag",
  "now_matches", "lag_matches",
  "valid_now", "valid_lag", "valid_both",
  "peid_map", "parfam_final", "switch_to", "switch_from", "stay"
)

missing_required <- setdiff(required_vars, names(df_all))

cat("\nMissing required variables:\n")
print(missing_required)
stopifnot(length(missing_required) == 0)

removed_vars_still_present <- intersect(
  c("age_group", "male", "lr_self", "stfdem", "election_date", "election_date_lag"),
  names(df_all)
)

cat("\nRemoved alternative variables still present:\n")
print(removed_vars_still_present)
stopifnot(length(removed_vars_still_present) == 0)

# ------------------------------------------------
# 4. Basic checks and compact diagnostics
# ------------------------------------------------

df_all <- df_all %>%
  # Ensure 'family' exists before using it, and assign default value if missing
  dplyr::mutate(
    family = ifelse(!"family" %in% names(df_all), "non", family),  # Create 'family' if missing
    family = as.character(family),  # Convert 'family' to character if it exists
    
    party_name = dplyr::if_else(
      parfam_final == "non" & is.na(party),
      "non-voters",
      as.character(party)
    ),
    
    # Replace missing 'family' values with "non"
    family = ifelse(parfam_final == "non" & is.na(family), "non", family),
    
    # Handle missing 'parlgov_id_1' gracefully
    peid_map = dplyr::case_when(
      parfam_final == "non" ~ "non",
      !is.na(peid_map) ~ as.character(peid_map),
      !"parlgov_id_1" %in% names(df_all) ~ "non",  # Handle missing 'parlgov_id_1' gracefully
      TRUE ~ paste0("stack_", stack)  # Fallback for when 'parlgov_id_1' doesn't exist
    ),
    
    gender = as.character(gender),
    age = as.numeric(age),
    lrself = as.numeric(lrself),
    satdem = as.numeric(satdem)
  ) %>%
  dplyr::select(
    -dplyr::any_of(c("age_group", "male", "lr_self", "stfdem", "election_date", "election_date_lag"))
  ) %>%
  reorder_final_columns()

# Reconstruct switching labels from matched alternatives
df_all <- df_all %>%
  dplyr::group_by(iso2c_file, elec_id, id) %>%
  dplyr::mutate(
    switch_to = parfam_final[voted_now %in% TRUE][1],
    switch_from = parfam_final[voted_lag %in% TRUE][1],
    stay = switch_to == switch_from
  ) %>%
  dplyr::ungroup() %>%
  reorder_final_columns()

required_vars <- c(
  "iso2c", "iso2c_file", "source_file", "year", "elec_id", "id",
  "weights", "age", "gender", "lrself", "satdem",
  "alt", "stack", "vote", "l_vote",
  "voted_now", "voted_lag",
  "now_matches", "lag_matches",
  "valid_now", "valid_lag", "valid_both",
  "peid_map", "parfam_final", "switch_to", "switch_from", "stay"
)

missing_required <- setdiff(required_vars, names(df_all))

cat("\nMissing required variables:\n")
print(missing_required)
stopifnot(length(missing_required) == 0)

removed_vars_still_present <- intersect(
  c(
    "age_group", "male", "lr_self", "stfdem",
    "election_date", "election_date_lag", "edate", "edate_lag"
  ),
  names(df_all)
)

cat("\nRemoved alternative/date variables still present:\n")
print(removed_vars_still_present)
stopifnot(length(removed_vars_still_present) == 0)

# ------------------------------------------------
# 4a. Overall coverage
# ------------------------------------------------
print_header("Overall coverage")

coverage_overall <- df_all %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_countries = dplyr::n_distinct(iso2c_file),
    n_elections = dplyr::n_distinct(elec_id),
    n_sources = dplyr::n_distinct(source_file),
    n_respondent_elections = dplyr::n_distinct(
      paste(iso2c_file, elec_id, id, sep = "___")
    ),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )

print(coverage_overall)

coverage_by_country <- df_all %>%
  dplyr::distinct(iso2c_file, elec_id, year) %>%
  dplyr::group_by(iso2c_file) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(iso2c_file)

cat("\nCoverage by country:\n")
print(coverage_by_country, n = Inf)

# ------------------------------------------------
# 4b. Vote-switching structure
# ------------------------------------------------
print_header("Vote-switching structure")

match_check <- df_all %>%
  dplyr::group_by(iso2c_file, elec_id, id) %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    now_matches_check = sum(voted_now %in% TRUE, na.rm = TRUE),
    lag_matches_check = sum(voted_lag %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  )

vote_match_summary <- match_check %>%
  dplyr::summarise(
    n_respondent_elections = dplyr::n(),
    bad_now_matches = sum(now_matches_check != 1),
    bad_lag_matches = sum(lag_matches_check != 1),
    min_rows_per_respondent = min(n_rows, na.rm = TRUE),
    max_rows_per_respondent = max(n_rows, na.rm = TRUE)
  )

print(vote_match_summary)

cat("\nDistribution of current-vote matches:\n")
print(match_check %>% dplyr::count(now_matches_check, sort = FALSE))

cat("\nDistribution of lagged-vote matches:\n")
print(match_check %>% dplyr::count(lag_matches_check, sort = FALSE))

stopifnot(all(match_check$now_matches_check == 1))
stopifnot(all(match_check$lag_matches_check == 1))

non_check <- df_all %>%
  dplyr::group_by(iso2c_file, source_file, elec_id) %>%
  dplyr::summarise(
    has_non = any(parfam_final == "non"),
    non_peid_ok = all(peid_map[parfam_final == "non"] == "non", na.rm = TRUE),
    .groups = "drop"
  )

cat("\nNon-voting alternative check:\n")
print(non_check, n = Inf)

stopifnot(all(non_check$has_non))
stopifnot(all(non_check$non_peid_ok))

logical_switch_label_cases <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file, source_file, elec_id, id,
    switch_from, switch_to, stay
  ) %>%
  dplyr::filter(
    as.character(switch_from) %in% c("TRUE", "FALSE") |
      as.character(switch_to) %in% c("TRUE", "FALSE")
  )

cat("\nLogical switch-label cases after reconstruction:\n")
print(logical_switch_label_cases, n = Inf)

stopifnot(nrow(logical_switch_label_cases) == 0)

switching_summary <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(iso2c_file, elec_id, id, switch_from, switch_to, stay) %>%
  dplyr::summarise(
    n_respondent_elections = dplyr::n(),
    retention_rate = mean(stay, na.rm = TRUE),
    switching_rate = 1 - retention_rate,
    non_to_non_share = mean(switch_from == "non" & switch_to == "non", na.rm = TRUE),
    from_non_share = mean(switch_from == "non", na.rm = TRUE),
    to_non_share = mean(switch_to == "non", na.rm = TRUE)
  )

cat("\nSwitching summary:\n")
print(switching_summary)

# ------------------------------------------------
# 4c. Missingness on individual-level covariates
# ------------------------------------------------
print_header("Missingness on individual-level covariates")

covariate_missingness_overall <- df_all %>%
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
  )

print(covariate_missingness_overall)

covariate_availability_by_election <- df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file, source_file, elec_id, year, id,
    age, gender, lrself, satdem
  ) %>%
  dplyr::group_by(iso2c_file, source_file, elec_id, year) %>%
  dplyr::summarise(
    n_respondents = dplyr::n(),
    age_missing_share = mean(is.na(age)),
    gender_missing_share = mean(is.na(gender)),
    lrself_missing_share = mean(is.na(lrself)),
    satdem_missing_share = mean(is.na(satdem)),
    age_all_missing = all(is.na(age)),
    gender_all_missing = all(is.na(gender)),
    lrself_all_missing = all(is.na(lrself)),
    satdem_all_missing = all(is.na(satdem)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(age_all_missing | gender_all_missing | lrself_all_missing | satdem_all_missing),
    iso2c_file, year, elec_id
  )

cat("\nCovariate availability by election:\n")
print(covariate_availability_by_election, n = Inf)

fully_missing_covariates <- covariate_availability_by_election %>%
  dplyr::filter(
    age_all_missing |
      gender_all_missing |
      lrself_all_missing |
      satdem_all_missing
  )

cat("\nElections where at least one covariate is fully missing:\n")
print(fully_missing_covariates, n = Inf)

# ------------------------------------------------
# 5. Save appended file
# ------------------------------------------------
print_header("Save appended file")

df_all <- reorder_final_columns(df_all)

save(df_all, file = output_file)

cat("\nSaved appended valid-both data to:\n")
cat(output_file, "\n")









