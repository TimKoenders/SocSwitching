# ================================================================
# 33_Austria_2019.R
# Respondent-level stacked vote-switching data
# Non-voting integrated as core alternative
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tibble)
  library(tidyr)
  library(labelled)
})

# ------------------------------------------------
# 1. Configuration
# ------------------------------------------------
folder_location <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files"
survey_path     <- file.path(folder_location, "at20172024", "10874_da_en_v1_0.dta")

CTX_ISO2C         <- "AT"
CTX_COUNTRY       <- "Austria"
CTX_ELEC_ID       <- "AT-2019-09"
CTX_ELEC_ID_LAG   <- "AT-2017-10"
CTX_ELECTION_DATE <- as.Date("2019-09-29")
CTX_ELECTION_LAG  <- as.Date("2017-10-15")
CTX_YEAR          <- 2019L

output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/manual"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Survey variables
# ------------------------------------------------
VAR_TURNOUT_T   <- "w12_q11"
VAR_VOTE_T      <- "w12_q12"

VAR_TURNOUT_TM1 <- "w5_q9"
VAR_VOTE_TM1    <- "w5_q10"

VAR_CITIZENSHIP <- "sd1"
VAR_AGE_GROUP   <- "sd2x2"
VAR_GENDER      <- "sd3"
VAR_LRSELF      <- "w12_q9"
VAR_SATDEM      <- "w8_q8"

VAR_DESIGN_WEIGHT <- NULL


# ------------------------------------------------
# 3. Helpers
# ------------------------------------------------
as_num <- function(x) {
  as.numeric(haven::zap_labels(x))
}

get_var <- function(df, var) {
  if (is.null(var) || !var %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  as_num(df[[var]])
}

# ------------------------------------------------
# 4. Party mapping
# ------------------------------------------------
party_map_t <- tibble::tibble(
  map_vote_t = c(1L, 2L, 3L, 4L, 5L, 6L),
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  )
)

party_map_tm1 <- tibble::tibble(
  map_vote_tm1 = c(2L, 1L, 3L, 5L, 6L, 4L),
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  )
)

party_parlgov <- tibble::tibble(
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  ),
  parlgov_id_1 = c(1013, 973, 50, 2255, 2651, 1429)
)

party_family_map <- tibble::tibble(
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  ),
  family = c("chr", "soc", "nat", "lib", "eco", "eco")
)

party_map <- party_map_t %>%
  dplyr::mutate(stack = dplyr::row_number()) %>%
  dplyr::left_join(party_map_tm1, by = "party_name") %>%
  dplyr::left_join(party_parlgov, by = "party_name") %>%
  dplyr::left_join(party_family_map, by = "party_name") %>%
  dplyr::mutate(
    stack = as.numeric(stack),
    map_vote_t = as.numeric(map_vote_t),
    map_vote_tm1 = as.numeric(map_vote_tm1),
    parlgov_id_1 = as.numeric(parlgov_id_1),
    partyabbrev = NA_character_,
    parfam = family,
    parfam_harmonized = family,
    parfam_final = family
  ) %>%
  dplyr::select(
    stack, party_name, family,
    map_vote_t, map_vote_tm1, parlgov_id_1,
    partyabbrev, parfam, parfam_harmonized, parfam_final
  )

stopifnot(
  !anyDuplicated(party_map$stack),
  !anyDuplicated(party_map$party_name),
  !any(is.na(party_map$map_vote_t)),
  !any(is.na(party_map$map_vote_tm1)),
  !any(is.na(party_map$family))
)

non_stack <- max(party_map$stack, na.rm = TRUE) + 1

party_map_augmented <- party_map %>%
  dplyr::bind_rows(
    tibble::tibble(
      stack = non_stack,
      party_name = "non-voters",
      family = "non",
      map_vote_t = NA_real_,
      map_vote_tm1 = NA_real_,
      parlgov_id_1 = NA_real_,
      partyabbrev = "non",
      parfam = "non",
      parfam_harmonized = "non",
      parfam_final = "non"
    )
  )

# ------------------------------------------------
# 5. Load and prepare respondent-level data
# ------------------------------------------------
survey_raw <- haven::read_dta(survey_path)

cat("\nAge-group variable label:\n")
print(labelled::var_label(survey_raw[[VAR_AGE_GROUP]]))

cat("\nAge-group value labels:\n")
print(labelled::val_labels(survey_raw[[VAR_AGE_GROUP]]))

cat("\nAge-group distribution:\n")
print(table(haven::as_factor(survey_raw[[VAR_AGE_GROUP]]), useNA = "ifany"))

cat("\nAge-group numeric distribution:\n")
print(table(as.numeric(haven::zap_labels(survey_raw[[VAR_AGE_GROUP]])), useNA = "ifany"))

df_wide <- survey_raw %>%
  dplyr::mutate(
    iso2c = CTX_ISO2C,
    countryname = CTX_COUNTRY,
    year = as.numeric(CTX_YEAR),
    edate = CTX_ELECTION_DATE,
    edate_lag = CTX_ELECTION_LAG,
    elec_id = CTX_ELEC_ID,
    elec_id_lag = CTX_ELEC_ID_LAG,
    
    id = as.character(dplyr::row_number()),
    weights = if (is.null(VAR_DESIGN_WEIGHT)) 1 else get_var(., VAR_DESIGN_WEIGHT),
    
    part = get_var(., VAR_TURNOUT_T),
    l_part = get_var(., VAR_TURNOUT_TM1),
    
    vote_raw = get_var(., VAR_VOTE_T),
    l_vote_raw = get_var(., VAR_VOTE_TM1),
    
    vote = vote_raw,
    l_vote = l_vote_raw,
    
    citizenship = get_var(., VAR_CITIZENSHIP),
    
    age_group = as.character(haven::as_factor(.data[[VAR_AGE_GROUP]])),
    age_group_num = get_var(., VAR_AGE_GROUP),
    age = dplyr::case_when(
      age_group_num == 1 ~ 19,
      age_group_num == 2 ~ 24.5,
      age_group_num == 3 ~ 34.5,
      age_group_num == 4 ~ 44.5,
      age_group_num == 5 ~ 54.5,
      age_group_num == 6 ~ 64.5,
      age_group_num == 7 ~ 75,
      TRUE ~ NA_real_
    ),
    
    gender = as.character(get_var(., VAR_GENDER)),
    lrself = get_var(., VAR_LRSELF),
    satdem = get_var(., VAR_SATDEM)
  ) %>%
  dplyr::select(
    iso2c, countryname, year, edate, edate_lag,
    elec_id, elec_id_lag,
    id, weights,
    part, l_part,
    vote, l_vote,
    vote_raw, l_vote_raw,
    citizenship, age_group, age, gender, lrself, satdem
  )

# ------------------------------------------------
# 6. Diagnostics before stacking
# ------------------------------------------------
valid_vote_t_codes   <- party_map$map_vote_t
valid_vote_lag_codes <- party_map$map_vote_tm1

cat("\nCurrent vote_raw values:\n")
print(table(df_wide$vote_raw, useNA = "ifany"))

cat("\nLagged l_vote_raw values:\n")
print(table(df_wide$l_vote_raw, useNA = "ifany"))

cat("\nMapped current vote codes:\n")
print(valid_vote_t_codes)

cat("\nMapped lagged vote codes:\n")
print(valid_vote_lag_codes)

# ------------------------------------------------
# 7. Stack respondent-party alternatives
# ------------------------------------------------
df_long <- df_wide %>%
  tidyr::crossing(
    party_map_augmented %>%
      dplyr::select(
        stack, party_name, family, parlgov_id_1,
        partyabbrev, parfam, parfam_harmonized, parfam_final,
        map_vote_t, map_vote_tm1
      )
  ) %>%
  dplyr::mutate(
    voted_now = dplyr::case_when(
      part == 4 & vote_raw %in% valid_vote_t_codes &
        !is.na(map_vote_t) & vote_raw == map_vote_t ~ TRUE,
      stack == non_stack & part %in% c(1, 2, 3) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    voted_lag = dplyr::case_when(
      l_part == 4 & l_vote_raw %in% valid_vote_lag_codes &
        !is.na(map_vote_tm1) & l_vote_raw == map_vote_tm1 ~ TRUE,
      stack == non_stack & l_part %in% c(1, 2, 3) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    now_matches = voted_now,
    lag_matches = voted_lag,
    
    switch_to_tmp = dplyr::if_else(voted_now, parfam_final, NA_character_),
    switch_from_tmp = dplyr::if_else(voted_lag, parfam_final, NA_character_)
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    switch_to = switch_to_tmp[voted_now][1],
    switch_from = switch_from_tmp[voted_lag][1],
    stay = switch_to == switch_from,
    now_matches_n = sum(voted_now, na.rm = TRUE),
    lag_matches_n = sum(voted_lag, na.rm = TRUE),
    valid_now = now_matches_n == 1,
    valid_lag = lag_matches_n == 1,
    valid_both = valid_now & valid_lag
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-switch_to_tmp, -switch_from_tmp, -map_vote_t, -map_vote_tm1)

# ------------------------------------------------
# 8. Structural validation
# ------------------------------------------------
stopifnot(nrow(df_long) == nrow(df_wide) * nrow(party_map_augmented))
stopifnot(all(df_long$stack %in% party_map_augmented$stack))

stopifnot(sum(df_long$party_name == "non-voters" & df_long$stack != non_stack) == 0)
stopifnot(sum(df_long$party_name != "non-voters" & df_long$stack == non_stack) == 0)

cat("\nMatch distribution before valid-both filtering:\n")
print(
  df_long %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      now_matches = sum(voted_now, na.rm = TRUE),
      lag_matches = sum(voted_lag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::count(now_matches, lag_matches)
)

cat("\nCurrent turnout categories assigned to non:\n")
print(
  df_long %>%
    dplyr::filter(voted_now, party_name == "non-voters") %>%
    dplyr::count(part)
)

cat("\nLagged turnout categories assigned to non:\n")
print(
  df_long %>%
    dplyr::filter(voted_lag, party_name == "non-voters") %>%
    dplyr::count(l_part)
)

stopifnot(all(
  df_long %>%
    dplyr::filter(voted_now, party_name == "non-voters") %>%
    dplyr::pull(part) %in% c(1, 2, 3)
))

stopifnot(all(
  df_long %>%
    dplyr::filter(voted_lag, party_name == "non-voters") %>%
    dplyr::pull(l_part) %in% c(1, 2, 3)
))

cat("\nAssigned current votes:\n")
print(
  df_long %>%
    dplyr::filter(voted_now) %>%
    dplyr::count(party_name, family, sort = TRUE)
)

cat("\nAssigned lagged votes:\n")
print(
  df_long %>%
    dplyr::filter(voted_lag) %>%
    dplyr::count(party_name, family, sort = TRUE)
)

# ------------------------------------------------
# 9. Standard output datasets
# ------------------------------------------------
df_long_full <- df_long

df_long_valid_now <- df_long %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 10. Enforce consistent schema and typing
# ------------------------------------------------
coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "countryname", "elec_id", "elec_id_lag", "id",
      "party_name", "family", "partyabbrev",
      "parfam", "parfam_harmonized", "parfam_final",
      "switch_to", "switch_from",
      "age_group", "gender"
    ),
    names(df)
  )
  
  num_vars <- intersect(
    c(
      "year", "weights", "stack", "parlgov_id_1",
      "part", "l_part", "vote", "l_vote",
      "vote_raw", "l_vote_raw",
      "citizenship", "age", "lrself", "satdem",
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
    dplyr::mutate(
      dplyr::across(dplyr::all_of(char_vars), as.character),
      dplyr::across(dplyr::all_of(num_vars), as.numeric),
      dplyr::across(dplyr::all_of(date_vars), as.Date),
      dplyr::across(dplyr::all_of(logi_vars), as.logical)
    )
}

df_long_full       <- coerce_types(df_long_full)
df_long_valid_now  <- coerce_types(df_long_valid_now)
df_long_valid_lag  <- coerce_types(df_long_valid_lag)
df_long_valid_both <- coerce_types(df_long_valid_both)

standard_vars <- c(
  "iso2c", "countryname", "year", "edate", "edate_lag",
  "elec_id", "elec_id_lag",
  "id", "weights",
  "stack", "party_name", "family", "parlgov_id_1",
  "partyabbrev", "parfam", "parfam_harmonized", "parfam_final",
  "switch_to", "switch_from", "stay",
  "part", "l_part",
  "vote", "l_vote",
  "vote_raw", "l_vote_raw",
  "voted_now", "voted_lag",
  "now_matches", "lag_matches",
  "now_matches_n", "lag_matches_n",
  "valid_now", "valid_lag", "valid_both",
  "citizenship", "age_group", "age", "gender", "lrself", "satdem"
)

df_long_full       <- df_long_full %>% dplyr::select(dplyr::all_of(standard_vars))
df_long_valid_now  <- df_long_valid_now %>% dplyr::select(dplyr::all_of(standard_vars))
df_long_valid_lag  <- df_long_valid_lag %>% dplyr::select(dplyr::all_of(standard_vars))
df_long_valid_both <- df_long_valid_both %>% dplyr::select(dplyr::all_of(standard_vars))

stopifnot(identical(names(df_long_full), standard_vars))
stopifnot(identical(names(df_long_valid_now), standard_vars))
stopifnot(identical(names(df_long_valid_lag), standard_vars))
stopifnot(identical(names(df_long_valid_both), standard_vars))

# ------------------------------------------------
# 11. Save standard outputs
# ------------------------------------------------
output_rdata_full <- file.path(output_dir, "at_2019_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "at_2019_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "at_2019_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "at_2019_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved standard outputs:\n")
cat("Full      :", output_rdata_full, "\n")
cat("Valid now :", output_rdata_now, "\n")
cat("Valid lag :", output_rdata_lag, "\n")
cat("Valid both:", output_rdata_both, "\n")

cat("\nRows:\n")
cat("Full      :", nrow(df_long_full), "\n")
cat("Valid now :", nrow(df_long_valid_now), "\n")
cat("Valid lag :", nrow(df_long_valid_lag), "\n")
cat("Valid both:", nrow(df_long_valid_both), "\n")

cat("\nRespondents:", dplyr::n_distinct(df_long_valid_both$id), "\n")
cat("Alternatives:", dplyr::n_distinct(df_long_valid_both$stack), "\n")

# ------------------------------------------------
# 12. Switching checks
# ------------------------------------------------
print(
  df_long_valid_both %>%
    dplyr::summarise(
      rows = dplyr::n(),
      respondents = dplyr::n_distinct(id),
      alternatives = dplyr::n_distinct(stack),
      rows_per_respondent = rows / respondents
    )
)

print(
  df_long_valid_both %>%
    dplyr::count(id) %>%
    dplyr::count(rows_per_id = n)
)

print(
  df_long_valid_both %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      now_matches = sum(now_matches, na.rm = TRUE),
      lag_matches = sum(lag_matches, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::count(now_matches, lag_matches)
)

print(
  df_long_valid_both %>%
    dplyr::filter(now_matches) %>%
    dplyr::select(id, vote_to = party_name) %>%
    dplyr::left_join(
      df_long_valid_both %>%
        dplyr::filter(lag_matches) %>%
        dplyr::select(id, vote_from = party_name),
      by = "id"
    ) %>%
    dplyr::count(vote_from, vote_to) %>%
    tidyr::pivot_wider(
      names_from = vote_to,
      values_from = n,
      values_fill = 0
    )
)

print(
  df_long_valid_both %>%
    dplyr::filter(now_matches) %>%
    dplyr::select(id, family_to = family) %>%
    dplyr::left_join(
      df_long_valid_both %>%
        dplyr::filter(lag_matches) %>%
        dplyr::select(id, family_from = family),
      by = "id"
    ) %>%
    dplyr::count(family_from, family_to) %>%
    tidyr::pivot_wider(
      names_from = family_to,
      values_from = n,
      values_fill = 0
    )
)