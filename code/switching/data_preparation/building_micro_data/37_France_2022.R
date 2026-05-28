# ================================================================
# 37_France_2022.R
# Respondent-level stacked vote-switching data from CSES6
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
cses_path       <- file.path(folder_location, "cses6", "cses6.dta")

CTX_ISO2C         <- "FR"
CTX_COUNTRY       <- "France"
CTX_ELEC_ID       <- "FR-2022-04"
CTX_ELEC_ID_LAG   <- "FR-2017-04"
CTX_ELECTION_DATE <- as.Date("2022-04-10")
CTX_ELECTION_LAG  <- as.Date("2017-04-23")
CTX_YEAR          <- 2022L
CTX_CONTEXT_CODE  <- "FRA_2022"

output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/manual"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_rdata_full <- file.path(output_dir, "fr_2022_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "fr_2022_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "fr_2022_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "fr_2022_df_long_valid_both.RData")

# ------------------------------------------------
# 2. CSES6 harmonized variables
# ------------------------------------------------
VAR_CONTEXT       <- "F1004"
VAR_RESP_ID       <- "F1003_1"

VAR_TURNOUT_T     <- "F3010_PR_1"
VAR_VOTE_T        <- "F3011_PR_1"
VAR_TURNOUT_TM1   <- "F3015_PR_1"
VAR_VOTE_TM1      <- "F3016_PR_1"

VAR_AGE           <- "F2001_A"
VAR_GENDER        <- "F2002"
VAR_LRSELF        <- "F3020_R"
VAR_SATDEM        <- "F3022"

VAR_DESIGN_WEIGHT <- NULL

# ------------------------------------------------
# 3. Helpers
# ------------------------------------------------
num <- function(x) {
  as.numeric(haven::zap_labels(x))
}

clean_turnout <- function(x) {
  x <- num(x)
  x[x %in% c(93, 96, 97, 98, 99)] <- NA_real_
  x[!(x %in% c(0, 1))] <- NA_real_
  as.integer(x)
}

clean_vote <- function(x) {
  x <- num(x)
  x[x %in% c(999992, 999993, 999997, 999998, 999999)] <- NA_real_
  x
}

clean_cses_numeric <- function(x) {
  x <- num(x)
  x[x %in% c(93, 95, 96, 97, 98, 99, 9997, 9998, 9999, 999992, 999993, 999997, 999998, 999999)] <- NA_real_
  x
}

clean_gender <- function(x) {
  x <- num(x)
  dplyr::case_when(
    x == 0 ~ "male",
    x == 1 ~ "female",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------
# 4. Party/candidate mapping
# ------------------------------------------------
party_map_t <- tibble::tribble(
  ~map_vote_t, ~party_name,
  250001L, "The Republic Onwards!",
  250002L, "National Rally",
  250003L, "Indomitable France",
  250004L, "Reconquest",
  250005L, "The Republicans",
  250006L, "Europe Ecology - The Greens",
  250007L, "Resist!",
  250008L, "French Communist Party",
  250009L, "France Arise",
  250010L, "Socialist Party",
  250011L, "New Anticapitalist Party",
  250012L, "Workers' Struggle"
)

party_map_tm1 <- tibble::tribble(
  ~map_vote_tm1, ~party_name,
  250001L, "The Republic Onwards!",
  250002L, "National Rally",
  250003L, "Indomitable France",
  250005L, "The Republicans",
  250007L, "Resist!",
  250009L, "France Arise",
  250010L, "Socialist Party",
  250011L, "New Anticapitalist Party",
  250012L, "Workers' Struggle",
  250013L, "Popular Republican Union",
  250014L, "Solidarity and Progress"
)

party_parlgov <- tibble::tribble(
  ~party_name, ~parlgov_id_1,
  "The Republic Onwards!",       2643L,
  "National Rally",               270L,
  "Indomitable France",          2644L,
  "Reconquest",                  2860L,
  "The Republicans",              658L,
  "Europe Ecology - The Greens", 2813L,
  "Resist!",                NA_integer_,
  "French Communist Party",       686L,
  "France Arise",                2399L,
  "Socialist Party",             1539L,
  "New Anticapitalist Party", NA_integer_,
  "Workers' Struggle",       NA_integer_,
  "Popular Republican Union", NA_integer_,
  "Solidarity and Progress",  NA_integer_
)

party_family_map <- tibble::tribble(
  ~party_name, ~family,
  "The Republic Onwards!",       "lib",
  "National Rally",              "nat",
  "Indomitable France",          "lef",
  "Reconquest",                  "nat",
  "The Republicans",             "con",
  "Europe Ecology - The Greens", "eco",
  "Resist!",                     "oth",
  "French Communist Party",      "com",
  "France Arise",                "nat",
  "Socialist Party",             "soc",
  "New Anticapitalist Party",    "lef",
  "Workers' Struggle",           "lef",
  "Popular Republican Union",    "oth",
  "Solidarity and Progress",     "oth"
)

party_map <- dplyr::full_join(party_map_t, party_map_tm1, by = "party_name") %>%
  dplyr::mutate(stack = dplyr::row_number()) %>%
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
# 5. Load data
# ------------------------------------------------
cses_raw <- haven::read_dta(cses_path)
names(cses_raw) <- toupper(names(cses_raw))

needed_vars <- c(
  VAR_CONTEXT, VAR_RESP_ID,
  VAR_TURNOUT_T, VAR_VOTE_T,
  VAR_TURNOUT_TM1, VAR_VOTE_TM1,
  VAR_AGE, VAR_GENDER, VAR_LRSELF, VAR_SATDEM
)

if (!is.null(VAR_DESIGN_WEIGHT)) {
  needed_vars <- c(needed_vars, VAR_DESIGN_WEIGHT)
}

missing_vars <- setdiff(needed_vars, names(cses_raw))

if (length(missing_vars) > 0) {
  cat("\nMissing variables:\n")
  print(missing_vars)
  stop("Some required variables are missing.")
}

cses_ctx <- cses_raw %>%
  dplyr::filter(as.character(haven::as_factor(.data[[VAR_CONTEXT]])) == CTX_CONTEXT_CODE)

if (nrow(cses_ctx) == 0L) {
  stop("No rows found for requested election context: ", CTX_CONTEXT_CODE)
}

# ------------------------------------------------
# 6. Print labels for verification
# ------------------------------------------------
print_var_labels <- function(dat, vars) {
  for (v in vars) {
    cat("\n============================================================\n")
    cat(v, "\n")
    cat("Variable label:\n")
    print(labelled::var_label(dat[[v]]))
    cat("Value labels:\n")
    print(labelled::val_labels(dat[[v]]))
  }
}

print_var_labels(
  cses_ctx,
  c(
    VAR_CONTEXT, VAR_RESP_ID,
    VAR_TURNOUT_T, VAR_VOTE_T,
    VAR_TURNOUT_TM1, VAR_VOTE_TM1,
    VAR_AGE, VAR_GENDER, VAR_LRSELF, VAR_SATDEM
  )
)

# ------------------------------------------------
# 7. Clean respondent-level data
# ------------------------------------------------
df_wide <- cses_ctx %>%
  dplyr::mutate(
    part = clean_turnout(.data[[VAR_TURNOUT_T]]),
    l_part = clean_turnout(.data[[VAR_TURNOUT_TM1]]),
    vote_raw = clean_vote(.data[[VAR_VOTE_T]]),
    l_vote_raw = clean_vote(.data[[VAR_VOTE_TM1]]),
    
    vote_raw = dplyr::if_else(part == 0L, NA_real_, vote_raw),
    l_vote_raw = dplyr::if_else(l_part == 0L, NA_real_, l_vote_raw)
  ) %>%
  dplyr::transmute(
    iso2c = CTX_ISO2C,
    countryname = CTX_COUNTRY,
    year = as.numeric(CTX_YEAR),
    edate = CTX_ELECTION_DATE,
    edate_lag = CTX_ELECTION_LAG,
    elec_id = CTX_ELEC_ID,
    elec_id_lag = CTX_ELEC_ID_LAG,
    
    id = paste0(CTX_ELEC_ID, "-", as.character(.data[[VAR_RESP_ID]])),
    
    weights = if (!is.null(VAR_DESIGN_WEIGHT)) {
      num(.data[[VAR_DESIGN_WEIGHT]])
    } else {
      1
    },
    
    part,
    l_part,
    vote_raw,
    l_vote_raw,
    
    vote = vote_raw,
    l_vote = l_vote_raw,
    
    citizenship = NA_real_,
    age_group = NA_character_,
    age = clean_cses_numeric(.data[[VAR_AGE]]),
    gender = clean_gender(.data[[VAR_GENDER]]),
    lrself = clean_cses_numeric(.data[[VAR_LRSELF]]),
    satdem = clean_cses_numeric(.data[[VAR_SATDEM]])
  ) %>%
  dplyr::mutate(
    weights = dplyr::if_else(is.na(weights) | weights <= 0, 1, weights)
  )

stopifnot(!anyDuplicated(df_wide$id))

# ------------------------------------------------
# 8. Diagnostics before stacking
# ------------------------------------------------
valid_vote_t_codes   <- party_map$map_vote_t[!is.na(party_map$map_vote_t)]
valid_vote_lag_codes <- party_map$map_vote_tm1[!is.na(party_map$map_vote_tm1)]

cat("\nCurrent vote_raw values:\n")
print(table(df_wide$vote_raw, useNA = "ifany"))

cat("\nLagged l_vote_raw values:\n")
print(table(df_wide$l_vote_raw, useNA = "ifany"))

cat("\nMapped current vote codes:\n")
print(valid_vote_t_codes)

cat("\nMapped lagged vote codes:\n")
print(valid_vote_lag_codes)

# ------------------------------------------------
# 9. Stack respondent-party alternatives
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
      vote_raw %in% valid_vote_t_codes &
        !is.na(map_vote_t) &
        vote_raw == map_vote_t ~ TRUE,
      stack == non_stack &
        !(vote_raw %in% valid_vote_t_codes) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    voted_lag = dplyr::case_when(
      l_vote_raw %in% valid_vote_lag_codes &
        !is.na(map_vote_tm1) &
        l_vote_raw == map_vote_tm1 ~ TRUE,
      stack == non_stack &
        !(l_vote_raw %in% valid_vote_lag_codes) ~ TRUE,
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
# 10. Structural validation
# ------------------------------------------------
stopifnot(nrow(df_long) == nrow(df_wide) * nrow(party_map_augmented))
stopifnot(all(df_long$stack %in% party_map_augmented$stack))

stopifnot(sum(df_long$party_name == "non-voters" & df_long$stack != non_stack) == 0)
stopifnot(sum(df_long$party_name != "non-voters" & df_long$stack == non_stack) == 0)

stopifnot(all(
  df_long %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n = sum(voted_now, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

stopifnot(all(
  df_long %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n = sum(voted_lag, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

stopifnot(all(
  df_long %>%
    dplyr::filter(!(vote_raw %in% valid_vote_t_codes), party_name == "non-voters") %>%
    dplyr::pull(voted_now)
))

stopifnot(all(
  df_long %>%
    dplyr::filter(!(l_vote_raw %in% valid_vote_lag_codes), party_name == "non-voters") %>%
    dplyr::pull(voted_lag)
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
# 11. Standard output datasets
# ------------------------------------------------
df_long_full <- df_long

df_long_valid_now <- df_long %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 12. Enforce consistent schema and typing
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
# 13. Save standard outputs
# ------------------------------------------------
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

cat("\nRespondents:\n")
cat("Full      :", dplyr::n_distinct(df_long_full$id), "\n")
cat("Valid now :", dplyr::n_distinct(df_long_valid_now$id), "\n")
cat("Valid lag :", dplyr::n_distinct(df_long_valid_lag$id), "\n")
cat("Valid both:", dplyr::n_distinct(df_long_valid_both$id), "\n")

cat("\nAlternatives:", dplyr::n_distinct(df_long_full$stack), "\n")

# ------------------------------------------------
# 14. Switching diagnostics
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

cat("\nParty-level switching matrix:\n")

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

cat("\nFamily-level switching matrix:\n")

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