# ================================================================
# 02_Austria.R
# Build and validate country-specific microdata file from voteswitchR
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

# ------------------------------------------------
# 0. Package checks
# ------------------------------------------------
required_packages <- c("voteswitchR", "shiny")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install these before running the reproducibility workflow.",
    call. = FALSE
  )
}
suppressPackageStartupMessages({
  library(voteswitchR)
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------
# 1. Check package versions
# ------------------------------------------------
cat("\n========================================\n")
cat("voteswitchR version:", as.character(packageVersion("voteswitchR")), "\n")
cat("shiny version      :", as.character(packageVersion("shiny")), "\n")
cat("========================================\n\n")

# ------------------------------------------------
# 2. Build/load country data
# ------------------------------------------------
# data_file is loaded from the generated voteswitchR country bundle below.

country_prefix <- "AT"
country_name   <- "Austria"

input_rdata <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro", "at_data_file.RData")

load(input_rdata)

df_wide <- data_file$data

cat("\n========================================\n")
cat("Loaded country file for:", country_name, "\n")
cat("Input path:", input_rdata, "\n")
cat("========================================\n\n")

# ------------------------------------------------
# 3. Load package mappings
# ------------------------------------------------
data("mappings", package = "voteswitchR")

map_ctry <- mappings %>%
  dplyr::filter(iso2c == country_prefix)

cat("\n========================================\n")
cat("Mapping diagnostics for:", country_name, "\n")
cat("========================================\n\n")


# ------------------------------------------------
# 4. Reshape stacked party blocks from wide to long
# ------------------------------------------------
df_long <- df_wide %>%
  dplyr::mutate(
    election_date = as.Date(election_date, origin = "1970-01-01"),
    vote_raw = vote,
    l_vote_raw = l_vote,
    pid_raw = pid,
    vote = dplyr::na_if(vote, 99L),
    l_vote = dplyr::na_if(l_vote, 99L),
    pid = dplyr::na_if(pid, 99L)
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      starts_with("stack_"),
      starts_with("peid_"),
      starts_with("party_"),
      starts_with("party_harmonized_"),
      starts_with("map_vote_"),
      starts_with("map_lr_"),
      starts_with("vote_share_"),
      starts_with("vote_share_lag_"),
      starts_with("turnout_"),
      starts_with("turnout_lag_")
    ),
    names_to = c(".value", "alt"),
    names_pattern = "^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_(\\d+)$"
  ) %>%
  dplyr::mutate(
    alt = as.integer(alt)
  ) %>%
  dplyr::arrange(elec_id, id, alt)

# ------------------------------------------------
# 5. Keep only valid modeled party alternatives
# ------------------------------------------------
df_long_valid <- df_long %>%
  dplyr::filter(!is.na(stack))

max_stack <- max(df_long_valid$stack, na.rm = TRUE)

df_long_valid %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_ids = dplyr::n_distinct(id),
    max_stack = max(stack, na.rm = TRUE),
    n_missing_stack = sum(is.na(stack))
  ) %>%
  print()

# ------------------------------------------------
# 6. Attach party metadata using elec_id + stack
# ------------------------------------------------
ctry_party_map <- map_ctry %>%
  dplyr::select(
    elec_id,
    stack,
    peid_map = peid,
    party_map = party,
    party_harmonized_map = party_harmonized,
    party_name_map = party_name,
    partyabbrev_map = partyabbrev,
    parfam,
    parfam_harmonized,
    map_vote_map = map_vote,
    map_lr_map = map_lr,
    parlgov_election_id,
    ppeg_party_id
  ) %>%
  dplyr::distinct()

ctry_party_map %>%
  dplyr::count(elec_id, stack) %>%
  dplyr::filter(n > 1) %>%
  print()

df_long_valid <- df_long_valid %>%
  dplyr::left_join(
    ctry_party_map,
    by = c("elec_id", "stack")
  )

# ------------------------------------------------
# 7. Mapping checks and final family variable
# ------------------------------------------------
df_long_valid <- df_long_valid %>%
  dplyr::mutate(
    peid_match = dplyr::if_else(is.na(peid) | is.na(peid_map), NA, peid == peid_map),
    party_match = dplyr::if_else(is.na(party) | is.na(party_map), NA, party == party_map),
    party_harmonized_match = dplyr::if_else(
      is.na(party_harmonized) | is.na(party_harmonized_map),
      NA,
      party_harmonized == party_harmonized_map
    ),
    map_vote_match = dplyr::if_else(is.na(map_vote) | is.na(map_vote_map), NA, map_vote == map_vote_map),
    map_lr_match = dplyr::if_else(is.na(map_lr) | is.na(map_lr_map), NA, map_lr == map_lr_map),
    parfam_harmonized_original = parfam_harmonized,
    parfam_final = parfam_harmonized
  )

df_long_valid %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_missing_peid_map = sum(is.na(peid_map)),
    n_missing_parfam = sum(is.na(parfam)),
    n_missing_parfam_harmonized = sum(is.na(parfam_harmonized)),
    n_bad_peid = sum(peid_match == FALSE, na.rm = TRUE),
    n_bad_party = sum(party_match == FALSE, na.rm = TRUE),
    n_bad_party_harmonized = sum(party_harmonized_match == FALSE, na.rm = TRUE),
    n_bad_map_vote = sum(map_vote_match == FALSE, na.rm = TRUE),
    n_bad_map_lr = sum(map_lr_match == FALSE, na.rm = TRUE)
  ) %>%
  print()

# ------------------------------------------------
# 8. Add generalized non-party alternative
# ------------------------------------------------
add_non_alternative <- function(df, max_stack) {
  
  non_rows <- df %>%
    dplyr::distinct(elec_id, id, .keep_all = TRUE) %>%
    dplyr::mutate(
      alt = 0,
      stack = max_stack + 1,
      peid = NA_character_,
      peid_map = "non",
      party_map = NA_real_,
      party_harmonized_map = NA_real_,
      party_name_map = "non-voters",
      partyabbrev_map = "non",
      parfam = "non",
      parfam_harmonized = "non",
      parfam_harmonized_original = "non",
      parfam_final = "non",
      map_vote_map = NA_real_,
      map_lr_map = NA_character_,
      peid_match = NA,
      party_match = NA,
      party_harmonized_match = NA,
      map_vote_match = NA,
      map_lr_match = NA
    )
  
  non_rows <- non_rows[, names(df)]
  
  dplyr::bind_rows(df, non_rows) %>%
    dplyr::mutate(
      vote_match = dplyr::if_else(is.na(vote) | vote > max_stack, max_stack + 1, as.numeric(vote)),
      l_vote_match = dplyr::if_else(is.na(l_vote) | l_vote > max_stack, max_stack + 1, as.numeric(l_vote)),
      voted_now = vote_match == stack,
      voted_lag = l_vote_match == stack,
      switch_to = voted_now & !voted_lag,
      switch_from = !voted_now & voted_lag,
      stay = voted_now & voted_lag
    ) %>%
    dplyr::select(-vote_match, -l_vote_match)
}

df_long_valid <- add_non_alternative(df_long_valid, max_stack)

# ------------------------------------------------
# 9. Respondent-level matching indicators
# ------------------------------------------------
match_flags <- df_long_valid %>%
  dplyr::group_by(elec_id, id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now, na.rm = TRUE),
    lag_matches = sum(voted_lag, na.rm = TRUE),
    vote_missing = all(is.na(vote)),
    l_vote_missing = all(is.na(l_vote)),
    valid_now = now_matches == 1,
    valid_lag = lag_matches == 1,
    valid_both = now_matches == 1 & lag_matches == 1,
    .groups = "drop"
  )

df_long_valid <- df_long_valid %>%
  dplyr::left_join(match_flags, by = c("elec_id", "id")) %>%
  dplyr::mutate(
    valid_vote_now = valid_now,
    valid_vote_lag = valid_lag,
    valid_votes_both = valid_both,
    respondent_election_uid = paste(elec_id, id, sep = "__")
  )

# ------------------------------------------------
# 10. Validation checks
# ------------------------------------------------
df_long_valid %>%
  dplyr::group_by(elec_id, id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now, na.rm = TRUE),
    lag_matches = sum(voted_lag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::summarise(
    min_now = min(now_matches),
    max_now = max(now_matches),
    min_lag = min(lag_matches),
    max_lag = max(lag_matches),
    bad_now = sum(now_matches != 1),
    bad_lag = sum(lag_matches != 1)
  ) %>%
  print()

df_long_valid %>%
  dplyr::summarise(
    n_non_rows = sum(parfam_final == "non", na.rm = TRUE),
    n_party_rows_wrong_non = sum(stack <= max_stack & parfam_final == "non", na.rm = TRUE),
    n_chosen_non_now = sum(voted_now & parfam_final == "non", na.rm = TRUE),
    n_chosen_non_lag = sum(voted_lag & parfam_final == "non", na.rm = TRUE)
  ) %>%
  print()

stopifnot(sum(is.na(df_long_valid$stack)) == 0)
stopifnot(sum(is.na(df_long_valid$peid_map)) == 0)
stopifnot(sum(is.na(df_long_valid$parfam)) == 0)
stopifnot(sum(is.na(df_long_valid$parfam_harmonized)) == 0)
stopifnot(sum(df_long_valid$peid_match == FALSE, na.rm = TRUE) == 0)
stopifnot(sum(df_long_valid$party_match == FALSE, na.rm = TRUE) == 0)
stopifnot(sum(df_long_valid$party_harmonized_match == FALSE, na.rm = TRUE) == 0)
stopifnot(sum(df_long_valid$map_vote_match == FALSE, na.rm = TRUE) == 0)
stopifnot(sum(df_long_valid$map_lr_match == FALSE, na.rm = TRUE) == 0)
stopifnot(sum(df_long_valid$stack <= max_stack & df_long_valid$parfam_final == "non", na.rm = TRUE) == 0)

# ------------------------------------------------
# 11. Standard output datasets
# ------------------------------------------------
df_long_full <- df_long_valid

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 12. Enforce consistent schema before saving
# ------------------------------------------------
coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id", "respondent_election_uid",
      "peid", "peid_map",
      "party_name_map", "partyabbrev_map",
      "parfam", "parfam_harmonized", "parfam_harmonized_original",
      "parfam_final",
      "map_lr", "map_lr_map", "region", "source_file"
    ),
    names(df)
  )
  
  num_vars <- intersect(
    c(
      "year", "election_date", "weights", "male", "age",
      "lr_self", "strength1", "strength2", "stfdem",
      "alt", "stack", "vote", "l_vote", "pid",
      "vote_raw", "l_vote_raw", "pid_raw",
      "party", "party_map", "party_harmonized", "party_harmonized_map",
      "map_vote", "map_vote_map",
      "vote_share", "vote_share_lag",
      "turnout", "turnout_lag",
      "now_matches", "lag_matches"
    ),
    names(df)
  )
  
  logi_vars <- intersect(
    c(
      "voted_now", "voted_lag",
      "switch_to", "switch_from", "stay",
      "vote_missing", "l_vote_missing",
      "valid_vote_now", "valid_vote_lag", "valid_votes_both",
      "valid_now", "valid_lag", "valid_both",
      "peid_match", "party_match", "party_harmonized_match",
      "map_vote_match", "map_lr_match"
    ),
    names(df)
  )
  
  df %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(char_vars), as.character),
      dplyr::across(dplyr::all_of(num_vars), as.numeric),
      dplyr::across(dplyr::all_of(logi_vars), as.logical)
    )
}

df_long_full       <- coerce_types(df_long_full)
df_long_valid_now  <- coerce_types(df_long_valid_now)
df_long_valid_lag  <- coerce_types(df_long_valid_lag)
df_long_valid_both <- coerce_types(df_long_valid_both)

# ------------------------------------------------
# 13. Save standard outputs
# ------------------------------------------------
output_dir <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro")

output_rdata_full <- file.path(output_dir, "at_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "at_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "at_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "at_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned country files for:", country_name, "\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")










