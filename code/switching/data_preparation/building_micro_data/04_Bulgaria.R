# ================================================================
# 04_Bulgaria.R
# Build and validate country-specific microdata file from voteswitchR
# ================================================================

rm(list = ls())

options(stringsAsFactors = FALSE)

# ------------------------------------------------
# 0. Package checks
# ------------------------------------------------
required_shiny_version <- "1.7.2"

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("voteswitchR", quietly = TRUE)) {
  remotes::install_github("denis-cohen/voteswitchR")
}

if (!requireNamespace("shiny", quietly = TRUE) ||
    as.character(utils::packageVersion("shiny")) != required_shiny_version) {
  message("Installing shiny ", required_shiny_version, " for build_data_file() compatibility...")
  remotes::install_version("shiny", version = required_shiny_version, upgrade = "never")
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
# 2. Launch the Shiny app for data procurement/build
#    OR load a previously saved data_file object
# ------------------------------------------------
data_file <- voteswitchR::build_data_file()

# ------------------------------------------------
# 3. Set country-specific inputs
# ------------------------------------------------
country_prefix <- "BG"
country_name   <- "Bulgaria"

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/bg_data_file.RData"
output_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/bg_df_long.RData"

# ------------------------------------------------
# 4. Load saved data_file
# ------------------------------------------------
load(input_rdata)

if (!exists("data_file")) {
  stop("data_file not found after loading")
}

# Correct assignment
df <- data_file$data


# ------------------------------------------------
# 5. Diagnostics: structure and content
# ------------------------------------------------

cat("\n========================================\n")
cat("BASIC STRUCTURE\n")
cat("========================================\n")

print(names(df))
print(dim(df))
dplyr::glimpse(df)


cat("\n========================================\n")
cat("STACKED BLOCK STRUCTURE\n")
cat("========================================\n")

var_names <- names(df)

stack_vars  <- var_names[grepl("^stack_", var_names)]
peid_vars   <- var_names[grepl("^peid_", var_names)]
party_vars  <- var_names[grepl("^party_", var_names)]
pharm_vars  <- var_names[grepl("^party_harmonized_", var_names)]
map_vote    <- var_names[grepl("^map_vote_", var_names)]
map_lr      <- var_names[grepl("^map_lr_", var_names)]
vshare      <- var_names[grepl("^vote_share_", var_names)]
vshare_lag  <- var_names[grepl("^vote_share_lag_", var_names)]
turnout     <- var_names[grepl("^turnout_", var_names)]
turnout_lag <- var_names[grepl("^turnout_lag_", var_names)]

cat("Max alternative number:\n")
alts <- as.integer(gsub(".*_", "", stack_vars))
print(max(alts, na.rm = TRUE))


cat("\n========================================\n")
cat("ELECTION COVERAGE\n")
cat("========================================\n")

print(unique(df$elec_id))
print(table(df$elec_id))

if ("year" %in% names(df)) {
  print(summary(df$year))
}


cat("\n========================================\n")
cat("MAPPING STRUCTURE (Bulgaria)\n")
cat("========================================\n")

mappings <- voteswitchR::mappings

map_bg <- mappings %>%
  dplyr::filter(stringr::str_starts(elec_id, "BG"))

print(dim(map_bg))
print(length(unique(map_bg$elec_id)))
print(table(map_bg$elec_id))

print(unique(map_bg$parfam))
print(unique(map_bg$parfam_harmonized))

map_bg %>%
  dplyr::select(elec_id, stack, peid, party_name, partyabbrev, parfam, parfam_harmonized) %>%
  dplyr::distinct() %>%
  head(20) %>%
  print()


cat("\n========================================\n")
cat("RESHAPE DIAGNOSTICS\n")
cat("========================================\n")

df_long <- df %>%
  tidyr::pivot_longer(
    cols = matches("^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_"),
    names_to = c(".value", "alt"),
    names_pattern = "(.*)_(\\d+)"
  )

print(dim(df_long))
print(table(df_long$elec_id, df_long$alt))

cat("Missing stack:\n")
print(sum(is.na(df_long$stack)))

print(table(df_long$elec_id, is.na(df_long$stack)))


# ------------------------------------------------
# 6. Drop structurally empty alternatives
# ------------------------------------------------

df_long <- df_long %>%
  dplyr::filter(!is.na(stack))

# Check result
table(df_long$elec_id, df_long$alt)


# ------------------------------------------------
# 7. Join mapping
# ------------------------------------------------

df_long <- df_long %>%
  dplyr::left_join(
    map_bg %>%
      dplyr::select(
        elec_id,
        stack,
        peid_map = peid,
        party_name_map = party_name,
        partyabbrev_map = partyabbrev,
        parfam,
        parfam_harmonized
      ),
    by = c("elec_id", "stack")
  )

# Mapping diagnostics
cat("Missing peid_map:\n")
print(sum(is.na(df_long$peid_map)))

cat("Missing parfam_harmonized:\n")
print(sum(is.na(df_long$parfam_harmonized)))


# ------------------------------------------------
# 8. Recode vote variables
# ------------------------------------------------

df_long <- df_long %>%
  dplyr::mutate(
    vote   = ifelse(vote %in% c(98, 99), NA, vote),
    l_vote = ifelse(l_vote %in% c(98, 99), NA, l_vote)
  )


# ------------------------------------------------
# 9. Construct vote indicators
# ------------------------------------------------

df_long <- df_long %>%
  dplyr::mutate(
    voted_now = (vote == stack),
    voted_lag = (l_vote == stack)
  )


# ------------------------------------------------
# 10. Respondent-level validation
# ------------------------------------------------

df_check <- df_long %>%
  dplyr::group_by(id, elec_id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now, na.rm = TRUE),
    lag_matches = sum(voted_lag, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n========================================\n")
cat("VOTE MATCHING DIAGNOSTICS\n")
cat("========================================\n")

print(table(df_check$now_matches))
print(table(df_check$lag_matches))

df_check %>%
  dplyr::filter(now_matches == 0 | lag_matches == 0) %>%
  head(10) %>%
  print()

# ------------------------------------------------
# 11. Add final family variable
# ------------------------------------------------

df_long <- df_long %>%
  dplyr::mutate(
    parfam_final = parfam_harmonized
  )

# ------------------------------------------------
# 12. Add generalized non-party alternative before saving
# ------------------------------------------------

add_non_alternative_before_saving <- function(df) {
  
  old_max_stack <- max(df$stack, na.rm = TRUE)
  
  df <- df %>%
    dplyr::select(-dplyr::any_of(c(
      "now_matches", "lag_matches",
      "valid_now", "valid_lag", "valid_both",
      "valid_vote_now", "valid_vote_lag", "valid_votes_both",
      "switch_to", "switch_from", "stay"
    )))
  
  if (!"respondent_election_uid" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(respondent_election_uid = paste(elec_id, id, sep = "__"))
  }
  
  if (!"parfam_final" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(parfam_final = parfam_harmonized)
  }
  
  non_rows <- df %>%
    dplyr::distinct(elec_id, id, .keep_all = TRUE) %>%
    dplyr::mutate(
      alt = 0,
      stack = old_max_stack + 1,
      peid = NA_character_,
      peid_map = "non",
      party_name_map = "non-voters",
      parfam = "non",
      parfam_harmonized = "non",
      parfam_final = "non"
    )
  
  if ("partyabbrev" %in% names(non_rows)) {
    non_rows <- non_rows %>% dplyr::mutate(partyabbrev = "non")
  }
  
  if ("partyabbrev_map" %in% names(non_rows)) {
    non_rows <- non_rows %>% dplyr::mutate(partyabbrev_map = "non")
  }
  
  non_rows <- non_rows[, names(df)]
  
  df_out <- df %>%
    dplyr::bind_rows(non_rows) %>%
    dplyr::mutate(
      vote_match = dplyr::if_else(
        is.na(vote) | vote > old_max_stack,
        old_max_stack + 1,
        as.numeric(vote)
      ),
      l_vote_match = dplyr::if_else(
        is.na(l_vote) | l_vote > old_max_stack,
        old_max_stack + 1,
        as.numeric(l_vote)
      ),
      voted_now = vote_match == stack,
      voted_lag = l_vote_match == stack,
      switch_to = voted_now & !voted_lag,
      switch_from = !voted_now & voted_lag,
      stay = voted_now & voted_lag
    ) %>%
    dplyr::select(-vote_match, -l_vote_match)
  
  match_flags <- df_out %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::summarise(
      now_matches = sum(voted_now, na.rm = TRUE),
      lag_matches = sum(voted_lag, na.rm = TRUE),
      valid_now = now_matches == 1,
      valid_lag = lag_matches == 1,
      valid_both = now_matches == 1 & lag_matches == 1,
      .groups = "drop"
    )
  
  df_out <- df_out %>%
    dplyr::left_join(match_flags, by = c("elec_id", "id")) %>%
    dplyr::mutate(
      valid_vote_now = valid_now,
      valid_vote_lag = valid_lag,
      valid_votes_both = valid_both
    )
  
  check_matches <- df_out %>%
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
    )
  
  check_non <- df_out %>%
    dplyr::summarise(
      n_non_rows = sum(parfam_final == "non", na.rm = TRUE),
      n_party_rows_wrong_non = sum(stack <= old_max_stack & parfam_final == "non", na.rm = TRUE),
      n_chosen_non_now = sum(voted_now & parfam_final == "non", na.rm = TRUE),
      n_chosen_non_lag = sum(voted_lag & parfam_final == "non", na.rm = TRUE)
    )
  
  print(check_matches)
  print(check_non)
  
  stopifnot(check_matches$bad_now == 0)
  stopifnot(check_matches$bad_lag == 0)
  stopifnot(check_non$n_party_rows_wrong_non == 0)
  
  df_out
}

df_long <- df_long %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack)
  )
df_long <- add_non_alternative_before_saving(df_long)

# ------------------------------------------------
# 13. Standard output datasets
# ------------------------------------------------

df_long_full <- df_long

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 14. Enforce consistent schema before saving
# ------------------------------------------------

coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id", "respondent_election_uid",
      "peid", "peid_map",
      "party_name_map", "partyabbrev", "partyabbrev_map",
      "parfam", "parfam_harmonized", "parfam_final",
      "map_lr", "region", "source_file"
    ),
    names(df)
  )
  
  num_vars <- intersect(
    c(
      "year", "election_date", "weights", "male", "age",
      "lr_self", "strength1", "strength2", "stfdem",
      "alt", "stack", "vote", "l_vote", "pid",
      "party", "party_harmonized", "map_vote",
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
      "valid_vote_now", "valid_vote_lag", "valid_votes_both",
      "valid_now", "valid_lag", "valid_both"
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
# 15. Save standard outputs
# ------------------------------------------------

output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "bg_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "bg_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "bg_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "bg_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned Bulgaria files:\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")