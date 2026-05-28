# ================================================================
# 06_CzechRepublic.R
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
country_prefix <- "CZ"
country_name   <- "Czech Republic"

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/cz_data_file.RData"
output_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/cz_df_long.RData"

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
cat("DATA OBJECT STRUCTURE\n")
cat("========================================\n")

str(data_file, max.level = 1)
print(names(data_file))


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

if ("election_date" %in% names(df)) {
  print(summary(df$election_date))
}


cat("\n========================================\n")
cat("MAPPING STRUCTURE (Czech Republic)\n")
cat("========================================\n")

mappings <- voteswitchR::mappings

map_cz <- mappings %>%
  dplyr::filter(stringr::str_starts(elec_id, "CZ"))

print(dim(map_cz))
print(length(unique(map_cz$elec_id)))
print(table(map_cz$elec_id))

print(unique(map_cz$parfam))
print(unique(map_cz$parfam_harmonized))

map_cz %>%
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


cat("\n========================================\n")
cat("REMOVE PADDED ALTERNATIVES\n")
cat("========================================\n")

df_long <- df_long %>%
  dplyr::filter(!is.na(stack))

print(dim(df_long))


cat("\n========================================\n")
cat("JOIN MAPPING\n")
cat("========================================\n")

df_long <- df_long %>%
  dplyr::mutate(
    alt = as.integer(alt),
    stack = as.numeric(stack)
  ) %>%
  dplyr::left_join(
    map_cz %>%
      dplyr::transmute(
        elec_id,
        stack = as.numeric(stack),
        peid_map = peid,
        party_name_map = party_name,
        partyabbrev_map = partyabbrev,
        parfam,
        parfam_harmonized
      ) %>%
      dplyr::distinct(),
    by = c("elec_id", "stack")
  )

cat("Missing peid_map:\n")
print(sum(is.na(df_long$peid_map)))

cat("Agreement peid vs peid_map:\n")
print(mean(df_long$peid == df_long$peid_map, na.rm = TRUE))

df_long %>%
  dplyr::select(parfam, parfam_harmonized, party_name_map) %>%
  dplyr::distinct() %>%
  print(n = 50)


cat("\n========================================\n")
cat("VOTE RECODING + MATCHING DIAGNOSTICS\n")
cat("========================================\n")

df_long <- df_long %>%
  dplyr::mutate(
    vote   = dplyr::na_if(vote, 98),
    vote   = dplyr::na_if(vote, 99),
    l_vote = dplyr::na_if(l_vote, 98),
    l_vote = dplyr::na_if(l_vote, 99),
    voted_now = (vote == stack),
    voted_lag = (l_vote == stack)
  )

df_check <- df_long %>%
  dplyr::group_by(id, elec_id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now, na.rm = TRUE),
    lag_matches = sum(voted_lag, na.rm = TRUE),
    .groups = "drop"
  )

print(table(df_check$now_matches))
print(table(df_check$lag_matches))

df_check %>%
  dplyr::filter(now_matches > 1 | lag_matches > 1) %>%
  head(10) %>%
  print()

df_check %>%
  dplyr::filter(now_matches == 0 | lag_matches == 0) %>%
  head(10) %>%
  print()


cat("\n========================================\n")
cat("VOTE MISSINGNESS\n")
cat("========================================\n")

df_long %>%
  dplyr::group_by(elec_id) %>%
  dplyr::summarise(
    vote_missing  = mean(is.na(vote)),
    lvote_missing = mean(is.na(l_vote)),
    .groups = "drop"
  ) %>%
  print()

df_long %>%
  dplyr::filter(!is.na(vote) & !voted_now) %>%
  dplyr::distinct(elec_id, vote) %>%
  arrange(elec_id, vote) %>%
  print()


cat("\n========================================\n")
cat("FAMILY DIAGNOSTICS\n")
cat("========================================\n")

df_long %>%
  dplyr::filter(parfam_harmonized == "nat" | is.na(parfam_harmonized)) %>%
  dplyr::distinct(party_name_map, parfam, parfam_harmonized) %>%
  print(n = 50)


# ------------------------------------------------
# 6. Add generalized non-party alternative before saving
# ------------------------------------------------

add_non_alternative_before_saving <- function(df) {
  
  df <- df %>%
    dplyr::mutate(
      alt = as.numeric(alt),
      stack = as.numeric(stack),
      vote = as.numeric(vote),
      l_vote = as.numeric(l_vote),
      parfam_final = parfam_harmonized
    )
  
  old_max_stack <- max(df$stack, na.rm = TRUE)
  non_stack <- old_max_stack + 1
  
  non_rows <- df %>%
    dplyr::group_by(id, elec_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stack = non_stack,
      alt = 0,
      peid_map = "non",
      party_name_map = "non-voters",
      partyabbrev_map = "non",
      parfam = "non",
      parfam_harmonized = "non",
      parfam_final = "non"
    )
  
  if ("partyabbrev" %in% names(non_rows)) {
    non_rows <- non_rows %>%
      dplyr::mutate(partyabbrev = "non")
  }
  
  df %>%
    dplyr::select(
      -dplyr::any_of(c(
        "voted_now", "voted_lag",
        "switch_to", "switch_from", "stay",
        "valid_now", "valid_lag", "valid_both",
        "now_matches", "lag_matches"
      ))
    ) %>%
    dplyr::bind_rows(non_rows) %>%
    dplyr::mutate(
      voted_now = dplyr::case_when(
        stack == vote ~ TRUE,
        stack == non_stack & (is.na(vote) | vote > old_max_stack) ~ TRUE,
        TRUE ~ FALSE
      ),
      voted_lag = dplyr::case_when(
        stack == l_vote ~ TRUE,
        stack == non_stack & (is.na(l_vote) | l_vote > old_max_stack) ~ TRUE,
        TRUE ~ FALSE
      ),
      switch_to = dplyr::if_else(voted_now, parfam_final, NA_character_),
      switch_from = dplyr::if_else(voted_lag, parfam_final, NA_character_)
    ) %>%
    dplyr::group_by(id, elec_id) %>%
    dplyr::mutate(
      switch_to = switch_to[voted_now][1],
      switch_from = switch_from[voted_lag][1],
      stay = switch_to == switch_from,
      now_matches = sum(voted_now, na.rm = TRUE),
      lag_matches = sum(voted_lag, na.rm = TRUE),
      valid_now = now_matches == 1,
      valid_lag = lag_matches == 1,
      valid_both = valid_now & valid_lag
    ) %>%
    dplyr::ungroup()
}

df_long <- add_non_alternative_before_saving(df_long)

df_long_full <- df_long
df_long_valid_now  <- df_long_full %>% dplyr::filter(valid_now)
df_long_valid_lag  <- df_long_full %>% dplyr::filter(valid_lag)
df_long_valid_both <- df_long_full %>% dplyr::filter(valid_both)

# ------------------------------------------------
# 7. Final validation checks
# ------------------------------------------------

cat("\n========================================\n")
cat("FINAL VALIDATION\n")
cat("========================================\n")

stopifnot(sum(is.na(df_long_full$stack) | is.nan(df_long_full$stack)) == 0)
stopifnot(sum(is.na(df_long_full$peid_map)) == 0)

stopifnot(all(
  df_long_full %>%
    dplyr::group_by(id, elec_id) %>%
    dplyr::summarise(n = sum(voted_now, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

stopifnot(all(
  df_long_full %>%
    dplyr::group_by(id, elec_id) %>%
    dplyr::summarise(n = sum(voted_lag, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

stopifnot(all(
  df_long_full %>%
    dplyr::filter(party_name_map != "non-voters") %>%
    dplyr::pull(stack) <= max(df_long_full$stack[df_long_full$party_name_map != "non-voters"], na.rm = TRUE)
))

cat("Full long dataset rows      :", nrow(df_long_full), "\n")
cat("Current-vote valid rows     :", nrow(df_long_valid_now), "\n")
cat("Lagged-vote valid rows      :", nrow(df_long_valid_lag), "\n")
cat("Both-period valid rows      :", nrow(df_long_valid_both), "\n")

print(table(df_long_full$now_matches))
print(table(df_long_full$lag_matches))

# ------------------------------------------------
# 8. Enforce consistent schema before saving
# ------------------------------------------------

coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id",
      "peid", "peid_map",
      "party_name_map", "partyabbrev_map",
      "parfam", "parfam_harmonized", "parfam_final",
      "switch_to", "switch_from",
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
      "stay", "valid_now", "valid_lag", "valid_both"
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
# 9. Save standard outputs
# ------------------------------------------------

output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "cz_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "cz_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "cz_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "cz_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned Czech Republic files:\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")