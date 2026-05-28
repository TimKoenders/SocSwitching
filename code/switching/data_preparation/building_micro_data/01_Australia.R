# ================================================================
# 01_Australia.R
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
})

# ------------------------------------------------
# 1. Check package versions
# ------------------------------------------------
cat("\n========================================\n")
cat("voteswitchR version:", as.character(packageVersion("voteswitchR")), "\n")
cat("shiny version      :", as.character(packageVersion("shiny")), "\n")
cat("========================================\n\n")

# ------------------------------------------------
# 2. Inspect package data objects if needed
# ------------------------------------------------
data(package = "voteswitchR")


# ------------------------------------------------
# 3. Launch the Shiny app for data procurement/build
# ------------------------------------------------
data_file <- voteswitchR::build_data_file()

# ------------------------------------------------
# 4. Set country-specific inputs
# ------------------------------------------------
country_prefix <- "AU"
country_name <- "Australia"

input_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/au_data_file.RData"
output_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/au_df_long.RData"

# Optional country-specific family corrections.
nat_party_overrides <- c("One Nation")

party_name_corrections <- c(
  "Austrlian Democrats" = "Australian Democrats"
)

# ------------------------------------------------
# 5. Load country-specific data file
# ------------------------------------------------
load(input_rdata)
cat("\n========================================\n")
cat("Loaded country file for:", country_name, "\n")
cat("Input path:", input_rdata, "\n")
cat("========================================\n\n")

names(data_file)
names(data_file$data)
names(data_file$info_aux)

df_wide <- data_file$data

# ------------------------------------------------
# 6. Load package mappings for selected country
# ------------------------------------------------
data("mappings", package = "voteswitchR")

map_ctry <- mappings %>%
  dplyr::filter(countryname == country_name)

cat("\n========================================\n")
cat("Mapping diagnostics for:", country_name, "\n")
cat("========================================\n\n")

map_ctry %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_elections = dplyr::n_distinct(elec_id)
  ) %>%
  print()

x <- map_ctry %>%
  dplyr::count(elec_id, sort = TRUE)

x

map_ctry %>%
  dplyr::distinct(parfam, parfam_harmonized) %>%
  dplyr::arrange(parfam_harmonized) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ------------------------------------------------
# 7. Reshape stacked party blocks from wide to long
# ------------------------------------------------
df_long <- df_wide %>%
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

df_long %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_ids = dplyr::n_distinct(id)
  ) %>%
  print()

df_long %>%
  dplyr::count(elec_id, alt, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------
# 8. Keep only valid modeled alternatives
# ------------------------------------------------
# Extra reshaped alternatives have missing stack and are not part of the
# modeled choice set.
df_long_valid <- df_long %>%
  dplyr::filter(!is.na(stack))

df_long_valid %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_missing_stack = sum(is.na(stack))
  ) %>%
  print()

# ------------------------------------------------
# 9. Attach party metadata using elec_id + stack
# ------------------------------------------------
# stack is the correct micro-level choice-set alignment variable.
# peid is useful as an interpretable party-election label, but the
# metadata join should be done on elec_id + stack.
ctry_party_map <- map_ctry %>%
  dplyr::select(
    elec_id,
    stack,
    peid_map = peid,
    party_name_map = party_name,
    partyabbrev,
    parfam,
    parfam_harmonized,
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
# 10. Check mapping coverage
# ------------------------------------------------
df_long_valid %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_missing_peid_map = sum(is.na(peid_map)),
    n_missing_parfam = sum(is.na(parfam)),
    n_missing_parfam_harmonized = sum(is.na(parfam_harmonized))
  ) %>%
  print()

df_long_valid %>%
  dplyr::summarise(
    n_nonmissing_both = sum(!is.na(peid) & !is.na(peid_map)),
    n_exact_match = sum(peid == peid_map, na.rm = TRUE),
    prop_exact_match = mean(peid == peid_map, na.rm = TRUE)
  ) %>%
  print()

# ------------------------------------------------
# 11. Define dyadic vote indicators
# ------------------------------------------------
# The correct alignment is with stack, not map_vote.
df_long_valid <- df_long_valid %>%
  dplyr::mutate(
    voted_now = as.logical(vote == stack),
    voted_lag = as.logical(l_vote == stack),
    switch_to = voted_now & !voted_lag,
    switch_from = !voted_now & voted_lag,
    stay = voted_now & voted_lag
  )

df_long_valid %>%
  dplyr::summarise(
    switch_to = sum(switch_to, na.rm = TRUE),
    switch_from = sum(switch_from, na.rm = TRUE),
    stay = sum(stay, na.rm = TRUE)
  ) %>%
  print()

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
    max_lag = max(lag_matches)
  ) %>%
  print()

# ------------------------------------------------
# 12. Inspect empirical family mapping
# ------------------------------------------------
df_long_valid %>%
  dplyr::distinct(parfam, parfam_harmonized) %>%
  dplyr::arrange(parfam_harmonized) %>%
  print(n = Inf)

df_long_valid %>%
  dplyr::filter(parfam_harmonized == "nat") %>%
  dplyr::distinct(elec_id, party_name_map) %>%
  dplyr::arrange(elec_id) %>%
  print(n = Inf)

# ------------------------------------------------
# 13a. Final family variable and country-specific corrections
# ------------------------------------------------
df_long_valid <- df_long_valid %>%
  dplyr::mutate(
    party_name_map = dplyr::recode(party_name_map, !!!party_name_corrections),
    parfam_final = dplyr::case_when(
      party_name_map %in% nat_party_overrides ~ "nat",
      TRUE ~ parfam_harmonized
    )
  )

df_long_valid %>%
  dplyr::count(parfam_final, party_name_map, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------
# 13b. Add generalized non-party alternative
# ------------------------------------------------
add_non_alternative <- function(df, max_stack) {
  
  non_rows <- df %>%
    dplyr::distinct(elec_id, id, .keep_all = TRUE) %>%
    dplyr::mutate(
      alt = 0,
      stack = max_stack + 1,
      peid = NA_character_,
      peid_map = "non",
      party_name_map = "non-voters",
      partyabbrev = "non",
      parfam = "non",
      parfam_harmonized = "non",
      parfam_final = "non"
    )
  
  non_rows <- non_rows[, names(df)]
  
  dplyr::bind_rows(df, non_rows) %>%
    dplyr::mutate(
      vote_match = dplyr::if_else(is.na(vote) | vote > max_stack, max_stack + 1, vote),
      l_vote_match = dplyr::if_else(is.na(l_vote) | l_vote > max_stack, max_stack + 1, l_vote),
      voted_now = vote_match == stack,
      voted_lag = l_vote_match == stack,
      switch_to = voted_now & !voted_lag,
      switch_from = !voted_now & voted_lag,
      stay = voted_now & voted_lag
    ) %>%
    dplyr::select(-vote_match, -l_vote_match)
}

max_stack <- max(df_long_valid$stack, na.rm = TRUE)

df_long_valid <- add_non_alternative(df_long_valid, max_stack)


# ------------------------------------------------
# 14. Respondent-level matching indicators
# ------------------------------------------------
match_flags <- df_long_valid %>%
  dplyr::group_by(elec_id, id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now %in% TRUE, na.rm = TRUE),
    lag_matches = sum(voted_lag %in% TRUE, na.rm = TRUE),
    valid_now  = now_matches == 1,
    valid_lag  = lag_matches == 1,
    valid_both = now_matches == 1 & lag_matches == 1,
    .groups = "drop"
  )

df_long_valid <- df_long_valid %>%
  dplyr::left_join(match_flags, by = c("elec_id", "id"))

# ------------------------------------------------
# 15. Standard output datasets
# ------------------------------------------------
df_long_full <- df_long_valid

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 16. Enforce consistent schema before saving
# ------------------------------------------------
coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id",
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
# 17. Save standard outputs
# ------------------------------------------------
output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "au_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "au_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "au_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "au_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned country files for:", country_name, "\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")




