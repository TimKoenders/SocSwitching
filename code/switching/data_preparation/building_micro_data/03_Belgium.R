# ================================================================
# 03_Belgium.R
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
# 1. Country inputs
# ------------------------------------------------
country_prefix <- "BE"
country_name   <- "Belgium"

input_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/be_data_file.RData"
output_dir  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

load(input_rdata)

if (!exists("data_file")) {
  stop("data_file not found after loading")
}

df <- data_file$data

cat("\n========================================\n")
cat("Loaded country file for:", country_name, "\n")
cat("Input path:", input_rdata, "\n")
cat("========================================\n\n")

# ------------------------------------------------
# 2. Load mappings
# ------------------------------------------------
mappings <- voteswitchR::mappings

map_be <- mappings %>%
  dplyr::filter(stringr::str_starts(elec_id, "BE")) %>%
  dplyr::transmute(
    elec_id,
    stack = as.numeric(stack),
    peid_map = peid,
    party_name_map = party_name,
    partyabbrev_map = partyabbrev,
    parfam,
    parfam_harmonized
  ) %>%
  dplyr::distinct()

cat("\n========================================\n")
cat("Mapping diagnostics for:", country_name, "\n")
cat("========================================\n\n")

map_be %>%
  dplyr::distinct(parfam, parfam_harmonized) %>%
  dplyr::arrange(parfam_harmonized) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

# ------------------------------------------------
# 3. Reshape stacked party blocks from wide to long
# ------------------------------------------------
df_long <- df %>%
  tidyr::pivot_longer(
    cols = matches("^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_"),
    names_to = c(".value", "alt"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  dplyr::mutate(
    alt = as.integer(alt),
    stack = as.numeric(stack)
  ) %>%
  dplyr::filter(!is.na(stack)) %>%
  dplyr::left_join(map_be, by = c("elec_id", "stack"))

stopifnot("peid_map" %in% names(df_long))
stopifnot(sum(is.na(df_long$peid_map)) == 0)

df_long <- df_long %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::case_when(
      elec_id == "BE-WA-2019-05" &
        stack == 1 &
        peid_map == "Democratic Federalist Independent (BE-WA-2019-05)" ~ "mrp",
      TRUE ~ parfam_harmonized
    ),
    parfam = dplyr::case_when(
      elec_id == "BE-WA-2019-05" &
        stack == 1 &
        peid_map == "Democratic Federalist Independent (BE-WA-2019-05)" ~ "mrp",
      TRUE ~ parfam
    )
  )

# ------------------------------------------------
# 4. Initial vote indicators
# ------------------------------------------------
invalid_vote_codes <- c(97, 98, 99, 997, 998, 999)

df_long <- df_long %>%
  dplyr::mutate(
    vote_raw = vote,
    l_vote_raw = l_vote,
    pid_raw = pid,
    vote = ifelse(vote %in% invalid_vote_codes, NA, vote),
    l_vote = ifelse(l_vote %in% invalid_vote_codes, NA, l_vote),
    pid = ifelse(pid %in% invalid_vote_codes, NA, pid),
    parfam_final = parfam_harmonized,
    voted_now = vote == stack,
    voted_lag = l_vote == stack
  )

# ------------------------------------------------
# 5. Add generalized non-party alternative before saving
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
  
  if ("parfam_harmonized_original" %in% names(non_rows)) {
    non_rows <- non_rows %>% dplyr::mutate(parfam_harmonized_original = "non")
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
      switch_to_tmp = dplyr::if_else(voted_now, parfam_final, NA_character_),
      switch_from_tmp = dplyr::if_else(voted_lag, parfam_final, NA_character_)
    ) %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::mutate(
      switch_to = switch_to_tmp[voted_now][1],
      switch_from = switch_from_tmp[voted_lag][1],
      stay = switch_to == switch_from
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-vote_match, -l_vote_match, -switch_to_tmp, -switch_from_tmp)
  
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

df_long <- add_non_alternative_before_saving(df_long)


df_long %>%
  dplyr::summarise(
    switch_to_class = paste(class(switch_to), collapse = ", "),
    switch_from_class = paste(class(switch_from), collapse = ", "),
    stay_class = paste(class(stay), collapse = ", ")
  )

df_long %>%
  dplyr::filter(voted_now) %>%
  dplyr::count(parfam_final, switch_to, sort = TRUE) %>%
  print(n = Inf)

df_long %>%
  dplyr::filter(voted_lag) %>%
  dplyr::count(parfam_final, switch_from, sort = TRUE) %>%
  print(n = Inf)

df_long %>%
  dplyr::filter(voted_now) %>%
  dplyr::summarise(
    n = dplyr::n(),
    non_now = sum(switch_to == "non", na.rm = TRUE),
    share_non_now = mean(switch_to == "non", na.rm = TRUE)
  )

df_long %>%
  dplyr::filter(voted_lag) %>%
  dplyr::summarise(
    n = dplyr::n(),
    non_lag = sum(switch_from == "non", na.rm = TRUE),
    share_non_lag = mean(switch_from == "non", na.rm = TRUE)
  )

df_long %>%
  dplyr::filter(voted_now) %>%
  dplyr::count(switch_from, switch_to, sort = TRUE) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

df_long %>%
  dplyr::filter(voted_now | voted_lag, is.na(parfam_final)) %>%
  dplyr::count(elec_id, vote_raw, l_vote_raw, stack, peid_map, party_name_map, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------
# 6. Standard output datasets
# ------------------------------------------------
df_long_full <- df_long

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 7. Enforce consistent schema before saving
# ------------------------------------------------
coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id", "respondent_election_uid",
      "peid", "peid_map",
      "party_name_map", "partyabbrev", "partyabbrev_map",
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
      "vote_raw", "l_vote_raw", "pid_raw",
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
      "stay",
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


# ------------------------------------------------
# 8. Save standard outputs
# ------------------------------------------------
output_rdata_full <- file.path(output_dir, "be_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "be_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "be_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "be_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned country files for:", country_name, "\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")

