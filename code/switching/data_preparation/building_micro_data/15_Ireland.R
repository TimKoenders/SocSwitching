# ================================================================
# 15_Ireland.R
# Build, validate, and clean country-specific microdata file
# from voteswitchR
# Non-voting integrated as core alternative
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
# 1. Country inputs
# ------------------------------------------------

country_prefix <- "IE"
country_name   <- "Ireland"

input_rdata <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro", "ie_data_file.RData")
output_dir  <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro")

output_rdata_full <- file.path(output_dir, "ie_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "ie_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "ie_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "ie_df_long_valid_both.RData")

invalid_vote_codes <- c(97, 98, 99, 997, 998, 999)

print_header <- function(x) {
  cat("\n========================================\n")
  cat(x, "\n")
  cat("========================================\n")
}

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------

if (file.exists(input_rdata)) {
  load(input_rdata)
} else {
  # data_file is loaded from the generated voteswitchR country bundle below.
  save(data_file, file = input_rdata)
}

if (!exists("data_file")) {
  stop("Object 'data_file' was not found.")
}

df <- if (is.data.frame(data_file)) {
  data_file
} else if ("data" %in% names(data_file) && is.data.frame(data_file$data)) {
  data_file$data
} else {
  df_candidates <- names(data_file)[vapply(data_file, is.data.frame, logical(1))]
  if (length(df_candidates) == 1) {
    data_file[[df_candidates]]
  } else {
    stop("Could not uniquely determine the respondent-level data frame inside data_file.")
  }
}

cat("\nLoaded country file for:", country_name, "\n")
cat("Rows:", nrow(df), "\n")

# ------------------------------------------------
# 3. Load mappings
# ------------------------------------------------

mappings <- voteswitchR::mappings

mapping_join <- mappings %>%
  dplyr::filter(iso2c == country_prefix) %>%
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

stopifnot(nrow(mapping_join) > 0)

# ------------------------------------------------
# 4. Reshape stacked party blocks from wide to long
# ------------------------------------------------

stub_pattern <- "^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_[0-9]+$"

long_diag <- df %>%
  tidyr::pivot_longer(
    cols = matches(stub_pattern),
    names_to = c(".value", "alt"),
    names_pattern = "^(.*)_([0-9]+)$"
  ) %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack)
  )

long_nopad <- long_diag %>%
  dplyr::filter(!is.na(stack))

# ------------------------------------------------
# 5. Initial cleaning and mapping join
# ------------------------------------------------

df_long <- long_nopad %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack),
    vote_raw = as.numeric(vote),
    l_vote_raw = as.numeric(l_vote),
    vote = dplyr::if_else(vote_raw %in% invalid_vote_codes, NA_real_, vote_raw),
    l_vote = dplyr::if_else(l_vote_raw %in% invalid_vote_codes, NA_real_, l_vote_raw)
  ) %>%
  dplyr::left_join(mapping_join, by = c("elec_id", "stack")) %>%
  dplyr::mutate(
    parfam_final = parfam_harmonized
  )

stopifnot(sum(is.na(df_long$peid_map)) == 0)

# ------------------------------------------------
# 6. Add generalized non-party alternative before saving
# ------------------------------------------------

add_non_alternative_before_saving <- function(df) {
  
  df <- df %>%
    dplyr::select(
      -dplyr::any_of(c(
        "voted_now", "voted_lag",
        "switch_to", "switch_from", "stay",
        "valid_now", "valid_lag", "valid_both",
        "now_matches", "lag_matches"
      ))
    ) %>%
    dplyr::mutate(
      alt = as.numeric(alt),
      stack = as.numeric(stack),
      vote = as.numeric(vote),
      l_vote = as.numeric(l_vote)
    )
  
  old_max_stack <- max(df$stack, na.rm = TRUE)
  non_stack <- old_max_stack + 1
  
  non_rows <- df %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stack = non_stack,
      alt = 0,
      peid = NA_character_,
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
  
  non_rows <- non_rows[, names(df)]
  
  df_out <- df %>%
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
      switch_to_tmp = dplyr::if_else(voted_now, parfam_final, NA_character_),
      switch_from_tmp = dplyr::if_else(voted_lag, parfam_final, NA_character_)
    ) %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::mutate(
      switch_to = switch_to_tmp[voted_now][1],
      switch_from = switch_from_tmp[voted_lag][1],
      stay = switch_to == switch_from,
      now_matches = sum(voted_now, na.rm = TRUE),
      lag_matches = sum(voted_lag, na.rm = TRUE),
      valid_now = now_matches == 1,
      valid_lag = lag_matches == 1,
      valid_both = valid_now & valid_lag
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-switch_to_tmp, -switch_from_tmp)
  
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

# ------------------------------------------------
# 7. Standard output datasets
# ------------------------------------------------

df_long_full <- df_long

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

# ------------------------------------------------
# 8. Validation checks
# ------------------------------------------------

print_header("Validation checks")

print(
  df_long_full %>%
    dplyr::summarise(
      switch_to_class = paste(class(switch_to), collapse = ", "),
      switch_from_class = paste(class(switch_from), collapse = ", "),
      stay_class = paste(class(stay), collapse = ", ")
    )
)

print(
  df_long_full %>%
    dplyr::filter(voted_now) %>%
    dplyr::count(parfam_final, switch_to, sort = TRUE),
  n = Inf
)

print(
  df_long_full %>%
    dplyr::filter(voted_lag) %>%
    dplyr::count(parfam_final, switch_from, sort = TRUE),
  n = Inf
)

print(
  df_long_full %>%
    dplyr::filter(voted_now) %>%
    dplyr::summarise(
      n = dplyr::n(),
      non_now = sum(switch_to == "non", na.rm = TRUE),
      share_non_now = mean(switch_to == "non", na.rm = TRUE),
      non_lag = sum(switch_from == "non", na.rm = TRUE),
      share_non_lag = mean(switch_from == "non", na.rm = TRUE)
    )
)

print(
  df_long_full %>%
    dplyr::filter(voted_now) %>%
    dplyr::count(switch_from, switch_to, sort = TRUE) %>%
    dplyr::mutate(share = n / sum(n)),
  n = Inf
)

print(
  df_long_full %>%
    dplyr::filter(voted_now | voted_lag, is.na(parfam_final)) %>%
    dplyr::count(elec_id, vote_raw, l_vote_raw, stack, peid_map, party_name_map, sort = TRUE),
  n = Inf
)

stopifnot(sum(is.na(df_long_full$stack) | is.nan(df_long_full$stack)) == 0)
stopifnot(sum(is.na(df_long_full$peid_map)) == 0)
stopifnot(is.character(df_long_full$switch_to))
stopifnot(is.character(df_long_full$switch_from))
stopifnot(is.logical(df_long_full$stay))

stopifnot(all(
  df_long_full %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::summarise(n = sum(voted_now, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

stopifnot(all(
  df_long_full %>%
    dplyr::group_by(elec_id, id) %>%
    dplyr::summarise(n = sum(voted_lag, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(n) == 1
))

# ------------------------------------------------
# 9. Enforce consistent schema before saving
# ------------------------------------------------

coerce_types <- function(df) {
  char_vars <- intersect(
    c(
      "iso2c", "elec_id", "id",
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
      "vote_raw", "l_vote_raw",
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
# 10. Save standard outputs
# ------------------------------------------------

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned country files for:", country_name, "\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")