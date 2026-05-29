# ================================================================
# 20_Netherlands.R
# Build, validate, and clean country-specific microdata file
# from voteswitchR
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
  library(haven)
  library(purrr)
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
# data_file is loaded from the generated voteswitchR country bundle below.


# ------------------------------------------------
# 3. Set country-specific inputs
# ------------------------------------------------
country_prefix <- "NL"
country_name   <- "Netherlands"

input_rdata <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro", "nl_data_file.RData")
output_dir  <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "micro")

output_rdata_full <- file.path(output_dir, "nl_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "nl_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "nl_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "nl_df_long_valid_both.RData")

# ------------------------------------------------
# 4. Load previously built data_file object
# ------------------------------------------------
if (file.exists(input_rdata)) {
  load(input_rdata)
  
  if (!exists("data_file")) {
    stop("Object 'data_file' was not found after loading: ", input_rdata)
  }
  
  cat("\n========================================\n")
  cat("Loaded existing data_file for:", country_name, "\n")
  cat("Input path:", input_rdata, "\n")
  cat("========================================\n\n")
} else {
  cat("\n========================================\n")
  cat("No existing input_rdata found. Using data_file from Shiny build.\n")
  cat("Expected path:", input_rdata, "\n")
  cat("========================================\n\n")
}

# ------------------------------------------------
# 5. Helper objects and functions
# ------------------------------------------------
print_header <- function(x) {
  cat("\n========================================\n")
  cat(x, "\n")
  cat("========================================\n")
}

find_df_inside_data_file <- function(x) {
  if (is.data.frame(x)) return(x)
  
  nms <- names(x)
  if (is.null(nms)) stop("data_file has no names and is not a data frame.")
  
  df_candidates <- nms[vapply(x, is.data.frame, logical(1))]
  
  if ("data" %in% df_candidates) return(x[["data"]])
  if (length(df_candidates) == 1) return(x[[df_candidates]])
  
  cat("\nData-frame candidates inside data_file:\n")
  print(df_candidates)
  stop("Could not uniquely determine the respondent-level data frame inside data_file.")
}

find_df_source_name <- function(x) {
  if (is.data.frame(x)) return("data_file")
  
  nms <- names(x)
  if (is.null(nms)) return(NA_character_)
  
  df_candidates <- nms[vapply(x, is.data.frame, logical(1))]
  
  if ("data" %in% df_candidates) return("data_file$data")
  if (length(df_candidates) == 1) return(paste0("data_file$", df_candidates))
  
  NA_character_
}

invalid_vote_codes <- c(97, 98, 99, 997, 998, 999)


# ------------------------------------------------
# 6. Diagnostics before cleaning
# ------------------------------------------------
print_header("Diagnostics before cleaning")

cat("\nClass of data_file:\n")
print(class(data_file))

cat("\nStructure of data_file (max.level = 1):\n")
str(data_file, max.level = 1)

cat("\nNames(data_file):\n")
print(names(data_file))

df_source <- find_df_source_name(data_file)
df <- find_df_inside_data_file(data_file)

cat("\nAssigned df from:\n")
print(df_source)

cat("\nNames(df):\n")
print(names(df))

cat("\nDimensions of df:\n")
print(dim(df))

cat("\nCompact glimpse of df:\n")
dplyr::glimpse(df)

# ------------------------------------------------
# 7. Final cleaning with generalized non-party alternative
# ------------------------------------------------
print_header("Final cleaning with generalized non-party alternative")

mappings <- getFromNamespace("mappings", "voteswitchR")

mapping_country <- mappings %>%
  dplyr::filter(iso2c == country_prefix)

mapping_join <- mapping_country %>%
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

df_long <- df %>%
  tidyr::pivot_longer(
    cols = matches("^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_[0-9]+$"),
    names_to = c(".value", "alt"),
    names_pattern = "^(.*)_([0-9]+)$"
  ) %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack),
    vote = as.numeric(vote),
    l_vote = as.numeric(l_vote)
  ) %>%
  dplyr::filter(!is.na(stack)) %>%
  dplyr::left_join(mapping_join, by = c("elec_id", "stack")) %>%
  dplyr::mutate(
    parfam_final = parfam_harmonized
  )

add_non_alternative_before_saving <- function(df) {
  
  df <- df %>%
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
    dplyr::group_by(elec_id, id) %>%
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

cat("\nDistribution of now_matches:\n")
df_long %>%
  dplyr::distinct(elec_id, id, now_matches) %>%
  dplyr::count(now_matches, sort = FALSE) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

cat("\nDistribution of lag_matches:\n")
df_long %>%
  dplyr::distinct(elec_id, id, lag_matches) %>%
  dplyr::count(lag_matches, sort = FALSE) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

# ------------------------------------------------
# 8. Standard output datasets
# ------------------------------------------------
print_header("Construct standard output datasets")

df_long_full <- df_long

df_long_valid_now <- df_long_full %>%
  dplyr::filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  dplyr::filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  dplyr::filter(valid_both)

cat("\nDimensions of full long data:\n")
print(dim(df_long_full))

cat("\nDimensions of current-vote valid data:\n")
print(dim(df_long_valid_now))

cat("\nDimensions of lagged-vote valid data:\n")
print(dim(df_long_valid_lag))

cat("\nDimensions of both-period valid data:\n")
print(dim(df_long_valid_both))

# ------------------------------------------------
# 9. Final validation checks
# ------------------------------------------------
print_header("Final validation checks")

cat("\nMissing peid_map in full long data:\n")
print(sum(is.na(df_long_full$peid_map)))

cat("\nMissing key identifiers:\n")
print(
  df_long_full %>%
    dplyr::summarise(
      missing_elec_id = sum(is.na(elec_id)),
      missing_id = sum(is.na(id)),
      missing_stack = sum(is.na(stack)),
      missing_peid_map = sum(is.na(peid_map))
    )
)

if ("peid" %in% names(df_long_full)) {
  cat("\nNumber of non-missing peid / peid_map mismatches:\n")
  print(
    df_long_full %>%
      dplyr::filter(!is.na(peid), !is.na(peid_map), peid_map != "non") %>%
      dplyr::summarise(n = sum(as.character(peid) != as.character(peid_map))) %>%
      dplyr::pull(n)
  )
}

stopifnot(sum(is.na(df_long_full$elec_id)) == 0)
stopifnot(sum(is.na(df_long_full$id)) == 0)
stopifnot(sum(is.na(df_long_full$stack)) == 0)
stopifnot(sum(is.na(df_long_full$peid_map)) == 0)

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

stopifnot(all(
  df_long_full %>%
    dplyr::filter(party_name_map != "non-voters") %>%
    dplyr::pull(stack) <= max(df_long_full$stack[df_long_full$party_name_map != "non-voters"], na.rm = TRUE)
))

if ("peid" %in% names(df_long_full)) {
  stopifnot(
    df_long_full %>%
      dplyr::filter(!is.na(peid), !is.na(peid_map), peid_map != "non") %>%
      dplyr::summarise(ok = all(as.character(peid) == as.character(peid_map))) %>%
      dplyr::pull(ok)
  )
}

cat("Full long dataset rows      :", nrow(df_long_full), "\n")
cat("Current-vote valid rows     :", nrow(df_long_valid_now), "\n")
cat("Lagged-vote valid rows      :", nrow(df_long_valid_lag), "\n")
cat("Both-period valid rows      :", nrow(df_long_valid_both), "\n")

print(table(df_long_full$now_matches))
print(table(df_long_full$lag_matches))

# ------------------------------------------------
# 10. Enforce consistent schema before saving
# ------------------------------------------------
print_header("Enforce consistent schema before saving")

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
      "vote_share", "vote_share_lag", "turnout", "turnout_lag",
      "now_matches", "lag_matches"
    ),
    names(df)
  )
  
  logi_vars <- intersect(
    c(
      "voted_now", "voted_lag", "stay", "valid_now", "valid_lag", "valid_both"
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
# 11. Save standard outputs
# ------------------------------------------------
print_header("Save standard outputs")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned country files for:", country_name, "\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")