# ================================================================
# 22_Norway.R
# Build, validate, and clean country-specific microdata file
# from voteswitchR
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
  message(
    "Installing shiny ", required_shiny_version,
    " for build_data_file() compatibility..."
  )
  remotes::install_version(
    "shiny",
    version = required_shiny_version,
    upgrade = "never"
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
data_file <- voteswitchR::build_data_file()

# ------------------------------------------------
# 3. Set country-specific inputs
# ------------------------------------------------
country_prefix <- "NO"
country_name   <- "Norway"

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/no_data_file.RData"
output_dir   <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "no_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "no_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "no_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "no_df_long_valid_both.RData")

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
  cat("No existing input_rdata found. Using current in-memory data_file.\n")
  cat("Expected path:", input_rdata, "\n")
  cat("========================================\n\n")
}

# ------------------------------------------------
# 5. Minimal diagnostics before cleaning
# ------------------------------------------------
cat("\n========================================\n")
cat("Minimal diagnostics before cleaning\n")
cat("========================================\n")

print_header <- function(x) {
  cat("\n========================================\n")
  cat(x, "\n")
  cat("========================================\n")
}

find_df_inside_data_file <- function(x) {
  if (is.data.frame(x)) return(x)
  
  nms <- names(x)
  if (is.null(nms)) {
    stop("data_file has no names and is not a data frame.")
  }
  
  df_candidates <- nms[vapply(x, is.data.frame, logical(1))]
  
  if ("data" %in% df_candidates) {
    return(x[["data"]])
  }
  
  if (length(df_candidates) == 1) {
    return(x[[df_candidates]])
  }
  
  stop("Could not uniquely determine the respondent-level data frame inside data_file.")
}

find_df_source_name <- function(x) {
  if (is.data.frame(x)) return("data_file")
  
  nms <- names(x)
  if (is.null(nms)) return(NA_character_)
  
  df_candidates <- nms[vapply(x, is.data.frame, logical(1))]
  
  if ("data" %in% df_candidates) {
    return("data_file$data")
  }
  
  if (length(df_candidates) == 1) {
    return(paste0("data_file$", df_candidates))
  }
  
  NA_character_
}

invalid_vote_codes <- c(97, 98, 99, 997, 998, 999)

# 5.1 Identify df
print_header("Top-level inspection and df assignment")

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

cat("\nDimensions of df:\n")
print(dim(df))

cat("\nFirst variable names of df:\n")
print(head(names(df), 40))

# 5.2 Election coverage
print_header("Election coverage")

if (!"elec_id" %in% names(df)) {
  stop("Variable 'elec_id' not found in df.")
}

cat("\nObserved elec_id values:\n")
print(sort(unique(df$elec_id)))

cat("\nCounts by elec_id:\n")
print(df %>% dplyr::count(elec_id, sort = FALSE))

available_data <- getFromNamespace("available_data", "voteswitchR")

expected_contexts <- available_data %>%
  dplyr::filter(iso2c == country_prefix) %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id) %>%
  dplyr::pull(elec_id)

cat("\nExpected contexts from available_data:\n")
print(expected_contexts)

cat("\nExpected but missing in df:\n")
print(setdiff(expected_contexts, unique(df$elec_id)))

cat("\nPresent in df but not expected:\n")
print(setdiff(unique(df$elec_id), expected_contexts))

# 5.3 Wide structure
print_header("Alternative-specific wide structure")

all_vars <- names(df)

stack_vars     <- grep("^stack_[0-9]+$", all_vars, value = TRUE)
peid_vars      <- grep("^peid_[0-9]+$", all_vars, value = TRUE)
party_vars     <- grep("^party_[0-9]+$", all_vars, value = TRUE)
partyharm_vars <- grep("^party_harmonized_[0-9]+$", all_vars, value = TRUE)
mapvote_vars   <- grep("^map_vote_[0-9]+$", all_vars, value = TRUE)
maplr_vars     <- grep("^map_lr_[0-9]+$", all_vars, value = TRUE)

cat("\nstack_* variables:\n")
print(stack_vars)

cat("\npeid_* variables:\n")
print(peid_vars)

cat("\nparty_* variables:\n")
print(party_vars)

cat("\nparty_harmonized_* variables:\n")
print(partyharm_vars)

cat("\nmap_vote_* variables:\n")
print(mapvote_vars)

cat("\nmap_lr_* variables:\n")
print(maplr_vars)

alt_nums <- c(
  as.integer(stringr::str_extract(stack_vars, "\\d+$")),
  as.integer(stringr::str_extract(peid_vars, "\\d+$")),
  as.integer(stringr::str_extract(party_vars, "\\d+$")),
  as.integer(stringr::str_extract(partyharm_vars, "\\d+$")),
  as.integer(stringr::str_extract(mapvote_vars, "\\d+$")),
  as.integer(stringr::str_extract(maplr_vars, "\\d+$"))
)

alt_nums <- alt_nums[!is.na(alt_nums)]

cat("\nMaximum alternative number detected:\n")
print(if (length(alt_nums) == 0) NA_integer_ else max(alt_nums))

# 5.4 Diagnostic reshape
print_header("Diagnostic reshape")

stub_pattern <- "^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_[0-9]+$"

long_diag <- df %>%
  tidyr::pivot_longer(
    cols = matches(stub_pattern),
    names_to = c(".value", "alt"),
    names_pattern = "^(.*)_([0-9]+)$"
  ) %>%
  dplyr::mutate(alt = as.integer(alt))

cat("\nDimensions after reshape:\n")
print(dim(long_diag))

cat("\nCounts by elec_id and alt:\n")
print(long_diag %>% dplyr::count(elec_id, alt, sort = FALSE))

cat("\nRows with missing stack:\n")
print(sum(is.na(long_diag$stack)))

cat("\nCounts by elec_id and stack missingness:\n")
print(long_diag %>% dplyr::count(elec_id, stack_missing = is.na(stack), sort = FALSE))

# 5.5 Remove padded alternatives
print_header("Drop padded alternatives")

long_nopad <- long_diag %>%
  dplyr::filter(!is.na(stack))

cat("\nDimensions after dropping padded alternatives:\n")
print(dim(long_nopad))

cat("\nCounts by elec_id after dropping padded alternatives:\n")
print(long_nopad %>% dplyr::count(elec_id, sort = FALSE))

# 5.6 Mapping join diagnostic
print_header("Mapping join diagnostic")

mappings <- getFromNamespace("mappings", "voteswitchR")

mapping_join <- mappings %>%
  dplyr::filter(iso2c == country_prefix) %>%
  dplyr::transmute(
    elec_id,
    stack = as.numeric(stack),
    peid_map = peid,
    parfam,
    parfam_harmonized
  ) %>%
  dplyr::distinct()

df_mapcheck <- long_nopad %>%
  dplyr::mutate(stack = as.numeric(stack)) %>%
  dplyr::left_join(mapping_join, by = c("elec_id", "stack"))

cat("\nMissing peid_map after join:\n")
print(sum(is.na(df_mapcheck$peid_map)))

if ("peid" %in% names(df_mapcheck)) {
  cat("\npeid agreement with peid_map:\n")
  print(
    df_mapcheck %>%
      dplyr::filter(!is.na(peid), !is.na(peid_map)) %>%
      dplyr::mutate(match = as.character(peid) == as.character(peid_map)) %>%
      dplyr::count(match)
  )
}

# 5.7 Vote-match diagnostic
print_header("Vote-match diagnostic")

df_votecheck <- df_mapcheck %>%
  dplyr::mutate(
    vote = ifelse(vote %in% invalid_vote_codes, NA, vote),
    l_vote = ifelse(l_vote %in% invalid_vote_codes, NA, l_vote),
    voted_now = vote == stack,
    voted_lag = l_vote == stack
  )

match_diag <- df_votecheck %>%
  dplyr::group_by(elec_id, id) %>%
  dplyr::summarise(
    now_matches = sum(voted_now %in% TRUE, na.rm = TRUE),
    lag_matches = sum(voted_lag %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nDistribution of now_matches:\n")
print(match_diag %>% dplyr::count(now_matches, sort = FALSE))

cat("\nDistribution of lag_matches:\n")
print(match_diag %>% dplyr::count(lag_matches, sort = FALSE))

cat("\nCases with now_matches > 1:\n")
print(match_diag %>% dplyr::filter(now_matches > 1))

cat("\nCases with lag_matches > 1:\n")
print(match_diag %>% dplyr::filter(lag_matches > 1))

# ------------------------------------------------
# 6. Final cleaning with generalized non-party alternative
# ------------------------------------------------
print_header("Final cleaning with generalized non-party alternative")

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

df_long <- long_nopad %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack),
    vote = as.numeric(vote),
    l_vote = as.numeric(l_vote)
  ) %>%
  dplyr::left_join(mapping_join, by = c("elec_id", "stack")) %>%
  dplyr::mutate(parfam_final = parfam_harmonized)

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

# ------------------------------------------------
# 7. Standard output datasets
# ------------------------------------------------
print_header("Construct standard output datasets")

df_long_full <- df_long
df_long_valid_now  <- df_long_full %>% dplyr::filter(valid_now)
df_long_valid_lag  <- df_long_full %>% dplyr::filter(valid_lag)
df_long_valid_both <- df_long_full %>% dplyr::filter(valid_both)

# ------------------------------------------------
# 8. Final validation checks
# ------------------------------------------------
print_header("Final validation checks")

stopifnot(sum(is.na(df_long_full$stack) | is.nan(df_long_full$stack)) == 0)
stopifnot(sum(is.na(df_long_full$peid_map)) == 0)

stopifnot(all(df_long_full %>%
                dplyr::group_by(elec_id, id) %>%
                dplyr::summarise(n = sum(voted_now, na.rm = TRUE), .groups = "drop") %>%
                dplyr::pull(n) == 1))

stopifnot(all(df_long_full %>%
                dplyr::group_by(elec_id, id) %>%
                dplyr::summarise(n = sum(voted_lag, na.rm = TRUE), .groups = "drop") %>%
                dplyr::pull(n) == 1))

if ("peid" %in% names(df_long_full)) {
  stopifnot(df_long_full %>%
              dplyr::filter(!is.na(peid), !is.na(peid_map), peid_map != "non") %>%
              dplyr::summarise(ok = all(as.character(peid) == as.character(peid_map))) %>%
              dplyr::pull(ok))
}

# ------------------------------------------------
# 9. Enforce consistent schema before saving
# ------------------------------------------------
print_header("Enforce consistent schema before saving")

coerce_types <- function(df) {
  char_vars <- intersect(
    c("iso2c", "elec_id", "id", "peid", "peid_map", "party_name_map",
      "partyabbrev_map", "parfam", "parfam_harmonized", "parfam_final",
      "switch_to", "switch_from", "map_lr", "region", "source_file"),
    names(df)
  )
  
  num_vars <- intersect(
    c("year", "election_date", "weights", "male", "age", "lr_self",
      "strength1", "strength2", "stfdem", "alt", "stack", "vote",
      "l_vote", "pid", "party", "party_harmonized", "map_vote",
      "vote_share", "vote_share_lag", "turnout", "turnout_lag",
      "now_matches", "lag_matches"),
    names(df)
  )
  
  logi_vars <- intersect(
    c("voted_now", "voted_lag", "stay", "valid_now", "valid_lag", "valid_both"),
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