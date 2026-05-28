# ================================================================
# 09_Finland.R
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
  library(haven)
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
country_prefix <- "FI"
country_name   <- "Finland"

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/fi_data_file.RData"
output_rdata <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/fi_df_long.RData"

# ------------------------------------------------
# 4. Load built country data_file object
# ------------------------------------------------
if (!file.exists(input_rdata)) {
  stop("Input file does not exist: ", input_rdata)
}

load(input_rdata)

if (!exists("data_file")) {
  stop("Object 'data_file' was not found after loading: ", input_rdata)
}

cat("\n========================================\n")
cat("Loaded data_file for:", country_name, "\n")
cat("Input path:", input_rdata, "\n")
cat("========================================\n\n")

# ------------------------------------------------
# 5. Basic object diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Basic object diagnostics\n")
cat("========================================\n")

cat("\nClass of data_file:\n")
print(class(data_file))

cat("\nNames(data_file):\n")
print(names(data_file))

if (!"data" %in% names(data_file)) {
  stop("data_file does not contain a 'data' element.")
}

cat("\nDimensions of data_file$data:\n")
print(dim(data_file$data))

cat("\nColumn names of data_file$data:\n")
print(names(data_file$data))

if ("info_aux" %in% names(data_file)) {
  cat("\nNames of data_file$info_aux:\n")
  print(names(data_file$info_aux))
} else {
  cat("\nNo info_aux element found in data_file.\n")
}

cat("\nCompact glimpse of data_file$data:\n")
dplyr::glimpse(data_file$data)

df <- data_file$data

# ------------------------------------------------
# 6. Election coverage diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Election coverage diagnostics\n")
cat("========================================\n")

if ("elec_id" %in% names(df)) {
  cat("\nDistinct elec_id values in final data:\n")
  print(sort(unique(df$elec_id)))
  
  cat("\nCounts by elec_id:\n")
  print(df %>% count(elec_id, sort = FALSE))
  
  cat("\nNumber of distinct elections:\n")
  print(dplyr::n_distinct(df$elec_id))
} else {
  cat("\nVariable 'elec_id' not found in data_file$data.\n")
}

if ("available_data" %in% names(data_file$info_aux)) {
  expected_contexts <- data_file$info_aux$available_data %>%
    dplyr::filter(.data$iso2c == country_prefix) %>%
    dplyr::pull(.data$elec_id) %>%
    unique() %>%
    sort()
  
  observed_contexts <- if ("elec_id" %in% names(df)) sort(unique(df$elec_id)) else character(0)
  
  cat("\nExpected contexts from available_data:\n")
  print(expected_contexts)
  
  cat("\nContexts present in final data:\n")
  print(observed_contexts)
  
  cat("\nExpected but not present in final data:\n")
  print(setdiff(expected_contexts, observed_contexts))
  
  cat("\nPresent in final data but not expected:\n")
  print(setdiff(observed_contexts, expected_contexts))
}

# ------------------------------------------------
# 7. ID diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("ID diagnostics\n")
cat("========================================\n")

id_candidates <- c("id", "resp_id", "caseid", "uid", "obs_id")
id_present <- id_candidates[id_candidates %in% names(df)]

cat("\nID-like variables present:\n")
print(id_present)

if ("id" %in% names(df)) {
  cat("\nNumber of missing ids:\n")
  print(sum(is.na(df$id)))
  
  cat("\nNumber of duplicated ids overall:\n")
  print(sum(duplicated(df$id)))
  
  if ("elec_id" %in% names(df)) {
    dup_within_election <- df %>%
      count(elec_id, id) %>%
      filter(n > 1)
    
    cat("\nDuplicated ids within election:\n")
    print(dup_within_election)
  }
}

# ------------------------------------------------
# 8. Alternative-specific block diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Alternative-specific block diagnostics\n")
cat("========================================\n")

all_vars <- names(df)

stack_vars        <- grep("^stack_[0-9]+$", all_vars, value = TRUE)
peid_vars         <- grep("^peid_[0-9]+$", all_vars, value = TRUE)
party_vars        <- grep("^party_[0-9]+$", all_vars, value = TRUE)
partyharm_vars    <- grep("^party_harmonized_[0-9]+$", all_vars, value = TRUE)
mapvote_vars      <- grep("^map_vote_[0-9]+$", all_vars, value = TRUE)
maplr_vars        <- grep("^map_lr_[0-9]+$", all_vars, value = TRUE)
voteshare_vars    <- grep("^vote_share_[0-9]+$", all_vars, value = TRUE)
votesharelag_vars <- grep("^vote_share_lag_[0-9]+$", all_vars, value = TRUE)
turnout_vars      <- grep("^turnout_[0-9]+$", all_vars, value = TRUE)
turnoutlag_vars   <- grep("^turnout_lag_[0-9]+$", all_vars, value = TRUE)

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

cat("\nvote_share_* variables:\n")
print(voteshare_vars)

cat("\nvote_share_lag_* variables:\n")
print(votesharelag_vars)

cat("\nturnout_* variables:\n")
print(turnout_vars)

cat("\nturnout_lag_* variables:\n")
print(turnoutlag_vars)

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

# ------------------------------------------------
# 9. Core variable presence and missingness
# ------------------------------------------------
cat("\n========================================\n")
cat("Core variable presence and missingness\n")
cat("========================================\n")

core_vars <- c(
  "elec_id", "id", "vote", "l_vote", "pid", "pid2", "pid_any",
  "male", "age", "lr_self", "income", "educ", "weights",
  "dwght", "swght", "raked_weights", "stfdem"
)

core_present <- intersect(core_vars, names(df))
core_missing <- setdiff(core_vars, names(df))

cat("\nCore variables present:\n")
print(core_present)

cat("\nCore variables absent:\n")
print(core_missing)

if (length(core_present) > 0) {
  cat("\nMissingness in core variables:\n")
  print(
    tibble(
      variable = core_present,
      n_missing = sapply(df[core_present], function(z) sum(is.na(z))),
      pct_missing = sapply(df[core_present], function(z) mean(is.na(z)))
    )
  )
}

# ------------------------------------------------
# 10. Vote and lagged vote diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Vote diagnostics\n")
cat("========================================\n")

if ("vote" %in% names(df)) {
  cat("\nDistribution of vote:\n")
  print(sort(table(df$vote, useNA = "ifany")))
}

if ("l_vote" %in% names(df)) {
  cat("\nDistribution of l_vote:\n")
  print(sort(table(df$l_vote, useNA = "ifany")))
}

if (all(c("elec_id", "vote") %in% names(df))) {
  cat("\nVote by election:\n")
  print(df %>% count(elec_id, vote, sort = FALSE))
}

if (all(c("elec_id", "l_vote") %in% names(df))) {
  cat("\nLagged vote by election:\n")
  print(df %>% count(elec_id, l_vote, sort = FALSE))
}

# ------------------------------------------------
# 11. Party mapping diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Party mapping diagnostics\n")
cat("========================================\n")

check_nonmissing_counts <- function(var_names, data) {
  if (length(var_names) == 0) {
    return(tibble(variable = character(), n_nonmissing = integer(), n_unique = integer()))
  }
  
  tibble(
    variable = var_names,
    n_nonmissing = sapply(data[var_names], function(z) sum(!is.na(z))),
    n_unique = sapply(data[var_names], function(z) dplyr::n_distinct(z, na.rm = TRUE))
  )
}

cat("\nNon-missing counts for party_* block:\n")
print(check_nonmissing_counts(party_vars, df))

cat("\nNon-missing counts for party_harmonized_* block:\n")
print(check_nonmissing_counts(partyharm_vars, df))

cat("\nNon-missing counts for map_vote_* block:\n")
print(check_nonmissing_counts(mapvote_vars, df))

cat("\nNon-missing counts for map_lr_* block:\n")
print(check_nonmissing_counts(maplr_vars, df))

# ------------------------------------------------
# 12. Weight diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Weight diagnostics\n")
cat("========================================\n")

for (w in c("weights", "dwght", "swght", "raked_weights")) {
  if (w %in% names(df)) {
    cat("\nSummary for", w, ":\n")
    print(summary(df[[w]]))
    
    cat("Number missing in", w, ":\n")
    print(sum(is.na(df[[w]])))
  }
}

# ------------------------------------------------
# 13. All-NA row diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("All-NA row diagnostics\n")
cat("========================================\n")

non_id_vars <- setdiff(names(df), c("id", "elec_id", "country"))

if (length(non_id_vars) > 0) {
  all_na_rows <- apply(df[non_id_vars], 1, function(z) all(is.na(z)))
  
  cat("\nNumber of rows that are NA on all non-ID variables:\n")
  print(sum(all_na_rows))
  
  if ("id" %in% names(df) && "elec_id" %in% names(df) && sum(all_na_rows) > 0) {
    cat("\nIDs of rows that are NA on all non-ID variables:\n")
    print(df %>% filter(all_na_rows) %>% select(elec_id, id))
  }
}

# ------------------------------------------------
# 14. Quick election-level summaries
# ------------------------------------------------
cat("\n========================================\n")
cat("Quick election-level summaries\n")
cat("========================================\n")

summary_vars <- intersect(
  c("age", "male", "lr_self", "pid_any", "weights", "dwght", "swght", "stfdem"),
  names(df)
)

if ("elec_id" %in% names(df) && length(summary_vars) > 0) {
  for (v in summary_vars) {
    cat("\nSummary by elec_id for:", v, "\n")
    print(
      df %>%
        group_by(elec_id) %>%
        summarise(
          n = n(),
          n_nonmissing = sum(!is.na(.data[[v]])),
          mean = if (is.numeric(.data[[v]])) mean(.data[[v]], na.rm = TRUE) else NA_real_,
          sd = if (is.numeric(.data[[v]])) sd(.data[[v]], na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
    )
  }
}

# ------------------------------------------------
# 15. Reshape, add generalized non-party alternative, validate, and save
# ------------------------------------------------

mappings <- voteswitchR::mappings

map_fi <- mappings %>%
  dplyr::filter(stringr::str_starts(elec_id, "FI")) %>%
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
    cols = matches("^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_"),
    names_to = c(".value", "alt"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  dplyr::mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack),
    vote = as.numeric(vote),
    l_vote = as.numeric(l_vote)
  ) %>%
  dplyr::filter(!is.na(stack)) %>%
  dplyr::left_join(map_fi, by = c("elec_id", "stack")) %>%
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

output_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "fi_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "fi_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "fi_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "fi_df_long_valid_both.RData")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned Finland files:\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")