# ================================================================
# 10_France.R
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
country_prefix <- "FR"
country_name   <- "France"
select  <- dplyr::select

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/fr_data_file.RData"
output_dir   <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "fr_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "fr_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "fr_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "fr_df_long_valid_both.RData")

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
# 5. Helper functions
# ------------------------------------------------
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

check_nonmissing_counts <- function(var_names, data) {
  if (length(var_names) == 0) {
    return(tibble(
      variable = character(),
      n_nonmissing = integer(),
      n_unique = integer()
    ))
  }
  
  tibble(
    variable = var_names,
    n_nonmissing = sapply(data[var_names], function(z) sum(!is.na(z))),
    n_unique = sapply(data[var_names], function(z) dplyr::n_distinct(z, na.rm = TRUE))
  )
}

safe_label <- function(x) {
  lb <- attr(x, "label", exact = TRUE)
  if (is.null(lb)) NA_character_ else as.character(lb)
}

invalid_vote_codes <- c(97, 98, 99, 997, 998, 999)

# ------------------------------------------------
# 6. Top-level inspection and df assignment
# ------------------------------------------------
print_header("Top-level data_file inspection")

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

# ------------------------------------------------
# 7. Basic object structure
# ------------------------------------------------
print_header("Basic object structure")

cat("\nNames(df):\n")
print(names(df))

cat("\nDimensions of df:\n")
print(dim(df))

cat("\nCompact glimpse of df:\n")
dplyr::glimpse(df)

# ------------------------------------------------
# 8. Alternative-specific block diagnostics
# ------------------------------------------------
print_header("Alternative-specific block diagnostics")

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
  as.integer(stringr::str_extract(maplr_vars, "\\d+$")),
  as.integer(stringr::str_extract(voteshare_vars, "\\d+$")),
  as.integer(stringr::str_extract(votesharelag_vars, "\\d+$")),
  as.integer(stringr::str_extract(turnout_vars, "\\d+$")),
  as.integer(stringr::str_extract(turnoutlag_vars, "\\d+$"))
)

alt_nums <- alt_nums[!is.na(alt_nums)]
max_alt <- if (length(alt_nums) == 0) NA_integer_ else max(alt_nums)

cat("\nMaximum alternative number detected:\n")
print(max_alt)


# ------------------------------------------------
# 9. Election coverage diagnostics
# ------------------------------------------------
print_header("Election coverage diagnostics")

if ("elec_id" %in% names(df)) {
  cat("\nDistinct elec_id values in df:\n")
  print(sort(unique(df$elec_id)))
  
  cat("\nCounts by elec_id:\n")
  print(df %>% count(elec_id, sort = FALSE))
  
  cat("\nNumber of distinct elections:\n")
  print(dplyr::n_distinct(df$elec_id))
} else {
  cat("\nVariable 'elec_id' not found in df.\n")
}

date_like_vars <- grep("date|year|wave|survey|edate", names(df), ignore.case = TRUE, value = TRUE)

cat("\nDate/year-like variables in df:\n")
print(date_like_vars)

if (length(date_like_vars) > 0) {
  for (v in date_like_vars) {
    cat("\nSummary of", v, ":\n")
    print(summary(df[[v]]))
  }
}

# ------------------------------------------------
# 10. available_data diagnostics and expected contexts
# ------------------------------------------------
print_header("available_data and expected context diagnostics")

available_data <- getFromNamespace("available_data", "voteswitchR")

cat("\nRows in voteswitchR::available_data for this country:\n")
print(available_data %>% filter(iso2c == country_prefix) %>% nrow())

cat("\nExpected contexts from voteswitchR::available_data:\n")
expected_contexts <- available_data %>%
  filter(iso2c == country_prefix) %>%
  distinct(elec_id) %>%
  arrange(elec_id) %>%
  pull(elec_id)

print(expected_contexts)

if ("elec_id" %in% names(df)) {
  observed_contexts <- sort(unique(df$elec_id))
  
  cat("\nContexts present in df:\n")
  print(observed_contexts)
  
  cat("\nExpected but not present in df:\n")
  print(setdiff(expected_contexts, observed_contexts))
  
  cat("\nPresent in df but not expected:\n")
  print(setdiff(observed_contexts, expected_contexts))
}


# ------------------------------------------------
# 11. Mapping diagnostics from voteswitchR::mappings
# ------------------------------------------------
print_header("voteswitchR mapping diagnostics")

mappings <- getFromNamespace("mappings", "voteswitchR")

country_mappings <- mappings %>%
  filter(iso2c == country_prefix)

cat("\nNumber of rows in country mapping:\n")
print(nrow(country_mappings))

cat("\nNumber of distinct elections in country mapping:\n")
print(country_mappings %>% summarise(n = n_distinct(elec_id)))

cat("\nCounts by elec_id in mapping:\n")
print(country_mappings %>% count(elec_id, sort = FALSE))

cat("\nDistinct combinations of parfam and parfam_harmonized:\n")
print(
  country_mappings %>%
    distinct(parfam, parfam_harmonized) %>%
    arrange(parfam, parfam_harmonized)
)

cat("\nPreview of key mapping columns:\n")
print(
  country_mappings %>%
    select(
      any_of(c(
        "elec_id", "stack", "peid", "party_name",
        "partyabbrev", "parfam", "parfam_harmonized"
      ))
    ) %>%
    arrange(elec_id, stack) %>%
    head(50)
)

mapping_join <- country_mappings %>%
  transmute(
    elec_id,
    stack = as.numeric(stack),
    peid_map = peid,
    party_name_map = party_name,
    partyabbrev_map = partyabbrev,
    parfam,
    parfam_harmonized
  ) %>%
  distinct()


# ------------------------------------------------
# 12. Required variables: existence and semantic checks
# ------------------------------------------------
print_header("Required variables: existence and semantic checks")

required_concepts <- c(
  "id", "elec_id", "vote", "l_vote", "male", "age", "income",
  "educ", "lr_self", "stfdem", "dwght", "swght"
)

label_tbl <- tibble(
  variable = names(df),
  label = sapply(df, safe_label)
)

cat("\nVariable labels preview:\n")
print(label_tbl %>% head(100))

concept_report <- tibble(
  concept = required_concepts,
  exists = required_concepts %in% names(df),
  label = sapply(required_concepts, function(v) {
    if (v %in% names(df)) safe_label(df[[v]]) else NA_character_
  })
)

cat("\nRequired concept report:\n")
print(concept_report)

if (!"age" %in% names(df)) {
  birth_candidates <- names(df)[grepl("birth|geb|yob|byear", names(df), ignore.case = TRUE)]
  
  cat("\nNo direct age variable found. Birth-year-like candidates:\n")
  print(tibble(variable = birth_candidates, label = sapply(df[birth_candidates], safe_label)))
  
  if (length(birth_candidates) > 0) {
    cat("\nAge may need to be derived from a birth-year variable after inspection.\n")
  }
}

for (w in c("dwght", "swght")) {
  if (w %in% names(df)) {
    cat("\nSummary for", w, ":\n")
    print(summary(df[[w]]))
  } else {
    cat("\nWeight variable", w, "not found in raw df. Do not reconstruct; keep corresponding entries as NA in available_data logic if needed.\n")
  }
}

# ------------------------------------------------
# 13. ID diagnostics
# ------------------------------------------------
print_header("ID diagnostics")

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
# 14. Core variable presence and missingness
# ------------------------------------------------
print_header("Core variable presence and missingness")

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
# 15. Wide-to-long diagnostic reshape
# ------------------------------------------------
print_header("Diagnostic reshape from wide to long")

stub_pattern <- "^(stack|peid|party|party_harmonized|map_vote|map_lr|vote_share|vote_share_lag|turnout|turnout_lag)_[0-9]+$"

long_diag <- df %>%
  pivot_longer(
    cols = matches(stub_pattern),
    names_to = c(".value", "alt"),
    names_pattern = "^(.*)_([0-9]+)$"
  ) %>%
  mutate(alt = as.integer(alt))

cat("\nDimensions after diagnostic reshape:\n")
print(dim(long_diag))

if ("elec_id" %in% names(long_diag)) {
  cat("\nCounts by elec_id and alt:\n")
  print(long_diag %>% count(elec_id, alt, sort = FALSE))
}

cat("\nCount of missing stack in reshaped data:\n")
print(sum(is.na(long_diag$stack)))

if ("elec_id" %in% names(long_diag)) {
  cat("\nCounts by elec_id and whether stack is missing:\n")
  print(long_diag %>% count(elec_id, stack_missing = is.na(stack), sort = FALSE))
}

# ------------------------------------------------
# 16. Mandatory padded-structure filter
# ------------------------------------------------
print_header("Filter padded alternatives using !is.na(stack)")

long_nopad <- long_diag %>%
  filter(!is.na(stack))

cat("\nDimensions after dropping padded alternatives:\n")
print(dim(long_nopad))

if ("elec_id" %in% names(long_nopad)) {
  cat("\nCounts by elec_id after dropping padded alternatives:\n")
  print(long_nopad %>% count(elec_id, sort = FALSE))
}

# ------------------------------------------------
# 17. Join mapping and inspect mapping quality
# ------------------------------------------------
print_header("Join mapping and inspect mapping quality")

df_long <- long_nopad %>%
  mutate(stack = as.numeric(stack)) %>%
  left_join(mapping_join, by = c("elec_id", "stack"))

cat("\nMissing values after mapping join:\n")
print(
  tibble(
    n_missing_peid_map = sum(is.na(df_long$peid_map)),
    n_missing_parfam = sum(is.na(df_long$parfam)),
    n_missing_parfam_harmonized = sum(is.na(df_long$parfam_harmonized))
  )
)

if ("peid" %in% names(df_long)) {
  peid_agreement <- df_long %>%
    filter(!is.na(peid), !is.na(peid_map)) %>%
    mutate(peid_match = as.character(peid) == as.character(peid_map))
  
  cat("\nAgreement between peid and peid_map:\n")
  print(peid_agreement %>% count(peid_match))
  
  cat("\nExamples of peid mismatches:\n")
  print(
    peid_agreement %>%
      filter(!peid_match) %>%
      select(any_of(c("elec_id", "id", "alt", "stack", "peid", "peid_map", "party_name_map"))) %>%
      head(50)
  )
}

cat("\nDistinct combinations of parfam, parfam_harmonized, and party_name_map:\n")
print(
  df_long %>%
    distinct(parfam, parfam_harmonized, party_name_map) %>%
    arrange(parfam, parfam_harmonized, party_name_map)
)

# ------------------------------------------------
# 18. Add generalized non-party alternative and respondent-level diagnostics
# ------------------------------------------------
print_header("Add generalized non-party alternative and respondent-level diagnostics")

df_long <- df_long %>%
  mutate(
    alt = as.numeric(alt),
    stack = as.numeric(stack),
    vote = as.numeric(vote),
    l_vote = as.numeric(l_vote),
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

respondent_match_diag <- df_long %>%
  group_by(elec_id, id) %>%
  summarise(
    now_matches = first(now_matches),
    lag_matches = first(lag_matches),
    vote_value = dplyr::first(vote),
    l_vote_value = dplyr::first(l_vote),
    switch_to = dplyr::first(switch_to),
    switch_from = dplyr::first(switch_from),
    .groups = "drop"
  )

cat("\nDistribution of now_matches:\n")
print(respondent_match_diag %>% count(now_matches, sort = FALSE))

cat("\nDistribution of lag_matches:\n")
print(respondent_match_diag %>% count(lag_matches, sort = FALSE))

cat("\nCases with more than one current-vote match:\n")
print(respondent_match_diag %>% filter(now_matches > 1))

cat("\nCases with more than one lagged-vote match:\n")
print(respondent_match_diag %>% filter(lag_matches > 1))

cat("\nExamples with zero current-vote matches:\n")
print(respondent_match_diag %>% filter(now_matches == 0) %>% head(50))

cat("\nExamples with zero lagged-vote matches:\n")
print(respondent_match_diag %>% filter(lag_matches == 0) %>% head(50))

# ------------------------------------------------
# 19. Vote missingness and observed vote codes
# ------------------------------------------------
print_header("Vote missingness and observed vote codes")

if (all(c("elec_id", "vote") %in% names(df_long))) {
  cat("\nVote missingness by election:\n")
  print(
    df_long %>%
      group_by(elec_id) %>%
      summarise(
        n_rows = n(),
        share_missing_vote = mean(is.na(vote)),
        .groups = "drop"
      )
  )
  
  cat("\nDistinct observed vote codes:\n")
  print(sort(unique(df_long$vote)))
}

if (all(c("elec_id", "l_vote") %in% names(df_long))) {
  cat("\nLagged vote missingness by election:\n")
  print(
    df_long %>%
      group_by(elec_id) %>%
      summarise(
        n_rows = n(),
        share_missing_l_vote = mean(is.na(l_vote)),
        .groups = "drop"
      )
  )
  
  cat("\nDistinct observed lagged vote codes:\n")
  print(sort(unique(df_long$l_vote)))
}

# ------------------------------------------------
# 20. Country-specific family correction diagnostics
# ------------------------------------------------
print_header("Country-specific family correction diagnostics")

cat("\nParties with parfam_harmonized == 'nat' or missing parfam_harmonized:\n")
print(
  df_long %>%
    distinct(elec_id, stack, party_name_map, partyabbrev_map, parfam, parfam_harmonized) %>%
    filter(is.na(parfam_harmonized) | parfam_harmonized == "nat") %>%
    arrange(elec_id, stack)
)

cat("\nPotential party_name_map inconsistencies:\n")
print(
  df_long %>%
    distinct(elec_id, stack, party_name_map, partyabbrev_map, parfam, parfam_harmonized) %>%
    arrange(elec_id, party_name_map, stack)
)

# ------------------------------------------------
# 21. Standard output datasets
# ------------------------------------------------
print_header("Construct standard output datasets")

df_long_full <- df_long

df_long_valid_now <- df_long_full %>%
  filter(valid_now)

df_long_valid_lag <- df_long_full %>%
  filter(valid_lag)

df_long_valid_both <- df_long_full %>%
  filter(valid_both)

cat("\nDimensions of full long data:\n")
print(dim(df_long_full))

cat("\nDimensions of current-vote valid data:\n")
print(dim(df_long_valid_now))

cat("\nDimensions of lagged-vote valid data:\n")
print(dim(df_long_valid_lag))

cat("\nDimensions of both-period valid data:\n")
print(dim(df_long_valid_both))

# ------------------------------------------------
# 22. Final validation checks
# ------------------------------------------------
print_header("Final validation checks")

required_key_vars <- c("elec_id", "id", "alt", "stack", "peid_map")

cat("\nMissingness in required key variables, full long data:\n")
print(
  tibble(
    variable = required_key_vars,
    n_missing = sapply(required_key_vars, function(v) sum(is.na(df_long_full[[v]])))
  )
)

stopifnot(sum(is.na(df_long_full$stack) | is.nan(df_long_full$stack)) == 0)
stopifnot(sum(is.na(df_long_full$peid_map)) == 0)

stopifnot(all(
  df_long_full %>%
    group_by(elec_id, id) %>%
    summarise(n = sum(voted_now, na.rm = TRUE), .groups = "drop") %>%
    pull(n) == 1
))

stopifnot(all(
  df_long_full %>%
    group_by(elec_id, id) %>%
    summarise(n = sum(voted_lag, na.rm = TRUE), .groups = "drop") %>%
    pull(n) == 1
))

stopifnot(all(
  df_long_full %>%
    filter(party_name_map != "non-voters") %>%
    pull(stack) <= max(df_long_full$stack[df_long_full$party_name_map != "non-voters"], na.rm = TRUE)
))

if ("peid" %in% names(df_long_full)) {
  peid_mismatch_n <- df_long_full %>%
    filter(!is.na(peid), !is.na(peid_map)) %>%
    summarise(n = sum(as.character(peid) != as.character(peid_map))) %>%
    pull(n)
  
  cat("\nNumber of non-missing peid / peid_map mismatches:\n")
  print(peid_mismatch_n)
}

validate_subset <- function(dat, name) {
  cat("\n---", name, "---\n")
  
  out <- dat %>%
    group_by(elec_id, id) %>%
    summarise(
      now_matches = sum(voted_now %in% TRUE, na.rm = TRUE),
      lag_matches = sum(voted_lag %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nDistribution of now_matches:\n")
  print(out %>% count(now_matches, sort = FALSE))
  
  cat("\nDistribution of lag_matches:\n")
  print(out %>% count(lag_matches, sort = FALSE))
}

validate_subset(df_long_valid_now,  "Validation: current-vote valid subset")
validate_subset(df_long_valid_lag,  "Validation: lagged-vote valid subset")
validate_subset(df_long_valid_both, "Validation: both-period valid subset")

cat("Full long dataset rows      :", nrow(df_long_full), "\n")
cat("Current-vote valid rows     :", nrow(df_long_valid_now), "\n")
cat("Lagged-vote valid rows      :", nrow(df_long_valid_lag), "\n")
cat("Both-period valid rows      :", nrow(df_long_valid_both), "\n")

print(table(df_long_full$now_matches))
print(table(df_long_full$lag_matches))

# ------------------------------------------------
# 23. Enforce consistent schema before saving
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
# 24. Save standard outputs
# ------------------------------------------------
print_header("Save standard outputs")

save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\nSaved cleaned France files:\n")
cat("Full long data        :", output_rdata_full, "\n")
cat("Current-vote valid    :", output_rdata_now, "\n")
cat("Lagged-vote valid     :", output_rdata_lag, "\n")
cat("Both-period valid     :", output_rdata_both, "\n")