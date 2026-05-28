# ================================================================
# 17_Italy.R
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
# 2. Set country-specific inputs
# ------------------------------------------------
country_prefix <- "IT"
country_name   <- "Italy"

input_rdata  <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro/it_data_file.RData"
output_dir   <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/micro"

output_rdata_full <- file.path(output_dir, "it_df_long_full.RData")
output_rdata_now  <- file.path(output_dir, "it_df_long_valid_now.RData")
output_rdata_lag  <- file.path(output_dir, "it_df_long_valid_lag.RData")
output_rdata_both <- file.path(output_dir, "it_df_long_valid_both.RData")

# ------------------------------------------------
# 3. Clean raw Italy 2018 file BEFORE build_data_file()
# ------------------------------------------------
it2018_path <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/it2018/Itanes_2018_release01_panel_pre_post.dta"
it2018_backup <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/it2018/Itanes_2018_release01_panel_pre_post_backup_before_cleanup.dta"

if (!file.exists(it2018_path)) {
  stop("Italy 2018 raw file not found at: ", it2018_path)
}

x2018 <- haven::read_dta(it2018_path)

if (!file.exists(it2018_backup)) {
  file.copy(it2018_path, it2018_backup, overwrite = FALSE)
}

available_data <- getFromNamespace("available_data", "voteswitchR")

it2018_meta <- available_data %>%
  filter(elec_id == "IT-2018-03")

if (nrow(it2018_meta) != 1) {
  stop("Could not uniquely identify IT-2018-03 in available_data.")
}

cat("\n========================================\n")
cat("Italy 2018 raw-file cleanup before build\n")
cat("========================================\n")

cat("Expected Italy 2018 concept mapping from available_data:\n")
print(
  it2018_meta %>%
    select(any_of(c(
      "vote", "l_vote", "part", "l_part", "pid",
      "strength1", "strength2", "male", "age", "lr_self", "dwght"
    )))
)

cat("\nRelevant variables before cleanup:\n")
print(
  grep(
    "voto|partid|party_id|d9_1|d0_1|d0_2|d6_1_1_slice|dem01|dem02|lrself|age|eta|età|birth|nasc|sex|gender|male|female|sesso|genere|^ov_",
    names(x2018),
    value = TRUE,
    ignore.case = TRUE
  )
)

drop_ov <- names(x2018)[grepl("^ov_", names(x2018))]
if (length(drop_ov) > 0) {
  x2018 <- x2018 %>%
    select(-all_of(drop_ov))
}

if (!"votoC1" %in% names(x2018) && "voto4_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(votoC1 = as.numeric(voto4_post))
}

if (!"d9_1" %in% names(x2018) && "voto2013_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(d9_1 = as.numeric(voto2013_post))
}

if (!"votoA" %in% names(x2018) && "voto1_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(votoA = as.numeric(voto1_post))
}

if (!"party_id1" %in% names(x2018) && "partid1_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(party_id1 = as.numeric(partid1_post))
}

if (!"party_id2" %in% names(x2018) && "partid2_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(party_id2 = as.numeric(partid2_post))
}

# dem01_post: 1 = Uomo, 2 = Donna
# Recode to male indicator expected by voteswitchR: 1 = male, 0 = female
if (!"d0_1" %in% names(x2018) && "dem01_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(
      d0_1 = case_when(
        dem01_post == 1 ~ 1,
        dem01_post == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
}

if (!"d0_2" %in% names(x2018) && "dem02_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(d0_2 = as.numeric(dem02_post))
}

if (!"d6_1_1_slice" %in% names(x2018) && "lrself_post" %in% names(x2018)) {
  x2018 <- x2018 %>%
    mutate(d6_1_1_slice = as.numeric(lrself_post))
}

expected_vars <- it2018_meta %>%
  select(any_of(c(
    "vote", "l_vote", "part", "l_part", "pid",
    "strength1", "strength2", "male", "age", "lr_self", "dwght"
  ))) %>%
  unlist(use.names = TRUE)

expected_vars <- expected_vars[!is.na(expected_vars)]

cat("\nExpected non-missing concept variables for IT-2018-03:\n")
print(expected_vars)

cat("\nExpected variables still missing from raw file AFTER cleanup:\n")
print(setdiff(expected_vars, names(x2018)))

haven::write_dta(x2018, it2018_path)

cat("\nRelevant variables after cleanup:\n")
print(
  grep(
    "voto|partid|party_id|d9_1|d0_1|d0_2|d6_1_1_slice|dem01|dem02|lrself|age|eta|età|birth|nasc|sex|gender|male|female|sesso|genere|^ov_",
    names(x2018),
    value = TRUE,
    ignore.case = TRUE
  )
)
cat("========================================\n\n")

# ------------------------------------------------
# 4. Build data_file
# ------------------------------------------------
data_file <- voteswitchR::build_data_file()

if (!file.exists(input_rdata)) {
  stop("Input RData not found at: ", input_rdata)
}

loaded_objs <- load(input_rdata)

cat("\n========================================\n")
cat("Objects loaded from input_rdata:\n")
print(loaded_objs)
cat("========================================\n\n")

if (!exists("data_file")) {
  if (length(loaded_objs) == 1 && exists(loaded_objs[1])) {
    obj <- get(loaded_objs[1])
    if (is.list(obj)) {
      data_file <- obj
    } else {
      stop("Loaded object is not a list-like data_file object.")
    }
  } else {
    stop("Could not find `data_file` after loading ", input_rdata)
  }
}

# ------------------------------------------------
# 5. Assign the actual data frame from data_file
# ------------------------------------------------
cat("\n========================================\n")
cat("Inspecting structure of `data_file`\n")
cat("========================================\n")

str(data_file, max.level = 1)
cat("\nNames(data_file):\n")
print(names(data_file))

find_df_in_data_file <- function(x) {
  if (inherits(x, "data.frame")) return(x)
  
  nms <- names(x)
  
  preferred <- c("data", "df", "survey", "survey_data")
  for (nm in preferred) {
    if (nm %in% nms && inherits(x[[nm]], "data.frame")) return(x[[nm]])
  }
  
  is_df <- vapply(x, inherits, logical(1), what = "data.frame")
  df_candidates <- x[is_df]
  
  if (length(df_candidates) == 1) return(df_candidates[[1]])
  
  if (length(df_candidates) > 1) {
    dims <- vapply(df_candidates, nrow, numeric(1))
    return(df_candidates[[which.max(dims)]])
  }
  
  stop("Could not identify a data frame inside `data_file`.")
}

df <- find_df_in_data_file(data_file)

cat("\nAssigned object: `df`\n")
cat("Class:\n")
print(class(df))
cat("\nDimensions:\n")
print(dim(df))
cat("\nVariable names:\n")
print(names(df))
cat("\nGlimpse:\n")
print(dplyr::glimpse(df, width = 80))

# ------------------------------------------------
# 6. Basic election coverage
# ------------------------------------------------
cat("\n========================================\n")
cat("Election coverage in loaded file\n")
cat("========================================\n")

if ("elec_id" %in% names(df)) {
  cat("Distinct elec_id values:\n")
  print(sort(unique(df$elec_id)))
  
  cat("\nCounts by elec_id:\n")
  print(df %>% count(elec_id, name = "n"))
}

if ("year" %in% names(df)) {
  cat("\nSummary of year:\n")
  print(summary(df$year))
}

if ("election_date" %in% names(df)) {
  cat("\nSummary of election_date:\n")
  print(summary(df$election_date))
}

# ------------------------------------------------
# 7. Stacked block structure in wide data
# ------------------------------------------------
cat("\n========================================\n")
cat("Stacked block structure\n")
cat("========================================\n")

stack_patterns <- c(
  "^stack_\\d+$", "^peid_\\d+$", "^party_\\d+$", "^party_harmonized_\\d+$",
  "^map_vote_\\d+$", "^map_lr_\\d+$", "^vote_share_\\d+$", "^vote_share_lag_\\d+$",
  "^turnout_\\d+$", "^turnout_lag_\\d+$"
)

stack_vars <- names(df)[Reduce(`|`, lapply(stack_patterns, function(p) grepl(p, names(df))))]

cat("Relevant stacked variables:\n")
print(stack_vars)

alt_nums <- stringr::str_extract(stack_vars, "\\d+$") %>% as.integer()
max_alt <- if (length(alt_nums) == 0 || all(is.na(alt_nums))) NA_integer_ else max(alt_nums, na.rm = TRUE)

cat("\nMaximum alternative number detected:\n")
print(max_alt)

# ------------------------------------------------
# 8. Mapping structure from voteswitchR::mappings
# ------------------------------------------------
cat("\n========================================\n")
cat("voteswitchR mapping structure for Italy\n")
cat("========================================\n")

data("mappings", package = "voteswitchR", envir = environment())

mappings_it <- mappings %>%
  filter(iso2c == country_prefix)

cat("Rows in mappings for Italy:\n")
print(nrow(mappings_it))

cat("\nDistinct elections in mappings:\n")
print(length(unique(mappings_it$elec_id)))

cat("\nCounts by elec_id:\n")
print(mappings_it %>% count(elec_id, name = "n"))

cat("\nDistinct combinations of parfam and parfam_harmonized:\n")
print(
  mappings_it %>%
    distinct(parfam, parfam_harmonized) %>%
    arrange(parfam, parfam_harmonized)
)

cat("\nPreview of key mapping columns:\n")
preview_cols <- c("elec_id", "stack", "peid", "party_name", "partyabbrev", "parfam", "parfam_harmonized")
preview_cols <- preview_cols[preview_cols %in% names(mappings_it)]

print(
  mappings_it %>%
    select(all_of(preview_cols)) %>%
    arrange(elec_id, stack) %>%
    head(100)
)

# ------------------------------------------------
# 8. Mapping structure from voteswitchR::mappings
# ------------------------------------------------
cat("\n========================================\n")
cat("voteswitchR mapping structure for Italy\n")
cat("========================================\n")

data("mappings", package = "voteswitchR", envir = environment())

mappings_it <- mappings %>%
  filter(iso2c == country_prefix)

cat("Rows in mappings for Italy:\n")
print(nrow(mappings_it))

cat("\nDistinct elections in mappings:\n")
print(length(unique(mappings_it$elec_id)))

cat("\nCounts by elec_id:\n")
print(mappings_it %>% count(elec_id, name = "n"))

cat("\nDistinct combinations of parfam and parfam_harmonized:\n")
print(
  mappings_it %>%
    distinct(parfam, parfam_harmonized) %>%
    arrange(parfam, parfam_harmonized)
)

cat("\nPreview of key mapping columns:\n")
preview_cols <- c("elec_id", "stack", "peid", "party_name", "partyabbrev", "parfam", "parfam_harmonized")
preview_cols <- preview_cols[preview_cols %in% names(mappings_it)]

print(
  mappings_it %>%
    select(all_of(preview_cols)) %>%
    arrange(elec_id, stack) %>%
    head(100)
)

# ------------------------------------------------
# 9. Diagnostic reshape from wide to long
# ------------------------------------------------
cat("\n========================================\n")
cat("Diagnostic reshape from wide to long\n")
cat("========================================\n")

required_long_roots <- c(
  "stack", "peid", "party", "party_harmonized", "map_vote", "map_lr",
  "vote_share", "vote_share_lag", "turnout", "turnout_lag"
)

present_roots <- required_long_roots[
  vapply(
    required_long_roots,
    function(root) any(grepl(paste0("^", root, "_\\d+$"), names(df))),
    logical(1)
  )
]

if (length(present_roots) == 0) {
  stop("No stacked wide variables detected; cannot reshape.")
}

df_long_diag <- df %>%
  pivot_longer(
    cols = matches(paste0("^(", paste(present_roots, collapse = "|"), ")_\\d+$")),
    names_to = c(".value", "alt"),
    names_pattern = "^(.*)_(\\d+)$"
  ) %>%
  mutate(alt = as.integer(alt))

cat("Dimensions after pivot_longer:\n")
print(dim(df_long_diag))

if ("elec_id" %in% names(df_long_diag)) {
  cat("\nCounts by elec_id and alt:\n")
  print(df_long_diag %>% count(elec_id, alt, name = "n"))
}

cat("\nCount of missing stack:\n")
print(sum(is.na(df_long_diag$stack)))

if ("elec_id" %in% names(df_long_diag)) {
  cat("\nCounts by elec_id and whether stack is missing:\n")
  print(
    df_long_diag %>%
      mutate(stack_missing = is.na(stack)) %>%
      count(elec_id, stack_missing, name = "n")
  )
}

# ------------------------------------------------
# 10. Remove padded alternatives
# ------------------------------------------------
cat("\n========================================\n")
cat("Remove padded alternatives\n")
cat("========================================\n")

df_long <- df_long_diag %>%
  filter(!is.na(stack))

cat("Dimensions after removing padded alternatives:\n")
print(dim(df_long))

if ("elec_id" %in% names(df_long)) {
  cat("\nCounts by elec_id after removing padded alternatives:\n")
  print(df_long %>% count(elec_id, name = "n"))
}

# ------------------------------------------------
# 11. Join mapping and inspect mapping quality
# ------------------------------------------------
cat("\n========================================\n")
cat("Join mapping and inspect mapping quality\n")
cat("========================================\n")

join_cols <- c("elec_id", "stack", "peid", "party_name", "partyabbrev", "parfam", "parfam_harmonized")
join_cols <- join_cols[join_cols %in% names(mappings_it)]

mapping_join <- mappings_it %>%
  select(all_of(join_cols)) %>%
  distinct()

if ("peid" %in% names(mapping_join)) {
  mapping_join <- mapping_join %>% rename(peid_map = peid)
}
if ("party_name" %in% names(mapping_join)) {
  mapping_join <- mapping_join %>% rename(party_name_map = party_name)
}
if ("partyabbrev" %in% names(mapping_join)) {
  mapping_join <- mapping_join %>% rename(partyabbrev_map = partyabbrev)
}

df_long <- df_long %>%
  left_join(mapping_join, by = c("elec_id", "stack"))

if ("peid_map" %in% names(df_long)) {
  cat("Missing peid_map:\n")
  print(sum(is.na(df_long$peid_map)))
}

if ("parfam" %in% names(df_long)) {
  cat("\nMissing parfam:\n")
  print(sum(is.na(df_long$parfam)))
}

if ("parfam_harmonized" %in% names(df_long)) {
  cat("\nMissing parfam_harmonized:\n")
  print(sum(is.na(df_long$parfam_harmonized)))
}

if (all(c("peid", "peid_map") %in% names(df_long))) {
  cat("\nAgreement between peid and peid_map:\n")
  print(
    df_long %>%
      mutate(peid_agree = case_when(
        is.na(peid) | is.na(peid_map) ~ NA,
        TRUE ~ peid == peid_map
      )) %>%
      count(peid_agree, name = "n")
  )
}

show_cols <- c("parfam", "parfam_harmonized", "party_name_map")
show_cols <- show_cols[show_cols %in% names(df_long)]

if (length(show_cols) > 0) {
  cat("\nDistinct combinations of parfam, parfam_harmonized, and party_name_map:\n")
  print(
    df_long %>%
      distinct(across(all_of(show_cols))) %>%
      arrange(across(all_of(show_cols)))
  )
}

# ------------------------------------------------
# 12. Add generalized non-party alternative and inspect respondent-level match structure
# ------------------------------------------------
cat("\n========================================\n")
cat("Add generalized non-party alternative and respondent-level match structure\n")
cat("========================================\n")

if (!all(c("vote", "l_vote", "stack") %in% names(df_long))) {
  stop("`vote`, `l_vote`, or `stack` missing from df_long.")
}

if (!"id" %in% names(df_long)) {
  stop("`id` not found in df_long.")
}

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

cat("Distribution of now_matches:\n")
print(df_long %>% distinct(elec_id, id, now_matches) %>% count(now_matches, name = "n"))

cat("\nDistribution of lag_matches:\n")
print(df_long %>% distinct(elec_id, id, lag_matches) %>% count(lag_matches, name = "n"))

cat("\nCases with now_matches > 1:\n")
print(nrow(df_long %>% distinct(elec_id, id, now_matches) %>% filter(now_matches > 1)))

cat("\nCases with lag_matches > 1:\n")
print(nrow(df_long %>% distinct(elec_id, id, lag_matches) %>% filter(lag_matches > 1)))

cat("\nExamples with now_matches == 0:\n")
print(
  df_long %>%
    distinct(elec_id, id, vote, now_matches) %>%
    filter(now_matches == 0) %>%
    head(20)
)

cat("\nExamples with lag_matches == 0:\n")
print(
  df_long %>%
    distinct(elec_id, id, l_vote, lag_matches) %>%
    filter(lag_matches == 0) %>%
    head(20)
)

# ------------------------------------------------
# 13. Vote missingness by election
# ------------------------------------------------
cat("\n========================================\n")
cat("Vote missingness by election\n")
cat("========================================\n")

print(
  df_long %>%
    group_by(elec_id) %>%
    summarise(
      share_missing_vote   = mean(is.na(vote)),
      share_missing_l_vote = mean(is.na(l_vote)),
      .groups = "drop"
    )
)

cat("\nDistinct current vote codes in long data:\n")
print(sort(unique(df_long$vote)))

cat("\nDistinct lagged vote codes in long data:\n")
print(sort(unique(df_long$l_vote)))

# ------------------------------------------------
# 14. Family diagnostics
# ------------------------------------------------
cat("\n========================================\n")
cat("Country-specific family diagnostics\n")
cat("========================================\n")

family_diag_cols <- c("elec_id", "stack", "party_name_map", "partyabbrev_map", "parfam", "parfam_harmonized")
family_diag_cols <- family_diag_cols[family_diag_cols %in% names(df_long)]

if ("parfam_harmonized" %in% names(df_long)) {
  cat("Parties with parfam_harmonized == 'nat' or missing:\n")
  print(
    df_long %>%
      filter(is.na(parfam_harmonized) | parfam_harmonized == "nat") %>%
      distinct(across(all_of(family_diag_cols))) %>%
      arrange(elec_id, stack)
  )
}

name_diag_cols <- c("elec_id", "stack", "party", "party_harmonized", "party_name_map", "partyabbrev_map")
name_diag_cols <- name_diag_cols[name_diag_cols %in% names(df_long)]

cat("\nPotential name inconsistencies in mapped parties:\n")
print(
  df_long %>%
    distinct(across(all_of(name_diag_cols))) %>%
    arrange(elec_id, stack) %>%
    head(200)
)

# ------------------------------------------------
# 15. Construct standard output datasets
# ------------------------------------------------
cat("\n========================================\n")
cat("Construct standard output datasets\n")
cat("========================================\n")

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
# 16. Final validation checks
# ------------------------------------------------
cat("\n========================================\n")
cat("Final validation checks\n")
cat("========================================\n")

cat("\nMissing peid_map in full long data:\n")
print(sum(is.na(df_long_full$peid_map)))

if ("peid" %in% names(df_long_full)) {
  cat("\nNumber of non-missing peid / peid_map mismatches:\n")
  print(
    df_long_full %>%
      filter(!is.na(peid), !is.na(peid_map), peid_map != "non") %>%
      summarise(n = sum(as.character(peid) != as.character(peid_map))) %>%
      pull(n)
  )
}

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
  stopifnot(
    df_long_full %>%
      filter(!is.na(peid), !is.na(peid_map), peid_map != "non") %>%
      summarise(ok = all(as.character(peid) == as.character(peid_map))) %>%
      pull(ok)
  )
}

cat("Full long dataset rows      :", nrow(df_long_full), "\n")
cat("Current-vote valid rows     :", nrow(df_long_valid_now), "\n")
cat("Lagged-vote valid rows      :", nrow(df_long_valid_lag), "\n")
cat("Both-period valid rows      :", nrow(df_long_valid_both), "\n")

print(table(df_long_full$now_matches))
print(table(df_long_full$lag_matches))

# ------------------------------------------------
# 17. Enforce consistent schema before saving
# ------------------------------------------------
cat("\n========================================\n")
cat("Enforce consistent schema before saving\n")
cat("========================================\n")

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
    mutate(
      across(all_of(char_vars), as.character),
      across(all_of(num_vars), as.numeric),
      across(all_of(logi_vars), as.logical)
    )
}

df_long_full       <- coerce_types(df_long_full)
df_long_valid_now  <- coerce_types(df_long_valid_now)
df_long_valid_lag  <- coerce_types(df_long_valid_lag)
df_long_valid_both <- coerce_types(df_long_valid_both)

# ------------------------------------------------
# 18. Save standard outputs
# ------------------------------------------------
save(df_long_full, file = output_rdata_full)
save(df_long_valid_now, file = output_rdata_now)
save(df_long_valid_lag, file = output_rdata_lag)
save(df_long_valid_both, file = output_rdata_both)

cat("\n========================================\n")
cat("Saved cleaned country files for:", country_name, "\n")
cat("Full long file      :", output_rdata_full, "\n")
cat("Valid current vote  :", output_rdata_now, "\n")
cat("Valid lagged vote   :", output_rdata_lag, "\n")
cat("Valid both          :", output_rdata_both, "\n")
cat("========================================\n\n")