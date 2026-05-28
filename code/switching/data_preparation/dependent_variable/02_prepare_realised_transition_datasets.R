# ================================================================
# 02_prepare_realised_transition_datasets.R
# Construct realised transition datasets
# Social-democratic vote-switching project
#
# This script constructs respondent-election transition datasets.
# Party-system availability is taken from the structural election-level
# availability object created in 01_prepare_master_data.R, not from
# observed realised vote flows.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching"

analysis_dir <- file.path(project_dir, "data", "analysis")
processed_dir <- file.path(project_dir, "data", "processed")

input_file <- file.path(
  processed_dir,
  "df_all_classified_social_democratic.rds"
)

path_party_system_availability <- file.path(
  processed_dir,
  "party_system_availability_social_democratic.rds"
)

dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------

df_all <- readRDS(input_file)
party_system_availability <- readRDS(path_party_system_availability)

stopifnot(is.data.frame(df_all), nrow(df_all) > 0)
stopifnot(is.data.frame(party_system_availability), nrow(party_system_availability) > 0)

required_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "id",
  "parfam_final",
  "voted_now",
  "voted_lag",
  "switch_from",
  "switch_to",
  "stay",
  "bloc",
  "party_bloc_detailed",
  "switch_from_bloc",
  "switch_to_bloc",
  "switch_from_bloc_detailed",
  "switch_to_bloc_detailed",
  "social_democratic",
  "far_right",
  "mainstream_right",
  "green",
  "far_left",
  "other_left",
  "non_voter"
)

missing_vars <- setdiff(required_vars, names(df_all))

if (length(missing_vars) > 0) {
  stop("Missing variables in df_all: ", paste(missing_vars, collapse = ", "))
}

required_availability_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "available_far_left",
  "available_green",
  "available_social_democratic",
  "available_mainstream_right",
  "available_far_right",
  "available_left",
  "available_other",
  "available_non",
  "available_other_left_pooled",
  "n_parties",
  "n_detailed_party_blocs",
  "n_pooled_blocs",
  "party_families_available",
  "pooled_blocs_available"
)

missing_availability_vars <- setdiff(
  required_availability_vars,
  names(party_system_availability)
)

if (length(missing_availability_vars) > 0) {
  stop(
    "Missing variables in party_system_availability: ",
    paste(missing_availability_vars, collapse = ", ")
  )
}

availability_key_check <- party_system_availability %>%
  dplyr::count(iso2c_file, elec_id, year) %>%
  dplyr::filter(n > 1)

if (nrow(availability_key_check) > 0) {
  cat("\nDuplicate election keys in party-system availability table:\n")
  print(availability_key_check, n = Inf, width = Inf)
  stop("Party-system availability is not unique by iso2c_file, elec_id, year.")
}

cat("\nData loaded successfully\n")
print(dim(df_all))

# ------------------------------------------------
# 3. Validate transition variables from 01 script
# ------------------------------------------------

cat("\nUnrestricted detailed bloc transition matrix:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc_detailed,
    switch_to_bloc_detailed
  ) %>%
  dplyr::count(switch_from_bloc_detailed, switch_to_bloc_detailed) %>%
  print(n = Inf)

cat("\nUnrestricted pooled bloc transition matrix, for robustness reference:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    id,
    switch_from_bloc,
    switch_to_bloc
  ) %>%
  dplyr::count(switch_from_bloc, switch_to_bloc) %>%
  print(n = Inf)

cat("\nCurrent detailed bloc support:\n")
df_all %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(iso2c_file, elec_id, id, switch_to_bloc_detailed) %>%
  dplyr::count(switch_to_bloc_detailed) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

# ------------------------------------------------
# 4. Structural election-level party-system availability
# ------------------------------------------------

bloc_availability <- party_system_availability %>%
  dplyr::transmute(
    iso2c_file,
    elec_id,
    year,
    
    social_democratic_present = available_social_democratic,
    far_right_present = available_far_right,
    mainstream_right_present = available_mainstream_right,
    green_present = available_green,
    far_left_present = available_far_left,
    non_present = available_non,
    other_left_present = available_other_left_pooled,
    
    available_far_left,
    available_green,
    available_social_democratic,
    available_mainstream_right,
    available_far_right,
    available_left,
    available_other,
    available_non,
    available_other_left_pooled,
    
    n_parties,
    n_detailed_party_blocs,
    n_pooled_blocs,
    party_families_available,
    pooled_blocs_available
  )

cat("\nStructural election-level party-system availability:\n")
bloc_availability %>%
  dplyr::count(
    social_democratic_present,
    far_right_present,
    mainstream_right_present,
    green_present,
    far_left_present,
    non_present,
    other_left_present
  ) %>%
  print(n = Inf, width = Inf)

cat("\nParty-system configurations relevant for detailed SD models:\n")
bloc_availability %>%
  dplyr::count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available social-democratic party:\n")
bloc_availability %>%
  dplyr::filter(social_democratic_present == FALSE) %>%
  dplyr::arrange(iso2c_file, year, elec_id) %>%
  print(n = Inf, width = Inf)

cat("\nElections without a structurally available far-right party:\n")
bloc_availability %>%
  dplyr::filter(far_right_present == FALSE) %>%
  dplyr::select(iso2c_file, elec_id, year, party_families_available) %>%
  dplyr::arrange(iso2c_file, year, elec_id) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 5. Attach structural availability and restrict transitions
# ------------------------------------------------

availability_cols_to_remove <- c(
  "social_democratic_present",
  "far_right_present",
  "mainstream_right_present",
  "green_present",
  "far_left_present",
  "non_present",
  "other_left_present",
  "available_far_left",
  "available_green",
  "available_social_democratic",
  "available_mainstream_right",
  "available_far_right",
  "available_left",
  "available_other",
  "available_non",
  "available_other_left_pooled",
  "n_parties",
  "n_detailed_party_blocs",
  "n_pooled_blocs",
  "party_families_available",
  "pooled_blocs_available"
)

df_all_with_availability <- df_all %>%
  dplyr::select(-dplyr::any_of(availability_cols_to_remove)) %>%
  dplyr::left_join(
    bloc_availability,
    by = c("iso2c_file", "elec_id", "year")
  )

availability_merge_check <- df_all_with_availability %>%
  dplyr::filter(voted_now == TRUE) %>%
  dplyr::distinct(
    iso2c_file,
    elec_id,
    year,
    social_democratic_present,
    far_right_present,
    mainstream_right_present,
    green_present,
    far_left_present,
    non_present
  ) %>%
  dplyr::filter(
    is.na(social_democratic_present) |
      is.na(far_right_present) |
      is.na(mainstream_right_present) |
      is.na(green_present) |
      is.na(far_left_present) |
      is.na(non_present)
  )

cat("\nStructural availability merge check:\n")
print(availability_merge_check, n = Inf, width = Inf)

stopifnot(nrow(availability_merge_check) == 0)

primary_blocs <- c(
  "social_democratic",
  "far_right",
  "mainstream_right",
  "green",
  "far_left",
  "non"
)

df_transitions_primary <- df_all_with_availability %>%
  dplyr::filter(
    voted_now == TRUE,
    switch_from_bloc_detailed %in% primary_blocs,
    switch_to_bloc_detailed %in% primary_blocs,
    social_democratic_present == TRUE
  )

df_transitions_primary_fr_contexts <- df_transitions_primary %>%
  dplyr::filter(far_right_present == TRUE)

cat("\nRestricted primary detailed bloc transition matrix, including non-voters:\n")
df_transitions_primary %>%
  dplyr::count(switch_from_bloc_detailed, switch_to_bloc_detailed) %>%
  print(n = Inf)

cat("\nRestricted primary detailed bloc transition matrix in far-right-present contexts:\n")
df_transitions_primary_fr_contexts %>%
  dplyr::count(switch_from_bloc_detailed, switch_to_bloc_detailed) %>%
  print(n = Inf)

cat("\nRestriction summary, primary transition categories including non-voters:\n")
tibble::tibble(
  dataset = c(
    "unrestricted respondent-elections",
    "primary SD transition universe",
    "primary FR-context transition universe"
  ),
  n = c(
    nrow(df_all_with_availability %>% dplyr::filter(voted_now == TRUE)),
    nrow(df_transitions_primary),
    nrow(df_transitions_primary_fr_contexts)
  )
) %>%
  dplyr::mutate(retained_share = n / n[1]) %>%
  print(width = Inf)

# ------------------------------------------------
# 6. Create primary multinomial datasets
# ------------------------------------------------

outward_sd_switching <- df_transitions_primary %>%
  dplyr::filter(switch_from_bloc_detailed == "social_democratic") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc_detailed == "social_democratic" ~ "retention",
      switch_to_bloc_detailed == "far_left" ~ "to_far_left",
      switch_to_bloc_detailed == "green" ~ "to_green",
      switch_to_bloc_detailed == "mainstream_right" ~ "to_mainstream_right",
      switch_to_bloc_detailed == "far_right" ~ "to_far_right",
      switch_to_bloc_detailed == "non" ~ "to_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "retention",
        "to_far_left",
        "to_green",
        "to_mainstream_right",
        "to_far_right",
        "to_non"
      )
    )
  )

inward_sd_switching <- df_transitions_primary %>%
  dplyr::filter(switch_from_bloc_detailed != "social_democratic") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc_detailed != "social_democratic" ~ "not_to_sd",
      switch_from_bloc_detailed == "far_left" &
        switch_to_bloc_detailed == "social_democratic" ~ "from_far_left",
      switch_from_bloc_detailed == "green" &
        switch_to_bloc_detailed == "social_democratic" ~ "from_green",
      switch_from_bloc_detailed == "mainstream_right" &
        switch_to_bloc_detailed == "social_democratic" ~ "from_mainstream_right",
      switch_from_bloc_detailed == "far_right" &
        switch_to_bloc_detailed == "social_democratic" ~ "from_far_right",
      switch_from_bloc_detailed == "non" &
        switch_to_bloc_detailed == "social_democratic" ~ "from_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "not_to_sd",
        "from_far_left",
        "from_green",
        "from_mainstream_right",
        "from_far_right",
        "from_non"
      )
    )
  )

outward_fr_switching <- df_transitions_primary_fr_contexts %>%
  dplyr::filter(switch_from_bloc_detailed == "far_right") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc_detailed == "far_right" ~ "retention",
      switch_to_bloc_detailed == "social_democratic" ~ "to_social_democratic",
      switch_to_bloc_detailed == "far_left" ~ "to_far_left",
      switch_to_bloc_detailed == "green" ~ "to_green",
      switch_to_bloc_detailed == "mainstream_right" ~ "to_mainstream_right",
      switch_to_bloc_detailed == "non" ~ "to_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "retention",
        "to_social_democratic",
        "to_far_left",
        "to_green",
        "to_mainstream_right",
        "to_non"
      )
    )
  )

inward_fr_switching <- df_transitions_primary_fr_contexts %>%
  dplyr::filter(switch_from_bloc_detailed != "far_right") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc_detailed != "far_right" ~ "not_to_fr",
      switch_from_bloc_detailed == "social_democratic" &
        switch_to_bloc_detailed == "far_right" ~ "from_social_democratic",
      switch_from_bloc_detailed == "far_left" &
        switch_to_bloc_detailed == "far_right" ~ "from_far_left",
      switch_from_bloc_detailed == "green" &
        switch_to_bloc_detailed == "far_right" ~ "from_green",
      switch_from_bloc_detailed == "mainstream_right" &
        switch_to_bloc_detailed == "far_right" ~ "from_mainstream_right",
      switch_from_bloc_detailed == "non" &
        switch_to_bloc_detailed == "far_right" ~ "from_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "not_to_fr",
        "from_social_democratic",
        "from_far_left",
        "from_green",
        "from_mainstream_right",
        "from_non"
      )
    )
  )

# ------------------------------------------------
# 7. Robustness datasets with non and pooled other-left
# ------------------------------------------------

robustness_blocs <- c(
  "social_democratic",
  "far_right",
  "mainstream_right",
  "other_left",
  "non"
)

df_transitions_robustness <- df_all_with_availability %>%
  dplyr::filter(
    voted_now == TRUE,
    switch_from_bloc %in% robustness_blocs,
    switch_to_bloc %in% robustness_blocs,
    social_democratic_present == TRUE
  )

outward_sd_switching_robustness <- df_transitions_robustness %>%
  dplyr::filter(switch_from_bloc == "social_democratic") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc == "social_democratic" ~ "retention",
      switch_to_bloc == "other_left" ~ "to_other_left",
      switch_to_bloc == "mainstream_right" ~ "to_mainstream_right",
      switch_to_bloc == "far_right" ~ "to_far_right",
      switch_to_bloc == "non" ~ "to_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "retention",
        "to_other_left",
        "to_mainstream_right",
        "to_far_right",
        "to_non"
      )
    )
  )

inward_sd_switching_robustness <- df_transitions_robustness %>%
  dplyr::filter(switch_from_bloc != "social_democratic") %>%
  dplyr::mutate(
    outcome = dplyr::case_when(
      switch_to_bloc != "social_democratic" ~ "not_to_sd",
      switch_from_bloc == "other_left" &
        switch_to_bloc == "social_democratic" ~ "from_other_left",
      switch_from_bloc == "mainstream_right" &
        switch_to_bloc == "social_democratic" ~ "from_mainstream_right",
      switch_from_bloc == "far_right" &
        switch_to_bloc == "social_democratic" ~ "from_far_right",
      switch_from_bloc == "non" &
        switch_to_bloc == "social_democratic" ~ "from_non",
      TRUE ~ NA_character_
    ),
    outcome = factor(
      outcome,
      levels = c(
        "not_to_sd",
        "from_other_left",
        "from_mainstream_right",
        "from_far_right",
        "from_non"
      )
    )
  )

cat("\nRestricted robustness pooled bloc transition matrix:\n")
df_transitions_robustness %>%
  dplyr::count(switch_from_bloc, switch_to_bloc) %>%
  print(n = Inf)

cat("\nRestriction summary, robustness transition categories:\n")
tibble::tibble(
  unrestricted_rows = nrow(df_all_with_availability %>% dplyr::filter(voted_now == TRUE)),
  restricted_rows = nrow(df_transitions_robustness),
  retained_share = restricted_rows / unrestricted_rows
) %>%
  print(width = Inf)

# ------------------------------------------------
# 8. Choice-set diagnostics for later multinomial denominators
# ------------------------------------------------

cat("\nOutward SD structural availability by realised outcome:\n")
outward_sd_switching %>%
  dplyr::mutate(
    realised_destination_available = dplyr::case_when(
      outcome == "retention" ~ TRUE,
      outcome == "to_far_left" ~ available_far_left,
      outcome == "to_green" ~ available_green,
      outcome == "to_mainstream_right" ~ available_mainstream_right,
      outcome == "to_far_right" ~ available_far_right,
      outcome == "to_non" ~ available_non,
      TRUE ~ NA
    )
  ) %>%
  dplyr::count(outcome, realised_destination_available) %>%
  print(n = Inf, width = Inf)

cat("\nInward SD structural availability by realised origin:\n")
inward_sd_switching %>%
  dplyr::mutate(
    realised_origin_available = dplyr::case_when(
      outcome == "not_to_sd" ~ TRUE,
      outcome == "from_far_left" ~ available_far_left,
      outcome == "from_green" ~ available_green,
      outcome == "from_mainstream_right" ~ available_mainstream_right,
      outcome == "from_far_right" ~ available_far_right,
      outcome == "from_non" ~ available_non,
      TRUE ~ NA
    )
  ) %>%
  dplyr::count(outcome, realised_origin_available) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 9. Diagnostics
# ------------------------------------------------

cat("\nOutward SD outcome support, primary:\n")
print(table(outward_sd_switching$outcome, useNA = "ifany"))

cat("\nInward SD outcome support, primary:\n")
print(table(inward_sd_switching$outcome, useNA = "ifany"))

cat("\nOutward FR outcome support, primary, far-right-present contexts only:\n")
print(table(outward_fr_switching$outcome, useNA = "ifany"))

cat("\nInward FR outcome support, primary, far-right-present contexts only:\n")
print(table(inward_fr_switching$outcome, useNA = "ifany"))

cat("\nOutward SD outcome support, robustness:\n")
print(table(outward_sd_switching_robustness$outcome, useNA = "ifany"))

cat("\nInward SD outcome support, robustness:\n")
print(table(inward_sd_switching_robustness$outcome, useNA = "ifany"))

cat("\nDataset sizes:\n")
tibble::tibble(
  dataset = c(
    "df_transitions_primary",
    "outward_sd_switching",
    "inward_sd_switching",
    "df_transitions_primary_fr_contexts",
    "outward_fr_switching",
    "inward_fr_switching",
    "df_transitions_robustness",
    "outward_sd_switching_robustness",
    "inward_sd_switching_robustness"
  ),
  n = c(
    nrow(df_transitions_primary),
    nrow(outward_sd_switching),
    nrow(inward_sd_switching),
    nrow(df_transitions_primary_fr_contexts),
    nrow(outward_fr_switching),
    nrow(inward_fr_switching),
    nrow(df_transitions_robustness),
    nrow(outward_sd_switching_robustness),
    nrow(inward_sd_switching_robustness)
  )
) %>%
  print(width = Inf)

cat("\nOutward switching from social democracy, primary:\n")
outward_sd_switching %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nInward switching to social democracy, primary:\n")
inward_sd_switching %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nOutward switching from far right, primary:\n")
outward_fr_switching %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nInward switching to far right, primary:\n")
inward_fr_switching %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nOutward switching from social democracy, robustness:\n")
outward_sd_switching_robustness %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

cat("\nInward switching to social democracy, robustness:\n")
inward_sd_switching_robustness %>%
  dplyr::count(outcome) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  print(n = Inf)

# ------------------------------------------------
# 10. Validation checks
# ------------------------------------------------

stopifnot(all(df_transitions_primary$social_democratic_present == TRUE))
stopifnot(all(df_transitions_robustness$social_democratic_present == TRUE))

stopifnot(all(df_transitions_primary$switch_from_bloc_detailed %in% primary_blocs))
stopifnot(all(df_transitions_primary$switch_to_bloc_detailed %in% primary_blocs))

stopifnot(all(df_transitions_robustness$switch_from_bloc %in% robustness_blocs))
stopifnot(all(df_transitions_robustness$switch_to_bloc %in% robustness_blocs))

stopifnot(all(outward_sd_switching$switch_from_bloc_detailed == "social_democratic"))
stopifnot(all(inward_sd_switching$switch_from_bloc_detailed != "social_democratic"))

stopifnot(all(outward_fr_switching$switch_from_bloc_detailed == "far_right"))
stopifnot(all(inward_fr_switching$switch_from_bloc_detailed != "far_right"))
stopifnot(all(outward_fr_switching$far_right_present == TRUE))
stopifnot(all(inward_fr_switching$far_right_present == TRUE))

stopifnot(all(outward_sd_switching_robustness$switch_from_bloc == "social_democratic"))
stopifnot(all(inward_sd_switching_robustness$switch_from_bloc != "social_democratic"))

stopifnot(all(!is.na(outward_sd_switching$outcome)))
stopifnot(all(!is.na(inward_sd_switching$outcome)))
stopifnot(all(!is.na(outward_fr_switching$outcome)))
stopifnot(all(!is.na(inward_fr_switching$outcome)))

stopifnot(all(!is.na(outward_sd_switching_robustness$outcome)))
stopifnot(all(!is.na(inward_sd_switching_robustness$outcome)))

stopifnot(all(levels(outward_sd_switching$outcome) == c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)))

stopifnot(all(levels(inward_sd_switching$outcome) == c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)))

stopifnot(all(levels(outward_fr_switching$outcome) == c(
  "retention",
  "to_social_democratic",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_non"
)))

stopifnot(all(levels(inward_fr_switching$outcome) == c(
  "not_to_fr",
  "from_social_democratic",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_non"
)))

stopifnot(all(levels(outward_sd_switching_robustness$outcome) == c(
  "retention",
  "to_other_left",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)))

stopifnot(all(levels(inward_sd_switching_robustness$outcome) == c(
  "not_to_sd",
  "from_other_left",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)))

# ------------------------------------------------
# 11. Save
# ------------------------------------------------

saveRDS(
  df_all_with_availability,
  file.path(analysis_dir, "df_realised_transitions_all_social_democratic.rds")
)

saveRDS(
  bloc_availability,
  file.path(analysis_dir, "bloc_availability_social_democratic.rds")
)

saveRDS(
  df_transitions_primary,
  file.path(analysis_dir, "df_realised_transitions_primary_social_democratic.rds")
)

saveRDS(
  df_transitions_primary_fr_contexts,
  file.path(analysis_dir, "df_realised_transitions_primary_fr_contexts_social_democratic.rds")
)

saveRDS(
  outward_sd_switching,
  file.path(analysis_dir, "df_outward_social_democratic.rds")
)

saveRDS(
  inward_sd_switching,
  file.path(analysis_dir, "df_inward_social_democratic.rds")
)

saveRDS(
  outward_fr_switching,
  file.path(analysis_dir, "df_outward_far_right_social_democratic_project.rds")
)

saveRDS(
  inward_fr_switching,
  file.path(analysis_dir, "df_inward_far_right_social_democratic_project.rds")
)

saveRDS(
  df_transitions_robustness,
  file.path(
    analysis_dir,
    "df_realised_transitions_pooled_other_left_non_social_democratic.rds"
  )
)

saveRDS(
  outward_sd_switching_robustness,
  file.path(
    analysis_dir,
    "df_outward_social_democratic_pooled_other_left_non.rds"
  )
)

saveRDS(
  inward_sd_switching_robustness,
  file.path(
    analysis_dir,
    "df_inward_social_democratic_pooled_other_left_non.rds"
  )
)

cat("\nDatasets saved to:\n")
cat(file.path(analysis_dir, "df_realised_transitions_all_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "bloc_availability_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_realised_transitions_primary_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_realised_transitions_primary_fr_contexts_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_outward_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_inward_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_outward_far_right_social_democratic_project.rds"), "\n")
cat(file.path(analysis_dir, "df_inward_far_right_social_democratic_project.rds"), "\n")
cat(file.path(analysis_dir, "df_realised_transitions_pooled_other_left_non_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_outward_social_democratic_pooled_other_left_non.rds"), "\n")
cat(file.path(analysis_dir, "df_inward_social_democratic_pooled_other_left_non.rds"), "\n")

cat("\nScript completed successfully\n")