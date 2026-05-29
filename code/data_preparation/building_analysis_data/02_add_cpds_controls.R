# ================================================================
# 02_add_cpds_controls.R
# Build CPDS election controls and merge them into the social-
# democratic inward/outward analysis datasets.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(haven)
  library(tibble)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

processed_dir <- file.path(project_dir, "data", "processed")
analysis_dir <- file.path(project_dir, "data", "analysis", "building_analysis_data")
diagnostic_dir <- file.path(processed_dir, "cpds_diagnostics")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

path_cpds_raw <- file.path(
  project_dir,
  "data",
  "external",
  "CPDS_1960-2023_Update_2025.dta"
)

path_election_contexts <- file.path(processed_dir, "election_contexts.rds")

path_outward <- file.path(
  analysis_dir,
  "df_analysis_outward_social_democratic.rds"
)

path_inward <- file.path(
  analysis_dir,
  "df_analysis_inward_social_democratic.rds"
)

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Required input not found: ", path, call. = FALSE)
  }
}

invisible(lapply(
  c(path_cpds_raw, path_election_contexts, path_outward, path_inward),
  stop_if_missing
))

iso2_to_iso3 <- c(
  "AT" = "AUT", "AU" = "AUS", "BE" = "BEL", "BG" = "BGR",
  "CA" = "CAN", "CH" = "CHE", "CZ" = "CZE", "DE" = "DEU",
  "DK" = "DNK", "EE" = "EST", "ES" = "ESP", "FI" = "FIN",
  "FR" = "FRA", "GB" = "GBR", "GR" = "GRC", "HU" = "HUN",
  "IE" = "IRL", "IL" = "ISR", "IS" = "ISL", "IT" = "ITA",
  "LT" = "LTU", "LV" = "LVA", "NL" = "NLD", "NO" = "NOR",
  "NZ" = "NZL", "PL" = "POL", "PT" = "PRT", "RO" = "ROU",
  "SE" = "SWE", "SI" = "SVN", "SK" = "SVK"
)

scale_z <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

cpds_raw <- haven::read_dta(path_cpds_raw) %>%
  dplyr::mutate(
    year = as.integer(year),
    cpds_left_incumbent = dplyr::if_else(
      !is.na(gov_left1) & gov_left1 > 0,
      1,
      0
    )
  ) %>%
  dplyr::arrange(iso, year) %>%
  dplyr::group_by(iso) %>%
  dplyr::mutate(
    cpds_realgdpgr_lag1 = dplyr::lag(realgdpgr),
    cpds_unemp_lag1 = dplyr::lag(unemp),
    cpds_outlays_lag1 = dplyr::lag(outlays),
    cpds_openc_lag1 = dplyr::lag(openc),
    cpds_ud_lag1 = dplyr::lag(ud),
    cpds_postfisc_gini_lag1 = dplyr::lag(postfisc_gini)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    cpds_iso3 = iso,
    cpds_country_name = country,
    election_year = year,
    cpds_election_date = as.Date(elect),
    cpds_vturn = suppressWarnings(as.numeric(vturn)),
    cpds_left_incumbent,
    cpds_gov_left1 = suppressWarnings(as.numeric(gov_left1)),
    cpds_effpar_ele = suppressWarnings(as.numeric(effpar_ele)),
    cpds_dis_gall = suppressWarnings(as.numeric(dis_gall)),
    cpds_realgdpgr_lag1,
    cpds_unemp_lag1,
    cpds_outlays_lag1,
    cpds_openc_lag1,
    cpds_ud_lag1,
    cpds_postfisc_gini_lag1
  )

election_contexts <- readRDS(path_election_contexts) %>%
  dplyr::mutate(
    country = as.character(country),
    cpds_iso3 = unname(iso2_to_iso3[country]),
    election_year = as.integer(year),
    election_date = as.Date(election_date)
  ) %>%
  dplyr::select(elec_id, country, election_date, election_year, cpds_iso3)

cpds_controls <- election_contexts %>%
  dplyr::left_join(
    cpds_raw,
    by = c("cpds_iso3", "election_year")
  ) %>%
  dplyr::mutate(
    exact_date_match = !is.na(cpds_election_date) &
      cpds_election_date == election_date,
    cpds_days_between_project_and_cpds_election =
      as.integer(election_date - cpds_election_date),
    cpds_match_status = dplyr::case_when(
      is.na(cpds_iso3) ~ "missing_iso3_mapping",
      is.na(cpds_country_name) ~ "missing_country_year",
      TRUE ~ "matched_country_year"
    ),
    cpds_multiple_election_year_warning = FALSE,
    cpds_vturn_z = scale_z(cpds_vturn),
    cpds_effpar_ele_z = scale_z(cpds_effpar_ele),
    cpds_dis_gall_z = scale_z(cpds_dis_gall),
    cpds_realgdpgr_lag1_z = scale_z(cpds_realgdpgr_lag1),
    cpds_unemp_lag1_z = scale_z(cpds_unemp_lag1),
    cpds_outlays_lag1_z = scale_z(cpds_outlays_lag1),
    cpds_openc_lag1_z = scale_z(cpds_openc_lag1),
    cpds_ud_lag1_z = scale_z(cpds_ud_lag1),
    cpds_postfisc_gini_lag1_z = scale_z(cpds_postfisc_gini_lag1)
  )

cpds_control_specification <- list(
  source = "Comparative Political Data Set, 1960-2023 Update 2025",
  raw_file = "data/external/CPDS_1960-2023_Update_2025.dta",
  merge_key = c("country ISO3", "election year"),
  lagged_controls = c(
    "realgdpgr", "unemp", "outlays", "openc", "ud", "postfisc_gini"
  ),
  standardized_controls = c(
    "cpds_vturn_z",
    "cpds_effpar_ele_z",
    "cpds_dis_gall_z",
    "cpds_realgdpgr_lag1_z",
    "cpds_unemp_lag1_z",
    "cpds_outlays_lag1_z",
    "cpds_openc_lag1_z",
    "cpds_ud_lag1_z",
    "cpds_postfisc_gini_lag1_z"
  )
)

write_csv(
  cpds_controls,
  file.path(processed_dir, "cpds_election_controls_model_input.csv")
)

saveRDS(
  cpds_controls,
  file.path(processed_dir, "cpds_election_controls_model_input.rds")
)

saveRDS(
  cpds_control_specification,
  file.path(processed_dir, "cpds_control_specification.rds")
)

cpds_link_diagnostics <- cpds_controls %>%
  dplyr::count(cpds_match_status, name = "n_elections")

write_csv(
  cpds_link_diagnostics,
  file.path(processed_dir, "cpds_election_controls_link_diagnostics.csv")
)

saveRDS(
  cpds_link_diagnostics,
  file.path(processed_dir, "cpds_election_controls_link_diagnostics.rds")
)

merge_cpds_controls <- function(path_in, path_out) {
  df <- readRDS(path_in)

  df_cpds <- df %>%
    dplyr::select(-dplyr::any_of(setdiff(names(cpds_controls), "elec_id"))) %>%
    dplyr::left_join(cpds_controls, by = "elec_id")

  saveRDS(df_cpds, path_out)

  invisible(df_cpds)
}

df_out <- merge_cpds_controls(
  path_outward,
  file.path(analysis_dir, "df_analysis_outward_social_democratic_cpds_controls.rds")
)

df_in <- merge_cpds_controls(
  path_inward,
  file.path(analysis_dir, "df_analysis_inward_social_democratic_cpds_controls.rds")
)

cat("\nCPDS control preparation completed.\n")
cat("Election controls:", nrow(cpds_controls), "rows\n")
cat("Outward rows:", nrow(df_out), "\n")
cat("Inward rows:", nrow(df_in), "\n")
print(cpds_link_diagnostics)
