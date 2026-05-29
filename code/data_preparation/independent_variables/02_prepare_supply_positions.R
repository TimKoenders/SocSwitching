# ================================================================
# 02_prepare_supply_positions.R
# Social-democratic vote-switching project
#
# Main supply-side position indicators from MARPOR:
#   1) sd_investmentconsumption_std
#   2) sd_stateconomy_std
#   3) sd_libcons_std
#
# Main dynamic position indicators:
#   1) sd_investmentconsumption_move_std
#   2) sd_stateconomy_move_std
#   3) sd_libcons_move_std
#
# Investment-consumption operationalisations:
#   1) marpor_narrow:
#        investment pole  = per506 Education Expansion + per702 Labour Groups Negative
#        consumption pole = per507 Education Limitation + per701 Labour Groups Positive
#
#   2) marpor_complete:
#        same investment-consumption coding as marpor_narrow, but combined with
#        the broader complete state-economy and liberal-conservative measures.
#        This remains the primary operationalisation used in the model-ready merge.
#
#   3) marpor_education_only:
#        investment pole  = per506 Education Expansion
#        consumption pole = per507 Education Limitation
#
#   4) marpor_abou_chadi_wagner:
#        investment pole  = per402 Incentives: Positive + per406 Protectionism: Negative +
#                           per411 Technology and Infrastructure + per506 Education Expansion
#        consumption pole = per407 Protectionism: Positive + per409 Keynesian Demand Management +
#                           per412 Controlled Economy + per701 Labour Groups: Positive
#
# Higher investmentconsumption = more investment-oriented.
# Higher stateconomy = more market-oriented relative to state-oriented.
# Higher libcons = more conservative/nationalist relative to liberal/cosmopolitan.
#
# Also adds:
#   - individual controls
#   - ENP / party-system fragmentation
#   - structural party-system availability for later choice-set denominators
# ================================================================

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(readxl)
  library(tibble)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

analysis_dir <- file.path(project_dir, "data", "analysis")

manifesto_file <- file.path(
  project_dir,
  "data",
  "manifesto",
  "MPDataset_MPDS2025a_stata14.dta"
)

enp_file <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "external", "Electoral-fragmentation-2026.xlsx")

primary_operationalisation <- "marpor_complete"

# ------------------------------------------------
# 2. Helper functions
# ------------------------------------------------

scale_z <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric((x - m) / s)
}

weighted_or_mean <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  valid_x <- !is.na(x)
  if (!any(valid_x)) return(NA_real_)
  valid_w <- valid_x & !is.na(w) & w > 0
  if (any(valid_w) && sum(w[valid_w], na.rm = TRUE) > 0) {
    return(stats::weighted.mean(x[valid_w], w[valid_w], na.rm = TRUE))
  }
  mean(x[valid_x], na.rm = TRUE)
}

make_edate_from_elec_id <- function(elec_id) {
  year <- stringr::str_extract(elec_id, "\\d{4}") %>% as.integer()
  month <- stringr::str_extract(elec_id, "(?<=-)\\d{2}$") %>% as.integer()
  month <- ifelse(is.na(month), 6L, month)
  as.Date(sprintf("%04d-%02d-15", year, month))
}

sum_codes <- function(df, codes) {
  out <- rep(0, nrow(df))
  for (code in codes) {
    if (!code %in% names(df)) {
      stop("MARPOR code not found in manifesto data: ", code)
    }
    out <- out + as.numeric(df[[code]])
  }
  out
}

make_marpor_positions <- function(
    df,
    operationalisation,
    state_measure = c("narrow", "complete"),
    libcons_measure = c("narrow", "complete"),
    investment_codes,
    consumption_codes
) {
  state_measure <- match.arg(state_measure)
  libcons_measure <- match.arg(libcons_measure)
  
  if (state_measure == "narrow") {
    state_left_codes <- c("per504")
    state_right_codes <- c("per505")
  } else {
    state_left_codes <- c("per403", "per404", "per406", "per412", "per413", "per504", "per506", "per701")
    state_right_codes <- c("per401", "per402", "per407", "per414", "per505")
  }
  
  if (libcons_measure == "narrow") {
    lib_left_codes <- c("per602", "per607")
    lib_right_codes <- c("per601", "per608")
  } else {
    lib_left_codes <- c("per103", "per105", "per106", "per107", "per201", "per202")
    lib_right_codes <- c("per104", "per203", "per305", "per601", "per603", "per605", "per606")
  }
  
  df %>%
    dplyr::mutate(
      operationalisation = operationalisation,
      source = "marpor",
      countryname = as.character(countryname),
      party = as.numeric(party),
      partyname = as.character(partyname),
      source_edate = as.Date(edate),
      total = as.numeric(total),
      pervote = as.numeric(pervote),
      
      state_left_share = sum_codes(dplyr::cur_data_all(), state_left_codes),
      state_right_share = sum_codes(dplyr::cur_data_all(), state_right_codes),
      state_left_n = total * state_left_share / 100,
      state_right_n = total * state_right_share / 100,
      stateconomy = dplyr::if_else(
        !is.na(total) & total > 0,
        log((state_right_n + 0.5) / (state_left_n + 0.5)),
        NA_real_
      ),
      
      lib_left_share = sum_codes(dplyr::cur_data_all(), lib_left_codes),
      lib_right_share = sum_codes(dplyr::cur_data_all(), lib_right_codes),
      lib_left_n = total * lib_left_share / 100,
      lib_right_n = total * lib_right_share / 100,
      libcons = dplyr::if_else(
        !is.na(total) & total > 0,
        log((lib_right_n + 0.5) / (lib_left_n + 0.5)),
        NA_real_
      ),
      
      investment_share = sum_codes(dplyr::cur_data_all(), investment_codes),
      consumption_share = sum_codes(dplyr::cur_data_all(), consumption_codes),
      investment_n = total * investment_share / 100,
      consumption_n = total * consumption_share / 100,
      investmentconsumption = dplyr::if_else(
        !is.na(total) & total > 0,
        log((investment_n + 0.5) / (consumption_n + 0.5)),
        NA_real_
      )
    ) %>%
    dplyr::transmute(
      operationalisation,
      source,
      countryname,
      party,
      partyname,
      source_edate,
      marpor_party_family = as.numeric(parfam),
      marpor_vote_share = pervote,
      investmentconsumption,
      stateconomy,
      libcons,
      total
    )
}

# ------------------------------------------------
# 3. Load realised-transition datasets
# ------------------------------------------------

df_realised_transitions <- readRDS(
  file.path(analysis_dir, "df_realised_transitions_all_social_democratic.rds")
)

df_outward_social_democratic <- readRDS(
  file.path(analysis_dir, "df_outward_social_democratic.rds")
)

df_inward_social_democratic <- readRDS(
  file.path(analysis_dir, "df_inward_social_democratic.rds")
)

df_outward_far_right <- readRDS(
  file.path(analysis_dir, "df_outward_far_right_social_democratic_project.rds")
)

df_inward_far_right <- readRDS(
  file.path(analysis_dir, "df_inward_far_right_social_democratic_project.rds")
)

bloc_availability <- readRDS(
  file.path(analysis_dir, "bloc_availability_social_democratic.rds")
)

df_manifesto <- haven::read_dta(manifesto_file) %>%
  dplyr::mutate(party = as.numeric(party))

stopifnot(is.data.frame(df_realised_transitions), nrow(df_realised_transitions) > 0)
stopifnot(is.data.frame(df_outward_social_democratic), nrow(df_outward_social_democratic) > 0)
stopifnot(is.data.frame(df_inward_social_democratic), nrow(df_inward_social_democratic) > 0)
stopifnot(is.data.frame(bloc_availability), nrow(bloc_availability) > 0)

cat("\nDatasets loaded\n")
cat("\nRealised transitions:\n")
print(dim(df_realised_transitions))

# ------------------------------------------------
# 4. Add origin and destination variables
# ------------------------------------------------

origin_destination_lookup <- df_realised_transitions %>%
  dplyr::group_by(iso2c_file, source_file, year, elec_id, id) %>%
  dplyr::summarise(
    origin_alt_id = as.character(alt[voted_lag %in% TRUE][1]),
    origin_party = as.numeric(party[voted_lag %in% TRUE][1]),
    origin_party_name = as.character(party_name_map[voted_lag %in% TRUE][1]),
    origin_peid = as.character(peid_map[voted_lag %in% TRUE][1]),
    origin_parfam = as.character(parfam_final[voted_lag %in% TRUE][1]),
    origin_bloc = as.character(switch_from_bloc[voted_lag %in% TRUE][1]),
    origin_bloc_detailed = as.character(switch_from_bloc_detailed[voted_lag %in% TRUE][1]),
    origin_social_democratic = social_democratic[voted_lag %in% TRUE][1],
    origin_far_right = far_right[voted_lag %in% TRUE][1],
    origin_mainstream_right = mainstream_right[voted_lag %in% TRUE][1],
    origin_green = green[voted_lag %in% TRUE][1],
    origin_far_left = far_left[voted_lag %in% TRUE][1],
    
    destination_alt_id = as.character(alt[voted_now %in% TRUE][1]),
    destination_party = as.numeric(party[voted_now %in% TRUE][1]),
    destination_party_name = as.character(party_name_map[voted_now %in% TRUE][1]),
    destination_peid = as.character(peid_map[voted_now %in% TRUE][1]),
    destination_parfam = as.character(parfam_final[voted_now %in% TRUE][1]),
    destination_bloc = as.character(switch_to_bloc[voted_now %in% TRUE][1]),
    destination_bloc_detailed = as.character(switch_to_bloc_detailed[voted_now %in% TRUE][1]),
    destination_social_democratic = social_democratic[voted_now %in% TRUE][1],
    destination_far_right = far_right[voted_now %in% TRUE][1],
    destination_mainstream_right = mainstream_right[voted_now %in% TRUE][1],
    destination_green = green[voted_now %in% TRUE][1],
    destination_far_left = far_left[voted_now %in% TRUE][1],
    .groups = "drop"
  )

party_lookup <- df_realised_transitions %>%
  dplyr::distinct(
    elec_id,
    iso2c_file,
    year,
    alt_id = alt,
    party,
    party_name = party_name_map,
    peid = peid_map,
    parfam_final,
    bloc,
    party_bloc_detailed,
    social_democratic,
    far_right,
    mainstream_right,
    green,
    far_left,
    other_left,
    non_voter
  ) %>%
  dplyr::mutate(
    alt_id = as.character(alt_id),
    party = as.numeric(party),
    year = as.integer(year)
  )

add_origin_destination <- function(df) {
  df %>%
    dplyr::select(
      -dplyr::any_of(
        names(origin_destination_lookup)[
          !names(origin_destination_lookup) %in%
            c("iso2c_file", "source_file", "year", "elec_id", "id")
        ]
      )
    ) %>%
    dplyr::left_join(
      origin_destination_lookup,
      by = c("iso2c_file", "source_file", "year", "elec_id", "id")
    )
}

df_outward_social_democratic <- add_origin_destination(df_outward_social_democratic)
df_inward_social_democratic <- add_origin_destination(df_inward_social_democratic)
df_outward_far_right <- add_origin_destination(df_outward_far_right)
df_inward_far_right <- add_origin_destination(df_inward_far_right)

# ------------------------------------------------
# 5. Required variable check
# ------------------------------------------------

required_vars <- c(
  "iso2c_file", "source_file", "year", "elec_id", "id",
  "origin_alt_id", "origin_party", "origin_party_name", "origin_peid",
  "origin_parfam", "origin_bloc", "origin_bloc_detailed",
  "destination_alt_id", "destination_party", "destination_party_name", "destination_peid",
  "destination_parfam", "destination_bloc", "destination_bloc_detailed",
  "age", "gender", "lrself", "satdem", "weights", "outcome"
)

check_required <- function(df, name) {
  missing_vars <- setdiff(required_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(name, " is missing required variables: ", paste(missing_vars, collapse = ", "))
  }
}

check_required(df_outward_social_democratic, "df_outward_social_democratic")
check_required(df_inward_social_democratic, "df_inward_social_democratic")
check_required(df_outward_far_right, "df_outward_far_right")
check_required(df_inward_far_right, "df_inward_far_right")

cat("\nAll required variables are present in the four multinomial datasets\n")

# ------------------------------------------------
# 6. Country mapping
# ------------------------------------------------

country_prefix_map <- tibble::tribble(
  ~countryname,                 ~iso2c_file,
  "Albania",                    "AL",
  "Argentina",                  "AR",
  "Armenia",                    "AM",
  "Australia",                  "AU",
  "Austria",                    "AT",
  "Azerbaijan",                 "AZ",
  "Belarus",                    "BY",
  "Belgium",                    "BE",
  "Bolivia",                    "BO",
  "Bosnia-Herzegovina",         "BA",
  "Brazil",                     "BR",
  "Bulgaria",                   "BG",
  "Canada",                     "CA",
  "Chile",                      "CL",
  "Colombia",                   "CO",
  "Costa Rica",                 "CR",
  "Croatia",                    "HR",
  "Cyprus",                     "CY",
  "Czechia",                    "CZ",
  "Czech Republic",             "CZ",
  "Denmark",                    "DK",
  "Dominican Republic",         "DO",
  "Ecuador",                    "EC",
  "Estonia",                    "EE",
  "Finland",                    "FI",
  "France",                     "FR",
  "Georgia",                    "GE",
  "Germany",                    "DE",
  "German Democratic Republic", "DD",
  "Great Britain",              "GB",
  "United Kingdom",             "GB",
  "Greece",                     "GR",
  "Hungary",                    "HU",
  "Iceland",                    "IS",
  "Ireland",                    "IE",
  "Israel",                     "IL",
  "Italy",                      "IT",
  "Japan",                      "JP",
  "Latvia",                     "LV",
  "Lithuania",                  "LT",
  "Luxembourg",                 "LU",
  "Malta",                      "MT",
  "Mexico",                     "MX",
  "Moldova",                    "MD",
  "Montenegro",                 "ME",
  "Netherlands",                "NL",
  "New Zealand",                "NZ",
  "North Macedonia",            "MK",
  "Norway",                     "NO",
  "Panama",                     "PA",
  "Peru",                       "PE",
  "Poland",                     "PL",
  "Portugal",                   "PT",
  "Romania",                    "RO",
  "Russia",                     "RU",
  "Serbia",                     "RS",
  "Slovakia",                   "SK",
  "Slovenia",                   "SI",
  "South Africa",               "ZA",
  "South Korea",                "KR",
  "Spain",                      "ES",
  "Sri Lanka",                  "LK",
  "Sweden",                     "SE",
  "Switzerland",                "CH",
  "Turkey",                     "TR",
  "Ukraine",                    "UA",
  "United States",              "US",
  "Uruguay",                    "UY"
)

# ------------------------------------------------
# 7. Prepare individual-level controls
# ------------------------------------------------

prepare_controls <- function(df) {
  df %>%
    dplyr::mutate(
      year = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
      country_id = factor(iso2c_file),
      election_id = factor(elec_id),
      origin_alt_id = as.character(origin_alt_id),
      destination_alt_id = as.character(destination_alt_id),
      origin_party = as.numeric(origin_party),
      destination_party = as.numeric(destination_party),
      origin_party_election_id = factor(paste(elec_id, origin_alt_id, sep = "___")),
      destination_party_election_id = factor(paste(elec_id, destination_alt_id, sep = "___")),
      gender = dplyr::case_when(
        as.character(gender) %in% c("1", "male", "Male", "Man", "man") ~ 1,
        as.character(gender) %in% c("0", "2", "female", "Female", "Vrouw", "vrouw") ~ 0,
        TRUE ~ NA_real_
      ),
      age = as.numeric(age),
      age = dplyr::if_else(!is.na(age) & age >= 18 & age <= 110, age, NA_real_),
      age_group = dplyr::case_when(
        !is.na(age) & age < 35 ~ "18-34",
        !is.na(age) & age >= 35 & age < 55 ~ "35-54",
        !is.na(age) & age >= 55 ~ "55+",
        TRUE ~ NA_character_
      ),
      age_group = factor(age_group, levels = c("18-34", "35-54", "55+")),
      lrself = as.numeric(lrself),
      satdem = as.numeric(satdem),
      lrself = dplyr::if_else(!is.na(lrself) & lrself >= 0 & lrself <= 10, lrself, NA_real_),
      satdem = dplyr::if_else(!is.na(satdem) & satdem >= 1 & satdem <= 4, satdem, NA_real_),
      lrself_z = scale_z(lrself),
      satdem_z = scale_z(satdem),
      weights = dplyr::if_else(is.na(weights), 1, as.numeric(weights))
    )
}

df_outward_social_democratic <- prepare_controls(df_outward_social_democratic)
df_inward_social_democratic <- prepare_controls(df_inward_social_democratic)
df_outward_far_right <- prepare_controls(df_outward_far_right)
df_inward_far_right <- prepare_controls(df_inward_far_right)

# ------------------------------------------------
# 8. Prepare ENP data
# ------------------------------------------------

df_enp_raw <- readxl::read_excel(enp_file)

df_enp <- df_enp_raw %>%
  dplyr::rename(
    country_raw = country,
    year_raw = year,
    enp_raw = enep
  ) %>%
  dplyr::mutate(
    countryname = country_raw %>%
      as.character() %>%
      stringr::str_remove("\\s+(I|II|III|IV)$") %>%
      stringr::str_trim(),
    year = year_raw %>%
      as.character() %>%
      stringr::str_extract("\\d{4}") %>%
      as.integer(),
    enp = enp_raw %>%
      as.character() %>%
      stringr::str_replace(",", ".") %>%
      as.numeric()
  ) %>%
  dplyr::left_join(country_prefix_map, by = "countryname") %>%
  dplyr::filter(!is.na(iso2c_file), !is.na(year), !is.na(enp)) %>%
  dplyr::arrange(iso2c_file, year) %>%
  dplyr::distinct(iso2c_file, year, .keep_all = TRUE) %>%
  dplyr::mutate(enp_z = scale_z(enp))

# ------------------------------------------------
# 9. Manual MARPOR party-code corrections
# ------------------------------------------------

party_code_manual <- tibble::tribble(
  ~iso2c_file, ~elec_id,      ~party_name,                                            ~party_manual,
  "AT",        "AT-2019-09",  "Austrian People's Party",                              42520,
  "AT",        "AT-2019-09",  "NEOS",                                                 42430,
  "AT",        "AT-2019-09",  "Social Democratic Party of Austria",                  42320,
  "FR",        "FR-2022-04",  "The Republic Onwards!",                                31425,
  "FR",        "FR-2022-04",  "The Republicans",                                      31061,
  "FR",        "FR-2022-04",  "Socialist Party",                                      31320,
  "NL",        "NL-2021-03",  "Democrats'66",                                         22330,
  "NL",        "NL-2021-03",  "People's Party for Freedom and Democracy",             22420,
  "NL",        "NL-2021-03",  "Labour Party",                                         22320,
  "NL",        "NL-2021-03",  "DENK",                                                 22321,
  "NZ",        "NZ-2020-10",  "ACT New Zealand",                                      64420,
  "NZ",        "NZ-2020-10",  "New Zealand Labour Party",                             64320,
  "PT",        "PT-2019-10",  "Alliance",                                             35313,
  "PT",        "PT-2022-01",  "Social Democratic Party",                              35313,
  "PT",        "PT-2022-01",  "Liberal Initiative",                                   35410,
  "PT",        "PT-2022-01",  "CDS-PP",                                               35520,
  "PT",        "PT-2022-01",  "Socialist Party",                                      35311,
  "RO",        "RO-2004-11",  "Christian Democratic National Peasants' Party (PNTCD)", 93041,
  "SE",        "SE-2022-09",  "Liberals",                                             11420,
  "SE",        "SE-2022-09",  "Christian Democrats",                                  11520,
  "SE",        "SE-2022-09",  "Moderate Party",                                       11620,
  "SE",        "SE-2022-09",  "Centre Party",                                         11810,
  "SE",        "SE-2022-09",  "Social Democrats",                                     11320
)

party_lookup <- party_lookup %>%
  dplyr::left_join(
    party_code_manual,
    by = c("iso2c_file", "elec_id", "party_name")
  ) %>%
  dplyr::mutate(
    party = dplyr::coalesce(as.numeric(party), as.numeric(party_manual))
  ) %>%
  dplyr::select(-party_manual)

party_lookup_marpor <- party_lookup %>%
  dplyr::filter(!is.na(party)) %>%
  dplyr::arrange(
    elec_id,
    iso2c_file,
    party,
    dplyr::desc(social_democratic),
    dplyr::desc(far_right),
    dplyr::desc(mainstream_right),
    dplyr::desc(green),
    dplyr::desc(far_left),
    alt_id
  ) %>%
  dplyr::group_by(elec_id, iso2c_file, party) %>%
  dplyr::summarise(
    alt_id = dplyr::first(alt_id),
    party_name = dplyr::first(party_name),
    peid = dplyr::first(peid),
    parfam_final = dplyr::first(parfam_final),
    bloc = dplyr::first(bloc),
    party_bloc_detailed = dplyr::first(party_bloc_detailed),
    social_democratic = any(social_democratic == TRUE, na.rm = TRUE),
    far_right = any(far_right == TRUE, na.rm = TRUE),
    mainstream_right = any(mainstream_right == TRUE, na.rm = TRUE),
    green = any(green == TRUE, na.rm = TRUE),
    far_left = any(far_left == TRUE, na.rm = TRUE),
    other_left = any(other_left == TRUE, na.rm = TRUE),
    non_voter = any(non_voter == TRUE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nDuplicate MARPOR party keys after collapsing party_lookup:\n")
party_lookup_marpor %>%
  dplyr::count(elec_id, iso2c_file, party) %>%
  dplyr::filter(n > 1) %>%
  print(n = Inf)

# ------------------------------------------------
# 10. Prepare MARPOR position measures
# ------------------------------------------------

flow_lookup <- party_lookup %>%
  dplyr::distinct(elec_id, iso2c_file) %>%
  dplyr::mutate(flow_edate = make_edate_from_elec_id(elec_id))

positions_marpor_narrow <- make_marpor_positions(
  df = df_manifesto,
  operationalisation = "marpor_narrow",
  state_measure = "narrow",
  libcons_measure = "narrow",
  investment_codes = c("per506", "per702"),
  consumption_codes = c("per507", "per701")
)

positions_marpor_complete <- make_marpor_positions(
  df = df_manifesto,
  operationalisation = "marpor_complete",
  state_measure = "complete",
  libcons_measure = "complete",
  investment_codes = c("per506", "per702"),
  consumption_codes = c("per507", "per701")
)

positions_marpor_education_only <- make_marpor_positions(
  df = df_manifesto,
  operationalisation = "marpor_education_only",
  state_measure = "complete",
  libcons_measure = "complete",
  investment_codes = c("per506"),
  consumption_codes = c("per507")
)

positions_marpor_abou_chadi_wagner <- make_marpor_positions(
  df = df_manifesto,
  operationalisation = "marpor_abou_chadi_wagner",
  state_measure = "complete",
  libcons_measure = "complete",
  investment_codes = c("per402", "per406", "per411", "per506"),
  consumption_codes = c("per407", "per409", "per412", "per701")
)

positions_marpor_raw <- dplyr::bind_rows(
  positions_marpor_narrow,
  positions_marpor_complete,
  positions_marpor_education_only,
  positions_marpor_abou_chadi_wagner
) %>%
  dplyr::left_join(country_prefix_map, by = "countryname")

cat("\nMARPOR operationalisations prepared:\n")
positions_marpor_raw %>%
  dplyr::count(operationalisation, sort = TRUE) %>%
  print(n = Inf, width = Inf)

positions_marpor_matched <- positions_marpor_raw %>%
  dplyr::filter(!is.na(iso2c_file), !is.na(source_edate), !is.na(party)) %>%
  dplyr::left_join(flow_lookup, by = "iso2c_file", relationship = "many-to-many") %>%
  dplyr::mutate(
    match_lag_days = abs(as.numeric(flow_edate - source_edate)),
    match_lag_months = match_lag_days / 30.44
  ) %>%
  dplyr::group_by(operationalisation, elec_id, iso2c_file, party) %>%
  dplyr::slice_min(match_lag_days, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(match_lag_days <= 120) %>%
  dplyr::transmute(
    operationalisation,
    source,
    elec_id,
    iso2c_file,
    party,
    partyname,
    source_edate,
    flow_edate,
    marpor_party_family,
    marpor_vote_share,
    investmentconsumption,
    stateconomy,
    libcons,
    total,
    match_lag_days,
    match_lag_months
  ) %>%
  dplyr::left_join(
    party_lookup_marpor,
    by = c("elec_id", "iso2c_file", "party"),
    relationship = "many-to-one"
  )

cat("\nDuplicate MARPOR matched party keys:\n")
positions_marpor_matched %>%
  dplyr::count(operationalisation, elec_id, iso2c_file, party) %>%
  dplyr::filter(n > 1) %>%
  print(n = Inf)

# ------------------------------------------------
# 11. MARPOR matching diagnostics
# ------------------------------------------------

missing_sd_after <- party_lookup %>%
  dplyr::filter(social_democratic == TRUE, is.na(party)) %>%
  dplyr::distinct(iso2c_file, elec_id, year, party_name, peid) %>%
  dplyr::arrange(iso2c_file, year, party_name)

cat("\nRemaining social-democratic parties with missing MARPOR party codes:\n")
print(missing_sd_after, n = Inf, width = Inf)

sd_strategy_diagnostic <- party_lookup %>%
  dplyr::filter(social_democratic == TRUE) %>%
  dplyr::distinct(elec_id, iso2c_file, year, alt_id, party, party_name, bloc) %>%
  dplyr::left_join(
    positions_marpor_matched %>%
      dplyr::filter(operationalisation == primary_operationalisation) %>%
      dplyr::distinct(
        elec_id,
        iso2c_file,
        party,
        partyname,
        investmentconsumption,
        stateconomy,
        libcons,
        match_lag_days
      ),
    by = c("elec_id", "iso2c_file", "party")
  ) %>%
  dplyr::group_by(iso2c_file, elec_id, year) %>%
  dplyr::summarise(
    n_sd_parties_lookup = dplyr::n(),
    n_sd_parties_with_party_code = sum(!is.na(party)),
    n_sd_parties_matched_marpor = sum(!is.na(investmentconsumption) | !is.na(stateconomy) | !is.na(libcons)),
    all_sd_unmatched = all(is.na(investmentconsumption) & is.na(stateconomy) & is.na(libcons)),
    min_match_lag_days = suppressWarnings(min(match_lag_days, na.rm = TRUE)),
    sd_parties_lookup = paste(unique(party_name), collapse = "; "),
    sd_parties_marpor = paste(unique(stats::na.omit(partyname)), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    min_match_lag_days = dplyr::if_else(is.infinite(min_match_lag_days), NA_real_, min_match_lag_days)
  ) %>%
  dplyr::arrange(iso2c_file, year)

cat("\nElections where all social-democratic parties remain unmatched to MARPOR:\n")
print(sd_strategy_diagnostic %>% dplyr::filter(all_sd_unmatched), n = Inf, width = Inf)

cat("\nSummary of social-democratic MARPOR matching:\n")
sd_strategy_diagnostic %>%
  dplyr::summarise(
    n_elections = dplyr::n(),
    n_fully_unmatched = sum(all_sd_unmatched),
    share_fully_unmatched = mean(all_sd_unmatched),
    n_parties_lookup = sum(n_sd_parties_lookup),
    n_parties_with_party_code = sum(n_sd_parties_with_party_code),
    n_parties_matched_marpor = sum(n_sd_parties_matched_marpor)
  ) %>%
  print(width = Inf)

cat("\nCandidate MARPOR matches for missing social-democratic party codes:\n")

sd_missing_match_candidates <- missing_sd_after %>%
  dplyr::mutate(flow_edate = make_edate_from_elec_id(elec_id)) %>%
  dplyr::left_join(
    country_prefix_map,
    by = c("iso2c_file" = "iso2c_file"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    df_manifesto %>%
      dplyr::mutate(
        countryname = as.character(countryname),
        party = as.numeric(party),
        partyname = as.character(partyname),
        source_edate = as.Date(edate),
        marpor_parfam = as.numeric(parfam),
        marpor_pervote = as.numeric(pervote)
      ) %>%
      dplyr::select(
        countryname,
        party,
        partyname,
        source_edate,
        marpor_parfam,
        marpor_pervote
      ),
    by = "countryname",
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    match_lag_days = abs(as.numeric(flow_edate - source_edate))
  ) %>%
  dplyr::filter(
    match_lag_days <= 120,
    marpor_parfam %in% c(30, 20, 10)
  ) %>%
  dplyr::arrange(
    iso2c_file,
    elec_id,
    party_name,
    match_lag_days,
    dplyr::desc(marpor_pervote)
  ) %>%
  dplyr::select(
    iso2c_file,
    elec_id,
    year,
    party_name,
    peid,
    candidate_party_code = party,
    candidate_partyname = partyname,
    source_edate,
    match_lag_days,
    marpor_parfam,
    marpor_pervote
  )

print(sd_missing_match_candidates, n = Inf, width = Inf)

# ------------------------------------------------
# 12. Party-level supply variables
# ------------------------------------------------

party_supply <- positions_marpor_matched %>%
  dplyr::filter(!is.na(alt_id)) %>%
  dplyr::mutate(
    year = as.integer(stringr::str_extract(elec_id, "\\d{4}")),
    party = as.numeric(party)
  ) %>%
  dplyr::arrange(operationalisation, iso2c_file, party, year, elec_id) %>%
  dplyr::group_by(operationalisation, iso2c_file, party) %>%
  dplyr::mutate(
    investmentconsumption_lag = dplyr::lag(investmentconsumption),
    stateconomy_lag = dplyr::lag(stateconomy),
    libcons_lag = dplyr::lag(libcons),
    
    investmentconsumption_move = investmentconsumption - investmentconsumption_lag,
    stateconomy_move = stateconomy - stateconomy_lag,
    libcons_move = libcons - libcons_lag
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(operationalisation) %>%
  dplyr::mutate(
    investmentconsumption_std = scale_z(investmentconsumption),
    stateconomy_std = scale_z(stateconomy),
    libcons_std = scale_z(libcons),
    
    investmentconsumption_lag_std = scale_z(investmentconsumption_lag),
    stateconomy_lag_std = scale_z(stateconomy_lag),
    libcons_lag_std = scale_z(libcons_lag),
    
    investmentconsumption_move_std = scale_z(investmentconsumption_move),
    stateconomy_move_std = scale_z(stateconomy_move),
    libcons_move_std = scale_z(libcons_move)
  ) %>%
  dplyr::ungroup()

origin_party_supply <- party_supply %>%
  dplyr::filter(operationalisation == primary_operationalisation) %>%
  dplyr::rename(
    origin_alt_id = alt_id,
    origin_party = party,
    origin_party_marpor_name = partyname,
    origin_marpor_vote_share = marpor_vote_share,
    origin_investmentconsumption = investmentconsumption,
    origin_stateconomy = stateconomy,
    origin_libcons = libcons,
    origin_investmentconsumption_lag = investmentconsumption_lag,
    origin_stateconomy_lag = stateconomy_lag,
    origin_libcons_lag = libcons_lag,
    origin_investmentconsumption_move = investmentconsumption_move,
    origin_stateconomy_move = stateconomy_move,
    origin_libcons_move = libcons_move,
    origin_investmentconsumption_std = investmentconsumption_std,
    origin_stateconomy_std = stateconomy_std,
    origin_libcons_std = libcons_std,
    origin_investmentconsumption_move_std = investmentconsumption_move_std,
    origin_stateconomy_move_std = stateconomy_move_std,
    origin_libcons_move_std = libcons_move_std,
    origin_match_lag_days = match_lag_days
  )

destination_party_supply <- party_supply %>%
  dplyr::filter(operationalisation == primary_operationalisation) %>%
  dplyr::rename(
    destination_alt_id = alt_id,
    destination_party = party,
    destination_party_marpor_name = partyname,
    destination_marpor_vote_share = marpor_vote_share,
    destination_investmentconsumption = investmentconsumption,
    destination_stateconomy = stateconomy,
    destination_libcons = libcons,
    destination_investmentconsumption_lag = investmentconsumption_lag,
    destination_stateconomy_lag = stateconomy_lag,
    destination_libcons_lag = libcons_lag,
    destination_investmentconsumption_move = investmentconsumption_move,
    destination_stateconomy_move = stateconomy_move,
    destination_libcons_move = libcons_move,
    destination_investmentconsumption_std = investmentconsumption_std,
    destination_stateconomy_std = stateconomy_std,
    destination_libcons_std = libcons_std,
    destination_investmentconsumption_move_std = investmentconsumption_move_std,
    destination_stateconomy_move_std = stateconomy_move_std,
    destination_libcons_move_std = libcons_move_std,
    destination_match_lag_days = match_lag_days
  )

# ------------------------------------------------
# 13. Election-level social-democratic supply strategy
# ------------------------------------------------

sd_election_supply_all <- positions_marpor_matched %>%
  dplyr::filter(social_democratic == TRUE) %>%
  dplyr::mutate(
    year = as.integer(stringr::str_extract(elec_id, "\\d{4}"))
  ) %>%
  dplyr::group_by(operationalisation, elec_id, iso2c_file, year) %>%
  dplyr::summarise(
    n_sd_parties_marpor = dplyr::n(),
    sd_total_vote_share_marpor = sum(marpor_vote_share, na.rm = TRUE),
    sd_investmentconsumption = weighted_or_mean(investmentconsumption, marpor_vote_share),
    sd_stateconomy = weighted_or_mean(stateconomy, marpor_vote_share),
    sd_libcons = weighted_or_mean(libcons, marpor_vote_share),
    sd_mean_match_lag_days = mean(match_lag_days, na.rm = TRUE),
    sd_max_match_lag_days = max(match_lag_days, na.rm = TRUE),
    sd_parties_marpor = paste(unique(stats::na.omit(partyname)), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(operationalisation, iso2c_file, year, elec_id) %>%
  dplyr::group_by(operationalisation, iso2c_file) %>%
  dplyr::mutate(
    sd_investmentconsumption_lag = dplyr::lag(sd_investmentconsumption),
    sd_stateconomy_lag = dplyr::lag(sd_stateconomy),
    sd_libcons_lag = dplyr::lag(sd_libcons),
    
    sd_investmentconsumption_move = sd_investmentconsumption - sd_investmentconsumption_lag,
    sd_stateconomy_move = sd_stateconomy - sd_stateconomy_lag,
    sd_libcons_move = sd_libcons - sd_libcons_lag
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(operationalisation) %>%
  dplyr::mutate(
    sd_investmentconsumption_std = scale_z(sd_investmentconsumption),
    sd_stateconomy_std = scale_z(sd_stateconomy),
    sd_libcons_std = scale_z(sd_libcons),
    
    sd_investmentconsumption_lag_std = scale_z(sd_investmentconsumption_lag),
    sd_stateconomy_lag_std = scale_z(sd_stateconomy_lag),
    sd_libcons_lag_std = scale_z(sd_libcons_lag),
    
    sd_investmentconsumption_move_std = scale_z(sd_investmentconsumption_move),
    sd_stateconomy_move_std = scale_z(sd_stateconomy_move),
    sd_libcons_move_std = scale_z(sd_libcons_move)
  ) %>%
  dplyr::ungroup()

sd_election_supply <- sd_election_supply_all %>%
  dplyr::filter(operationalisation == primary_operationalisation)

cat("\nSocial-democratic election-level supply coverage by operationalisation:\n")
sd_election_supply_all %>%
  dplyr::group_by(operationalisation) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    investmentconsumption_nonmissing = sum(!is.na(sd_investmentconsumption_std)),
    stateconomy_nonmissing = sum(!is.na(sd_stateconomy_std)),
    libcons_nonmissing = sum(!is.na(sd_libcons_std)),
    investmentconsumption_move_nonmissing = sum(!is.na(sd_investmentconsumption_move_std)),
    stateconomy_move_nonmissing = sum(!is.na(sd_stateconomy_move_std)),
    libcons_move_nonmissing = sum(!is.na(sd_libcons_move_std)),
    .groups = "drop"
  ) %>%
  print(width = Inf)

cat("\nPrimary social-democratic election-level supply coverage:\n")
sd_election_supply %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    investmentconsumption_nonmissing = sum(!is.na(sd_investmentconsumption_std)),
    stateconomy_nonmissing = sum(!is.na(sd_stateconomy_std)),
    libcons_nonmissing = sum(!is.na(sd_libcons_std)),
    investmentconsumption_move_nonmissing = sum(!is.na(sd_investmentconsumption_move_std)),
    stateconomy_move_nonmissing = sum(!is.na(sd_stateconomy_move_std)),
    libcons_move_nonmissing = sum(!is.na(sd_libcons_move_std))
  ) %>%
  print(width = Inf)

# ------------------------------------------------
# 14. Election-level competitor supply context
# ------------------------------------------------

competitor_supply_context <- positions_marpor_matched %>%
  dplyr::filter(
    operationalisation == primary_operationalisation,
    party_bloc_detailed %in% c("far_right", "mainstream_right", "green", "far_left")
  ) %>%
  dplyr::mutate(
    competitor = party_bloc_detailed,
    year = as.integer(stringr::str_extract(elec_id, "\\d{4}"))
  ) %>%
  dplyr::group_by(elec_id, iso2c_file, year, competitor) %>%
  dplyr::summarise(
    n_parties_marpor = dplyr::n(),
    total_vote_share_marpor = sum(marpor_vote_share, na.rm = TRUE),
    investmentconsumption = weighted_or_mean(investmentconsumption, marpor_vote_share),
    stateconomy = weighted_or_mean(stateconomy, marpor_vote_share),
    libcons = weighted_or_mean(libcons, marpor_vote_share),
    mean_match_lag_days = mean(match_lag_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = competitor,
    values_from = c(
      n_parties_marpor,
      total_vote_share_marpor,
      investmentconsumption,
      stateconomy,
      libcons,
      mean_match_lag_days
    ),
    names_glue = "{competitor}_{.value}"
  )

# ------------------------------------------------
# 15. Election-level structural party-system variables
# ------------------------------------------------

party_system_context <- bloc_availability %>%
  dplyr::transmute(
    iso2c_file,
    elec_id,
    year,
    
    social_democratic_present,
    far_right_present,
    mainstream_right_present,
    other_left_present,
    non_present,
    green_present,
    far_left_present,
    
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

party_system_context_key_check <- party_system_context %>%
  dplyr::count(iso2c_file, elec_id, year) %>%
  dplyr::filter(n > 1)

if (nrow(party_system_context_key_check) > 0) {
  cat("\nDuplicate keys in structural party-system context:\n")
  print(party_system_context_key_check, n = Inf, width = Inf)
  stop("party_system_context is not unique by iso2c_file, elec_id, year.")
}

cat("\nStructural party-system context summary:\n")
party_system_context %>%
  dplyr::count(
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

# ------------------------------------------------
# 16. Merge model variables
# ------------------------------------------------

drop_model_vars <- c(
  "enp", "enp_z",
  
  "social_democratic_present",
  "far_right_present",
  "mainstream_right_present",
  "other_left_present",
  "non_present",
  "green_present",
  "far_left_present",
  
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
  "pooled_blocs_available",
  
  "n_social_democratic_parties",
  "n_far_right_parties",
  "n_mainstream_right_parties",
  "n_green_parties",
  "n_far_left_parties",
  
  setdiff(
    names(origin_party_supply),
    c("elec_id", "iso2c_file", "year", "origin_alt_id", "origin_party")
  ),
  setdiff(
    names(destination_party_supply),
    c("elec_id", "iso2c_file", "year", "destination_alt_id", "destination_party")
  ),
  setdiff(
    names(sd_election_supply),
    c("operationalisation", "elec_id", "iso2c_file", "year")
  ),
  setdiff(
    names(competitor_supply_context),
    c("elec_id", "iso2c_file", "year")
  )
)

drop_model_vars <- setdiff(drop_model_vars, "year")

add_model_vars <- function(df) {
  df %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::select(-dplyr::any_of(drop_model_vars)) %>%
    dplyr::left_join(
      party_system_context,
      by = c("iso2c_file", "elec_id", "year")
    ) %>%
    dplyr::left_join(
      df_enp %>% dplyr::select(iso2c_file, year, enp, enp_z),
      by = c("iso2c_file", "year")
    ) %>%
    dplyr::left_join(
      origin_party_supply,
      by = c("elec_id", "iso2c_file", "origin_alt_id", "origin_party")
    ) %>%
    dplyr::left_join(
      destination_party_supply,
      by = c("elec_id", "iso2c_file", "destination_alt_id", "destination_party")
    ) %>%
    dplyr::left_join(
      sd_election_supply %>%
        dplyr::select(-operationalisation),
      by = c("elec_id", "iso2c_file", "year")
    ) %>%
    dplyr::left_join(
      competitor_supply_context,
      by = c("elec_id", "iso2c_file", "year")
    ) %>%
    dplyr::mutate(
      origin_destination_investmentconsumption_distance =
        destination_investmentconsumption - origin_investmentconsumption,
      origin_destination_stateconomy_distance =
        destination_stateconomy - origin_stateconomy,
      origin_destination_libcons_distance =
        destination_libcons - origin_libcons,
      
      origin_destination_investmentconsumption_distance_abs =
        abs(origin_destination_investmentconsumption_distance),
      origin_destination_stateconomy_distance_abs =
        abs(origin_destination_stateconomy_distance),
      origin_destination_libcons_distance_abs =
        abs(origin_destination_libcons_distance),
      
      origin_destination_investmentconsumption_distance_std =
        scale_z(origin_destination_investmentconsumption_distance),
      origin_destination_stateconomy_distance_std =
        scale_z(origin_destination_stateconomy_distance),
      origin_destination_libcons_distance_std =
        scale_z(origin_destination_libcons_distance),
      
      origin_destination_investmentconsumption_distance_abs_std =
        scale_z(origin_destination_investmentconsumption_distance_abs),
      origin_destination_stateconomy_distance_abs_std =
        scale_z(origin_destination_stateconomy_distance_abs),
      origin_destination_libcons_distance_abs_std =
        scale_z(origin_destination_libcons_distance_abs)
    )
}

df_outward_social_democratic_analysis <- add_model_vars(df_outward_social_democratic)
df_inward_social_democratic_analysis <- add_model_vars(df_inward_social_democratic)
df_outward_far_right_analysis <- add_model_vars(df_outward_far_right)
df_inward_far_right_analysis <- add_model_vars(df_inward_far_right)

# ------------------------------------------------
# 17. Coverage checks
# ------------------------------------------------

check_analysis_coverage <- function(df, name) {
  cat("\nAnalysis-variable coverage:", name, "\n")
  
  df %>%
    dplyr::summarise(
      n = dplyr::n(),
      enp_nonmissing = sum(!is.na(enp_z)),
      
      sd_investmentconsumption_nonmissing = sum(!is.na(sd_investmentconsumption_std)),
      sd_stateconomy_nonmissing = sum(!is.na(sd_stateconomy_std)),
      sd_libcons_nonmissing = sum(!is.na(sd_libcons_std)),
      
      sd_investmentconsumption_move_nonmissing = sum(!is.na(sd_investmentconsumption_move_std)),
      sd_stateconomy_move_nonmissing = sum(!is.na(sd_stateconomy_move_std)),
      sd_libcons_move_nonmissing = sum(!is.na(sd_libcons_move_std)),
      
      origin_investmentconsumption_nonmissing = sum(!is.na(origin_investmentconsumption_std)),
      origin_stateconomy_nonmissing = sum(!is.na(origin_stateconomy_std)),
      origin_libcons_nonmissing = sum(!is.na(origin_libcons_std)),
      destination_investmentconsumption_nonmissing = sum(!is.na(destination_investmentconsumption_std)),
      destination_stateconomy_nonmissing = sum(!is.na(destination_stateconomy_std)),
      destination_libcons_nonmissing = sum(!is.na(destination_libcons_std)),
      
      dyadic_investmentconsumption_distance_nonmissing = sum(!is.na(origin_destination_investmentconsumption_distance_std)),
      dyadic_stateconomy_distance_nonmissing = sum(!is.na(origin_destination_stateconomy_distance_std)),
      dyadic_libcons_distance_nonmissing = sum(!is.na(origin_destination_libcons_distance_std)),
      
      basic_controls_nonmissing = sum(
        !is.na(gender) &
          !is.na(age_group)
      ),
      
      supplementary_controls_nonmissing = sum(
        !is.na(lrself_z) &
          !is.na(satdem_z)
      ),
      
      all_controls_nonmissing = sum(
        !is.na(gender) &
          !is.na(age_group) &
          !is.na(lrself_z) &
          !is.na(satdem_z)
      ),
      
      main_level_model_nonmissing = sum(
        !is.na(outcome) &
          !is.na(sd_investmentconsumption_std) &
          !is.na(sd_stateconomy_std) &
          !is.na(sd_libcons_std)
      ),
      
      main_change_model_nonmissing = sum(
        !is.na(outcome) &
          !is.na(sd_investmentconsumption_move_std) &
          !is.na(sd_stateconomy_move_std) &
          !is.na(sd_libcons_move_std)
      ),
      
      main_change_model_with_enp_nonmissing = sum(
        !is.na(outcome) &
          !is.na(sd_investmentconsumption_move_std) &
          !is.na(sd_stateconomy_move_std) &
          !is.na(sd_libcons_move_std) &
          !is.na(enp_z)
      ),
      
      main_change_model_with_basic_controls_nonmissing = sum(
        !is.na(outcome) &
          !is.na(sd_investmentconsumption_move_std) &
          !is.na(sd_stateconomy_move_std) &
          !is.na(sd_libcons_move_std) &
          !is.na(enp_z) &
          !is.na(gender) &
          !is.na(age_group)
      ),
      
      supplementary_change_model_nonmissing = sum(
        !is.na(outcome) &
          !is.na(sd_investmentconsumption_move_std) &
          !is.na(sd_stateconomy_move_std) &
          !is.na(sd_libcons_move_std) &
          !is.na(enp_z) &
          !is.na(gender) &
          !is.na(age_group) &
          !is.na(lrself_z) &
          !is.na(satdem_z)
      )
    ) %>%
    print(width = Inf)
  
  cat("\nOutcome support:", name, "\n")
  df %>%
    dplyr::count(outcome, sort = TRUE) %>%
    print(n = Inf)
}

check_analysis_coverage(df_outward_social_democratic_analysis, "outward social-democratic")
check_analysis_coverage(df_inward_social_democratic_analysis, "inward social-democratic")
check_analysis_coverage(df_outward_far_right_analysis, "outward far-right")
check_analysis_coverage(df_inward_far_right_analysis, "inward far-right")

cat("\nStructural party-system availability in SD analysis datasets:\n")

dplyr::bind_rows(
  df_outward_social_democratic_analysis %>%
    dplyr::mutate(model = "outward_sd"),
  df_inward_social_democratic_analysis %>%
    dplyr::mutate(model = "inward_sd")
) %>%
  dplyr::distinct(
    model,
    iso2c_file,
    elec_id,
    year,
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  dplyr::count(
    model,
    available_far_left,
    available_green,
    available_mainstream_right,
    available_far_right
  ) %>%
  print(n = Inf, width = Inf)

sd_analysis_all <- dplyr::bind_rows(
  df_outward_social_democratic_analysis %>%
    dplyr::mutate(flow_model = "outward_sd"),
  df_inward_social_democratic_analysis %>%
    dplyr::mutate(flow_model = "inward_sd")
)

coverage_sample_summary <- function(df, condition, name) {
  df %>%
    dplyr::filter({{ condition }}) %>%
    dplyr::summarise(
      sample = name,
      n_transitions = dplyr::n(),
      n_elections = dplyr::n_distinct(elec_id),
      n_countries = dplyr::n_distinct(iso2c_file),
      first_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE)
    )
}

sample_composition_supply_summary <- dplyr::bind_rows(
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(outcome),
    "Primary SD transition universe"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_std) &
      !is.na(sd_stateconomy_std) &
      !is.na(sd_libcons_std),
    "MARPOR level position specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std),
    "MARPOR change position specification"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std) &
      !is.na(enp_z) &
      !is.na(gender) &
      !is.na(age_group),
    "MARPOR change position specification with ENP and basic controls"
  ),
  coverage_sample_summary(
    sd_analysis_all,
    !is.na(sd_investmentconsumption_move_std) &
      !is.na(sd_stateconomy_move_std) &
      !is.na(sd_libcons_move_std) &
      !is.na(enp_z) &
      !is.na(gender) &
      !is.na(age_group) &
      !is.na(lrself_z) &
      !is.na(satdem_z),
    "Supplementary change position specification"
  )
)

cat("\nSupply-side sample-composition summary:\n")
print(sample_composition_supply_summary, width = Inf)

# ------------------------------------------------
# 18. Save
# ------------------------------------------------

saveRDS(
  df_enp,
  file.path(analysis_dir, "enp_election_social_democratic.rds")
)

saveRDS(
  positions_marpor_matched,
  file.path(analysis_dir, "positions_marpor_investment_state_libcons_matched.rds")
)

saveRDS(
  party_supply,
  file.path(analysis_dir, "party_supply_investment_state_libcons.rds")
)

saveRDS(
  origin_party_supply,
  file.path(analysis_dir, "origin_party_supply_investment_state_libcons.rds")
)

saveRDS(
  destination_party_supply,
  file.path(analysis_dir, "destination_party_supply_investment_state_libcons.rds")
)

saveRDS(
  sd_election_supply_all,
  file.path(analysis_dir, "sd_election_supply_investment_state_libcons_all_operationalisations.rds")
)

saveRDS(
  sd_election_supply,
  file.path(analysis_dir, "sd_election_supply_investment_state_libcons.rds")
)

saveRDS(
  competitor_supply_context,
  file.path(analysis_dir, "competitor_supply_context_investment_state_libcons.rds")
)

saveRDS(
  party_system_context,
  file.path(analysis_dir, "party_system_context_social_democratic.rds")
)

saveRDS(
  party_lookup,
  file.path(analysis_dir, "party_lookup_realised_transitions_social_democratic.rds")
)

saveRDS(
  party_lookup_marpor,
  file.path(analysis_dir, "party_lookup_marpor_social_democratic.rds")
)

saveRDS(
  sd_strategy_diagnostic,
  file.path(analysis_dir, "sd_strategy_marpor_matching_diagnostic.rds")
)

saveRDS(
  sd_missing_match_candidates,
  file.path(analysis_dir, "sd_strategy_marpor_missing_match_candidates.rds")
)

saveRDS(
  sample_composition_supply_summary,
  file.path(analysis_dir, "sample_composition_supply_summary_social_democratic.rds")
)

saveRDS(
  df_outward_social_democratic_analysis,
  file.path(analysis_dir, "df_outward_social_democratic_multinom_analysis.rds")
)

saveRDS(
  df_inward_social_democratic_analysis,
  file.path(analysis_dir, "df_inward_social_democratic_multinom_analysis.rds")
)

saveRDS(
  df_outward_far_right_analysis,
  file.path(analysis_dir, "df_outward_far_right_social_democratic_project_multinom_analysis.rds")
)

saveRDS(
  df_inward_far_right_analysis,
  file.path(analysis_dir, "df_inward_far_right_social_democratic_project_multinom_analysis.rds")
)

cat("\nSaved files:\n")
cat(file.path(analysis_dir, "enp_election_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "positions_marpor_investment_state_libcons_matched.rds"), "\n")
cat(file.path(analysis_dir, "party_supply_investment_state_libcons.rds"), "\n")
cat(file.path(analysis_dir, "origin_party_supply_investment_state_libcons.rds"), "\n")
cat(file.path(analysis_dir, "destination_party_supply_investment_state_libcons.rds"), "\n")
cat(file.path(analysis_dir, "sd_election_supply_investment_state_libcons_all_operationalisations.rds"), "\n")
cat(file.path(analysis_dir, "sd_election_supply_investment_state_libcons.rds"), "\n")
cat(file.path(analysis_dir, "competitor_supply_context_investment_state_libcons.rds"), "\n")
cat(file.path(analysis_dir, "party_system_context_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "party_lookup_realised_transitions_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "party_lookup_marpor_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "sd_strategy_marpor_matching_diagnostic.rds"), "\n")
cat(file.path(analysis_dir, "sd_strategy_marpor_missing_match_candidates.rds"), "\n")
cat(file.path(analysis_dir, "sample_composition_supply_summary_social_democratic.rds"), "\n")
cat(file.path(analysis_dir, "df_outward_social_democratic_multinom_analysis.rds"), "\n")
cat(file.path(analysis_dir, "df_inward_social_democratic_multinom_analysis.rds"), "\n")
cat(file.path(analysis_dir, "df_outward_far_right_social_democratic_project_multinom_analysis.rds"), "\n")
cat(file.path(analysis_dir, "df_inward_far_right_social_democratic_project_multinom_analysis.rds"), "\n")

cat("\nAnalysis-ready multinomial datasets saved successfully\n")
cat("\nPrimary operationalisation used for model-ready merge:", primary_operationalisation, "\n")
cat("\nAll operationalisations saved in: sd_election_supply_investment_state_libcons_all_operationalisations.rds\n")
cat("\nScript completed successfully\n")

# ------------------------------------------------
# 19. Visualise social-democratic positional shifts
# ------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

shift_plot_dir <- file.path(project_dir, "plots", "supply_position_shifts")
dir.create(shift_plot_dir, recursive = TRUE, showWarnings = FALSE)

shift_dimension_labels <- tibble::tribble(
  ~dimension,              ~dimension_label,                    ~std_var,
  "investmentconsumption", "Investment-consumption position",  "sd_investmentconsumption_move_std",
  "stateconomy",           "Economic left-right position",     "sd_stateconomy_move_std",
  "libcons",               "Liberal-conservative position",   "sd_libcons_move_std"
)

operationalisation_labels <- c(
  marpor_narrow = "Narrow education/labour measure",
  marpor_complete = "Primary education/labour measure",
  marpor_education_only = "Education-only measure",
  marpor_abou_chadi_wagner = "Abou-Chadi/Wagner measure"
)

shift_plot_data <- purrr::map_dfr(
  seq_len(nrow(shift_dimension_labels)),
  function(i) {
    dim_now <- shift_dimension_labels[i, ]
    
    sd_election_supply_all %>%
      dplyr::transmute(
        operationalisation,
        operationalisation_label = dplyr::recode(
          operationalisation,
          !!!operationalisation_labels,
          .default = operationalisation
        ),
        dimension = dim_now$dimension,
        dimension_label = dim_now$dimension_label,
        iso2c_file,
        elec_id,
        year,
        sd_parties_marpor,
        movement_std = .data[[dim_now$std_var]]
      )
  }
) %>%
  dplyr::filter(!is.na(movement_std)) %>%
  dplyr::mutate(
    party_label = paste0(sd_parties_marpor, " (", iso2c_file, " ", year, ")")
  )

shift_summary <- shift_plot_data %>%
  dplyr::group_by(
    operationalisation,
    operationalisation_label,
    dimension,
    dimension_label
  ) %>%
  dplyr::summarise(
    n_elections = dplyr::n(),
    mean_movement_std = mean(movement_std, na.rm = TRUE),
    median_movement_std = median(movement_std, na.rm = TRUE),
    sd_movement_std = sd(movement_std, na.rm = TRUE),
    min_movement_std = min(movement_std, na.rm = TRUE),
    max_movement_std = max(movement_std, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(
  shift_summary,
  file.path(analysis_dir, "sd_selected_supply_position_shift_summary.rds")
)

readr::write_csv(
  shift_summary,
  file.path(analysis_dir, "sd_selected_supply_position_shift_summary.csv")
)

plot_sd_position_movement <- function(
    dimension_name,
    operationalisation_name = NULL
) {
  
  if (is.null(operationalisation_name)) {
    operationalisation_name <- if (dimension_name == "investmentconsumption") {
      "marpor_abou_chadi_wagner"
    } else {
      primary_operationalisation
    }
  }
  
  df_plot <- shift_plot_data %>%
    dplyr::filter(
      dimension == dimension_name,
      operationalisation == operationalisation_name
    )
  
  mean_movement <- mean(df_plot$movement_std, na.rm = TRUE)
  y_max <- max(df_plot$year, na.rm = TRUE)
  
  p <- ggplot(df_plot, aes(x = movement_std, y = year)) +
    geom_vline(xintercept = 0, linewidth = 0.6) +
    geom_vline(
      xintercept = mean_movement,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_point(size = 1.8, alpha = 0.75) +
    ggrepel::geom_text_repel(
      aes(label = party_label),
      size = 2.4,
      alpha = 0.55,
      max.overlaps = 40,
      min.segment.length = 0,
      segment.alpha = 0.25
    ) +
    scale_y_reverse(
      limits = c(y_max, 1960),
      breaks = seq(1960, y_max, by = 10),
      minor_breaks = seq(1960, y_max, by = 5)
    ) +
    labs(
      title = paste0("Movements on the ", unique(df_plot$dimension_label)),
      x = "Movement from t-1 to t, in standard deviations",
      y = NULL,
      caption = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_line(linewidth = 0.2),
      panel.grid.major = element_line(linewidth = 0.35),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.border = element_rect(fill = NA, linewidth = 0.6)
    )
  
  file_stub <- paste0(
    "sd_position_movement_",
    operationalisation_name,
    "_",
    dimension_name
  )
  
  ggsave(
    filename = file.path(shift_plot_dir, paste0(file_stub, ".pdf")),
    plot = p,
    width = 9,
    height = 8
  )
  
  ggsave(
    filename = file.path(shift_plot_dir, paste0(file_stub, ".png")),
    plot = p,
    width = 9,
    height = 8,
    dpi = 300
  )
  
  p
}

plot_sd_position_movement("libcons")
plot_sd_position_movement("stateconomy")
plot_sd_position_movement("investmentconsumption", "marpor_abou_chadi_wagner")