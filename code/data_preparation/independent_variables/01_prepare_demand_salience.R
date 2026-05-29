# ================================================================
# 01_prepare_demand_salience.R
# Demand-side issue-priority measures from Eurobarometer
#
# Goal:
#   Construct country-wave and country-election measures of public
#   issue salience for:
#     1) Immigration
#     2) Unemployment
#     3) Environment / climate
#
# Main measures:
#   Country-election salience based on the nearest Eurobarometer wave
#   before each election:
#     immigration
#     unemployment
#     environment_climate
#
# Dynamic measures:
#   Election-to-election differences within country:
#     immigration_move_tminus1_to_t
#     unemployment_move_tminus1_to_t
#     environment_climate_move_tminus1_to_t
#
# Added robustness / diagnostic measures:
#   Country-election salience by left-right subgroup:
#     immigration_left, immigration_centre, immigration_right
#     unemployment_left, unemployment_centre, unemployment_right
#     environment_climate_left, environment_climate_centre,
#     environment_climate_right
#
# Output:
#   data/processed/eb_mip_raw.rds
#   data/processed/eb_lr_variable_diagnostics.rds
#   data/processed/eb_salience_country_wave.rds
#   data/processed/eb_salience_country_wave_lr_long.rds
#   data/processed/eb_salience_country_wave_lr_wide.rds
#   data/processed/eb_salience_election_model_input.rds
#   data/processed/eb_salience_election_link_diagnostics.rds
#   data/processed/eb_salience_eb_wave_usage_diagnostics.rds
#   data/processed/eb_salience_missing_prior_wave_links.rds
# ================================================================

#### Load packages and helpers ------------------------------------------------

source(here::here("code", "utils", "packages.R"))
load_packages()

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

#### Paths --------------------------------------------------------------------

eurobaro_path <- here::here("data", "eurobarometer")
output_path   <- here::here("data", "processed")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

path_eb_mip_raw <- here::here("data", "processed", "eb_mip_raw.rds")

#### List Eurobarometer files -------------------------------------------------

eb_files <- list.files(
  path = eurobaro_path,
  pattern = "\\.(dta|sav)$",
  recursive = TRUE,
  full.names = TRUE
)

#### File index ---------------------------------------------------------------

eb_index <- tibble::tibble(
  file = eb_files,
  folder = basename(dirname(eb_files)),
  filename = basename(eb_files),
  extension = stringr::str_to_lower(tools::file_ext(eb_files))
) %>%
  dplyr::mutate(
    za = stringr::str_extract(filename, "\\d{4}")
  ) %>%
  dplyr::filter(!is.na(za)) %>%
  dplyr::arrange(za, folder, filename)

#### Wave lookup --------------------------------------------------------------

eb_lookup <- tibble::tribble(
  ~za,    ~eb_wave, ~year, ~month_mid, ~country_q, ~personal_q, ~eu_q,
  "3640", "57.2",   2002,  5.0,        "Q2",       NA,          NA,
  "3904", "59.1",   2003,  3.5,        "Q5",       NA,          NA,
  "3938", "60.1",   2003, 10.5,        "Q26",      NA,          NA,
  "4056", "61.0",   2004,  2.5,        "Q27",      NA,          NA,
  "4229", "62.0",   2004, 10.5,        "Q33",      NA,          NA,
  "4411", "63.4",   2005,  5.5,        "QA26",     NA,          NA,
  "4414", "64.2",   2005, 10.5,        "QA30",     NA,          NA,
  "4506", "65.2",   2006,  4.0,        "QA28",     NA,          NA,
  "4507", "65.3",   2006,  5.5,        "QD1",      NA,          NA,
  "4526", "66.1",   2006,  9.5,        "QA23",     NA,          NA,
  "4528", "66.3",   2006, 11.5,        "QA26",     NA,          NA,
  "4530", "67.2",   2007,  4.5,        "QA18",     NA,          NA,
  "4565", "68.1",   2007, 10.0,        "QA6",      NA,          NA,
  "4744", "69.2",   2008,  4.0,        "QA6",      NA,          NA,
  "4819", "70.1",   2008, 10.5,        "QA8",      "QA9",       NA,
  "4971", "71.1",   2009,  1.5,        "QA5",      "QA6",       NA,
  "4973", "71.3",   2009,  6.5,        "QA4",      "QA5",       NA,
  "4994", "72.4",   2009, 10.5,        "QA5",      "QA6",       NA,
  "5234", "73.4",   2010,  5.0,        "QA7",      "QA8",       NA,
  "5449", "74.2",   2010, 11.5,        "QA6",      "QA7",       "QA8",
  "5481", "75.3",   2011,  5.0,        "QA7",      "QA8",       "QA9",
  "5567", "76.3",   2011, 11.0,        "QA6",      "QA7",       "QA8",
  "5612", "77.3",   2012,  5.0,        "QA7",      "QA8",       "QA9",
  "5685", "78.1",   2012, 11.0,        "QA5",      "QA6",       "QA7",
  "5689", "79.3",   2013,  5.0,        "QA6",      "QA7",       "QA8",
  "5876", "80.1",   2013, 11.0,        "QA4",      "QA5",       "QA6",
  "5913", "81.2",   2014,  3.0,        "QA4",      "QA5",       "QA6",
  "5928", "81.4",   2014,  5.5,        "QA4",      "QA5",       "QA6",
  "5932", "82.3",   2014, 11.0,        "QA3",      "QA4",       "QA5",
  "5964", "83.1",   2015,  2.5,        "QA3",      "QA4",       "QA5",
  "5998", "83.3",   2015,  5.0,        "QA3",      "QA4",       "QA5",
  "6643", "84.3",   2015, 11.0,        "QA3",      "QA4",       "QA5",
  "6694", "85.2",   2016,  5.0,        "QA3",      "QA4",       "QA5",
  "6788", "86.2",   2016, 11.0,        "QA3",      "QA4",       "QA5",
  "6863", "87.3",   2017,  5.0,        "QA3",      "QA4",       "QA5",
  "6928", "88.3",   2017, 11.0,        "QA3",      "QA4",       "QA5",
  "6963", "89.1",   2018,  3.0,        "QA3",      "QA4",       "QA5",
  "7489", "90.3",   2018, 11.0,        "QA3",      "QA4",       "QA5",
  "7562", "91.2",   2019,  3.0,        "QA1",      NA,          "QA2",
  "7576", "91.5",   2019,  6.5,        "QA3",      "QA4",       "QA5",
  "7601", "92.3",   2019, 11.5,        "QA3",      "QA4",       "QA5",
  "7649", "93.1",   2020,  7.5,        "QA3",      "QA4",       "QA5",
  "7780", "94.3",   2020, 10.5,        "QA3",      "QA4",       "QA5",
  "7783", "95.3",   2021,  6.5,        "QA3",      "QA4",       "QA5",
  "7848", "96.3",   2021, 10.5,        "QA3",      "QA4",       "QA5",
  "7902", "97.5",   2022,  6.5,        "QA3",      "QA4",       "QA5",
  "7953", "98.2",   2023,  1.5,        "QA3",      "QA4",       "QA5",
  "7997", "99.4",   2023,  5.5,        "QA3",      "QA4",       "QA5",
  "8779", "100.2",  2023, 10.5,        "QA3",      "QA4",       "QA5"
)

#### Match lookup -------------------------------------------------------------

eb_index <- eb_index %>%
  dplyr::left_join(eb_lookup, by = "za") %>%
  dplyr::filter(!is.na(eb_wave)) %>%
  dplyr::mutate(
    wave_date = as.Date(sprintf(
      "%d-%02d-15",
      year,
      pmin(pmax(round(month_mid), 1), 12)
    )),
    wave_time = year + (month_mid - 1) / 12
  )

#### Helpers ------------------------------------------------------------------

read_eb_file <- function(file, extension) {
  switch(
    extension,
    dta = haven::read_dta(file),
    sav = haven::read_sav(file),
    stop("Unsupported file extension: ", extension)
  )
}

find_country_var <- function(df) {
  candidates <- c("isocntry", "country", "tnscntry", "nation", "v7", "v6")
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) NA_character_ else hits[1]
}

find_id_var <- function(df) {
  candidates <- c("v5", "caseid", "id", "serial", "uniqid", "unique_caseid")
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) NA_character_ else hits[1]
}

find_lr_var <- function(df) {
  nm <- names(df)
  
  labels <- unname(labelled::var_label(df))
  labels_chr <- vapply(
    labels,
    function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) {
        NA_character_
      } else {
        as.character(x)[1]
      }
    },
    character(1)
  )
  labels_lc <- stringr::str_to_lower(labels_chr)
  
  candidate_by_label <- nm[
    !is.na(labels_lc) &
      (
        stringr::str_detect(labels_lc, "left.*right") |
          stringr::str_detect(labels_lc, "right.*left") |
          stringr::str_detect(labels_lc, "left-right")
      ) &
      !stringr::str_detect(labels_lc, "marital|married|civil status")
  ]
  
  if (length(candidate_by_label) == 0) {
    return(NA_character_)
  }
  
  score_candidate <- function(v) {
    x <- suppressWarnings(as.numeric(haven::zap_labels(df[[v]])))
    vals <- sort(unique(na.omit(x)))
    
    valid_1_10 <- length(vals[vals >= 1 & vals <= 10]) >= 5 &&
      all(vals[!(vals >= 1 & vals <= 10)] %in% c(97, 98, 99, 999))
    
    as.numeric(valid_1_10) * 100 + sum(!is.na(x))
  }
  
  scores <- vapply(candidate_by_label, score_candidate, numeric(1))
  candidate_by_label[which.max(scores)]
}

diagnose_lr_var_wave <- function(file, extension, za, eb_wave, year,
                                 month_mid, wave_date, wave_time) {
  df <- read_eb_file(file, extension)
  lr_var <- find_lr_var(df)
  
  if (is.na(lr_var)) {
    return(tibble::tibble(
      file = file,
      filename = basename(file),
      za = za,
      eb_wave = eb_wave,
      year = year,
      month_mid = month_mid,
      wave_date = wave_date,
      wave_time = wave_time,
      lr_var = NA_character_,
      lr_label = NA_character_,
      lr_min = NA_real_,
      lr_max = NA_real_,
      lr_n_nonmissing = NA_integer_,
      lr_values = NA_character_
    ))
  }
  
  lr_raw <- df[[lr_var]]
  lr_num <- suppressWarnings(as.numeric(haven::zap_labels(lr_raw)))
  lr_num_valid <- lr_num
  lr_num_valid[!(lr_num_valid >= 1 & lr_num_valid <= 10)] <- NA_real_
  
  lr_lab <- labelled::var_label(df[[lr_var]])
  lr_lab <- if (is.null(lr_lab) || length(lr_lab) == 0 || all(is.na(lr_lab))) {
    NA_character_
  } else {
    as.character(lr_lab)[1]
  }
  
  vals <- sort(unique(na.omit(lr_num)))
  
  tibble::tibble(
    file = file,
    filename = basename(file),
    za = za,
    eb_wave = eb_wave,
    year = year,
    month_mid = month_mid,
    wave_date = wave_date,
    wave_time = wave_time,
    lr_var = lr_var,
    lr_label = lr_lab,
    lr_min = suppressWarnings(min(lr_num, na.rm = TRUE)),
    lr_max = suppressWarnings(max(lr_num, na.rm = TRUE)),
    lr_n_nonmissing = sum(!is.na(lr_num_valid)),
    lr_values = paste(vals[1:min(20, length(vals))], collapse = ", ")
  )
}

find_block_vars <- function(df, q_base, labels = NULL) {
  if (is.na(q_base) || is.null(q_base) || q_base == "") {
    return(character(0))
  }
  
  nm <- names(df)
  nm_lc <- tolower(nm)
  q_base_lc <- tolower(q_base)
  q_base_escaped <- stringr::str_replace_all(q_base_lc, "([.])", "\\\\\\1")
  
  name_patterns <- c(
    paste0("^", q_base_escaped, "([_.]\\d+)$"),
    paste0("^", q_base_escaped, "([a-z][_.]\\d+)$"),
    paste0("^", q_base_escaped, "([a-z]\\d+)$")
  )
  
  name_hits <- nm[
    Reduce(
      `|`,
      lapply(name_patterns, function(pat) grepl(pat, nm_lc, perl = TRUE))
    )
  ]
  
  if (length(name_hits) > 0) {
    return(name_hits)
  }
  
  if (is.null(labels)) {
    labels <- unname(labelled::var_label(df))
  }
  
  labels_chr <- vapply(
    labels,
    function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) {
        NA_character_
      } else {
        as.character(x)[1]
      }
    },
    character(1)
  )
  labels_lc <- tolower(labels_chr)
  q_label_pattern <- paste0("^", q_base_lc, "([a-z0-9_]*|\\b)")
  
  label_hits <- nm[
    !is.na(labels_lc) &
      grepl(q_label_pattern, labels_lc, perl = TRUE) &
      grepl("import|important", labels_lc) &
      grepl("issues", labels_lc) &
      !grepl("eu role|eu priorities|personal|pers|\\btcc\\b|cy-tcc", labels_lc) &
      (
        grepl("immig", labels_lc) |
          grepl("unemploy", labels_lc) |
          grepl("environment|climate", labels_lc)
      )
  ]
  
  if (length(label_hits) > 0) {
    return(label_hits)
  }
  
  label_hits_fallback <- nm[
    !is.na(labels_lc) &
      grepl("import|important", labels_lc) &
      grepl("issues", labels_lc) &
      grepl("ctry|country|national|nat", labels_lc) &
      !grepl("eu role|eu priorities|personal|pers|\\btcc\\b|cy-tcc", labels_lc) &
      (
        grepl("immig", labels_lc) |
          grepl("unemploy", labels_lc) |
          grepl("environment|climate", labels_lc)
      )
  ]
  
  label_hits_fallback
}

harmonize_issue_vec <- function(issue_label_raw) {
  x <- tolower(ifelse(is.na(issue_label_raw), "", issue_label_raw))
  
  out <- rep(NA_character_, length(x))
  out[x == ""] <- NA_character_
  out[grepl("immig", x)] <- "immigration"
  out[grepl("unemploy", x)] <- "unemployment"
  out[grepl("environment|climate", x)] <- "environment_climate"
  
  out
}

recode_selected_vec <- function(x) {
  x_num <- suppressWarnings(as.numeric(haven::zap_labels(x)))
  out <- rep(NA_real_, length(x_num))
  out[x_num == 1] <- 1
  out[x_num == 0] <- 0
  out
}

collapse_selected <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (any(x == 1)) {
    return(1)
  }
  if (all(x == 0)) {
    return(0)
  }
  
  NA_real_
}

extract_item_number_vec <- function(var_name, issue_label_raw) {
  var_lc <- tolower(var_name)
  out <- stringr::str_extract(var_lc, "(?<=_|\\.)\\d+$")
  out_num <- suppressWarnings(as.numeric(out))
  label_lc <- tolower(ifelse(is.na(issue_label_raw), "", issue_label_raw))
  
  out_num[is.na(out_num) & grepl("immig", label_lc)] <- 9
  out_num[is.na(out_num) & grepl("unemploy", label_lc)] <- 5
  out_num[is.na(out_num) & grepl("environment|climate", label_lc)] <- 13
  
  out_num
}

extract_mip_block <- function(df, q_base) {
  country_var <- find_country_var(df)
  id_var <- find_id_var(df)
  lr_var <- find_lr_var(df)
  
  if (is.na(country_var) || is.na(id_var)) {
    return(NULL)
  }
  
  var_labels <- unname(labelled::var_label(df))
  names(var_labels) <- names(df)
  
  vars <- find_block_vars(df, q_base, labels = var_labels)
  
  if (length(vars) == 0) {
    return(NULL)
  }
  
  issue_label_raw_lookup <- var_labels[vars]
  issue_label_raw_lookup <- vapply(
    issue_label_raw_lookup,
    function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) {
        NA_character_
      } else {
        as.character(x)[1]
      }
    },
    character(1)
  )
  
  select_vars <- unique(c(country_var, id_var, lr_var, vars))
  select_vars <- select_vars[!is.na(select_vars)]
  
  out <- df %>%
    dplyr::select(dplyr::all_of(select_vars)) %>%
    dplyr::rename(
      isocntry_raw = dplyr::all_of(country_var),
      resp_id_raw = dplyr::all_of(id_var)
    )
  
  if (!is.na(lr_var) && lr_var %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        lr_num = suppressWarnings(as.numeric(haven::zap_labels(.data[[lr_var]]))),
        lr_num = dplyr::if_else(lr_num >= 1 & lr_num <= 10, lr_num, NA_real_),
        lr_group = dplyr::case_when(
          !is.na(lr_num) & lr_num >= 1 & lr_num <= 4 ~ "left",
          !is.na(lr_num) & lr_num >= 5 & lr_num <= 6 ~ "centre",
          !is.na(lr_num) & lr_num >= 7 & lr_num <= 10 ~ "right",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select(-dplyr::all_of(lr_var))
  } else {
    out <- out %>%
      dplyr::mutate(
        lr_num = NA_real_,
        lr_group = NA_character_
      )
  }
  
  out <- out %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "issue_var",
      values_to = "selected_raw"
    )
  
  out$issue_label_raw <- unname(issue_label_raw_lookup[out$issue_var])
  out$item_num <- extract_item_number_vec(out$issue_var, out$issue_label_raw)
  out$selected_num <- suppressWarnings(as.numeric(haven::zap_labels(out$selected_raw)))
  out$selected_bin <- recode_selected_vec(out$selected_raw)
  out$issue_harmonized <- harmonize_issue_vec(out$issue_label_raw)
  out$isocntry <- as.character(out$isocntry_raw)
  out$resp_id <- as.character(out$resp_id_raw)
  
  out %>%
    dplyr::select(
      resp_id,
      isocntry,
      lr_num,
      lr_group,
      issue_var,
      item_num,
      issue_label_raw,
      issue_harmonized,
      selected_raw,
      selected_num,
      selected_bin
    )
}

guess_country_q <- function(file, extension) {
  df <- read_eb_file(file, extension)
  
  nm <- names(df)
  nm_lc <- stringr::str_to_lower(nm)
  
  lab <- unname(labelled::var_label(df))
  lab <- purrr::map_chr(
    lab,
    ~ if (is.null(.x) || length(.x) == 0 || all(is.na(.x))) {
      NA_character_
    } else {
      as.character(.x)[1]
    }
  )
  lab_lc <- stringr::str_to_lower(lab)
  
  candidates <- tibble::tibble(
    var = nm,
    var_lc = nm_lc,
    lab = lab,
    lab_lc = lab_lc
  ) %>%
    dplyr::filter(
      !is.na(lab_lc),
      stringr::str_detect(lab_lc, "important issues"),
      !stringr::str_detect(lab_lc, "pers|personal|eu|european union|tcc"),
      (
        stringr::str_detect(lab_lc, "immigration") |
          stringr::str_detect(lab_lc, "unemployment") |
          stringr::str_detect(lab_lc, "environment|climate")
      )
    ) %>%
    dplyr::mutate(
      q_base = stringr::str_extract(stringr::str_to_upper(lab), "Q[A-Z]*\\d+")
    ) %>%
    dplyr::filter(!is.na(q_base))
  
  if (nrow(candidates) > 0) {
    return(candidates$q_base[1])
  }
  
  candidates_name <- tibble::tibble(var = nm, var_lc = nm_lc) %>%
    dplyr::mutate(
      q_base = stringr::str_extract(stringr::str_to_upper(var), "^Q[A-Z]*\\d+")
    ) %>%
    dplyr::filter(!is.na(q_base))
  
  if (nrow(candidates_name) > 0) {
    return(candidates_name$q_base[1])
  }
  
  NA_character_
}

extract_mip_wave <- function(file, extension, za, eb_wave, year,
                             month_mid, wave_date, wave_time, country_q) {
  if (is.na(country_q) || is.null(country_q) || country_q == "") {
    country_q <- guess_country_q(file, extension)
  }
  
  if (is.na(country_q) || is.null(country_q) || country_q == "") {
    return(NULL)
  }
  
  df <- read_eb_file(file, extension)
  out <- extract_mip_block(df, country_q)
  
  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }
  
  out$file <- file
  out$filename <- basename(file)
  out$za <- za
  out$eb_wave <- eb_wave
  out$year <- year
  out$month_mid <- month_mid
  out$wave_date <- wave_date
  out$wave_time <- wave_time
  
  out %>%
    dplyr::select(
      file,
      filename,
      za,
      eb_wave,
      year,
      month_mid,
      wave_date,
      wave_time,
      resp_id,
      isocntry,
      lr_num,
      lr_group,
      issue_var,
      item_num,
      issue_label_raw,
      issue_harmonized,
      selected_raw,
      selected_num,
      selected_bin
    )
}

#### Fill missing country_q ---------------------------------------------------

if (!"country_q" %in% names(eb_index)) {
  eb_index <- eb_index %>%
    dplyr::mutate(country_q = NA_character_)
}

eb_index <- eb_index %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    country_q = dplyr::coalesce(country_q, guess_country_q(file, extension))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    wave_date = as.Date(sprintf(
      "%d-%02d-15",
      year,
      pmin(pmax(round(month_mid), 1), 12)
    )),
    wave_time = year + (month_mid - 1) / 12
  )

#### Left-right diagnostics ---------------------------------------------------

lr_diagnostics <- purrr::pmap_dfr(
  list(
    file = eb_index$file,
    extension = eb_index$extension,
    za = eb_index$za,
    eb_wave = eb_index$eb_wave,
    year = eb_index$year,
    month_mid = eb_index$month_mid,
    wave_date = eb_index$wave_date,
    wave_time = eb_index$wave_time
  ),
  diagnose_lr_var_wave
)

saveRDS(
  lr_diagnostics,
  here::here("data", "processed", "eb_lr_variable_diagnostics.rds")
)

cat("\nLeft-right variable diagnostics:\n")
print(
  lr_diagnostics %>%
    dplyr::select(
      za,
      eb_wave,
      year,
      filename,
      lr_var,
      lr_label,
      lr_min,
      lr_max,
      lr_n_nonmissing,
      lr_values
    ),
  n = Inf,
  width = Inf
)

cat("\nWaves without identified left-right variable:\n")
print(
  lr_diagnostics %>%
    dplyr::filter(is.na(lr_var)) %>%
    dplyr::select(za, eb_wave, year, filename),
  n = Inf
)

#### Extract or load raw MIP data ---------------------------------------------

if (file.exists(path_eb_mip_raw)) {
  
  cat("\nLoading existing eb_mip_raw.rds\n")
  eb_mip_raw <- readRDS(path_eb_mip_raw)
  
} else {
  
  cat("\nExtracting Eurobarometer MIP data\n")
  
  future::plan(
    future::multisession,
    workers = max(1, parallelly::availableCores() - 1)
  )
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  with_progress({
    p <- progressor(steps = nrow(eb_index))
    
    eb_mip_raw <- furrr::future_pmap_dfr(
      list(
        file       = eb_index$file,
        extension  = eb_index$extension,
        za         = eb_index$za,
        eb_wave    = eb_index$eb_wave,
        year       = eb_index$year,
        month_mid  = eb_index$month_mid,
        wave_date  = eb_index$wave_date,
        wave_time  = eb_index$wave_time,
        country_q  = eb_index$country_q
      ),
      function(file, extension, za, eb_wave, year,
               month_mid, wave_date, wave_time, country_q) {
        p()
        extract_mip_wave(
          file = file,
          extension = extension,
          za = za,
          eb_wave = eb_wave,
          year = year,
          month_mid = month_mid,
          wave_date = wave_date,
          wave_time = wave_time,
          country_q = country_q
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  })
  
  future::plan(future::sequential)
  
  saveRDS(
    eb_mip_raw,
    path_eb_mip_raw
  )
}

#### Check extracted waves ----------------------------------------------------

expected_waves <- eb_index %>%
  dplyr::distinct(za, eb_wave, year, filename, country_q) %>%
  dplyr::arrange(year, za)

extracted_waves <- eb_mip_raw %>%
  dplyr::distinct(za, eb_wave, year, filename) %>%
  dplyr::arrange(year, za)

missing_extracted_waves <- expected_waves %>%
  dplyr::anti_join(
    extracted_waves,
    by = c("za", "eb_wave", "year", "filename")
  )

unexpected_extracted_waves <- extracted_waves %>%
  dplyr::anti_join(
    expected_waves,
    by = c("za", "eb_wave", "year", "filename")
  )

cat("\nExpected waves:\n")
print(expected_waves, n = Inf)

cat("\nExtracted waves:\n")
print(extracted_waves, n = Inf)

cat("\nWaves present in lookup but missing from extracted data:\n")
print(missing_extracted_waves, n = Inf)

cat("\nWaves extracted but not expected from lookup:\n")
print(unexpected_extracted_waves, n = Inf)

cat("\nNumber of expected waves:\n")
print(nrow(expected_waves))

cat("\nNumber of extracted waves:\n")
print(nrow(extracted_waves))

cat("\nNumber of missing waves:\n")
print(nrow(missing_extracted_waves))

cat("\nExtracted years:\n")
print(sort(unique(extracted_waves$year)))

cat("\nYears in lookup but absent from extracted data:\n")
print(setdiff(sort(unique(expected_waves$year)), sort(unique(extracted_waves$year))))

#### Collapse to respondent level --------------------------------------------

eb_mip_resp <- eb_mip_raw %>%
  dplyr::filter(!is.na(issue_harmonized)) %>%
  dplyr::group_by(
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    isocntry,
    resp_id,
    lr_num,
    lr_group,
    issue_harmonized
  ) %>%
  dplyr::summarise(
    selected_bin = collapse_selected(selected_bin),
    .groups = "drop"
  )

#### Aggregate to country-wave ------------------------------------------------

eb_salience <- eb_mip_resp %>%
  dplyr::group_by(
    isocntry,
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(selected_bin, na.rm = TRUE),
    n = sum(!is.na(selected_bin)),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n >= 500,
    issue_harmonized %in% c(
      "immigration",
      "unemployment",
      "environment_climate"
    )
  ) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      isocntry %in% c("GB", "GB-GBN", "GB-NIR") ~ "GB",
      isocntry %in% c("DE-W", "DE-E") ~ "DE",
      isocntry == "CY-TCC" ~ "CY",
      isocntry == "RS-KM" ~ "RS",
      TRUE ~ isocntry
    )
  )

#### Aggregate to country-wave by left-right group ----------------------------

eb_salience_lr <- eb_mip_resp %>%
  dplyr::filter(!is.na(lr_group)) %>%
  dplyr::group_by(
    isocntry,
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    lr_group,
    issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(selected_bin, na.rm = TRUE),
    n = sum(!is.na(selected_bin)),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n >= 100,
    issue_harmonized %in% c(
      "immigration",
      "unemployment",
      "environment_climate"
    )
  ) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      isocntry %in% c("GB", "GB-GBN", "GB-NIR") ~ "GB",
      isocntry %in% c("DE-W", "DE-E") ~ "DE",
      isocntry == "CY-TCC" ~ "CY",
      isocntry == "RS-KM" ~ "RS",
      TRUE ~ isocntry
    )
  )

#### Wide country-wave series -------------------------------------------------

eb_salience_wide <- eb_salience %>%
  dplyr::group_by(
    country,
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(salience, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = issue_harmonized,
    values_from = salience
  ) %>%
  dplyr::arrange(country, wave_date)

#### Wide country-wave series by left-right group -----------------------------

eb_salience_lr_wide <- eb_salience_lr %>%
  dplyr::group_by(
    country,
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    lr_group,
    issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(salience, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    issue_group = paste(issue_harmonized, lr_group, sep = "_")
  ) %>%
  dplyr::select(
    country,
    za,
    eb_wave,
    year,
    month_mid,
    wave_date,
    issue_group,
    salience
  ) %>%
  tidyr::pivot_wider(
    names_from = issue_group,
    values_from = salience
  ) %>%
  dplyr::arrange(country, wave_date)

eb_salience_wide <- eb_salience_wide %>%
  dplyr::left_join(
    eb_salience_lr_wide %>%
      dplyr::select(
        country,
        za,
        eb_wave,
        year,
        month_mid,
        wave_date,
        dplyr::ends_with("_left"),
        dplyr::ends_with("_centre"),
        dplyr::ends_with("_right")
      ),
    by = c("country", "za", "eb_wave", "year", "month_mid", "wave_date")
  ) %>%
  dplyr::mutate(
    wave_time = lubridate::year(wave_date) +
      (lubridate::month(wave_date) - 1) / 12
  ) %>%
  dplyr::arrange(country, wave_date)

#### Load election panel ------------------------------------------------------

election_path <- here::here("data", "processed", "election_contexts.rds")

elections_raw <- readRDS(election_path)

elections <- elections_raw %>%
  dplyr::transmute(
    elec_id = elec_id,
    country = as.character(country),
    election_date = as.Date(election_date)
  ) %>%
  dplyr::filter(!is.na(country), !is.na(election_date)) %>%
  dplyr::distinct(elec_id, country, election_date) %>%
  dplyr::arrange(country, election_date)

#### Link each election to nearest prior Eurobarometer wave --------------------

salience_vars <- c(
  "immigration",
  "unemployment",
  "environment_climate",
  "immigration_left",
  "immigration_centre",
  "immigration_right",
  "unemployment_left",
  "unemployment_centre",
  "unemployment_right",
  "environment_climate_left",
  "environment_climate_centre",
  "environment_climate_right"
)

eb_salience_election <- elections %>%
  dplyr::left_join(
    eb_salience_wide,
    by = "country",
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(
    wave_date < election_date
  ) %>%
  dplyr::group_by(elec_id, country, election_date) %>%
  dplyr::slice_max(
    order_by = wave_date,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    elec_id,
    country,
    election_date,
    eb_za = za,
    eb_wave = eb_wave,
    eb_wave_year = year,
    eb_wave_month_mid = month_mid,
    eb_wave_date = wave_date,
    days_between_eb_and_election =
      as.numeric(election_date - wave_date),
    dplyr::across(dplyr::all_of(salience_vars))
  ) %>%
  dplyr::arrange(country, election_date)

#### Identify elections without previous EB wave ------------------------------

missing_eb_prior_wave <- elections %>%
  dplyr::anti_join(
    eb_salience_election,
    by = c("elec_id", "country", "election_date")
  ) %>%
  dplyr::arrange(country, election_date)

cat("\nElections without any prior Eurobarometer wave:\n")
print(missing_eb_prior_wave, n = Inf)

#### Construct election-to-election differences -------------------------------

eb_salience_election_model_input <- eb_salience_election %>%
  dplyr::arrange(country, election_date) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    previous_elec_id = dplyr::lag(elec_id),
    previous_election_date = dplyr::lag(election_date),
    previous_eb_wave = dplyr::lag(eb_wave),
    previous_eb_wave_date = dplyr::lag(eb_wave_date),
    years_since_previous_election =
      as.numeric(election_date - previous_election_date) / 365.25,
    dplyr::across(
      dplyr::all_of(salience_vars),
      ~ dplyr::lag(.x),
      .names = "{.col}_tminus1"
    ),
    dplyr::across(
      dplyr::all_of(salience_vars),
      ~ .x - dplyr::lag(.x),
      .names = "{.col}_move_tminus1_to_t"
    ),
    dplyr::across(
      dplyr::ends_with("_move_tminus1_to_t"),
      ~ .x / years_since_previous_election,
      .names = "{.col}_annualised"
    )
  ) %>%
  dplyr::ungroup()

#### Standardise change measures ----------------------------------------------

change_vars <- names(eb_salience_election_model_input) %>%
  stringr::str_subset("_move_tminus1_to_t$")

annualised_change_vars <- names(eb_salience_election_model_input) %>%
  stringr::str_subset("_move_tminus1_to_t_annualised$")

eb_salience_election_model_input <- eb_salience_election_model_input %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(change_vars),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    ),
    dplyr::across(
      dplyr::all_of(annualised_change_vars),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )

#### Relative priorities at linked EB wave ------------------------------------

eb_salience_election_model_input <- eb_salience_election_model_input %>%
  dplyr::mutate(
    immigration_minus_unemployment = immigration - unemployment,
    immigration_minus_environment = immigration - environment_climate,
    environment_minus_unemployment = environment_climate - unemployment,
    
    immigration_minus_unemployment_left =
      immigration_left - unemployment_left,
    immigration_minus_unemployment_centre =
      immigration_centre - unemployment_centre,
    immigration_minus_unemployment_right =
      immigration_right - unemployment_right,
    
    immigration_minus_environment_left =
      immigration_left - environment_climate_left,
    immigration_minus_environment_centre =
      immigration_centre - environment_climate_centre,
    immigration_minus_environment_right =
      immigration_right - environment_climate_right,
    
    environment_minus_unemployment_left =
      environment_climate_left - unemployment_left,
    environment_minus_unemployment_centre =
      environment_climate_centre - unemployment_centre,
    environment_minus_unemployment_right =
      environment_climate_right - unemployment_right
  )

relative_vars <- c(
  "immigration_minus_unemployment",
  "immigration_minus_environment",
  "environment_minus_unemployment",
  "immigration_minus_unemployment_left",
  "immigration_minus_unemployment_centre",
  "immigration_minus_unemployment_right",
  "immigration_minus_environment_left",
  "immigration_minus_environment_centre",
  "immigration_minus_environment_right",
  "environment_minus_unemployment_left",
  "environment_minus_unemployment_centre",
  "environment_minus_unemployment_right"
)

eb_salience_election_model_input <- eb_salience_election_model_input %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(relative_vars),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )

#### Diagnostics: which EB waves are used? ------------------------------------

eb_wave_usage_diagnostics <- eb_salience_election_model_input %>%
  dplyr::count(
    eb_za,
    eb_wave,
    eb_wave_date,
    name = "n_elections_using_wave"
  ) %>%
  dplyr::right_join(
    eb_salience_wide %>%
      dplyr::distinct(
        eb_za = za,
        eb_wave,
        eb_wave_date = wave_date
      ),
    by = c("eb_za", "eb_wave", "eb_wave_date")
  ) %>%
  dplyr::mutate(
    n_elections_using_wave = tidyr::replace_na(n_elections_using_wave, 0L),
    used_at_least_once = n_elections_using_wave > 0
  ) %>%
  dplyr::arrange(eb_wave_date)

cat("\nEurobarometer wave usage diagnostics:\n")
print(eb_wave_usage_diagnostics, n = Inf)

cat("\nEurobarometer waves never used:\n")
print(
  eb_wave_usage_diagnostics %>%
    dplyr::filter(!used_at_least_once),
  n = Inf
)

cat("\nEurobarometer waves used most often:\n")
print(
  eb_wave_usage_diagnostics %>%
    dplyr::arrange(dplyr::desc(n_elections_using_wave), eb_wave_date),
  n = 25
)

#### Diagnostics: election links ----------------------------------------------

salience_election_link_diagnostics <- elections %>%
  dplyr::left_join(
    eb_salience_election_model_input %>%
      dplyr::select(
        elec_id,
        country,
        election_date,
        eb_za,
        eb_wave,
        eb_wave_date,
        days_between_eb_and_election,
        previous_elec_id,
        previous_election_date,
        previous_eb_wave,
        previous_eb_wave_date,
        years_since_previous_election,
        immigration,
        immigration_tminus1,
        immigration_move_tminus1_to_t,
        unemployment,
        unemployment_tminus1,
        unemployment_move_tminus1_to_t,
        environment_climate,
        environment_climate_tminus1,
        environment_climate_move_tminus1_to_t
      ),
    by = c("elec_id", "country", "election_date")
  ) %>%
  dplyr::arrange(country, election_date)

cat("\nElection-to-EB link diagnostics:\n")
print(
  salience_election_link_diagnostics %>%
    dplyr::select(
      country,
      elec_id,
      election_date,
      eb_wave,
      eb_wave_date,
      days_between_eb_and_election,
      previous_election_date,
      previous_eb_wave,
      previous_eb_wave_date,
      immigration,
      immigration_tminus1,
      immigration_move_tminus1_to_t
    ),
  n = 150,
  width = Inf
)

cat("\nFirst usable election per country should have missing change variables:\n")
print(
  salience_election_link_diagnostics %>%
    dplyr::group_by(country) %>%
    dplyr::slice_min(election_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      country,
      elec_id,
      election_date,
      eb_wave,
      eb_wave_date,
      immigration,
      immigration_tminus1,
      immigration_move_tminus1_to_t,
      unemployment_move_tminus1_to_t,
      environment_climate_move_tminus1_to_t
    ),
  n = Inf,
  width = Inf
)

cat("\nExample cases for manual inspection:\n")
print(
  salience_election_link_diagnostics %>%
    dplyr::filter(country %in% c("AT", "DE", "NL", "SE", "GB")) %>%
    dplyr::select(
      country,
      elec_id,
      election_date,
      eb_wave,
      eb_wave_date,
      days_between_eb_and_election,
      previous_election_date,
      previous_eb_wave,
      previous_eb_wave_date,
      immigration,
      immigration_tminus1,
      immigration_move_tminus1_to_t,
      unemployment,
      unemployment_tminus1,
      unemployment_move_tminus1_to_t,
      environment_climate,
      environment_climate_tminus1,
      environment_climate_move_tminus1_to_t
    ) %>%
    dplyr::arrange(country, election_date),
  n = Inf,
  width = Inf
)

#### Add EB prefix for downstream model scripts --------------------------------

eb_salience_election_model_input <- eb_salience_election_model_input %>%
  dplyr::rename_with(
    ~ paste0("eb_", .x),
    -c(elec_id, country, election_date)
  )

#### Save ---------------------------------------------------------------------

saveRDS(
  eb_salience_wide,
  here::here("data", "processed", "eb_salience_country_wave.rds")
)

saveRDS(
  eb_salience_lr,
  here::here("data", "processed", "eb_salience_country_wave_lr_long.rds")
)

saveRDS(
  eb_salience_lr_wide,
  here::here("data", "processed", "eb_salience_country_wave_lr_wide.rds")
)

saveRDS(
  eb_salience_election_model_input,
  here::here("data", "processed", "eb_salience_election_model_input.rds")
)

saveRDS(
  salience_election_link_diagnostics,
  here::here("data", "processed", "eb_salience_election_link_diagnostics.rds")
)

saveRDS(
  eb_wave_usage_diagnostics,
  here::here("data", "processed", "eb_salience_eb_wave_usage_diagnostics.rds")
)

saveRDS(
  missing_eb_prior_wave,
  here::here("data", "processed", "eb_salience_missing_prior_wave_links.rds")
)