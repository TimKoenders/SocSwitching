#### 01_data_preparation.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()
source(here::here("code", "utils", "helper_functions.R"))
?voteswitchR

#### Load data ---------------------------------------------------------------
## Raw 
raw <- voteswitchR::switches

## Raw imputed (list of 5)
raw_imp <- voteswitchR::switches_imp

## Raked
raked <- voteswitchR::raked_switches

## Raked imputed (list of 5)
raked_imp <- voteswitchR::raked_switches_imp

# Create local copy of mapping table
mappings <- voteswitchR::mappings

# Helper to attach party labels
attach_party_labels <- function(dat, mappings){
  
  dat %>%
    dplyr::select(-dplyr::any_of(c("name_from","name_to"))) %>%
    dplyr::left_join(
      mappings %>% dplyr::select(elec_id, stack, party_name),
      by = c("elec_id","switch_from" = "stack")
    ) %>%
    dplyr::rename(name_from = party_name) %>%
    dplyr::left_join(
      mappings %>% dplyr::select(elec_id, stack, party_name),
      by = c("elec_id","switch_to" = "stack")
    ) %>%
    dplyr::rename(name_to = party_name) %>%
    dplyr::mutate(
      name_from = dplyr::case_when(
        switch_from == 98 ~ "others",
        switch_from == 99 ~ "non-voters",
        TRUE ~ name_from
      ),
      name_to = dplyr::case_when(
        switch_to == 98 ~ "others",
        switch_to == 99 ~ "non-voters",
        TRUE ~ name_to
      )
    )
}

# Define locations
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
context_dir <- file.path(project_dir, "code", "adding_contexts")

#### Quality check ---------------------------------------------------------------
## Raw
me_raw <- voteswitchR::calculate_meas_error(raw, type = "mae")
err_raw <- mean(me_raw$elec_errors$mean_error_t, na.rm = TRUE)

## Raked
me_raked <- voteswitchR::calculate_meas_error(raked, type = "mae")
err_raked <- mean(me_raked$elec_errors$mean_error_t, na.rm = TRUE)

## Raw imputed
me_raw_imp <- lapply(raw_imp, function(x)
  voteswitchR::calculate_meas_error(x, type = "mae")
)

err_raw_imp <- mean(
  sapply(me_raw_imp, function(x)
    mean(x$elec_errors$mean_error_t, na.rm = TRUE))
)

## Raked imputed
me_raked_imp <- lapply(raked_imp, function(x)
  voteswitchR::calculate_meas_error(x, type = "mae")
)

err_raked_imp <- mean(
  sapply(me_raked_imp, function(x)
    mean(x$elec_errors$mean_error_t, na.rm = TRUE))
)

## Compare data quality
tibble::tibble(
  dataset = c("raw", "raw_imp", "raked", "raked_imp"),
  mean_mae = c(err_raw, err_raw_imp, err_raked, err_raked_imp)
)

## Select best raked imputation
mae_imp <- sapply(me_raked_imp, function(x)
  mean(x$elec_errors$mean_error_t, na.rm = TRUE)
)

best_imp <- which.min(mae_imp)
best_raked_imp <- raked_imp[[best_imp]]

## Drop unused objects
rm(
  raw, raw_imp, raked, raked_imp,
  me_raw, me_raked, me_raw_imp, me_raked_imp,
  mae_imp, best_imp
)

#### Check electoral contexts ------------------------------------------------
best_raked_imp %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id) %>%
  print(n = Inf)

#### Link labels to parties ----------------------------------------------
# Inspect structure
dplyr::glimpse(best_raked_imp)

# Attach party labels
best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

#### Subset to relevant electoral contexts ------------------------------------
# Scope conditions for inclusion in the analysis
#
# Criterion 1: Presence of a social democratic party
# The party system must contain a party belonging to the social democratic
# or democratic socialist family (e.g. Labour, Socialist, Social Democratic,
# or comparable parties).
#
# Criterion 2: Multiparty competition
# The electoral system must contain more than two relevant parties. The
# analysis examines voter flows between social democratic parties and
# multiple competing party families. Pure two-party systems therefore
# cannot generate the type of polyadic competition required for the
# research question.
#
# Country excluded:
# US United States
# - No independent social democratic party
# - Party competition effectively limited to two parties
#

excluded_countries <- c("US")

best_raked_imp <- best_raked_imp %>%
  dplyr::filter(!(substr(elec_id, 1, 2) %in% excluded_countries))

# Verify remaining electoral contexts
best_raked_imp %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id) %>%
  print(n = Inf)


#### Adding electoral contexts: Austria 2019 -----------------------------------------------------------------------
at_2019_file <- file.path(context_dir, "AT_2019.R")
at_2019_txt  <- readLines(at_2019_file)

at_2019_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  at_2019_txt
)

writeLines(at_2019_txt, at_2019_file)

source(at_2019_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_AT2019_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_AT2019_ext, at_2019_file, at_2019_txt)


#### Adding electoral contexts: Austria 2024 ---------------------------------------------------------------------

at_2024_file <- file.path(context_dir, "AT_2024.R")
at_2024_txt  <- readLines(at_2024_file)

at_2024_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  at_2024_txt
)

writeLines(at_2024_txt, at_2024_file)

source(at_2024_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_AT2024_ext
)


#### Adding electoral contexts: Denmark 2022 ---------------------------------------------------------------------

dnk_2022_file <- file.path(context_dir, "DK_2022.R")
dnk_2022_txt  <- readLines(dnk_2022_file)

dnk_2022_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  dnk_2022_txt
)

writeLines(dnk_2022_txt, dnk_2022_file)

source(dnk_2022_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_DNK2022_ext
)

# Fill party families only when uniquely determined within parlgov_id_1
family_summary <- mappings %>%
  dplyr::filter(!is.na(parlgov_id_1)) %>%
  dplyr::group_by(parlgov_id_1) %>%
  dplyr::summarise(
    n_parfam = dplyr::n_distinct(parfam[!is.na(parfam)]),
    n_parfam_harmonized = dplyr::n_distinct(parfam_harmonized[!is.na(parfam_harmonized)]),
    parfam_fill = {
      x <- parfam[!is.na(parfam)]
      if (length(x) == 0) NA_character_ else dplyr::first(x)
    },
    parfam_harmonized_fill = {
      x <- parfam_harmonized[!is.na(parfam_harmonized)]
      if (length(x) == 0) NA_character_ else dplyr::first(x)
    },
    .groups = "drop"
  )

mappings <- mappings %>%
  dplyr::left_join(
    family_summary,
    by = "parlgov_id_1"
  ) %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::case_when(
      is.na(parfam_harmonized) & n_parfam_harmonized == 1 ~ parfam_harmonized_fill,
      TRUE ~ parfam_harmonized
    ),
    parfam = dplyr::case_when(
      is.na(parfam) & n_parfam == 1 & n_parfam_harmonized == 1 ~ parfam_fill,
      TRUE ~ parfam
    )
  ) %>%
  dplyr::select(
    -n_parfam,
    -n_parfam_harmonized,
    -parfam_fill,
    -parfam_harmonized_fill
  )

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(
  added_rows,
  mapping_DNK2022_ext,
  dnk_2022_file,
  dnk_2022_txt,
  family_summary
)

#### Adding electoral contexts: France 2022 ---------------------------------------------------------------------

fra_2022_file <- file.path(context_dir, "FR_2022.R")
fra_2022_txt  <- readLines(fra_2022_file)

fra_2022_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  fra_2022_txt
)

writeLines(fra_2022_txt, fra_2022_file)

source(fra_2022_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_FRA2022_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_FRA2022_ext, fra_2022_file, fra_2022_txt)

#### Adding electoral contexts: New Zealand 2023 ---------------------------------------------------------------------

nzl_2023_file <- file.path(context_dir, "NZ_2023.R")
nzl_2023_txt  <- readLines(nzl_2023_file)

nzl_2023_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  nzl_2023_txt
)

writeLines(nzl_2023_txt, nzl_2023_file)

source(nzl_2023_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_NZL2023_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_NZL2023_ext, nzl_2023_file, nzl_2023_txt)
#### Adding electoral contexts: Poland 2023 ---------------------------------------------------------------------

pol_2023_file <- file.path(context_dir, "PL_2023.R")
pol_2023_txt  <- readLines(pol_2023_file)

pol_2023_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  pol_2023_txt
)

writeLines(pol_2023_txt, pol_2023_file)

source(pol_2023_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_POL2023_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_POL2023_ext, pol_2023_file, pol_2023_txt)
#### Adding electoral contexts: Portugal 2022 -------------------------------------------------------------------

prt_2022_file <- file.path(context_dir, "PT_2022.R")
prt_2022_txt  <- readLines(prt_2022_file)

prt_2022_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  prt_2022_txt
)

writeLines(prt_2022_txt, prt_2022_file)

source(prt_2022_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_PRT2022_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_PRT2022_ext, prt_2022_file, prt_2022_txt)
#### Adding electoral contexts: Portugal 2024 -------------------------------------------------------------------

prt_2024_file <- file.path(context_dir, "PT_2024.R")
prt_2024_txt  <- readLines(prt_2024_file)

prt_2024_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  prt_2024_txt
)

writeLines(prt_2024_txt, prt_2024_file)

source(prt_2024_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_PRT2024_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_PRT2024_ext, prt_2024_file, prt_2024_txt)
#### Adding electoral contexts: Slovakia 2023 -------------------------------------------------------------------

svk_2023_file <- file.path(context_dir, "SK_2023.R")
svk_2023_txt  <- readLines(svk_2023_file)

svk_2023_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  svk_2023_txt
)

writeLines(svk_2023_txt, svk_2023_file)

source(svk_2023_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_SVK2023_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_SVK2023_ext, svk_2023_file, svk_2023_txt)
#### Adding electoral contexts: Slovenia 2022 -------------------------------------------------------------------

svn_2022_file <- file.path(context_dir, "SI_2022.R")
svn_2022_txt  <- readLines(svn_2022_file)

svn_2022_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  svn_2022_txt
)

writeLines(svn_2022_txt, svn_2022_file)

source(svn_2022_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_SVN2022_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_SVN2022_ext, svn_2022_file, svn_2022_txt)
#### Adding electoral contexts: Sweden 2022 -------------------------------------------------------------------

se_2022_file <- file.path(context_dir, "SE_2022.R")
se_2022_txt  <- readLines(se_2022_file)

se_2022_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  se_2022_txt
)

writeLines(se_2022_txt, se_2022_file)

source(se_2022_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_SWE2022_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_SWE2022_ext, se_2022_file, se_2022_txt)
#### Adding electoral contexts: Switzerland 2023 -------------------------------------------------------------------

ch_2023_file <- file.path(context_dir, "CH_2023.R")
ch_2023_txt  <- readLines(ch_2023_file)

ch_2023_txt <- sub(
  '^folder_location <- ".*"$',
  'folder_location <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")',
  ch_2023_txt
)

writeLines(ch_2023_txt, ch_2023_file)

source(ch_2023_file, local = FALSE)

best_raked_imp <- dplyr::bind_rows(
  best_raked_imp,
  added_rows
)

mappings <- dplyr::bind_rows(
  mappings,
  mapping_CHE2023_ext
)

best_raked_imp <- attach_party_labels(best_raked_imp, mappings)

rm(added_rows, mapping_CHE2023_ext, ch_2023_file, ch_2023_txt)
#### Clean party family indicators and party ----------------------------------

# 1. Inspect conflicting family assignments within ParlGov IDs
family_conflicts <- mappings %>%
  dplyr::filter(!is.na(parlgov_id_1)) %>%
  dplyr::group_by(parlgov_id_1) %>%
  dplyr::summarise(
    n_parfam = dplyr::n_distinct(parfam[!is.na(parfam)]),
    n_parfam_harmonized = dplyr::n_distinct(parfam_harmonized[!is.na(parfam_harmonized)]),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_parfam > 1 | n_parfam_harmonized > 1)

tibble::as_tibble(family_conflicts) %>%
  print(n = Inf)

# Optional inspection of conflicting IDs
conflict_ids <- family_conflicts$parlgov_id_1

mappings %>%
  dplyr::filter(parlgov_id_1 %in% conflict_ids) %>%
  dplyr::select(parlgov_id_1, elec_id, party_name, parfam, parfam_harmonized) %>%
  dplyr::distinct() %>%
  dplyr::arrange(parlgov_id_1, elec_id, party_name) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

# 2. Cautious automatic fill from shared ParlGov IDs
family_summary <- mappings %>%
  dplyr::filter(!is.na(parlgov_id_1)) %>%
  dplyr::group_by(parlgov_id_1) %>%
  dplyr::summarise(
    n_parfam = dplyr::n_distinct(parfam[!is.na(parfam)]),
    n_parfam_harmonized = dplyr::n_distinct(parfam_harmonized[!is.na(parfam_harmonized)]),
    parfam_fill = {
      x <- parfam[!is.na(parfam)]
      if (length(x) == 0) NA_character_ else dplyr::first(x)
    },
    parfam_harmonized_fill = {
      x <- parfam_harmonized[!is.na(parfam_harmonized)]
      if (length(x) == 0) NA_character_ else dplyr::first(x)
    },
    .groups = "drop"
  )

mappings <- mappings %>%
  dplyr::left_join(
    family_summary,
    by = "parlgov_id_1"
  ) %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::case_when(
      is.na(parfam_harmonized) & n_parfam_harmonized == 1 ~ parfam_harmonized_fill,
      TRUE ~ parfam_harmonized
    ),
    parfam = dplyr::case_when(
      is.na(parfam) & n_parfam == 1 & n_parfam_harmonized == 1 ~ parfam_fill,
      TRUE ~ parfam
    )
  ) %>%
  dplyr::select(
    -n_parfam,
    -n_parfam_harmonized,
    -parfam_fill,
    -parfam_harmonized_fill
  )

rm(family_summary)

# 3. Enforce valid 3-letter lowercase family codes only
valid_families <- c("eco", "soc", "nat", "mrp", "sip", "agr", "eth", "lef")

mappings <- mappings %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::if_else(
      !is.na(parfam_harmonized) & !(parfam_harmonized %in% valid_families),
      NA_character_,
      parfam_harmonized
    )
  )

# 4. Manual fill for remaining unresolved cases
family_manual <- tibble::tribble(
  ~party_name, ~parfam_harmonized,
  "Communist Party of Austria",                               "lef",
  "Christian Social Party",                                   "mrp",
  "Geneva Citizens' Movement",                                "nat",
  "Pirates and Mayors",                                       "sip",
  "Denmark Democrats - Inger Stojberg",                       "nat",
  "Hard Line",                                                "nat",
  "Independent Greens",                                       "eco",
  "Klaus Riskaer Pedersen List",                              "mrp",
  "Moderates",                                                "mrp",
  "Résistons!",                                               "nat",
  "Solidarity and Progress",                                  "lef",
  "National Unity (HaMahane HaMamlakhti)",                    "mrp",
  "Political Party \"List of Lithuania\"",                    "nat",
  "Latvian Russian Union",                                    "eth",
  "Advance New Zealand Party",                                "nat",
  "Animal Justice Party Aotearoa New Zealand",                "eco",
  "Aotearoa Legalise Cannabis Party",                         "sip",
  "DemocracyNZ",                                              "nat",
  "Leighton Baker Party",                                     "nat",
  "New Conservatives",                                        "nat",
  "New Zealand Loyal Party",                                  "nat",
  "NewZeal",                                                  "nat",
  "The Opportunities Party",                                  "mrp",
  "Civic Coalition",                                          "mrp",
  "Confederation Liberty and Independence",                   "nat",
  "Nonpartisan Local Government Activists",                   "agr",
  "Polish Coalition",                                         "agr",
  "The Left",                                                 "lef",
  "There is One Poland",                                      "nat",
  "Third Way",                                                "agr",
  "United Right",                                             "nat",
  "Unitarian Democratic Coalition",                           "lef",
  "Democratic Alliance",                                      "mrp",
  "Democratic Party of Pensioners of Slovenia",               "soc",
  "Freedom Movement",                                         "mrp",
  "Good State",                                               "mrp",
  "Healthy Society Movement",                                 "mrp",
  "Let's Connect Slovenia",                                   "mrp",
  "List of Boris Popovic - Let's Digitize Slovenia",          "nat",
  "List of Marjan Sarec",                                     "mrp",
  "Modern Centre Party",                                      "mrp",
  "Our Country",                                              "nat",
  "Party of Alenka Bratusek",                                 "mrp",
  "Pirate Party",                                             "sip",
  "Resni.ca",                                                 "nat",
  "VESNA - Green Party",                                      "eco",
  "Communist Party of Slovakia",                              "lef",
  "Democrats",                                                "mrp",
  "Good Choice",                                              "mrp",
  "Homeland",                                                 "nat",
  "Hungarian Community Togetherness",                         "eth",
  "Hungarian Forum",                                          "eth",
  "Justice",                                                  "mrp",
  "Movement REPUBLIKA",                                       "nat",
  "MySlovensko",                                              "nat",
  "Slovak Democratic and Christian Union - Democratic Party", "mrp",
  "The Blue(s), Bridge",                                      "mrp",
  "Voice - Social Democracy",                                 "soc"
)

mappings <- mappings %>%
  dplyr::left_join(
    family_manual,
    by = "party_name",
    suffix = c("", "_manual")
  ) %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::coalesce(parfam_harmonized, parfam_harmonized_manual)
  ) %>%
  dplyr::select(-parfam_harmonized_manual)

# Final check
mappings %>%
  dplyr::filter(is.na(parfam_harmonized)) %>%
  dplyr::select(parlgov_id_1, elec_id, party_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(elec_id, party_name) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

rm(family_conflicts, conflict_ids, family_manual)

mappings <- mappings %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::if_else(
      !is.na(parfam),
      tolower(substr(parfam, 1, 3)),
      parfam_harmonized
    )
  )

mappings %>%
  dplyr::filter(is.na(parfam_harmonized)) %>%
  dplyr::select(parlgov_id_1, elec_id, party_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(elec_id, party_name) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

family_manual_last <- tibble::tribble(
  ~parlgov_id_1, ~elec_id,      ~party_name,                                             ~parfam_harmonized,
  748,           "BE-VL-1991-11","ROSSEM",                                               "nat",
  2469,          "NZ-1996-10",  "United Party",                                          "mrp",
  2469,          "NZ-1999-11",  "United Party",                                          "mrp",
  2756,          "PT-2019-10",  "Alliance",                                              "mrp",
  1485,          "RO-2004-11",  "Christian Democratic National Peasants' Party (PNTCD)", "agr",
  1485,          "RO-2014-11",  "Christian-Liberal Alliance",                            "mrp"
)

mappings <- mappings %>%
  dplyr::left_join(
    family_manual_last,
    by = c("parlgov_id_1", "elec_id", "party_name"),
    suffix = c("", "_manual")
  ) %>%
  dplyr::mutate(
    parfam_harmonized = dplyr::coalesce(parfam_harmonized, parfam_harmonized_manual)
  ) %>%
  dplyr::select(-parfam_harmonized_manual)

mappings %>%
  dplyr::filter(is.na(parfam_harmonized)) %>%
  dplyr::select(map_vote, parlgov_id_1, elec_id, party_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(elec_id, map_vote) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

mappings <- mappings %>%
  dplyr::mutate(
    party_name = dplyr::case_when(
      elec_id == "FR-2022-04" & map_vote == 250004 ~ "Reconquest",
      elec_id == "FR-2022-04" & map_vote == 250006 ~ "Europe Ecology - The Greens",
      elec_id == "FR-2022-04" & map_vote == 250007 ~ "Resist!",
      elec_id == "FR-2022-04" & map_vote == 250011 ~ "New Anticapitalist Party",
      elec_id == "FR-2022-04" & map_vote == 250012 ~ "Workers' Struggle",
      TRUE ~ party_name
    ),
    parfam = dplyr::case_when(
      elec_id == "FR-2022-04" & map_vote == 250004 ~ "nationalist",
      elec_id == "FR-2022-04" & map_vote == 250006 ~ "green",
      elec_id == "FR-2022-04" & map_vote == 250007 ~ "conservative",
      elec_id == "FR-2022-04" & map_vote == 250011 ~ "radical_left",
      elec_id == "FR-2022-04" & map_vote == 250012 ~ "radical_left",
      TRUE ~ parfam
    ),
    parfam_harmonized = dplyr::case_when(
      elec_id == "FR-2022-04" & map_vote == 250004 ~ "nat",
      elec_id == "FR-2022-04" & map_vote == 250006 ~ "eco",
      elec_id == "FR-2022-04" & map_vote == 250007 ~ "mrp",
      elec_id == "FR-2022-04" & map_vote == 250011 ~ "lef",
      elec_id == "FR-2022-04" & map_vote == 250012 ~ "lef",
      TRUE ~ parfam_harmonized
    )
  )

mappings <- mappings %>%
  dplyr::mutate(
    party_name = dplyr::case_when(
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0092) ~ "Popular Republican Union",
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0018) ~ "Solidarity and Progress",
      TRUE ~ party_name
    ),
    parfam = dplyr::case_when(
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0092) ~ "nationalist",
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0018) ~ "radical_left",
      TRUE ~ parfam
    ),
    parfam_harmonized = dplyr::case_when(
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0092) ~ "nat",
      elec_id == "FR-2022-04" & dplyr::near(vote_share_lag, 0.0018) ~ "lef",
      TRUE ~ parfam_harmonized
    )
  )


#### Attach party and family information to transitions ------------------------
# Make the mapping
best_raked_imp_fam <- best_raked_imp %>%
  dplyr::left_join(
    mappings %>%
      dplyr::select(elec_id, stack, parfam_harmonized_from = parfam_harmonized),
    by = c("elec_id", "switch_from" = "stack")
  ) %>%
  dplyr::left_join(
    mappings %>%
      dplyr::select(elec_id, stack, parfam_harmonized_to = parfam_harmonized),
    by = c("elec_id", "switch_to" = "stack")
  )

# Impute for other and non voter categories
best_raked_imp_fam <- best_raked_imp_fam %>%
  dplyr::mutate(
    parfam_harmonized_from = dplyr::case_when(
      switch_from == 98 ~ "oth",
      switch_from == 99 ~ "non",
      TRUE ~ parfam_harmonized_from
    ),
    parfam_harmonized_to = dplyr::case_when(
      switch_to == 98 ~ "oth",
      switch_to == 99 ~ "non",
      TRUE ~ parfam_harmonized_to
    )
  )

# Set empty values in weights to 0 
best_raked_imp_fam <- best_raked_imp_fam %>%
  dplyr::mutate(
    weights = dplyr::coalesce(weights, 0)
  )

#### Save prepared voter transition data -------------------------------------

dir.create(
  here::here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  best_raked_imp_fam,
  here::here("data", "processed", "best_raked_imp_fam.rds")
)
