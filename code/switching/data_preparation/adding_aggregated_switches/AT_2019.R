# ================================================================
# AT_2019.R
# ================================================================

# ------------------------------------------------
# 0. Packages
# ------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tibble)
  library(anesrake)
})

# ------------------------------------------------
# 1. CONFIGURATION (ELECTION CONTEXT)
# ------------------------------------------------
# Paths
folder_location <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files"
survey_path     <- file.path(folder_location, "at20172024", "10874_da_en_v1_0.dta")

# Election identifiers
CTX_ISO2C         <- "AT"
CTX_ELEC_ID       <- "AT-2019-09"
CTX_ELECTION_DATE <- as.Date("2019-09-29")
CTX_YEAR          <- 2019L


# ------------------------------------------------
# 2. SURVEY VARIABLE MAPPING
# ------------------------------------------------
# Variables used to construct vote switching
VAR_TURNOUT_T      <- "w12_q11"
VAR_VOTE_T         <- "w12_q12"

VAR_TURNOUT_TM1    <- "w5_q9"
VAR_VOTE_TM1       <- "w5_q10"

# Optional design weight (NULL if unavailable)
VAR_DESIGN_WEIGHT  <- NULL


# ------------------------------------------------
# 3. CONTEXT-SPECIFIC ELECTORAL INFORMATION
# ------------------------------------------------

# Official turnout
CTX_TURNOUT     <- 0.756   # 2019 election
CTX_TURNOUT_LAG <- 0.800   # 2017 election


# ------------------------------------------------
# 3.1 Party mapping: survey vote codes → party labels
# ------------------------------------------------

# Current election (2019)
party_map_t <- tibble(
  map_vote_t = c(1L,2L,3L,4L,5L,6L),
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  )
)

# Previous election (2017)
party_map_tm1 <- tibble(
  map_vote_tm1 = c(2L,1L,3L,5L,6L,4L),
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  )
)


# ------------------------------------------------
# 3.2 ParlGov identifiers
# ------------------------------------------------
party_parlgov <- tibble(
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  ),
  parlgov_id_1 = c(1013,973,50,2255,2651,1429)
)


# ------------------------------------------------
# 3.3 Official vote shares (for raking targets)
# ------------------------------------------------
party_vote_shares <- tibble(
  party_name = c(
    "Austrian People's Party",
    "Social Democratic Party of Austria",
    "Freedom Party of Austria",
    "NEOS",
    "JETZT – Pilz List",
    "The Greens"
  ),
  vote_share = c(
    0.375,
    0.212,
    0.162,
    0.080,
    0.019,
    0.139
  ),
  vote_share_lag = c(
    0.315,
    0.269,
    0.260,
    0.053,
    0.044,
    0.039
  )
)


# ------------------------------------------------
# 4. CLEANING FUNCTIONS
# ------------------------------------------------

clean_turnout <- function(x) {
  
  x <- as.numeric(x)
  
  dplyr::case_when(
    x == 4 ~ 1,
    x %in% c(1,2,3) ~ 0,
    TRUE ~ NA_real_
  )
}


clean_vote <- function(x) {
  
  x <- as.numeric(x)
  
  x[x %in% c(12,88,99)] <- NA_real_
  
  x
}


# ------------------------------------------------
# 5. Load survey data and construct standardized micro data
# ------------------------------------------------

survey_raw <- haven::read_dta(survey_path)
names(survey_raw) <- toupper(names(survey_raw))

# Check number of obs in the relevant wave
survey_raw %>%
  summarise(
    n_wave5  = sum(W5_PANELIST  == 1, na.rm = TRUE),
    n_wave12 = sum(W12_PANELIST == 1, na.rm = TRUE),
    n_both   = sum(W5_PANELIST == 1 & W12_PANELIST == 1, na.rm = TRUE)
  )

# Keep only respondents present in both relevant waves
survey_raw <- survey_raw %>%
  dplyr::filter(
    W5_PANELIST  == 1,
    W12_PANELIST == 1
  )

# Harmonize
stopifnot(toupper(VAR_TURNOUT_T)   %in% names(survey_raw))
stopifnot(toupper(VAR_VOTE_T)      %in% names(survey_raw))
stopifnot(toupper(VAR_TURNOUT_TM1) %in% names(survey_raw))
stopifnot(toupper(VAR_VOTE_TM1)    %in% names(survey_raw))

VAR_TURNOUT_T   <- toupper(VAR_TURNOUT_T)
VAR_VOTE_T      <- toupper(VAR_VOTE_T)
VAR_TURNOUT_TM1 <- toupper(VAR_TURNOUT_TM1)
VAR_VOTE_TM1    <- toupper(VAR_VOTE_TM1)
if (!is.null(VAR_DESIGN_WEIGHT)) VAR_DESIGN_WEIGHT <- toupper(VAR_DESIGN_WEIGHT)

# Move to wide format
df_wide <- survey_raw %>%
  transmute(
    iso2c         = CTX_ISO2C,
    election_date = CTX_ELECTION_DATE,
    year          = CTX_YEAR,
    elec_id       = CTX_ELEC_ID,
    id            = sprintf("%s-%04d", CTX_ELEC_ID, row_number()),
    weights       = if (!is.null(VAR_DESIGN_WEIGHT)) as.numeric(.data[[VAR_DESIGN_WEIGHT]]) else 1,
    part          = clean_turnout(.data[[VAR_TURNOUT_T]]),
    l_part        = clean_turnout(.data[[VAR_TURNOUT_TM1]]),
    vote_raw      = clean_vote(.data[[VAR_VOTE_T]]),
    l_vote_raw    = clean_vote(.data[[VAR_VOTE_TM1]])
  ) %>%
  mutate(
    vote_raw   = ifelse(part   == 0, NA_real_, vote_raw),
    l_vote_raw = ifelse(l_part == 0, NA_real_, l_vote_raw)
  )


# ------------------------------------------------
# 6. Construct voteswitchR-compatible mapping
# ------------------------------------------------

mapping_AT2019 <- party_map_t %>%
  mutate(
    elec_id = CTX_ELEC_ID,
    stack   = row_number()
  ) %>%
  left_join(party_map_tm1, by = "party_name") %>%
  left_join(party_parlgov, by = "party_name") %>%
  left_join(party_vote_shares, by = "party_name") %>%
  mutate(
    turnout     = CTX_TURNOUT,
    turnout_lag = CTX_TURNOUT_LAG
  ) %>%
  rename(
    party_name = party_name
  ) %>%
  dplyr::select(
    elec_id,
    stack,
    party_name,
    map_vote_t,
    map_vote_tm1,
    parlgov_id_1,
    vote_share,
    vote_share_lag,
    turnout,
    turnout_lag
  )


# ------------------------------------------------
# 7. Map survey vote codes → stack indices
# ------------------------------------------------

df <- df_wide
mappings_k <- mapping_AT2019
n_prty <- nrow(mappings_k)

vote_map   <- setNames(mappings_k$stack, mappings_k$map_vote_t)
l_vote_map <- setNames(mappings_k$stack, mappings_k$map_vote_tm1)

df_m <- df %>%
  mutate(
    vote   = unname(vote_map[as.character(vote_raw)]),
    l_vote = unname(l_vote_map[as.character(l_vote_raw)])
  ) %>%
  mutate(
    vote = case_when(
      part == 0         ~ 99L,
      is.na(vote_raw)   ~ NA_integer_,
      is.na(vote)       ~ 98L,
      TRUE              ~ as.integer(vote)
    ),
    l_vote = case_when(
      l_part == 0       ~ 99L,
      is.na(l_vote_raw) ~ NA_integer_,
      is.na(l_vote)     ~ 98L,
      TRUE              ~ as.integer(l_vote)
    )
  )


# ------------------------------------------------
# 8. Rake survey weights to official vote shares
# ------------------------------------------------

known_stacks_t   <- mappings_k %>% filter(!is.na(vote_share))     %>% pull(stack) %>% as.integer()
known_stacks_tm1 <- mappings_k %>% filter(!is.na(vote_share_lag)) %>% pull(stack) %>% as.integer()

turnout   <- unique(stats::na.omit(mappings_k$turnout))[1]
l_turnout <- unique(stats::na.omit(mappings_k$turnout_lag))[1]

if (is.na(turnout) || is.na(l_turnout)) {
  
  message("Turnout information missing – raking skipped.")
  df_m$raked_weights <- NA_real_
  
} else {
  
  df_m <- df_m %>%
    mutate(
      vote_rake = case_when(
        is.na(vote)               ~ NA_integer_,
        vote == 99L               ~ 99L,
        vote %in% known_stacks_t  ~ vote,
        TRUE                      ~ 98L
      ),
      l_vote_rake = case_when(
        is.na(l_vote)                 ~ NA_integer_,
        l_vote == 99L                 ~ 99L,
        l_vote %in% known_stacks_tm1  ~ l_vote,
        TRUE                          ~ 98L
      )
    )
  
  vote_known <- mappings_k %>%
    filter(stack %in% known_stacks_t) %>%
    arrange(match(stack, known_stacks_t)) %>%
    pull(vote_share)
  
  l_vote_known <- mappings_k %>%
    filter(stack %in% known_stacks_tm1) %>%
    arrange(match(stack, known_stacks_tm1)) %>%
    pull(vote_share_lag)
  
  resid_t   <- max(0, 1 - sum(vote_known,   na.rm = TRUE))
  resid_tm1 <- max(0, 1 - sum(l_vote_known, na.rm = TRUE))
  
  target_vote <- c(vote_known, resid_t)
  names(target_vote) <- as.character(c(known_stacks_t, 98L))
  target_vote <- target_vote * turnout
  target_vote <- c(target_vote, `99` = 1 - turnout)
  
  target_l_vote <- c(l_vote_known, resid_tm1)
  names(target_l_vote) <- as.character(c(known_stacks_tm1, 98L))
  target_l_vote <- target_l_vote * l_turnout
  target_l_vote <- c(target_l_vote, `99` = 1 - l_turnout)
  
  target_vote   <- target_vote   / sum(target_vote,   na.rm = TRUE)
  target_l_vote <- target_l_vote / sum(target_l_vote, na.rm = TRUE)
  
  df_m <- df_m %>%
    mutate(
      weights = as.numeric(weights),
      weights = if_else(is.na(weights) | weights <= 0, 1, weights),
      weights = weights / mean(weights)
    )
  
  w <- df_m$weights
  names(w) <- df_m$id
  
  df_m$raked_weights <- anesrake::anesrake(
    inputter  = list(vote_rake = target_vote, l_vote_rake = target_l_vote),
    dataframe = as.data.frame(df_m %>% mutate(across(c(vote_rake, l_vote_rake), as.factor))),
    caseid    = df_m$id,
    weightvec = w,
    pctlim    = 0.005,
    cap       = 5
  )$weightvec
}

# ------------------------------------------------
# 9. Aggregate voter transitions (party-level)
# ------------------------------------------------

aggregate_switches <- function(dat, weights_var = "weights") {
  
  # total survey size for the election
  N <- nrow(dat)
  
  dat %>%
    transmute(
      elec_id = elec_id,
      switch_from = l_vote,
      switch_to   = vote,
      w = .data[[weights_var]]
    ) %>%
    filter(!is.na(switch_from), !is.na(switch_to), !is.na(w)) %>%
    group_by(elec_id, switch_from, switch_to) %>%
    summarise(
      weights = sum(w),
      n = N,
      .groups = "drop"
    )
}

switches_AT2019 <- aggregate_switches(df_m, "weights")

raked_switches_AT2019 <- if (all(is.na(df_m$raked_weights))) {
  NULL
} else {
  aggregate_switches(df_m, "raked_weights")
}


# ------------------------------------------------
# 10. Construct mapping file compatible with voteswitchR
# ------------------------------------------------

mapping_AT2019 <- mapping_AT2019 %>%
  mutate(
    elec_id = CTX_ELEC_ID
  ) %>%
  dplyr::select(
    elec_id,
    stack,
    party_name,
    map_vote_t,
    map_vote_tm1,
    parlgov_id_1,
    vote_share,
    vote_share_lag,
    turnout,
    turnout_lag
  )



# ------------------------------------------------
# 11. Quick diagnostic print
# ------------------------------------------------

cat("\nTransition matrix (raw weights)\n")
print(
  xtabs(weights ~ switch_from + switch_to,
        data = switches_AT2019)
)

if (!is.null(raked_switches_AT2019)) {
  
  cat("\nTransition matrix (raked weights)\n")
  
  print(
    xtabs(weights ~ switch_from + switch_to,
          data = raked_switches_AT2019)
  )
  
}


# ------------------------------------------------
# 11. Quick diagnostic print
# ------------------------------------------------

cat("\nTransition matrix (raw weights)\n")
print(
  xtabs(weights ~ switch_from + switch_to,
        data = switches_AT2019)
)

if (!is.null(raked_switches_AT2019)) {
  
  cat("\nTransition matrix (raked weights)\n")
  
  print(
    xtabs(weights ~ switch_from + switch_to,
          data = raked_switches_AT2019)
  )
  
}

# ------------------------------------------------
# 12. Construct mapping rows for this election
# ------------------------------------------------

mapping_AT2019_ext <- tibble::tibble(
  iso2c = "AT",
  countryname = "Austria",
  year = 2019,
  edate = as.Date("2019-09-29"),
  edate_lag = as.Date("2017-10-15"),
  elec_id = "AT-2019-09",
  elec_id_lag = "AT-2017-10",
  turnout = 0.756,
  turnout_lag = 0.7920535,
  stack = mapping_AT2019$stack,
  party_name = mapping_AT2019$party_name,
  map_vote = mapping_AT2019$map_vote_t,
  vote_share = mapping_AT2019$vote_share,
  vote_share_lag = mapping_AT2019$vote_share_lag,
  parlgov_id_1 = mapping_AT2019$parlgov_id_1
)

missing_cols <- setdiff(names(mappings), names(mapping_AT2019_ext))
mapping_AT2019_ext[missing_cols] <- NA

mapping_AT2019_ext <- mapping_AT2019_ext %>%
  dplyr::select(dplyr::all_of(names(mappings)))

# ------------------------------------------------
# 13. Construct electoral context rows
# ------------------------------------------------

added_rows <- if (!is.null(raked_switches_AT2019)) {
  raked_switches_AT2019
} else {
  switches_AT2019
}

added_rows %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id)

# ------------------------------------------------
# 14. Cleanup temporary objects
# ------------------------------------------------

rm(
  survey_raw,
  df_wide,
  df,
  df_m,
  mappings_k,
  vote_map,
  l_vote_map,
  party_map_t,
  party_map_tm1,
  party_parlgov,
  party_vote_shares,
  known_stacks_t,
  known_stacks_tm1,
  vote_known,
  l_vote_known,
  resid_t,
  resid_tm1,
  target_vote,
  target_l_vote,
  w,
  turnout,
  l_turnout,
  mapping_AT2019,
  raked_switches_AT2019,
  switches_AT2019
)
