# ================================================================
# AT_2024.R
# ================================================================

# ------------------------------------------------
# 0. Packages
# ------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tibble)
  library(labelled)
  library(anesrake)
})

# ------------------------------------------------
# 1. CONFIG (ELECTION-SPECIFIC)
# ------------------------------------------------
folder_location <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files"
cses_path <- file.path(folder_location, "cses6", "cses6.dta")

CTX_ISO2C         <- "AT"
CTX_ELEC_ID       <- "AT-2024-09"
CTX_ELECTION_DATE <- as.Date("2024-09-29")
CTX_YEAR          <- 2024L
CTX_CONTEXT_CODE  <- "AUT_2024"

# ------------------------------------------------
# 2. VARIABLE MAPPING (SURVEY-SPECIFIC)
# ------------------------------------------------
VAR_CONTEXT        <- "F1004"
VAR_TURNOUT_T      <- "F3010_LH"
VAR_VOTE_T         <- "F3011_LH_PL"
VAR_TURNOUT_TM1    <- "F3015_LH"
VAR_VOTE_TM1       <- "F3016_LH_PL"
VAR_DESIGN_WEIGHT  <- NULL

# ------------------------------------------------
# 3. CONTEXT HAND-CODING BLOCK
# ------------------------------------------------

# 3.1 Official turnout
CTX_TURNOUT     <- 0.777
CTX_TURNOUT_LAG <- 0.756

# 3.2 Harmonized party universe
lut_peid_t <- tibble::tribble(
  ~map_vote_t, ~peid,
  40001L, "Freedom Party of Austria",
  40002L, "Austrian People's Party",
  40003L, "Social Democratic Party of Austria",
  40004L, "NEOS",
  40005L, "The Greens",
  40006L, "Communist Party of Austria"
)

lut_peid_tm1 <- tibble::tribble(
  ~map_vote_tm1, ~peid,
  40001L, "Freedom Party of Austria",
  40002L, "Austrian People's Party",
  40003L, "Social Democratic Party of Austria",
  40004L, "NEOS",
  40005L, "The Greens",
  40006L, "Communist Party of Austria",
  40013L, "JETZT - Pilz List"
)

# 3.3 ParlGov identifiers
lut_parlgov <- tibble::tribble(
  ~peid, ~parlgov_id_1,
  "Freedom Party of Austria",             50L,
  "Austrian People's Party",            1013L,
  "Social Democratic Party of Austria",  973L,
  "NEOS",                               2255L,
  "The Greens",                         1429L,
  "JETZT - Pilz List",                  2651L,
  "Communist Party of Austria",          769L
)

lut_shares <- tibble::tribble(
  ~peid, ~vote_share, ~vote_share_lag,
  "Freedom Party of Austria",            0.2885, 0.1617,
  "Austrian People's Party",             0.2627, 0.3746,
  "Social Democratic Party of Austria",  0.2114, 0.2118,
  "NEOS",                                0.0914, 0.0810,
  "The Greens",                          0.0824, 0.1390,
  "Communist Party of Austria",          0.0239, 0.0069,
  "JETZT - Pilz List",                      NA, 0.0187
)

# ------------------------------------------------
# 4. Load data
# ------------------------------------------------
cses_raw <- haven::read_dta(cses_path)
names(cses_raw) <- toupper(names(cses_raw))

VAR_CONTEXT      <- toupper(VAR_CONTEXT)
VAR_TURNOUT_T    <- toupper(VAR_TURNOUT_T)
VAR_VOTE_T       <- toupper(VAR_VOTE_T)
VAR_TURNOUT_TM1  <- toupper(VAR_TURNOUT_TM1)
VAR_VOTE_TM1     <- toupper(VAR_VOTE_TM1)
if (!is.null(VAR_DESIGN_WEIGHT)) VAR_DESIGN_WEIGHT <- toupper(VAR_DESIGN_WEIGHT)

stopifnot(VAR_CONTEXT %in% names(cses_raw))
stopifnot(VAR_TURNOUT_T %in% names(cses_raw))
stopifnot(VAR_VOTE_T %in% names(cses_raw))
stopifnot(VAR_TURNOUT_TM1 %in% names(cses_raw))
stopifnot(VAR_VOTE_TM1 %in% names(cses_raw))

cses_ctx <- cses_raw %>%
  dplyr::filter(as.character(haven::as_factor(.data[[VAR_CONTEXT]])) == CTX_CONTEXT_CODE)

if (nrow(cses_ctx) == 0L) {
  stop("No rows found for requested election context.")
}

# ------------------------------------------------
# 5. Cleaning functions (CSES recodes)
# ------------------------------------------------
clean_turnout <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(93, 96, 97, 98, 99)] <- NA_real_
  x[!(x %in% c(0, 1))] <- NA_real_
  x
}

clean_vote <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(999992, 999993, 999997, 999998, 999999)] <- NA_real_
  x
}

# ------------------------------------------------
# 5b. Inspect party labels before collapsing to 98
# ------------------------------------------------
prune_val_labels <- function(x) {
  labs <- labelled::val_labels(x)
  vals_used <- unique(as.numeric(x))
  labs_pruned <- labs[labs %in% vals_used]
  labelled::val_labels(x) <- labs_pruned
  x
}

extract_party_labels <- function(x, varname) {
  labs <- labelled::val_labels(x)
  
  tibble(
    variable = varname,
    code     = unname(labs),
    label    = names(labs)
  ) %>%
    dplyr::arrange(code)
}

cses_ctx[[VAR_VOTE_T]]   <- prune_val_labels(cses_ctx[[VAR_VOTE_T]])
cses_ctx[[VAR_VOTE_TM1]] <- prune_val_labels(cses_ctx[[VAR_VOTE_TM1]])

party_labels_t   <- extract_party_labels(cses_ctx[[VAR_VOTE_T]], VAR_VOTE_T)
party_labels_tm1 <- extract_party_labels(cses_ctx[[VAR_VOTE_TM1]], VAR_VOTE_TM1)

cat("============================================================\n")
cat("Context:", CTX_CONTEXT_CODE, "\n")
cat("Rows in subset:", nrow(cses_ctx), "\n")
cat("============================================================\n")

cat("\n-----------------------------\n")
cat("Party labels in current election (t)\n")
cat("-----------------------------\n")
print(party_labels_t, n = Inf)

cat("\n-----------------------------\n")
cat("Party labels in lagged election (t-1)\n")
cat("-----------------------------\n")
print(party_labels_tm1, n = Inf)

cat("\n-----------------------------\n")
cat("Observed codes in current election (t)\n")
cat("-----------------------------\n")
print(table(cses_ctx[[VAR_VOTE_T]], useNA = "ifany"))

cat("\n-----------------------------\n")
cat("Observed codes in lagged election (t-1)\n")
cat("-----------------------------\n")
print(table(cses_ctx[[VAR_VOTE_TM1]], useNA = "ifany"))

# ------------------------------------------------
# 6a. Construct standardized microdata
# ------------------------------------------------
df_wide <- cses_ctx %>%
  dplyr::transmute(
    iso2c         = CTX_ISO2C,
    election_date = CTX_ELECTION_DATE,
    year          = CTX_YEAR,
    elec_id       = CTX_ELEC_ID,
    id            = sprintf("%s-%04d", CTX_ELEC_ID, row_number()),
    weights       = if (!is.null(VAR_DESIGN_WEIGHT)) as.numeric(.data[[VAR_DESIGN_WEIGHT]]) else 1,
    part          = clean_turnout(.data[[VAR_TURNOUT_T]]),
    l_part        = clean_turnout(.data[[VAR_TURNOUT_TM1]]),
    vote          = clean_vote(.data[[VAR_VOTE_T]]),
    l_vote        = clean_vote(.data[[VAR_VOTE_TM1]])
  ) %>%
  dplyr::mutate(
    vote   = ifelse(part == 0, NA_real_, vote),
    l_vote = ifelse(l_part == 0, NA_real_, l_vote)
  )

# Collapse only parties outside the harmonized universe to residual category 98
valid_codes_t   <- lut_peid_t$map_vote_t
valid_codes_tm1 <- lut_peid_tm1$map_vote_tm1

df_wide <- df_wide %>%
  dplyr::mutate(
    vote = dplyr::case_when(
      !is.na(vote) & !(vote %in% valid_codes_t) ~ 98L,
      TRUE ~ as.integer(vote)
    ),
    l_vote = dplyr::case_when(
      !is.na(l_vote) & !(l_vote %in% valid_codes_tm1) ~ 98L,
      TRUE ~ as.integer(l_vote)
    )
  )

# ------------------------------------------------
# 6b. Construct voteswitchR-compatible mapping
# ------------------------------------------------
mapping_AT2024 <- full_join(
  lut_peid_t,
  lut_peid_tm1,
  by = "peid"
) %>%
  mutate(
    elec_id = CTX_ELEC_ID,
    stack   = row_number()
  ) %>%
  left_join(lut_parlgov, by = "peid") %>%
  left_join(lut_shares,  by = "peid") %>%
  mutate(
    turnout     = CTX_TURNOUT,
    turnout_lag = CTX_TURNOUT_LAG
  ) %>%
  dplyr::select(
    elec_id,
    stack,
    peid,
    map_vote_t,
    map_vote_tm1,
    parlgov_id_1,
    vote_share,
    vote_share_lag,
    turnout,
    turnout_lag
  )

# ------------------------------------------------
# 7. Map survey vote codes -> stack indices
# ------------------------------------------------
df <- df_wide
mappings_k <- mapping_AT2024
n_prty <- nrow(mappings_k)

df <- df %>%
  mutate(
    vote_old   = vote,
    l_vote_old = l_vote
  )

vote_map   <- setNames(mappings_k$stack, mappings_k$map_vote_t)
l_vote_map <- setNames(mappings_k$stack, mappings_k$map_vote_tm1)

df_m <- df %>%
  mutate(
    vote   = unname(vote_map[as.character(vote_old)]),
    l_vote = unname(l_vote_map[as.character(l_vote_old)])
  ) %>%
  mutate(
    vote = case_when(
      part == 0         ~ 99L,
      is.na(vote_old)   ~ NA_integer_,
      is.na(vote)       ~ 98L,
      TRUE              ~ as.integer(vote)
    ),
    l_vote = case_when(
      l_part == 0       ~ 99L,
      is.na(l_vote_old) ~ NA_integer_,
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
        is.na(vote) ~ NA_integer_,
        vote == 99L ~ 99L,
        vote %in% known_stacks_t ~ vote,
        TRUE ~ 98L
      ),
      l_vote_rake = case_when(
        is.na(l_vote) ~ NA_integer_,
        l_vote == 99L ~ 99L,
        l_vote %in% known_stacks_tm1 ~ l_vote,
        TRUE ~ 98L
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
  
  df_m <- df_m %>%
    mutate(
      vote_rake   = factor(vote_rake,   levels = names(target_vote)),
      l_vote_rake = factor(l_vote_rake, levels = names(target_l_vote))
    )
  
  df_m$raked_weights <- anesrake::anesrake(
    inputter  = list(vote_rake = target_vote, l_vote_rake = target_l_vote),
    dataframe = as.data.frame(df_m),
    caseid    = df_m$id,
    weightvec = w,
    pctlim    = 0.005,
    cap       = 5
  )$weightvec
}

achieved_vote <- df_m %>%
  dplyr::filter(!is.na(vote_rake), !is.na(raked_weights)) %>%
  dplyr::group_by(vote_rake) %>%
  dplyr::summarise(
    achieved = sum(raked_weights),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    achieved = achieved / sum(achieved),
    target = as.numeric(target_vote[as.character(vote_rake)]),
    diff = achieved - target
  )

achieved_l_vote <- df_m %>%
  dplyr::filter(!is.na(l_vote_rake), !is.na(raked_weights)) %>%
  dplyr::group_by(l_vote_rake) %>%
  dplyr::summarise(
    achieved = sum(raked_weights),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    achieved = achieved / sum(achieved),
    target = as.numeric(target_l_vote[as.character(l_vote_rake)]),
    diff = achieved - target
  )

achieved_vote
achieved_l_vote

# ------------------------------------------------
# 9. Aggregate voter transitions (party-level)
# ------------------------------------------------
aggregate_switches <- function(dat, weights_var = "weights") {
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

switches_AT2024 <- aggregate_switches(df_m, "weights")

raked_switches_AT2024 <- if (all(is.na(df_m$raked_weights))) {
  NULL
} else {
  aggregate_switches(df_m, "raked_weights")
}

# ------------------------------------------------
# 10. Quick diagnostic print
# ------------------------------------------------
cat("\nTransition matrix (raw weights)\n")
print(
  xtabs(weights ~ switch_from + switch_to,
        data = switches_AT2024)
)

if (!is.null(raked_switches_AT2024)) {
  cat("\nTransition matrix (raked weights)\n")
  print(
    xtabs(weights ~ switch_from + switch_to,
          data = raked_switches_AT2024)
  )
}

# ------------------------------------------------
# 11. Construct mapping file compatible with voteswitchR
# ------------------------------------------------
mapping_AT2024_clean <- mapping_AT2024 %>%
  mutate(
    elec_id = CTX_ELEC_ID,
    party_name = peid
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
# 12. Construct mapping rows for this election
# ------------------------------------------------
mapping_AT2024_ext <- tibble::tibble(
  iso2c = "AT",
  countryname = "Austria",
  year = 2024,
  edate = as.Date("2024-09-29"),
  edate_lag = as.Date("2019-09-29"),
  elec_id = "AT-2024-09",
  elec_id_lag = "AT-2019-09",
  turnout = 0.777,
  turnout_lag = 0.756,
  stack = mapping_AT2024_clean$stack,
  party_name = mapping_AT2024_clean$party_name,
  map_vote = mapping_AT2024_clean$map_vote_t,
  parlgov_id_1 = mapping_AT2024_clean$parlgov_id_1,
  vote_share = mapping_AT2024_clean$vote_share,
  vote_share_lag = mapping_AT2024_clean$vote_share_lag
)

missing_cols <- setdiff(names(mappings), names(mapping_AT2024_ext))
mapping_AT2024_ext[missing_cols] <- NA

mapping_AT2024_ext <- mapping_AT2024_ext %>%
  dplyr::select(dplyr::all_of(names(mappings)))

# ------------------------------------------------
# 13. Construct electoral context rows
# ------------------------------------------------
added_rows <- if (!is.null(raked_switches_AT2024)) {
  raked_switches_AT2024
} else {
  switches_AT2024
}

added_rows %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id)

# ------------------------------------------------
# 14. Cleanup temporary objects
# ------------------------------------------------
rm(
  df_wide,
  df,
  df_m,
  mappings_k,
  vote_map,
  l_vote_map,
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
  valid_codes_t,
  valid_codes_tm1,
  mapping_AT2024,
  mapping_AT2024_clean,
  raked_switches_AT2024,
  switches_AT2024,
  cses_ctx,
  cses_raw,
  lut_parlgov,
  lut_peid_t,
  lut_peid_tm1,
  lut_shares
)


