# ================================================================
# PL_2023.R
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tibble)
  library(labelled)
})

# ------------------------------------------------
# 1. CONFIG (ELECTION-SPECIFIC)
# ------------------------------------------------
folder_location <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files"
cses_path <- file.path(folder_location, "cses6", "cses6.dta")

CTX_ISO2C         <- "PL"
CTX_ELEC_ID       <- "PL-2023-10"
CTX_ELECTION_DATE <- as.Date("2023-10-15")
CTX_YEAR          <- 2023L

CTX_CONTEXT_CODE  <- "POL_2023"

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
# 3. Helpers
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
    code = unname(labs),
    label = names(labs)
  ) %>%
    arrange(code)
}

# ------------------------------------------------
# 4. Load data, standardize names, and subset early
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
  filter(as.character(haven::as_factor(.data[[VAR_CONTEXT]])) == CTX_CONTEXT_CODE)

if (nrow(cses_ctx) == 0L) {
  stop("No rows found for requested election context.")
}

cses_ctx[[VAR_VOTE_T]]   <- prune_val_labels(cses_ctx[[VAR_VOTE_T]])
cses_ctx[[VAR_VOTE_TM1]] <- prune_val_labels(cses_ctx[[VAR_VOTE_TM1]])

party_labels_t   <- extract_party_labels(cses_ctx[[VAR_VOTE_T]], VAR_VOTE_T)
party_labels_tm1 <- extract_party_labels(cses_ctx[[VAR_VOTE_TM1]], VAR_VOTE_TM1)

cat("\n============================================================\n")
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
# 5. CONTEXT HAND-CODING BLOCK
# ------------------------------------------------

# 5.1 Official turnout
CTX_TURNOUT     <- 0.7438
CTX_TURNOUT_LAG <- 0.6158

# 5.2 Harmonized party universe
# Current election (T, POL_2023)
lut_peid_t <- tibble::tribble(
  ~map_vote_t, ~peid,
  616001L, "United Right",
  616002L, "Civic Coalition",
  616003L, "Third Way",
  616004L, "The Left",
  616006L, "Confederation Liberty and Independence",
  616009L, "Nonpartisan Local Government Activists",
  616010L, "There is One Poland"
)

# Lag election (T-1)
lut_peid_tm1 <- tibble::tribble(
  ~map_vote_tm1, ~peid,
  616001L, "United Right",
  616002L, "Civic Coalition",
  616004L, "The Left",
  616006L, "Confederation Liberty and Independence",
  616015L, "Polish Coalition"
)

# 5.3 ParlGov identifiers
lut_parlgov <- tibble::tribble(
  ~peid, ~parlgov_id_1,
  "United Right", NA_integer_,
  "Civic Coalition", NA_integer_,
  "Third Way", NA_integer_,
  "The Left", NA_integer_,
  "Confederation Liberty and Independence", NA_integer_,
  "Nonpartisan Local Government Activists", NA_integer_,
  "There is One Poland", NA_integer_,
  "Polish Coalition", NA_integer_
)

# 5.4 Official vote shares aligned to observed CSES categories
# Any official vote not represented by an explicit CSES category
# is left to the residual bucket (98) during raking.
lut_shares <- tibble::tribble(
  ~peid, ~vote_share, ~vote_share_lag,
  "United Right", 0.3538, 0.4359,
  "Civic Coalition", 0.3070, 0.2740,
  "Third Way", 0.1440, NA_real_,
  "The Left", 0.0861, 0.1256,
  "Confederation Liberty and Independence", 0.0716, 0.0681,
  "Nonpartisan Local Government Activists", 0.0186, NA_real_,
  "There is One Poland", 0.0163, NA_real_,
  "Polish Coalition", NA_real_, 0.0855
)


# ------------------------------------------------
# 6. Construct standardized microdata
# ------------------------------------------------
clean_turnout <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(93, 96, 97, 98, 99)] <- NA_real_
  x[!(x %in% c(0, 1))] <- NA_real_
  x
}

clean_vote <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(999993, 999997, 999998, 999999)] <- NA_real_
  x
}

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

df_wide <- cses_ctx %>%
  transmute(
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
  mutate(
    vote   = ifelse(part == 0, NA_real_, vote),
    l_vote = ifelse(l_part == 0, NA_real_, l_vote)
  )

# ------------------------------------------------
# 7. Construct voteswitchR-compatible mapping
# ------------------------------------------------
mapping_POL2023 <- full_join(
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
# 8. Map survey vote codes to stack indices
# ------------------------------------------------
df <- df_wide
mappings_k <- mapping_POL2023
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
# 9. Rake survey weights to official vote shares
# ------------------------------------------------
known_stacks_t   <- mappings_k %>% filter(!is.na(vote_share))     %>% pull(stack) %>% as.integer()
known_stacks_tm1 <- mappings_k %>% filter(!is.na(vote_share_lag)) %>% pull(stack) %>% as.integer()

turnout   <- unique(stats::na.omit(mappings_k$turnout))[1]
l_turnout <- unique(stats::na.omit(mappings_k$turnout_lag))[1]

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

resid_t   <- max(0, 1 - sum(vote_known, na.rm = TRUE))
resid_tm1 <- max(0, 1 - sum(l_vote_known, na.rm = TRUE))

target_vote <- vote_known
names(target_vote) <- as.character(known_stacks_t)

if (resid_t > 0) {
  target_vote <- c(target_vote, `98` = resid_t)
}

target_vote <- target_vote * turnout
target_vote <- c(target_vote, `99` = 1 - turnout)
target_vote <- target_vote[target_vote > 0]
target_vote <- target_vote / sum(target_vote)

target_l_vote <- l_vote_known
names(target_l_vote) <- as.character(known_stacks_tm1)

if (resid_tm1 > 0) {
  target_l_vote <- c(target_l_vote, `98` = resid_tm1)
}

target_l_vote <- target_l_vote * l_turnout
target_l_vote <- c(target_l_vote, `99` = 1 - l_turnout)
target_l_vote <- target_l_vote[target_l_vote > 0]
target_l_vote <- target_l_vote / sum(target_l_vote)

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
    vote_rake   = factor(vote_rake, levels = names(target_vote)),
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

# ------------------------------------------------
# 10. Aggregate voter transitions
# ------------------------------------------------
switches_POL2023 <- aggregate_switches(df_m, "weights")

raked_switches_POL2023 <- if (all(is.na(df_m$raked_weights))) {
  NULL
} else {
  aggregate_switches(df_m, "raked_weights")
}

all_codes <- c(seq_len(nrow(mapping_POL2023)), 98, 99)

added_rows <- if (!is.null(raked_switches_POL2023)) {
  raked_switches_POL2023
} else {
  switches_POL2023
}

# ------------------------------------------------
# 11. Diagnostics: achieved margins after raking
# ------------------------------------------------
achieved_t <- df_m %>%
  filter(!is.na(vote_rake), !is.na(raked_weights)) %>%
  group_by(vote_rake) %>%
  summarise(
    p = sum(raked_weights) / sum(df_m$raked_weights, na.rm = TRUE),
    .groups = "drop"
  )

achieved_tm1 <- df_m %>%
  filter(!is.na(l_vote_rake), !is.na(raked_weights)) %>%
  group_by(l_vote_rake) %>%
  summarise(
    p = sum(raked_weights) / sum(df_m$raked_weights, na.rm = TRUE),
    .groups = "drop"
  )

target_t_df <- tibble(
  vote_rake = factor(names(target_vote), levels = names(target_vote)),
  target = as.numeric(target_vote)
)

target_tm1_df <- tibble(
  l_vote_rake = factor(names(target_l_vote), levels = names(target_l_vote)),
  target = as.numeric(target_l_vote)
)

check_t <- target_t_df %>%
  left_join(achieved_t, by = "vote_rake") %>%
  mutate(diff = p - target)

check_tm1 <- target_tm1_df %>%
  left_join(achieved_tm1, by = "l_vote_rake") %>%
  mutate(diff = p - target)

print(check_t, n = Inf)
print(check_tm1, n = Inf)

# ------------------------------------------------
# 12. Diagnostics: matrix-implied margins
# ------------------------------------------------
M_raw <- xtabs(
  weights ~ factor(switch_from, levels = all_codes) +
    factor(switch_to, levels = all_codes),
  data = switches_POL2023
)

M_raked <- xtabs(
  weights ~ factor(switch_from, levels = all_codes) +
    factor(switch_to, levels = all_codes),
  data = raked_switches_POL2023
)

M_raw_prop   <- M_raw / sum(M_raw)
M_raked_prop <- M_raked / sum(M_raked)

col_raw   <- colSums(M_raw_prop)
col_raked <- colSums(M_raked_prop)

row_raw   <- rowSums(M_raw_prop)
row_raked <- rowSums(M_raked_prop)

target_t_vec   <- target_vote
target_tm1_vec <- target_l_vote

col_comp <- tibble(
  party  = names(target_t_vec),
  raw    = as.numeric(col_raw[names(target_t_vec)]),
  raked  = as.numeric(col_raked[names(target_t_vec)]),
  target = as.numeric(target_t_vec)
) %>%
  mutate(
    diff_raw = raw - target,
    diff_raked = raked - target,
    abs_diff_raw = abs(diff_raw),
    abs_diff_raked = abs(diff_raked),
    raked_better = case_when(
      abs_diff_raked < abs_diff_raw ~ "yes",
      abs_diff_raked > abs_diff_raw ~ "no",
      TRUE ~ "equal"
    )
  )

row_comp <- tibble(
  party  = names(target_tm1_vec),
  raw    = as.numeric(row_raw[names(target_tm1_vec)]),
  raked  = as.numeric(row_raked[names(target_tm1_vec)]),
  target = as.numeric(target_tm1_vec)
) %>%
  mutate(
    diff_raw = raw - target,
    diff_raked = raked - target,
    abs_diff_raw = abs(diff_raw),
    abs_diff_raked = abs(diff_raked),
    raked_better = case_when(
      abs_diff_raked < abs_diff_raw ~ "yes",
      abs_diff_raked > abs_diff_raw ~ "no",
      TRUE ~ "equal"
    )
  )

print(col_comp, n = Inf)
print(row_comp, n = Inf)

# ------------------------------------------------
# 13. Quick diagnostic print
# ------------------------------------------------
cat("\nTransition matrix (raw weights)\n")
print(M_raw)

if (!is.null(raked_switches_POL2023)) {
  cat("\nTransition matrix (raked weights)\n")
  print(M_raked)
}

# ------------------------------------------------
# 14. Construct mapping file compatible with voteswitchR
# ------------------------------------------------
mapping_POL2023_clean <- mapping_POL2023 %>%
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
# 15. Construct mapping rows for this election
# ------------------------------------------------
mapping_POL2023_ext <- tibble::tibble(
  iso2c = "PL",
  countryname = "Poland",
  year = 2023,
  edate = as.Date("2023-10-15"),
  edate_lag = as.Date("2019-10-13"),
  elec_id = "PL-2023-10",
  elec_id_lag = "POL-2019-10",
  turnout = 0.7438,
  turnout_lag = 0.6158,
  stack = mapping_POL2023_clean$stack,
  party_name = mapping_POL2023_clean$party_name,
  map_vote = mapping_POL2023_clean$map_vote_t,
  vote_share = mapping_POL2023_clean$vote_share,
  vote_share_lag = mapping_POL2023_clean$vote_share_lag,
  parlgov_id_1 = mapping_POL2023_clean$parlgov_id_1
)

missing_cols <- setdiff(names(mappings), names(mapping_POL2023_ext))
mapping_POL2023_ext[missing_cols] <- NA

mapping_POL2023_ext <- mapping_POL2023_ext %>%
  dplyr::select(dplyr::all_of(names(mappings)))

# ------------------------------------------------
# 16. Construct electoral context rows
# ------------------------------------------------
added_rows <- if (!is.null(raked_switches_POL2023)) {
  raked_switches_POL2023
} else {
  switches_POL2023
}

added_rows %>%
  dplyr::distinct(elec_id) %>%
  dplyr::arrange(elec_id)

# ------------------------------------------------
# 17. Cleanup temporary objects
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
  mapping_POL2023,
  mapping_POL2023_clean,
  raked_switches_POL2023,
  switches_POL2023,
  cses_ctx,
  cses_raw,
  lut_parlgov,
  lut_peid_t,
  lut_peid_tm1,
  lut_shares,
  all_codes,
  n_prty,
  party_labels_t,
  party_labels_tm1,
  achieved_t,
  achieved_tm1,
  target_t_df,
  target_tm1_df,
  check_t,
  check_tm1,
  M_raw,
  M_raked,
  M_raw_prop,
  M_raked_prop,
  col_raw,
  col_raked,
  row_raw,
  row_raked,
  target_t_vec,
  target_tm1_vec,
  col_comp,
  row_comp
)
