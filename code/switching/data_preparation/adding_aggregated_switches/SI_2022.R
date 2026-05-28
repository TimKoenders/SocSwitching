# ================================================================
# SI_2022.R
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

CTX_ISO2C         <- "SI"
CTX_ELEC_ID       <- "SI-2022-04"
CTX_ELECTION_DATE <- as.Date("2022-04-24")
CTX_YEAR          <- 2022L

CTX_CONTEXT_CODE  <- "SVN_2022"

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
# 4. Load data, subset early, and inspect
# ------------------------------------------------
cses_raw <- haven::read_dta(cses_path)
names(cses_raw) <- toupper(names(cses_raw))

VAR_CONTEXT      <- toupper(VAR_CONTEXT)
VAR_TURNOUT_T    <- toupper(VAR_TURNOUT_T)
VAR_VOTE_T       <- toupper(VAR_VOTE_T)
VAR_TURNOUT_TM1  <- toupper(VAR_TURNOUT_TM1)
VAR_VOTE_TM1     <- toupper(VAR_VOTE_TM1)

cses_ctx <- cses_raw %>%
  filter(as.character(haven::as_factor(.data[[VAR_CONTEXT]])) == CTX_CONTEXT_CODE)

if (nrow(cses_ctx) == 0L) {
  stop("No rows found for requested election context.")
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
# 5. CONTEXT HAND-CODING BLOCK
# ------------------------------------------------

# 5.1 Official turnout
CTX_TURNOUT     <- 0.7097
CTX_TURNOUT_LAG <- 0.5263

# 5.2 Harmonized party universe
# Current election (T, SVN_2022)
lut_peid_t <- tibble::tribble(
  ~map_vote_t, ~peid,
  705001L, "Freedom Movement",
  705002L, "Slovenian Democratic Party",
  705003L, "New Slovenia - Christian People's Party",
  705004L, "Social Democrats",
  705005L, "The Left",
  705006L, "List of Marjan Sarec",
  705007L, "Let's Connect Slovenia",
  705008L, "Resni.ca",
  705009L, "Party of Alenka Bratusek",
  705010L, "Healthy Society Movement",
  705011L, "Good State",
  705012L, "Pirate Party",
  705013L, "Our Country",
  705014L, "Slovenian National Party",
  705015L, "VESNA - Green Party",
  705016L, "Democratic Party of Pensioners of Slovenia",
  705017L, "List of Boris Popovic - Let's Digitize Slovenia"
)

# Lag election (T-1, SVN_2018)
lut_peid_tm1 <- tibble::tribble(
  ~map_vote_tm1, ~peid,
  705002L, "Slovenian Democratic Party",
  705003L, "New Slovenia - Christian People's Party",
  705004L, "Social Democrats",
  705005L, "The Left",
  705006L, "List of Marjan Sarec",
  705007L, "Let's Connect Slovenia",
  705009L, "Party of Alenka Bratusek",
  705011L, "Good State",
  705012L, "Pirate Party",
  705016L, "Democratic Party of Pensioners of Slovenia",
  705018L, "Modern Centre Party"
)

# 5.3 ParlGov identifiers
lut_parlgov <- tibble::tribble(
  ~peid, ~parlgov_id_1,
  "Freedom Movement", 2856L,
  "Slovenian Democratic Party", 179L,
  "New Slovenia - Christian People's Party", 1047L,
  "Social Democrats", 706L,
  "The Left", 2670L,
  "List of Marjan Sarec", 2668L,
  "Let's Connect Slovenia", NA_integer_,
  "Resni.ca", 2885L,
  "Party of Alenka Bratusek", 2337L,
  "Healthy Society Movement", 2886L,
  "Good State", 2669L,
  "Pirate Party", 2338L,
  "Our Country", 2866L,
  "Slovenian National Party", 981L,
  "VESNA - Green Party", 2865L,
  "Democratic Party of Pensioners of Slovenia", 1178L,
  "List of Boris Popovic - Let's Digitize Slovenia", NA_integer_,
  "Modern Centre Party", 2618L
)


# 5.4 Official vote shares aligned to observed CSES categories
# Any official vote not represented by an explicit CSES categoryis left to the residual bucket (98) during raking.
lut_shares <- tibble::tribble(
  ~peid, ~vote_share, ~vote_share_lag,
  "Freedom Movement", 0.3445, NA_real_,
  "Slovenian Democratic Party", 0.2348, 0.2492,
  "New Slovenia - Christian People's Party", 0.0686, 0.0716,
  "Social Democrats", 0.0669, 0.0993,
  "The Left", 0.0446, 0.0933,
  "List of Marjan Sarec", 0.0372, 0.1260,
  "Let's Connect Slovenia", 0.0370, NA_real_,
  "Resni.ca", 0.0286, NA_real_,
  "Party of Alenka Bratusek", 0.0261, 0.0511,
  "Healthy Society Movement", 0.0176, NA_real_,
  "Good State", 0.0170, 0.0152,
  "Pirate Party", 0.0163, 0.0215,
  "Our Country", 0.0150, NA_real_,
  "Slovenian National Party", 0.0149, 0.0417,
  "VESNA - Green Party", 0.0135, NA_real_,
  "Democratic Party of Pensioners of Slovenia", NA_real_, 0.0493,
  "List of Boris Popovic - Let's Digitize Slovenia", NA_real_, NA_real_,
  "Modern Centre Party", NA_real_, 0.0975
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
  x[x %in% c(999993, 999998, 999999)] <- NA_real_
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
mapping_SVN2022 <- full_join(
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
mappings_k <- mapping_SVN2022
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
switches_SVN2022 <- aggregate_switches(df_m, "weights")

raked_switches_SVN2022 <- if (all(is.na(df_m$raked_weights))) {
  NULL
} else {
  aggregate_switches(df_m, "raked_weights")
}

all_codes <- c(seq_len(nrow(mapping_SVN2022)), 98, 99)

added_rows <- if (!is.null(raked_switches_SVN2022)) {
  raked_switches_SVN2022
} else {
  switches_SVN2022
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
  data = switches_SVN2022
)

M_raked <- xtabs(
  weights ~ factor(switch_from, levels = all_codes) +
    factor(switch_to, levels = all_codes),
  data = raked_switches_SVN2022
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

if (!is.null(raked_switches_SVN2022)) {
  cat("\nTransition matrix (raked weights)\n")
  print(M_raked)
}

# ------------------------------------------------
# 14. Construct mapping file compatible with voteswitchR
# ------------------------------------------------
mapping_SVN2022_clean <- mapping_SVN2022 %>%
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
mapping_SVN2022_ext <- tibble::tibble(
  iso2c = "SI",
  countryname = "Slovenia",
  year = 2022,
  edate = as.Date("2022-04-24"),
  edate_lag = as.Date("2018-06-03"),
  elec_id = "SI-2022-04",
  elec_id_lag = "SVN-2018-06",
  turnout = 0.7097,
  turnout_lag = 0.5263,
  stack = mapping_SVN2022_clean$stack,
  party_name = mapping_SVN2022_clean$party_name,
  map_vote = mapping_SVN2022_clean$map_vote_t,
  vote_share = mapping_SVN2022_clean$vote_share,
  vote_share_lag = mapping_SVN2022_clean$vote_share_lag,
  parlgov_id_1 = mapping_SVN2022_clean$parlgov_id_1
)

missing_cols <- setdiff(names(mappings), names(mapping_SVN2022_ext))
mapping_SVN2022_ext[missing_cols] <- NA

mapping_SVN2022_ext <- mapping_SVN2022_ext %>%
  dplyr::select(dplyr::all_of(names(mappings)))


# ------------------------------------------------
# 16. Construct electoral context rows
# ------------------------------------------------
added_rows <- if (!is.null(raked_switches_SVN2022)) {
  raked_switches_SVN2022
} else {
  switches_SVN2022
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
  mapping_SVN2022,
  mapping_SVN2022_clean,
  raked_switches_SVN2022,
  switches_SVN2022,
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





