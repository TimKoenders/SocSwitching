# ================================================================
# 00_prepare_vote_shares_parlgov.R
# Construct election-level family vote shares from Parlgov
# and align the SD series to the model sample
# ================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(ggplot2)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------
path_parlgov <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/parlgov/parlgov-stable.xlsx"
path_flow    <- here("data", "processed", "best_raked_imp_fam.rds")
path_vote_shares <- here("data", "processed", "parlgov_supply_vote_shares.rds")

path_out_long <- here("data", "processed", "parlgov_vote_shares_long.rds")
path_out_wide <- here("data", "processed", "parlgov_vote_shares_wide.rds")
path_fig_sd   <- here("figures", "sd_vote_share_parlgov_model_sample.png")

dir.create(dirname(path_out_long), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_fig_sd), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_vote_shares), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------
elections_raw <- readxl::read_excel(
  path = path_parlgov,
  sheet = "election"
)

party_raw <- readxl::read_excel(
  path = path_parlgov,
  sheet = "party"
)

flow_data <- readRDS(path_flow)

# ------------------------------------------------
# 3. Clean election data
# ------------------------------------------------
country_map <- c(
  "AUS" = "AU",
  "AUT" = "AT",
  "BEL" = "BE",
  "BGR" = "BG",
  "CAN" = "CA",
  "CHE" = "CH",
  "CYP" = "CY",
  "CZE" = "CZ",
  "DEU" = "DE",
  "DNK" = "DK",
  "ESP" = "ES",
  "EST" = "EE",
  "FIN" = "FI",
  "FRA" = "FR",
  "GBR" = "GB",
  "GRC" = "GR",
  "HUN" = "HU",
  "IRL" = "IE",
  "ISL" = "IS",
  "ITA" = "IT",
  "LTU" = "LT",
  "LUX" = "LU",
  "LVA" = "LV",
  "MLT" = "MT",
  "NLD" = "NL",
  "NOR" = "NO",
  "NZL" = "NZ",
  "POL" = "PL",
  "PRT" = "PT",
  "ROU" = "RO",
  "SVK" = "SK",
  "SVN" = "SI",
  "SWE" = "SE"
)

elections <- elections_raw %>%
  dplyr::rename_with(tolower) %>%
  dplyr::transmute(
    country_raw = as.character(country_name_short),
    country_name = as.character(country_name),
    election_type = as.character(election_type),
    election_date = as.Date(election_date),
    vote_share = as.numeric(vote_share),
    election_id = as.numeric(election_id),
    party_id = as.numeric(party_id),
    party_name_short = as.character(party_name_short),
    party_name = as.character(party_name)
  ) %>%
  dplyr::filter(
    election_type == "parliament",
    !is.na(election_date),
    !is.na(vote_share),
    !is.na(party_id)
  ) %>%
  dplyr::mutate(
    country = dplyr::recode(country_raw, !!!country_map, .default = country_raw),
    year = format(election_date, "%Y"),
    month = format(election_date, "%m"),
    elec_id = paste0(country, "-", year, "-", month)
  )

# ------------------------------------------------
# 4. Clean party data
# ------------------------------------------------
party <- party_raw %>%
  dplyr::rename_with(tolower) %>%
  dplyr::transmute(
    party_id = as.numeric(party_id),
    party_name_short = as.character(party_name_short),
    party_name = as.character(party_name),
    family_name_short = as.character(family_name_short),
    family_name = as.character(family_name)
  ) %>%
  dplyr::distinct(party_id, .keep_all = TRUE)

# ------------------------------------------------
# 5. Merge election data with party families
# ------------------------------------------------
parlgov_long <- elections %>%
  dplyr::left_join(
    party %>% dplyr::select(party_id, family_name_short, family_name),
    by = "party_id"
  )

unmatched_parties <- parlgov_long %>%
  dplyr::filter(is.na(family_name_short)) %>%
  dplyr::distinct(country, elec_id, party_id, party_name_short, party_name)

cat("\n==============================\n")
cat("Unmatched Parlgov parties\n")
cat("==============================\n")
cat("N unmatched party rows:", nrow(unmatched_parties), "\n")

# ------------------------------------------------
# 6. Aggregate to election-level family vote shares
# ------------------------------------------------
parlgov_vote_shares_long <- parlgov_long %>%
  dplyr::filter(!is.na(family_name_short)) %>%
  dplyr::group_by(
    country, country_name, election_date, year, month,
    elec_id, family_name_short, family_name
  ) %>%
  dplyr::summarise(
    vote_share = sum(vote_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(country, election_date, family_name_short)

# ------------------------------------------------
# 7. Wide version
# ------------------------------------------------
parlgov_vote_shares_wide <- parlgov_vote_shares_long %>%
  dplyr::select(
    country, country_name, election_date, year, month,
    elec_id, family_name_short, vote_share
  ) %>%
  tidyr::pivot_wider(
    names_from = family_name_short,
    values_from = vote_share,
    values_fill = 0
  ) %>%
  dplyr::arrange(country, election_date)

# ------------------------------------------------
# 8b. Construct supply-side vote-share panel
# ------------------------------------------------
family_map_supply_parlgov <- function(family_name_short) {
  dplyr::case_when(
    family_name_short == "soc" ~ "social_democratic",
    family_name_short %in% c("lef", "com") ~ "far_left",
    family_name_short == "lib" ~ "liberal",
    family_name_short == "eco" ~ "green",
    family_name_short %in% c("nat", "right") ~ "far_right",
    family_name_short %in% c("chr", "con") ~ "con",
    family_name_short %in% c("agr", "oth", "sip", "eth", "mrp") ~ "other_parties",
    TRUE ~ "other_parties"
  )
}

model_competitor_levels <- c(
  "social_democratic",
  "far_left",
  "green",
  "liberal",
  "con",
  "far_right",
  "other_parties"
)

election_lookup <- parlgov_vote_shares_long %>%
  dplyr::distinct(
    elec_id, country, country_name, election_date, year, month
  ) %>%
  dplyr::mutate(
    year_num = as.integer(year)
  )

vote_share_base <- parlgov_vote_shares_long %>%
  dplyr::mutate(
    competitor = family_map_supply_parlgov(family_name_short)
  ) %>%
  dplyr::group_by(elec_id, competitor) %>%
  dplyr::summarise(
    vote_share = sum(vote_share, na.rm = TRUE),
    .groups = "drop"
  )

parlgov_supply_vote_shares <- tidyr::expand_grid(
  elec_id = election_lookup$elec_id,
  competitor = model_competitor_levels
) %>%
  dplyr::left_join(vote_share_base, by = c("elec_id", "competitor")) %>%
  dplyr::mutate(
    vote_share = dplyr::coalesce(vote_share, 0)
  ) %>%
  dplyr::left_join(election_lookup, by = "elec_id") %>%
  dplyr::arrange(country, competitor, election_date, elec_id) %>%
  dplyr::group_by(country, competitor) %>%
  dplyr::mutate(
    vote_share_lag = dplyr::lag(vote_share),
    vote_share_change = vote_share - vote_share_lag
  ) %>%
  dplyr::ungroup()

sd_vote_lookup <- parlgov_supply_vote_shares %>%
  dplyr::filter(competitor == "social_democratic") %>%
  dplyr::select(
    elec_id,
    sd_vote_share = vote_share,
    sd_vote_share_lag = vote_share_lag,
    sd_vote_share_change = vote_share_change
  )

parlgov_supply_vote_shares <- parlgov_supply_vote_shares %>%
  dplyr::left_join(sd_vote_lookup, by = "elec_id") %>%
  dplyr::mutate(
    relative_vote_share = vote_share - sd_vote_share,
    relative_vote_share_change = vote_share_change - sd_vote_share_change
  )

saveRDS(parlgov_supply_vote_shares, path_vote_shares)
saveRDS(parlgov_vote_shares_long, path_out_long)
saveRDS(parlgov_vote_shares_wide, path_out_wide)
# ------------------------------------------------
# 9. Diagnostics for Parlgov aggregation
# ------------------------------------------------
cat("\n==============================\n")
cat("Parlgov family vote shares\n")
cat("==============================\n")
cat("Long rows:", nrow(parlgov_vote_shares_long), "\n")
cat("Wide rows:", nrow(parlgov_vote_shares_wide), "\n")
cat("Countries:", dplyr::n_distinct(parlgov_vote_shares_long$country), "\n")
cat("Elections:", dplyr::n_distinct(parlgov_vote_shares_long$elec_id), "\n")

check_totals <- parlgov_vote_shares_long %>%
  dplyr::group_by(elec_id) %>%
  dplyr::summarise(
    total_vote_share = sum(vote_share, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nVote-share total summary across elections:\n")
print(summary(check_totals$total_vote_share))

# ------------------------------------------------
# 10. Harmonise flow election IDs to Parlgov style
# ------------------------------------------------
sample_elections <- flow_data %>%
  dplyr::distinct(elec_id) %>%
  dplyr::mutate(
    elec_id_parlgov = dplyr::case_when(
      stringr::str_detect(elec_id, "^DNK-") ~ stringr::str_replace(elec_id, "^DNK-", "DK-"),
      TRUE ~ elec_id
    )
  )

match_check <- sample_elections %>%
  dplyr::mutate(
    in_parlgov = elec_id_parlgov %in% parlgov_vote_shares_long$elec_id
  )

cat("\n==============================\n")
cat("Flow sample match to Parlgov\n")
cat("==============================\n")
print(match_check %>% dplyr::count(in_parlgov))

unmatched_elections <- sample_elections %>%
  dplyr::filter(!(elec_id_parlgov %in% parlgov_vote_shares_long$elec_id)) %>%
  dplyr::arrange(elec_id)

cat("\nUnmatched flow elections:\n")
print(unmatched_elections, n = 100)

sample_elections_parlgov <- sample_elections %>%
  dplyr::filter(elec_id_parlgov %in% parlgov_vote_shares_long$elec_id)

# ------------------------------------------------
# 11. Build SD plotting sample matched to model sample
# ------------------------------------------------
df_sd_sample <- parlgov_vote_shares_long %>%
  dplyr::filter(family_name_short == "soc") %>%
  dplyr::semi_join(
    sample_elections_parlgov,
    by = c("elec_id" = "elec_id_parlgov")
  ) %>%
  dplyr::mutate(
    year_num = as.integer(year)
  ) %>%
  dplyr::arrange(country, election_date)

# ------------------------------------------------
# 12. Print included countries
# ------------------------------------------------
countries_included <- df_sd_sample %>%
  dplyr::distinct(country, country_name) %>%
  dplyr::arrange(country)

cat("\n==============================\n")
cat("Countries included in matched SD sample\n")
cat("==============================\n")
print(countries_included, n = Inf)

cat("\nCountry codes included:\n")
cat(paste(countries_included$country, collapse = ", "), "\n")

# ------------------------------------------------
# 13. Diagnostics for matched SD sample
# ------------------------------------------------
cat("\n==============================\n")
cat("Matched SD plotting sample\n")
cat("==============================\n")

df_sd_sample %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year_num, na.rm = TRUE),
    max_year = max(year_num, na.rm = TRUE)
  ) %>%
  print()

# ------------------------------------------------
# 14. Prepare yearly mean family vote shares, 1965–2019
# ------------------------------------------------
family_levels <- c(
  "Far Left",
  "Agrarian",
  "Liberal",
  "Green",
  "Social Democratic",
  "Christian Democratic",
  "Conservative",
  "Far Right"
)

colors <- c(
  "Far Left" = "#8b2f2f",
  "Agrarian" = "#8c6d1f",
  "Liberal" = "#d4c900",
  "Green" = "green4",
  "Social Democratic" = "red",
  "Christian Democratic" = "#5c88da",
  "Conservative" = "blue",
  "Far Right" = "purple"
)

df_plot <- parlgov_vote_shares_long %>%
  dplyr::mutate(year_num = as.integer(year)) %>%
  dplyr::filter(
    year_num >= 1965,
    year_num <= 2019,
    family_name_short %in% c("com", "lef", "agr", "lib", "eco", "soc", "chr", "con", "right", "nat")
  ) %>%
  dplyr::mutate(
    family = dplyr::case_when(
      family_name_short %in% c("com", "lef")   ~ "Far Left",
      family_name_short == "agr"               ~ "Agrarian",
      family_name_short == "lib"               ~ "Liberal",
      family_name_short == "eco"               ~ "Green",
      family_name_short == "soc"               ~ "Social Democratic",
      family_name_short == "chr"               ~ "Christian Democratic",
      family_name_short == "con"               ~ "Conservative",
      family_name_short %in% c("right", "nat") ~ "Far Right",
      TRUE                                     ~ NA_character_
    ),
    family = factor(family, levels = family_levels)
  ) %>%
  dplyr::filter(!is.na(family)) %>%
  dplyr::group_by(year_num, family) %>%
  dplyr::summarise(
    mean_vote_share = mean(vote_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(family, year_num)

# ------------------------------------------------
# 15. Plot LOWESS-smoothed yearly means
# ------------------------------------------------
p <- ggplot(
  df_plot,
  aes(x = year_num, y = mean_vote_share, colour = family)
) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.95,
    se = FALSE,
    linewidth = 1.1
  ) +
  scale_colour_manual(
    values = colors,
    breaks = family_levels
  ) +
  scale_x_continuous(
    breaks = c(seq(1965, 2015, 5), 2019)
  ) +
  scale_y_continuous(
    breaks = seq(0, 35, 5)
  ) +
  coord_cartesian(
    xlim = c(1965, 2019),
    ylim = c(0, 35)
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Vote share (in %)",
    colour = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.4
    ),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.key.width = grid::unit(2.0, "lines"),
    legend.spacing.x = grid::unit(0.5, "lines"),
    legend.text = element_text(size = 11),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "#e6eff2", linewidth = 0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(
    colour = guide_legend(
      nrow = 2,
      ncol = 4,
      byrow = TRUE
    )
  )

print(p)