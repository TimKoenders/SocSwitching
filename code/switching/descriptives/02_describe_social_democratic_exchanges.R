# ================================================================
# 02_describe_social_democratic_exchanges.R
# Descriptive social-democratic voter exchanges
# Social-democratic vote-switching project
#
# This script describes realised voter exchanges between social
# democratic parties and each competitor category.
#
# It uses the realised transition datasets created in
# 02_prepare_realised_transition_datasets.R and does not estimate
# any model.
#
# Main quantities:
#   Outward flows:
#     realised losses from social democracy to competitor k.
#
#   Inward flows:
#     realised gains to social democracy from competitor k.
#
#   Net balances:
#     inward flow minus outward flow.
#
#   Transfer volumes:
#     inward flow plus outward flow.
#
# The main descriptive figure reports averages of election-level
# weighted percentages. Therefore, the plotted values can be read as
# average percentage-point exchanges per election.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(ggplot2)
  library(forcats)
  library(patchwork)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

analysis_dir <- file.path(project_dir, "data", "analysis")

output_dir <- file.path(
  analysis_dir,
  "descriptives",
  "social_democratic_exchanges"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_outward_sd <- file.path(
  analysis_dir,
  "df_outward_social_democratic.rds"
)

path_inward_sd <- file.path(
  analysis_dir,
  "df_inward_social_democratic.rds"
)

path_transitions_primary <- file.path(
  analysis_dir,
  "df_realised_transitions_primary_social_democratic.rds"
)

required_inputs <- c(
  path_outward_sd,
  path_inward_sd,
  path_transitions_primary
)

names(required_inputs) <- c(
  "outward social-democratic transitions",
  "inward social-democratic transitions",
  "primary realised transitions"
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "The following input files were not found:\n",
    paste(names(missing_inputs), missing_inputs, sep = ": ", collapse = "\n")
  )
}

# ------------------------------------------------
# 2. Load data
# ------------------------------------------------

outward_sd <- readRDS(path_outward_sd)
inward_sd <- readRDS(path_inward_sd)
transitions_primary <- readRDS(path_transitions_primary)

stopifnot(is.data.frame(outward_sd), nrow(outward_sd) > 0)
stopifnot(is.data.frame(inward_sd), nrow(inward_sd) > 0)
stopifnot(is.data.frame(transitions_primary), nrow(transitions_primary) > 0)

required_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "id",
  "outcome"
)

missing_out <- setdiff(required_vars, names(outward_sd))
missing_in <- setdiff(required_vars, names(inward_sd))

if (length(missing_out) > 0) {
  stop("Missing variables in outward_sd: ", paste(missing_out, collapse = ", "))
}

if (length(missing_in) > 0) {
  stop("Missing variables in inward_sd: ", paste(missing_in, collapse = ", "))
}

if (!"weights" %in% names(outward_sd)) {
  outward_sd <- outward_sd %>%
    mutate(weights = 1)
}

if (!"weights" %in% names(inward_sd)) {
  inward_sd <- inward_sd %>%
    mutate(weights = 1)
}

# ------------------------------------------------
# 3. Competitor map
# ------------------------------------------------

competitor_map <- tibble::tibble(
  competitor = c(
    "far_left",
    "green",
    "mainstream_right",
    "far_right",
    "non"
  ),
  competitor_label = c(
    "Far left",
    "Green",
    "Mainstream right",
    "Far right",
    "Non-voting"
  ),
  outward_outcome = c(
    "to_far_left",
    "to_green",
    "to_mainstream_right",
    "to_far_right",
    "to_non"
  ),
  inward_outcome = c(
    "from_far_left",
    "from_green",
    "from_mainstream_right",
    "from_far_right",
    "from_non"
  )
)

competitor_order <- c(
  "far_left",
  "green",
  "mainstream_right",
  "far_right",
  "non"
)

competitor_label_order <- competitor_map$competitor_label[
  match(competitor_order, competitor_map$competitor)
]

# ------------------------------------------------
# 4. Clean transition datasets
# ------------------------------------------------

outward_sd_clean <- outward_sd %>%
  mutate(
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    year = as.integer(year),
    election_id = paste(iso2c_file, elec_id, sep = "__"),
    respondent_election_id = paste(iso2c_file, elec_id, id, sep = "__"),
    outcome = as.character(outcome),
    weights = if_else(is.na(weights), 1, as.numeric(weights))
  ) %>%
  filter(
    !is.na(iso2c_file),
    !is.na(elec_id),
    !is.na(year),
    !is.na(id),
    !is.na(outcome),
    !is.na(weights),
    weights > 0
  )

inward_sd_clean <- inward_sd %>%
  mutate(
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    year = as.integer(year),
    election_id = paste(iso2c_file, elec_id, sep = "__"),
    respondent_election_id = paste(iso2c_file, elec_id, id, sep = "__"),
    outcome = as.character(outcome),
    weights = if_else(is.na(weights), 1, as.numeric(weights))
  ) %>%
  filter(
    !is.na(iso2c_file),
    !is.na(elec_id),
    !is.na(year),
    !is.na(id),
    !is.na(outcome),
    !is.na(weights),
    weights > 0
  )

# ------------------------------------------------
# 5. Risk-set sizes
# ------------------------------------------------

sd_risk_set_sizes <- outward_sd_clean %>%
  distinct(
    iso2c_file,
    elec_id,
    year,
    election_id,
    respondent_election_id,
    weights
  ) %>%
  group_by(iso2c_file, elec_id, year, election_id) %>%
  summarise(
    weighted_n_previous_sd = sum(weights, na.rm = TRUE),
    n_previous_sd = n(),
    .groups = "drop"
  )

non_sd_risk_set_sizes <- inward_sd_clean %>%
  distinct(
    iso2c_file,
    elec_id,
    year,
    election_id,
    respondent_election_id,
    weights
  ) %>%
  group_by(iso2c_file, elec_id, year, election_id) %>%
  summarise(
    weighted_n_previous_non_sd = sum(weights, na.rm = TRUE),
    n_previous_non_sd = n(),
    .groups = "drop"
  )

risk_set_sizes <- sd_risk_set_sizes %>%
  full_join(
    non_sd_risk_set_sizes,
    by = c("iso2c_file", "elec_id", "year", "election_id")
  ) %>%
  mutate(
    weighted_n_previous_sd = replace_na(weighted_n_previous_sd, 0),
    weighted_n_previous_non_sd = replace_na(weighted_n_previous_non_sd, 0),
    n_previous_sd = replace_na(n_previous_sd, 0),
    n_previous_non_sd = replace_na(n_previous_non_sd, 0),
    weighted_n_total = weighted_n_previous_sd + weighted_n_previous_non_sd,
    n_total = n_previous_sd + n_previous_non_sd,
    sd_risk_set_share = if_else(
      weighted_n_total > 0,
      weighted_n_previous_sd / weighted_n_total,
      NA_real_
    ),
    non_sd_risk_set_share = if_else(
      weighted_n_total > 0,
      weighted_n_previous_non_sd / weighted_n_total,
      NA_real_
    )
  )

# ------------------------------------------------
# 6. Outward flows
# ------------------------------------------------

outward_flows <- outward_sd_clean %>%
  filter(outcome != "retention") %>%
  group_by(
    iso2c_file,
    elec_id,
    year,
    election_id,
    outcome
  ) %>%
  summarise(
    outward_n = n(),
    outward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(outward_outcome = outcome) %>%
  left_join(
    competitor_map,
    by = "outward_outcome"
  ) %>%
  filter(!is.na(competitor))

# ------------------------------------------------
# 7. Inward flows
# ------------------------------------------------

inward_flows <- inward_sd_clean %>%
  filter(outcome != "not_to_sd") %>%
  group_by(
    iso2c_file,
    elec_id,
    year,
    election_id,
    outcome
  ) %>%
  summarise(
    inward_n = n(),
    inward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(inward_outcome = outcome) %>%
  left_join(
    competitor_map,
    by = "inward_outcome"
  ) %>%
  filter(!is.na(competitor))

# ------------------------------------------------
# 8. Election-competitor exchange table
# ------------------------------------------------

election_competitor_grid <- risk_set_sizes %>%
  select(iso2c_file, elec_id, year, election_id) %>%
  distinct() %>%
  tidyr::crossing(competitor_map)

sd_exchange_election_competitor <- election_competitor_grid %>%
  left_join(
    outward_flows %>%
      select(
        iso2c_file,
        elec_id,
        year,
        election_id,
        competitor,
        outward_n,
        outward_weighted_n
      ),
    by = c("iso2c_file", "elec_id", "year", "election_id", "competitor")
  ) %>%
  left_join(
    inward_flows %>%
      select(
        iso2c_file,
        elec_id,
        year,
        election_id,
        competitor,
        inward_n,
        inward_weighted_n
      ),
    by = c("iso2c_file", "elec_id", "year", "election_id", "competitor")
  ) %>%
  left_join(
    risk_set_sizes,
    by = c("iso2c_file", "elec_id", "year", "election_id")
  ) %>%
  mutate(
    outward_n = replace_na(outward_n, 0),
    inward_n = replace_na(inward_n, 0),
    outward_weighted_n = replace_na(outward_weighted_n, 0),
    inward_weighted_n = replace_na(inward_weighted_n, 0),
    
    net_n = inward_n - outward_n,
    net_weighted_n = inward_weighted_n - outward_weighted_n,
    
    transfer_volume_n = inward_n + outward_n,
    transfer_volume_weighted_n = inward_weighted_n + outward_weighted_n,
    
    outward_share_previous_sd = if_else(
      weighted_n_previous_sd > 0,
      outward_weighted_n / weighted_n_previous_sd,
      NA_real_
    ),
    inward_share_previous_non_sd = if_else(
      weighted_n_previous_non_sd > 0,
      inward_weighted_n / weighted_n_previous_non_sd,
      NA_real_
    ),
    outward_share_total = if_else(
      weighted_n_total > 0,
      outward_weighted_n / weighted_n_total,
      NA_real_
    ),
    inward_share_total = if_else(
      weighted_n_total > 0,
      inward_weighted_n / weighted_n_total,
      NA_real_
    ),
    net_share_total = if_else(
      weighted_n_total > 0,
      net_weighted_n / weighted_n_total,
      NA_real_
    ),
    transfer_volume_share_total = if_else(
      weighted_n_total > 0,
      transfer_volume_weighted_n / weighted_n_total,
      NA_real_
    )
  ) %>%
  arrange(iso2c_file, year, elec_id, competitor)

# ------------------------------------------------
# 9. Election-level total SD exchanges
# ------------------------------------------------

sd_exchange_election_total <- sd_exchange_election_competitor %>%
  group_by(
    iso2c_file,
    elec_id,
    year,
    election_id,
    weighted_n_previous_sd,
    weighted_n_previous_non_sd,
    weighted_n_total,
    n_previous_sd,
    n_previous_non_sd,
    n_total
  ) %>%
  summarise(
    outward_n = sum(outward_n, na.rm = TRUE),
    inward_n = sum(inward_n, na.rm = TRUE),
    outward_weighted_n = sum(outward_weighted_n, na.rm = TRUE),
    inward_weighted_n = sum(inward_weighted_n, na.rm = TRUE),
    net_n = sum(net_n, na.rm = TRUE),
    net_weighted_n = sum(net_weighted_n, na.rm = TRUE),
    transfer_volume_n = sum(transfer_volume_n, na.rm = TRUE),
    transfer_volume_weighted_n = sum(transfer_volume_weighted_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    outward_share_previous_sd = if_else(
      weighted_n_previous_sd > 0,
      outward_weighted_n / weighted_n_previous_sd,
      NA_real_
    ),
    inward_share_previous_non_sd = if_else(
      weighted_n_previous_non_sd > 0,
      inward_weighted_n / weighted_n_previous_non_sd,
      NA_real_
    ),
    outward_share_total = if_else(
      weighted_n_total > 0,
      outward_weighted_n / weighted_n_total,
      NA_real_
    ),
    inward_share_total = if_else(
      weighted_n_total > 0,
      inward_weighted_n / weighted_n_total,
      NA_real_
    ),
    net_share_total = if_else(
      weighted_n_total > 0,
      net_weighted_n / weighted_n_total,
      NA_real_
    ),
    transfer_volume_share_total = if_else(
      weighted_n_total > 0,
      transfer_volume_weighted_n / weighted_n_total,
      NA_real_
    )
  ) %>%
  arrange(iso2c_file, year, elec_id)

# ------------------------------------------------
# 10. Pooled totals across all observations
# ------------------------------------------------

sd_exchange_competitor_pooled_totals <- sd_exchange_election_competitor %>%
  group_by(
    competitor,
    competitor_label,
    outward_outcome,
    inward_outcome
  ) %>%
  summarise(
    outward_n = sum(outward_n, na.rm = TRUE),
    inward_n = sum(inward_n, na.rm = TRUE),
    outward_weighted_n = sum(outward_weighted_n, na.rm = TRUE),
    inward_weighted_n = sum(inward_weighted_n, na.rm = TRUE),
    net_n = sum(net_n, na.rm = TRUE),
    net_weighted_n = sum(net_weighted_n, na.rm = TRUE),
    transfer_volume_n = sum(transfer_volume_n, na.rm = TRUE),
    transfer_volume_weighted_n = sum(transfer_volume_weighted_n, na.rm = TRUE),
    weighted_n_previous_sd = sum(weighted_n_previous_sd, na.rm = TRUE),
    weighted_n_previous_non_sd = sum(weighted_n_previous_non_sd, na.rm = TRUE),
    weighted_n_total = sum(weighted_n_total, na.rm = TRUE),
    n_elections = n_distinct(election_id),
    n_countries = n_distinct(iso2c_file),
    .groups = "drop"
  ) %>%
  mutate(
    outward_share_previous_sd = outward_weighted_n / weighted_n_previous_sd,
    inward_share_previous_non_sd = inward_weighted_n / weighted_n_previous_non_sd,
    outward_share_total = outward_weighted_n / weighted_n_total,
    inward_share_total = inward_weighted_n / weighted_n_total,
    net_share_total = net_weighted_n / weighted_n_total,
    transfer_volume_share_total = transfer_volume_weighted_n / weighted_n_total
  ) %>%
  arrange(desc(transfer_volume_weighted_n))

# ------------------------------------------------
# 11. Average election-level percentages by competitor
# ------------------------------------------------

sd_exchange_competitor_average_election <- sd_exchange_election_competitor %>%
  group_by(
    competitor,
    competitor_label,
    outward_outcome,
    inward_outcome
  ) %>%
  summarise(
    mean_outward_share_total = mean(outward_share_total, na.rm = TRUE),
    mean_inward_share_total = mean(inward_share_total, na.rm = TRUE),
    mean_net_share_total = mean(net_share_total, na.rm = TRUE),
    mean_transfer_volume_share_total = mean(
      transfer_volume_share_total,
      na.rm = TRUE
    ),
    
    median_outward_share_total = median(outward_share_total, na.rm = TRUE),
    median_inward_share_total = median(inward_share_total, na.rm = TRUE),
    median_net_share_total = median(net_share_total, na.rm = TRUE),
    median_transfer_volume_share_total = median(
      transfer_volume_share_total,
      na.rm = TRUE
    ),
    
    sd_outward_share_total = sd(outward_share_total, na.rm = TRUE),
    sd_inward_share_total = sd(inward_share_total, na.rm = TRUE),
    sd_net_share_total = sd(net_share_total, na.rm = TRUE),
    sd_transfer_volume_share_total = sd(
      transfer_volume_share_total,
      na.rm = TRUE
    ),
    
    min_net_share_total = min(net_share_total, na.rm = TRUE),
    max_net_share_total = max(net_share_total, na.rm = TRUE),
    
    outward_weighted_n = sum(outward_weighted_n, na.rm = TRUE),
    inward_weighted_n = sum(inward_weighted_n, na.rm = TRUE),
    net_weighted_n = sum(net_weighted_n, na.rm = TRUE),
    transfer_volume_weighted_n = sum(
      transfer_volume_weighted_n,
      na.rm = TRUE
    ),
    
    n_elections = n_distinct(election_id),
    n_countries = n_distinct(iso2c_file),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_transfer_volume_share_total))

# ------------------------------------------------
# 12. Country-competitor summaries
# ------------------------------------------------

sd_exchange_country_competitor <- sd_exchange_election_competitor %>%
  group_by(
    iso2c_file,
    competitor,
    competitor_label,
    outward_outcome,
    inward_outcome
  ) %>%
  summarise(
    outward_n = sum(outward_n, na.rm = TRUE),
    inward_n = sum(inward_n, na.rm = TRUE),
    outward_weighted_n = sum(outward_weighted_n, na.rm = TRUE),
    inward_weighted_n = sum(inward_weighted_n, na.rm = TRUE),
    net_n = sum(net_n, na.rm = TRUE),
    net_weighted_n = sum(net_weighted_n, na.rm = TRUE),
    transfer_volume_n = sum(transfer_volume_n, na.rm = TRUE),
    transfer_volume_weighted_n = sum(transfer_volume_weighted_n, na.rm = TRUE),
    weighted_n_previous_sd = sum(weighted_n_previous_sd, na.rm = TRUE),
    weighted_n_previous_non_sd = sum(weighted_n_previous_non_sd, na.rm = TRUE),
    weighted_n_total = sum(weighted_n_total, na.rm = TRUE),
    n_elections = n_distinct(election_id),
    .groups = "drop"
  ) %>%
  mutate(
    outward_share_previous_sd = outward_weighted_n / weighted_n_previous_sd,
    inward_share_previous_non_sd = inward_weighted_n / weighted_n_previous_non_sd,
    outward_share_total = outward_weighted_n / weighted_n_total,
    inward_share_total = inward_weighted_n / weighted_n_total,
    net_share_total = net_weighted_n / weighted_n_total,
    transfer_volume_share_total = transfer_volume_weighted_n / weighted_n_total
  ) %>%
  arrange(iso2c_file, desc(transfer_volume_weighted_n))

# ------------------------------------------------
# 13. Flow support diagnostics
# ------------------------------------------------

outward_support <- outward_sd_clean %>%
  count(outcome, name = "n") %>%
  mutate(
    share = n / sum(n),
    flow = "outward"
  )

inward_support <- inward_sd_clean %>%
  count(outcome, name = "n") %>%
  mutate(
    share = n / sum(n),
    flow = "inward"
  )

flow_support <- bind_rows(
  outward_support,
  inward_support
) %>%
  relocate(flow, .before = outcome)

retention_diagnostics <- outward_sd_clean %>%
  group_by(iso2c_file, elec_id, year, election_id) %>%
  summarise(
    retention_weighted_n = sum(weights[outcome == "retention"], na.rm = TRUE),
    outward_weighted_n = sum(weights[outcome != "retention"], na.rm = TRUE),
    previous_sd_weighted_n = sum(weights, na.rm = TRUE),
    retention_share_previous_sd = retention_weighted_n / previous_sd_weighted_n,
    outward_share_previous_sd = outward_weighted_n / previous_sd_weighted_n,
    .groups = "drop"
  )

not_to_sd_diagnostics <- inward_sd_clean %>%
  group_by(iso2c_file, elec_id, year, election_id) %>%
  summarise(
    not_to_sd_weighted_n = sum(weights[outcome == "not_to_sd"], na.rm = TRUE),
    inward_weighted_n = sum(weights[outcome != "not_to_sd"], na.rm = TRUE),
    previous_non_sd_weighted_n = sum(weights, na.rm = TRUE),
    not_to_sd_share_previous_non_sd =
      not_to_sd_weighted_n / previous_non_sd_weighted_n,
    inward_share_previous_non_sd =
      inward_weighted_n / previous_non_sd_weighted_n,
    .groups = "drop"
  )

flow_diagnostics <- retention_diagnostics %>%
  full_join(
    not_to_sd_diagnostics,
    by = c("iso2c_file", "elec_id", "year", "election_id")
  ) %>%
  left_join(
    sd_exchange_election_total %>%
      select(
        iso2c_file,
        elec_id,
        year,
        election_id,
        total_outward_weighted_n = outward_weighted_n,
        total_inward_weighted_n = inward_weighted_n,
        total_net_weighted_n = net_weighted_n,
        total_transfer_volume_weighted_n = transfer_volume_weighted_n,
        total_net_share_total = net_share_total,
        total_transfer_volume_share_total = transfer_volume_share_total
      ),
    by = c("iso2c_file", "elec_id", "year", "election_id")
  ) %>%
  arrange(iso2c_file, year, elec_id)

# ------------------------------------------------
# 14. Four-panel plot based on average election-level percentages
# ------------------------------------------------

plot_sd_exchange_average_election <- function(
    data,
    sd_label = "social democrats",
    save_plot = TRUE,
    output_dir = output_dir
) {
  
  df_plot <- data %>%
    filter(competitor != "non") %>%
    mutate(
      competitor = factor(
        competitor,
        levels = setdiff(competitor_order, "non")
      ),
      competitor_label = factor(
        competitor_label,
        levels = competitor_label_order[
          competitor_label_order != "Non-voting"
        ]
      ),
      outflow_pct = 100 * mean_outward_share_total,
      inflow_pct = 100 * mean_inward_share_total,
      net_pct = 100 * mean_net_share_total,
      transfer_pct = 100 * mean_transfer_volume_share_total
    )
  
  max_flow <- max(
    c(df_plot$outflow_pct, df_plot$inflow_pct, df_plot$transfer_pct),
    na.rm = TRUE
  )
  
  if (!is.finite(max_flow) || max_flow == 0) {
    max_flow <- 1
  }
  
  net_lim <- max(abs(df_plot$net_pct), na.rm = TRUE)
  
  if (!is.finite(net_lim) || net_lim == 0) {
    net_lim <- 1
  }
  
  p_out <- df_plot %>%
    mutate(
      competitor_label = forcats::fct_reorder(competitor_label, outflow_pct)
    ) %>%
    ggplot(aes(x = outflow_pct, y = competitor_label)) +
    geom_col(fill = "#ee9a9a", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f", outflow_pct)),
      hjust = -0.15,
      size = 3
    ) +
    scale_x_continuous(
      limits = c(0, max_flow * 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0("A. Average switching rates to ", sd_label),
      x = "Percentage points",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  p_in <- df_plot %>%
    mutate(
      competitor_label = forcats::fct_reorder(competitor_label, inflow_pct)
    ) %>%
    ggplot(aes(x = inflow_pct, y = competitor_label)) +
    geom_col(fill = "#9ccc9c", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f", inflow_pct)),
      hjust = -0.15,
      size = 3
    ) +
    scale_x_continuous(
      limits = c(0, max_flow * 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0("B. Average switching rates from ", sd_label),
      x = "Percentage points",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  p_net <- df_plot %>%
    mutate(
      competitor_label = forcats::fct_reorder(competitor_label, abs(net_pct))
    ) %>%
    ggplot(aes(x = net_pct, y = competitor_label)) +
    geom_col(aes(fill = net_pct >= 0), width = 0.7, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        "TRUE" = "#9ccc9c",
        "FALSE" = "#ee9a9a"
      )
    ) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_text(
      aes(
        label = sprintf("%+.1f", net_pct),
        hjust = ifelse(net_pct >= 0, -0.15, 1.15)
      ),
      size = 3
    ) +
    scale_x_continuous(
      limits = c(-net_lim * 1.30, net_lim * 1.30),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = paste0("C. Net exchanges involving ", sd_label),
      x = "Percentage points",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  p_transfer <- df_plot %>%
    mutate(
      competitor_label = forcats::fct_reorder(competitor_label, transfer_pct)
    ) %>%
    ggplot(aes(x = transfer_pct, y = competitor_label)) +
    geom_col(fill = "grey70", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f", transfer_pct)),
      hjust = -0.15,
      size = 3
    ) +
    scale_x_continuous(
      limits = c(0, max_flow * 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0("D. Transfer volumes involving ", sd_label),
      x = "Percentage points",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  p <- (p_out + p_in) / (p_net + p_transfer) +
    plot_annotation(
      title = "",
      caption = "",
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.caption = element_text(hjust = 0, size = 8)
      )
    )
  
  if (save_plot) {
    
    ggsave(
      filename = file.path(output_dir, "descriptives_switching.png"),
      plot = p,
      width = 11,
      height = 8,
      dpi = 300
    )
    
    ggsave(
      filename = file.path(output_dir, "descriptives_switching.pdf"),
      plot = p,
      width = 11,
      height = 8
    )
    
    ggsave(
      filename = file.path(output_dir, "figure_sd_exchanges_average_election.png"),
      plot = p,
      width = 11,
      height = 8,
      dpi = 300
    )
    
    ggsave(
      filename = file.path(output_dir, "figure_sd_exchanges_average_election.pdf"),
      plot = p,
      width = 11,
      height = 8
    )
  }
  
  p
}

switching_plot <- plot_sd_exchange_average_election(
  data = sd_exchange_competitor_average_election,
  sd_label = "social democrats",
  save_plot = TRUE,
  output_dir = output_dir
)

print(switching_plot)

# ------------------------------------------------
# 15. Validation checks
# ------------------------------------------------

stopifnot(all(outward_sd_clean$outcome %in% c(
  "retention",
  "to_far_left",
  "to_green",
  "to_mainstream_right",
  "to_far_right",
  "to_non"
)))

stopifnot(all(inward_sd_clean$outcome %in% c(
  "not_to_sd",
  "from_far_left",
  "from_green",
  "from_mainstream_right",
  "from_far_right",
  "from_non"
)))

stopifnot(all(sd_exchange_election_competitor$outward_weighted_n >= 0))
stopifnot(all(sd_exchange_election_competitor$inward_weighted_n >= 0))
stopifnot(all(sd_exchange_election_competitor$transfer_volume_weighted_n >= 0))

stopifnot(all(!is.na(sd_exchange_competitor_average_election$mean_net_share_total)))
stopifnot(all(!is.na(sd_exchange_competitor_average_election$mean_transfer_volume_share_total)))

