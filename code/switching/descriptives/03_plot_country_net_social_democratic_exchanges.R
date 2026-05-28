# ================================================================
# 03_plot_country_net_social_democratic_exchanges.R
# Selected country-level net exchanges with social-democratic parties
# Social-democratic vote-switching project
#
# This script plots average election-level net voter exchanges between
# social-democratic parties and selected national competitors in
# Austria, Germany, the Netherlands, and Denmark.
#
# Net exchange is defined as:
#   weighted inflow to social democracy from party j
#   minus
#   weighted outflow from social democracy to party j.
#
# Values are expressed as percentage points of the electorate and then
# averaged across elections in each country.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching"

analysis_dir <- file.path(project_dir, "data", "analysis")

output_dir <- file.path(
  analysis_dir,
  "descriptives",
  "social_democratic_exchanges"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_transitions_primary <- file.path(
  analysis_dir,
  "df_realised_transitions_primary_social_democratic.rds"
)

required_inputs <- c(path_transitions_primary)

names(required_inputs) <- c("primary realised transitions")

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

transitions_primary <- readRDS(path_transitions_primary)

stopifnot(is.data.frame(transitions_primary), nrow(transitions_primary) > 0)

required_transition_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "id",
  "vote",
  "l_vote",
  "weights"
)

missing_transition_vars <- setdiff(required_transition_vars, names(transitions_primary))

if (length(missing_transition_vars) > 0) {
  stop(
    "Missing variables in transitions_primary: ",
    paste(missing_transition_vars, collapse = ", ")
  )
}

# ------------------------------------------------
# 3. Country and party labels
# ------------------------------------------------

target_countries <- c("AT", "DE", "NL", "DK")

country_labels <- c(
  "AT" = "Austria",
  "DE" = "Germany",
  "NL" = "Netherlands",
  "DK" = "Denmark"
)

make_party_label <- function(country, party_name) {
  label <- party_name

  label <- dplyr::case_when(
    party_name == "non-voters" ~ "Non-vote",

    country == "AT" & party_name == "Austrian People's Party" ~ "OVP",
    country == "AT" & party_name == "Freedom Party of Austria" ~ "FPO",
    country == "AT" & party_name == "The Greens" ~ "Greens",
    country == "AT" & grepl("^JETZT", party_name) ~ "JETZT",
    country == "AT" & party_name == "Communist Party of Austria" ~ "KPO",

    country == "DE" & party_name == "Christian Democratic Union/Christian Social Union" ~ "CDU/CSU",
    country == "DE" & party_name == "Free Democratic Party" ~ "FDP",
    country == "DE" & party_name %in% c("The Greens", "Greens/Alliance'90") ~ "Greens",
    country == "DE" & party_name %in% c(
      "Party of Democratic Socialism",
      "The Left. Party of Democratic Socialism",
      "The Left"
    ) ~ "The Left",
    country == "DE" & party_name == "Alternative for Germany" ~ "AfD",

    country == "NL" & party_name == "People's Party for Freedom and Democracy" ~ "VVD",
    country == "NL" & party_name == "Democrats'66" ~ "D66",
    country == "NL" & party_name == "Christian Democratic Appeal" ~ "CDA",
    country == "NL" & party_name == "Green Left" ~ "GL",
    country == "NL" & party_name == "Socialist Party" ~ "SP",
    country == "NL" & party_name == "Party of Freedom" ~ "PVV",
    country == "NL" & party_name == "Forum for Democracy" ~ "FvD",
    country == "NL" & party_name == "Party for the Animals" ~ "PvdD",

    country == "DK" & party_name == "Socialist People's Party" ~ "Socialist PP",
    country == "DK" & party_name == "Danish People's Party" ~ "People's Party",
    country == "DK" & party_name == "Conservative People's Party" ~ "Conservatives",
    country == "DK" & party_name == "Progress Party" ~ "Progress",
    country == "DK" & party_name %in% c(
      "Christian People's Party",
      "Christian Democrats"
    ) ~ "Christian Democrats",

    TRUE ~ label
  )

  label
}

selected_parties <- tibble::tribble(
  ~iso2c_file, ~party_label, ~party_order,
  "AT", "FPO", 1,
  "AT", "Greens", 2,
  "AT", "NEOS", 3,
  "AT", "Non-vote", 4,
  "AT", "OVP", 5,
  "DE", "Non-vote", 1,
  "DE", "The Left", 2,
  "DE", "Greens", 3,
  "DE", "FDP", 4,
  "DE", "AfD", 5,
  "DE", "CDU/CSU", 6,
  "NL", "Non-vote", 1,
  "NL", "D66", 2,
  "NL", "SP", 3,
  "NL", "GL", 4,
  "NL", "VVD", 5,
  "NL", "PVV", 6,
  "NL", "CDA", 7,
  "DK", "Non-vote", 1,
  "DK", "Socialist PP", 2,
  "DK", "People's Party", 3,
  "DK", "Conservatives", 4,
  "DK", "Progress", 5,
  "DK", "Christian Democrats", 6
) %>%
  mutate(
    country_label = country_labels[iso2c_file]
  )

# ------------------------------------------------
# 4. Prepare transition data
# ------------------------------------------------

transitions_clean <- transitions_primary %>%
  mutate(
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    year = as.integer(year),
    id = as.character(id),
    election_id = paste(iso2c_file, elec_id, sep = "__"),
    vote_alt_id = as.character(vote),
    lag_alt_id = as.character(l_vote),
    weights = if_else(is.na(weights), 1, as.numeric(weights))
  ) %>%
  filter(
    iso2c_file %in% target_countries,
    !is.na(elec_id),
    !is.na(year),
    !is.na(id),
    !is.na(weights),
    weights > 0
  ) %>%
  distinct(iso2c_file, elec_id, year, id, .keep_all = TRUE)

lag_party_lookup <- transitions_clean %>%
  filter(
    !is.na(vote_alt_id),
    !is.na(party_label_best),
    !is.na(party_bloc_detailed)
  ) %>%
  transmute(
    iso2c_file,
    elec_id,
    year,
    lag_alt_id = vote_alt_id,
    from_party_name_lookup = as.character(party_label_best),
    from_party_bloc_lookup = as.character(party_bloc_detailed)
  ) %>%
  distinct(iso2c_file, elec_id, year, lag_alt_id, .keep_all = TRUE)

transitions_with_parties <- transitions_clean %>%
  left_join(
    lag_party_lookup,
    by = c("iso2c_file", "elec_id", "year", "lag_alt_id")
  ) %>%
  mutate(
    to_party_name = as.character(party_label_best),
    to_party_bloc = as.character(party_bloc_detailed),
    from_party_name = coalesce(
      from_party_name_lookup,
      if_else(
        switch_from_bloc_detailed == "non",
        "non-voters",
        NA_character_
      )
    ),
    from_party_bloc = coalesce(
      from_party_bloc_lookup,
      as.character(switch_from_bloc_detailed)
    )
  ) %>%
  filter(
    !is.na(to_party_name),
    !is.na(from_party_name),
    !is.na(to_party_bloc),
    !is.na(from_party_bloc)
  )

# ------------------------------------------------
# 5. Election-level net exchanges
# ------------------------------------------------

election_totals <- transitions_with_parties %>%
  group_by(iso2c_file, elec_id, year, election_id) %>%
  summarise(
    election_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

outward_election_flows <- transitions_with_parties %>%
  filter(
    from_party_bloc == "social_democratic",
    to_party_bloc != "social_democratic"
  ) %>%
  mutate(
    party_label = make_party_label(iso2c_file, to_party_name)
  ) %>%
  group_by(iso2c_file, elec_id, year, election_id, party_label) %>%
  summarise(
    outward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

inward_election_flows <- transitions_with_parties %>%
  filter(
    to_party_bloc == "social_democratic",
    from_party_bloc != "social_democratic"
  ) %>%
  mutate(
    party_label = make_party_label(iso2c_file, from_party_name)
  ) %>%
  group_by(iso2c_file, elec_id, year, election_id, party_label) %>%
  summarise(
    inward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

election_party_grid <- election_totals %>%
  left_join(
    selected_parties %>%
      select(iso2c_file, party_label),
    by = "iso2c_file"
  )

election_net_exchanges <- election_party_grid %>%
  left_join(
    outward_election_flows,
    by = c("iso2c_file", "elec_id", "year", "election_id", "party_label")
  ) %>%
  left_join(
    inward_election_flows,
    by = c("iso2c_file", "elec_id", "year", "election_id", "party_label")
  ) %>%
  mutate(
    outward_weighted_n = replace_na(outward_weighted_n, 0),
    inward_weighted_n = replace_na(inward_weighted_n, 0),
    net_weighted_n = inward_weighted_n - outward_weighted_n,
    outward_pct = 100 * outward_weighted_n / election_weighted_n,
    inward_pct = 100 * inward_weighted_n / election_weighted_n,
    net_pct = 100 * net_weighted_n / election_weighted_n
  )

net_exchanges <- election_net_exchanges %>%
  group_by(iso2c_file, party_label) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    n_elections = n_distinct(election_id),
    outward_pct = mean(outward_pct, na.rm = TRUE),
    inward_pct = mean(inward_pct, na.rm = TRUE),
    net_pct = mean(net_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(selected_parties, by = c("iso2c_file", "party_label")) %>%
  mutate(
    country_panel = paste0(
      country_label,
      " (",
      first_year,
      "-",
      last_year,
      ")"
    )
  ) %>%
  arrange(iso2c_file, party_order)

readr::write_csv(
  election_net_exchanges,
  file.path(output_dir, "country_election_net_social_democratic_exchanges.csv")
)

readr::write_csv(
  net_exchanges,
  file.path(output_dir, "country_net_social_democratic_exchanges.csv")
)

saveRDS(
  net_exchanges,
  file.path(output_dir, "country_net_social_democratic_exchanges.rds")
)

# ------------------------------------------------
# 6. Plot
# ------------------------------------------------

panel_levels <- net_exchanges %>%
  distinct(iso2c_file, country_panel) %>%
  arrange(match(iso2c_file, target_countries)) %>%
  pull(country_panel)

party_levels <- net_exchanges %>%
  arrange(match(iso2c_file, target_countries), desc(party_order)) %>%
  mutate(party_panel_label = paste(party_label, country_panel, sep = "__")) %>%
  pull(party_panel_label)

plot_data <- net_exchanges %>%
  mutate(
    country_panel = factor(country_panel, levels = panel_levels),
    party_panel_label = paste(party_label, country_panel, sep = "__"),
    party_panel_label = factor(party_panel_label, levels = party_levels)
  )

max_abs_net <- max(abs(plot_data$net_pct), na.rm = TRUE)
x_limit <- max(0.8, ceiling(max_abs_net * 10) / 10)

p <- ggplot(
  plot_data,
  aes(x = net_pct, y = party_panel_label)
) +
  geom_col(
    aes(fill = net_pct >= 0),
    width = 0.72,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#9ccc9c",
      "FALSE" = "#ee9a9a"
    )
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "grey25") +
  scale_y_discrete(
    labels = function(x) sub("__.*$", "", x)
  ) +
  coord_cartesian(
    xlim = c(-x_limit, x_limit),
    clip = "off"
  ) +
  facet_wrap(
    ~ country_panel,
    ncol = 2,
    scales = "free_y"
  ) +
  labs(
    x = "Net exchange with social democrats (percentage points)",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )

ggsave(
  filename = file.path(output_dir, "figure_country_net_social_democratic_exchanges.png"),
  plot = p,
  width = 6.6,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "figure_country_net_social_democratic_exchanges.pdf"),
  plot = p,
  width = 6.6,
  height = 5
)

message("Saved country net exchange plot and tables to: ", output_dir)
