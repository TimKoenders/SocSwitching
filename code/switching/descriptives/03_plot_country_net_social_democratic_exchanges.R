# ================================================================
# 03_plot_country_net_social_democratic_exchanges.R
# Selected country-level net exchanges with social-democratic parties
# Social-democratic vote-switching project
#
# This script plots pooled net voter exchanges between
# social-democratic parties and selected national competitors in
# Austria, Germany, the Netherlands, and Denmark.
#
# Net exchange is defined as:
#   weighted inflow to social democracy from party j
#   minus
#   weighted outflow from social democracy to party j.
#
# Values are expressed as percentage points of the country electorate.
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

path_switching_bundle <- file.path(
  project_dir,
  "data",
  "processed",
  "switching_datasets.RData"
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

switching_bundle_env <- new.env(parent = emptyenv())

if (file.exists(path_switching_bundle)) {
  load(path_switching_bundle, envir = switching_bundle_env)
}

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
    country == "AT" & party_name == "Austrian Social Democratic Party" ~ "SPO",
    country == "AT" & party_name == "Freedom Party of Austria" ~ "FPO",
    country == "AT" & party_name == "Austrian Freedom Party" ~ "FPO",
    country == "AT" & party_name == "The Greens" ~ "Greens",
    country == "AT" & party_name == "The New Austria (NEOS)" ~ "NEOS",
    country == "AT" & grepl("^JETZT", party_name) ~ "JETZT",
    country == "AT" & party_name == "Peter Pilz List" ~ "JETZT",
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
  "AT", "KPO", 4,
  "AT", "OVP", 5,
  "DE", "The Left", 1,
  "DE", "Greens", 2,
  "DE", "FDP", 3,
  "DE", "AfD", 4,
  "DE", "CDU/CSU", 5,
  "NL", "D66", 1,
  "NL", "SP", 2,
  "NL", "GL", 3,
  "NL", "VVD", 4,
  "NL", "PVV", 5,
  "NL", "CDA", 6,
  "DK", "Socialist PP", 1,
  "DK", "People's Party", 2,
  "DK", "Conservatives", 3,
  "DK", "Progress", 4,
  "DK", "Christian Democrats", 5
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

if (exists("df_all", envir = switching_bundle_env)) {
  df_all_bundle <- get("df_all", envir = switching_bundle_env)

  at_historical_source <- df_all_bundle %>%
    mutate(
      iso2c_file = as.character(iso2c_file),
      elec_id = as.character(elec_id),
      year = as.integer(year)
    ) %>%
    filter(
      iso2c_file == "AT",
      year < min(transitions_clean$year[transitions_clean$iso2c_file == "AT"], na.rm = TRUE)
    )

  at_historical_map <- at_historical_source %>%
    transmute(
      elec_id,
      year,
      alt_id = as.character(stack),
      party_name = as.character(party_label_best),
      party_bloc = case_when(
        parfam_harmonized == "soc" ~ "social_democratic",
        parfam_harmonized == "eco" ~ "green",
        parfam_harmonized == "nat" ~ "far_right",
        parfam_harmonized %in% c("chr", "mrp", "con", "lib") ~ "mainstream_right",
        TRUE ~ as.character(parfam_harmonized)
      )
    ) %>%
    filter(!is.na(alt_id), !is.na(party_name), !is.na(party_bloc)) %>%
    distinct(elec_id, year, alt_id, .keep_all = TRUE)

  at_historical_transitions <- at_historical_source %>%
    transmute(
      iso2c_file,
      elec_id,
      year,
      id = as.character(id),
      election_id = paste(iso2c_file, elec_id, sep = "__"),
      vote_alt_id = as.character(vote),
      lag_alt_id = as.character(l_vote),
      weights = if_else(is.na(weights), 1, as.numeric(weights))
    ) %>%
    filter(!is.na(id), !is.na(weights), weights > 0) %>%
    distinct(iso2c_file, elec_id, year, id, .keep_all = TRUE) %>%
    left_join(
      at_historical_map %>%
        rename(
          vote_alt_id = alt_id,
          to_party_name = party_name,
          to_party_bloc = party_bloc
        ),
      by = c("elec_id", "year", "vote_alt_id")
    ) %>%
    left_join(
      at_historical_map %>%
        rename(
          lag_alt_id = alt_id,
          from_party_name = party_name,
          from_party_bloc = party_bloc
        ),
      by = c("elec_id", "year", "lag_alt_id")
    ) %>%
    filter(
      !is.na(to_party_name),
      !is.na(from_party_name),
      !is.na(to_party_bloc),
      !is.na(from_party_bloc)
    )

  transitions_with_parties <- transitions_with_parties %>%
    bind_rows(
      at_historical_transitions %>%
        select(
          iso2c_file,
          elec_id,
          year,
          id,
          election_id,
          weights,
          to_party_name,
          to_party_bloc,
          from_party_name,
          from_party_bloc
        )
    )
}

# ------------------------------------------------
# 5. Pooled country-level net exchanges
# ------------------------------------------------

country_years <- transitions_with_parties %>%
  group_by(iso2c_file) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    country_label = country_labels[iso2c_file],
    country_panel = paste0(
      country_label,
      " (",
      first_year,
      "-",
      last_year,
      ")"
    )
  )

country_dyads <- transitions_with_parties %>%
  mutate(
    from_label = if_else(
      from_party_bloc == "social_democratic",
      "Social democrats",
      make_party_label(iso2c_file, from_party_name)
    ),
    to_label = if_else(
      to_party_bloc == "social_democratic",
      "Social democrats",
      make_party_label(iso2c_file, to_party_name)
    )
  ) %>%
  filter(!is.na(from_label), !is.na(to_label)) %>%
  group_by(iso2c_file, from_label, to_label) %>%
  summarise(
    weights = sum(weights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(iso2c_file) %>%
  mutate(
    p_ij = weights / sum(weights, na.rm = TRUE)
  ) %>%
  ungroup()

losses_sd <- country_dyads %>%
  filter(from_label == "Social democrats", to_label != "Social democrats") %>%
  transmute(
    iso2c_file,
    party_label = to_label,
    outward_pct = 100 * p_ij
  )

gains_sd <- country_dyads %>%
  filter(to_label == "Social democrats", from_label != "Social democrats") %>%
  transmute(
    iso2c_file,
    party_label = from_label,
    inward_pct = 100 * p_ij
  )

country_net_exchanges <- full_join(
  losses_sd,
  gains_sd,
  by = c("iso2c_file", "party_label")
) %>%
  mutate(
    outward_pct = replace_na(outward_pct, 0),
    inward_pct = replace_na(inward_pct, 0),
    net_pct = inward_pct - outward_pct
  ) %>%
  group_by(iso2c_file, party_label) %>%
  summarise(
    outward_pct = sum(outward_pct, na.rm = TRUE),
    inward_pct = sum(inward_pct, na.rm = TRUE),
    net_pct = sum(net_pct, na.rm = TRUE),
    .groups = "drop"
  )

net_exchanges <- country_net_exchanges %>%
  filter(
    party_label != "Non-vote",
    !(iso2c_file == "AT" & party_label %in% c("JETZT")),
    !(iso2c_file == "DE" & party_label %in% c("NPD", "REP")),
    !(iso2c_file == "NL" & party_label %in% c(
      "50Plus", "CD", "Union 55+", "Reformed Political League",
      "Reformatory Political Federation", "LN", "SGP", "PvdD",
      "List Pim Fortuyn", "Christian Union", "FvD"
    )),
    !(iso2c_file == "DK" & party_label %in% c(
      "Alternative", "The New Right", "New Alliance", "Rad. Venstre",
      "Venstre", "Liberal Alliance", "Centre Democrats", "Red-Green",
      "Justice Party", "Common Course", "Independent Greens",
      "Denmark Democrats - Inger Stojberg", "Moderates", "Hard Line",
      "Klaus Riskaer Pedersen List", "Left Socialist Party",
      "Danish Communist Party"
    ))
  ) %>%
  semi_join(
    selected_parties %>% select(iso2c_file, party_label),
    by = c("iso2c_file", "party_label")
  ) %>%
  left_join(
    selected_parties %>%
      select(iso2c_file, party_label, party_order),
    by = c("iso2c_file", "party_label")
  ) %>%
  left_join(
    country_years %>%
      select(iso2c_file, country_label, country_panel),
    by = "iso2c_file"
  ) %>%
  group_by(iso2c_file, party_label) %>%
  summarise(
    country_label = first(country_label),
    country_panel = first(country_panel),
    party_order = first(party_order),
    outward_pct = sum(outward_pct, na.rm = TRUE),
    inward_pct = sum(inward_pct, na.rm = TRUE),
    net_pct = sum(net_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso2c_file, party_order)

readr::write_csv(
  country_net_exchanges,
  file.path(output_dir, "country_all_net_social_democratic_exchanges.csv")
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
  arrange(match(iso2c_file, target_countries), desc(net_pct)) %>%
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
