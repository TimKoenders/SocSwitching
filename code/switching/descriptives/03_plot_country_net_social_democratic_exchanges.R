# ================================================================
# 03_plot_country_net_social_democratic_exchanges.R
# Country-level net exchanges with social-democratic parties
# Social-democratic vote-switching project
#
# This script plots pooled net voter exchanges between social
# democratic parties and national competitors in Austria, the
# Netherlands, Germany, and Denmark.
#
# Net exchange is defined as:
#   weighted inflow to social democracy from party j
#   minus
#   weighted outflow from social democracy to party j.
#
# Values are shown as percentage points of all valid respondents in
# the pooled country sample. Negative values are net losses for social
# democracy, positive values are net gains.
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

path_party_lookup <- file.path(
  analysis_dir,
  "party_lookup_realised_transitions_social_democratic.rds"
)

required_inputs <- c(
  path_transitions_primary,
  path_party_lookup
)

names(required_inputs) <- c(
  "primary realised transitions",
  "party lookup for realised transitions"
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

transitions_primary <- readRDS(path_transitions_primary)
party_lookup <- readRDS(path_party_lookup)

stopifnot(is.data.frame(transitions_primary), nrow(transitions_primary) > 0)
stopifnot(is.data.frame(party_lookup), nrow(party_lookup) > 0)

required_transition_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "id",
  "vote",
  "l_vote",
  "weights"
)

required_lookup_vars <- c(
  "iso2c_file",
  "elec_id",
  "year",
  "alt_id",
  "party_name",
  "party_bloc_detailed"
)

missing_transition_vars <- setdiff(required_transition_vars, names(transitions_primary))
missing_lookup_vars <- setdiff(required_lookup_vars, names(party_lookup))

if (length(missing_transition_vars) > 0) {
  stop(
    "Missing variables in transitions_primary: ",
    paste(missing_transition_vars, collapse = ", ")
  )
}

if (length(missing_lookup_vars) > 0) {
  stop(
    "Missing variables in party_lookup: ",
    paste(missing_lookup_vars, collapse = ", ")
  )
}

# ------------------------------------------------
# 3. Country and party labels
# ------------------------------------------------

target_countries <- c("AT", "NL", "DE", "DK")

country_labels <- c(
  "AT" = "Austria",
  "NL" = "Netherlands",
  "DE" = "Germany",
  "DK" = "Denmark"
)

make_party_label <- function(country, party_name) {
  label <- party_name

  label <- dplyr::case_when(
    party_name == "non-voters" ~ "Non-vote",

    country == "AT" & party_name == "Social Democratic Party of Austria" ~ "SPO",
    country == "AT" & party_name == "Austrian People's Party" ~ "OVP",
    country == "AT" & party_name == "Freedom Party of Austria" ~ "FPO",
    country == "AT" & party_name == "The Greens" ~ "Greens",
    country == "AT" & grepl("^JETZT", party_name) ~ "JETZT",
    country == "AT" & party_name == "Communist Party of Austria" ~ "KPO",

    country == "NL" & party_name == "Labour Party" ~ "PvdA",
    country == "NL" & party_name == "People's Party for Freedom and Democracy" ~ "VVD",
    country == "NL" & party_name == "Democrats'66" ~ "D66",
    country == "NL" & party_name == "Christian Democratic Appeal" ~ "CDA",
    country == "NL" & party_name == "Green Left" ~ "GL",
    country == "NL" & party_name == "Socialist Party" ~ "SP",
    country == "NL" & party_name == "Party of Freedom" ~ "PVV",
    country == "NL" & party_name == "Forum for Democracy" ~ "FvD",
    country == "NL" & party_name == "Party for the Animals" ~ "PvdD",
    country == "NL" & party_name == "Christian Union" ~ "CU",
    country == "NL" & party_name == "Reformed Political Party" ~ "SGP",
    country == "NL" & party_name == "List Pim Fortuyn" ~ "LPF",
    country == "NL" & party_name == "Livable Netherlands" ~ "LN",
    country == "NL" & party_name == "Centre Democrats" ~ "CD",

    country == "DE" & party_name == "Social Democratic Party of Germany" ~ "SPD",
    country == "DE" & party_name == "Christian Democratic Union/Christian Social Union" ~ "CDU",
    country == "DE" & party_name == "Free Democratic Party" ~ "FDP",
    country == "DE" & party_name %in% c("The Greens", "Greens/Alliance'90") ~ "Greens",
    country == "DE" & party_name %in% c(
      "The Left",
      "The Left. Party of Democratic Socialism",
      "Party of Democratic Socialism"
    ) ~ "The Left",
    country == "DE" & party_name == "Alternative for Germany" ~ "AfD",
    country == "DE" & party_name == "The Republicans" ~ "REP",

    country == "DK" & party_name %in% c(
      "Social Democratic Party",
      "Social Democrats"
    ) ~ "Social Dem.",
    country == "DK" & party_name %in% c(
      "Danish Social-Liberal Party",
      "Danish Social Liberal Party"
    ) ~ "Rad. Venstre",
    country == "DK" & party_name %in% c(
      "Liberals",
      "Venstre, Denmark's Liberal Party"
    ) ~ "Venstre",
    country == "DK" & party_name == "Conservative People's Party" ~ "Conservatives",
    country == "DK" & party_name == "Danish People's Party" ~ "Danish PP",
    country == "DK" & party_name == "Socialist People's Party" ~ "Socialist PP",
    country == "DK" & party_name %in% c(
      "Red-Green Unity List",
      "Unity List - Red-Green Alliance"
    ) ~ "Red-Green",
    country == "DK" & party_name %in% c(
      "Alternativ",
      "The Alternative"
    ) ~ "Alternative",
    country == "DK" & party_name == "Liberal Alliance" ~ "Lib. Alliance",
    country == "DK" & party_name == "The New Right" ~ "New Right",
    country == "DK" & party_name %in% c(
      "Christian People's Party",
      "Christian Democrats"
    ) ~ "Chr. Dem.",
    country == "DK" & party_name == "Centre Democrats" ~ "Centre Dem.",
    country == "DK" & party_name == "Denmark Democrats - Inger Stojberg" ~ "Denmark Dem.",
    country == "DK" & party_name == "Independent Greens" ~ "Ind. Greens",
    country == "DK" & party_name == "Klaus Riskaer Pedersen List" ~ "Riskaer",
    country == "DK" & party_name == "Left Socialist Party" ~ "Left Socialists",

    TRUE ~ label
  )

  label
}

# ------------------------------------------------
# 4. Prepare transition data
# ------------------------------------------------

lookup_clean <- party_lookup %>%
  transmute(
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    year = as.integer(year),
    alt_id = as.character(alt_id),
    lookup_party_name = as.character(party_name),
    lookup_party_bloc = as.character(party_bloc_detailed)
  ) %>%
  filter(
    iso2c_file %in% target_countries,
    !is.na(elec_id),
    !is.na(year),
    !is.na(alt_id)
  ) %>%
  distinct(iso2c_file, elec_id, year, alt_id, .keep_all = TRUE)

transitions_clean <- transitions_primary %>%
  mutate(
    iso2c_file = as.character(iso2c_file),
    elec_id = as.character(elec_id),
    year = as.integer(year),
    id = as.character(id),
    vote_alt_id = as.character(vote),
    lag_alt_id = as.character(l_vote),
    weights = if_else(is.na(weights), 1, as.numeric(weights))
  ) %>%
  filter(
    iso2c_file %in% target_countries,
    !is.na(elec_id),
    !is.na(year),
    !is.na(id),
    !is.na(vote_alt_id),
    !is.na(lag_alt_id),
    !is.na(weights),
    weights > 0
  ) %>%
  distinct(iso2c_file, elec_id, year, id, .keep_all = TRUE)

transitions_with_parties <- transitions_clean %>%
  left_join(
    lookup_clean %>%
      rename(
        to_party_name = lookup_party_name,
        to_party_bloc = lookup_party_bloc,
        vote_alt_id = alt_id
      ),
    by = c("iso2c_file", "elec_id", "year", "vote_alt_id")
  ) %>%
  left_join(
    lookup_clean %>%
      rename(
        from_party_name = lookup_party_name,
        from_party_bloc = lookup_party_bloc,
        lag_alt_id = alt_id
      ),
    by = c("iso2c_file", "elec_id", "year", "lag_alt_id")
  ) %>%
  mutate(
    to_party_bloc = coalesce(to_party_bloc, as.character(party_bloc_detailed)),
    from_party_bloc = coalesce(
      from_party_bloc,
      as.character(switch_from_bloc_detailed)
    ),
    to_party_name = coalesce(to_party_name, as.character(party_label_best)),
    from_party_name = coalesce(from_party_name, as.character(switch_from))
  ) %>%
  filter(
    !is.na(to_party_bloc),
    !is.na(from_party_bloc),
    !is.na(to_party_name),
    !is.na(from_party_name)
  )

# ------------------------------------------------
# 5. Net exchanges
# ------------------------------------------------

country_totals <- transitions_with_parties %>%
  group_by(iso2c_file) %>%
  summarise(
    country_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

outward_flows <- transitions_with_parties %>%
  filter(
    from_party_bloc == "social_democratic",
    to_party_bloc != "social_democratic"
  ) %>%
  group_by(iso2c_file, competitor_party_name = to_party_name) %>%
  summarise(
    outward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

inward_flows <- transitions_with_parties %>%
  filter(
    to_party_bloc == "social_democratic",
    from_party_bloc != "social_democratic"
  ) %>%
  group_by(iso2c_file, competitor_party_name = from_party_name) %>%
  summarise(
    inward_weighted_n = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

net_exchanges_detailed <- full_join(
  outward_flows,
  inward_flows,
  by = c("iso2c_file", "competitor_party_name")
) %>%
  mutate(
    outward_weighted_n = replace_na(outward_weighted_n, 0),
    inward_weighted_n = replace_na(inward_weighted_n, 0)
  ) %>%
  left_join(country_totals, by = "iso2c_file") %>%
  mutate(
    net_weighted_n = inward_weighted_n - outward_weighted_n,
    transfer_weighted_n = inward_weighted_n + outward_weighted_n,
    country_label = country_labels[iso2c_file],
    party_label = make_party_label(iso2c_file, competitor_party_name)
  ) %>%
  filter(
    !is.na(country_label),
    transfer_weighted_n > 0
  )

net_exchanges <- net_exchanges_detailed %>%
  group_by(
    iso2c_file,
    country_label,
    party_label,
    country_weighted_n
  ) %>%
  summarise(
    competitor_party_names = paste(
      sort(unique(competitor_party_name)),
      collapse = "; "
    ),
    outward_weighted_n = sum(outward_weighted_n, na.rm = TRUE),
    inward_weighted_n = sum(inward_weighted_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    net_weighted_n = inward_weighted_n - outward_weighted_n,
    transfer_weighted_n = inward_weighted_n + outward_weighted_n,
    outward_pct = 100 * outward_weighted_n / country_weighted_n,
    inward_pct = 100 * inward_weighted_n / country_weighted_n,
    net_pct = 100 * net_weighted_n / country_weighted_n,
    transfer_pct = 100 * transfer_weighted_n / country_weighted_n
  ) %>%
  arrange(iso2c_file, net_pct)

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

plot_data <- net_exchanges %>%
  mutate(
    country_label = factor(
      country_label,
      levels = country_labels[target_countries]
    )
  ) %>%
  arrange(country_label, desc(net_pct)) %>%
  mutate(
    party_panel_label = paste(party_label, country_label, sep = "__")
  )

plot_data$party_panel_label <- factor(
  plot_data$party_panel_label,
  levels = unique(plot_data$party_panel_label)
)

max_abs_net <- max(abs(plot_data$net_pct), na.rm = TRUE)
x_limit <- max_abs_net * 1.25

if (!is.finite(x_limit) || x_limit == 0) {
  x_limit <- 1
}

p <- ggplot(
  plot_data,
  aes(x = net_pct, y = party_panel_label)
) +
  geom_col(
    aes(fill = net_pct >= 0),
    width = 0.7,
    show.legend = FALSE
  ) +
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
  scale_y_discrete(
    labels = function(x) sub("__.*$", "", x)
  ) +
  coord_cartesian(
    xlim = c(-x_limit, x_limit),
    clip = "off"
  ) +
  facet_wrap(
    ~ country_label,
    ncol = 2,
    scales = "free_y"
  ) +
  labs(
    title = "",
    x = "Net exchange with social democrats (percentage points)",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(5.5, 28, 5.5, 5.5)
  )

ggsave(
  filename = file.path(output_dir, "figure_country_net_social_democratic_exchanges.png"),
  plot = p,
  width = 11,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "figure_country_net_social_democratic_exchanges.pdf"),
  plot = p,
  width = 11,
  height = 8
)

message("Saved country net exchange plot and table to: ", output_dir)
