#### 01_austrian_example.R ----------------------------------------------------

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(here)
})

#### Load prepared Austrian micro-data ----------------------------------------

load(here::here("data", "micro", "at_df_long_valid_both.RData"))
at_harmonized <- df_long_valid_both

load(here::here("data", "micro", "manual", "at_2024_df_long_valid_both.RData"))
at_manual <- df_long_valid_both

prepare_transition_rows <- function(data) {
  data %>%
    dplyr::transmute(
      elec_id = as.character(elec_id),
      id = as.character(id),
      stack = as.integer(stack),
      switch_to = as.logical(switch_to),
      switch_from = as.logical(switch_from),
      weights = as.numeric(weights)
    )
}

transitions_at <- dplyr::bind_rows(
  prepare_transition_rows(at_harmonized),
  prepare_transition_rows(at_manual)
) %>%
  dplyr::group_by(elec_id, id) %>%
  dplyr::summarise(
    switch_from = {
      from_stack <- stack[switch_from %in% TRUE]
      if (length(from_stack) == 0) 99 else from_stack[[1]]
    },
    switch_to = {
      to_stack <- stack[switch_to %in% TRUE]
      if (length(to_stack) == 0) 99 else to_stack[[1]]
    },
    weights = dplyr::first(weights),
    .groups = "drop"
  )

#### Plot settings -------------------------------------------------------------

col_support <- "#AFC8F5"
col_loss <- "#F4A7A3"
col_gain <- "#A9D8A3"
col_grid <- "grey88"
col_text <- "grey20"

theme_contrib <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, colour = col_text),
      axis.title = element_text(size = 10, colour = col_text),
      axis.text = element_text(size = 10, colour = col_text),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.4),
      plot.margin = margin(8, 10, 8, 10)
    )
}

#### Build transition matrix ---------------------------------------------------

build_transition_matrix <- function(data, elec_id, party_order, label_map) {
  df_raw <- data %>%
    dplyr::filter(elec_id == !!elec_id) %>%
    dplyr::mutate(
      from = dplyr::case_when(
        switch_from %in% c(8, 99) ~ "NON",
        switch_from %in% 1:6 ~ paste0("P", switch_from),
        TRUE ~ "OTH"
      ),
      to = dplyr::case_when(
        switch_to %in% c(8, 99) ~ "NON",
        switch_to %in% 1:6 ~ paste0("P", switch_to),
        TRUE ~ "OTH"
      ),
      from = factor(from, levels = party_order),
      to = factor(to, levels = party_order)
    ) %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(weight = sum(weights, na.rm = TRUE), .groups = "drop")
  
  total_weight <- sum(df_raw$weight, na.rm = TRUE)
  
  tidyr::expand_grid(
    from = factor(party_order, levels = party_order),
    to = factor(party_order, levels = party_order)
  ) %>%
    dplyr::left_join(df_raw, by = c("from", "to")) %>%
    dplyr::mutate(
      weight = dplyr::coalesce(weight, 0),
      value = 100 * weight / total_weight,
      from_lab = dplyr::recode(as.character(from), !!!label_map),
      to_lab = dplyr::recode(as.character(to), !!!label_map)
    )
}

#### Austria 2024 --------------------------------------------------------------

party_order <- c("P3", "P2", "P1", "P5", "P4", "P6", "OTH", "NON")

label_map <- c(
  "P3" = "SPO",
  "P2" = "OVP",
  "P1" = "FPO",
  "P5" = "GRUNE",
  "P4" = "NEOS",
  "P6" = "KPO",
  "OTH" = "Other",
  "NON" = "Non-vote"
)

transition_mat <- build_transition_matrix(
  data = transitions_at,
  elec_id = "AT-2024-09",
  party_order = party_order,
  label_map = label_map
)

focal_party <- "SPO"

#### Panel A: aggregate SPO support -------------------------------------------

panel_a <- tibble::tibble(
  year = factor(c("2019", "2024"), levels = c("2019", "2024")),
  value = c(
    transition_mat %>%
      dplyr::filter(from_lab == focal_party) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::pull(value),
    transition_mat %>%
      dplyr::filter(to_lab == focal_party) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::pull(value)
  )
)

p_a <- ggplot(panel_a, aes(x = year, y = value)) +
  geom_col(width = 0.52, fill = col_support) +
  geom_text(aes(label = sprintf("%.1f", value)), vjust = -0.35, size = 4.1) +
  scale_y_continuous(
    limits = c(0, max(panel_a$value, na.rm = TRUE) * 1.15),
    expand = expansion(mult = c(0, 0.07))
  ) +
  labs(
    title = "A. Aggregate SPO support",
    x = NULL,
    y = "Percent of eligible electorate"
  ) +
  theme_contrib()

#### Flows involving SPO -------------------------------------------------------

outgoing <- transition_mat %>%
  dplyr::filter(from_lab == focal_party, to_lab != focal_party) %>%
  dplyr::transmute(actor = to_lab, outflow = value)

incoming <- transition_mat %>%
  dplyr::filter(to_lab == focal_party, from_lab != focal_party) %>%
  dplyr::transmute(actor = from_lab, inflow = value)

panel_b <- outgoing %>%
  dplyr::arrange(outflow, actor) %>%
  dplyr::mutate(
    actor = factor(actor, levels = actor),
    outflow_label = sprintf("%.1f", outflow)
  )

p_b <- ggplot(panel_b, aes(x = outflow, y = actor)) +
  geom_col(width = 0.62, fill = col_loss) +
  geom_text(aes(label = outflow_label), hjust = -0.15, size = 3.8) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.04))
  ) +
  coord_cartesian(xlim = c(0, max(panel_b$outflow) + 0.5), clip = "off") +
  labs(
    title = "B. Gross outflows from SPO",
    x = "Percentage points of eligible electorate",
    y = NULL
  ) +
  theme_contrib()

panel_c <- incoming %>%
  dplyr::arrange(inflow, actor) %>%
  dplyr::mutate(
    actor = factor(actor, levels = actor),
    inflow_label = sprintf("%.1f", inflow)
  )

p_c <- ggplot(panel_c, aes(x = inflow, y = actor)) +
  geom_col(width = 0.62, fill = col_gain) +
  geom_text(aes(label = inflow_label), hjust = -0.15, size = 3.8) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.04))
  ) +
  coord_cartesian(xlim = c(0, max(panel_c$inflow) + 0.5), clip = "off") +
  labs(
    title = "C. Gross inflows to SPO",
    x = "Percentage points of eligible electorate",
    y = NULL
  ) +
  theme_contrib()

panel_d <- dplyr::full_join(incoming, outgoing, by = "actor") %>%
  dplyr::mutate(
    inflow = tidyr::replace_na(inflow, 0),
    outflow = tidyr::replace_na(outflow, 0),
    net = inflow - outflow,
    direction = dplyr::if_else(net >= 0, "Net gain", "Net loss"),
    net_label = sprintf("%+.1f", net)
  ) %>%
  dplyr::arrange(net, actor) %>%
  dplyr::mutate(actor = factor(actor, levels = actor))

p_d <- ggplot(panel_d, aes(x = net, y = actor, fill = direction)) +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "grey45") +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = net_label, hjust = if_else(net >= 0, -0.15, 1.15)),
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Net loss" = col_loss, "Net gain" = col_gain),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(
    xlim = c(min(panel_d$net) - 0.5, max(panel_d$net) + 0.5),
    clip = "off"
  ) +
  labs(
    title = "D. Net exchanges involving SPO",
    x = "Net percentage points of eligible electorate",
    y = NULL
  ) +
  theme_contrib()

#### Display ------------------------------------------------------------------

top_row <- p_a + p_b + patchwork::plot_layout(widths = c(0.95, 1.15))
bottom_row <- p_c + p_d + patchwork::plot_layout(widths = c(1.10, 1.20))

combined_plot <- top_row / bottom_row + patchwork::plot_layout(heights = c(1, 1))

print(combined_plot)
