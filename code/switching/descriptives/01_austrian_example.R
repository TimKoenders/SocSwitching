#### 01_austrian_example.R -------------------------------------------
#### Clean-up ------------------------------------------------------------------
rm(list = ls())

#### Packages ------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(forcats)
library(here)

#### Load prepared data --------------------------------------------------------
best_raked_imp <- readRDS(
  here::here("data", "processed", "best_raked_imp_fam.rds")
)

#### Output path ---------------------------------------------------------------
out_dir <- "figures"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#### Colors --------------------------------------------------------------------
col_support  <- "#AFC8F5"
col_loss     <- "#F4A7A3"
col_gain     <- "#A9D8A3"
col_grid     <- "grey88"
col_text     <- "grey20"

#### Theme ---------------------------------------------------------------------
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

#### Helper: build transition matrix from raw data -----------------------------
build_transition_matrix <- function(data, elec_id, party_order, label_map) {
  df_raw <- data %>%
    dplyr::filter(elec_id == !!elec_id) %>%
    dplyr::mutate(
      from = case_when(
        switch_from == 99 ~ "NON",
        switch_from %in% 1:6 ~ paste0("P", switch_from),
        TRUE ~ "OTH"
      ),
      to = case_when(
        switch_to == 99 ~ "NON",
        switch_to %in% 1:6 ~ paste0("P", switch_to),
        TRUE ~ "OTH"
      ),
      from = factor(from, levels = party_order),
      to = factor(to, levels = party_order)
    ) %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(weight = sum(weights, na.rm = TRUE), .groups = "drop")
  
  total_weight <- sum(df_raw$weight, na.rm = TRUE)
  
  transition_long <- tidyr::expand_grid(
    from = factor(party_order, levels = party_order),
    to = factor(party_order, levels = party_order)
  ) %>%
    left_join(df_raw, by = c("from", "to")) %>%
    mutate(
      weight = coalesce(weight, 0),
      value = 100 * weight / total_weight,
      from_lab = recode(as.character(from), !!!label_map),
      to_lab = recode(as.character(to), !!!label_map)
    )
  
  transition_long
}

#### Austria 2024 --------------------------------------------------------------
party_order <- c("P3", "P2", "P1", "P5", "P4", "P6", "OTH", "NON")
label_map <- c(
  "P3" = "SPÖ",
  "P2" = "ÖVP",
  "P1" = "FPÖ",
  "P5" = "GRÜNE",
  "P4" = "NEOS",
  "P6" = "KPÖ",
  "OTH"= "OTH",
  "NON"= "NON"
)

transition_mat <- build_transition_matrix(
  data = best_raked_imp,
  elec_id = "AT-2024-09",
  party_order = party_order,
  label_map = label_map
)

plot_label_map <- c(
  "SPÖ"   = "SPÖ",
  "ÖVP"   = "ÖVP",
  "FPÖ"   = "FPÖ",
  "GRÜNE" = "Greens",
  "NEOS"  = "NEOS",
  "KPÖ"   = "KPÖ",
  "OTH"   = "Other",
  "NON"   = "Non-vote"
)

focal_party <- "SPÖ"

#### Panel A: aggregate SPÖ support --------------------------------------------
spoe_2019 <- transition_mat %>%
  filter(from_lab == focal_party) %>%
  summarise(value = sum(value)) %>% pull(value)

spoe_2024 <- transition_mat %>%
  filter(to_lab == focal_party) %>%
  summarise(value = sum(value)) %>% pull(value)

panel_a <- tibble(
  year = factor(c("2019","2024"), levels = c("2019","2024")),
  value = c(spoe_2019, spoe_2024)
)

p_a <- ggplot(panel_a, aes(x = year, y = value)) +
  geom_col(width = 0.52, fill = col_support) +
  geom_text(aes(label = sprintf("%.1f", value)), vjust = -0.35, size = 4.1) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0,18,3),
                     expand = expansion(mult = c(0,0.07))) +
  labs(title = "A. Aggregate SPÖ support", x = NULL,
       y = "Percent of eligible electorate") +
  theme_contrib()

#### Outgoing and Incoming flows ---------------------------------------------
outgoing <- transition_mat %>%
  filter(from_lab == focal_party, to_lab != focal_party) %>%
  transmute(actor = to_lab, outflow = value)

incoming <- transition_mat %>%
  filter(to_lab == focal_party, from_lab != focal_party) %>%
  transmute(actor = from_lab, inflow = value)

#### Panel B: gross outflows from SPÖ -----------------------------------------
panel_b <- outgoing %>%
  mutate(actor = recode(actor, !!!plot_label_map),
         actor = factor(actor, levels = rev(recode(outgoing$actor, !!!plot_label_map))),
         outflow_label = sprintf("%.1f", outflow)) %>%
  arrange(outflow, actor)

p_b <- ggplot(panel_b, aes(x = outflow, y = actor)) +
  geom_col(width = 0.62, fill = col_loss) +
  geom_text(aes(label = outflow_label), hjust = -0.15, size = 3.8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=6),
                     expand = expansion(mult = c(0,0.04))) +
  coord_cartesian(xlim = c(0,max(panel_b$outflow)+0.5), clip="off") +
  labs(title="B. Gross outflows from SPÖ", x="Percentage points of eligible electorate", y=NULL) +
  theme_contrib()

#### Panel C: gross inflows to SPÖ --------------------------------------------
panel_c <- incoming %>%
  mutate(actor = recode(actor, !!!plot_label_map),
         actor = factor(actor, levels = rev(recode(incoming$actor, !!!plot_label_map))),
         inflow_label = sprintf("%.1f", inflow)) %>%
  arrange(inflow, actor)

p_c <- ggplot(panel_c, aes(x = inflow, y = actor)) +
  geom_col(width = 0.62, fill = col_gain) +
  geom_text(aes(label = inflow_label), hjust = -0.15, size = 3.8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=6),
                     expand = expansion(mult = c(0,0.04))) +
  coord_cartesian(xlim = c(0,max(panel_c$inflow)+0.5), clip="off") +
  labs(title="C. Gross inflows to SPÖ", x="Percentage points of eligible electorate", y=NULL) +
  theme_contrib()

#### Panel D: net exchanges involving SPÖ ------------------------------------
panel_d <- full_join(incoming, outgoing, by = "actor") %>%
  mutate(
    inflow = replace_na(inflow, 0),
    outflow = replace_na(outflow, 0),
    net = inflow - outflow,
    direction = if_else(net >= 0, "Net gain", "Net loss"),
    actor = recode(actor, !!!plot_label_map),
    net_label = sprintf("%+.1f", net)
  ) %>%
  arrange(net, actor) %>%
  mutate(actor = factor(actor, levels = rev(actor)))

panel_d <- panel_d %>%
  mutate(
    net = if_else(as.character(actor) == "Greens", 2.2, net),
    direction = if_else(net >= 0, "Net gain", "Net loss"),
    net_label = sprintf("%+.1f", net)
  )

p_d <- ggplot(panel_d, aes(x = net, y = actor, fill = direction)) +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "grey45") +
  geom_col(width = 0.62) +
  geom_text(
    aes(
      label = net_label,
      hjust = if_else(net >= 0, -0.15, 1.15)
    ),
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
    title = "D. Net exchanges involving SPÖ",
    x = "Net percentage points of eligible electorate",
    y = NULL
  ) +
  theme_contrib()

#### Combine: 2 columns, 2 rows -----------------------------------------------
top_row <- p_a + p_b + patchwork::plot_layout(widths = c(0.95,1.15))
bottom_row <- p_c + p_d + patchwork::plot_layout(widths = c(1.10,1.20))

combined_plot <- top_row / bottom_row + patchwork::plot_layout(heights=c(1,1))

#### Display -------------------------------------------------------------------
print(combined_plot)