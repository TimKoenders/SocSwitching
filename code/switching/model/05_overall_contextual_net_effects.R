# ================================================================
# 05_overall_contextual_net_effects.R
# Compute overall net effects across party-family competitors
#
# Contextual variables:
#   Demand side:
#     1) Immigration salience
#     2) Environmental salience
#     3) Unemployment salience
#
#   Supply side:
#     4) SD cultural position
#     5) SD state-economy position
#     6) SD investment-consumption position
#
# Definition:
#   Overall net effect = sum of competitor-specific net effects
#   across Far right, Mainstream right, Green, and Far left.
#
# Interpretation:
#   Positive values imply a favorable net effect for social-democratic
#   support across the four competitor channels.
# ================================================================

options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

analysis_dir <- file.path(project_dir, "data", "analysis")

model_root_dir <- file.path(
  analysis_dir,
  "models"
)

salience_model_dir <- file.path(
  model_root_dir,
  "salience_change"
)

supply_model_dir <- file.path(
  model_root_dir,
  "supply_position_change"
)

output_dir <- file.path(
  model_root_dir,
  "overall_contextual_net_effects"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_salience_net <- file.path(
  salience_model_dir,
  "restricted_choice_set_net_effects_with_delta_ci.rds"
)

path_supply_net_main <- file.path(
  supply_model_dir,
  "restricted_choice_set_net_effects_with_delta_ci.rds"
)

path_supply_net_all_operationalisations <- file.path(
  model_root_dir,
  "supply_position_change_all_operationalisations_net_effects_with_delta_ci.rds"
)

# ------------------------------------------------
# 2. Helpers
# ------------------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
}

add_missing_column <- function(df, column, value = NA_real_) {
  if (!column %in% names(df)) {
    df[[column]] <- value
  }
  df
}

normalise_uncertainty_summary <- function(df) {
  df <- df %>%
    add_missing_column("point_estimate") %>%
    add_missing_column("estimate") %>%
    add_missing_column("std.error") %>%
    add_missing_column("delta_se") %>%
    add_missing_column("n_success") %>%
    add_missing_column("n_delta_draws_success")
  
  df %>%
    mutate(
      point_estimate = coalesce(point_estimate, estimate),
      uncertainty_se = coalesce(std.error, delta_se),
      n_uncertainty_success = coalesce(n_success, n_delta_draws_success)
    )
}

relabel_supply_predictor <- function(df) {
  if (!"predictor_label" %in% names(df)) {
    return(df)
  }
  
  df %>%
    mutate(
      predictor_label = recode(
        predictor_label,
        "Change in SD education-labour position" =
          "Change in SD investment-consumption position"
      )
    )
}

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
}

competitor_order <- c(
  "Far right",
  "Mainstream right",
  "Green",
  "Far left"
)

demand_predictor_order <- c(
  "Change in immigration salience",
  "Change in environmental salience",
  "Change in unemployment salience"
)

supply_predictor_order <- c(
  "Change in SD cultural position",
  "Change in SD state-economy position",
  "Change in SD investment-consumption position"
)

all_predictor_order <- c(
  demand_predictor_order,
  supply_predictor_order
)

# ------------------------------------------------
# 3. Load demand-side net effects
# ------------------------------------------------

stop_if_missing(path_salience_net)

salience_net <- readRDS(path_salience_net) %>%
  normalise_uncertainty_summary() %>%
  mutate(
    context_side = "Demand side",
    contextual_variable = predictor_label
  ) %>%
  filter(
    predictor_label %in% demand_predictor_order,
    actor_label %in% competitor_order
  )

# ------------------------------------------------
# 4. Load supply-side net effects
# ------------------------------------------------

stop_if_missing(path_supply_net_main)

supply_net_main <- readRDS(path_supply_net_main) %>%
  normalise_uncertainty_summary() %>%
  relabel_supply_predictor() %>%
  filter(
    predictor %in% c(
      "sd_libcons_move_std",
      "sd_stateconomy_move_std",
      "sd_investmentconsumption_move_std"
    )
  )

if (file.exists(path_supply_net_all_operationalisations)) {
  supply_net_investment_consumption <- readRDS(path_supply_net_all_operationalisations) %>%
    normalise_uncertainty_summary() %>%
    relabel_supply_predictor() %>%
    filter(
      operationalisation == "marpor_abou_chadi_wagner",
      predictor == "sd_investmentconsumption_move_std"
    )
  
  if (nrow(supply_net_investment_consumption) > 0) {
    supply_net_main <- supply_net_main %>%
      filter(predictor != "sd_investmentconsumption_move_std")
    
    supply_net_main <- bind_rows(
      supply_net_main,
      supply_net_investment_consumption
    )
  }
} else {
  cat(
    "\nCombined supply-position operationalisation file not found; ",
    "using main supply-position net effects for all supply-side rows.\n",
    sep = ""
  )
}

supply_net <- supply_net_main %>%
  mutate(
    context_side = "Supply side",
    contextual_variable = predictor_label
  ) %>%
  filter(
    predictor_label %in% supply_predictor_order,
    actor_label %in% competitor_order
  )

# ------------------------------------------------
# 5. Combine all six contextual variables
# ------------------------------------------------

contextual_net_effects <- bind_rows(
  salience_net,
  supply_net
) %>%
  mutate(
    contextual_variable = factor(
      contextual_variable,
      levels = all_predictor_order
    ),
    actor_label = factor(
      actor_label,
      levels = competitor_order
    )
  ) %>%
  arrange(
    contextual_variable,
    actor_label
  )

panel_check <- contextual_net_effects %>%
  count(context_side, contextual_variable)

cat("\n================================================\n")
cat("Panel check: each contextual variable should have four competitors\n")
cat("================================================\n")
print(panel_check, n = Inf, width = Inf)

if (any(panel_check$n != 4)) {
  stop("At least one contextual variable does not have four competitor-specific net effects.")
}

if (nrow(panel_check) != 6) {
  stop("Expected six contextual variables, but found: ", nrow(panel_check))
}

# ------------------------------------------------
# 6. Compute overall net effects
# ------------------------------------------------

overall_net_effects <- contextual_net_effects %>%
  group_by(
    context_side,
    contextual_variable
  ) %>%
  summarise(
    overall_net_effect = sum(point_estimate, na.rm = TRUE),
    overall_net_effect_pp = 100 * overall_net_effect,
    average_net_effect = mean(point_estimate, na.rm = TRUE),
    average_net_effect_pp = 100 * average_net_effect,
    n_competitors = n(),
    min_n_uncertainty_success = suppressWarnings(min(n_uncertainty_success, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    contextual_variable = factor(
      contextual_variable,
      levels = all_predictor_order
    ),
    direction = case_when(
      overall_net_effect > 0 ~ "Favorable to SD",
      overall_net_effect < 0 ~ "Unfavorable to SD",
      TRUE ~ "Zero"
    )
  ) %>%
  arrange(contextual_variable)

# ------------------------------------------------
# 7. Competitor-level decomposition
# ------------------------------------------------

net_effect_decomposition <- contextual_net_effects %>%
  transmute(
    context_side,
    contextual_variable,
    competitor = as.character(actor_label),
    net_effect_pp = 100 * point_estimate
  ) %>%
  pivot_wider(
    names_from = competitor,
    values_from = net_effect_pp
  ) %>%
  left_join(
    overall_net_effects %>%
      select(
        context_side,
        contextual_variable,
        overall_net_effect_pp,
        average_net_effect_pp,
        direction
      ),
    by = c("context_side", "contextual_variable")
  ) %>%
  arrange(contextual_variable)

# ------------------------------------------------
# 8. Specific balance checks
# ------------------------------------------------

balance_checks <- net_effect_decomposition %>%
  mutate(
    green_far_right_balance_pp = Green + `Far right`,
    green_vs_far_right_verdict = case_when(
      green_far_right_balance_pp < 0 ~ "Green losses outweigh far-right gains",
      green_far_right_balance_pp > 0 ~ "Far-right gains outweigh Green losses",
      TRUE ~ "Green and far-right components offset"
    ),
    left_right_balance_pp = `Far left` + `Mainstream right`,
    left_right_verdict = case_when(
      left_right_balance_pp < 0 ~ "Far-left losses outweigh mainstream-right gains",
      left_right_balance_pp > 0 ~ "Mainstream-right gains outweigh far-left losses",
      TRUE ~ "Far-left and mainstream-right components offset"
    )
  )

overall_net_effects_latex <- overall_net_effects %>%
  transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    overall_net_effect_pp = format_num(overall_net_effect_pp, 3),
    average_net_effect_pp = format_num(average_net_effect_pp, 3),
    n_competitors,
    direction
  )

net_effect_decomposition_latex <- net_effect_decomposition %>%
  transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    far_right = format_num(`Far right`, 3),
    mainstream_right = format_num(`Mainstream right`, 3),
    green = format_num(Green, 3),
    far_left = format_num(`Far left`, 3),
    overall_net_effect_pp = format_num(overall_net_effect_pp, 3),
    average_net_effect_pp = format_num(average_net_effect_pp, 3),
    direction
  )

balance_checks_latex <- balance_checks %>%
  transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    green_far_right_balance_pp = format_num(green_far_right_balance_pp, 3),
    green_vs_far_right_verdict,
    left_right_balance_pp = format_num(left_right_balance_pp, 3),
    left_right_verdict
  )

# ------------------------------------------------
# 9. Plot all overall net effects together
# ------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

overall_plot_dir <- file.path(
  output_dir,
  "figures"
)

dir.create(overall_plot_dir, recursive = TRUE, showWarnings = FALSE)

plot_data_competitor_components <- contextual_net_effects %>%
  dplyr::transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    effect_type = as.character(actor_label),
    effect_pp = 100 * point_estimate
  )

plot_data_overall <- overall_net_effects %>%
  dplyr::transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    effect_type = "Overall",
    effect_pp = overall_net_effect_pp
  )

overall_net_plot_data <- dplyr::bind_rows(
  plot_data_competitor_components,
  plot_data_overall
) %>%
  dplyr::mutate(
    contextual_variable_short = dplyr::recode(
      contextual_variable,
      "Change in immigration salience" = "Immigration salience",
      "Change in environmental salience" = "Environment salience",
      "Change in unemployment salience" = "Unemployment salience",
      "Change in SD cultural position" = "Cultural position",
      "Change in SD state-economy position" = "Economic position",
      "Change in SD investment-consumption position" = "Social investment position"
    ),
    contextual_variable_short = factor(
      contextual_variable_short,
      levels = rev(c(
        "Immigration salience",
        "Environment salience",
        "Unemployment salience",
        "Cultural position",
        "Economic position",
        "Social investment position"
      ))
    ),
    context_side = factor(
      context_side,
      levels = c("Demand side", "Supply side")
    ),
    effect_type = factor(
      effect_type,
      levels = c(
        "Far right",
        "Mainstream right",
        "Green",
        "Far left",
        "Overall"
      )
    ),
    point_size = dplyr::if_else(effect_type == "Overall", 3.8, 2.3)
  )

fig_overall_contextual_net_effects <- ggplot(
  overall_net_plot_data,
  aes(
    x = effect_pp,
    y = contextual_variable_short,
    color = effect_type,
    shape = effect_type
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_point(
    aes(size = point_size),
    stroke = 0.6
  ) +
  facet_grid(
    context_side ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_color_manual(
    values = c(
      "Far right" = "grey20",
      "Mainstream right" = "dodgerblue4",
      "Green" = "darkgreen",
      "Far left" = "purple4",
      "Overall" = "red3"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Far right" = 16,
      "Mainstream right" = 17,
      "Green" = 15,
      "Far left" = 3,
      "Overall" = 8
    )
  ) +
  scale_size_identity() +
  labs(
    x = "Net effect on social-democratic support, percentage points",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),
    legend.key.width = unit(0.8, "lines"),
    panel.spacing = unit(0.7, "lines")
  )

print(fig_overall_contextual_net_effects)

ggsave(
  filename = file.path(
    overall_plot_dir,
    "overall_contextual_net_effects_all_variables.pdf"
  ),
  plot = fig_overall_contextual_net_effects,
  width = 9,
  height = 5.8
)

ggsave(
  filename = file.path(
    overall_plot_dir,
    "overall_contextual_net_effects_all_variables.png"
  ),
  plot = fig_overall_contextual_net_effects,
  width = 9,
  height = 5.8,
  dpi = 300
)

# ------------------------------------------------
# 10. Print numerical total net effects
# ------------------------------------------------

total_net_results_print <- net_effect_decomposition %>%
  dplyr::transmute(
    context_side,
    contextual_variable = as.character(contextual_variable),
    far_right = `Far right`,
    mainstream_right = `Mainstream right`,
    green = Green,
    far_left = `Far left`,
    total = overall_net_effect_pp
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(far_right, mainstream_right, green, far_left, total),
      ~ round(.x, 3)
    )
  )

cat("\n================================================\n")
cat("Total net effects across competitor channels, percentage points\n")
cat("================================================\n")
print(total_net_results_print, n = Inf, width = Inf)

readr::write_csv(
  total_net_results_print,
  file.path(output_dir, "total_net_effects_print_table.csv")
)


# ------------------------------------------------
# 11. Write files
# ------------------------------------------------

write_csv(
  contextual_net_effects,
  file.path(output_dir, "contextual_competitor_specific_net_effects.csv")
)

write_csv(
  overall_net_effects,
  file.path(output_dir, "overall_contextual_net_effects.csv")
)

write_csv(
  net_effect_decomposition,
  file.path(output_dir, "overall_contextual_net_effects_decomposition.csv")
)

write_csv(
  balance_checks,
  file.path(output_dir, "overall_contextual_net_effects_balance_checks.csv")
)

write_csv(
  overall_net_effects_latex,
  file.path(output_dir, "overall_contextual_net_effects_latex_ready.csv")
)

write_csv(
  net_effect_decomposition_latex,
  file.path(output_dir, "overall_contextual_net_effects_decomposition_latex_ready.csv")
)

write_csv(
  balance_checks_latex,
  file.path(output_dir, "overall_contextual_net_effects_balance_checks_latex_ready.csv")
)

# ------------------------------------------------
# 11. Console output
# ------------------------------------------------

cat("\n================================================\n")
cat("Overall net effects across four competitors\n")
cat("================================================\n")
print(overall_net_effects, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Competitor-level decomposition, percentage points\n")
cat("================================================\n")
print(net_effect_decomposition, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Balance checks, percentage points\n")
cat("================================================\n")
print(balance_checks, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Files written\n")
cat("================================================\n")
cat(file.path(output_dir, "contextual_competitor_specific_net_effects.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects_decomposition.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects_balance_checks.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects_latex_ready.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects_decomposition_latex_ready.csv"), "\n")
cat(file.path(output_dir, "overall_contextual_net_effects_balance_checks_latex_ready.csv"), "\n")

cat("\nScript completed successfully.\n")








