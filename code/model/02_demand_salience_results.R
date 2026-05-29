# ================================================================
# 02_demand_salience_results.R
# Reporting script for joint salience-change models
#
# Purpose:
#   Produce coefficient figures from point-estimate model output.
#   If direct delta-method uncertainty results exist, also produce
#   AME and net-effect figures.
#
# Compatible uncertainty input files:
#   1. restricted_choice_set_ames_with_delta_ci.rds
#      restricted_choice_set_net_effects_with_delta_ci.rds
#
#   2. restricted_choice_set_ames_with_delta_ci.csv
#      restricted_choice_set_net_effects_with_delta_ci.csv
#
# Notes:
#   This script is written for the simplified model script that no
#   longer saves bootstrap results or simulation-draw summaries.
#   It expects AME/net-effect uncertainty columns such as:
#     estimate, std.error, conf.low, conf.high
#   and normalises them internally to:
#     point_estimate, uncertainty_se, conf.low, conf.high.
# ================================================================

options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(purrr)
  library(grid)
  library(ggh4x)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

analysis_dir <- file.path(project_dir, "data", "analysis")

model_dir <- file.path(
  analysis_dir,
  "models",
  "salience_change"
)

figure_dir <- file.path(
  model_dir,
  "figures"
)

table_dir <- file.path(
  model_dir,
  "tables"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

path_coefficients <- file.path(
  model_dir,
  "restricted_choice_set_coefficients.csv"
)

path_final_results <- file.path(
  model_dir,
  "final_salience_change_model_results.rds"
)

path_ame_candidates <- c(
  file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.rds"),
  file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.csv")
)

path_net_candidates <- c(
  file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.rds"),
  file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.csv")
)

if (!file.exists(path_coefficients)) {
  stop("Coefficient file not found: ", path_coefficients)
}

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    NA_character_
  } else {
    existing[1]
  }
}

path_ame_results <- first_existing_path(path_ame_candidates)
path_net_results <- first_existing_path(path_net_candidates)

uncertainty_available <- !is.na(path_ame_results) && !is.na(path_net_results)

if (!uncertainty_available) {
  cat("\nAME and net-effect uncertainty results not found. Producing quick point-estimate coefficient results only.\n")
} else {
  cat("\nAME uncertainty file found:\n")
  cat(path_ame_results, "\n")
  cat("\nNet-effect uncertainty file found:\n")
  cat(path_net_results, "\n")
}

# ------------------------------------------------
# 2. Label helpers
# ------------------------------------------------

salience_predictors <- c(
  "eb_immigration_move_tminus1_to_t_z",
  "eb_environment_climate_move_tminus1_to_t_z",
  "eb_unemployment_move_tminus1_to_t_z"
)

salience_predictor_pattern <- paste(
  salience_predictors,
  collapse = "|"
)

predictor_order <- c(
  "Change in immigration salience",
  "Change in environmental salience",
  "Change in unemployment salience"
)

actor_order_no_non <- c(
  "Far right",
  "Mainstream right",
  "Green",
  "Far left"
)

outward_alt_labels <- c(
  retention = "Retention",
  to_far_left = "Far left",
  to_green = "Green",
  to_mainstream_right = "Mainstream right",
  to_far_right = "Far right",
  to_non = "Non-voting"
)

inward_alt_labels <- c(
  not_to_sd = "Not to SD",
  from_far_left = "Far left",
  from_green = "Green",
  from_mainstream_right = "Mainstream right",
  from_far_right = "Far right",
  from_non = "Non-voting"
)

term_to_actor <- function(term, flow) {
  dplyr::case_when(
    flow == "outward" & stringr::str_detect(term, "to_far_left") ~ "Far left",
    flow == "outward" & stringr::str_detect(term, "to_green") ~ "Green",
    flow == "outward" & stringr::str_detect(term, "to_mainstream_right") ~ "Mainstream right",
    flow == "outward" & stringr::str_detect(term, "to_far_right") ~ "Far right",
    flow == "outward" & stringr::str_detect(term, "to_non") ~ "Non-voting",
    flow == "inward" & stringr::str_detect(term, "from_far_left") ~ "Far left",
    flow == "inward" & stringr::str_detect(term, "from_green") ~ "Green",
    flow == "inward" & stringr::str_detect(term, "from_mainstream_right") ~ "Mainstream right",
    flow == "inward" & stringr::str_detect(term, "from_far_right") ~ "Far right",
    flow == "inward" & stringr::str_detect(term, "from_non") ~ "Non-voting",
    TRUE ~ NA_character_
  )
}

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
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
    dplyr::mutate(
      point_estimate = dplyr::coalesce(point_estimate, estimate),
      uncertainty_se = dplyr::coalesce(std.error, delta_se),
      n_uncertainty_success = dplyr::coalesce(n_success, n_delta_draws_success)
    )
}

read_uncertainty_file <- function(path) {
  if (stringr::str_ends(path, "\\.rds")) {
    readRDS(path)
  } else if (stringr::str_ends(path, "\\.csv")) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Unknown file type: ", path)
  }
}

# ------------------------------------------------
# 3. Quick coefficient results: log-odds
# ------------------------------------------------

coef_results <- readr::read_csv(
  path_coefficients,
  show_col_types = FALSE
)

coef_plot_data <- coef_results %>%
  dplyr::filter(
    stringr::str_detect(term, salience_predictor_pattern),
    !stringr::str_detect(term, "retention|not_to_sd")
  ) %>%
  dplyr::mutate(
    actor_label = term_to_actor(term, flow),
    predictor_label = factor(
      predictor_label,
      levels = predictor_order
    ),
    flow_label = factor(
      flow_label,
      levels = c("Outward switching", "Inward switching")
    ),
    actor_label = factor(
      actor_label,
      levels = actor_order_no_non
    )
  ) %>%
  dplyr::filter(
    !is.na(actor_label),
    actor_label != "Non-voting"
  )

x_max <- max(
  abs(c(coef_plot_data$conf.low, coef_plot_data$conf.high)),
  na.rm = TRUE
)

x_limit <- ceiling((x_max + 0.02) * 10) / 10

x_breaks <- seq(
  from = -x_limit,
  to = x_limit,
  by = 0.1
)

fig_salience_logodds <- ggplot(
  coef_plot_data,
  aes(
    x = estimate,
    y = actor_label
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0,
    linewidth = 0.35
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_grid(
    predictor_label ~ flow_label
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = function(x) sprintf("%.1f", x)
  ) +
  coord_cartesian(
    xlim = c(-x_limit, x_limit)
  ) +
  labs(
    x = "",
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    panel.spacing = unit(1.3, "lines")
  )

print(fig_salience_logodds)

ggsave(
  filename = file.path(figure_dir, "salience_change_logodds_outward_inward_quick.pdf"),
  plot = fig_salience_logodds,
  width = 8,
  height = 5.8
)

ggsave(
  filename = file.path(figure_dir, "salience_change_logodds_outward_inward_quick.png"),
  plot = fig_salience_logodds,
  width = 8,
  height = 5.8,
  dpi = 300
)

coef_table <- coef_plot_data %>%
  dplyr::arrange(
    predictor_label,
    flow_label,
    actor_label
  ) %>%
  dplyr::transmute(
    predictor = as.character(predictor_label),
    flow = as.character(flow_label),
    competitor = as.character(actor_label),
    estimate,
    std_error = std.error,
    conf_low = conf.low,
    conf_high = conf.high,
    p_value = p.value
  )

coef_latex_table <- coef_table %>%
  dplyr::mutate(
    estimate_se = paste0(
      format_num(estimate, 3),
      " (",
      format_num(std_error, 3),
      ")"
    ),
    ci = paste0(
      "[",
      format_num(conf_low, 3),
      ", ",
      format_num(conf_high, 3),
      "]"
    ),
    p_value = format_num(p_value, 3)
  ) %>%
  dplyr::select(
    predictor,
    flow,
    competitor,
    estimate_se,
    ci,
    p_value
  )

readr::write_csv(
  coef_table,
  file.path(table_dir, "salience_change_logodds_quick_reporting_table.csv")
)

readr::write_csv(
  coef_latex_table,
  file.path(table_dir, "salience_change_logodds_quick_latex_ready.csv")
)

cat("\n================================================\n")
cat("Quick log-odds coefficient table\n")
cat("================================================\n")
print(coef_table, n = Inf, width = Inf)

# ------------------------------------------------
# 4. AME and net-effect uncertainty results
# ------------------------------------------------

if (uncertainty_available) {
  
  ame_results <- read_uncertainty_file(path_ame_results) %>%
    normalise_uncertainty_summary()
  
  net_results <- read_uncertainty_file(path_net_results) %>%
    normalise_uncertainty_summary()
  
  final_results <- if (file.exists(path_final_results)) {
    readRDS(path_final_results)
  } else {
    NULL
  }
  
  required_ame_cols <- c(
    "predictor_label",
    "flow",
    "flow_label",
    "alt",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high"
  )
  
  required_net_cols <- c(
    "predictor_label",
    "actor_label",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high"
  )
  
  missing_ame_cols <- setdiff(required_ame_cols, names(ame_results))
  missing_net_cols <- setdiff(required_net_cols, names(net_results))
  
  if (length(missing_ame_cols) > 0) {
    stop(
      "AME uncertainty results are missing required columns: ",
      paste(missing_ame_cols, collapse = ", ")
    )
  }
  
  if (length(missing_net_cols) > 0) {
    stop(
      "Net-effect uncertainty results are missing required columns: ",
      paste(missing_net_cols, collapse = ", ")
    )
  }
  
  actor_order_plot <- c(
    "Far right",
    "Mainstream right",
    "Green",
    "Far left"
  )
  
  x_scale_outward <- scale_x_continuous(
    limits = c(-0.02, 0.02),
    breaks = seq(-0.02, 0.02, length.out = 7),
    labels = function(x) sprintf("%.1f", 100 * x)
  )
  
  x_scale_inward <- scale_x_continuous(
    limits = c(-0.005, 0.005),
    breaks = seq(-0.005, 0.005, length.out = 5),
    labels = function(x) sprintf("%.1f", 100 * x)
  )
  
  x_scale_net <- scale_x_continuous(
    limits = c(-0.010, 0.010),
    breaks = seq(-0.010, 0.010, length.out = 5),
    labels = function(x) sprintf("%.1f", 100 * x)
  )
  
  ame_plot_data <- ame_results %>%
    dplyr::mutate(
      predictor_label = factor(
        predictor_label,
        levels = predictor_order
      ),
      flow_label = factor(
        flow_label,
        levels = c("Outward switching", "Inward switching")
      ),
      alternative_label = dplyr::case_when(
        flow == "outward" ~ dplyr::recode(as.character(alt), !!!outward_alt_labels),
        flow == "inward" ~ dplyr::recode(as.character(alt), !!!inward_alt_labels),
        TRUE ~ as.character(alt)
      ),
      alternative_label = factor(
        alternative_label,
        levels = c("Retention", "Not to SD", "Non-voting", actor_order_plot)
      )
    )
  
  ame_competitor_plot_data <- ame_plot_data %>%
    dplyr::filter(
      !alternative_label %in% c("Retention", "Not to SD", "Non-voting")
    ) %>%
    dplyr::mutate(
      alternative_label = factor(
        as.character(alternative_label),
        levels = actor_order_plot
      )
    )
  
  net_plot_data <- net_results %>%
    dplyr::filter(
      actor_label != "Non-voting"
    ) %>%
    dplyr::mutate(
      predictor_label = factor(
        predictor_label,
        levels = predictor_order
      ),
      actor_label = factor(
        actor_label,
        levels = actor_order_plot
      ),
      flow = "net",
      flow_label = "Net effect"
    )
  
  outward_for_combined <- ame_competitor_plot_data %>%
    dplyr::filter(flow == "outward") %>%
    dplyr::transmute(
      predictor,
      predictor_label,
      flow = "outward",
      flow_label = "Outward switching",
      actor_label = alternative_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  inward_for_combined <- ame_competitor_plot_data %>%
    dplyr::filter(flow == "inward") %>%
    dplyr::transmute(
      predictor,
      predictor_label,
      flow = "inward",
      flow_label = "Inward switching",
      actor_label = alternative_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  net_for_combined <- net_plot_data %>%
    dplyr::transmute(
      predictor,
      predictor_label,
      flow = "net",
      flow_label = "Net effect",
      actor_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  combined_plot_data <- dplyr::bind_rows(
    outward_for_combined,
    inward_for_combined,
    net_for_combined
  ) %>%
    dplyr::mutate(
      flow_label = factor(
        flow_label,
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label = factor(
        actor_label,
        levels = actor_order_plot
      )
    )
  
  fig_salience_combined <- ggplot(
    combined_plot_data,
    aes(
      x = point_estimate,
      y = actor_label
    )
  ) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.35,
      linetype = "dashed"
    ) +
    geom_errorbarh(
      aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      height = 0,
      linewidth = 0.35
    ) +
    geom_point(
      size = 1.7
    ) +
    facet_grid(
      predictor_label ~ flow_label,
      scales = "free_x"
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        flow_label == "Outward switching" ~ x_scale_outward,
        flow_label == "Inward switching" ~ x_scale_inward,
        flow_label == "Net effect" ~ x_scale_net
      )
    ) +
    labs(
      x = "Average marginal effect",
      y = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      panel.spacing = unit(1.3, "lines")
    )
  
  print(fig_salience_combined)
  
  ggsave(
    filename = file.path(figure_dir, "salience_change_outward_inward_net_effects.pdf"),
    plot = fig_salience_combined,
    width = 9,
    height = 6.5
  )
  
  ggsave(
    filename = file.path(figure_dir, "salience_change_outward_inward_net_effects.png"),
    plot = fig_salience_combined,
    width = 9,
    height = 6.5,
    dpi = 300
  )
  
  x_scale_net_only <- scale_x_continuous(
    limits = c(-0.010, 0.010),
    breaks = seq(-0.010, 0.010, length.out = 5),
    labels = function(x) sprintf("%.1f", 100 * x)
  )
  
  fig_salience_net <- ggplot(
    net_plot_data,
    aes(
      x = point_estimate,
      y = actor_label
    )
  ) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.35,
      linetype = "dashed"
    ) +
    geom_errorbarh(
      aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      height = 0,
      linewidth = 0.35
    ) +
    geom_point(
      size = 1.8
    ) +
    facet_wrap(
      ~ predictor_label,
      ncol = 1,
      scales = "free_x"
    ) +
    x_scale_net_only +
    labs(
      x = "Net effect on social-democratic support, percentage points",
      y = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      panel.spacing = unit(0.9, "lines")
    )
  
  print(fig_salience_net)
  
  ggsave(
    filename = file.path(figure_dir, "salience_change_net_effects_only.pdf"),
    plot = fig_salience_net,
    width = 7,
    height = 6
  )
  
  ggsave(
    filename = file.path(figure_dir, "salience_change_net_effects_only.png"),
    plot = fig_salience_net,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  combined_table <- combined_plot_data %>%
    dplyr::arrange(
      predictor_label,
      flow_label,
      actor_label
    ) %>%
    dplyr::transmute(
      predictor = as.character(predictor_label),
      flow = as.character(flow_label),
      competitor = as.character(actor_label),
      estimate = point_estimate,
      estimate_pp = 100 * point_estimate,
      std_error = uncertainty_se,
      std_error_pp = 100 * uncertainty_se,
      conf_low = conf.low,
      conf_high = conf.high,
      conf_low_pp = 100 * conf.low,
      conf_high_pp = 100 * conf.high
    )
  
  net_table <- net_plot_data %>%
    dplyr::arrange(
      predictor_label,
      actor_label
    ) %>%
    dplyr::transmute(
      predictor = as.character(predictor_label),
      competitor = as.character(actor_label),
      estimate = point_estimate,
      estimate_pp = 100 * point_estimate,
      std_error = uncertainty_se,
      std_error_pp = 100 * uncertainty_se,
      conf_low = conf.low,
      conf_high = conf.high,
      conf_low_pp = 100 * conf.low,
      conf_high_pp = 100 * conf.high
    )
  
  ame_table <- ame_competitor_plot_data %>%
    dplyr::arrange(
      predictor_label,
      flow_label,
      alternative_label
    ) %>%
    dplyr::transmute(
      predictor = as.character(predictor_label),
      flow = as.character(flow_label),
      competitor = as.character(alternative_label),
      estimate = point_estimate,
      estimate_pp = 100 * point_estimate,
      std_error = uncertainty_se,
      std_error_pp = 100 * uncertainty_se,
      conf_low = conf.low,
      conf_high = conf.high,
      conf_low_pp = 100 * conf.low,
      conf_high_pp = 100 * conf.high
    )
  
  readr::write_csv(
    combined_table,
    file.path(table_dir, "salience_change_outward_inward_net_reporting_table.csv")
  )
  
  readr::write_csv(
    net_table,
    file.path(table_dir, "salience_change_net_effects_reporting_table.csv")
  )
  
  readr::write_csv(
    ame_table,
    file.path(table_dir, "salience_change_ame_reporting_table.csv")
  )
  
  combined_latex_table <- combined_table %>%
    dplyr::mutate(
      estimate_se = paste0(
        format_num(estimate_pp, 2),
        " (",
        format_num(std_error_pp, 2),
        ")"
      ),
      ci = paste0(
        "[",
        format_num(conf_low_pp, 2),
        ", ",
        format_num(conf_high_pp, 2),
        "]"
      )
    ) %>%
    dplyr::select(
      predictor,
      flow,
      competitor,
      estimate_se,
      ci
    )
  
  net_latex_table <- net_table %>%
    dplyr::mutate(
      estimate_se = paste0(
        format_num(estimate_pp, 2),
        " (",
        format_num(std_error_pp, 2),
        ")"
      ),
      ci = paste0(
        "[",
        format_num(conf_low_pp, 2),
        ", ",
        format_num(conf_high_pp, 2),
        "]"
      )
    ) %>%
    dplyr::select(
      predictor,
      competitor,
      estimate_se,
      ci
    )
  
  ame_latex_table <- ame_table %>%
    dplyr::mutate(
      estimate_se = paste0(
        format_num(estimate_pp, 2),
        " (",
        format_num(std_error_pp, 2),
        ")"
      ),
      ci = paste0(
        "[",
        format_num(conf_low_pp, 2),
        ", ",
        format_num(conf_high_pp, 2),
        "]"
      )
    ) %>%
    dplyr::select(
      predictor,
      flow,
      competitor,
      estimate_se,
      ci
    )
  
  readr::write_csv(
    combined_latex_table,
    file.path(table_dir, "salience_change_outward_inward_net_latex_ready.csv")
  )
  
  readr::write_csv(
    net_latex_table,
    file.path(table_dir, "salience_change_net_effects_latex_ready.csv")
  )
  
  readr::write_csv(
    ame_latex_table,
    file.path(table_dir, "salience_change_ames_latex_ready.csv")
  )
  
  cat("\n================================================\n")
  cat("AME and net-effect combined table, excluding non-voting\n")
  cat("================================================\n")
  print(combined_table, n = Inf, width = Inf)
  
  cat("\n================================================\n")
  cat("Net effects, excluding non-voting\n")
  cat("================================================\n")
  print(net_table, n = Inf, width = Inf)
  
  cat("\n================================================\n")
  cat("Outward and inward AMEs, excluding non-voting\n")
  cat("================================================\n")
  print(ame_table, n = Inf, width = Inf)
}

# ------------------------------------------------
# 4a. Issue-specific AME and net-effect figures
# ------------------------------------------------

if (uncertainty_available) {
  
  issue_specific_dir <- file.path(
    figure_dir,
    "issue_specific"
  )
  
  dir.create(issue_specific_dir, recursive = TRUE, showWarnings = FALSE)
  
  issue_file_labels <- c(
    "Change in immigration salience" = "immigration_salience",
    "Change in environmental salience" = "environmental_salience",
    "Change in unemployment salience" = "unemployment_salience"
  )
  
  for (this_predictor in names(issue_file_labels)) {
    
    this_plot_data <- combined_plot_data %>%
      dplyr::filter(
        as.character(predictor_label) == this_predictor
      )
    
    this_title <- stringr::str_remove(
      this_predictor,
      "^Change in "
    )
    
    fig_issue_specific <- ggplot(
      this_plot_data,
      aes(
        x = point_estimate,
        y = actor_label
      )
    ) +
      geom_vline(
        xintercept = 0,
        linewidth = 0.35,
        linetype = "dashed"
      ) +
      geom_errorbarh(
        aes(
          xmin = conf.low,
          xmax = conf.high
        ),
        height = 0,
        linewidth = 0.35
      ) +
      geom_point(
        size = 1.8
      ) +
      facet_grid(
        ~ flow_label,
        scales = "free_x"
      ) +
      ggh4x::facetted_pos_scales(
        x = list(
          flow_label == "Outward switching" ~ x_scale_outward,
          flow_label == "Inward switching" ~ x_scale_inward,
          flow_label == "Net effect" ~ x_scale_net
        )
      ) +
      labs(
        title = stringr::str_to_sentence(this_title),
        x = "Average marginal effect",
        y = NULL
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        panel.spacing = unit(1.3, "lines")
      )
    
    print(fig_issue_specific)
    
    ggsave(
      filename = file.path(
        issue_specific_dir,
        paste0("salience_change_", issue_file_labels[[this_predictor]], "_outward_inward_net.pdf")
      ),
      plot = fig_issue_specific,
      width = 9,
      height = 3.6
    )
    
    ggsave(
      filename = file.path(
        issue_specific_dir,
        paste0("salience_change_", issue_file_labels[[this_predictor]], "_outward_inward_net.png")
      ),
      plot = fig_issue_specific,
      width = 9,
      height = 3.6,
      dpi = 300
    )
  }
}

# ------------------------------------------------
# 4b. Console summary of plotted effect sizes and p-values
# ------------------------------------------------

cat("\n================================================\n")
cat("Plotted log-odds effects with p-values\n")
cat("================================================\n")

coef_console <- coef_plot_data %>%
  dplyr::arrange(
    predictor_label,
    flow_label,
    actor_label
  ) %>%
  dplyr::transmute(
    predictor = as.character(predictor_label),
    flow = as.character(flow_label),
    competitor = as.character(actor_label),
    estimate = format_num(estimate, 3),
    std_error = format_num(std.error, 3),
    conf_low = format_num(conf.low, 3),
    conf_high = format_num(conf.high, 3),
    p_value = format_num(p.value, 3)
  )

print(coef_console, n = Inf, width = Inf)

if (uncertainty_available) {
  
  cat("\n================================================\n")
  cat("Plotted AMEs and net effects with approximate p-values\n")
  cat("================================================\n")
  
  combined_console <- combined_plot_data %>%
    dplyr::mutate(
      z_value = point_estimate / uncertainty_se,
      p_value = 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
    ) %>%
    dplyr::arrange(
      predictor_label,
      flow_label,
      actor_label
    ) %>%
    dplyr::transmute(
      predictor = as.character(predictor_label),
      flow = as.character(flow_label),
      competitor = as.character(actor_label),
      estimate_pp = format_num(100 * point_estimate, 2),
      std_error_pp = format_num(100 * uncertainty_se, 2),
      conf_low_pp = format_num(100 * conf.low, 2),
      conf_high_pp = format_num(100 * conf.high, 2),
      p_value = format_num(p_value, 3)
    )
  
  print(combined_console, n = Inf, width = Inf)
  
  cat("\nNote: AME and net-effect p-values are approximate two-sided normal p-values based on the delta-method standard errors.\n")
}


# ------------------------------------------------
# 5. Files written
# ------------------------------------------------

cat("\n================================================\n")
cat("Files written\n")
cat("================================================\n")

cat("Quick coefficient figures:\n")
cat(file.path(figure_dir, "salience_change_logodds_outward_inward_quick.pdf"), "\n")
cat(file.path(figure_dir, "salience_change_logodds_outward_inward_quick.png"), "\n")

cat("\nQuick coefficient tables:\n")
cat(file.path(table_dir, "salience_change_logodds_quick_reporting_table.csv"), "\n")
cat(file.path(table_dir, "salience_change_logodds_quick_latex_ready.csv"), "\n")

if (uncertainty_available) {
  
  cat("\nAME and net-effect figures:\n")
  cat(file.path(figure_dir, "salience_change_outward_inward_net_effects.pdf"), "\n")
  cat(file.path(figure_dir, "salience_change_outward_inward_net_effects.png"), "\n")
  cat(file.path(figure_dir, "salience_change_net_effects_only.pdf"), "\n")
  cat(file.path(figure_dir, "salience_change_net_effects_only.png"), "\n")
  
  cat("\nAME and net-effect tables:\n")
  cat(file.path(table_dir, "salience_change_outward_inward_net_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "salience_change_net_effects_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "salience_change_ame_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "salience_change_outward_inward_net_latex_ready.csv"), "\n")
  cat(file.path(table_dir, "salience_change_net_effects_latex_ready.csv"), "\n")
  cat(file.path(table_dir, "salience_change_ames_latex_ready.csv"), "\n")
  
} else {
  
  cat("\nAME and net-effect figures and tables skipped because uncertainty files are not available.\n")
}

cat("\nScript completed successfully.\n")
