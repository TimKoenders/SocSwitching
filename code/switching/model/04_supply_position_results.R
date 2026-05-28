# ================================================================
# 04_supply_position_results.R
# Reporting script for joint supply-position-change models
#
# Main figure:
#   3 rows x 3 columns:
#     1) Change in SD cultural position
#     2) Change in SD state-economy position
#     3) Change in SD investment-consumption position
#   Columns:
#     1) Outward switching
#     2) Inward switching
#     3) Net effect
#
# Important:
#   The investment-consumption row in the main figure uses the
#   Abou-Chadi/Wagner operationalisation. The cultural and state-economy
#   rows are taken from the main model output folder.
#
# Appendix figure:
#   2 rows x 3 columns:
#     1) Narrow investment-consumption scale
#     2) Education expansion vs. education limitation
#   Columns:
#     1) Outward switching
#     2) Inward switching
#     3) Net effect
#
# Note:
#   The model script may have saved the first predictor label as
#   "Change in SD education-labour position". This reporting script
#   relabels it as "Change in SD investment-consumption position"
#   without changing any estimates.
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

project_dir <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching"

analysis_dir <- file.path(project_dir, "data", "analysis")

model_dir <- file.path(
  analysis_dir,
  "models",
  "sd_restricted_choice_set_mixed_conditional_logit_country_re_supply_position_change"
)

figure_dir <- file.path(
  model_dir,
  "figures"
)

table_dir <- file.path(
  model_dir,
  "tables"
)

alt_model_root_dir <- file.path(
  analysis_dir,
  "models"
)

alt_figure_dir <- file.path(
  alt_model_root_dir,
  "figures"
)

alt_table_dir <- file.path(
  alt_model_root_dir,
  "tables"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(alt_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(alt_table_dir, recursive = TRUE, showWarnings = FALSE)

path_coefficients <- file.path(
  model_dir,
  "restricted_choice_set_coefficients.csv"
)

path_final_results <- file.path(
  model_dir,
  "final_supply_position_change_model_results.rds"
)

path_ame_candidates <- c(
  file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.rds"),
  file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.csv")
)

path_net_candidates <- c(
  file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.rds"),
  file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.csv")
)

path_alt_ames <- file.path(
  alt_model_root_dir,
  "supply_position_change_all_operationalisations_ames_with_delta_ci.rds"
)

path_alt_net <- file.path(
  alt_model_root_dir,
  "supply_position_change_all_operationalisations_net_effects_with_delta_ci.rds"
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

supply_predictors <- c(
  "sd_investmentconsumption_move_std",
  "sd_stateconomy_move_std",
  "sd_libcons_move_std"
)

supply_predictor_pattern <- paste(
  supply_predictors,
  collapse = "|"
)

relabel_predictor <- function(df) {
  if (!"predictor_label" %in% names(df)) {
    return(df)
  }
  
  df %>%
    dplyr::mutate(
      predictor_label = dplyr::recode(
        predictor_label,
        "Change in SD education-labour position" =
          "Change in SD investment-consumption position"
      )
    )
}

predictor_order <- c(
  "Change in SD cultural position",
  "Change in SD state-economy position",
  "Change in SD investment-consumption position"
)

actor_order_plot <- c(
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
    add_missing_column("delta_se") %>%
    add_missing_column("std.error") %>%
    add_missing_column("n_delta_draws_success") %>%
    add_missing_column("n_success")
  
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

check_required_cols <- function(df, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

# Scales are shared by the main and appendix AME/net-effect figures.
x_scale_outward <- scale_x_continuous(
  limits = c(-0.015, 0.015),
  breaks = seq(-0.015, 0.015, length.out = 7),
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

# ------------------------------------------------
# 3. Quick coefficient results: log-odds
# ------------------------------------------------

coef_results <- readr::read_csv(
  path_coefficients,
  show_col_types = FALSE
) %>%
  relabel_predictor()

coef_plot_data <- coef_results %>%
  dplyr::filter(
    stringr::str_detect(term, supply_predictor_pattern),
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
      levels = actor_order_plot
    )
  ) %>%
  dplyr::filter(
    !is.na(predictor_label),
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

fig_supply_logodds <- ggplot(
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

print(fig_supply_logodds)

ggsave(
  filename = file.path(figure_dir, "supply_position_change_logodds_outward_inward_quick.pdf"),
  plot = fig_supply_logodds,
  width = 8,
  height = 5.8
)

ggsave(
  filename = file.path(figure_dir, "supply_position_change_logodds_outward_inward_quick.png"),
  plot = fig_supply_logodds,
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
  file.path(table_dir, "supply_position_change_logodds_quick_reporting_table.csv")
)

readr::write_csv(
  coef_latex_table,
  file.path(table_dir, "supply_position_change_logodds_quick_latex_ready.csv")
)

cat("\n================================================\n")
cat("Quick log-odds coefficient table\n")
cat("================================================\n")
print(coef_table, n = Inf, width = Inf)

# ------------------------------------------------
# 4a. Main AME and net-effect results
# ------------------------------------------------

if (uncertainty_available) {
  
  ame_results_main_folder <- read_uncertainty_file(path_ame_results) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  net_results_main_folder <- read_uncertainty_file(path_net_results) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  final_results <- if (file.exists(path_final_results)) {
    readRDS(path_final_results)
  } else {
    NULL
  }
  
  required_ame_cols <- c(
    "predictor",
    "predictor_label",
    "flow",
    "flow_label",
    "alt",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high",
    "n_uncertainty_success"
  )
  
  required_net_cols <- c(
    "predictor",
    "predictor_label",
    "actor_label",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high",
    "n_uncertainty_success"
  )
  
  check_required_cols(
    ame_results_main_folder,
    required_ame_cols,
    "Main-folder AME uncertainty results"
  )
  
  check_required_cols(
    net_results_main_folder,
    required_net_cols,
    "Main-folder net-effect uncertainty results"
  )
  
  if (!file.exists(path_alt_ames) || !file.exists(path_alt_net)) {
    stop(
      "The main plot requires the Abou-Chadi/Wagner investment-consumption results. Expected files were not found:\n",
      path_alt_ames, "\n",
      path_alt_net
    )
  }
  
  alt_ame_results_all <- readRDS(path_alt_ames) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  alt_net_results_all <- readRDS(path_alt_net) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  check_required_cols(
    alt_ame_results_all,
    c(required_ame_cols, "operationalisation"),
    "All-operationalisations AME uncertainty results"
  )
  
  check_required_cols(
    alt_net_results_all,
    c(required_net_cols, "operationalisation"),
    "All-operationalisations net-effect uncertainty results"
  )
  
  main_ame_investmentconsumption <- alt_ame_results_all %>%
    dplyr::filter(
      operationalisation == "marpor_abou_chadi_wagner",
      predictor == "sd_investmentconsumption_move_std"
    )
  
  main_net_investmentconsumption <- alt_net_results_all %>%
    dplyr::filter(
      operationalisation == "marpor_abou_chadi_wagner",
      predictor == "sd_investmentconsumption_move_std"
    )
  
  if (nrow(main_ame_investmentconsumption) == 0) {
    stop(
      "No Abou-Chadi/Wagner AME rows were found for sd_investmentconsumption_move_std."
    )
  }
  
  if (nrow(main_net_investmentconsumption) == 0) {
    stop(
      "No Abou-Chadi/Wagner net-effect rows were found for sd_investmentconsumption_move_std."
    )
  }
  
  ame_results <- ame_results_main_folder %>%
    dplyr::filter(
      predictor %in% c("sd_stateconomy_move_std", "sd_libcons_move_std")
    ) %>%
    dplyr::bind_rows(main_ame_investmentconsumption) %>%
    relabel_predictor()
  
  net_results <- net_results_main_folder %>%
    dplyr::filter(
      predictor %in% c("sd_stateconomy_move_std", "sd_libcons_move_std")
    ) %>%
    dplyr::bind_rows(main_net_investmentconsumption) %>%
    relabel_predictor()
  
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
    ) %>%
    dplyr::filter(
      !is.na(predictor_label)
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
    ) %>%
    dplyr::filter(
      !is.na(alternative_label)
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
    ) %>%
    dplyr::filter(
      !is.na(predictor_label),
      !is.na(actor_label)
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
  
  main_panel_count <- combined_plot_data %>%
    dplyr::count(predictor_label, flow_label)
  
  cat("\n================================================\n")
  cat("Main figure panel check\n")
  cat("================================================\n")
  print(main_panel_count, n = Inf, width = Inf)
  
  if (nrow(main_panel_count) != 9) {
    stop(
      "The main figure does not contain 3 predictors x 3 flow columns. Check combined_plot_data."
    )
  }
  
  fig_supply_combined <- ggplot(
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
      scales = "free_x",
      drop = FALSE
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
  
  print(fig_supply_combined)
  
  ggsave(
    filename = file.path(figure_dir, "supply_position_change_outward_inward_net_effects.pdf"),
    plot = fig_supply_combined,
    width = 9,
    height = 6.5
  )
  
  ggsave(
    filename = file.path(figure_dir, "supply_position_change_outward_inward_net_effects.png"),
    plot = fig_supply_combined,
    width = 9,
    height = 6.5,
    dpi = 300
  )
  
  x_scale_net_only <- scale_x_continuous(
    limits = c(-0.010, 0.010),
    breaks = seq(-0.010, 0.010, length.out = 5),
    labels = function(x) sprintf("%.1f", 100 * x)
  )
  
  fig_supply_net <- ggplot(
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
  
  print(fig_supply_net)
  
  ggsave(
    filename = file.path(figure_dir, "supply_position_change_net_effects_only.pdf"),
    plot = fig_supply_net,
    width = 7,
    height = 6
  )
  
  ggsave(
    filename = file.path(figure_dir, "supply_position_change_net_effects_only.png"),
    plot = fig_supply_net,
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
      conf_high_pp = 100 * conf.high,
      n_uncertainty_success
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
      conf_high_pp = 100 * conf.high,
      n_uncertainty_success
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
      conf_high_pp = 100 * conf.high,
      n_uncertainty_success
    )
  
  readr::write_csv(
    combined_table,
    file.path(table_dir, "supply_position_change_outward_inward_net_reporting_table.csv")
  )
  
  readr::write_csv(
    net_table,
    file.path(table_dir, "supply_position_change_net_effects_reporting_table.csv")
  )
  
  readr::write_csv(
    ame_table,
    file.path(table_dir, "supply_position_change_ame_reporting_table.csv")
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
      ci,
      n_uncertainty_success
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
      ci,
      n_uncertainty_success
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
      ci,
      n_uncertainty_success
    )
  
  readr::write_csv(
    combined_latex_table,
    file.path(table_dir, "supply_position_change_outward_inward_net_latex_ready.csv")
  )
  
  readr::write_csv(
    net_latex_table,
    file.path(table_dir, "supply_position_change_net_effects_latex_ready.csv")
  )
  
  readr::write_csv(
    ame_latex_table,
    file.path(table_dir, "supply_position_change_ames_latex_ready.csv")
  )
  
  cat("\n================================================\n")
  cat("AME and net-effect combined table, excluding non-voting\n")
  cat("Investment-consumption row uses the Abou-Chadi/Wagner operationalisation.\n")
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
# 4b. Appendix plot: other investment-consumption operationalisations
# ------------------------------------------------

if (file.exists(path_alt_ames) && file.exists(path_alt_net)) {
  
  cat("\nAppendix investment-consumption operationalisation AME file found:\n")
  cat(path_alt_ames, "\n")
  cat("\nAppendix investment-consumption operationalisation net-effect file found:\n")
  cat(path_alt_net, "\n")
  
  alt_ame_results <- readRDS(path_alt_ames) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  alt_net_results <- readRDS(path_alt_net) %>%
    normalise_uncertainty_summary() %>%
    relabel_predictor()
  
  required_ame_cols_appendix <- c(
    "operationalisation",
    "predictor",
    "predictor_label",
    "flow",
    "flow_label",
    "alt",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high",
    "n_uncertainty_success"
  )
  
  required_net_cols_appendix <- c(
    "operationalisation",
    "predictor",
    "predictor_label",
    "actor_label",
    "point_estimate",
    "uncertainty_se",
    "conf.low",
    "conf.high",
    "n_uncertainty_success"
  )
  
  check_required_cols(
    alt_ame_results,
    required_ame_cols_appendix,
    "Appendix all-operationalisations AME uncertainty results"
  )
  
  check_required_cols(
    alt_net_results,
    required_net_cols_appendix,
    "Appendix all-operationalisations net-effect uncertainty results"
  )
  
  appendix_operationalisation_order <- c(
    "Narrow investment-consumption scale",
    "Education expansion vs. education limitation"
  )
  
  appendix_ame_plot_data <- alt_ame_results %>%
    dplyr::filter(
      operationalisation %in% c(
        "marpor_complete",
        "marpor_education_only"
      ),
      predictor == "sd_investmentconsumption_move_std",
      flow %in% c("outward", "inward")
    ) %>%
    dplyr::mutate(
      operationalisation_label = dplyr::case_when(
        operationalisation == "marpor_complete" ~
          "Narrow investment-consumption scale",
        operationalisation == "marpor_education_only" ~
          "Education expansion vs. education limitation",
        TRUE ~ as.character(operationalisation)
      ),
      operationalisation_label = factor(
        operationalisation_label,
        levels = appendix_operationalisation_order
      ),
      flow_label = dplyr::case_when(
        flow == "outward" ~ "Outward switching",
        flow == "inward" ~ "Inward switching",
        TRUE ~ as.character(flow_label)
      ),
      flow_label = factor(
        flow_label,
        levels = c("Outward switching", "Inward switching", "Net effect")
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
    ) %>%
    dplyr::filter(
      !is.na(operationalisation_label)
    )
  
  appendix_ame_competitor_plot_data <- appendix_ame_plot_data %>%
    dplyr::filter(
      !alternative_label %in% c("Retention", "Not to SD", "Non-voting")
    ) %>%
    dplyr::mutate(
      alternative_label = factor(
        as.character(alternative_label),
        levels = actor_order_plot
      )
    ) %>%
    dplyr::filter(
      !is.na(alternative_label)
    )
  
  appendix_net_plot_data <- alt_net_results %>%
    dplyr::filter(
      operationalisation %in% c(
        "marpor_complete",
        "marpor_education_only"
      ),
      predictor == "sd_investmentconsumption_move_std",
      actor_label != "Non-voting"
    ) %>%
    dplyr::mutate(
      operationalisation_label = dplyr::case_when(
        operationalisation == "marpor_complete" ~
          "Narrow investment-consumption scale",
        operationalisation == "marpor_education_only" ~
          "Education expansion vs. education limitation",
        TRUE ~ as.character(operationalisation)
      ),
      operationalisation_label = factor(
        operationalisation_label,
        levels = appendix_operationalisation_order
      ),
      actor_label = factor(
        actor_label,
        levels = actor_order_plot
      ),
      flow = "net",
      flow_label = factor(
        "Net effect",
        levels = c("Outward switching", "Inward switching", "Net effect")
      )
    ) %>%
    dplyr::filter(
      !is.na(operationalisation_label),
      !is.na(actor_label)
    )
  
  appendix_outward_for_combined <- appendix_ame_competitor_plot_data %>%
    dplyr::filter(flow == "outward") %>%
    dplyr::transmute(
      operationalisation,
      operationalisation_label,
      predictor,
      predictor_label,
      flow = "outward",
      flow_label = factor(
        "Outward switching",
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label = alternative_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  appendix_inward_for_combined <- appendix_ame_competitor_plot_data %>%
    dplyr::filter(flow == "inward") %>%
    dplyr::transmute(
      operationalisation,
      operationalisation_label,
      predictor,
      predictor_label,
      flow = "inward",
      flow_label = factor(
        "Inward switching",
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label = alternative_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  appendix_net_for_combined <- appendix_net_plot_data %>%
    dplyr::transmute(
      operationalisation,
      operationalisation_label,
      predictor,
      predictor_label,
      flow = "net",
      flow_label = factor(
        "Net effect",
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label,
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )
  
  appendix_combined_plot_data <- dplyr::bind_rows(
    appendix_outward_for_combined,
    appendix_inward_for_combined,
    appendix_net_for_combined
  ) %>%
    dplyr::mutate(
      operationalisation_label = factor(
        operationalisation_label,
        levels = appendix_operationalisation_order
      ),
      flow_label = factor(
        flow_label,
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label = factor(
        actor_label,
        levels = actor_order_plot
      )
    )
  
  appendix_panel_count <- appendix_combined_plot_data %>%
    dplyr::count(operationalisation_label, flow_label)
  
  cat("\n================================================\n")
  cat("Appendix figure panel check\n")
  cat("================================================\n")
  print(appendix_panel_count, n = Inf, width = Inf)
  
  if (nrow(appendix_outward_for_combined) == 0) {
    stop("No appendix outward AME rows were found.")
  }
  
  if (nrow(appendix_inward_for_combined) == 0) {
    stop("No appendix inward AME rows were found.")
  }
  
  if (nrow(appendix_net_for_combined) == 0) {
    stop(
      "No appendix net-effect rows were found. Check whether path_alt_net contains marpor_complete and marpor_education_only rows for sd_investmentconsumption_move_std."
    )
  }
  
  if (nrow(appendix_panel_count) != 6) {
    stop(
      "The appendix figure does not contain 2 operationalisations x 3 flow columns. Check appendix_combined_plot_data."
    )
  }
  
  fig_supply_alt_operationalisations <- ggplot(
    appendix_combined_plot_data,
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
      operationalisation_label ~ flow_label,
      scales = "free_x",
      drop = FALSE
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
      strip.text = element_text(face = "bold", size = 9),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      panel.spacing = unit(1.1, "lines")
    )
  
  print(fig_supply_alt_operationalisations)
  
  ggsave(
    filename = file.path(
      alt_figure_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net.pdf"
    ),
    plot = fig_supply_alt_operationalisations,
    width = 11,
    height = 5.8
  )
  
  ggsave(
    filename = file.path(
      alt_figure_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net.png"
    ),
    plot = fig_supply_alt_operationalisations,
    width = 11,
    height = 5.8,
    dpi = 300
  )
  
  appendix_table <- appendix_combined_plot_data %>%
    dplyr::arrange(
      operationalisation_label,
      flow_label,
      actor_label
    ) %>%
    dplyr::transmute(
      operationalisation = as.character(operationalisation_label),
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
      conf_high_pp = 100 * conf.high,
      n_uncertainty_success
    )
  
  appendix_latex_table <- appendix_table %>%
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
      operationalisation,
      predictor,
      flow,
      competitor,
      estimate_se,
      ci,
      n_uncertainty_success
    )
  
  readr::write_csv(
    appendix_table,
    file.path(
      alt_table_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net_reporting_table.csv"
    )
  )
  
  readr::write_csv(
    appendix_latex_table,
    file.path(
      alt_table_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net_latex_ready.csv"
    )
  )
  
  cat("\n================================================\n")
  cat("Appendix investment-consumption operationalisations, excluding non-voting\n")
  cat("================================================\n")
  print(appendix_table, n = Inf, width = Inf)
  
} else {
  
  cat("\nAppendix operationalisation files not found. Skipping appendix investment-consumption operationalisation plot.\n")
  cat("Expected files:\n")
  cat(path_alt_ames, "\n")
  cat(path_alt_net, "\n")
}

# ------------------------------------------------
# 4a. Console summary of plotted supply effects and p-values
# ------------------------------------------------

cat("\n================================================\n")
cat("Plotted supply-position AMEs and net effects with approximate p-values\n")
cat("Investment-consumption row uses the Abou-Chadi/Wagner operationalisation.\n")
cat("================================================\n")

supply_combined_console <- combined_plot_data %>%
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
    estimate_pp = format_num(100 * point_estimate, 3),
    std_error_pp = format_num(100 * uncertainty_se, 3),
    conf_low_pp = format_num(100 * conf.low, 3),
    conf_high_pp = format_num(100 * conf.high, 3),
    p_value = format_num(p_value, 3)
  )

print(supply_combined_console, n = Inf, width = Inf)

cat("\nNote: AME and net-effect p-values are approximate two-sided normal p-values based on the delta-method standard errors.\n")

# ------------------------------------------------
# 4a. Main specification figures, one by one
# ------------------------------------------------

if (uncertainty_available) {
  
  main_spec_figure_dir <- file.path(
    figure_dir,
    "main_specifications_separate"
  )
  
  dir.create(main_spec_figure_dir, recursive = TRUE, showWarnings = FALSE)
  
  main_spec_file_labels <- c(
    "Change in SD cultural position" = "cultural",
    "Change in SD state-economy position" = "state_economy",
    "Change in SD investment-consumption position" = "investment_consumption"
  )
  
  main_spec_plot_titles <- c(
    "Change in SD cultural position" = "Cultural position",
    "Change in SD state-economy position" = "Economic position",
    "Change in SD investment-consumption position" = "Social investment position"
  )
  
  for (this_predictor in names(main_spec_file_labels)) {
    
    this_plot_data <- combined_plot_data %>%
      dplyr::filter(
        as.character(predictor_label) == this_predictor
      )
    
    if (nrow(this_plot_data) == 0) {
      stop("No rows found for: ", this_predictor)
    }
    
    fig_main_spec <- ggplot(
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
        title = main_spec_plot_titles[[this_predictor]],
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
    
    print(fig_main_spec)

    
  }
}

# ------------------------------------------------
# 4b. Console summary of alternative investment-compensation
#     operationalisations and p-values
# ------------------------------------------------

cat("\n================================================\n")
cat("Alternative investment-compensation operationalisations with approximate p-values\n")
cat("================================================\n")

appendix_console <- appendix_combined_plot_data %>%
  dplyr::mutate(
    z_value = point_estimate / uncertainty_se,
    p_value = 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  ) %>%
  dplyr::arrange(
    operationalisation_label,
    flow_label,
    actor_label
  ) %>%
  dplyr::transmute(
    operationalisation = as.character(operationalisation_label),
    flow = as.character(flow_label),
    competitor = as.character(actor_label),
    estimate_pp = format_num(100 * point_estimate, 3),
    std_error_pp = format_num(100 * uncertainty_se, 3),
    conf_low_pp = format_num(100 * conf.low, 3),
    conf_high_pp = format_num(100 * conf.high, 3),
    p_value = format_num(p_value, 3)
  )

print(appendix_console, n = Inf, width = Inf)

cat("\nNote: P-values are approximate two-sided normal p-values based on the delta-method standard errors.\n")



# ------------------------------------------------
# 5. Files written
# ------------------------------------------------

cat("\n================================================\n")
cat("Files written\n")
cat("================================================\n")

cat("Quick coefficient figures:\n")
cat(file.path(figure_dir, "supply_position_change_logodds_outward_inward_quick.pdf"), "\n")
cat(file.path(figure_dir, "supply_position_change_logodds_outward_inward_quick.png"), "\n")

cat("\nQuick coefficient tables:\n")
cat(file.path(table_dir, "supply_position_change_logodds_quick_reporting_table.csv"), "\n")
cat(file.path(table_dir, "supply_position_change_logodds_quick_latex_ready.csv"), "\n")

if (uncertainty_available) {
  
  cat("\nMain AME and net-effect figures:\n")
  cat(file.path(figure_dir, "supply_position_change_outward_inward_net_effects.pdf"), "\n")
  cat(file.path(figure_dir, "supply_position_change_outward_inward_net_effects.png"), "\n")
  cat(file.path(figure_dir, "supply_position_change_net_effects_only.pdf"), "\n")
  cat(file.path(figure_dir, "supply_position_change_net_effects_only.png"), "\n")
  
  cat("\nMain AME and net-effect tables:\n")
  cat(file.path(table_dir, "supply_position_change_outward_inward_net_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "supply_position_change_net_effects_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "supply_position_change_ame_reporting_table.csv"), "\n")
  cat(file.path(table_dir, "supply_position_change_outward_inward_net_latex_ready.csv"), "\n")
  cat(file.path(table_dir, "supply_position_change_net_effects_latex_ready.csv"), "\n")
  cat(file.path(table_dir, "supply_position_change_ames_latex_ready.csv"), "\n")
  
} else {
  
  cat("\nMain AME and net-effect figures and tables skipped because uncertainty files are not available.\n")
}

if (file.exists(path_alt_ames) && file.exists(path_alt_net)) {
  
  cat("\nAppendix investment-consumption operationalisation figures:\n")
  cat(
    file.path(
      alt_figure_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net.pdf"
    ),
    "\n"
  )
  cat(
    file.path(
      alt_figure_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net.png"
    ),
    "\n"
  )
  
  cat("\nAppendix investment-consumption operationalisation tables:\n")
  cat(
    file.path(
      alt_table_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net_reporting_table.csv"
    ),
    "\n"
  )
  cat(
    file.path(
      alt_table_dir,
      "supply_position_change_appendix_investment_consumption_operationalisations_outward_inward_net_latex_ready.csv"
    ),
    "\n"
  )
  
} else {
  
  cat("\nAppendix investment-consumption operationalisation figures and tables skipped because the combined operationalisation files are not available.\n")
}

cat("\nScript completed successfully.\n")
