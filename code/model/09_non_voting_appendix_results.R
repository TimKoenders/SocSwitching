# ================================================================
# 09_non_voting_appendix_results.R
# Appendix reporting script for non-voting results
#
# Purpose:
#   Plot the non-voting category that is omitted from the main
#   demand-salience and supply-position figures.
#
# Inputs:
#   This script reads the AME and net-effect uncertainty files written
#   by the main model scripts and follows the plotting style used in:
#     02_demand_salience_results.R
#     04_supply_position_results.R
# ================================================================

options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(grid)
  library(ggh4x)
})

# ------------------------------------------------
# 1. Paths
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

analysis_dir <- file.path(project_dir, "data", "analysis")
model_root_dir <- file.path(analysis_dir, "models")

salience_model_dir <- file.path(
  model_root_dir,
  "salience_change"
)

supply_model_dir <- file.path(
  model_root_dir,
  "supply_position_change"
)

salience_figure_dir <- file.path(
  model_root_dir,
  "figures",
  "non_voting_appendix"
)

salience_table_dir <- file.path(
  model_root_dir,
  "tables",
  "non_voting_appendix"
)

supply_figure_dir <- file.path(
  model_root_dir,
  "figures",
  "non_voting_appendix"
)

supply_table_dir <- file.path(
  model_root_dir,
  "tables",
  "non_voting_appendix"
)

alt_figure_dir <- file.path(
  model_root_dir,
  "figures",
  "non_voting_appendix"
)

alt_table_dir <- file.path(
  model_root_dir,
  "tables",
  "non_voting_appendix"
)

dir.create(salience_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(salience_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supply_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supply_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(alt_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(alt_table_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------
# 2. Helpers
# ------------------------------------------------

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

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    NA_character_
  } else {
    existing[1]
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

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
}

relabel_supply_predictor <- function(df) {
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

make_non_voting_combined_data <- function(
  ame_results,
  net_results,
  predictor_order,
  row_var = "predictor_label"
) {

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

  if (row_var != "predictor_label") {
    required_ame_cols <- c(required_ame_cols, row_var)
    required_net_cols <- c(required_net_cols, row_var)
  }

  check_required_cols(ame_results, required_ame_cols, "AME uncertainty results")
  check_required_cols(net_results, required_net_cols, "Net-effect uncertainty results")

  row_vars_to_keep <- setdiff(row_var, "predictor_label")

  ame_non_voting <- ame_results %>%
    dplyr::mutate(
      predictor_label = factor(
        predictor_label,
        levels = predictor_order
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
      )
    ) %>%
    dplyr::filter(
      alternative_label == "Non-voting",
      flow %in% c("outward", "inward"),
      !is.na(predictor_label)
    )

  net_non_voting <- net_results %>%
    dplyr::mutate(
      predictor_label = factor(
        predictor_label,
        levels = predictor_order
      ),
      flow = "net",
      flow_label = factor(
        "Net effect",
        levels = c("Outward switching", "Inward switching", "Net effect")
      )
    ) %>%
    dplyr::filter(
      actor_label == "Non-voting",
      !is.na(predictor_label)
    )

  ame_outward <- ame_non_voting %>%
    dplyr::filter(flow == "outward") %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(row_vars_to_keep)),
      predictor,
      predictor_label,
      flow = "outward",
      flow_label,
      actor_label = "Non-voting",
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )

  ame_inward <- ame_non_voting %>%
    dplyr::filter(flow == "inward") %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(row_vars_to_keep)),
      predictor,
      predictor_label,
      flow = "inward",
      flow_label,
      actor_label = "Non-voting",
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )

  net_data <- net_non_voting %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(row_vars_to_keep)),
      predictor,
      predictor_label,
      flow = "net",
      flow_label,
      actor_label = "Non-voting",
      point_estimate,
      uncertainty_se,
      conf.low,
      conf.high,
      n_uncertainty_success
    )

  dplyr::bind_rows(
    ame_outward,
    ame_inward,
    net_data
  ) %>%
    dplyr::mutate(
      flow_label = factor(
        flow_label,
        levels = c("Outward switching", "Inward switching", "Net effect")
      ),
      actor_label = factor(
        actor_label,
        levels = "Non-voting"
      )
    )
}

nice_symmetric_limit <- function(x, padding = 1.08) {
  x_max <- max(abs(x), na.rm = TRUE)

  if (!is.finite(x_max) || x_max == 0) {
    return(0.01)
  }

  padded_max <- x_max * padding
  magnitude <- 10^floor(log10(padded_max))
  ceiling(padded_max / magnitude) * magnitude
}

make_x_scales <- function(outward_limit, inward_limit, net_limit) {
  list(
    outward = scale_x_continuous(
      limits = c(-outward_limit, outward_limit),
      breaks = seq(-outward_limit, outward_limit, length.out = 7),
      labels = function(x) sprintf("%.1f", 100 * x)
    ),
    inward = scale_x_continuous(
      limits = c(-inward_limit, inward_limit),
      breaks = seq(-inward_limit, inward_limit, length.out = 5),
      labels = function(x) sprintf("%.1f", 100 * x)
    ),
    net = scale_x_continuous(
      limits = c(-net_limit, net_limit),
      breaks = seq(-net_limit, net_limit, length.out = 5),
      labels = function(x) sprintf("%.1f", 100 * x)
    )
  )
}

make_x_scales_from_data <- function(plot_data) {
  flow_limit <- function(flow_name) {
    flow_data <- plot_data %>%
      dplyr::filter(flow_label == flow_name)

    nice_symmetric_limit(
      c(flow_data$conf.low, flow_data$conf.high)
    )
  }

  make_x_scales(
    outward_limit = flow_limit("Outward switching"),
    inward_limit = flow_limit("Inward switching"),
    net_limit = flow_limit("Net effect")
  )
}

plot_non_voting_effects <- function(
  plot_data,
  row_facet,
  x_scales,
  strip_text_size = 11
) {
  ggplot(
    plot_data,
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
      stats::as.formula(paste(row_facet, "~ flow_label")),
      scales = "free_x",
      drop = FALSE
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        flow_label == "Outward switching" ~ x_scales$outward,
        flow_label == "Inward switching" ~ x_scales$inward,
        flow_label == "Net effect" ~ x_scales$net
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
      strip.text = element_text(face = "bold", size = strip_text_size),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      panel.spacing = unit(1.3, "lines")
    )
}

write_non_voting_outputs <- function(
  plot_data,
  figure,
  figure_dir,
  table_dir,
  file_stub,
  width,
  height,
  table_group_cols = character()
) {

  ggsave(
    filename = file.path(figure_dir, paste0(file_stub, ".pdf")),
    plot = figure,
    width = width,
    height = height
  )

  ggsave(
    filename = file.path(figure_dir, paste0(file_stub, ".png")),
    plot = figure,
    width = width,
    height = height,
    dpi = 300
  )

  arrange_cols <- c(table_group_cols, "predictor_label", "flow_label")

  table <- plot_data %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols))) %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(table_group_cols)),
      predictor = as.character(predictor_label),
      flow = as.character(flow_label),
      category = as.character(actor_label),
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

  latex_table <- table %>%
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
      dplyr::all_of(table_group_cols),
      predictor,
      flow,
      category,
      estimate_se,
      ci,
      n_uncertainty_success
    )

  readr::write_csv(
    table,
    file.path(table_dir, paste0(file_stub, "_reporting_table.csv"))
  )

  readr::write_csv(
    latex_table,
    file.path(table_dir, paste0(file_stub, "_latex_ready.csv"))
  )

  table
}

read_model_uncertainty <- function(model_dir) {
  path_ame_results <- first_existing_path(
    c(
      file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.rds"),
      file.path(model_dir, "restricted_choice_set_ames_with_delta_ci.csv")
    )
  )

  path_net_results <- first_existing_path(
    c(
      file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.rds"),
      file.path(model_dir, "restricted_choice_set_net_effects_with_delta_ci.csv")
    )
  )

  if (is.na(path_ame_results) || is.na(path_net_results)) {
    stop(
      "AME and net-effect uncertainty files were not found in: ",
      model_dir
    )
  }

  list(
    ame = read_uncertainty_file(path_ame_results) %>%
      normalise_uncertainty_summary(),
    net = read_uncertainty_file(path_net_results) %>%
      normalise_uncertainty_summary(),
    path_ame = path_ame_results,
    path_net = path_net_results
  )
}

# ------------------------------------------------
# 3. Demand-salience non-voting results
# ------------------------------------------------

salience_predictor_order <- c(
  "Change in immigration salience",
  "Change in environmental salience",
  "Change in unemployment salience"
)

salience_results <- read_model_uncertainty(salience_model_dir)

cat("\nDemand-salience AME file:\n")
cat(salience_results$path_ame, "\n")
cat("\nDemand-salience net-effect file:\n")
cat(salience_results$path_net, "\n")

salience_non_voting_plot_data <- make_non_voting_combined_data(
  ame_results = salience_results$ame,
  net_results = salience_results$net,
  predictor_order = salience_predictor_order
)

salience_panel_count <- salience_non_voting_plot_data %>%
  dplyr::count(predictor_label, flow_label)

cat("\n================================================\n")
cat("Demand-salience non-voting panel check\n")
cat("================================================\n")
print(salience_panel_count, n = Inf, width = Inf)

if (nrow(salience_panel_count) != 9) {
  stop(
    "Demand-salience non-voting figure does not contain 3 predictors x 3 flow columns."
  )
}

fig_salience_non_voting <- plot_non_voting_effects(
  plot_data = salience_non_voting_plot_data,
  row_facet = "predictor_label",
  x_scales = make_x_scales_from_data(salience_non_voting_plot_data)
)

print(fig_salience_non_voting)

salience_non_voting_table <- write_non_voting_outputs(
  plot_data = salience_non_voting_plot_data,
  figure = fig_salience_non_voting,
  figure_dir = salience_figure_dir,
  table_dir = salience_table_dir,
  file_stub = "salience_change_non_voting_outward_inward_net",
  width = 9,
  height = 4.8
)

cat("\n================================================\n")
cat("Demand-salience non-voting AMEs and net effects\n")
cat("================================================\n")
print(salience_non_voting_table, n = Inf, width = Inf)

# ------------------------------------------------
# 4. Supply-position non-voting results
# ------------------------------------------------

supply_predictor_order <- c(
  "Change in SD cultural position",
  "Change in SD state-economy position",
  "Change in SD investment-consumption position"
)

supply_results_main_folder <- read_model_uncertainty(supply_model_dir)

cat("\nSupply-position AME file:\n")
cat(supply_results_main_folder$path_ame, "\n")
cat("\nSupply-position net-effect file:\n")
cat(supply_results_main_folder$path_net, "\n")

path_alt_ames <- file.path(
  model_root_dir,
  "supply_position_change_all_operationalisations_ames_with_delta_ci.rds"
)

path_alt_net <- file.path(
  model_root_dir,
  "supply_position_change_all_operationalisations_net_effects_with_delta_ci.rds"
)

has_alt_supply_operationalisations <- file.exists(path_alt_ames) &&
  file.exists(path_alt_net)

supply_ame_results_main_folder <- supply_results_main_folder$ame %>%
  relabel_supply_predictor()

supply_net_results_main_folder <- supply_results_main_folder$net %>%
  relabel_supply_predictor()

if (has_alt_supply_operationalisations) {
  alt_ame_results_all <- readRDS(path_alt_ames) %>%
    normalise_uncertainty_summary() %>%
    relabel_supply_predictor()
  
  alt_net_results_all <- readRDS(path_alt_net) %>%
    normalise_uncertainty_summary() %>%
    relabel_supply_predictor()
  
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
  
  supply_ame_results <- supply_ame_results_main_folder %>%
    dplyr::filter(
      predictor %in% c("sd_stateconomy_move_std", "sd_libcons_move_std")
    ) %>%
    dplyr::bind_rows(main_ame_investmentconsumption) %>%
    relabel_supply_predictor()
  
  supply_net_results <- supply_net_results_main_folder %>%
    dplyr::filter(
      predictor %in% c("sd_stateconomy_move_std", "sd_libcons_move_std")
    ) %>%
    dplyr::bind_rows(main_net_investmentconsumption) %>%
    relabel_supply_predictor()
} else {
  cat(
    "\nCombined supply-position operationalisation files not found; ",
    "using the main supply-position results for all non-voting appendix rows.\n",
    sep = ""
  )
  
  supply_ame_results <- supply_ame_results_main_folder
  supply_net_results <- supply_net_results_main_folder
}

supply_non_voting_plot_data <- make_non_voting_combined_data(
  ame_results = supply_ame_results,
  net_results = supply_net_results,
  predictor_order = supply_predictor_order
)

supply_panel_count <- supply_non_voting_plot_data %>%
  dplyr::count(predictor_label, flow_label)

cat("\n================================================\n")
cat("Supply-position non-voting panel check\n")
cat("================================================\n")
print(supply_panel_count, n = Inf, width = Inf)

if (nrow(supply_panel_count) != 9) {
  stop(
    "Supply-position non-voting figure does not contain 3 predictors x 3 flow columns."
  )
}

fig_supply_non_voting <- plot_non_voting_effects(
  plot_data = supply_non_voting_plot_data,
  row_facet = "predictor_label",
  x_scales = make_x_scales_from_data(supply_non_voting_plot_data)
)

print(fig_supply_non_voting)

supply_non_voting_table <- write_non_voting_outputs(
  plot_data = supply_non_voting_plot_data,
  figure = fig_supply_non_voting,
  figure_dir = supply_figure_dir,
  table_dir = supply_table_dir,
  file_stub = "supply_position_change_non_voting_outward_inward_net",
  width = 9,
  height = 4.8
)

cat("\n================================================\n")
cat("Supply-position non-voting AMEs and net effects\n")
if (has_alt_supply_operationalisations) {
  cat("Investment-consumption row uses the Abou-Chadi/Wagner operationalisation.\n")
} else {
  cat("Rows use the main supply-position model results.\n")
}
cat("================================================\n")
print(supply_non_voting_table, n = Inf, width = Inf)

# ------------------------------------------------
# 5. Supply-position alternative operationalisations
# ------------------------------------------------

appendix_operationalisation_order <- c(
  "Narrow investment-consumption scale",
  "Education expansion vs. education limitation"
)

if (has_alt_supply_operationalisations) {
alt_ame_results <- alt_ame_results_all %>%
  dplyr::filter(
    operationalisation %in% c(
      "marpor_complete",
      "marpor_education_only"
    ),
    predictor == "sd_investmentconsumption_move_std"
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
    )
  )

alt_net_results <- alt_net_results_all %>%
  dplyr::filter(
    operationalisation %in% c(
      "marpor_complete",
      "marpor_education_only"
    ),
    predictor == "sd_investmentconsumption_move_std"
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
    )
  )

appendix_non_voting_plot_data <- make_non_voting_combined_data(
  ame_results = alt_ame_results,
  net_results = alt_net_results,
  predictor_order = supply_predictor_order,
  row_var = "operationalisation_label"
) %>%
  dplyr::mutate(
    operationalisation_label = factor(
      operationalisation_label,
      levels = appendix_operationalisation_order
    )
  )

appendix_panel_count <- appendix_non_voting_plot_data %>%
  dplyr::count(operationalisation_label, flow_label)

cat("\n================================================\n")
cat("Supply-position alternative-operationalisation non-voting panel check\n")
cat("================================================\n")
print(appendix_panel_count, n = Inf, width = Inf)

if (nrow(appendix_panel_count) != 6) {
  stop(
    "Supply-position alternative-operationalisation non-voting figure does not contain 2 operationalisations x 3 flow columns."
  )
}

fig_supply_alt_non_voting <- plot_non_voting_effects(
  plot_data = appendix_non_voting_plot_data,
  row_facet = "operationalisation_label",
  x_scales = make_x_scales_from_data(appendix_non_voting_plot_data),
  strip_text_size = 9
)

print(fig_supply_alt_non_voting)

appendix_non_voting_table <- write_non_voting_outputs(
  plot_data = appendix_non_voting_plot_data,
  figure = fig_supply_alt_non_voting,
  figure_dir = alt_figure_dir,
  table_dir = alt_table_dir,
  file_stub = "supply_position_change_non_voting_appendix_investment_consumption_operationalisations_outward_inward_net",
  width = 9,
  height = 3.8,
  table_group_cols = "operationalisation_label"
)

cat("\n================================================\n")
cat("Alternative investment-consumption non-voting AMEs and net effects\n")
cat("================================================\n")
print(appendix_non_voting_table, n = Inf, width = Inf)
} else {
  cat("\nAlternative supply-position operationalisation files not found. Skipping that appendix-only comparison.\n")
  cat("Expected files:\n")
  cat(path_alt_ames, "\n")
  cat(path_alt_net, "\n")
}

# ------------------------------------------------
# 6. Files written
# ------------------------------------------------

cat("\n================================================\n")
cat("Files written\n")
cat("================================================\n")

cat("\nDemand-salience non-voting figures:\n")
cat(file.path(salience_figure_dir, "salience_change_non_voting_outward_inward_net.pdf"), "\n")
cat(file.path(salience_figure_dir, "salience_change_non_voting_outward_inward_net.png"), "\n")

cat("\nDemand-salience non-voting tables:\n")
cat(file.path(salience_table_dir, "salience_change_non_voting_outward_inward_net_reporting_table.csv"), "\n")
cat(file.path(salience_table_dir, "salience_change_non_voting_outward_inward_net_latex_ready.csv"), "\n")

cat("\nSupply-position non-voting figures:\n")
cat(file.path(supply_figure_dir, "supply_position_change_non_voting_outward_inward_net.pdf"), "\n")
cat(file.path(supply_figure_dir, "supply_position_change_non_voting_outward_inward_net.png"), "\n")

cat("\nSupply-position non-voting tables:\n")
cat(file.path(supply_table_dir, "supply_position_change_non_voting_outward_inward_net_reporting_table.csv"), "\n")
cat(file.path(supply_table_dir, "supply_position_change_non_voting_outward_inward_net_latex_ready.csv"), "\n")

if (has_alt_supply_operationalisations) {
  cat("\nSupply-position alternative-operationalisation non-voting figures:\n")
  cat(file.path(alt_figure_dir, "supply_position_change_non_voting_appendix_investment_consumption_operationalisations_outward_inward_net.pdf"), "\n")
  cat(file.path(alt_figure_dir, "supply_position_change_non_voting_appendix_investment_consumption_operationalisations_outward_inward_net.png"), "\n")
  
  cat("\nSupply-position alternative-operationalisation non-voting tables:\n")
  cat(file.path(alt_table_dir, "supply_position_change_non_voting_appendix_investment_consumption_operationalisations_outward_inward_net_reporting_table.csv"), "\n")
  cat(file.path(alt_table_dir, "supply_position_change_non_voting_appendix_investment_consumption_operationalisations_outward_inward_net_latex_ready.csv"), "\n")
}

cat("\nScript completed successfully.\n")
