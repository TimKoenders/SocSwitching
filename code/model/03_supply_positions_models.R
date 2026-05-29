# ================================================================
# 03_supply_positions_models.R
# Mixed conditional logit models for social-democratic voter exchanges
# with election-specific party-family choice sets
#
# Goal:
#   Estimate outward, inward, and net social-democratic voter exchanges
#   while allowing available alternatives to vary across elections.
#
# Main predictors:
#   Election-to-election changes in social-democratic party-family
#   supply-side positions:
#     sd_investmentconsumption_move_std
#     sd_stateconomy_move_std
#     sd_libcons_move_std
#
# Controls:
#   Individual level:
#     gender
#     age_group
#
#   Context level:
#     enp_z
#
# Net effects:
#   Net = s_nonSD * AME_inward - s_SD * AME_outward
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(stringr)
  library(mclogit)
  library(tictoc)
})

run_delta_method_se <- TRUE
delta_step <- 1e-5

# ------------------------------------------------
# 1. Paths and inputs
# ------------------------------------------------

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
analysis_dir <- file.path(project_dir, "data", "analysis")
input_dir <- file.path(analysis_dir, "building_analysis_data")
output_dir <- file.path(analysis_dir, "models", "supply_position_change")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_outward <- file.path(input_dir, "df_analysis_outward_social_democratic.rds")
path_inward <- file.path(input_dir, "df_analysis_inward_social_democratic.rds")

tictoc::tic("Load data")
if (!file.exists(path_outward)) {
  stop("Outward modelling data not found: ", path_outward)
}
if (!file.exists(path_inward)) {
  stop("Inward modelling data not found: ", path_inward)
}

df_out <- readRDS(path_outward)
df_in <- readRDS(path_inward)
tictoc::toc()

stopifnot(is.data.frame(df_out), nrow(df_out) > 0)
stopifnot(is.data.frame(df_in), nrow(df_in) > 0)

supply_predictors <- c(
  "sd_investmentconsumption_move_std",
  "sd_stateconomy_move_std",
  "sd_libcons_move_std"
)

choice_level_controls <- "enp_z"
model_predictors <- c(supply_predictors, choice_level_controls)

supply_specification <- "joint_supply_position_change_block_marpor_complete_state_libcons"
supply_operationalisation <- "marpor_complete"
operationalisation_label <- "Primary investment-consumption scale"
net_share_specification <- "fixed_original_sample_risk_set_shares"

required_vars <- c(
  "iso2c_file", "elec_id", "outcome", "weights",
  "gender", "age_group", supply_predictors, choice_level_controls
)

missing_out <- setdiff(required_vars, names(df_out))
missing_in <- setdiff(required_vars, names(df_in))
if (length(missing_out) > 0) {
  stop("Missing required variables in outward data: ", paste(missing_out, collapse = ", "))
}
if (length(missing_in) > 0) {
  stop("Missing required variables in inward data: ", paste(missing_in, collapse = ", "))
}

cat("\nOutward outcome support:\n")
print(df_out %>% dplyr::count(outcome, sort = TRUE), n = Inf)
cat("\nInward outcome support:\n")
print(df_in %>% dplyr::count(outcome, sort = TRUE), n = Inf)

cat("\nMissing supply-side predictor and ENP values in outward data:\n")
print(
  df_out %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(model_predictors), ~sum(is.na(.x)))),
  width = Inf
)
cat("\nMissing supply-side predictor and ENP values in inward data:\n")
print(
  df_in %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(model_predictors), ~sum(is.na(.x)))),
  width = Inf
)

# ------------------------------------------------
# 2. Model metadata
# ------------------------------------------------

predictor_specs <- tibble::tibble(
  predictor = supply_predictors,
  predictor_label = c(
    "Change in SD investment-consumption position",
    "Change in SD state-economy position",
    "Change in SD cultural position"
  ),
  file_stub = c(
    "sd_investment_consumption_change",
    "sd_stateconomy_change",
    "sd_libcons_change"
  )
)

flow_specs <- tibble::tibble(
  flow = c("outward", "inward"),
  flow_label = c("Outward switching", "Inward switching"),
  reference_alt = c("retention", "not_to_sd")
)

outward_alt_levels <- c(
  "retention", "to_far_left", "to_green",
  "to_mainstream_right", "to_far_right", "to_non"
)

inward_alt_levels <- c(
  "not_to_sd", "from_far_left", "from_green",
  "from_mainstream_right", "from_far_right", "from_non"
)

alt_label_map <- c(
  retention = "Retention",
  to_far_left = "To far left",
  to_green = "To green",
  to_mainstream_right = "To mainstream right",
  to_far_right = "To far right",
  to_non = "To non-voting",
  not_to_sd = "Not to SD",
  from_far_left = "From far left",
  from_green = "From green",
  from_mainstream_right = "From mainstream right",
  from_far_right = "From far right",
  from_non = "From non-voting"
)

net_alt_map <- tibble::tibble(
  actor = c("far_left", "green", "mainstream_right", "far_right", "non"),
  actor_label = c("Far left", "Green", "Mainstream right", "Far right", "Non-voting"),
  outward_alt = c("to_far_left", "to_green", "to_mainstream_right", "to_far_right", "to_non"),
  inward_alt = c("from_far_left", "from_green", "from_mainstream_right", "from_far_right", "from_non")
)

make_block_formula <- function(model_predictors) {
  stats::as.formula(paste(
    "cbind(chosen, choice_set_id) ~",
    paste(
      c("alt", paste0("alt:", model_predictors), "alt:gender", "alt:age_group"),
      collapse = " + "
    )
  ))
}

make_prediction_formula <- function(model_predictors) {
  stats::as.formula(paste(
    "~",
    paste(
      c("alt", paste0("alt:", model_predictors), "alt:gender", "alt:age_group"),
      collapse = " + "
    )
  ))
}

detect_predictor_in_term <- function(term, predictors) {
  hits <- predictors[stringr::str_detect(term, stringr::fixed(predictors))]
  if (length(hits) == 0) {
    NA_character_
  } else {
    hits[[1]]
  }
}

add_row_id <- function(df) {
  if ("respondent_election_id" %in% names(df)) {
    df %>% dplyr::mutate(choice_id = as.character(respondent_election_id))
  } else if (all(c("id", "elec_id") %in% names(df))) {
    df %>% dplyr::mutate(choice_id = paste(iso2c_file, elec_id, id, sep = "__"))
  } else if ("respondent_election_uid" %in% names(df) && !all(is.na(df$respondent_election_uid))) {
    df %>% dplyr::mutate(choice_id = as.character(respondent_election_uid))
  } else {
    df %>% dplyr::mutate(choice_id = paste(iso2c_file, elec_id, dplyr::row_number(), sep = "__"))
  }
}

get_available_alternatives <- function(df, alt_levels) {
  df %>%
    dplyr::filter(outcome %in% alt_levels) %>%
    dplyr::distinct(iso2c_file, elec_id, outcome) %>%
    dplyr::rename(alt = outcome) %>%
    dplyr::mutate(alt = as.character(alt))
}

prepare_restricted_choice_data <- function(df, flow, flow_label, alt_levels, reference_alt) {
  cat("\nPreparing restricted-choice-set data\n")
  cat("Flow:", flow_label, "\n")
  cat("Supply predictors:", paste(supply_predictors, collapse = ", "), "\n")
  cat("Controls:", paste(choice_level_controls, collapse = ", "), "\n")
  cat("Reference alternative:", reference_alt, "\n")
  
  alt_levels_model <- c(setdiff(alt_levels, reference_alt), reference_alt)
  
  df_base <- df %>%
    add_row_id() %>%
    dplyr::filter(outcome %in% alt_levels) %>%
    dplyr::mutate(
      outcome = as.character(outcome),
      dplyr::across(dplyr::all_of(model_predictors), as.numeric),
      gender = as.numeric(gender),
      age_group = factor(age_group, levels = c("18-34", "35-54", "55+")),
      weights = dplyr::if_else(is.na(weights), 1, as.numeric(weights)),
      country_id = factor(iso2c_file),
      election_id = factor(paste(iso2c_file, elec_id, sep = "__")),
      iso2c_file = as.character(iso2c_file),
      elec_id = as.character(elec_id)
    ) %>%
    dplyr::filter(
      !is.na(outcome),
      !is.na(choice_id),
      !is.na(country_id),
      !is.na(election_id),
      !dplyr::if_any(dplyr::all_of(model_predictors), is.na),
      !is.na(gender),
      !is.na(age_group),
      !is.na(weights),
      weights > 0
    ) %>%
    droplevels()
  
  available_alts <- get_available_alternatives(df = df_base, alt_levels = alt_levels)
  
  df_long <- df_base %>%
    dplyr::select(
      choice_id, iso2c_file, elec_id, country_id, election_id,
      outcome, dplyr::all_of(model_predictors), gender, age_group, weights
    ) %>%
    dplyr::left_join(available_alts, by = c("iso2c_file", "elec_id"), relationship = "many-to-many") %>%
    dplyr::mutate(
      alt = factor(as.character(alt), levels = alt_levels_model),
      outcome = factor(as.character(outcome), levels = alt_levels_model),
      chosen = as.integer(outcome == alt),
      alt_label = dplyr::recode(as.character(alt), !!!alt_label_map),
      reference_alt = reference_alt,
      flow = flow,
      flow_label = flow_label,
      model_block = "supply_position_change"
    ) %>%
    dplyr::group_by(choice_id) %>%
    dplyr::filter(sum(chosen, na.rm = TRUE) == 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      choice_set_id = as.integer(factor(choice_id)),
      chosen = as.numeric(chosen)
    ) %>%
    droplevels()
  
  choice_set_summary <- df_long %>%
    dplyr::group_by(choice_id) %>%
    dplyr::summarise(
      n_available_alternatives = dplyr::n_distinct(alt),
      chosen_n = sum(chosen),
      .groups = "drop"
    )
  
  if (any(choice_set_summary$chosen_n != 1)) {
    stop("At least one choice set does not have exactly one chosen alternative.")
  }
  
  predictor_summary <- df_long %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(model_predictors),
      list(
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~stats::sd(.x, na.rm = TRUE)
      )
    ))
  
  diagnostics <- tibble::tibble(
    operationalisation = supply_operationalisation,
    operationalisation_label = operationalisation_label,
    model_block = "supply_position_change",
    flow = flow,
    flow_label = flow_label,
    n_long_rows = nrow(df_long),
    n_respondent_elections = dplyr::n_distinct(df_long$choice_id),
    n_elections = dplyr::n_distinct(df_long$election_id),
    n_countries = dplyr::n_distinct(df_long$country_id),
    mean_available_alternatives = mean(choice_set_summary$n_available_alternatives),
    min_available_alternatives = min(choice_set_summary$n_available_alternatives),
    max_available_alternatives = max(choice_set_summary$n_available_alternatives),
    weighted_n_choices = sum(df_long %>% dplyr::distinct(choice_id, weights) %>% dplyr::pull(weights), na.rm = TRUE),
    reference_alt = reference_alt,
    internal_reference_last = TRUE,
    supply_specification = supply_specification,
    net_share_specification = net_share_specification
  ) %>%
    dplyr::bind_cols(predictor_summary)
  
  cat("\nPrepared data diagnostics:\n")
  print(diagnostics, width = Inf)
  
  list(
    data = df_long,
    diagnostics = diagnostics,
    available_alts = available_alts,
    alt_levels_model = alt_levels_model
  )
}

fit_restricted_choice_mixed_clogit <- function(df_long, flow, flow_label, reference_alt) {
  cat("\n================================================\n")
  cat("Fitting joint supply-position-change restricted-choice-set mixed conditional logit\n")
  cat("Flow:", flow_label, "\n")
  cat("Supply predictors:", paste(supply_predictors, collapse = ", "), "\n")
  cat("Controls:", paste(choice_level_controls, collapse = ", "), "\n")
  cat("Reference alternative:", reference_alt, "\n")
  cat("Random effects: alternative-specific country intercepts\n")
  cat("================================================\n\n")
  
  model_formula <- make_block_formula(model_predictors)
  fit_time <- system.time({
    fit <- mclogit::mclogit(
      model_formula,
      data = df_long,
      weights = weights,
      random = ~ 0 + alt | country_id,
      method = "PQL"
    )
  })
  
  cat("\nFinished model\n")
  cat("Elapsed seconds:", round(fit_time[["elapsed"]], 2), "\n")
  
  coef_table <- as.data.frame(summary(fit)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      estimate = Estimate,
      std.error = `Std. Error`,
      statistic = `z value`,
      p.value = `Pr(>|z|)`
    ) %>%
    dplyr::mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      predictor = purrr::map_chr(term, detect_predictor_in_term, predictors = model_predictors),
      alt = stringr::str_extract(term, "(?<=^alt)[^:]+"),
      alt_label = dplyr::recode(alt, !!!alt_label_map, .default = alt),
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label,
      model_block = "supply_position_change",
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = "restricted_choice_set_mixed_conditional_logit_country_re",
      random_effect = "alternative_specific_country_intercepts",
      supply_specification = supply_specification,
      .before = 1
    ) %>%
    dplyr::left_join(predictor_specs, by = "predictor") %>%
    dplyr::relocate(
      dplyr::any_of(c("predictor_label", "file_stub")),
      .after = "predictor"
    )
  
  diagnostics <- tibble::tibble(
    operationalisation = supply_operationalisation,
    operationalisation_label = operationalisation_label,
    model_block = "supply_position_change",
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt,
    model = "restricted_choice_set_mixed_conditional_logit_country_re",
    random_effect = "alternative_specific_country_intercepts",
    n_coefficients = length(stats::coef(fit)),
    logLik = suppressWarnings(as.numeric(stats::logLik(fit))),
    AIC = suppressWarnings(stats::AIC(fit)),
    BIC = suppressWarnings(stats::BIC(fit)),
    elapsed_seconds = unname(fit_time[["elapsed"]]),
    supply_specification = supply_specification
  )
  
  list(
    fit = fit,
    coefficients = coef_table,
    diagnostics = diagnostics,
    formula = model_formula
  )
}

predict_probabilities_from_beta <- function(beta, df_long, model_predictors) {
  prediction_formula <- make_prediction_formula(model_predictors)
  mm <- stats::model.matrix(prediction_formula, data = df_long)
  
  keep <- intersect(colnames(mm), names(beta))
  eta <- as.numeric(mm[, keep, drop = FALSE] %*% beta[keep])
  eta <- eta - ave(eta, df_long$choice_set_id, FUN = max)
  exp_eta <- exp(eta)
  denom <- ave(exp_eta, df_long$choice_set_id, FUN = sum)
  exp_eta / denom
}

compute_ames_from_beta <- function(beta, vcov_mat, df_long, predictor, predictor_label, file_stub) {
  base_prob <- predict_probabilities_from_beta(
    beta = beta,
    df_long = df_long,
    model_predictors = model_predictors
  )
  
  shifted <- df_long
  shifted[[predictor]] <- shifted[[predictor]] + 1
  shifted_prob <- predict_probabilities_from_beta(
    beta = beta,
    df_long = shifted,
    model_predictors = model_predictors
  )
  
  ame_point <- df_long %>%
    dplyr::mutate(prob_diff = shifted_prob - base_prob) %>%
    dplyr::group_by(alt, alt_label) %>%
    dplyr::summarise(
      estimate = stats::weighted.mean(prob_diff, weights, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!isTRUE(run_delta_method_se)) {
    return(
      ame_point %>%
        dplyr::mutate(
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          uncertainty = "not_calculated"
        )
    )
  }
  
  beta_names <- names(beta)
  vcov_mat <- vcov_mat[beta_names, beta_names, drop = FALSE]
  
  one_alt_ame <- function(beta_now, alt_now) {
    base_now <- predict_probabilities_from_beta(
      beta = beta_now,
      df_long = df_long,
      model_predictors = model_predictors
    )
    shifted_now <- predict_probabilities_from_beta(
      beta = beta_now,
      df_long = shifted,
      model_predictors = model_predictors
    )
    mean_data <- df_long %>%
      dplyr::mutate(prob_diff = shifted_now - base_now) %>%
      dplyr::filter(as.character(alt) == alt_now)
    stats::weighted.mean(mean_data$prob_diff, mean_data$weights, na.rm = TRUE)
  }
  
  se_table <- purrr::map_dfr(as.character(ame_point$alt), function(alt_now) {
    gradient <- numeric(length(beta))
    names(gradient) <- beta_names
    
    for (nm in beta_names) {
      beta_plus <- beta
      beta_minus <- beta
      beta_plus[[nm]] <- beta_plus[[nm]] + delta_step
      beta_minus[[nm]] <- beta_minus[[nm]] - delta_step
      gradient[[nm]] <- (
        one_alt_ame(beta_plus, alt_now) -
          one_alt_ame(beta_minus, alt_now)
      ) / (2 * delta_step)
    }
    
    variance <- as.numeric(t(gradient) %*% vcov_mat %*% gradient)
    tibble::tibble(
      alt = alt_now,
      std.error = ifelse(is.finite(variance) && variance >= 0, sqrt(variance), NA_real_)
    )
  })
  
  ame_point %>%
    dplyr::mutate(alt = as.character(alt)) %>%
    dplyr::left_join(se_table, by = "alt") %>%
    dplyr::mutate(
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(-abs(statistic)),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      uncertainty = "ame_delta_se"
    )
}

compute_supply_ames_for_predictor <- function(model_obj, df_long, predictor, predictor_label, file_stub) {
  beta_hat <- stats::coef(model_obj$fit)
  vcov_mat <- stats::vcov(model_obj$fit)
  
  compute_ames_from_beta(
    beta = beta_hat,
    vcov_mat = vcov_mat,
    df_long = df_long,
    predictor = predictor,
    predictor_label = predictor_label,
    file_stub = file_stub
  ) %>%
    dplyr::mutate(
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub,
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label,
      supply_specification = supply_specification,
      .before = 1
    )
}

compute_supply_ames_all_predictors <- function(model_obj, df_long) {
  purrr::pmap_dfr(predictor_specs, function(predictor, predictor_label, file_stub) {
    compute_supply_ames_for_predictor(
      model_obj = model_obj,
      df_long = df_long,
      predictor = predictor,
      predictor_label = predictor_label,
      file_stub = file_stub
    )
  })
}

get_weighted_risk_set_n <- function(df_long) {
  df_long %>%
    dplyr::distinct(choice_id, weights) %>%
    dplyr::summarise(weighted_n = sum(weights, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(weighted_n)
}

compute_risk_set_shares <- function(df_out_long, df_in_long) {
  n_sd <- get_weighted_risk_set_n(df_out_long)
  n_non_sd <- get_weighted_risk_set_n(df_in_long)
  n_total <- n_sd + n_non_sd
  
  tibble::tibble(
    s_sd = n_sd / n_total,
    s_non_sd = n_non_sd / n_total,
    weighted_n_sd = n_sd,
    weighted_n_non_sd = n_non_sd,
    weighted_n_total = n_total
  )
}

compute_net_effects_from_ames <- function(outward_ames, inward_ames, shares, source) {
  join_vars <- c("predictor", "predictor_label", "file_stub", "actor", "actor_label")
  
  outward_clean <- outward_ames %>%
    dplyr::mutate(outward_alt = as.character(alt)) %>%
    dplyr::filter(outward_alt %in% net_alt_map$outward_alt) %>%
    dplyr::left_join(net_alt_map, by = "outward_alt") %>%
    dplyr::select(
      predictor, predictor_label, file_stub, actor, actor_label,
      outward_alt, outward_ame = estimate, outward_se = std.error
    )
  
  inward_clean <- inward_ames %>%
    dplyr::mutate(inward_alt = as.character(alt)) %>%
    dplyr::filter(inward_alt %in% net_alt_map$inward_alt) %>%
    dplyr::left_join(net_alt_map, by = "inward_alt") %>%
    dplyr::select(
      predictor, predictor_label, file_stub, actor, actor_label,
      inward_alt, inward_ame = estimate, inward_se = std.error
    )
  
  outward_clean %>%
    dplyr::inner_join(inward_clean, by = join_vars) %>%
    dplyr::mutate(
      flow = "net",
      flow_label = "Net effect",
      s_sd = shares$s_sd,
      s_non_sd = shares$s_non_sd,
      weighted_n_sd = shares$weighted_n_sd,
      weighted_n_non_sd = shares$weighted_n_non_sd,
      weighted_n_total = shares$weighted_n_total,
      estimate = s_non_sd * inward_ame - s_sd * outward_ame,
      std.error = sqrt((s_non_sd^2 * inward_se^2) + (s_sd^2 * outward_se^2)),
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(-abs(statistic)),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      uncertainty = "ame_delta_se_independent_net_combination",
      effect_type = "net_effect",
      source = source,
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label,
      supply_specification = supply_specification,
      net_share_specification = net_share_specification,
      .before = 1
    )
}

run_block_model <- function(flow, flow_label, reference_alt) {
  df_now <- if (flow == "outward") df_out else df_in
  alt_levels_now <- if (flow == "outward") outward_alt_levels else inward_alt_levels
  
  prepared <- prepare_restricted_choice_data(
    df = df_now,
    flow = flow,
    flow_label = flow_label,
    alt_levels = alt_levels_now,
    reference_alt = reference_alt
  )
  
  model_obj <- fit_restricted_choice_mixed_clogit(
    df_long = prepared$data,
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt
  )
  
  ames <- compute_supply_ames_all_predictors(
    model_obj = model_obj,
    df_long = prepared$data
  ) %>%
    dplyr::mutate(
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = "restricted_choice_set_mixed_conditional_logit_country_re",
      random_effect = "alternative_specific_country_intercepts",
      .before = 1
    )
  
  cat("\nAverage marginal effects of a one-standard-deviation increase in each supply-side position change:\n")
  print(ames, n = Inf, width = Inf)
  
  saveRDS(
    list(
      model_block = "supply_position_change",
      flow = flow,
      flow_label = flow_label,
      reference_alt = reference_alt,
      model = model_obj,
      data = prepared$data,
      data_diagnostics = prepared$diagnostics,
      available_alternatives = prepared$available_alts,
      alt_levels_model = prepared$alt_levels_model,
      ames = ames,
      supply_predictors = supply_predictors,
      control_predictors = choice_level_controls,
      model_predictors = model_predictors,
      supply_specification = supply_specification,
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label
    ),
    file.path(output_dir, paste0("model_supply_position_change_", flow, ".rds"))
  )
  
  list(
    model_block = "supply_position_change",
    flow = flow,
    flow_label = flow_label,
    reference_alt = reference_alt,
    model = model_obj,
    data = prepared$data,
    data_diagnostics = prepared$diagnostics,
    coefficients = model_obj$coefficients,
    diagnostics = model_obj$diagnostics,
    available_alternatives = prepared$available_alts,
    alt_levels_model = prepared$alt_levels_model,
    ames = ames
  )
}

# ------------------------------------------------
# 3. Estimate models
# ------------------------------------------------

tictoc::tic("Run joint supply-position-change restricted-choice-set mixed conditional logit models")

all_results <- list()
all_results[["supply_position_change__outward"]] <- run_block_model(
  flow = "outward",
  flow_label = "Outward switching",
  reference_alt = "retention"
)
all_results[["supply_position_change__inward"]] <- run_block_model(
  flow = "inward",
  flow_label = "Inward switching",
  reference_alt = "not_to_sd"
)

tictoc::toc()

# ------------------------------------------------
# 4. Combine and save outputs
# ------------------------------------------------

combined_data_diagnostics <- purrr::map_dfr(all_results, "data_diagnostics", .id = "model_id")
combined_model_diagnostics <- purrr::map_dfr(all_results, "diagnostics", .id = "model_id")
combined_coefficients <- purrr::map_dfr(all_results, "coefficients", .id = "model_id")
combined_ames <- purrr::map_dfr(all_results, "ames", .id = "model_id")

combined_available_alternatives <- purrr::imap_dfr(all_results, function(x, nm) {
  x$available_alternatives %>%
    dplyr::mutate(
      model_id = nm,
      model_block = "supply_position_change",
      flow = x$flow,
      flow_label = x$flow_label,
      .before = 1
    )
})

shares_point <- compute_risk_set_shares(
  df_out_long = all_results[["supply_position_change__outward"]]$data,
  df_in_long = all_results[["supply_position_change__inward"]]$data
)

point_net_effects <- compute_net_effects_from_ames(
  outward_ames = all_results[["supply_position_change__outward"]]$ames,
  inward_ames = all_results[["supply_position_change__inward"]]$ames,
  shares = shares_point,
  source = "point_estimate"
)

cat("\n================================================\n")
cat("Combined AMEs\n")
cat("================================================\n")
print(combined_ames, n = Inf, width = Inf)

cat("\n================================================\n")
cat("Point-estimate net effects\n")
cat("================================================\n")
print(point_net_effects, n = Inf, width = Inf)

readr::write_csv(combined_data_diagnostics, file.path(output_dir, "restricted_choice_set_data_diagnostics.csv"))
readr::write_csv(combined_model_diagnostics, file.path(output_dir, "restricted_choice_set_model_diagnostics.csv"))
readr::write_csv(combined_coefficients, file.path(output_dir, "restricted_choice_set_coefficients.csv"))
readr::write_csv(combined_ames, file.path(output_dir, "restricted_choice_set_ames.csv"))
readr::write_csv(combined_available_alternatives, file.path(output_dir, "restricted_choice_set_available_alternatives.csv"))
readr::write_csv(point_net_effects, file.path(output_dir, "restricted_choice_set_net_effects_point_estimates.csv"))

saveRDS(point_net_effects, file.path(output_dir, "restricted_choice_set_net_effects_point_estimates.rds"))

ames_with_delta_ci <- combined_ames
net_effects_with_delta_ci <- point_net_effects

readr::write_csv(ames_with_delta_ci, file.path(output_dir, "restricted_choice_set_ames_with_delta_ci.csv"))
readr::write_csv(net_effects_with_delta_ci, file.path(output_dir, "restricted_choice_set_net_effects_with_delta_ci.csv"))
saveRDS(ames_with_delta_ci, file.path(output_dir, "restricted_choice_set_ames_with_delta_ci.rds"))
saveRDS(net_effects_with_delta_ci, file.path(output_dir, "restricted_choice_set_net_effects_with_delta_ci.rds"))

final_results <- list(
  operationalisation = supply_operationalisation,
  operationalisation_label = operationalisation_label,
  predictor_specs = predictor_specs,
  flow_specs = flow_specs,
  outward_alt_levels = outward_alt_levels,
  inward_alt_levels = inward_alt_levels,
  net_alt_map = net_alt_map,
  supply_predictors = supply_predictors,
  control_predictors = choice_level_controls,
  model_predictors = model_predictors,
  supply_specification = supply_specification,
  net_share_specification = net_share_specification,
  all_results = all_results,
  combined_data_diagnostics = combined_data_diagnostics,
  combined_model_diagnostics = combined_model_diagnostics,
  combined_coefficients = combined_coefficients,
  combined_ames_point = combined_ames,
  point_net_effects = point_net_effects,
  risk_set_shares = shares_point,
  delta_method_se_was_run = isTRUE(run_delta_method_se),
  delta_step = delta_step,
  ames_with_delta_ci = ames_with_delta_ci,
  net_effects_with_delta_ci = net_effects_with_delta_ci
)

saveRDS(final_results, file.path(output_dir, "all_model_results.rds"))
saveRDS(final_results, file.path(output_dir, "final_supply_position_change_model_results.rds"))

# ------------------------------------------------
# 5. Final estimation-sample composition
# ------------------------------------------------

summarise_estimation_sample <- function(result_obj) {
  df_long <- result_obj$data
  
  respondent_level <- df_long %>%
    dplyr::select(choice_id, iso2c_file, elec_id, outcome, weights) %>%
    dplyr::distinct()
  
  sample_summary <- respondent_level %>%
    dplyr::summarise(
      model_block = result_obj$model_block,
      flow = result_obj$flow,
      flow_label = result_obj$flow_label,
      n_respondents = dplyr::n_distinct(choice_id),
      n_respondent_alternative_combinations = nrow(df_long),
      n_elections = dplyr::n_distinct(elec_id),
      n_countries = dplyr::n_distinct(iso2c_file),
      mean_available_alternatives =
        n_respondent_alternative_combinations / n_respondents,
      weighted_n_respondents = sum(weights, na.rm = TRUE),
      reference_alt = result_obj$reference_alt,
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label,
      supply_specification = supply_specification
    )
  
  outcome_summary <- respondent_level %>%
    dplyr::count(outcome, name = "n_respondents") %>%
    dplyr::mutate(
      model_block = result_obj$model_block,
      flow = result_obj$flow,
      flow_label = result_obj$flow_label,
      share = n_respondents / sum(n_respondents),
      operationalisation = supply_operationalisation,
      operationalisation_label = operationalisation_label,
      supply_specification = supply_specification,
      .before = 1
    )
  
  list(sample_summary = sample_summary, outcome_summary = outcome_summary)
}

estimation_sample_objects <- purrr::map(all_results, summarise_estimation_sample)
estimation_sample_summary <- purrr::map_dfr(estimation_sample_objects, "sample_summary", .id = "model_id")
estimation_outcome_summary <- purrr::map_dfr(estimation_sample_objects, "outcome_summary", .id = "model_id")

cat("\n================================================\n")
cat("Final estimation-sample summary\n")
cat("================================================\n")
print(estimation_sample_summary, width = Inf)

cat("\n================================================\n")
cat("Final estimation-sample outcome composition\n")
cat("================================================\n")
print(estimation_outcome_summary, n = Inf, width = Inf)

readr::write_csv(estimation_sample_summary, file.path(output_dir, "estimation_sample_summary.csv"))
readr::write_csv(estimation_outcome_summary, file.path(output_dir, "estimation_outcome_summary.csv"))
saveRDS(estimation_sample_summary, file.path(output_dir, "estimation_sample_summary.rds"))
saveRDS(estimation_outcome_summary, file.path(output_dir, "estimation_outcome_summary.rds"))

cat("\nSupply-position model outputs saved to:\n")
cat(output_dir, "\n")
cat("\nScript completed successfully.\n")
