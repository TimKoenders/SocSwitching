# Run the SocSwitch reproduction workflow.
#
# Usage examples:
#   Rscript code/00_run_all.R --targets=check
#   Rscript code/00_run_all.R --targets=data
#   Rscript code/00_run_all.R --targets=models,results,descriptives
#   Rscript code/00_run_all.R --targets=all

rm(list = ls())
options(stringsAsFactors = FALSE)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

targets <- strsplit(get_arg("targets", "all"), ",", fixed = TRUE)[[1]]
targets <- trimws(tolower(targets))
if ("all" %in% targets) {
  targets <- c("check", "micro", "dependent", "independent", "analysis", "models", "results", "descriptives")
}
if ("data" %in% targets) {
  targets <- unique(unlist(lapply(targets, function(x) {
    if (identical(x, "data")) {
      c("check", "micro", "dependent", "independent", "analysis")
    } else {
      x
    }
  })))
}

run_script <- function(path) {
  full_path <- file.path(repo_root, path)
  if (!file.exists(full_path)) {
    stop("Workflow script not found: ", path)
  }
  cat("\n>>> ", path, "\n", sep = "")
  status <- system2(rscript, shQuote(full_path))
  if (!identical(status, 0L)) {
    stop("Script failed: ", path, call. = FALSE)
  }
}

run_many <- function(paths) {
  for (path in paths) {
    run_script(path)
  }
}

micro_dir <- file.path("code", "switching", "data_preparation", "building_micro_data")

workflow <- list(
  check = "code/00_check_inputs.R",
  micro = c(
    file.path(micro_dir, sprintf("%02d_%s.R", 1:31, c(
      "Australia", "Austria", "Belgium", "Bulgaria", "Canada",
      "CzechRepublic", "Denmark", "Estonia", "Finland", "France",
      "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel",
      "Italy", "Latvia", "Lithuania", "Netherlands", "NewZealand",
      "Norway", "Poland", "Portugal", "Romania", "Slovakia",
      "Slovenia", "Spain", "Sweden", "Switzerland", "UnitedKingdom"
    ))),
    file.path(micro_dir, c(
      "33_Austria_2019.R",
      "34_Austria_2024.R",
      "35_Switzerland_2023.R",
      "36_Denmark_2022.R",
      "37_France_2022.R",
      "38_NewZealand_2023.R",
      "39_Poland_2023.R",
      "40_Portugal_2022.R",
      "41_Portugal_2024.R",
      "42_Sweden_2022.R",
      "43_Slovenia_2022.R",
      "44_Slovakia_2023.R",
      "32_append_country_files.R"
    ))
  ),
  dependent = file.path("code", "switching", "data_preparation", "dependent_variable", c(
    "00_prepare_vote_shares_parlgov.R",
    "01_prepare_switching_data.R",
    "02_prepare_realised_transition_datasets.R"
  )),
  independent = file.path("code", "switching", "data_preparation", "independent_variables", c(
    "01_prepare_demand_salience.R",
    "02_prepare_supply_positions.R"
  )),
  analysis = file.path("code", "switching", "data_preparation", "building_analysis_data", c(
    "01_build_analysis_data.R",
    "02_add_cpds_controls.R"
  )),
  models = file.path("code", "switching", "model", c(
    "01_demand_salience_models.R",
    "03_supply_positions_models.R",
    "06_vote_share_change_models.R",
    "07_demand_salience_models_controls.R",
    "08_supply_position_models_controls.R"
  )),
  results = file.path("code", "switching", "model", c(
    "02_demand_salience_results.R",
    "04_supply_position_results.R",
    "05_overall_contextual_net_effects.R",
    "09_non_voting_appendix_results.R"
  )),
  descriptives = file.path("code", "switching", "descriptives", c(
    "01_austrian_example.R",
    "02_describe_social_democratic_exchanges.R",
    "03_plot_country_net_social_democratic_exchanges.R"
  ))
)

unknown_targets <- setdiff(targets, names(workflow))
if (length(unknown_targets) > 0) {
  stop("Unknown target(s): ", paste(unknown_targets, collapse = ", "))
}

cat("\nSocSwitch workflow\n")
cat("==================\n")
cat("Targets: ", paste(targets, collapse = ", "), "\n", sep = "")

for (target in targets) {
  run_many(workflow[[target]])
}

cat("\nWorkflow completed.\n")
