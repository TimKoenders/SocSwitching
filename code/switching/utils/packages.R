# Package loading helpers for the SocSwitch reproduction workflow.
#
# The workflow should not install or update packages while it runs. Package
# installation changes the computational environment and can fail on systems
# where the global R library is not writable. Missing packages are therefore
# reported explicitly before the scripts continue.

load_packages <- function(extra = character()) {
  packages <- unique(c(
    "tidyverse",
    "haven",
    "psych",
    "here",
    "scales",
    "readxl",
    "writexl",
    "cluster",
    "clubSandwich",
    "lmtest",
    "pscl",
    "marginaleffects",
    "ggalluvial",
    "ggplot2",
    "forcats",
    "patchwork",
    "progressr",
    "nnet",
    "emmeans",
    "metafor",
    "purrr",
    "stringr",
    "tidytext",
    "moments",
    "lubridate",
    "manifestoR",
    "rstan",
    "tictoc",
    "voteswitchR",
    extra
  ))

  missing <- packages[!vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]

  if (length(missing) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing, collapse = ", "),
      ". Install these before running the reproducibility workflow.",
      call. = FALSE
    )
  }

  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }))
}

load_packages()
