user_lib <- Sys.getenv("R_LIBS_USER")

dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = user_lib, repos = "https://cloud.r-project.org")
  }
}

cran_packages <- c(
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
  "tictoc"
)

for (pkg in cran_packages) {
  install_if_missing(pkg)
}

install_if_missing("remotes")

if (!requireNamespace("shiny", quietly = TRUE) ||
    as.character(utils::packageVersion("shiny")) != "1.7.2") {
  remotes::install_version(
    "shiny",
    version = "1.7.2",
    lib = user_lib,
    upgrade = "never",
    repos = "https://cloud.r-project.org"
  )
}

if (!requireNamespace("voteswitchR", quietly = TRUE)) {
  remotes::install_github(
    "denis-cohen/voteswitchR",
    lib = user_lib,
    upgrade = "never"
  )
}

cat("Dependency installation completed.\n")
cat("User library:", user_lib, "\n")
