# code/utils/packages.R
# ------------------------------------------------------------
# Package management: install (if missing) and load
# ------------------------------------------------------------

#' Load required packages
#'
#' Installs CRAN or GitHub packages if they are not available
#' and then loads them into the session. Intended for project-
#' specific reproducibility.
#'
#' @return Invisibly returns TRUE if all packages are successfully loaded.
#' @examples
#' load_packages()
#'
# code/utils/packages.R
# ------------------------------------------------------------
# Package management: install (if missing) and load
# ------------------------------------------------------------

#' Load required packages
#'
#' Installs CRAN or GitHub packages if they are not available
#' and then loads them into the session. Includes VoteSwitchR
#' from GitHub (denis-cohen/voteswitchR).
#'
#' @return Invisibly returns TRUE if all packages are successfully loaded.
#' @examples
#' load_packages()
#'
# code/utils/packages.R
# ------------------------------------------------------------
# Package management: install (if missing) and load
# ------------------------------------------------------------

#' Load required packages
#'
#' Installs CRAN or GitHub packages if they are not available
#' and then loads them into the session. Includes VoteSwitchR
#' from GitHub (denis-cohen/voteswitchR).
#'
#' @return Invisibly returns TRUE if all packages are successfully loaded.
#' @examples
#' load_packages()
#'
load_packages <- function() {
  # Required CRAN packages
  pkgs_cran <- c(
    "dplyr", "tidyr", "rstan", "tictoc",
    "remotes", "manifestoR", "lubridate"
  )
  
  # Required GitHub packages (in "owner/repo" format)
  pkgs_github <- c("denis-cohen/voteswitchR")
  
  # Install and load CRAN packages
  for (pkg in pkgs_cran) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
  
  # Install and load GitHub packages
  for (repo in pkgs_github) {
    pkg_name <- sub(".*/", "", repo)  # e.g. "voteswitchR"
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      remotes::install_github(repo, upgrade = "never")
    }
    suppressPackageStartupMessages(
      library(pkg_name, character.only = TRUE)
    )
  }
  
  invisible(TRUE)
}


# code/utils/packages.R
# ------------------------------------------------------------
# Package management: install (if missing) and load
# ------------------------------------------------------------

## Load package function
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c(
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
  "progressr"
)

ipak(packages)



