# ================================================================
# Inspect which raw files voteswitchR expects and compare them
# to the files currently stored in the local data/files directory
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
options(na.print = NULL)

suppressPackageStartupMessages({
  library(voteswitchR)
  library(dplyr)
  library(tibble)
  library(purrr)
})

# ------------------------------------------------
# 1. Set local base path and load internal metadata
# ------------------------------------------------
base_path <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), "data", "files")

available_data <- getFromNamespace("available_data", "voteswitchR")

# ------------------------------------------------
# 2. Inspect namespace objects relevant for file building
# ------------------------------------------------
cat("\n====================================================\n")
cat("Relevant internal voteswitchR objects\n")
cat("====================================================\n")

objs <- ls(getNamespace("voteswitchR"), all.names = TRUE)
print(objs[grepl("context|file|build|meta|mapping", objs, ignore.case = TRUE)])

# ------------------------------------------------
# 3. Inspect available_data structure
# ------------------------------------------------
cat("\n====================================================\n")
cat("Structure of available_data\n")
cat("====================================================\n")
print(names(available_data))

# ------------------------------------------------
# 4. Expected folders and filenames for all contexts
# ------------------------------------------------
expected_all <- available_data %>%
  dplyr::select(elec_id, iso2c, folder_name, file_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(iso2c, elec_id) %>%
  tibble::as_tibble()

cat("\n====================================================\n")
cat("Expected folder/file combinations for all contexts\n")
cat("====================================================\n")
print(expected_all, n = Inf)

# ------------------------------------------------
# 5. Compare expected structure with local files on disk
# ------------------------------------------------
actual_check <- purrr::map_dfr(seq_len(nrow(expected_all)), function(i) {
  folder_i <- file.path(base_path, expected_all$folder_name[i])
  file_i   <- expected_all$file_name[i]
  
  tibble::tibble(
    elec_id = expected_all$elec_id[i],
    iso2c = expected_all$iso2c[i],
    folder_name = expected_all$folder_name[i],
    expected_file = file_i,
    folder_exists = dir.exists(folder_i),
    file_exists = file.exists(file.path(folder_i, file_i))
  )
}) %>%
  dplyr::arrange(iso2c, elec_id)

cat("\n====================================================\n")
cat("Folder/file availability check\n")
cat("====================================================\n")
print(actual_check, n = Inf)

