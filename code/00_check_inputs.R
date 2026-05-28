# Check local input files required for the SocSwitch workflow.

rm(list = ls())
options(stringsAsFactors = FALSE)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
manifest_path <- file.path(repo_root, "config", "required_inputs.csv")
template_path <- file.path(repo_root, "config", "data_paths_template.yml")
local_config_path <- file.path(repo_root, "config", "data_paths.yml")

read_simple_yaml <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  lines <- readLines(path, warn = FALSE)
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  key <- sub(":.*$", "", lines)
  value <- sub("^[^:]+:\\s*", "", lines)
  value <- gsub('^["\\\']|["\\\']$', "", trimws(value))
  stats::setNames(value, key)
}

normalize_config_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  if (grepl("^[A-Za-z]:/|^/", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
}

config <- read_simple_yaml(template_path)
local_config <- read_simple_yaml(local_config_path)
config[names(local_config)] <- local_config
config[["repo_root"]] <- repo_root

path_keys <- grep("(_dir|_root|_file)$", names(config), value = TRUE)
config[path_keys] <- vapply(config[path_keys], normalize_config_path, character(1))

if (!file.exists(manifest_path)) {
  stop("Missing input manifest: ", manifest_path)
}

inputs <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("stage", "group", "country", "election", "script", "path", "required", "note")
missing_columns <- setdiff(required_columns, names(inputs))
if (length(missing_columns) > 0) {
  stop("Input manifest is missing columns: ", paste(missing_columns, collapse = ", "))
}

resolve_placeholders <- function(x, values) {
  for (key in names(values)) {
    x <- gsub(paste0("\\{", key, "\\}"), values[[key]], x, fixed = FALSE)
  }
  normalizePath(gsub("\\\\", "/", x), winslash = "/", mustWork = FALSE)
}

inputs$resolved_path <- vapply(inputs$path, resolve_placeholders, character(1), values = config)
inputs$required <- tolower(inputs$required) %in% c("true", "t", "1", "yes", "y")
inputs$exists <- file.exists(inputs$resolved_path)
inputs$status <- ifelse(inputs$exists, "FOUND", ifelse(inputs$required, "MISSING", "OPTIONAL_MISSING"))

display <- inputs[, c(
  "status", "stage", "group", "country", "election", "script",
  "resolved_path", "note"
)]

cat("\nSocSwitch input check\n")
cat("=====================\n\n")
cat("Repository: ", repo_root, "\n", sep = "")
cat("Config:     ", if (file.exists(local_config_path)) local_config_path else template_path, "\n\n", sep = "")

print(display, row.names = FALSE, right = FALSE)

summary_table <- as.data.frame(table(inputs$status), stringsAsFactors = FALSE)
names(summary_table) <- c("status", "n")

cat("\nSummary\n")
cat("-------\n")
print(summary_table, row.names = FALSE)

missing_required <- inputs[inputs$required & !inputs$exists, ]

if (nrow(missing_required) > 0) {
  cat("\nMissing required inputs\n")
  cat("-----------------------\n")
  print(
    missing_required[, c("stage", "country", "election", "path", "resolved_path")],
    row.names = FALSE,
    right = FALSE
  )
  quit(status = 1)
}

cat("\nAll required inputs were found.\n")
