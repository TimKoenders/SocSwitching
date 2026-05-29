# SocSwitch

Code repository for **Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families**.

This project studies voter flows between social-democratic parties and their competitors across comparative election contexts. The repository contains the project-specific code used after vote-switching data have been harmonized, including scripts to build contextual measures, estimate the models, and produce the descriptive figures, model results, and appendix material.

The underlying vote-switching harmonization procedure is not reimplemented here. It follows the [`voteswitchR`](https://github.com/denis-cohen/voteswitchR) infrastructure for harmonizing national election studies. For this study, additional CSES Module 6 elections were added manually to extend the set of available electoral contexts.

## About

`SocSwitching` is a reproducible R workflow for studying how social-democratic parties exchange voters with other party families across multi-party systems. It is built as an open-code, restricted-data research repository: the full analysis logic is version controlled, while the original election-study files remain local because they are governed by third-party access conditions.

The repository provides:

- A single workflow entry point: `code/00_run_all.R`.
- Input checks for local restricted files: `code/00_check_inputs.R`.
- Project-specific data preparation after `voteswitchR`-style harmonization.
- Scripts for model estimation, result extraction, descriptive figures, and appendix plots.
- Machine-readable data manifests and local path templates.
- Compact workflow logging under `data/analysis/logs/workflow/`.

The intended use is straightforward: clone the repository, add the authorized local data files, configure local paths if needed, run the input checker, and then run the workflow targets documented below.

## Data Availability

The raw respondent-level election studies used in this project are not redistributed in this repository. Many of the underlying national election studies and comparative survey files are governed by third-party terms of use, registration requirements, or restricted-access agreements. The rights to distribute those data remain with the original data providers.

Researchers with authorized access can reproduce the analyses by obtaining the required files from the original providers, reconstructing or obtaining the required `voteswitchR`-style harmonized switching objects locally, and placing the resulting files in the local folder structure described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md). The repository is therefore designed as an **open-code, restricted-data** replication package.

## Repository Structure

```text
code/
  00_check_inputs.R        # Verify local restricted inputs before running the pipeline
  00_run_all.R             # Orchestrate checks, data preparation, models, and outputs
  data_preparation/        # Project-specific preparation after voteswitchR-style harmonization
  descriptives/            # Descriptive plots and tables
  model/                   # Model estimation and result scripts
  utils/                   # Shared package and helper scripts
config/
  data_paths_template.yml  # Copy to data_paths.yml and adapt local paths
  required_inputs.csv      # Source-level manifest used by the input checker
data/
  README.md                # Local data folder instructions
  data_manifest.csv        # Machine-readable guide to required data groups
plots/
  README.md                # Local output folder instructions
DATA_AVAILABILITY.md       # Source-level data access information
REPRODUCIBILITY.md         # Reproduction workflow for authorized users
VALIDATION.md              # Commands used to verify the repository workflow
```

## Workflow Map

The repository is organized around one reproducible project pipeline. The raw survey files remain local and restricted, while the code, manifests, and execution order are tracked.

| Stage | Command target | Main scripts | Main local outputs |
| --- | --- | --- | --- |
| Input checks | `check` | `code/00_check_inputs.R` | Console/log report of available local inputs |
| Microdata construction | `micro` | `code/data_preparation/building_micro_data/` | Combined project microdata under `data/analysis/` |
| Dependent variables | `dependent` | `code/data_preparation/dependent_variable/` | Vote-share and switching outcome files |
| Contextual predictors | `independent` | `code/data_preparation/independent_variables/` | Demand salience and supply position files |
| Analysis data | `analysis` | `code/data_preparation/building_analysis_data/` | Final model-ready datasets |
| Model estimation | `models` | `code/model/01_*`, `03_*`, `06_*`, `07_*`, `08_*` | Local model objects under `data/analysis/models/` |
| Model results | `results` | `code/model/02_*`, `04_*`, `05_*`, `09_*` | Tables and result objects under local output folders |
| Descriptives | `descriptives` | `code/descriptives/` | Descriptive figures and summaries |

The complete workflow is launched with `Rscript code/00_run_all.R --targets=all`. By default, the runner prints only a short status line for each script and writes full script output to timestamped logs under `data/analysis/logs/workflow/`. Use `--verbose=true` to print each child script directly to the console.

## Reproducibility

The analyses are reproducible conditional on lawful access to the underlying data. The workflow is:

1. Obtain the required survey, party, election, and contextual datasets from the original providers.
2. Follow the `voteswitchR` harmonization workflow for the baseline vote-switching infrastructure.
3. Place the original `.dta`, `.sav`, or equivalent survey files under `data/files/`, using the election subfolders expected by the `voteswitchR` data procurement workflow and the direct country scripts. A country can require multiple source files, and some election subfolders can refer to the same multi-election dataset when one file covers several elections.
4. Copy `config/data_paths_template.yml` to `config/data_paths.yml` and adapt local paths when the data are not stored under the default repository folders.
5. Run `Rscript code/00_check_inputs.R` to verify the local setup.
6. Run `Rscript code/00_run_all.R --targets=data` to rebuild the analysis data, or `Rscript code/00_run_all.R --targets=all` to run the complete workflow.

The microdata stage runs `code/data_preparation/building_micro_data/`: scripts `01`-`31` start from generated `voteswitchR` country bundles stored under `data/micro/`, each of which can be built from multiple original files in `data/files/`; scripts `33` onward add the manually coded CSES Module 6 election studies from `data/files/`; and `32_append_country_files.R` appends the country files. The appended micro-level files are then processed by `code/data_preparation/building_analysis_data/`.

More detailed instructions are in [REPRODUCIBILITY.md](REPRODUCIBILITY.md).

## Software

The project is written in R. Install dependencies once with `Rscript scripts/install_repro_deps.R`. Package loading is centralized in `code/utils/packages.R`; shared helpers are in `code/utils/helper_functions.R`. Start an R session from the repository root and run:

```r
source("code/utils/packages.R")
```

The same package helper is used by the workflow scripts.

## Citation

If you use this repository, please cite:

Koenders, Tim. *Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families*.

## License

Copyright (c) 2026 Tim Koenders. All rights reserved. See [LICENSE](LICENSE). This repository does not grant permission to redistribute third-party data.
