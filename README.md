# SocSwitch

Code repository for **Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families**.

This project studies voter flows between social-democratic parties and their competitors across comparative election contexts. The repository contains the project-specific code used after vote-switching data have been harmonized, including scripts to build contextual measures, estimate the models, and produce the descriptive figures, model results, and appendix material.

The underlying vote-switching harmonization procedure is not reimplemented here. It follows the [`voteswitchR`](https://github.com/denis-cohen/voteswitchR) infrastructure for harmonizing national election studies. For this study, additional CSES Module 6 elections were added manually to extend the set of available electoral contexts.

## Data Availability

The raw respondent-level election studies used in this project are not redistributed in this repository. Many of the underlying national election studies and comparative survey files are governed by third-party terms of use, registration requirements, or restricted-access agreements. The rights to distribute those data remain with the original data providers.

Researchers with authorized access can reproduce the analyses by obtaining the required files from the original providers, reconstructing or obtaining the required `voteswitchR`-style harmonized switching objects locally, and placing the resulting files in the local folder structure described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md). The repository is therefore designed as an **open-code, restricted-data** replication package.

## Repository Structure

```text
code/
  00_check_inputs.R        # Verify local restricted inputs before running the pipeline
  00_run_all.R             # Orchestrate checks, data preparation, models, and outputs
  switching/
    data_preparation/      # Project-specific preparation after voteswitchR-style harmonization
    descriptives/          # Descriptive plots and tables
    model/                 # Model estimation and result scripts
    utils/                 # Shared package and helper scripts
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
```

## Reproducibility

The analyses are reproducible conditional on lawful access to the underlying data. The workflow is:

1. Obtain the required survey, party, election, and contextual datasets from the original providers.
2. Follow the `voteswitchR` harmonization workflow for the baseline vote-switching infrastructure.
3. Place the original `.dta`, `.sav`, or equivalent survey files in the local data folders expected by the country scripts.
4. Copy `config/data_paths_template.yml` to `config/data_paths.yml` and adapt local paths when the data are not stored under the default repository folders.
5. Run `Rscript code/00_check_inputs.R` to verify the local setup.
6. Run `Rscript code/00_run_all.R --targets=data` to rebuild the analysis data, or `Rscript code/00_run_all.R --targets=all` to run the complete workflow.

The microdata stage runs `code/switching/data_preparation/building_micro_data/`: scripts `01`-`31` prepare country files through the Cohen/`voteswitchR` harmonization infrastructure, scripts `33` onward add the manually coded CSES Module 6 election studies, and `32_append_country_files.R` appends the country files. The appended micro-level files are then processed by `code/switching/data_preparation/building_analysis_data/`.

More detailed instructions are in [REPRODUCIBILITY.md](REPRODUCIBILITY.md).

## Software

The project is written in R. Package installation and loading are centralized in `code/switching/utils/packages.R`; shared helpers are in `code/switching/utils/helper_functions.R`. Start an R session from the repository root and run:

```r
source("code/switching/utils/packages.R")
load_packages()
```

The same package helper is used by the workflow scripts.

## Citation

If you use this repository, please cite:

Koenders, Tim. *Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families*.

## License

Copyright (c) 2026 Tim Koenders. All rights reserved. See [LICENSE](LICENSE). This repository does not grant permission to redistribute third-party data.
