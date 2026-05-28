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
  switching/
    data_preparation/      # Project-specific preparation after voteswitchR-style harmonization
    descriptives/          # Descriptive plots and tables
    model/                 # Model estimation and result scripts
    utils/                 # Shared package and helper scripts
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
3. Add the project-specific CSES Module 6 election extensions locally.
4. Place the local harmonized and derived files under `data/` using the structure described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md).
5. Install and load the required R packages through `code/switching/utils/packages.R`.
6. Run the SocSwitch data-preparation, model, descriptive, and result scripts to recreate figures and tables.

More detailed instructions are in [REPRODUCIBILITY.md](REPRODUCIBILITY.md).

## Software

The project is written in R. Package installation and loading are centralized in `code/switching/utils/packages.R`; shared helpers are in `code/switching/utils/helper_functions.R`. Start an R session from the repository root and run:

```r
source("code/switching/utils/packages.R")
load_packages()
```

## Citation

If you use this repository, please cite:

Koenders, Tim. *Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families*.

## License

Copyright (c) 2026 Tim Koenders. All rights reserved. See [LICENSE](LICENSE). This repository does not grant permission to redistribute third-party data.
