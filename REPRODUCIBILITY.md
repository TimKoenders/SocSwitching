# Reproducibility Guide

This guide describes the reproduction workflow for users who have lawful access to the required third-party datasets.

## 1. Clone the Repository

```bash
git clone https://github.com/TimKoenders/SocSwitch.git
cd SocSwitch
```

## 2. Obtain the Data

Raw data are not included. See [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md) for source groups, access conditions, and local folder expectations.

The underlying vote-switching harmonization follows [`voteswitchR`](https://github.com/denis-cohen/voteswitchR). This repository assumes that the required `voteswitchR`-style harmonized switching objects are available locally. Additional CSES Module 6 elections were added manually for this project and must also be present locally for full reproduction.

Place the required raw, harmonized, and derived files under `data/` using the local folder structure described there.

## 3. Install R Packages

Package installation and loading are handled by the project helper:

```r
source("code/switching/utils/packages.R")
load_packages()
```

The helper installs missing CRAN packages and the GitHub dependency `denis-cohen/voteswitchR`.

## 4. Run Project-Specific Data Preparation

After the upstream vote-switching harmonization and CSES Module 6 additions are available locally, run the SocSwitch data-preparation scripts in numerical order. The main locations are:

```text
code/switching/data_preparation/building_micro_data/
code/switching/data_preparation/dependent_variable/
code/switching/data_preparation/independent_variables/
code/switching/data_preparation/building_analysis_data/
```

Some scripts depend on local restricted files or locally generated harmonized objects and will fail if those files are absent.

## 5. Estimate Models

Run the model scripts in:

```text
code/switching/model/
```

The model scripts may create large local objects under `data/analysis/models/`. These are ignored by Git.

## 6. Recreate Figures and Tables

Descriptive figures are produced from:

```text
code/switching/descriptives/
```

Model-result figures and tables are produced from the result scripts in:

```text
code/switching/model/
```

Outputs are written to local `plots/` or `data/analysis/` subfolders and are ignored unless explicitly whitelisted.

## 7. What Is and Is Not Reproducible from Git Alone

Reproducible from Git alone:

- The code structure.
- The project-specific data-processing logic after upstream harmonization.
- The modeling and plotting scripts.
- Documentation of data requirements.

Not reproducible from Git alone:

- Raw respondent-level survey files.
- The full upstream vote-switching harmonization procedure, which is documented in `voteswitchR`.
- Processed respondent-level data.
- Imputed respondent-level data.
- Large model objects generated from restricted data.

Full computational reproduction requires authorized access to the underlying data.
