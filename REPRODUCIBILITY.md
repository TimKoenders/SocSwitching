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

Place the required raw, harmonized, and derived files under `data/` using the local folder structure described there. For the baseline `voteswitchR` countries, a country can require multiple original files, including CSES, European Voter Project, or national election-study files. The generated country-level `*_data_file.RData` objects are then used by scripts `01`-`31`. The direct CSES Module 6 scripts contain the expected names of the original `.dta`, `.sav`, or equivalent files they read.

For local path configuration, copy the template and edit the copy:

```bash
cp config/data_paths_template.yml config/data_paths.yml
```

On Windows PowerShell:

```powershell
Copy-Item config/data_paths_template.yml config/data_paths.yml
```

`config/data_paths.yml` is ignored by Git.

## 3. Install R Packages

Package installation and loading are handled by the project helper:

```r
source("code/switching/utils/packages.R")
load_packages()
```

The helper installs missing CRAN packages and the GitHub dependency `denis-cohen/voteswitchR`.

## 4. Check Inputs

Run the input checker before launching the workflow:

```bash
Rscript code/00_check_inputs.R
```

The checker reads `config/required_inputs.csv`, applies the local folder settings from `config/data_paths.yml`, and prints which inputs are found or missing. For scripts `01`-`31`, it checks the generated `voteswitchR` country bundles. It does not enumerate every raw source file behind those bundles, because that source list is governed by the `voteswitchR` data guide and can include multiple files per country.

## 5. Run Project-Specific Data Preparation

The full data-preparation stage can be launched with:

```bash
Rscript code/00_run_all.R --targets=data
```

Internally, the data-preparation scripts run in the following order:

```text
code/switching/data_preparation/building_micro_data/
code/switching/data_preparation/dependent_variable/
code/switching/data_preparation/independent_variables/
code/switching/data_preparation/building_analysis_data/
```

In `building_micro_data/`, scripts `01`-`31` process the country-level `voteswitchR` bundles produced from the original election-study files. Scripts `33` onward add the manually coded CSES Module 6 election studies. `32_append_country_files.R` appends the country-level files into the combined micro-level dataset.

After the appended micro-level files are available, `building_analysis_data/` constructs the datasets used by the dependent-variable, independent-variable, model, and plotting scripts.

Some scripts depend on local restricted files or locally generated harmonized objects and will fail if those files are absent.

## 6. Estimate Models

Run the model scripts through the workflow:

```bash
Rscript code/00_run_all.R --targets=models
```

The model scripts may create large local objects under `data/analysis/models/`. These are ignored by Git.

## 7. Recreate Figures and Tables

Recreate result tables and figures with:

```bash
Rscript code/00_run_all.R --targets=results,descriptives
```

The complete workflow is:

```bash
Rscript code/00_run_all.R --targets=all
```

Outputs are written to local `plots/` or `data/analysis/` subfolders and are ignored unless explicitly whitelisted.

## 8. What Is and Is Not Reproducible from Git Alone

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
