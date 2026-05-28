# Reproducibility Guide

This repository is intended to be reproducible for users who have lawful access to the required third-party datasets.

## 1. Clone the Repository

```bash
git clone https://github.com/TimKoenders/SocSwitch.git
cd SocSwitch
```

## 2. Obtain the Data

Raw data are not included. See [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md) for source groups, access conditions, and local folder expectations.

Place the required files under `data/` using the local folder structure described there.

## 3. Restore the R Environment

If an `renv.lock` file is available, restore the package environment with:

```r
renv::restore()
```

If `renv.lock` has not yet been created, install the packages listed in `code/switching/utils/packages.R`.

## 4. Rebuild the Data

Run the data-preparation scripts in numerical order. The main locations are:

```text
code/switching/data_preparation/building_micro_data/
code/switching/data_preparation/dependent_variable/
code/switching/data_preparation/independent_variables/
code/switching/data_preparation/building_analysis_data/
```

Some scripts depend on local raw survey files and will fail if the corresponding restricted files are absent.

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
- The data-processing logic.
- The modeling and plotting scripts.
- Documentation of data requirements.

Not reproducible from Git alone:

- Raw respondent-level survey files.
- Processed respondent-level data.
- Imputed respondent-level data.
- Large model objects generated from restricted data.

Full computational reproduction requires authorized access to the underlying data.

## Recommended Future Improvements

- Add `renv.lock`.
- Add a single orchestration script, for example `code/00_run_all.R`.
- Add checks that verify required local files before long scripts run.
- Add a small synthetic test dataset to confirm that the pipeline executes without restricted data.
