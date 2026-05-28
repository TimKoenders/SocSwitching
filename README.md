# SocSwitch

Code repository for **Social Democracy in Polyadic Competition: Explaining Voter Flows Across Party Families**.

This project studies voter flows between social-democratic parties and their competitors across comparative election contexts. The repository contains the code used to prepare vote-switching data, build contextual measures, estimate the models, and produce the descriptive figures, model results, and appendix material.

## Data Availability

The raw respondent-level election studies used in this project are not redistributed in this repository. Many of the underlying national election studies and comparative survey files are governed by third-party terms of use, registration requirements, or restricted-access agreements. The rights to distribute those data remain with the original data providers.

Researchers with authorized access can reproduce the analyses by obtaining the required files from the original providers and placing them in the local folder structure described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md). The repository is therefore designed as an **open-code, restricted-data** replication package.

Where legally permitted, derived aggregate outputs may be shared separately. Raw survey files, harmonized respondent-level data, imputed data, processed microdata, and large model objects should not be committed to this repository.

## Repository Structure

```text
code/
  switching/
    data_preparation/      # Microdata harmonization, dependent variables, context data
    descriptives/          # Descriptive plots and tables
    model/                 # Model estimation and result scripts
    utils/                 # Shared package and helper scripts
data/
  README.md                # Local data folder instructions
  data_manifest.csv        # Machine-readable guide to required data groups
plots/
  README.md                # Local output folder instructions
DATA_AVAILABILITY.md       # Source-level data access information
REPRODUCIBILITY.md         # How to recreate outputs once data are available
```

## Reproducibility

The analyses are reproducible conditional on lawful access to the underlying data. A typical workflow is:

1. Obtain the required survey, party, election, and contextual datasets from the original providers.
2. Place the files under `data/` using the structure described in [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md).
3. Restore the R package environment if an `renv.lock` file is available.
4. Run the numbered scripts in `code/switching/data_preparation/`.
5. Run the scripts in `code/switching/model/`.
6. Run the scripts in `code/switching/descriptives/` and result scripts to recreate figures and tables.

More detailed instructions are in [REPRODUCIBILITY.md](REPRODUCIBILITY.md).

## Software

The project is written in R. Package loading is centralized in `code/switching/utils/packages.R` and shared helpers are in `code/switching/utils/helper_functions.R`.

For formal replication, the recommended next step is to initialize `renv` and commit `renv.lock`:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

## Citation

If you use this repository, please cite the associated paper. A formal citation file can be added once the paper metadata are final.

## License

No license is currently declared. Until a license is added, all rights are reserved by the author. This repository does not grant permission to redistribute third-party data.
