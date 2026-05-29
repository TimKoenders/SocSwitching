# Data Availability

This repository does not include raw respondent-level election studies or processed respondent-level versions derived from them. Many of the underlying datasets are distributed by third-party providers under terms that require registration, acceptance of conditions, data-use agreements, or research proposals. Users who wish to fully reproduce the analyses must obtain the data directly from the original providers.

The repository provides code and documentation for authorized users to rebuild the SocSwitch analysis locally with the required vote-switching data infrastructure.

## Vote-Switching Harmonization

The baseline harmonization procedure for the vote-switching data follows [`voteswitchR`](https://github.com/denis-cohen/voteswitchR). That package documents the general workflow for harmonizing, imputing, mapping, raking, and aggregating comparative vote-switching data.

SocSwitch does not duplicate the full `voteswitchR` harmonization documentation. Replicators should consult `voteswitchR` for the upstream procedure and then apply the project-specific extensions used here. In particular, additional CSES Module 6 elections were added manually for this project.

## General Policy

- Raw survey data are not redistributed.
- Harmonized respondent-level data are not redistributed.
- Imputed respondent-level data are not redistributed.
- Large generated model objects and local intermediate files are not redistributed.
- Derived aggregate outputs may only be shared when permitted by the relevant data licenses.

## Expected Local Folder Structure

```text
data/
  files/          # Restricted original survey files in election/provider subfolders
  micro/          # Generated country-level microdata bundles and appended microdata
  ess/            # European Social Survey files used for validation/contextual checks
  eurobarometer/  # Eurobarometer files used for demand-side salience measures
  manifesto/      # Manifesto Project / MARPOR files
  parlgov/        # ParlGov files
  processed/      # Locally generated intermediate data
  analysis/       # Locally generated analysis-ready data and model inputs
  saved/          # Local saved objects or archived intermediate outputs
```

These folders are intentionally ignored by Git except for README files and the manifest.

## Data Source Groups

| Data group | Role in project | Access notes | Local location |
|---|---|---|---|
| National election studies | Respondent-level vote choice and lagged vote choice data used to construct switching measures | Access conditions vary by country and election. Some require only terms-of-use acceptance; others require registration, requests, or data-use agreements. | `data/files/` |
| CSES modules | Comparative respondent-level election studies for several election contexts, including manually added CSES Module 6 elections | Download from CSES after accepting terms of use. A single CSES module file can cover several election subfolders. | `data/files/` |
| European Voter / national continuity files | Respondent-level vote-switching data for selected historical contexts | May require access through GESIS or other providers. A single continuity file can cover several election subfolders. | `data/files/` |
| voteswitchR harmonization infrastructure | Upstream mapping and vote-switching infrastructure used to harmonize or compare switching data | Consult [`voteswitchR`](https://github.com/denis-cohen/voteswitchR); respect the original package and data terms. | local R package / `data/processed/` |
| Manifesto Project / MARPOR | Party positions and party metadata | Download from Manifesto Project after accepting provider terms. | `data/manifesto/` |
| ParlGov | Party and election metadata, vote shares, cabinet information | Public data source; cite according to ParlGov instructions. | `data/parlgov/` |
| Eurobarometer | Demand-side issue salience measures | Access through GESIS or official Eurobarometer distribution channels; terms vary by file. | `data/eurobarometer/` |
| European Social Survey | Validation and contextual individual-level measures where used | Download after registration and acceptance of ESS terms. | `data/ess/` |
| CPDS or other macro controls | Country-year controls where used | Access according to provider terms. | `data/external/` |

## File Manifest

The file [data/data_manifest.csv](data/data_manifest.csv) gives a machine-readable overview of the expected data groups, local folders, and redistribution status. It is source-level because the restricted files are obtained directly from third-party providers and can differ in file names or delivery formats.

## Local File Names

The raw survey staging folder is `data/files/`. It contains subfolders for election contexts or provider-specific bundles used by the `voteswitchR` data procurement workflow. These subfolders are not necessarily one-to-one with unique raw datasets: several election subfolders can use the same source file when a CSES module, European Voter Project file, or national continuity file covers multiple elections.

The baseline country scripts `01`-`31` in `code/switching/data_preparation/building_micro_data/` start from country-level `data_file` objects generated by `voteswitchR::build_data_file()` and stored under `data/micro/`. Those generated objects are not one-to-one raw source files. Depending on the country and election coverage, the underlying raw files can include multiple CSES files, European Voter Project files, or national election-study files.

The manually added CSES Module 6 scripts, from `33` onward, contain the expected names of the original `.dta`, `.sav`, or equivalent survey files where they are read directly. `32_append_country_files.R` combines the country-level outputs.

The same information is summarized in `config/required_inputs.csv`. For scripts `01`-`31`, the manifest checks for the generated `voteswitchR` country bundles in `data/micro/` and for the presence of the raw staging root `data/files/`; it does not enumerate every original raw file used to build the bundles. To adapt paths to a local machine, copy `config/data_paths_template.yml` to `config/data_paths.yml` and edit the folder locations. `config/data_paths.yml` is ignored by Git.

## Notes for Replicators

1. Consult `voteswitchR` for the baseline vote-switching harmonization procedure.
2. Obtain the raw files from the original providers.
3. Place all original files under `data/files/` in the subfolders used by `voteswitchR` and by the direct CSES Module 6 scripts.
4. Run `Rscript code/00_check_inputs.R`.
5. Run `Rscript code/00_run_all.R --targets=data` for data preparation or `Rscript code/00_run_all.R --targets=all` for the complete workflow.
6. Do not commit raw or processed restricted data to Git.

Provider access conditions and file names are governed by the original data providers.
