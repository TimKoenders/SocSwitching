# Data Availability

This repository does not include raw respondent-level election studies or processed respondent-level versions derived from them. Many of the underlying datasets are distributed by third-party providers under terms that require registration, acceptance of conditions, data-use agreements, or research proposals. Users who wish to fully reproduce the analyses must obtain the data directly from the original providers.

The repository provides code and documentation for authorized users to rebuild the SocSwitch analysis locally once the required vote-switching data infrastructure has been reconstructed.

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
  micro/          # Restricted files used by voteswitchR and manual CSES 6 additions
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
| National election studies | Respondent-level vote choice and lagged vote choice data used to construct switching measures | Access conditions vary by country and election. Some require only terms-of-use acceptance; others require registration, requests, or data-use agreements. | `data/micro/` |
| CSES modules | Comparative respondent-level election studies for several election contexts, including manually added CSES Module 6 elections | Download from CSES after accepting terms of use. | `data/micro/` |
| European Voter / national continuity files | Respondent-level vote-switching data for selected historical contexts | May require access through GESIS or other providers. | `data/micro/` |
| voteswitchR harmonization infrastructure | Upstream mapping and vote-switching infrastructure used to harmonize or compare switching data | Consult [`voteswitchR`](https://github.com/denis-cohen/voteswitchR); respect the original package and data terms. | local R package / `data/processed/` |
| Manifesto Project / MARPOR | Party positions and party metadata | Download from Manifesto Project after accepting provider terms. | `data/manifesto/` |
| ParlGov | Party and election metadata, vote shares, cabinet information | Public data source; cite according to ParlGov instructions. | `data/parlgov/` |
| Eurobarometer | Demand-side issue salience measures | Access through GESIS or official Eurobarometer distribution channels; terms vary by file. | `data/eurobarometer/` |
| European Social Survey | Validation and contextual individual-level measures where used | Download after registration and acceptance of ESS terms. | `data/ess/` |
| CPDS or other macro controls | Country-year controls where used | Access according to provider terms. | `data/processed/` or `data/analysis/` |

## File Manifest

The file [data/data_manifest.csv](data/data_manifest.csv) gives a machine-readable overview of the expected data groups, local folders, and redistribution status. It is intentionally source-level rather than file-level until the final replication archive is frozen.

## Notes for Replicators

1. Consult `voteswitchR` for the baseline vote-switching harmonization procedure.
2. Obtain the raw files from the original providers.
3. Apply the project-specific manual additions for CSES Module 6 elections.
4. Place the locally generated harmonized and derived files in the expected directories.
5. Run the SocSwitch data-preparation, model, and plotting scripts in order.
6. Do not commit raw or processed restricted data to Git.

If a data provider changes access conditions or file names, update this document and `data/data_manifest.csv`.
