# Local Data Directory

This directory is for local data only. Raw and processed data files are intentionally not tracked by Git because many source datasets are governed by third-party access conditions.

The upstream vote-switching harmonization follows [`voteswitchR`](https://github.com/denis-cohen/voteswitchR). SocSwitch expects the original restricted `.dta`, `.sav`, or equivalent survey files to be stored locally under the folders used by `voteswitchR` and by the direct country scripts. A country can require multiple raw source files from CSES, the European Voter Project, or national election studies. Scripts `01`-`31` in `code/switching/data_preparation/building_micro_data/` start from generated `voteswitchR` country bundles, scripts `33` onward add the manually coded CSES Module 6 election studies, and `32_append_country_files.R` appends them.

Use this structure:

```text
data/
  micro/
  ess/
  eurobarometer/
  manifesto/
  parlgov/
  processed/
  analysis/
  saved/
```

See `DATA_AVAILABILITY.md` in the repository root for details.
