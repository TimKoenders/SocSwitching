# Local Data Directory

This directory is for local data only. Raw and processed data files are intentionally not tracked by Git because many source datasets are governed by third-party access conditions.

The upstream vote-switching harmonization follows [`voteswitchR`](https://github.com/denis-cohen/voteswitchR). SocSwitch expects original restricted `.dta`, `.sav`, or equivalent survey files under `data/files/`, in the election or provider subfolders used by `voteswitchR` and by the direct country scripts. Some subfolders can use the same raw dataset when one CSES or European Voter Project file covers multiple elections. Scripts `01`-`31` in `code/data_preparation/building_micro_data/` start from generated `voteswitchR` country bundles under `data/micro/`; scripts `33` onward add the manually coded CSES Module 6 election studies; and `32_append_country_files.R` appends them.

Use this structure:

```text
data/
  files/
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
