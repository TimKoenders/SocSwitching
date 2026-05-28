# Local Data Directory

This directory is for local data only. Raw and processed data files are intentionally not tracked by Git because many source datasets are governed by third-party access conditions.

The upstream vote-switching harmonization follows [`voteswitchR`](https://github.com/denis-cohen/voteswitchR). SocSwitch expects locally available `voteswitchR`-style harmonized switching objects, plus the manually added CSES Module 6 election extensions used in this project.

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
