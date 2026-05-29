# Validation

This file records the workflow checks used to validate the repository structure and execution order.

## Last Validated

May 29, 2026.

## Local Validation Environment

- Repository path: `C:/Users/koend/Documents/Codex/2026-05-28/wat-kan-codex-beter-dan-chatgpt/SocSwitch-reproducible`
- Restricted input data: available locally, not tracked by Git
- Operating system: Windows
- R workflow entry point: `code/00_run_all.R`

## Checks Run

The complete workflow was run successfully before the final folder flattening:

```bash
Rscript code/00_run_all.R --targets=all
```

After the folder flattening, the following checks were run successfully:

```bash
Rscript -e "files <- list.files('code', pattern = '[.]R$', recursive = TRUE, full.names = TRUE); invisible(lapply(files, parse))"
Rscript code/00_check_inputs.R
Rscript code/00_run_all.R --targets=data
Rscript code/00_run_all.R --targets=results,descriptives
```

After adding quiet workflow logging, the runner was checked again with:

```bash
Rscript code/00_run_all.R --targets=check
Rscript code/00_run_all.R --targets=results,descriptives
```

## Expected Warnings

Some model-result scripts can print convergence or deprecation warnings from upstream R packages, including `mclogit` convergence messages and `ggplot2` `geom_errorbarh()` lifecycle warnings. These warnings were present during validation, but the workflow completed.

## Data Boundary

Full computational validation depends on local restricted survey files and generated harmonized objects that are not redistributed. This repository validates the project code, execution order, and local reproducibility workflow conditional on authorized access to those data.
