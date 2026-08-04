------------------------------------------------------------------------

# EffortDataTools 

The **`EffortDataTools`** R package provides utilities for NOAA Fisheries groundfish survey personnel checking effort data in season.

------------------------------------------------------------------------

## Installation

You can install the development version of `EffortDataTools` directly from GitHub using `pak` or `remotes`:

```{r}
# install.packages("pak") 
pak::pak("afsc-gap-products/EffortDataTools")

# Or via remotes:
# remotes::install_github("afsc-gap-products/EffortDataTools")
```

To build and view package vignettes locally during installation, pass build_vignettes = TRUE:

```{r}
remotes::install_github("afsc-gap-products/EffortDataTools", 
                        build_vignettes = TRUE
                        ) 
```

## Prerequisites

- **Oracle Database Access**: Functions querying database tables directly require network access to the NOAA RACE database and active authentication credentials via `gapindex`.

- **Dependencies**: `EffortDataTools` relies on `gapindex`, `akgfmaps`, `sf`, `dplyr`, `xml2`, and `graphics`.

------------------------------------------------------------------------

## Function Summary

| Function | Primary Purpose | Key Outputs |
|------------------------|------------------------|------------------------|
| `check_haul_depths()` | Scans unfinalized haul data for missing depths or gear depth exceeding bottom depth. | Data frame of depth anomalies |
| `check_haul_abundance()` | Checks unfinalized haul data for improper gear, accessories, bad performance, or duplicate stations. | Data frame of flagged hauls with issues |
| `pull_repick_data()` | Queries RACE edit tables for repick haul metadata and writes results to CSV. | CSV file & data frame |
| `plot_haul_track()` | Plots haul GPS tracks over station grids colored by depth stratum using `akgfmaps`. | Base R plot & sf spatial list |
| `merge_scs_files()` | Combines chronologically ordered SCS XML data files with optional local time-trimming. | Merged XML file & xml_document object |

------------------------------------------------------------------------

## Quick Start Example

```{r}
library(EffortDataTools)
library(gapindex)

# 1. Connect to the Oracle database
con <- gapindex::get_connected(check_access = FALSE)

# 2. Extract repick haul metadata to a local CSV file
pull_repick_data(
  cruise = "202601",
  vessel = "176",
  channel = con,
  output_path = "haul_data_202601_176.csv"
)

# 3. Plot haul track over survey grid cells
plot_haul_track(cruise = 202601, 
                vessel = 176, 
                haul = 194,
                region = "AI", 
                channel = con
                )

# Audit depth anomalies
check_haul_depths(cruise = 202601, 
                  region = "AI", 
                  channel = con
                  )

# Check for protocol violations (gear, accessories, performance, tow duration, duplicates)
check_haul_abundance(cruise = 202601,
                     region = "AI", 
                     channel = con
                     )
```

------------------------------------------------------------------------

## Viewing Vignettes in RStudio

`EffortDataTools` includes detailed package vignettes that walk through complete workflows. Once installed, you can list and open them directly in RStudio:

```{r}
# List all available vignettes in the package 
vignette(package = "EffortDataTools")

# Open the main workflow vignette in RStudio's Help panel
vignette("getting-started", package = "EffortDataTools")
```
