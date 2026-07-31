# EffortDataTools 

<!-- badges: start -->
[![R-CMD-check](https://github.com/afsc-gap-products/EffortDataTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/afsc-gap-products/EffortDataTools/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The **`EffortDataTools`** R package provides a suite of utility functions for groundfish survey data managers and scientists working with NOAA Fisheries Resource Assessment and Conservation Engineering (RACE) survey data.

It streamlines key data auditing and field preparation workflows:
1. Extracting filtered **repick haul records** directly from Oracle RACE edit tables.
2. Auditing unfinalized haul data for **depth anomalies** and **abundance haul issues**.
3. Merging and time-trimming **Scientific Computing System (SCS) XML** data streams.
4. Visualizing **haul tracks** overlaid on survey grid cell maps shaded by depth stratum.

---

## Installation

You can install the development version of `EffortDataTools` directly from GitHub using `pak` or `remotes`:

```r
# install.packages("pak")
pak::pak("afsc-gap-products/EffortDataTools")

# Or via remotes:
# remotes::install_github("afsc-gap-products/EffortDataTools")