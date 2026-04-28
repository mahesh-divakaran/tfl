# EphiVIZ Shiny App — Dependency Information

## How to Run

```r
# 1. Install required packages (one-time)
install.packages(c(
  "shiny", "shinyjs", "dplyr", "tidyr",
  "ggplot2", "ggrepel", "DT", "haven",
  "readr", "survival", "flextable", "officer",
  "scales", "openxlsx", "patchwork"
))

# Optional (for real pharmaverse ADaM data)
install.packages("pharmaverseadam",
  repos = c("https://pharmaverse.r-universe.dev", getOption("repos")))

# 2. Run the app
shiny::runApp("EphiVIZ_shiny")
```

## Required R Packages

| Package | Purpose |
|---------|---------|
| shiny (>= 1.7.0) | Core Shiny framework |
| shinyjs | JavaScript helpers, show/hide, CSS animations |
| dplyr | Data manipulation |
| tidyr | Data reshaping |
| ggplot2 | All figures (eDISH, KM, Waterfall, Volcano, Lab Box) |
| ggrepel | Non-overlapping labels on eDISH and Volcano plots |
| DT | Interactive datatables for all TLF outputs |
| haven | Read SAS (.sas7bdat, .xpt) uploaded files |
| readr | Read CSV uploaded files |
| survival | KM curve computation |
| flextable | RTF table/listing export |
| officer | RTF document handling |
| scales | Axis formatting (percent, comma) |
| openxlsx | Excel export from Data Explorer |
| patchwork | Multi-panel plot layouts in Summary Dashboard |

## Optional Packages

| Package | Purpose |
|---------|---------|
| pharmaverseadam | Real CDISC ADaM example datasets (ADSL, ADAE, ADLB, etc.) |
| survminer / ggsurvfit | Enhanced KM plots (future enhancement) |

## Notes

- If `pharmaverseadam` is not installed, the app automatically generates
  realistic synthetic ADaM data (~254 subjects) as fallback.
- All R versions >= 4.2.0 are supported.
