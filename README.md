# tfl — EphiVIZ Clinical TLF Builder

A complete **R Shiny** application for generating clinical **Tables, Listings, and Figures (TLF)** from ADaM datasets.

## Quick Start

```r
# Install dependencies
install.packages(c(
  "shiny", "shinyjs", "dplyr", "tidyr", "ggplot2", "ggrepel",
  "DT", "haven", "readr", "survival", "flextable", "officer",
  "scales", "openxlsx", "patchwork"
))

# Optional: real pharmaverse ADaM data
install.packages("pharmaverseadam",
  repos = c("https://pharmaverse.r-universe.dev", getOption("repos")))

# Run the app
shiny::runApp("EphiVIZ_shiny")
```

## Features

| Screen | Description |
|--------|-------------|
| 📂 **Data › Dataset Loader** | Load ADSL, ADAE, ADLB, ADVS, ADCM, ADEFF from pharmaverseadam or upload SAS7BDAT / XPT / CSV |
| 🔍 **Data › Explorer** | Multi-filter data browser with per-column filter widgets (sliders, checkboxes, date ranges), quick search, active filter count badge, CSV/Excel export |
| 📊 **Data › Summary Dashboard** | Overview cards, data specs table, distribution plots (variable, treatment arm, missing data heatmap), correlation matrix, data quality flags |
| 📊 **Tables** | Demographics (14.1.1), AE Summary (14.3.1), AE by SOC/PT (14.3.2), Lab Summary (14.4.1), Primary Endpoint (14.2.1) — all with RTF download |
| 📋 **Listings** | All AEs, Serious AEs, Lab Abnormalities, Concomitant Meds, Subject Disposition, Vital Signs — with RTF download |
| 📈 **Figures** | eDISH (Hy's Law), KM Curve, Waterfall, Volcano, Lab Box Plot — all via ggplot2, with PNG download |

## File Structure

```
EphiVIZ_shiny/
├── app.R                  # Main Shiny app (UI + server)
├── DEPENDENCIES.md        # Package install instructions
├── www/
│   └── style.css          # Full EphiVIZ CSS (brown/orange palette)
└── R/
    ├── data_utils.R       # pharmaverseadam loader + upload + synthetic fallback
    ├── table_utils.R      # Table generation (demographics, AE, lab, efficacy)
    ├── listing_utils.R    # Listing generation (6 listing types)
    ├── figure_utils.R     # eDISH, KM, Waterfall, Volcano, Lab Box
    ├── explorer_utils.R   # Data Explorer filter logic + UI widgets
    └── summary_utils.R    # Summary Dashboard plots + DQ flags
```

## Design

- **No Bootstrap navbars** — pure custom CSS navigation with the EphiVIZ brown/orange palette (`#431407` / `#f97316`)
- App width 1340 × 840 px; sidebar 220 px
- Sub-tabs inside the Data screen for Loader / Explorer / Summary Dashboard
- All DT tables use header colour `#431407` with even-row `#fff7ed` striping
