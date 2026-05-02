# CES: Co-culture Efficacy Score

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

CES is a computational framework for quantifying compound activity in multicellular co-culture systems. It integrates cumulative activity and efficacy into a single interpretable score, separating general compound toxicity from true effector-mediated modulation. The framework supports diverse experimental setups including cancer immunotherapy drug screening and host-pathogen antiviral assays.

An interactive Shiny web-app is implemented for data processing, quality control, dose-response modeling, and CES calculation without requiring programming experience.

<p align="center">
  <img src="CES_logo.png" alt="CES logo" width="600">
</p>

## Repository structure

```
CES/
├── R/
│   ├── CES_functions.R          # Core CES functions (scoring, curve fitting, QC)
│   ├── preprocess_CES_data.R    # Merge annotation, experiment info, and raw data
│   └── K562_example_layout/     # Example input files for the preprocessing script
│       ├── anno file
│       ├── exp.info file
│       └── raw/
├── app/                         # Shiny web application
├── Dockerfile                   # Docker image for local deployment
└── manuscript_code/
    ├── data/                    # Datasets for figure reproduction
    ├── CES_run.R                # Standalone workflow example
    ├── CES_Figure_2.R
    ├── CES_Figure_3.R
    ├── CES_Figure_4.R
    └── CES_Figure_5.R
```

`R/` contains the core scoring functions, a preprocessing script that merges raw plate-reader files (annotation, experiment info, and signal data) into the format expected by the Shiny app, and example input files for a blood cancer cell line illustrating the required layout. `app/` contains the full source code for the Shiny web application. `manuscript_code/` contains the data and scripts needed to reproduce all figures from the publication, along with a standalone workflow example (`CES_run.R`) demonstrating the CES pipeline in a plain R environment.

## Getting started

Clone the repository:

```bash
git clone https://github.com/dias-dio/CES.git
cd CES
```

### Running the web application

#### Option 1: Docker (recommended)

Docker packages the app with all dependencies, so there is nothing to install manually.

**Pull from Docker Hub (fastest):**

```bash
docker pull diasdio/ces-app:latest
docker run -p 3838:3838 diasdio/ces-app:latest
```

**Or build locally from source:**

```bash
docker build -t ces-app .
docker run -p 3838:3838 ces-app
```

Then open [http://localhost:3838](http://localhost:3838) in your browser.

#### Option 2: Local R installation

Install the required packages and launch the app:

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "plotly", "DT",
  "readxl", "readr", "dplyr", "data.table", "openxlsx",
  "ggplot2", "drc", "caTools", "minpack.lm", "DEoptim", "tibble"
))

shiny::runApp("app")
```

The app allows uploading raw plate-reader data via annotation files or supplying pre-processed inhibition data directly. It includes interactive toxicity threshold tuning, dose-response curve visualization, spatial QC plots, and CES distribution exports. To prepare raw data for the app, see `R/preprocess_CES_data.R` and the example layout in `R/K562_example_layout/`.

### Reproducing manuscript figures

Install the required packages:

```r
install.packages(c(
  "dplyr", "tidyr", "tibble", "forcats",
  "ggplot2", "ggrepel", "scales", "patchwork",
  "reshape2", "pheatmap", "grid",
  "Hmisc", "openxlsx",
  "drc", "data.table", "caTools", "minpack.lm", "DEoptim"
))

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("pcaMethods")
```

Then run any of the figure scripts from within `manuscript_code/`:

```r
setwd("manuscript_code")
source("CES_Figure_2.R")
```

All scripts use relative paths and expect to be run from the `manuscript_code/` directory.

### Running the CES pipeline directly

`manuscript_code/CES_run.R` demonstrates the full scoring pipeline in a plain R environment. It supports both 2-condition (co-culture and target monoculture) and 3-condition (adding effector monoculture) setups via the `run_CES()` wrapper function defined in `R/CES_functions.R`.

Each input data frame (`df_cc`, `df_mono`, `df_ctrl`) should contain data for a single drug with four columns: `DRUG_NAME`, `CONCENTRATION`, `SCREEN_NAME`, `PERCENT_INHIBITION`.

| Argument  | Description                    | Required                          |
|-----------|--------------------------------|-----------------------------------|
| `df_cc`   | Co-culture condition           | Yes                               |
| `df_mono` | Target monoculture condition   | Yes                               |
| `df_ctrl` | Effector monoculture condition | No (omit for 2-condition scoring) |

```r
# Single drug, 3-condition therapeutic scoring
out <- run_CES(df_cc, df_mono, df_ctrl, scoring_model = "therapeutic")
print(out$results)

# Single drug, 2-condition scoring (no effector data)
out <- run_CES(df_cc, df_mono)

# Full compound library
results_all <- do.call(rbind, lapply(drugs, function(d) {
  run_CES(
    df_cc = coculture_data[coculture_data$DRUG_NAME == d, ],
    df_mono = mono_data[mono_data$DRUG_NAME == d, ],
    df_ctrl = control_data[control_data$DRUG_NAME == d, ],
    scoring_model = "therapeutic",
    plot = FALSE
  )$results
}))
```

## Methodology

The CES pipeline operates in four stages:

1. Raw measurements undergo plate-level quality control. Co-culture interaction profiles are computed by adjusting co-culture responses against target-only and effector-only baselines.
2. Interaction profiles are fitted with a mixture of symmetric Gaussian functions to capture monotonic and non-linear dose-response shapes.
3. Cumulative activity (nAUC) and maximal efficacy (Peak) are extracted from the fitted model.
4. The extracted features are mathematically combined into the final CES.

Two scoring models are supported: a therapeutic model that penalizes general effector-cell toxicity, and a mechanistic model that isolates effector-mediated modulation by adjusting for effector viability. Full mathematical details are provided in the accompanying manuscript.

## Citation

> Dias, D. et al. (2026). Selective scoring of drug effects in multicellular co-culture systems. GitHub repository: https://github.com/dias-dio/CES

## Contact

* **General Inquiries and Collaborations:** Diogo Dias - [diogo.dias@helsinki.fi](mailto:diogo.dias@helsinki.fi)
* **Web Application Support:** [cesframework@outlook.com](mailto:cesframework@outlook.com)

Developed at the Institute for Molecular Medicine Finland (FIMM), the Hematology Research Unit Helsinki (HRUH), and the University of Helsinki.
