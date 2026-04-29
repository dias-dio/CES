# CES: Co-culture Efficacy Score

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![App](https://img.shields.io/badge/Shiny_app-Available-FF9800?logo=rstudio)](https://<link-to-your-shiny-app>)

**CES** is a computational framework and interactive web application for quantifying compound activity in multicellular co-culture systems.

It integrates cumulative activity and efficacy into a single interpretable score, separating general compound toxicity from true effector-mediated modulation. CES supports diverse experimental setups, including cancer immunotherapy drug screening and host-pathogen antiviral assays.

---

## Table of contents

- [Repository structure](#repository-structure)
- [Web application](#web-application)
- [Manuscript reproducibility](#manuscript-reproducibility)
- [Methodology](#methodology)
- [Citation](#citation)
- [Contact](#contact)

## Repository structure

```text
CES/
├── app/                 # Shiny web application (UI, server logic, assets)
├── manuscript_code/     # Scripts and data to reproduce publication figures
│   ├── data/            # Datasets required for the scripts
│   ├── CES_run.R        # Standalone workflow example
│   ├── CES_Figure_2.R   # Figure 2 reproduction script
│   ├── CES_Figure_3.R   # Figure 3 reproduction script
│   ├── CES_Figure_4.R   # Figure 4 reproduction script
│   └── CES_Figure_5.R   # Figure 5 reproduction script
└── R/                   # Core mathematical and statistical functions
```

* **`app/`** — Full source code for the CES Shiny web application.
* **`manuscript_code/`** — Data and R scripts to reproduce all figures and analyses from the publication, plus a standalone workflow example.
* **`R/`** — Core modeling functions shared by the web application and standalone scripts.

## Web application

The CES Shiny app provides a graphical interface for data processing, quality control, dose-response modeling, and CES calculation.

### Features

* **Data processing** — Upload raw plate-reader signals via annotation files, or supply pre-processed inhibition/viability data directly.
* **Toxicity tuning** — Interactively adjust the control DSS threshold to classify compound toxicity.
* **Outputs** — Generate and export dose-response curves, spatial QC plots, and CES distribution rankings.

### Requirements

<details>
<summary>R packages (click to expand)</summary>

**UI and web:**

```r
shiny, bslib, plotly, DT, shinyjs
```

**Data processing:**

```r
readxl, readr, dplyr, data.table, openxlsx
```

**Modeling and plotting:**

```r
ggplot2, drc, caTools, minpack.lm, DEoptim
```

</details>

### Local installation

Clone the repository and launch the app from R or RStudio:

```r
# Clone the repository
# git clone https://github.com/dias-dio/CES.git

# Install required packages
install.packages(c(
  "shiny", "bslib", "shinyjs", "plotly", "DT",
  "readxl", "readr", "dplyr", "data.table", "openxlsx",
  "ggplot2", "drc", "caTools", "minpack.lm", "DEoptim"
))

# Run the application
shiny::runApp("CES/app")
```

## Manuscript reproducibility

All materials needed to reproduce the figures and analyses from the CES publication are in `manuscript_code/`. The directory structure is flat, so all scripts can access the required datasets and core functions using relative paths.

* **`data/`** — All necessary datasets.
* **`CES_Figure_2.R` to `CES_Figure_5.R`** — Scripts to reproduce the exact figures found in the manuscript.
* **`CES_run.R`** — Standalone script demonstrating the CES mathematical framework in a plain R environment, without the web interface.

### Requirements

<details>
<summary>R packages (click to expand)</summary>

```r
install.packages(c(
  "dplyr", "tidyr", "tibble", "ggplot2", "reshape2",
  "ggrepel", "scales", "patchwork", "pheatmap", "grid",
  "Hmisc", "openxlsx",
  "drc", "data.table", "caTools", "minpack.lm", "DEoptim"
))

# pcaMethods is installed via Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("pcaMethods")
```

</details>

## Methodology

The CES pipeline operates in four stages:

1. **Quality control and integration** — Raw measurements undergo plate-level QC. Co-culture interaction profiles are computed by adjusting co-culture responses against target-only and effector-only baselines.
2. **Gaussian mixture modeling** — Interaction profiles are fitted with a mixture of symmetric Gaussian functions to capture monotonic and non-linear dose-response landscapes.
3. **Feature extraction** — Cumulative activity (nAUC) and maximal efficacy (peak) are derived from the fitted model.
4. **Score integration** — Extracted features are mathematically combined to yield the final CES.

## Citation

A manuscript describing the CES methodology is currently in preparation. In the meantime, please cite this repository:

> Dias, D. et al. (2026). CES: a computational framework for quantifying compound activity in multicellular systems. GitHub repository: https://github.com/dias-dio/CES

## Contact

**Diogo Dias**
Method developer and web-app maintainer
📧 diogo.dias@helsinki.fi

Developed in collaboration with the Institute for Molecular Medicine Finland (FIMM), the Hematology Research Unit Helsinki (HRUH), and the University of Helsinki.
