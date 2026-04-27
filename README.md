# CES: Co-culture Efficacy Score Framework

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![App](https://img.shields.io/badge/App-Available_Now-FF9800?logo=rstudio)](https://<link-to-your-shiny-app>)

**CES** is a quantitative computational framework and interactive web application for measuring context-dependent compound activity in complex multicellular systems.

It integrates potency and efficacy into a single interpretable score, separating general compound toxicity from true interaction-dependent modulation. CES is applicable to diverse experimental setups, including cancer immunotherapy and host-pathogen antiviral screens.

## Table of Contents
- [Repository Structure](#repository-structure)
- [Web Application](#web-application)
  - [Local Installation](#local-installation)
- [Manuscript Reproducibility](#manuscript-reproducibility)
- [Methodology](#methodology)
- [Citation](#citation)
- [Contact](#contact)

## Repository Structure

The code in this repository is organized into three primary directories:

* **`app/`**
  Contains the complete source code, user interface, and graphical assets for the CES Shiny web application.

* **`manuscript_code/`**
  Provides the data and R scripts necessary to reproduce the figures and analyses presented in our publication. This also includes a standalone script (`example_workflow.R`) demonstrating how to apply the core methodology outside of the web app.

* **`R/`**
  Houses the core mathematical and statistical modeling functions that power both the web application and the standalone analysis scripts.

## Web Application

The CES Shiny application provides a graphical interface for data processing, quality control, dose-response modeling, and CES calculation.

### Features
* **Data Processing:** Upload raw plate reader signals (Annotation files) or pre-processed inhibition/viability data.
* **Toxicity Tuning:** Adjust the Control DSS threshold interactively to classify compound toxicity.
* **Outputs:** Generate and export dose-response curves, spatial QC plots, and CES distribution rankings.

### Local Installation

To run the CES application locally, clone this repository and execute it via R or RStudio:

    # Clone the repository
    git clone https://github.com/dias-dio/CES.git

    # Install required packages
    install.packages(c("shiny", "bslib", "shinyjs", "ggplot2", "plotly", "dplyr", "DT", "readr", "readxl", "writexl"))

    # Run the application
    shiny::runApp("CES/app")

## Manuscript Reproducibility

To reproduce the analyses and figures from the CES publication, refer to the manuscript_code/ directory.

* scripts/: Numbered R scripts to reproduce Figures 2 through 5.
* example_workflow.R: A standalone script detailing how to apply the CES mathematical framework in an R environment without the web interface.

## Methodology

The CES pipeline operates in four stages:

1. **Quality Control & Integration:** Raw measurements undergo plate-level QC. Co-culture interaction profiles are computed by adjusting co-culture responses against target-only and effector-only baselines.
2. **Gaussian Mixture Modeling:** Co-culture profiles are modeled using a mixture of symmetric Gaussian functions to capture monotonic and non-linear response landscapes.
3. **Feature Extraction:** Potency (normalized AUC) and maximal efficacy (Peak) are extracted from the fitted model.
4. **Integration:** Extracted features are mathematically integrated to calculate the final Co-culture Efficacy Score (CES).

## Citation

A manuscript detailing the CES methodology is currently in preparation. If you use CES or this code in your research, please cite this repository until the formal DOI is available:

> Dias, D., et al. (2026). CES: A robust computational framework for quantifying context-dependent compound activity in multicellular systems. GitHub Repository: https://github.com/dias-dio/CES.

## Contact

**Diogo Dias**
WebApp Maintainer and Method Developer
Email: diogo.dias@helsinki.fi

Developed in collaboration with:
* Institute for Molecular Medicine Finland (FIMM)
* Hematology Research Unit Helsinki (HRUH)
* University of Helsinki
