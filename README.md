# CES: Co-culture Efficacy Score

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![App](https://img.shields.io/badge/Shiny_app-Available-FF9800?logo=rstudio)](https://<link-to-your-shiny-app>)

**CES** is a computational framework and interactive web application for quantifying context-dependent compound activity in multicellular co-culture systems.

It integrates potency and efficacy into a single interpretable score, separating general compound toxicity from true interaction-dependent modulation. CES supports diverse experimental setups, including cancer immunotherapy drug screening and host–pathogen antiviral assays.

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
