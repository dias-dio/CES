# CES: Co-culture Efficacy Score Framework

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![ShinyApp](https://img.shields.io/badge/App-Available_Now-FF9800?logo=rstudio)](https://<link-to-your-shiny-app>)
[![DOI](https://img.shields.io/badge/DOI-Coming_Soon-lightgrey.svg)](#citation)

**CES** is a quantitative computational framework and interactive web application designed to measure context-dependent compound activity in complex multicellular systems. 

By integrating potency and efficacy into a single interpretable score, CES mathematically separates general compound-induced toxicity from true interaction-dependent modulation, enabling rapid compound prioritization across diverse settings like cancer immunotherapy and host-pathogen antiviral screens.

---

## 📑 Table of Contents
- [Repository Structure](#-repository-structure)
- [The CES Web Application](#-the-ces-web-application)
  - [Local Installation](#local-installation)
- [Reproducing the Manuscript](#-reproducing-the-manuscript)
- [Methodology Overview](#-methodology-overview)
- [Citation](#-citation)
- [Contact & Affiliations](#-contact--affiliations)

---

## 📂 Repository Structure

This repository provides both the source code for the interactive Shiny application and the exact scripts required to reproduce the analyses and figures from our manuscript.

```text
CES/
├── app/                       # Complete source code for the Shiny Web Application
│   ├── ui.R
│   ├── server.R
│   ├── www/                   # Assets (logos, custom CSS, images)
│   └── example_data/          # Example datasets for tutorials
├── manuscript_code/           # Scripts to reproduce manuscript figures
│   ├── data/                  # Raw/processed data required for the figures
│   ├── scripts/               # R scripts for generating Figs 2-5
│   └── example_workflow.R     # Standalone script demonstrating the core methodology
├── R/                         # Core mathematical and modeling functions
├── README.md
└── LICENSE
