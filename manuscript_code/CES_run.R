
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# Standalone workflow example demonstrating the CES framework
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

# Libraries
library(dplyr)
library(drc)
library(data.table)
library(caTools)
library(ggplot2)
library(minpack.lm)
library(DEoptim)

# Load CES functions
source("../R/CES_functions.R")

# Load data
screen_data <- openxlsx::read.xlsx("data/example_data_K562.xlsx")

mono_data <- screen_data %>% filter(SCREEN_NAME == "K562_R10_24h_20200709")
coculture_data <- screen_data %>% filter(SCREEN_NAME == "K562_NK_24h_20200709")
control_data <- screen_data %>% filter(SCREEN_NAME == "Control")

drugs <- unique(coculture_data$DRUG_NAME)


# Single drug example (3-condition, therapeutic) 
DRUG_NAME <- "NVP-LCL161"

out <- run_CES(
  df_cc = coculture_data[coculture_data$DRUG_NAME == DRUG_NAME, ],
  df_mono = mono_data[mono_data$DRUG_NAME == DRUG_NAME, ],
  df_ctrl = control_data[control_data$DRUG_NAME == DRUG_NAME, ],
  scoring_model = "therapeutic" # or mechanistic
)

print(out$results)
print(out$plot_curves)
print(out$plot_score)


# Single drug example (2-condition) ---
out_2cond <- run_CES(
  df_cc = coculture_data[coculture_data$DRUG_NAME == DRUG_NAME, ],
  df_mono = mono_data[mono_data$DRUG_NAME == DRUG_NAME, ]
)

print(out_2cond$results)


# Full library (3-condition, therapeutic) 

### THIS MAY TAKE ~3-5minutes! Please wait.

results_all <- do.call(rbind, lapply(drugs, function(d) {
  run_CES(
    df_cc = coculture_data[coculture_data$DRUG_NAME == d, ],
    df_mono = mono_data[mono_data$DRUG_NAME == d, ],
    df_ctrl = control_data[control_data$DRUG_NAME == d, ],
    scoring_model = "therapeutic",
    plot = FALSE
  )$results
}))

print(head(results_all))

