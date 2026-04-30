
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# R script for the generation of Main Figures 4
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
############################# Figure 4 #############################
####################################################################

##### Begin script #####

### Libraries (download if any missing)
library(dplyr)       
library(ggplot2)    
library(openxlsx)    

### Load data
# Therapeutic CES results from the CAR T-cell co-culture screen against the CD19-positive NALM-6 target cell line
CES_CART <- openxlsx::read.xlsx("data/Supplementary_Data_7.xlsx", sheet = 1)
# Therapeutic CES results, condition-specific dose-response parameters, and phenotypic classifications from the antiviral host-pathogen screening dataset
CES_antiviral <- openxlsx::read.xlsx("data/Supplementary_Data_8.xlsx", sheet = 1)


##### Main figures
colorPalette <- colorRampPalette(c(
  "#053061","#2166ac","#4393c3","#92c5de","#d1e5f0","#f7f7f7",
  "#fddbc7","#f4a582","#d6604d","#b2182b","#67001f"
))(100)


# Main 4b: CAR-T CES waterfall
cart_df <- CES_CART %>%
  transmute(Drug = as.character(Drug), CES = as.numeric(CES)) %>%
  filter(is.finite(CES)) %>%
  arrange(CES) %>%
  mutate(rank = row_number())

ggplot(cart_df, aes(rank, CES, color = CES)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 4) +
  scale_color_gradientn(colours = colorPalette, limits = c(-50, 50)) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = "Compound index", y = "CES") +
  theme_classic()


# CAR-T: Top 15 enhancers and  inhibitors
df_cart <- cart_df %>% distinct(Drug, .keep_all = TRUE)

top15_enh <- df_cart %>% slice_max(CES, n = 15) %>%
  mutate(Drug = factor(Drug, levels = rev(Drug)))

top15_inh <- df_cart %>% slice_min(CES, n = 15) %>%
  mutate(Drug = factor(Drug, levels = rev(Drug)))

ggplot(top15_enh, aes(Drug, CES, fill = CES)) +
  geom_col(width = 0.8, color = "black") +
  scale_fill_gradientn(colours = colorPalette, limits = c(-50, 50)) +
  coord_flip() +
  labs(x = NULL, y = "CES") +
  theme_classic()

ggplot(top15_inh, aes(Drug, CES, fill = CES)) +
  geom_col(width = 0.8, color = "black") +
  scale_fill_gradientn(colours = colorPalette, limits = c(-50, 50)) +
  coord_flip() +
  labs(x = NULL, y = "CES") +
  theme_classic() 


# Main 4c: Antiviral CES scatter plot
av_df <- CES_antiviral %>%
  mutate(CES = as.numeric(CES)) %>%
  filter(is.finite(CES)) %>%
  sample_frac(1) %>%
  mutate(drug_index = row_number(),
         CES_sign = ifelse(CES >= 0, "Rescued", "Dead"))

ggplot(av_df, aes(drug_index, CES, color = CES_sign)) +
  geom_hline(yintercept = 0, linewidth = 1, color = "black") +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_manual(values = c(Rescued = "#1B9E77", Dead = "#B2182B")) +
  coord_cartesian(ylim = c(-45, 45)) +
  labs(x = "Compound index", y = "CES") +
  theme_classic() 


# Main 4d: Antiviral - Rescued vs Dead counts
ces_counts <- CES_antiviral %>%
  mutate(CES = as.numeric(CES),
         CES_group = case_when(CES > 0 ~ "Rescued", CES < 0 ~ "Dead", TRUE ~ "Neutral")) %>%
  count(CES_group) %>%
  arrange(desc(n)) %>%
  mutate(CES_group = factor(CES_group, levels = CES_group))

ggplot(ces_counts, aes(CES_group, n, fill = CES_group)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c(Rescued="#1B9E77", Dead="#B2182B", Neutral="grey70")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Count") +
  theme_classic() 


# Main 4e: Antiviral - CES vs Effective dose
colorPalette_RG <- colorRampPalette(c(
  "#4d0013","#800026","#b2182b","#d7301f","#f16913",
  "#f7f7f7",
  "#31a354","#1b7837","#006837","#00441b"
))(100)

df_dose <- CES_antiviral %>%
  transmute(CES = as.numeric(CES), Effective_dose = as.numeric(Effective_dose)) %>%
  filter(is.finite(CES), is.finite(Effective_dose), Effective_dose > 0)

ggplot(df_dose, aes(Effective_dose, CES, color = CES)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 4, alpha = 0.9) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 50000)) +
  scale_y_continuous(limits = c(-45, 45), breaks = seq(-45, 45, 15)) +
  scale_color_gradientn(colours = colorPalette_RG, limits = c(-45, 45), oob = scales::squish) +
  labs(x = "Effective dose", y = "CES") +
  theme_classic() 



##### End script #####


