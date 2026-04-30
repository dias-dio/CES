
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# R script for the generation of Main Figures 3
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
############################# Figure 3 #############################
####################################################################

##### Begin script #####

### Libraries (download if any missing)
library(dplyr)      
library(tidyr)      
library(ggplot2)    
library(reshape2)   
library(Hmisc)       
library(openxlsx)  

### Load data
# Therapeutic CES results from the independent validation experiment profiling a 36-compound panel at high dose resolution across the hematological cancer cell lines
CES_concordance <- openxlsx::read.xlsx("data/Supplementary_Data_5.xlsx", sheet = 1)
# Therapeutic CES results across the five effector-to-target (E:T) ratio conditions in the OCI-AML3 model, assessing metric robustness across varying effector loads
CES_ratio <- openxlsx::read.xlsx("data/Supplementary_Data_6.xlsx", sheet = 1)


##### Main figures
# Main 3b: Primary/pilot vs Validation concordance (Pearson R per cell line)
df_cor <- CES_concordance %>%
  filter(is.finite(Pilot_CES), is.finite(Val_CES)) %>%
  group_by(cell) %>%
  summarise(Pearson_R = cor(Pilot_CES, Val_CES, use = "complete.obs"), .groups = "drop") %>%
  arrange(Pearson_R) %>%
  mutate(cell = factor(cell, levels = cell))

ggplot(df_cor, aes(x = cell, y = Pearson_R)) +
  geom_segment(aes(xend = cell, y = 0, yend = Pearson_R), color = "#00A896", linewidth = 1.5) +
  geom_point(fill = "#028090", shape = 21, size = 7, color = "black", stroke = 1) +
  geom_hline(yintercept = 0.5, color = "grey50", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Pearson R") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Main 3c: CES dot plot across E:T ratios
et_levels <- c("2:1","1:1","1:2","1:4","1:8")

plot_df <- CES_ratio %>%
  mutate(condition = factor(condition, levels = et_levels))

drug_order <- plot_df %>%
  group_by(Drug) %>%
  summarise(mean_CES = mean(CES, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_CES) %>% pull(Drug)

plot_df <- plot_df %>% mutate(Drug = factor(Drug, levels = drug_order))

lim_abs <- max(abs(plot_df$CES), na.rm = TRUE)

ggplot(plot_df, aes(x = Drug, y = condition)) +
  geom_point(aes(fill = CES), shape = 21, color = "black", stroke = 0.8, size = 6) +
  scale_y_discrete(limits = rev(et_levels)) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-lim_abs, lim_abs)) +
  labs(x = NULL, y = NULL, fill = "CES") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_line(color = "grey70", linetype = "dashed"))


# Main 3d: Effective dose – top 3 enhancers
top3 <- c("Pevonedistat", "Birinapant", "NVP-LCL161")
drug_colors <- c(Pevonedistat="#B23A48", Birinapant="#0B6E6E", `NVP-LCL161`="#3A4F7A")

dose_df <- CES_ratio %>%
  filter(Drug %in% top3, is.finite(Effective_dose), Effective_dose > 0) %>%
  mutate(condition = factor(condition, levels = c("1:8","1:4","1:2","1:1","2:1")))

ggplot(dose_df, aes(condition, log10(Effective_dose), group = Drug, color = Drug)) +
  geom_line(linewidth = 1.5) + geom_point(size = 5) +
  scale_color_manual(values = drug_colors) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(x = NULL, y = "log10(Effective dose)", color = NULL) +
  theme_classic() + theme(legend.position = "top")


# Main 3e: Effective dose – top 3 inhibitors
top3_inhib <- c("Dexamethasone", "Sotrastaurin", "Dasatinib")
drug_colors_inhib <- c(Dexamethasone="#C49A00", Sotrastaurin="#B5562A", Dasatinib="#6A8F3A")

dose_df_inhib <- CES_ratio %>%
  filter(Drug %in% top3_inhib, is.finite(Effective_dose), Effective_dose > 0) %>%
  mutate(condition = factor(condition, levels = c("1:8","1:4","1:2","1:1","2:1")))

ggplot(dose_df_inhib, aes(condition, log10(Effective_dose), group = Drug, color = Drug)) +
  geom_line(linewidth = 1.5) + geom_point(size = 5) +
  scale_color_manual(values = drug_colors_inhib) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(x = NULL, y = "log10(Effective dose)", color = NULL) +
  theme_classic() + theme(legend.position = "top")


# Main 3f: Spearman correlation across E:T ratios
df_ces_et <- CES_ratio %>%
  mutate(condition = factor(condition, levels = et_levels),
         CES = as.numeric(CES)) %>%
  filter(is.finite(CES)) %>%
  group_by(Drug, condition) %>%
  summarise(CES = mean(CES, na.rm = TRUE), .groups = "drop")

mat_et <- df_ces_et %>%
  pivot_wider(names_from = condition, values_from = CES) %>%
  dplyr::select(-Drug) %>% as.matrix()

C <- rcorr(mat_et, type = "spearman")$r

cor_df <- as.data.frame(as.table(C)) %>%
  rename(ET1 = Var1, ET2 = Var2, rho = Freq) %>%
  mutate(ET1 = factor(ET1, levels = et_levels),
         ET2 = factor(ET2, levels = et_levels)) %>%
  filter(as.integer(ET1) <= as.integer(ET2))

ggplot(cor_df, aes(ET1, ET2, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 5,
            color = ifelse(cor_df$rho > 0.6, "white", "black")) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", limits = c(0, 1)) +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = expression(Spearman~rho)) +
  theme_minimal() + theme(panel.grid = element_blank())



##### End script #####
