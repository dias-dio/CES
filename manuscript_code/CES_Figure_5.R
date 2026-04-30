
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# R script for the generation of Main and Supplementary Figures 5
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
############################# Figure 5 #############################
####################################################################

##### Begin script #####

### Libraries (download if any missing)
library(dplyr)       
library(tidyr)      
library(ggplot2)     
library(scales)      
library(patchwork)  
library(openxlsx)    

### Load data
# Therapeutic CES results, condition-specific dose-response parameters, and phenotypic classifications from the antiviral host-pathogen screening dataset
CES_antiviral <- openxlsx::read.xlsx("data/Supplementary_Data_8.xlsx", sheet = 1)
# Therapeutic and mechanistic CES profiles for the K562 hematological cancer cell line
CES_K562_therapeutic <- openxlsx::read.xlsx("data/Supplementary_Data_9.xlsx", sheet = 1)
CES_K562_mechanistic <- openxlsx::read.xlsx("data/Supplementary_Data_9.xlsx", sheet = 2)


##### Main figures
# Build K562 drug-level table
df_K562 <- CES_K562_therapeutic %>%
  transmute(
    Drug = as.character(Drug),
    CES = as.numeric(CES),
    dDSS = as.numeric(dDSS),
    IC50_CC = as.numeric(IC50_coculture),
    IC50_NK = as.numeric(IC50_NK),
    IC50_target = as.numeric(IC50_target),
    Toxic = as.character(Toxic),
    neg_logIC50 = ifelse(is.finite(IC50_CC) & IC50_CC > 0, -log10(IC50_CC * 1e-9), NA_real_)
  ) %>%
  left_join(
    CES_K562_mechanistic %>%
      transmute(Drug = as.character(Drug), Mech_CES = as.numeric(CES)),
    by = "Drug"
  )

# Complete data subset for scatter and cumulative plots
df_K562_all <- df_K562 %>%
  filter(is.finite(CES), is.finite(Mech_CES), is.finite(dDSS), !is.na(Toxic))


# Barplot helper
plot_top15 <- function(df, col, direction = "bottom", fill = "#08519C", break_by = 10) {
  dfp <- df %>% transmute(Drug, val = as.numeric(.data[[col]])) %>% filter(is.finite(val))
  dfp <- if (direction == "bottom") {
    dfp %>% arrange(val) %>% slice_head(n = 15)
  } else {
    dfp %>% arrange(desc(val)) %>% slice_head(n = 15) %>% arrange(val)
  }
  dfp <- dfp %>% mutate(Drug = factor(Drug, levels = Drug))
  
  ggplot(dfp, aes(Drug, val)) +
    geom_col(width = 0.7, fill = fill, color = "black") +
    scale_y_continuous(breaks = scales::breaks_width(break_by)) + 
    coord_flip() + labs(x = NULL, y = col) +
    theme_classic() +
    theme(axis.text = element_text(size = 9, color = "black"))
}

# Main 5b: Top 15 inhibitors (blue)
p_bot15_CES <- plot_top15(df_K562, "CES", "bottom", "#08519C", 10)
p_bot15_Mech <- plot_top15(df_K562, "Mech_CES", "bottom", "#08519C", 10)
p_bot15_dDSS <- plot_top15(df_K562, "dDSS", "bottom", "#08519C", 10)
p_bot15_pIC50 <- plot_top15(df_K562, "neg_logIC50", "bottom", "#08519C", 1)

# Supplementary 5a: Top 15 enhancers (red)
p_top15_CES <- plot_top15(df_K562, "CES", "top", "#b2182b", 5)
p_top15_Mech <- plot_top15(df_K562, "Mech_CES", "top", "#b2182b", 5)
p_top15_dDSS <- plot_top15(df_K562, "dDSS", "top", "#b2182b", 5)
p_top15_pIC50 <- plot_top15(df_K562, "neg_logIC50", "top", "#b2182b", 2)



# Show inhibitors 2x2
(p_bot15_CES | p_bot15_Mech) / (p_bot15_dDSS | p_bot15_pIC50)

# Show enhancers 2x2
(p_top15_CES | p_top15_Mech) / (p_top15_dDSS | p_top15_pIC50)



# Main 5c: Mechanistic CES vs dDSS scatter
df_scatter <- df_K562_all %>%
  mutate(category = case_when(
    Toxic == "No" & Mech_CES < 0 & dDSS >= 0 ~ "Rescued by mechanistic CES",
    Toxic == "No" & Mech_CES < 0 & dDSS < 0  ~ "Concordant non-toxic antagonist",
    Toxic != "No" ~ "Cytotoxic compound",
    TRUE ~ "Other"
  ))

ggplot(df_scatter, aes(dDSS, Mech_CES)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(data = filter(df_scatter, category %in% c("Cytotoxic compound","Other")),
             fill = "grey80", shape = 21, size = 4, alpha = 0.6) +
  geom_point(data = filter(df_scatter, category == "Concordant non-toxic antagonist"),
             fill = "#556B2F", shape = 21, size = 4) +
  geom_point(data = filter(df_scatter, category == "Rescued by mechanistic CES"),
             fill = "#D4A017", shape = 21, size = 4) +
  coord_fixed(ratio = 1, xlim = c(-50, 10), ylim = c(-20, 20)) +
  labs(x = "dDSS", y = "Mechanistic CES") +
  theme_classic() 


# Main 5d: Top 50 antagonists (stacked bar)
get_top50 <- function(df, col, label) {
  df %>% filter(is.finite(.data[[col]])) %>%
    arrange(.data[[col]]) %>% slice_head(n = 50) %>%
    mutate(Metric = label)
}

df_top50 <- bind_rows(
  get_top50(df_K562_all, "Mech_CES", "Mechanistic CES"),
  get_top50(df_K562_all, "CES", "Therapeutic CES"),
  get_top50(df_K562_all, "dDSS", "dDSS")
) %>%
  mutate(Tox = ifelse(Toxic == "No", "Non-toxic antagonist", "Cytotoxic compound"))

df_top50_sum <- df_top50 %>%
  count(Metric, Tox) %>%
  mutate(Metric = factor(Metric, levels = c("dDSS","Therapeutic CES","Mechanistic CES")))

ggplot(df_top50_sum, aes(Metric, n, fill = Tox)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("Cytotoxic compound"="#b2182b", "Non-toxic antagonist"="#0F8A8A")) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Count", fill = NULL) +
  theme_classic() + theme(legend.position = "top")



# Main 5f: CES vs SI (SI = Selectivity Index metric)
df_sets <- CES_antiviral %>%
  mutate(SI_valid  = is.finite(as.numeric(SI)) & !is.na(SI),
         CES_valid = is.finite(as.numeric(CES)) & !is.na(CES))

n_total <- nrow(df_sets)
n_SI_valid  <- sum(df_sets$SI_valid)
n_SI_missed <- sum(!df_sets$SI_valid)
n_CES_valid <- sum(df_sets$CES_valid)

df_bar <- tibble(
  metric = c("CES", "SI", "SI"),
  status = c("Scorable", "Scorable", "Missed"),
  n = c(n_CES_valid, n_SI_valid, n_SI_missed)
) %>%
  group_by(metric) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(fill_key = paste(metric, status, sep = "_"))

fill_cols <- c(CES_Scorable="#C8A200", CES_Missed="#F2E6A8",
               SI_Scorable="#1F1F1F",  SI_Missed="#9B9B9B")

ggplot(df_bar, aes(metric, pct, fill = fill_key)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = fill_cols, guide = "none") +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Proportion") +
  theme_classic()



# Main 5h: Activator vs Inactive (co-culture phenotype)
df_coculture <- CES_antiviral %>%
  filter(!is.na(Phenotype_coculture)) %>%
  mutate(Phenotype_coculture = factor(Phenotype_coculture, levels = c("Activator", "Inactive")))

ggplot(df_coculture, aes(Phenotype_coculture, CES, fill = Phenotype_coculture)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.5, shape = 21, color = "black") +
  scale_fill_manual(values = c(Activator="#1B7F79", Inactive="#6E6E6E")) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = NULL, y = "CES") +
  theme_classic() 





##### Supplementary figures
# Supplementary 5e: Quartile boxplot with Q1 vs Q4 CES on context TI (TI = Therapeutic Index metric)
df_K562 <- df_K562 %>%
  mutate(logTI_context = ifelse(is.finite(IC50_NK) & is.finite(IC50_CC) & IC50_NK > 0 & IC50_CC > 0, log10(IC50_NK) - log10(IC50_CC), NA_real_))

df_q <- df_K562 %>%
  filter(is.finite(CES), is.finite(logTI_context)) %>%
  mutate(q = ntile(CES, 4),
         group = case_when(q == 1 ~ "Bottom CES (Q1)", q == 4 ~ "Top CES (Q4)")) %>%
  filter(!is.na(group)) %>%
  mutate(group = factor(group, levels = c("Bottom CES (Q1)", "Top CES (Q4)")))


ggplot(df_q, aes(group, logTI_context)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(width = 0.5, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.25, color = "grey35") +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(x = NULL, y = "log10(TI context)") +
  theme_classic()


# Supplementary 5f: Cytotoxic vs Inactive (mono/control phenotype)
df_control <- CES_antiviral %>%
  filter(!is.na(Phenotype_mono)) %>%
  mutate(Phenotype_mono = factor(Phenotype_mono,
                                 levels = c("Cytotoxic", "Inactive")))

ggplot(df_control, aes(Phenotype_mono, CES, fill = Phenotype_mono)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.5, shape = 21, color = "black") +
  scale_fill_manual(values = c(Cytotoxic="#A63A3A", Inactive="#6E6E6E")) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = NULL, y = "CES") +
  theme_classic() 



# Supplementary 5g-h: Base calculation needed for both plots
build_cum <- function(df, col, label) {
  df %>% filter(is.finite(.data[[col]]), !is.na(Toxic)) %>%
    arrange(.data[[col]]) %>%
    mutate(rank = row_number(),
           is_toxic = as.integer(Toxic != "No"),
           cum_toxic = cumsum(is_toxic),
           cum_nontoxic = rank - cum_toxic,
           cum_frac = cum_toxic / rank,
           Metric = label)
}

df_cum <- bind_rows(
  build_cum(df_K562_all, "dDSS", "dDSS"),
  build_cum(df_K562_all, "CES", "Therapeutic CES"),
  build_cum(df_K562_all, "Mech_CES", "Mechanistic CES")
) %>%
  mutate(Metric = factor(Metric, levels = c("dDSS","Therapeutic CES","Mechanistic CES")))


# Supplementary 5g: Stacked bars of the Top-N antagonistic cutoff per metric
cutoffs <- c(10, 25, 50, 75, 100)

df_stacked <- df_cum %>%
  filter(rank %in% cutoffs) %>%
  dplyr::select(Metric, rank, cum_toxic, cum_nontoxic) %>%
  pivot_longer(c(cum_toxic, cum_nontoxic), names_to = "Tox", values_to = "n") %>%
  mutate(
    Tox = recode(Tox, cum_toxic = "Cytotoxic compound",
                 cum_nontoxic = "Non-toxic antagonist"),
    cutoff = factor(paste0("Top-", rank), levels = paste0("Top-", cutoffs))
  ) %>%
  group_by(Metric, cutoff) %>%
  mutate(frac = n / sum(n)) %>%
  ungroup()

ggplot(df_stacked, aes(cutoff, frac, fill = Tox)) +
  geom_col(width = 0.7, color = "black") +
  facet_wrap(~Metric, nrow = 1) +
  scale_fill_manual(values = c("Cytotoxic compound"="#b2182b",
                               "Non-toxic antagonist"="#0F8A8A")) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  labs(x = NULL, y = "Proportion", fill = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top")



# Supplementary 5h: Top-N antagonists Cumulative curves per metric
ggplot(df_cum %>% filter(rank <= 100), aes(rank, cum_frac, color = Metric)) +
  geom_step(linewidth = 1.5) +
  scale_color_manual(values = c(dDSS="#54278F", `Therapeutic CES`="#807DBA",
                                `Mechanistic CES`="#D94801")) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "Top-N antagonists", y = "Cumulative fraction cytotoxic", color = NULL) +
  theme_classic() + theme(legend.position = "top")



##### End script #####




