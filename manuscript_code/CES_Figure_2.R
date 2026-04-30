
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# R script for the generation of Main and Supplementary Figures 2
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
############################# Figure 2 #############################
####################################################################

##### Begin script #####

### Libraries (download if any missing)
library(dplyr)       
library(tidyr)      
library(tibble)      
library(ggplot2)     
library(pcaMethods)  
library(pheatmap)    
library(grid)        
library(ggrepel)     
library(reshape2)    
library(openxlsx)   
library(forcats)

### Load data
# Therapeutic Co-culture Efficacy Score (CES) results and comprehensive dose-response metrics across the ten hematological cancer cell line panel.
CES_results <- openxlsx::read.xlsx("data/Supplementary_Data_3.xlsx", sheet = 1)

##### Main figures
# Main 2b: PCA of drug CES profiles
drug_matrix <- CES_results %>%
  reshape2::dcast(Drug ~ cell, value.var = "CES") %>%
  column_to_rownames("Drug") %>%
  as.matrix()
drug_matrix[!is.finite(drug_matrix)] <- NA

pca_res <- ppca(drug_matrix)
score_df <- as.data.frame(scores(pca_res))
colnames(score_df)[1:2] <- c("PC1", "PC2")
score_df$median_CES <- apply(drug_matrix, 1, median, na.rm = TRUE)

R2_PC1 <- round(pca_res@R2cum[1], 3) * 100
R2_PC2 <- (round(pca_res@R2cum[2], 3) - round(pca_res@R2cum[1], 3)) * 100

ggplot(score_df, aes(PC1, PC2)) +
  geom_point(aes(fill = median_CES), shape = 21, size = 5, stroke = 0.8) +
  scale_fill_gradientn(
    colors = rev(c("#7F0000","#B2182B","#D6604D","#F4A582","#F7F7F7",
                   "#92C5DE","#4393C3","#2166AC","#08306B")),
    limits = c(-30, 30), oob = scales::squish
  ) +
  labs(x = paste0("PC1 (R² = ", R2_PC1, "%)"),
       y = paste0("PC2 (R² = ", R2_PC2, "%)"),
       fill = "Median CES") +
  theme_classic()



# Main 2c: Heatmap representing the top 50 enhancers and inhibitors (Peak and nAUC) using the median across the 10 blood cancers
TOP_N <- 50
cell_order <- c("MOLM14","THP1","HEL","CMK","OCIAML3",
                "K562","LAMA84","NALM6","SU-DHL4","MM1S")

# Annotation
anno_row <- data.frame(
  subtype = c("AML","AML","AML","AML","AML","CML","CML","B-ALL","BCL","MM"),
  row.names = cell_order
)
anno_colors <- list(subtype = c(AML="#6A51A3", CML="#E17C05", `B-ALL`="#C05A8D",
                                BCL="#1F9A8A", MM="#B8A200"))

# Drug selection ordered by median CES (lowest → highest)
drug_rank <- CES_results %>%
  group_by(Drug) %>%
  summarise(CES_median = median(CES, na.rm = TRUE), .groups = "drop")

top_enh <- drug_rank %>% arrange(desc(CES_median)) %>% slice_head(n = TOP_N) %>% pull(Drug)
top_inh <- drug_rank %>% arrange(CES_median) %>% slice_head(n = TOP_N) %>% pull(Drug)
sel_drugs <- unique(c(top_enh, top_inh))

drug_order <- drug_rank %>%
  filter(Drug %in% sel_drugs) %>%
  arrange(desc(CES_median)) %>%
  pull(Drug) %>% rev()

make_mat <- function(val_col) {
  mat <- CES_results %>%
    filter(cell %in% cell_order, Drug %in% drug_order) %>%
    mutate(cell = factor(cell, levels = cell_order),
           Drug = factor(Drug, levels = drug_order)) %>%
    dplyr::select(cell, Drug, !!sym(val_col)) %>%
    pivot_wider(names_from = Drug, values_from = !!sym(val_col)) %>%
    column_to_rownames("cell") %>%
    as.matrix()
  mat[cell_order, drug_order, drop = FALSE]
}

mat_Peak <- make_mat("Peak")
mat_AUC  <- make_mat("AUC")
mat_Peak[!is.finite(mat_Peak)] <- NA
mat_AUC[!is.finite(mat_AUC)]  <- NA

# Palettes
pal_Peak <- colorRampPalette(c("#3B0F70","#8C6BB1","#F7F7F7","#5DC0B0","#00441B"))(100)
breaks_Peak <- seq(-100, 100, length.out = 101)

q_auc <- quantile(mat_AUC, probs = c(0.02, 0.98), na.rm = TRUE)
if (!is.finite(q_auc[1]) || !is.finite(q_auc[2]) || q_auc[1] == q_auc[2]) {
  q_auc <- range(mat_AUC, na.rm = TRUE)
}
pal_AUC <- colorRampPalette(c("#1B2A41","#4F6D7A","#F7F7F7","#D9A441","#8C510A"))(100)
breaks_AUC <- seq(q_auc[1], q_auc[2], length.out = 101)

# Peak heatmap
pheatmap(mat_Peak, color = pal_Peak, breaks = breaks_Peak,
         cluster_rows = FALSE, cluster_cols = FALSE,
         labels_row = rep("", nrow(mat_Peak)),
         annotation_row = anno_row, annotation_colors = anno_colors,
         annotation_names_row = FALSE, annotation_legend = FALSE,
         border_color = "black", na_col = "grey85",
         fontsize = 8, angle_col = 90)

# nAUC heatmap
pheatmap(mat_AUC, color = pal_AUC, breaks = breaks_AUC,
         cluster_rows = FALSE, cluster_cols = FALSE,
         labels_row = rep("", nrow(mat_AUC)),
         annotation_row = anno_row, annotation_colors = anno_colors,
         annotation_names_row = FALSE, annotation_legend = FALSE,
         border_color = "black", na_col = "grey85",
         fontsize = 8, angle_col = 90)




# Main 2d: CES distribution per cell line (Median)
df_ces <- CES_results %>% filter(!is.na(CES), is.finite(CES))

cell_medians <- df_ces %>%
  group_by(cell) %>%
  summarise(median_CES = median(CES, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_CES)

df_ces <- df_ces %>% mutate(cell = factor(cell, levels = cell_medians$cell))

cell_colors <- c(CMK="#bb6565", K562="#487db8", HEL="#3b9ba7", THP1="#b58939",
                 LAMA84="#7b5d73", `SU-DHL4`="#cd6f39", MOLM14="#b0538b",
                 OCIAML3="#3a8b6c", NALM6="#785da3", MM1S="#c4403f")

ggplot(df_ces, aes(cell, CES, fill = cell)) +
  geom_violin(width = 1, trim = TRUE, scale = "width", alpha = 0.9) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_fill_manual(values = cell_colors, guide = "none") +
  coord_cartesian(ylim = c(-50, 50)) +
  labs(x = NULL, y = "CES") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Main 2e: Top 10 enhancers: Efficacy (Peak) and cumulative activity (nAUC)
top10 <- drug_rank %>% slice_max(CES_median, n = 10) %>% pull(Drug)

df_sum <- CES_results %>%
  filter(Drug %in% top10, cell %in% cell_order) %>%
  summarise(median_AUC = median(AUC, na.rm = TRUE),
            median_Peak = median(Peak, na.rm = TRUE),
            .by = Drug) %>%
  mutate(Drug = factor(Drug, levels = rev(top10)))

# Cumulative activity (AUC) – bars going left
ggplot(df_sum, aes(y = Drug, x = -abs(median_AUC))) +
  geom_col(width = 0.8, fill = "#C28E2B", color = "black") +
  scale_x_continuous(labels = function(x) abs(x)) +
  labs(x = "Median nAUC", y = NULL) +
  theme_classic()

# Efficacy (Peak) – bars going right
ggplot(df_sum, aes(y = Drug, x = abs(median_Peak))) +
  geom_col(width = 0.8, fill = "#2A9D8F", color = "black") +
  labs(x = "Median Peak", y = NULL) +
  theme_classic()



# Main 2f: Waterfall: median CES across all drugs
drug_summary <- CES_results %>%
  group_by(Drug) %>%
  summarise(
    median_CES = median(CES, na.rm = TRUE),
    min_CES = min(CES, na.rm = TRUE),
    max_CES = max(CES, na.rm = TRUE),
    q25_CES = quantile(CES, 0.25, na.rm = TRUE),
    q75_CES = quantile(CES, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(median_CES)) %>%
  arrange(median_CES) %>%
  mutate(rank = row_number())

ggplot(drug_summary, aes(rank, median_CES)) +
  geom_ribbon(aes(ymin = min_CES, ymax = max_CES), fill = "grey85", alpha = 0.7) +
  geom_ribbon(aes(ymin = q25_CES, ymax = q75_CES), fill = "grey55", alpha = 0.8) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#A50F15") +
  scale_y_continuous(limits = c(-50, 50), oob = scales::squish) +
  labs(x = "Compound index", y = "Median CES") +
  theme_classic()






##### Supplementary figures
# Supplementary 2b: Ranked top 20 enhancers and inhibitors 
drug_median <- CES_results %>%
  group_by(Drug) %>%
  summarise(Median_CES = median(CES, na.rm = TRUE), .groups = "drop")

TOP_N <- 20

topN <- drug_median %>%
  filter(Median_CES > 0) %>%
  slice_max(Median_CES, n = TOP_N) %>%
  mutate(Drug = fct_reorder(Drug, Median_CES))

bottomN <- drug_median %>%
  filter(Median_CES < 0) %>%
  slice_min(Median_CES, n = TOP_N) %>%
  mutate(Drug = fct_reorder(Drug, Median_CES, .desc = TRUE))

# Enhancers
ggplot(topN, aes(Drug, Median_CES, fill = Median_CES)) +
  geom_col(width = 0.8, color = "black") +
  coord_flip() +
  scale_fill_gradientn(colors = c("#fddbc7","#b2182b","#67001f"),
                       limits = c(0, 30), oob = scales::squish) +
  scale_y_continuous(limits = c(0, 7), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Median CES", fill = NULL) +
  theme_classic() 

# Inhibitors
ggplot(bottomN, aes(Drug, Median_CES, fill = Median_CES)) +
  geom_col(width = 0.8, color = "black") +
  coord_flip() +
  scale_fill_gradientn(colors = c("#053061","#2166ac","#f2f7fd"),
                       limits = c(-30, 0), oob = scales::squish) +
  scale_y_continuous(limits = c(-30, 0), expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Median CES", fill = NULL) +
  theme_classic()


# Supplementary 2c: CES vs log10 (Effective dose) – single cell example (K562)
EXAMPLE_CELL <- "K562"

df_cell <- CES_results %>%
  filter(cell == EXAMPLE_CELL, is.finite(CES)) %>%
  mutate(log10_ED = log10(as.numeric(Effective_dose)))

top3_pos <- df_cell %>% slice_max(CES, n = 3)
top3_neg <- df_cell %>% slice_min(CES, n = 3)

y_lim <- 50
ces_cols <- c(
  rev(colorRampPalette(c("#f2f7fd","#2166ac","#053061"))(100)),
  colorRampPalette(c("#fddbc7","#b2182b","#67001f"))(100)
)
ces_vals <- scales::rescale(seq(-y_lim, y_lim, length.out = length(ces_cols)))

ggplot(df_cell, aes(log10_ED, CES)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(aes(color = CES), size = 3) +
  geom_text_repel(data = bind_rows(top3_pos, top3_neg),
                  aes(label = Drug), size = 3, max.overlaps = Inf) +
  scale_color_gradientn(colours = ces_cols, values = ces_vals,
                        limits = c(-y_lim, y_lim), oob = scales::squish) +
  scale_y_continuous(limits = c(-y_lim, y_lim)) +
  labs(x = "log10(Effective dose)", y = "CES",
       title = EXAMPLE_CELL, color = "CES") +
  theme_classic()


##### End script #####

