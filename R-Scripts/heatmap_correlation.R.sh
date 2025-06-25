# Install required packages if missing
required_pkgs <- c("tidyverse", "readr", "pheatmap")
for (pkg in required_pkgs) {
if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)
library(readr)
library(pheatmap)
# Load your data
data <- read_tsv("relativeabundance_HCTS_R_filtered.txt", show_col_types = FALSE)
# Preserve sample order
sample_levels <- unique(data$sample)
# Pivot to wide format (bins as rows, samples as columns)
abundance_matrix <- data %>%
select(bin_name, sample, relative_abundance_basedetection) %>%
mutate(
sample = factor(sample, levels = sample_levels),
relative_abundance_basedetection = as.numeric(relative_abundance_basedetection)
) %>%
pivot_wider(
names_from = sample,
values_from = relative_abundance_basedetection,
values_fill = 0
)
# Convert to matrix
abundance_mat <- as.data.frame(abundance_matrix)
rownames(abundance_mat) <- abundance_mat$bin_name
abundance_mat <- abundance_mat[, -1]
# Remove bins with all zeros or constant values
abundance_mat <- abundance_mat[apply(abundance_mat, 1, function(x) length(unique(x)) > 1), ]
# Compute Pearson correlation matrix
cor_mat <- cor(t(abundance_mat), method = "pearson")
# Compute distance and clustering
dist_mat <- as.dist(1 - cor_mat)
clust_rows <- hclust(dist_mat)
clust_cols <- hclust(dist_mat)
# Plot heatmap with clustering and correlation values
pheatmap(
mat = cor_mat,
cluster_rows = clust_rows,
cluster_cols = clust_cols,
display_numbers = TRUE,
number_format = "%.2f",
color = colorRampPalette(c("red", "white", "cornflowerblue"))(100),
fontsize_row = 9,
fontsize_col = 9,
fontsize_number = 12,
main = "Correlation Heatmap of Bin Relative Abundance Trends Over Time - HCTS"
)