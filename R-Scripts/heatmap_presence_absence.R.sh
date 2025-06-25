# Load necessary packages
required_packages <- c("tidyverse", "scales")
installed <- rownames(installed.packages())
for (pkg in required_packages) {
if (!pkg %in% installed) install.packages(pkg)
}
library(tidyverse)
library(scales)
# Define custom color palette
custom_colors <- c(
"darkorange", "red", "darkgreen", "lightblue",
"cornflowerblue", "lightgreen", "black", "brown", "blueviolet" , "darkgrey", "darkturquoise"
)
# Read the data
data <- read.delim("relativeabundace_MLTS_2014.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# Prepare data
data_clean <- data %>%
select(bin_name, sample, relative_abundance_basedetection) %>%
mutate(
relative_abundance_basedetection = as.numeric(relative_abundance_basedetection),
sample = factor(sample, levels = unique(sample))
) %>%
filter(relative_abundance_basedetection > 0)
# Plot
ggplot(data_clean, aes(x = sample, y = relative_abundance_basedetection, color = bin_name, group = bin_name)) +
geom_line(linewidth = 1) +
geom_point(size = 2) +
scale_y_log10(
labels = scientific_format(),
breaks = trans_breaks("log10", function(x) 10^x)
) +
scale_color_manual(values = custom_colors) +
labs(
title = "Relative Abundance of Each Bin of genus Poseidonia Across Samples from MLTS",
x = "Sample",
y = "Relative Abundance ",
color = "Bin Name"
) +
theme_minimal(base_size = 11) +
theme(
axis.text.x = element_text(angle = 45, hjust = 1),
legend.position = "right"
)