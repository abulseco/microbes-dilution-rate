# Supplemental figure & analysis
# Cell count data

# SET UP ENVIRONMENT----
## Load necessary libraries----
library(dplyr); library(emmeans); library(multcompView); library(ggplot2); library(scales)

## load data----
cell_counts <- read.csv("input_files/bacterial_cell_counts_2021Jan.csv", header = TRUE)

# Filter out the "feed" data
cell_counts_nofeed <- cell_counts %>%
  filter(Chemostat !="FEED")

# Examine the data
cell_summary <- cell_counts_nofeed %>%
  group_by(chemostat_ID, dilution_rate) %>%
  summarise(
    mean_cells = mean(Abundance, na.rm = TRUE),
    sd_cells   = sd(Abundance, na.rm = TRUE),
    n          = sum(!is.na(Abundance)),
    .groups = "drop"
  ) 
cell_summary

# Plots----
## Boxplot----
cell_counts_boxplot <- ggplot(cell_counts_nofeed, aes(x = dilution_rate, y = Abundance), ) +
  geom_boxplot(aes(fill = dilution_rate), color = "black", alpha = 0.9, width = 0.25) +
  # geom_jitter(aes(color = DILUTION_RATE, shape = CHEMOSTAT_ID), width = 0.2, size = 3, alpha = 0.9) +
  scale_color_manual(values=c("#2B5B6C", "#E34F33", "#FFC87E")) +
  scale_fill_manual(values=c("#2B5B6C", "#E34F33", "#FFC87E")) +
  xlab("Dilution Rate") +
  ylab(expression("Bacteria (" * 10^6 * " cells mL"^-1 * ")")) +
  pretty.theme() +
  scale_y_log10(labels = label_scientific()) +
  theme(legend.position = "none")
cell_counts_boxplot
