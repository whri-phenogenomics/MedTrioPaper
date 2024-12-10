### Generate scatter plot for age vs medication overlap and summary stats for year of prescription

# Load libraries ----
library(ggplot2)

# Plot age of offspring and medication overlap ----
plot <- ggplot(cleaned_data, aes(x = O.age, y = drug_match_percentage)) +
  geom_point() +
  labs(
    title = "Age of Offspring vs. % of Shared Medications with Parents",
    x = "Age of Offspring",
    y = "Percentage of Shared Medications"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center and make title bold
    axis.title.x = element_text(size = 18, face = "bold"),  # Increase x-axis title size and make bold
    axis.title.y = element_text(size = 18, face = "bold"),  # Increase y-axis title size and make bold
    axis.text = element_text(size = 14, face = "bold"),  # Increase axis labels size and make bold
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(size = 0.8)  # Keep and slightly emphasize axis lines
  )

plot
saveRDS(plot, './Documents/Emma_Maia_Trios_G/age_vs_medication_overlap_graph_december.rds')
scatter_plot <- readRDS('/home/ivm/Documents/Emma_Maia_Trios_G/age_vs_medication_overlap_graph.rds')

# summary statistics on year of prescriptions ----
meds_with_year <- read.csv('/home/ivm/Documents/Emma_Maia_Trios_G/meds_with_year.csv')
# Ensure the clinical_year column is numeric
meds_with_year$clinical_year <- as.numeric(meds_with_year$clinical_year)

# Calculate summary statistics
mean_year <- mean(meds_with_year$clinical_year, na.rm = TRUE)
median_year <- median(meds_with_year$clinical_year, na.rm = TRUE)
sd_year <- sd(meds_with_year$clinical_year, na.rm = TRUE)
min_year <- min(meds_with_year$clinical_year, na.rm = TRUE)
max_year <- max(meds_with_year$clinical_year, na.rm = TRUE)
range_year <- max_year - min_year

# Create a summary data frame
summary_stats <- data.frame(
  Mean = mean_year,
  Median = median_year,
  SD = sd_year,
  Min = min_year,
  Max = max_year,
  Range = range_year
)

# Print summary statistics
print(summary_stats)

# run 13/08/2024
# > print(summary_stats)
# Mean Median       SD  Min  Max Range
# 1 2015.649   2017 5.530102 1989 2024    35