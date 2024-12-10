### Summary statistics for:
### - number of prescribed medications
### - medication overlap between offspring and parents

# load libraries ----
library(dplyr)

# Compute summary statistics ----
df3 <- read.csv('/home/ivm/Documents/Emma_Maia_Trios_G/trios_with_percentage_no_dups_sept16.csv')
cleaned_data <- df3 %>%
  rename(drug_match_percentage = percentage_overlap)

# Calculate summary statistics for drug_match_percentage 
summary_stats <- summary(cleaned_data$drug_match_percentage)
mean_value <- mean(cleaned_data$drug_match_percentage, na.rm = TRUE)
median_value <- median(cleaned_data$drug_match_percentage, na.rm = TRUE)
sd_value <- sd(cleaned_data$drug_match_percentage, na.rm = TRUE)
num_observations <- length(cleaned_data$drug_match_percentage)

# print summary statistics
summary_stats
mean_value
median_value
sd_value
num_observations

# Number of medications prescribed to offspring
summary_stats <- summary(cleaned_data$unique_offspring_drugs)
mean_value <- mean(cleaned_data$unique_offspring_drugs, na.rm = TRUE)
median_value <- median(cleaned_data$unique_offspring_drugs, na.rm = TRUE)
sd_value <- sd(cleaned_data$unique_offspring_drugs, na.rm = TRUE)
num_observations <- length(cleaned_data$unique_offspring_drugs)

# print summary statistics
summary_stats
mean_value
median_value
sd_value
num_observations


