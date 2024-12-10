#### Filter for only trios where both parents have linked medication data

# load libraries ----
library(dplyr)
library(readr)

# read in trios file with linked medication ----
trios <- read_csv("/finngen/red/EmmaMagavern/Maia/linked_medication.csv", col_types = cols(
  Offspring.WES = col_character(),
  Father.WES = col_character(),
  Mother.WES = col_character()
))


cleaned_data <- trios

# Process trios file  to remove trios with missing medication data ----

# Identify columns that end with "drug_1"
drug_1_columns <- grep("drug_1$", colnames(cleaned_data), value = TRUE)

# Filter out rows where any of the identified columns have NA values
cleaned_data <- cleaned_data %>%
  filter(if_all(all_of(drug_1_columns), ~ !is.na(.)))

# cleaned_data = 600 trios

