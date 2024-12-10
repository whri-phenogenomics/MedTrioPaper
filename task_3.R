### Calculate percentage overlap of medications between offspring and parents

# load libraries ----
library(readr)
library(stringr)
library(dplyr)

# Convert drug synonyms to representative names (captialised) ----
dups <- read.csv('/genesandhealth/red/Emma_Maia_Trios/duplicate_medication_1Aug.csv')
drug_columns <- grep("_drug_", names(cleaned_data), value = TRUE) # get drug cols
# for each row in drug cols, if matches in dups then replace with representative name
df2 <- dups
df2 <- data.frame(lapply(dups, as.character), stringsAsFactors = FALSE) # check if line needed
df2 <- df2[, colSums(!is.na(df2)) > 0] # remove drugs with no synonyms
# This block creates lists for each drug synonym with main drug name as list name
drug_synonyms <- list()
for (drug in names(df2)) {
  synonyms <- df2[[drug]]
  synonyms <- synonyms[synonyms != ""]
  print(synonyms)
  drug_synonyms[drug] <- list(synonyms)
}
drug_synonyms2 <- lapply(drug_synonyms, function(x) gsub(" ", "", x)) # remove spaces to enable regex to capture multiple word drugs

df1 <- cleaned_data # cleaned data from task 2 i.e. 600 Trios where both parents have linked medication data
df1[drug_columns] <- lapply(df1[drug_columns], tolower) # Convert all values to lower case to match the regex 
df1 <- as.data.frame(lapply(df1, function(x) gsub(" ", "", x)), stringsAsFactors = FALSE) # remove spaces to enable regex to capture multiple word drugs
# Loop over each drug column in df1 and replace with standardised drug name

for (drug in names(drug_synonyms2)) {
  # Create a regex pattern that matches any of the synonyms
  pattern <- paste0("\\b(", paste(drug_synonyms2[[drug]], collapse = "|"), ")\\b") # regex that matches any of the drug synonyms e.g. amoxil or amoxycillin
  
  # Apply the regex replacement across the specified columns using dplyr and mutate
  df1 <- df1 %>%
    mutate(across(all_of(drug_columns), ~ str_replace_all(., pattern, drug))) # replace with drug if pattern matches i.e. replace synonym with main drug name
}
write.csv(df1, file = "./Documents/Emma_Maia_Trios_G/trios_with_no_dup_drugs_sept16.csv")

# 2/ Calculate percentage of overlapping drugs between offspring and parents ----
# compute total number of unique drugs for each trio 
offspring_columns <- grep("Offspring_drug", names(df1), value = TRUE) # first get offspring cols
mother_columns <- grep("Mother_drug", names(df1), value = TRUE) 
father_columns <- grep("Father_drug", names(df1), value = TRUE) 
parents_columns <- c(mother_columns, father_columns) # then parent cols

df3 <- df1 %>%
  rowwise() %>%
  mutate(
    # Count unique drugs in offspring columns
    unique_offspring_drugs = length(unique(as.vector(na.omit(c_across(all_of(offspring_columns)))))),
    
    # Count unique drugs in parents columns
    unique_parents_drugs = length(unique(as.vector(na.omit(c_across(all_of(parents_columns)))))),
    
    # Calculate percentage of overlapping drugs
    unique_combined_drugs = length(unique(as.vector(na.omit(c_across(all_of(c(offspring_columns, parents_columns))))))),
    intersecting_drugs = length(intersect(unique(as.vector(na.omit(c_across(all_of(offspring_columns))))), unique(as.vector(na.omit(c_across(all_of(parents_columns))))))),
    
    percentage_overlap = intersecting_drugs / unique_combined_drugs * 100
  ) %>%
  ungroup()
write.csv(df3, file = "./Documents/Emma_Maia_Trios_G/trios_with_percentage_no_dups_sept16.csv")


