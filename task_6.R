### Compute top 50 overlapping drugs between offspring and parents

# Load libraries ----
library(data.table)
library(progress)

# Identify columns that start with "Offspring_drug", "Father_drug", and "Mother_drug"
df1 <- read.csv("trios_with_no_dup_drugs_sept.csv")
offspring_columns <- grep("^Offspring_drug", colnames(df1), value = TRUE)
father_columns <- grep("^Father_drug", colnames(df1), value = TRUE)
mother_columns <- grep("^Mother_drug", colnames(df1), value = TRUE)

# Combine Father and Mother columns for easier matching
parent_columns <- c(father_columns, mother_columns)

intersecting_drugs_list <- df1 %>%
  rowwise() %>%
  mutate(
    intersecting_drugs = list(intersect(
      unique(c_across(all_of(offspring_columns))),
      unique(c_across(all_of(parent_columns)))
    ))
  ) %>%
  pull(intersecting_drugs)

# Step 2: Combine the intersecting drugs across all trios into one vector
all_intersecting_drugs <- unlist(intersecting_drugs_list)

# Step 3: Count how many trios each drug overlaps in
drug_overlap_count <- table(all_intersecting_drugs)

drug_overlap_df <- as.data.frame(drug_overlap_count)
colnames(drug_overlap_df) <- c("Drug", "Overlap_Count")

top_50_drugs <- drug_overlap_df %>%
  arrange(desc(Overlap_Count)) %>%
  slice_head(n = 50)
write.csv(top_50_drugs, 'top_50_drugs_offspring_parent_overlap_sept16.csv')


