### Logistic regression for offspring-parent discontinuation accounting of confounders: sex, age, PCs 1-10

# Load libraries ----
library(data.table)
library(readr)
library(dplyr)

# filtered_trios <- filtered_parents_1
filtered_trios <- filtered_trios_96 
setDT(filtered_trios)

# Logistic regression including age and sex ---
mylogit <- glm(Offspring_discontinuation ~ 
                 Parent_discontinuation +  
                 O.age + 
                 O.sex, 
               data = filtered_trios, 
               family = "binomial")

# Print the summary of the logistic regression
summary(mylogit)
exp(coef(mylogit))
exp(cbind(OR=coef(mylogit), confint(mylogit)))

# (Intercept) Parent_discontinuation                  O.age                  O.sex 
# 33.4269107              1.6992347              0.9501214              0.3972329 

# Regression using Offspring_predicted_phenotype as independent variable
# 3 phenotypes ----
filtered_trios_96_3 <- filtered_trios_96 %>%
  select(Offspring, Father, Mother, O.sex, O.age, Offspring_discontinuation, Parent_discontinuation)

CYP2C19_3 <- CYP2C19_2 %>%
  select(IID_2, `10_94780653_G_A_G`, `10_94781859_G_A_G`) 

# Check *3 are all ints
table(CYP2C19$`10_94780653_G_A_G`)

# *17 included
CYP2C19_6 <- CYP2C19 %>%
  mutate(IID_2 = str_replace(IID, "^[^_]*_", "")) %>%
  mutate(`10_94781859_G_A_G` = case_when( # Round imputed values for *2
    `10_94781859_G_A_G` > 1.5 ~ 2,
    `10_94781859_G_A_G` > 0.5 & `10_94781859_G_A_G` <= 1.5 ~ 1,
    `10_94781859_G_A_G` <= 0.5 ~ 0
  )) %>%
  mutate(`10_94761900_C_T_C` = case_when( # Round imputed values for *17
    `10_94761900_C_T_C` > 1.5 ~ 2,
    `10_94761900_C_T_C` > 0.5 & `10_94761900_C_T_C` <= 1.5 ~ 1,
    `10_94761900_C_T_C` <= 0.5 ~ 0
  )) %>%
  select(IID_2, `10_94780653_G_A_G`, `10_94781859_G_A_G`, `10_94761900_C_T_C`) 

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_3, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_10_94780653_G_A_G = `10_94780653_G_A_G`, Offspring_10_94781859_G_A_G = `10_94781859_G_A_G`)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_3, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_10_94780653_G_A_G = `10_94780653_G_A_G`, Mother_10_94781859_G_A_G = `10_94781859_G_A_G`)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_3, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_10_94780653_G_A_G = `10_94780653_G_A_G`, Father_10_94781859_G_A_G = `10_94781859_G_A_G`)

write.csv(final_merged, 'trios_geno_discontinuation_joined.csv')

CYP2C19_4 <- CYP2C19_2 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    geno_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      geno_combined < 3 ~ "poor metabolizer",
      geno_combined == 3 ~ "intermediate metabolizer",
      geno_combined > 3 ~ "normal metabolizer"
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# run both as.factor and as.numeric

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_4, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_4, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_4, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

write.csv(final_merged, 'trios_geno_discontinuation_joined.csv')
trios_geno_discontinuation_joined <- read_csv('trios_geno_discontinuation_joined.csv')

# Do we need to account for trios?
final_merged_1 <- final_merged %>% filter(Parent_discontinuation == 1)

final_merged_0 <- final_merged %>% filter(Parent_discontinuation == 0)


# Try with offspring discontinuation
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)

# 4 phenotypes ----
# Assuming CYP219C_2 is the dataframe
CYP2C19_7 <- CYP2C19_6 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    lof_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      lof_combined < 3 ~ "poor metabolizer",
      lof_combined == 3 ~ "intermediate metabolizer",
      lof_combined > 3 ~ "normal metabolizer"
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_4, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_4, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_4, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

write.csv(final_merged, 'trios_geno_discontinuation_joined.csv')

# Don't we need both parent discontinuation phenotypes for the regression?
# Do we need to account for trios?
final_merged_1 <- final_merged %>% filter(Parent_discontinuation == 1)

final_merged_0 <- final_merged %>% filter(Parent_discontinuation == 0)

# Regression using 3 level metaboliser phenotypes + parent discontinuation THINK THIS SHOULD BE OFFSPRING DISCONTINUATION
mylogit_1 <- glm(Parent_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Parent_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)

# Try with offspring discontinuation
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)

# Looking into prevelance of genotypes ----
CYP2C19_test <- CYP2C19_6 %>%
  mutate_if(is.numeric, ~ case_when(
    . > 1.5 ~ 2,
    . > 0.5 & . <= 1.5 ~ 1,
    TRUE ~ 0 
  )) 

CYP2C19_test2 <- CYP2C19_test %>%
  rename(
    het1 = `10_94781859_G_A_HET`,
    het2 = `10_94761900_C_T_HET`,
    het3 = `10_94780653_G_A_HET`
  ) %>%
  mutate(het_sum = het1 + het2 + het3) %>%
  count(het_sum)
CYP2C19_test2

CYP2C19_star_two_hom <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 0)
nrow(CYP2C19_star_two_hom)
(nrow(CYP2C19_star_two_hom)/nrow(CYP2C19_test))*100

CYP2C19_star_three_hom <- CYP2C19_test %>%
  filter(`10_94780653_G_A_G` == 0)
nrow(CYP2C19_star_three_hom)
(nrow(CYP2C19_star_three_hom)/nrow(CYP2C19_test))*100

CYP2C19_star_seventeen_hom <- CYP2C19_test %>%
  filter(`10_94761900_C_T_C` == 0)
nrow(CYP2C19_star_seventeen_hom)
(nrow(CYP2C19_star_seventeen_hom)/nrow(CYP2C19_test))*100

CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 0 & `10_94780653_G_A_G` == 0 & `10_94761900_C_T_C` == 0)
nrow(CYP2C19_test3)
CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 0 & `10_94780653_G_A_G` == 0)
nrow(CYP2C19_test3)
CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 0 & `10_94761900_C_T_C` == 0)
nrow(CYP2C19_test3)
CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94780653_G_A_G` == 0 & `10_94761900_C_T_C` == 0)
nrow(CYP2C19_test3)

CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 1 & `10_94780653_G_A_G` == 0)
nrow(CYP2C19_test3)
CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94781859_G_A_G` == 1 & `10_94761900_C_T_C` == 0)
nrow(CYP2C19_test3)
CYP2C19_test3 <- CYP2C19_test %>%
  filter(`10_94780653_G_A_G` == 1 & `10_94761900_C_T_C` == 0)
nrow(CYP2C19_test3)


CYP2C19_combinations <- list()
# Enumerate all combinations for each column
values <- c(0, 1, 2)
# Index for storing results
index <- 1

for (val1 in values) {
  for (val2 in values) {
    for (val3 in values) {
      combination_filter <- CYP2C19_test %>%
        filter(
          `10_94781859_G_A_G` == val1 & 
            `10_94780653_G_A_G` == val2 & 
            `10_94761900_C_T_C` == val3
        )
      
      # Store the result in the list
      CYP2C19_combinations[[index]] <- combination_filter
      
      # Print the row count for each combination
      print(paste("Combination", val1, val2, val3, "Row Count:", nrow(combination_filter)))
      
      index <- index + 1
    }
  }
}

# Regression using first 10 PCs ----
# covars file
# Load the additional 10 PCs
covars <- read_delim('/genesandhealth/library-red/genesandhealth/GSAv3EAMD/Jan2024_51k_TOPMED-r3_Imputation_b38/GNH.51170.noEthnicOutliers.covariates.20PCs.tab')
covars2 <- covars %>% mutate(IID_2 = str_replace(IID, "^[^_]*_", "")) %>% select(IID_2, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10)

# Run as factor ----
# 96 trios with discontinuation phenotypes and age sex covars
filtered_trios_96_3 <- filtered_trios_96 %>%
  select(Offspring, Father, Mother, O.sex, O.age, Offspring_discontinuation, Parent_discontinuation)

# Variants file with coded phenotypes
CYP2C19_alpha <- CYP2C19_2 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    geno_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      geno_combined < 3 ~ "c_PM", # 
      geno_combined == 3 ~ "b_IM",
      geno_combined > 3 ~ "a_NM" # should be 3.5 
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# Add geno/phenotypes to trios file
# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_alpha, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_alpha, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_alpha, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

# Add PCs
final_merged2 <- final_merged %>%
  left_join(covars2,  by = c("Offspring" = "IID_2"))

# Check predicted_metaboliser type
final_merged2$predicted_phenotype <- as.factor(final_merged2$predicted_phenotype)
typeof(final_merged2$predicted_phenotype)
is.factor(final_merged2$predicted_phenotype)

# Filter for only parental discontinuation phenotype
final_merged_1 <- final_merged2 %>% filter(Parent_discontinuation == 1)

# Filter for only NO parental discontinuation phenotype
final_merged_0 <- final_merged2 %>% filter(Parent_discontinuation == 0)

# Run regression 
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)
exp(cbind(OR=coef(mylogit_1), confint(mylogit_1)))
exp(cbind(OR=coef(mylogit_0), confint(mylogit_0)))

# Run as numeric ----
CYP2C19_numeric <- CYP2C19_2 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    geno_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      geno_combined < 3 ~ "2",
      geno_combined == 3 ~ "1",
      geno_combined > 3 ~ "0"
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_numeric, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_numeric, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_numeric, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

# Add PCs
final_merged2 <- final_merged %>%
  left_join(covars2,  by = c("Offspring" = "IID_2"))

# Check predicted_metaboliser type
final_merged2$predicted_phenotype <- as.numeric(final_merged2$predicted_phenotype)
typeof(final_merged2$predicted_phenotype)
is.numeric(final_merged2$predicted_phenotype)

final_merged_1 <- final_merged2 %>% filter(Parent_discontinuation == 1)

final_merged_0 <- final_merged2 %>% filter(Parent_discontinuation == 0)

# Run regression 
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)

# Again but pool IM and PM factor ----
CYP2C19_alpha <- CYP2C19_2 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    geno_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      geno_combined <= 3 ~ "b_PM",
      geno_combined > 3 ~ "a_NM"
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_alpha, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_alpha, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_alpha, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

# Add PCs
final_merged2 <- final_merged %>%
  left_join(covars2,  by = c("Offspring" = "IID_2"))

# Check predicted_metaboliser type
final_merged2$predicted_phenotype <- as.factor(final_merged2$predicted_phenotype)
typeof(final_merged2$predicted_phenotype)
is.factor(final_merged2$predicted_phenotype)

final_merged_1 <- final_merged2 %>% filter(Parent_discontinuation == 1)

final_merged_0 <- final_merged2 %>% filter(Parent_discontinuation == 0)

# Run regression 
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)
exp(cbind(OR=coef(mylogit_1), confint(mylogit_1)))
exp(cbind(OR=coef(mylogit_0), confint(mylogit_0)))

# Again but pool IM and PM numeric ----
CYP2C19_numeric <- CYP2C19_2 %>%
  mutate(
    # Combine geno1, geno2, and geno3 into a new column
    geno_combined = `10_94780653_G_A_G` + `10_94781859_G_A_G`,
    # Convert geno3 to metabolizer status based on the value of n
    predicted_phenotype = case_when(
      geno_combined <= 3 ~ "1",
      geno_combined > 3 ~ "0"
    )
  ) %>%
  select(IID_2, predicted_phenotype)

# Merge for Offspring
offspring_merged <- merge(filtered_trios_96_3, CYP2C19_numeric, by.x = "Offspring", by.y = "IID_2", all.x = TRUE)
offspring_merged <- offspring_merged %>% 
  rename(Offspring_predicted_phenotype = predicted_phenotype)

# Merge for Mother
mother_merged <- merge(offspring_merged, CYP2C19_numeric, by.x = "Mother", by.y = "IID_2", all.x = TRUE)
mother_merged <- mother_merged %>% 
  rename(Mother_predicted_phenotype = predicted_phenotype)

# Merge for Father
final_merged <- merge(mother_merged, CYP2C19_numeric, by.x = "Father", by.y = "IID_2", all.x = TRUE)
final_merged <- final_merged %>% 
  rename(Father_predicted_phenotype = predicted_phenotype)

# Add PCs
final_merged2 <- final_merged %>%
  left_join(covars2,  by = c("Offspring" = "IID_2"))

# Check predicted_metaboliser type
final_merged2$predicted_phenotype <- as.numeric(final_merged2$predicted_phenotype)
typeof(final_merged2$predicted_phenotype)
is.numeric(final_merged2$predicted_phenotype)

final_merged_1 <- final_merged2 %>% filter(Parent_discontinuation == 1)

final_merged_0 <- final_merged2 %>% filter(Parent_discontinuation == 0)

# Run regression 
mylogit_1 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_1, 
                 family = "binomial")

mylogit_0 <- glm(Offspring_discontinuation ~ 
                   Offspring_predicted_phenotype +  
                   O.age + 
                   O.sex +
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10, 
                 data = final_merged_0, 
                 family = "binomial")

summary(mylogit_1)
summary(mylogit_0)