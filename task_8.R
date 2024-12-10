### Join genetic data to trios file with discontinuation status and compute predicted phenotypes

# load libraries ----
library(readr)
library(stringr)

# read in files ----
setwd('/home/ivm/Documents/Emma_Maia_Trios_G/')
CYP2C19 <- read_delim('CYP2C19.raw')
snp_list <- read_delim('snplist.txt', delim = '/t', col_names = FALSE)

# Round imputed values ----
# *2 and *3 only Analysis (3 level phenotype: poor, intermediate, normal)
# Map genotypes to trios
head(CYP2C19$IID)
# Clean IID col & round imputed genotypes - *2 col
CYP2C19_2 <- CYP2C19 %>%
  mutate(IID_2 = str_replace(IID, "^[^_]*_", "")) %>%
  mutate(`10_94781859_G_A_G` = case_when( # Round imputed values for *2
    `10_94781859_G_A_G` > 1.5 ~ 2,
    `10_94781859_G_A_G` > 0.5 & `10_94781859_G_A_G` <= 1.5 ~ 1,
    `10_94781859_G_A_G` <= 0.5 ~ 0
  ))
filtered_trios_96_2 <- filtered_trios_96 %>%
  select(c(3:8, 69516:69519))

# Clean IID col & round imputed genotypes - *17 col
CYP2C19_5 <- CYP2C19 %>%
  mutate(IID_2 = str_replace(IID, "^[^_]*_", "")) %>%
  mutate(`10_94761900_C_T_C` = case_when( # Round imputed values for *2
    `10_94761900_C_T_C` > 1.5 ~ 2,
    `10_94761900_C_T_C` > 0.5 & `10_94761900_C_T_C` <= 1.5 ~ 1,
    `10_94761900_C_T_C` <= 0.5 ~ 0
  ))

# Amitriptyline Sub groups based on discontinuation phenotype (offspring-parent) ----
# 1_1 
offspring_parent_discon_pheno_1_1 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 1 & Parent_discontinuation == 1) %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two_1_1 <- offspring_parent_discon_pheno_1_1 %>%
  count(`10_94781859_G_A_G`)
star_three_1_1 <- offspring_parent_discon_pheno_1_1 %>%
  count(`10_94780653_G_A_G`)

# *17
offspring_parent_discon_pheno_1_1 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 1 & Parent_discontinuation == 1) %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen_1_1 <- offspring_parent_discon_pheno_1_1 %>%
  count(`10_94761900_C_T_C`)
star_seventeen_1_1


# 1_0
offspring_parent_discon_pheno_1_0 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 1 & Parent_discontinuation == 0) %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two_1_0 <- offspring_parent_discon_pheno_1_0 %>%
  count(`10_94781859_G_A_G`)
star_three_1_0 <- offspring_parent_discon_pheno_1_0 %>%
  count(`10_94780653_G_A_G`)

# *17
offspring_parent_discon_pheno_1_0 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 1 & Parent_discontinuation == 0) %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen_1_0 <- offspring_parent_discon_pheno_1_0 %>%
  count(`10_94761900_C_T_C`)
star_seventeen_1_0

# 0_1
offspring_parent_discon_pheno_0_1 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 0 & Parent_discontinuation == 1) %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two_0_1 <- offspring_parent_discon_pheno_0_1 %>%
  count(`10_94781859_G_A_G`)
star_three_0_1 <- offspring_parent_discon_pheno_0_1 %>%
  count(`10_94780653_G_A_G`)

# *17
offspring_parent_discon_pheno_0_1 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 0 & Parent_discontinuation == 1) %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen_0_1 <- offspring_parent_discon_pheno_0_1 %>%
  count(`10_94761900_C_T_C`)
star_seventeen_0_1

# 0_0
offspring_parent_discon_pheno_0_0 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 0 & Parent_discontinuation == 0) %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two_0_0 <- offspring_parent_discon_pheno_0_0 %>%
  count(`10_94781859_G_A_G`)
star_three_0_0 <- offspring_parent_discon_pheno_0_0 %>%
  count(`10_94780653_G_A_G`)

# *17
offspring_parent_discon_pheno_0_0 <- filtered_trios_96_2 %>%
  filter(Offspring_discontinuation == 0 & Parent_discontinuation == 0) %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen_0_0 <- offspring_parent_discon_pheno_0_0 %>%
  count(`10_94761900_C_T_C`)
star_seventeen

# All offspring
all_offspring_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two <- all_offspring_discon_pheno %>%
  count(`10_94781859_G_A_G`)
star_three <- all_offspring_discon_pheno %>%
  count(`10_94780653_G_A_G`)

# *17
all_offspring_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen <- all_offspring_discon_pheno %>%
  count(`10_94761900_C_T_C`)
star_seventeen


# All offspring and parents
all_mothers_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_2, by = c("Mother" = "IID_2"))
star_two_mother <- all_mothers_discon_pheno %>%
  count(`10_94781859_G_A_G`)
star_three_mother <- all_mothers_discon_pheno %>%
  count(`10_94780653_G_A_G`)

all_fathers_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_2, by = c("Father" = "IID_2"))
star_two_father <- all_fathers_discon_pheno %>%
  count(`10_94781859_G_A_G`)
star_three_father <- all_fathers_discon_pheno %>%
  count(`10_94780653_G_A_G`)

# *17
all_mothers_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_5, by = c("Mother" = "IID_2"))
star_seventeen_mother <- all_mothers_discon_pheno %>%
  count(`10_94761900_C_T_C`)

all_fathers_discon_pheno <- filtered_trios_96_2 %>%
  left_join(CYP2C19_5, by = c("Father" = "IID_2"))
star_seventeen_father <- all_fathers_discon_pheno %>%
  count(`10_94761900_C_T_C`)

star_seventeen_mother
star_seventeen_father

# Whole population prescribed amitriptyline
filtered_trios_all_amitriptyline_prescriptions <- trios[
  apply(trios[, ..offspring_columns], 1, contains_amitriptyline, columns = offspring_columns) |
    apply(trios[, ..parent_columns], 1, contains_amitriptyline, columns = parent_columns)
] %>%
  select(3:8)
all_fathers <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_2, by = c("Father" = "IID_2"))
star_two_father <- all_fathers %>%
  count(`10_94781859_G_A_G`)
star_three_father <- all_fathers %>%
  count(`10_94780653_G_A_G`)

all_mothers <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_2, by = c("Mother" = "IID_2"))
star_two_mother <- all_mothers %>%
  count(`10_94781859_G_A_G`)
star_three_mother <- all_mothers %>%
  count(`10_94780653_G_A_G`)

all_offspring <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_2, by = c("Offspring" = "IID_2"))
star_two_offspring <- all_offspring %>%
  count(`10_94781859_G_A_G`)
star_three_offspring <- all_offspring %>%
  count(`10_94780653_G_A_G`)

# *17
filtered_trios_all_amitriptyline_prescriptions <- trios[
  apply(trios[, ..offspring_columns], 1, contains_amitriptyline, columns = offspring_columns) |
    apply(trios[, ..parent_columns], 1, contains_amitriptyline, columns = parent_columns)
] %>%
  select(3:8)
all_fathers <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_5, by = c("Father" = "IID_2"))
star_seventeen_father <- all_fathers %>%
  count(`10_94761900_C_T_C`)

all_mothers <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_5, by = c("Mother" = "IID_2"))
star_seventeen_mother <- all_mothers %>%
  count(`10_94761900_C_T_C`)

all_offspring <- filtered_trios_all_amitriptyline_prescriptions %>%
  left_join(CYP2C19_5, by = c("Offspring" = "IID_2"))
star_seventeen_offspring <- all_offspring %>%
  count(`10_94761900_C_T_C`)

star_seventeen_mother
star_seventeen_father
star_seventeen_offspring

# Whole population 
star_two_all <- CYP2C19_2 %>%
  distinct() %>%
  count(`10_94781859_G_A_G`)
star_three_all <- CYP2C19_2 %>%
  distinct() %>%
  count(`10_94780653_G_A_G`)

# *17
star_seventeen_all <- CYP2C19_5 %>%
  distinct() %>%
  count(`10_94761900_C_T_C`)
star_seventeen_all

