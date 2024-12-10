### Filter trios for trios with two generation Amitriptyline discontinuation and compute discontinuation phenotype status columns

# Load libraries ----
library(data.table)
library(lubridate)
library(progress)
library(readr)
library(dplyr)

meds_with_year <- read_csv("/home/ivm/Documents/Emma_Maia_Trios_G/meds_with_year.csv")

trios <- read_csv("/home/ivm/Documents/Emma_Maia_Trios_G/trios_with_percentage_no_dups_sept16.csv", col_types = cols(
  Offspring.WES = col_character(),
  Father.WES = col_character(),
  Mother.WES = col_character()
))

# Filter trios for amitriptyline, n = 96 ----
# Convert trios to data.table
setDT(trios)

# Identify columns that start with "Offspring_drug", "Father_drug", and "Mother_drug"
offspring_columns <- grep("^Offspring_drug", colnames(trios), value = TRUE)
father_columns <- grep("^Father_drug", colnames(trios), value = TRUE)
mother_columns <- grep("^Mother_drug", colnames(trios), value = TRUE)

# Combine Father and Mother columns for easier matching
parent_columns <- c(father_columns, mother_columns)

# Function to check for presence of "amitriptyline" in a row for a specific set of columns
contains_amitriptyline <- function(row, columns) {
  any(row[columns] %in% "amitriptyline", na.rm = TRUE)
}

# Filter rows where "amitriptyline" is present in both offspring columns and parent columns (either mother or father)
filtered_trios_96 <- trios[
  apply(trios[, ..offspring_columns], 1, contains_amitriptyline, columns = offspring_columns) &
    apply(trios[, ..parent_columns], 1, contains_amitriptyline, columns = parent_columns)
]


# Filter offspring for amitriptyline ----
# Ensure trios is a data.table
setDT(trios)

# Identify columns that need to be checked for "amitriptyline"
drug_columns <- grep("Offspring_drug_", colnames(trios), value = TRUE)

# Create a logical condition to check for the presence of "amitriptyline" in any of the drug columns
condition <- rowSums(trios[, lapply(.SD, function(x) grepl("amitriptyline", x, ignore.case = TRUE)), .SDcols = drug_columns]) > 0

# Filter the rows based on the condition
filtered_trios_whole <- trios[condition]

# Add discontinuation columns to trios files (filtered_trios_96 & filtered_trios_whole) ----
# filtered_trios_96 
# filter medication data frame for amitriptyline
setDT(meds_with_year)  

# Filter rows where 'drugname' column contains 'amitriptyline'
filtered_meds <- meds_with_year[drugname == "amitriptyline"]

# Ensure filtered_trios and filtered_meds are data.tables
setDT(filtered_trios_96)
setDT(filtered_meds)

# Identify columns in filtered_trios that end with .WES_2
wes_columns <- grep("\\.WES_2$", colnames(filtered_trios_96), value = TRUE)

# Loop through each .WES_2 column in filtered_trios
for (col in wes_columns) {
  # Create new column names for start and end dates
  base_name <- sub("\\.WES_2$", "", col)
  start_col <- paste0(base_name, "_start")
  end_col <- paste0(base_name, "_end")
  
  # Initialise the new columns in filtered_trios
  filtered_trios_96[[start_col]] <- as.Date(NA)
  filtered_trios_96[[end_col]] <- as.Date(NA)
  
  # Loop through each row in filtered_trios
  for (i in 1:nrow(filtered_trios_96)) {
    wes_value <- filtered_trios_96[[col]][i]
    
    # Skip if the WES value is NA
    if (is.na(wes_value) || wes_value == "") next
    
    # Find matches in filtered_meds
    matched_rows <- filtered_meds[pseudo_nhs_number == wes_value]
    
    # Skip if no matches found
    if (nrow(matched_rows) == 0) next
    
    # Convert clinical_effective_date to Date objects
    matched_dates <- as.Date(matched_rows$clinical_effective_date, format = "%Y-%m-%d")
    
    # Remove NA values
    matched_dates <- na.omit(matched_dates)
    
    # Skip if no valid dates found
    if (length(matched_dates) == 0) next
    
    # Find the earliest and latest dates
    earliest_date <- min(matched_dates)
    latest_date <- max(matched_dates)
    
    # Store the earliest and latest dates in the new columns
    filtered_trios_96[i, (start_col) := earliest_date]
    filtered_trios_96[i, (end_col) := latest_date]
  }
}

# Identify columns in filtered_trios that end with .WES_2
wes_columns <- grep("\\.WES_2$", colnames(filtered_trios_96), value = TRUE)

# Loop through each .WES_2 column in filtered_trios
for (col in wes_columns) {
  # Create new column names for start and end dates
  base_name <- sub("\\.WES_2$", "", col)
  start_col <- paste0(base_name, "_start")
  end_col <- paste0(base_name, "_end")
  discontinuation_col <- paste0(base_name, "_discontinuation")
  
  # Initialise the new column for discontinuation status
  filtered_trios_96[[discontinuation_col]] <- NA_integer_
  
  # Loop through each row in filtered_trios
  for (i in 1:nrow(filtered_trios_96)) {
    start_date <- filtered_trios_96[[start_col]][i]
    end_date <- filtered_trios_96[[end_col]][i]
    
    # Calculate the difference in days
    if (!is.na(start_date) && !is.na(end_date)) {
      date_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
      
      # Check if the difference is less than or equal to X days 
      filtered_trios_96[i, (discontinuation_col) := ifelse(date_diff <= 91, 1, 0)] 
    } else {
      # Assign NA if either date is missing
      filtered_trios_96[i, (discontinuation_col) := NA_integer_]
    }
  }
}

# Create binary Parent column for discontinuation status
# 0 = neither parents discontinued within the time frame
# 1 = One parent discontinued within the time frame
# 2 = both parents discontinued within the time frame

# Ensure filtered_trios is a data.table
setDT(filtered_trios_96)

# Initialise the new column Parent_discontinuation
filtered_trios_96[, Parent_discontinuation := NA_integer_]

# Loop through each row to calculate the Parent_discontinuation
for (i in 1:nrow(filtered_trios_96)) {
  father_val <- filtered_trios_96$Father_discontinuation[i]
  mother_val <- filtered_trios_96$Mother_discontinuation[i]
  
  if (!is.na(father_val) & father_val == 1 & !is.na(mother_val) & mother_val == 1) {
    filtered_trios_96$Parent_discontinuation[i] <- 2
  } else if ((!is.na(father_val) & father_val == 1) | (!is.na(mother_val) & mother_val == 1)) {
    filtered_trios_96$Parent_discontinuation[i] <- 1
  } else {
    filtered_trios_96$Parent_discontinuation[i] <- 0
  }
}


# Filter rows where Parent_discontinuation is 1
filtered_parents_1 <- filtered_trios_96[Parent_discontinuation == 1]

# filtered_trios_whole 
# Ensure filtered_trios and filtered_meds are data.tables
setDT(filtered_trios_whole)
setDT(filtered_meds)

# Identify columns in filtered_trios that end with .WES_2
wes_columns <- grep("\\.WES_2$", colnames(filtered_trios_whole), value = TRUE)

# Loop through each .WES_2 column in filtered_trios
for (col in wes_columns) {
  # Create new column names for start and end dates
  base_name <- sub("\\.WES_2$", "", col)
  start_col <- paste0(base_name, "_start")
  end_col <- paste0(base_name, "_end")
  
  # Initialise the new columns in filtered_trios
  filtered_trios_whole[[start_col]] <- as.Date(NA)
  filtered_trios_whole[[end_col]] <- as.Date(NA)
  
  # Loop through each row in filtered_trios
  for (i in 1:nrow(filtered_trios_whole)) {
    wes_value <- filtered_trios_whole[[col]][i]
    
    # Skip if the WES value is NA
    if (is.na(wes_value) || wes_value == "") next
    
    # Find matches in filtered_meds
    matched_rows <- filtered_meds[pseudo_nhs_number == wes_value]
    
    # Skip if no matches found
    if (nrow(matched_rows) == 0) next
    
    # Convert clinical_effective_date to Date objects
    matched_dates <- as.Date(matched_rows$clinical_effective_date, format = "%Y-%m-%d")
    
    # Remove NA values
    matched_dates <- na.omit(matched_dates)
    
    # Skip if no valid dates found
    if (length(matched_dates) == 0) next
    
    # Find the earliest and latest dates
    earliest_date <- min(matched_dates)
    latest_date <- max(matched_dates)
    
    # Store the earliest and latest dates in the new columns
    filtered_trios_whole[i, (start_col) := earliest_date]
    filtered_trios_whole[i, (end_col) := latest_date]
  }
}

# Identify columns in filtered_trios that end with .WES_2
wes_columns <- grep("\\.WES_2$", colnames(filtered_trios_whole), value = TRUE)

# Loop through each .WES_2 column in filtered_trios
for (col in wes_columns) {
  # Create new column names for start and end dates
  base_name <- sub("\\.WES_2$", "", col)
  start_col <- paste0(base_name, "_start")
  end_col <- paste0(base_name, "_end")
  discontinuation_col <- paste0(base_name, "_discontinuation")
  
  # Initialise the new column for discontinuation status
  filtered_trios_whole[[discontinuation_col]] <- NA_integer_
  
  # Loop through each row in filtered_trios
  for (i in 1:nrow(filtered_trios_whole)) {
    start_date <- filtered_trios_whole[[start_col]][i]
    end_date <- filtered_trios_whole[[end_col]][i]
    
    # Calculate the difference in days
    if (!is.na(start_date) && !is.na(end_date)) {
      date_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
      
      # Check if the difference is less than or equal to X days 
      filtered_trios_whole[i, (discontinuation_col) := ifelse(date_diff <= 91, 1, 0)] #### THIS IS CURRENTLY SET TO 3 MONTHS
    } else {
      # Assign NA if either date is missing
      filtered_trios_whole[i, (discontinuation_col) := NA_integer_]
    }
  }
}

### Create binary Parent column for discontinuation status
# 0 = neither parents discontinued within the time frame
# 1 = One parent discontinued within the time frame
# 2 = both parents discontinued within the time frame

# Ensure filtered_trios is a data.table
setDT(filtered_trios_whole)

# Initialise the new column Parent_discontinuation
filtered_trios_whole[, Parent_discontinuation := NA_integer_]

# Loop through each row to calculate the Parent_discontinuation
for (i in 1:nrow(filtered_trios_whole)) {
  father_val <- filtered_trios_whole$Father_discontinuation[i]
  mother_val <- filtered_trios_whole$Mother_discontinuation[i]
  
  if (!is.na(father_val) & father_val == 1 & !is.na(mother_val) & mother_val == 1) {
    filtered_trios_whole$Parent_discontinuation[i] <- 2
  } else if ((!is.na(father_val) & father_val == 1) | (!is.na(mother_val) & mother_val == 1)) {
    filtered_trios_whole$Parent_discontinuation[i] <- 1
  } else {
    filtered_trios_whole$Parent_discontinuation[i] <- 0
  }
}

# Demographics
num_offspring_discontinued_3_months <- filtered_trios_96 %>% filter(Offspring_discontinuation == 1)
nrow(num_offspring_discontinued_3_months)
sex_of_48_offspring <- num_offspring_discontinued_3_months %>% count(O.sex)
# 1 = 11, 2 = 37

# Parental discontinuation
filtered_parents_1 <- filtered_trios_96[Parent_discontinuation == 1]
nrow(filtered_parents_1) # 26
offspring_and_parent_discon <- filtered_parents_1[Offspring_discontinuation == 1]
nrow(offspring_and_parent_discon) # 16

ncol(filtered_parents_1)
filtered_parents_1_shortcols <- filtered_parents_1[,69505:69519]
# Mother only
mother_discon <- filtered_parents_1_shortcols %>% count(Mother_discontinuation)
# Father only
father_discon <- filtered_parents_1_shortcols %>% count(Father_discontinuation)

# Both parents
filtered_trios_96_2 <- filtered_trios_96[,69505:69519]
m_discon <- filtered_trios_96 %>% filter(Mother_discontinuation == 1)
m_plus_f_discon <- m_discon %>% filter(Father_discontinuation == 1)
nrow(m_plus_f_discon) #n = 0


# Number of mothers and fathers prescribed amitriptyline
# Mothers
drug_columns <- grep("Mother_drug_", colnames(trios), value = TRUE)
# Create a logical condition to check for the presence of "amitriptyline" in any of the drug columns
condition <- rowSums(filtered_trios_96[, lapply(.SD, function(x) grepl("amitriptyline", x, ignore.case = TRUE)), .SDcols = drug_columns]) > 0
# Filter the rows based on the condition
filtered_trios <- filtered_trios_96[condition]
nrow(filtered_trios)
# n = 87
# Fathers
drug_columns <- grep("Father_drug_", colnames(trios), value = TRUE)
# Create a logical condition to check for the presence of "amitriptyline" in any of the drug columns
condition <- rowSums(filtered_trios_96[, lapply(.SD, function(x) grepl("amitriptyline", x, ignore.case = TRUE)), .SDcols = drug_columns]) > 0
# Filter the rows based on the condition
filtered_trios <- filtered_trios_96[condition]
nrow(filtered_trios)
# n = 55
