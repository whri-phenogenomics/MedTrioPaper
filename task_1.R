### map Trios (672 of them) to Medication data using the link file for pseudonyms

# Load libraries ----
library(data.table)
library(dplyr)
library(readr)

# read in medication, trios and link files ----
drugs <- read_csv("/finngen/red/Medications/outputs/cleaned_prescription_data_16_11_23.csv")
trios <- read_table("/finngen/library-red/genesandhealth/trios/GH_44k_trios_kinship_sex_age_ME_QCed_WES-OrageneIDs.txt")
link_file <- read_delim("/finngen/library-red/genesandhealth/2024_05_03_OrageneID_PseudoNHS_Gender_withmissing_forTRE.tab", 
                        delim = "\t", escape_double = FALSE, 
                        col_types = cols(OrageneID = col_character()), 
                        trim_ws = TRUE)

# Map trios to medication data ----
# remove prefix and suffix from OrageneID
process_WES_cols <- function(df) {
  # Identify columns that end with .WES
  wes_cols <- grep("\\.WES$", names(df), value = TRUE)
  
  for (col in wes_cols) {
    # Remove GNH- 
    df[[col]] <- gsub("^GNH-", "", df[[col]])
    
    # Remove _ and number 
    df[[col]] <- gsub("_\\d+$", "", df[[col]])
    
  }
  
  return(df)
}

# Process trios file 
processed_data <- process_WES_cols(trios)

# Link pseudoNHS ID to trios data

# Identify columns in trios that end with .WES
wes_columns <- grep("\\.WES$", colnames(processed_data), value = TRUE)

# Loop through each row of the link_file and check for matches
for (i in 1:nrow(link_file)) {
  oragene_id <- link_file$OrageneID[i]
  pseudo_nhs <- link_file$'PseudoNHS_2024-05-03'[i]
  
  # Check if orageneID exists in any of the WES columns in the trios file
  for (wes_col in wes_columns) {
    matches <- which(processed_data[[wes_col]] == oragene_id)
    if (length(matches) > 0) {
      new_col_name <- paste0(wes_col, "_2")
      # If the new column does not already exist, create it and initialise with NA
      if (!(new_col_name %in% colnames(processed_data))) {
        processed_data[[new_col_name]] <- NA
      }
      # Add the pseudo_nhs value to the new column in the matched rows
      processed_data[matches, new_col_name] <- pseudo_nhs
    }
  }
}

# Link medication data to trios file

# Identify columns in processed_data that end with .WES_2
wes2_columns <- grep("\\.WES_2$", colnames(processed_data), value = TRUE)

# Loop through each WES_2 column in the processed_data file
for (wes2_col in wes2_columns) {
  # Extract base column name by removing .WES_2
  base_col_name <- sub("\\.WES_2$", "", wes2_col)
  
  # Initialise columns to store drug names
  max_drug_columns <- 0
  for (row_idx in 1:nrow(processed_data)) {
    pseudo_nhs_value <- processed_data[[wes2_col]][row_idx]
    
    # Find the rows in the drug file where pseudo_nhs_number matches
    matches <- which(drugs$pseudo_nhs_number == pseudo_nhs_value)
    
    if (length(matches) > 0) {
      max_drug_columns <- max(max_drug_columns, length(matches))
    }
  }
  
  # Create drug columns if not already present
  for (j in 1:max_drug_columns) {
    new_col_name <- paste0(base_col_name, "_drug_", j)
    if (!(new_col_name %in% colnames(processed_data))) {
      processed_data[[new_col_name]] <- NA
    }
  }
  
  # Loop through each row of the WES_2 column in the processed_data file
  for (row_idx in 1:nrow(processed_data)) {
    pseudo_nhs_value <- processed_data[[wes2_col]][row_idx]
    
    # Find the rows in the drug file where pseudo_nhs_number matches
    matches <- which(drugs$pseudo_nhs_number == pseudo_nhs_value)
    
    # Loop through each match and add the corresponding drugname to the processed_data file
    if (length(matches) > 0) {
      for (match_idx in 1:length(matches)) {
        drug_name <- drugs$drugname[matches[match_idx]]
        
        # Create new column name
        new_col_name <- paste0(base_col_name, "_drug_", match_idx)
        
        # Add the drug name to the new column in the corresponding row
        processed_data[row_idx, new_col_name] <- drug_name
      }
    }
  }
}

# save trios file with linked medication data ----
write.csv(processed_data, file = "Linked_medication.csv")
