library(data.table)

# Input file names
years <- 2020:2024
file_names <- paste0("EPC", years, ".csv")

# Required columns
columns_to_read <- c(
  "LMK_KEY",
  "CURRENT_ENERGY_RATING",
  "CURRENT_ENERGY_EFFICIENCY",
  "ENERGY_CONSUMPTION_CURRENT",
  "CO2_EMISSIONS_CURRENT",
  "ROOF_ENERGY_EFF",
  "WALLS_ENERGY_EFF",
  "MAIN_FUEL",
  "PROPERTY_TYPE",
  "BUILT_FORM",
  "TOTAL_FLOOR_AREA",
  "NUMBER_HABITABLE_ROOMS",
  "TENURE",
  "CONSTRUCTION_AGE_BAND",
  "LOCAL_AUTHORITY",
  "POSTCODE",
  "LODGEMENT_DATE"
)

insulation_cols <- c("ROOF_ENERGY_EFF", "WALLS_ENERGY_EFF")

epc_list <- list()

for (i in seq_along(file_names)) {
  
  # Read in selected columns
  epc <- fread(file_names[i], select = columns_to_read, na.strings = c("", "N/A", "NA"))
  
  # Remove any repeated headers if present as rows
  epc <- epc[CURRENT_ENERGY_RATING != "CURRENT_ENERGY_RATING"]
  
  # Add year column
  epc[, epc_year := years[i]]
  
  # Create EPC rating numeric
  epc[, epc_rating_numeric := match(CURRENT_ENERGY_RATING, c("A", "B", "C", "D", "E", "F", "G"))]
  
  # Extract postcode district
  epc[, postcode_district := sub(" .*", "", POSTCODE)]
  
  # Define mapping from energy efficiency label to numeric score
  efficiency_map <- c(
    "Very Poor" = 1,
    "Poor" = 2,
    "Average" = 3,
    "Good" = 4,
    "Very Good" = 5
  )
  
  # Convert the energy efficiency columns to numeric using the map
  epc[, ROOF_ENERGY_EFF := efficiency_map[ROOF_ENERGY_EFF]]
  epc[, WALLS_ENERGY_EFF := efficiency_map[WALLS_ENERGY_EFF]]

  
  # Compute insulation score
  epc[, insulation_score := rowMeans(.SD, na.rm = TRUE), .SDcols = insulation_cols]
  
  # Append to list
  epc_list[[i]] <- epc
}

# Combine all clean data
epc_master <- rbindlist(epc_list, use.names = TRUE, fill = TRUE)

# Optional: check for any leftover headers in rows
epc_master <- epc_master[CURRENT_ENERGY_RATING != "CURRENT_ENERGY_RATING"]

# Save output
fwrite(epc_master, "EPC_master_2020_2024.csv")

epc_master <- fread("EPC_master_2020_2024.csv", nThread = 4, showProgress = TRUE)

epc_master <- epc_master %>%
  rename(local_authority_code = LOCAL_AUTHORITY)

