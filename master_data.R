library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(data.table)
library(arrow)

# Cleaned copies of the aggregate-level datasets

income_clean <- income_adjusted_2024 %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(local_authority_code != "" & !is.na(local_authority_code))

weather_clean <- weather_data %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(local_authority_code != "" & !is.na(local_authority_code))

population_clean <- population_data %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(local_authority_code != "" & !is.na(local_authority_code))

library(dplyr)

# === Configuration ===
cols_with_missing <- c(
  "LMK_KEY", "CURRENT_ENERGY_EFFICIENCY",
  "ENERGY_CONSUMPTION_CURRENT", "CO2_EMISSIONS_CURRENT",
  "WALLS_ENERGY_EFF", "MAIN_FUEL",
  "TOTAL_FLOOR_AREA", "local_authority_code",
  "epc_rating_numeric", "insulation_score"
)

chunk_size <- 100000
n_rows <- nrow(epc_master)
n_chunks <- ceiling(n_rows / chunk_size)

# === Chunked Processing ===
for (i in seq_len(n_chunks)) {
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, n_rows)
  
  cat(sprintf("\n🔄 Processing chunk %d of %d (rows %d to %d)...\n", i, n_chunks, start, end))
  
  # Step 1: Extract and clean base chunk
  chunk <- epc_master[start:end, ]
  
  chunk_clean <- chunk %>%
    filter(!is.na(LMK_KEY) & LMK_KEY != "") %>%
    filter(if_all(all_of(cols_with_missing), ~ !is.na(.)))
  
  cat(sprintf("✅ Cleaned base: %d rows remain in chunk %d\n", nrow(chunk_clean), i))
  if (nrow(chunk_clean) == 0) {
    cat("⚠️  Skipping chunk: no clean rows\n")
    next
  }
  
  # Step 2: Join with aggregate datasets
  chunk_enriched <- chunk_clean %>%
    left_join(income_clean, by = "local_authority_code") %>%
    left_join(weather_clean, by = "local_authority_code") %>%
    left_join(population_clean, by = "local_authority_code") %>%
    filter(complete.cases(.))  # Remove new NAs
  
  cat(sprintf("📉 After aggregate joins and clean: %d rows\n", nrow(chunk_enriched)))
  if (nrow(chunk_enriched) == 0) {
    cat("⚠️  Skipping chunk after aggregate join: all rows incomplete\n")
    next
  }
  
  # Step 3: Filter fuel_bills_24 to matching LMK_KEYs only
  relevant_keys <- chunk_enriched$LMK_KEY
  fuel_subset <- fuel_bills_24 %>%
    filter(LMK_KEY %in% relevant_keys)
  
  # Step 4: Join with fuel data and final cleaning
  chunk_final <- chunk_enriched %>%
    left_join(fuel_subset, by = "LMK_KEY") %>%
    filter(complete.cases(.))  # Final filter for missing data
  
  cat(sprintf("🔥 After fuel join and clean: %d rows\n", nrow(chunk_final)))
  if (nrow(chunk_final) == 0) {
    cat("⚠️  Skipping chunk after fuel join: all rows incomplete\n")
    next
  }
  
  # Step 5: Save to CSV
  csv_path <- sprintf("clean_master_chunk_%03d.csv", i)
  write.csv(chunk_final, csv_path, row.names = FALSE)
  cat(sprintf("💾 Saved %d rows to '%s'\n", nrow(chunk_final), csv_path))
  
  # Step 6: Clean up memory
  rm(chunk, chunk_clean, chunk_enriched, chunk_final, fuel_subset, relevant_keys)
  gc()
}

# Get all chunk file names
chunk_files <- list.files(pattern = "^clean_master_chunk_\\d{3}\\.csv$")
chunk_files <- sort(chunk_files)
n_chunks <- length(chunk_files)

# Output file path
output_file <- "clean_master_combined.csv"
write_header <- TRUE

# Track total row count
total_rows <- 0

# Loop through all chunks
for (i in seq_along(chunk_files)) {
  file <- chunk_files[i]
  chunk <- read_csv(file, show_col_types = FALSE)
  rows_in_chunk <- nrow(chunk)
  total_rows <- total_rows + rows_in_chunk
  
  # Display progress in console
  cat(sprintf("🔄 Chunk %3d of %d | File: %-30s | Rows this chunk: %6d | Total rows so far: %9d\n",
              i, n_chunks, file, rows_in_chunk, total_rows))
  
  # Append to combined CSV
  write_csv(chunk, output_file, append = !write_header)
  write_header <- FALSE  # Only write header once
  
  # Cleanup memory
  rm(chunk)
  gc()
}

cat(sprintf("\n✅ DONE: Combined file saved as '%s'\n", output_file))
cat(sprintf("📊 FINAL ROW COUNT: %d rows total\n", total_rows))

clean_master <- fread("clean_master_combined.csv")



chunk_files <- list.files(pattern = "^clean_master_chunk_\\d{3}\\.csv$")
chunk_files <- sort(chunk_files)

# Get column types from first chunk
cat("🔍 Inspecting first chunk to lock column types...\n")
col_types_template <- read_csv(chunk_files[1], show_col_types = FALSE, n_max = 100)

# Force LMK_KEY and any ID-like variables to character
col_types_locked <- cols(
  LMK_KEY = col_character(),
  .default = col_guess()
)

# Read all chunks with locked types
all_chunks <- list()
for (i in seq_along(chunk_files)) {
  cat(sprintf("📦 Reading chunk %03d of %d: %s\n", i, length(chunk_files), chunk_files[i]))
  df <- read_csv(chunk_files[i], col_types = col_types_locked, show_col_types = FALSE)
  all_chunks[[i]] <- df
  rm(df)
  gc()
}

# Bind and write to parquet
cat("🧱 Binding all chunks together...\n")
clean_master <- bind_rows(all_chunks)
rm(all_chunks)
gc()

cat("💾 Writing to clean_master_combined.parquet...\n")
write_parquet(clean_master, "clean_master_combined.parquet")

cat("✅ All done and ready for modeling!\n")

