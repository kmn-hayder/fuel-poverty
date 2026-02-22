library(readxl)
library(readr)
library(arrow)
library(purrr)
library(brms)
library(dplyr)
library(tibble)

mcmc_data <- read_parquet("mcmc_split_clean_master.parquet")

# File path
supp_file <- "LILEE_supplementary_tables_2024.xlsx"

# Only required sheets
selected_sheets <- c(
  "Table_2_FPEER", "Table_3_SAP", "Table_4_rurality", "Table_5_region", "Table_6_dwelling_type",
  "Table_7_age_dwelling", "Table_8_floor_area", "Table_10_main_fuel",
  "Table_11_wall_insulation", "Table_12_tenure"
)

# Load selected sheets
list2env(
  set_names(
    map(selected_sheets, ~ read_excel(supp_file, sheet = .x)),
    selected_sheets
  ),
  envir = .GlobalEnv
)

mcmc_data$urban_rural_simplified <- dplyr::case_when(
  mcmc_data$urban_rural.y %in% c(
    "Urban: Majority nearer to a major town or city",
    "Urban: Majority further from a major town or city"
  ) ~ "Urban",
  
  mcmc_data$urban_rural.y %in% c(
    "Intermediate urban: Majority nearer to a major town or city",
    "Intermediate urban: Majority further from a major town or city",
    "Intermediate rural: Majority nearer to a major town or city",
    "Intermediate rural: Majority further from a major town or city"
  ) ~ "Semi-rural",
  
  mcmc_data$urban_rural.y %in% c(
    "Majority rural: Majority nearer to a major town or city",
    "Majority rural: Majority further from a major town or city"
  ) ~ "Rural",
  
  TRUE ~ NA_character_  # catch any unexpected values
)

# Convert epc_rating_numeric to character (if not already)
mcmc_data$epc_rating_band <- dplyr::recode(
  mcmc_data$epc_rating_numeric,
  `1` = "A",
  `2` = "B",
  `3` = "C",
  `4` = "D",
  `5` = "E",
  `6` = "F",
  `7` = "G"
)

mcmc_data$dwelling_type_mapped <- dplyr::case_when(
  mcmc_data$PROPERTY_TYPE == "House" ~ "Semi-detached",  # safest general default
  mcmc_data$PROPERTY_TYPE == "Maisonette" ~ "Purpose-built flat",
  mcmc_data$PROPERTY_TYPE == "Flat" ~ "Purpose-built flat",
  mcmc_data$PROPERTY_TYPE == "Bungalow" ~ "Detached",
  mcmc_data$PROPERTY_TYPE == "Park home" ~ NA_character_,  # to be reviewed
  TRUE ~ NA_character_
)

# Remove rows where PROPERTY_TYPE could not be mapped
mcmc_data <- mcmc_data %>%
  filter(!is.na(dwelling_type_mapped))

# Define the mapping
mcmc_data$dwelling_age_mapped <- dplyr::case_when(
  mcmc_data$CONSTRUCTION_AGE_BAND %in% c("England and Wales: before 1900", "England and Wales: 1900-1929") ~ "Pre 1919",
  mcmc_data$CONSTRUCTION_AGE_BAND == "England and Wales: 1930-1949" ~ "1919 to 1944",
  mcmc_data$CONSTRUCTION_AGE_BAND == "England and Wales: 1950-1966" ~ "1945 to 1964",
  mcmc_data$CONSTRUCTION_AGE_BAND %in% c("England and Wales: 1967-1975", "England and Wales: 1976-1982") ~ "1965 to 1980",
  mcmc_data$CONSTRUCTION_AGE_BAND == "England and Wales: 1983-1990" ~ "1981 to 1990",
  mcmc_data$CONSTRUCTION_AGE_BAND %in% c("England and Wales: 1991-1995", "England and Wales: 1996-2002") ~ "1991 to 2002",
  mcmc_data$CONSTRUCTION_AGE_BAND %in% c("England and Wales: 2003-2006", "England and Wales: 2007 onwards",
                                         "England and Wales: 2007-2011", "England and Wales: 2012 onwards") ~ "Post 2002",
  TRUE ~ NA_character_
)

# Remove rows that couldn’t be mapped
mcmc_data <- mcmc_data %>%
  filter(!is.na(dwelling_age_mapped))

mcmc_data <- mcmc_data %>%
  mutate(wall_insulation_cat = case_when(
    WALLS_ENERGY_EFF == 1 ~ "Cavity with insulation",
    WALLS_ENERGY_EFF == 2 ~ "Solid with insulation",
    WALLS_ENERGY_EFF == 3 ~ "Other",
    WALLS_ENERGY_EFF == 4 ~ "Solid uninsulated",
    WALLS_ENERGY_EFF == 5 ~ "Cavity uninsulated",
    TRUE ~ NA_character_
  ))

mcmc_data <- mcmc_data %>%
  mutate(tenure_cat = case_when(
    TENURE %in% c("owner-occupied", "Owner-occupied") ~ "Owner occupied",
    TENURE %in% c("rental (private)", "Rented (private)") ~ "Private rented",
    TENURE %in% c("rental (social)", "Rented (social)") ~ "Social housing",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(tenure_cat))  # remove rows where tenure was not defined

# Categorize total_floor_area before joining
mcmc_data <- mcmc_data %>%
  mutate(
    total_floor_area_bin = case_when(
      TOTAL_FLOOR_AREA < 50 ~ "Less than 50 sqm",
      TOTAL_FLOOR_AREA >= 50 & TOTAL_FLOOR_AREA < 70 ~ "50 to 69 sqm",
      TOTAL_FLOOR_AREA >= 70 & TOTAL_FLOOR_AREA < 90 ~ "70 to 89 sqm",
      TOTAL_FLOOR_AREA >= 90 & TOTAL_FLOOR_AREA < 110 ~ "90 to 109 sqm",
      TOTAL_FLOOR_AREA >= 110 ~ "110 sqm or more",
      TRUE ~ NA_character_
    )
  )

# --- 1. Load necessary packages ---
if (!require("brms")) install.packages("brms")
library(brms)
library(dplyr)

# --- 2. Sample a small chunk from your harmonized data ---
set.seed(123)
model_data <- mcmc_data[sample(nrow(mcmc_data), 10000), ]

# --- 3. Load probability table (one sheet with all categories + severity-adjusted probs) ---
library(readxl)
prob_table <- read_excel("mcmc_probabilities.xlsx")
prob_table <- prob_table %>%
  mutate(
    feature = tolower(trimws(feature)),
    feature_value = tolower(trimws(feature_value))
  )

# --- 4. Match on keys and get severity-adjusted probabilities ---

# Check if column names are harmonized
names(prob_table) <- tolower(gsub(" ", "_", names(prob_table)))

# Merge with MCMC features one by one (by multiple keys)
keys <- c(
  "epc_rating_band", "urban_rural_simplified", "region_name",
  "dwelling_type_mapped", "dwelling_age_mapped", "total_floor_area_bin",
  "fuel_category", "wall_insulation_cat", "tenure_cat"
)

# Convert keys in model_data to lower-case for join compatibility
model_data_join <- model_data
for (k in keys) {
  model_data_join[[k]] <- tolower(as.character(model_data_join[[k]]))
}

# Join using keys (assuming prob_table has a "feature" and "category" column)
combined <- model_data_join

for (feature in keys) {
  temp_prob <- prob_table %>%
    filter(feature == !!feature) %>%
    select(feature_value, severity_adjusted_probability)
  
  names(temp_prob) <- c(feature, paste0(feature, "_prob"))
  
  # Merge this feature's probability into the dataset
  combined <- left_join(combined, temp_prob, by = feature)
}

# --- 5. Combine the individual probabilities (geometric mean) ---
prob_cols <- grep("_prob$", names(combined), value = TRUE)
combined$combined_prob <- apply(combined[ , prob_cols], 1, function(x) {
  if (any(is.na(x))) return(NA)
  exp(mean(log(x)))
})

# --- 6. Generate synthetic target using MCMC prior ---
combined$fuel_poor <- rbinom(nrow(combined), 1, combined$combined_prob)

# Keep only complete cases
model_data <- combined %>% filter(!is.na(fuel_poor))
model_data$fuel_poor <- factor(model_data$fuel_poor, levels = c(0, 1))

# --- 7. Ensure all categorical vars are factors ---
cat_vars <- keys
model_data[cat_vars] <- lapply(model_data[cat_vars], factor)

# --- 8. Fit Bayesian model using {brms} ---
library(cmdstanr)
check_cmdstan_toolchain()
cmdstanr::install_cmdstan()

library(brms)
library(posterior)



formula <- bf(
  fuel_poor ~ epc_rating_band + urban_rural_simplified + region_name +
    dwelling_type_mapped + dwelling_age_mapped +
    total_floor_area_bin + fuel_category + wall_insulation_cat +
    tenure_cat,
  family = bernoulli()
)

fit_bayes <- brm(
  formula = formula,
  data = model_data,
  chains = 2,
  iter = 1000,
  warmup = 500,
  cores = 2,
  seed = 42
)

# --- 9. Inspect ---
print(summary(fit_bayes))
plot(fit_bayes)
pp_check(fit_bayes)
bayesplot::mcmc_rhat(rhat(fit_bayes))

# --- 10. Save the model ---

posterior_linpred(fit_bayes, transform = TRUE)  # Probabilities
saveRDS(fit_bayes, "bayesian_fuel_poverty_model.rds")

library(brms)

# --- Step 1: Get predicted probabilities from fitted model ---
# This gives a matrix: rows = observations, cols = posterior draws
pred_probs <- posterior_epred(fit_bayes)

# --- Step 2: Take the mean predicted probability per observation ---
mean_probs <- colMeans(pred_probs)  # length = nrow(model_data)

library(ggplot2)

ggplot(data.frame(mean_probs = mean_probs), aes(x = mean_probs)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = median(mean_probs)), linetype = "dashed", color = "red") +
  labs(title = "Distribution of Mean Predicted Probabilities",
       x = "Mean Predicted Probability",
       y = "Frequency")

quantile(mean_probs, probs = c(0.25, 0.5, 0.75, 0.9, 0.95))


# Set thresholds using your quantile output
thresholds <- c(-Inf, 0.00004, 0.0376, 0.0619, 0.0791, Inf)

# Assign ordinal risk categories
model_data$predicted_class <- cut(
  mean_probs,
  breaks = thresholds,
  labels = c("Very Low", "Low", "Medium", "High", "Very High"),
  right = TRUE,
  include.lowest = TRUE
)

# View class distribution
table(model_data$predicted_class)

# Ensure LMK_KEY is character for both datasets
model_data$LMK_KEY <- as.character(model_data$LMK_KEY)
ml_data$LMK_KEY    <- as.character(ml_data$LMK_KEY)

# Join only predicted_class from model_data
ml_model_data <- ml_data %>%
  left_join(model_data %>% select(LMK_KEY, predicted_class), by = "LMK_KEY")

# Optional: check distribution of classes
table(ml_model_data$predicted_class, useNA = "ifany")

ml_model_data <- ml_model_data %>%
  filter(complete.cases(.))
# Save the final dataset with predictions
write_parquet(ml_model_data, "ml_model_data_with_predictions.parquet")
write_csv(ml_model_data, "ml_model_data_with_predictions.csv")
