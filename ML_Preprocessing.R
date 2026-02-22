library(arrow)
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)
library(corrplot)
library(gridExtra)
library(smotefamily)
library(fastDummies)
install.packages("themis")
library(themis)
library(recipes)

# Load your data
df <- read_parquet("ml_model_data_with_predictions.parquet")
# Rename selected columns to lowercase
df <- df %>%
  rename(
    current_energy_efficiency = CURRENT_ENERGY_EFFICIENCY,
    energy_consumption_current = ENERGY_CONSUMPTION_CURRENT,
    co2_emissions_current = CO2_EMISSIONS_CURRENT,
    built_form= BUILT_FORM,
  )

df <- df %>%
  rename(
    elec_price_kwh = electricity_avg_variable_per_kWh_price,
    hh_winter_fuel_payment_2023 = num_households_received_winter_fuel_payment_2023,
    fuel_bill_individual_est = avg_fuel_bill_annual_individual_estimate,
    la_elec_use_kwh_hh = local_authority_mean_domessic_electricity_consumption_kWh_per_household,
    la_gas_use_kwh_meter = local_authority_mean_domestic_gas_consumption_kWh_per_meter,
    fuel_bill_local_avg_est = avg_fuel_bill_annual_local_average_estimate
  )

# Convert to factor
df$predicted_class <- factor(df$predicted_class)

# Class distribution
table(df$predicted_class)
prop.table(table(df$predicted_class))

# Bar plot
ggplot(df, aes(x = predicted_class, fill = predicted_class)) +
  geom_bar() +
  labs(title = "Distribution of Target Variable", x = "Class", y = "Count") +
  theme_minimal()

# Check missing values
missing_summary <- colSums(is.na(df))
missing_summary[missing_summary > 0]


# Select numeric columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]

# Summary statistics for numeric columns
summary_stats <- summary(df_numeric)
# Print summary statistics
print(summary_stats)

#=======Remedies based on summary stats=======
# log-transform energy_per_room
df <- df %>%
  mutate(log_energy_per_room = log(energy_per_room + 1))

# Remove `fuel_bill_difference`
df <- df %>% select(-fuel_bill_difference)
df <- df %>% select(-local_authority.x)

# Cap `floor_area_per_room` to 99th percentile and remove outliers
floor_cap <- quantile(df$floor_area_per_room, 0.99, na.rm = TRUE)
df <- df %>%
  filter(floor_area_per_room <= floor_cap)

# Cap `fuel_cost_ratio` to 99th percentile and remove extreme values
fuel_ratio_cap <- quantile(df$fuel_cost_ratio, 0.99, na.rm = TRUE)
df <- df %>%
  filter(fuel_cost_ratio <= fuel_ratio_cap)

# Log-transform `fuel_bill_individual_est`
df <- df %>%
  mutate(log_fuel_bill_individual_est = log(fuel_bill_individual_est + 1))  # +1 to avoid log(0)


# List of population-related columns
population_vars <- c(
  "population_2024",
  "child_population",
  "senior_population",
  "working_age_population"
)

# Loop to filter rows beyond the 95th percentile for any of these columns
for (var in population_vars) {
  cap <- quantile(df[[var]], 0.95, na.rm = TRUE)
  df <- df %>% filter(.data[[var]] <= cap)
}

# Drop extra TAS winter scenario columns (keep only 1.5 and 3.5)
tas_cols_to_keep <- c("tas_winter_1.5_median", "tas_winter_3.5_median")
tas_cols_to_drop <- grep("^tas_winter_.*_median$", names(df), value = TRUE)
tas_cols_to_drop <- setdiff(tas_cols_to_drop, tas_cols_to_keep)
df <- df %>% select(-all_of(tas_cols_to_drop))


#========= EDA: Histograms and Correlation Matrix ==========

library(ggplot2)
library(gridExtra)

# Ensure output folder exists
if (!dir.exists("eda_histograms")) dir.create("eda_histograms")

# Select numeric columns
numeric_cols <- df[sapply(df, is.numeric)]
var_names <- names(numeric_cols)

# Batch settings
batch_size <- 9
num_batches <- ceiling(length(var_names) / batch_size)

# Loop through batches
for (i in seq_len(num_batches)) {
  cat("Saving histogram batch", i, "of", num_batches, "\n")
  
  start <- (i - 1) * batch_size + 1
  end <- min(i * batch_size, length(var_names))
  vars_subset <- var_names[start:end]
  
  # Generate histogram plots
  plots <- lapply(vars_subset, function(v) {
    ggplot(df, aes(x = .data[[v]])) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(x = v, y = "Count") +
      theme_minimal()
  })
  
  # File name
  file_path <- paste0("eda_histograms/hist_batch_", i, ".png")
  
  # Save the grid of plots
  png(file_path, width = 1600, height = 1600, res = 200)
  grid.arrange(grobs = plots, ncol = 3)
  dev.off()
}


# Correlation matrix
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
write.csv(cor_matrix, "correlation_matrix.csv")

# Plot correlation matrix
# Define 15 selected variables
vars_selected <- c(
  "current_energy_efficiency",
  "fuel_cost_ratio",
  "fuel_bill_individual_est",
  "income_ahc_2024",
  "imd_avg_2019",
  "perc_low_income_households",
  "tas_winter_3.5_median",
  "elec_price_kwh",
  "insulation_score",
  "hdd_01_20_median",
  "senior_population",
  "average_household_size_2023",
  "total_floor_area",
  "floor_area_per_room",
  "gas_avg_variable_per_kWh_price"
)

# Subset and compute correlation
cor_subset <- df %>%
  dplyr::select(all_of(vars_selected)) %>%
  cor(use = "pairwise.complete.obs")

# Create corrplot
corrplot(cor_subset,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7,
         tl.col = "black",
         addgrid.col = "black",  # thinner and softer grid
         col = colorRampPalette(c("maroon", "white", "navy"))(200))  # your palette

#========= Real preprocessing begins=========
df <- df %>%
  dplyr::select(
    -postcode_district,
    -local_authority_code.x,
    -LMK_KEY               
  )
df <- df %>%
  dplyr::select(
    -LODGEMENT_DATE,
  )
# Convert categorical variables to factors
# Identify character columns
char_vars <- names(df)[sapply(df, is.character)]

# Convert character columns to factor in a loop
df[char_vars] <- lapply(df[char_vars], as.factor)

# Ensure target is a factor
df$predicted_class <- as.factor(df$predicted_class)

# Step 1: Separate target before encoding
target <- df$predicted_class
df_nontarget <- df %>% dplyr::select(-predicted_class)

# Step 2: Dummy encode only predictors
df_encoded <- fastDummies::dummy_cols(df_nontarget,
                                      remove_first_dummy = TRUE,
                                      remove_selected_columns = TRUE)

# Step 3: Reattach the target column
df_ready <- cbind(df_encoded, predicted_class = target)

rec <- recipe(predicted_class ~ ., data = df_ready) %>%
  step_smote(predicted_class) %>%
  prep()

df_balanced <- juice(rec)

# Check distribution of the target variable
table(df_balanced$predicted_class)

# View as proportions (optional)
prop.table(table(df_balanced$predicted_class))
