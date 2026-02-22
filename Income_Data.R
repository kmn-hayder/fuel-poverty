library(readxl)
library(readr)
library(dplyr)
library(purrr)

# Load adjustment table
income_adj <- read.csv("income_temporal_adjustment_data.csv") %>%
  select(`local.authority..district...unitary..as.of.April.2023.`, 'Adjustment_Factor_20_24') %>%
  rename(local_authority = `local.authority..district...unitary..as.of.April.2023.`) %>%
  mutate(
    adjustment_factor = as.numeric(Adjustment_Factor_20_24),
  )

# Load main sheet with 'Net annual income'
net_income <- read_excel("IncomeData2020.xlsx", sheet = "Net annual income") %>%
  select(local_authority = `Local authority name`,local_authority_code = `Local authority code`, net_income_2020 = `Net annual income (£)`)

# Load sheet: 'Net income before housing costs'
bhc_income <- read_excel("IncomeData2020.xlsx", sheet = "Net income before housing costs") %>%
  select(local_authority = `Local authority name`,local_authority_code = `Local authority code`, income_bhc_2020 = `Net annual income before housing costs (£)`)

# Load sheet: 'Net income after housing costs'
ahc_income <- read_excel("IncomeData2020.xlsx", sheet = "Net income after housing costs") %>%
  select(local_authority = `Local authority name`,local_authority_code = `Local authority code`, income_ahc_2020 = `Net annual income after housing costs (£)`)

net_income_agg <- net_income %>%
  group_by(local_authority_code,local_authority) %>%
  summarise(net_income_avg = mean(`net_income_2020`, na.rm = TRUE))

bhc_income_agg <- bhc_income %>%
  group_by(local_authority_code,local_authority) %>%
  summarise(bhc_income_avg = mean(`income_bhc_2020`, na.rm = TRUE))

ahc_income_agg <- ahc_income %>%
  group_by(local_authority_code,local_authority) %>%
  summarise(ahc_income_avg = mean(`income_ahc_2020`, na.rm = TRUE))

# Join all income columns
income_combined <- reduce(
  list(net_income_agg, bhc_income_agg, ahc_income_agg),
  full_join,
  by = c("local_authority_code", "local_authority")
)

# Join with adjustment factor
income_adjusted_2024 <- income_combined %>%
  left_join(income_adj, by = "local_authority") %>%
  mutate(
    net_income_2024 = as.numeric(net_income_avg) * adjustment_factor,
    income_bhc_2024 = as.numeric(bhc_income_avg) * adjustment_factor,
    income_ahc_2024 = as.numeric(ahc_income_avg) * adjustment_factor
  )

#Adding total household number to income_adjusted_2024

#Read the projection data from the "406" sheet
household_proj <- read_excel("2018basedhhpsprincipalprojection.xlsx", 
                             sheet = "406") %>%
  select(local_authority_code = `Area code`, total_households = `2024`) %>%
  mutate(total_households = as.numeric(total_households))

#Join with your existing data
income_adjusted_2024 <- income_adjusted_2024 %>%
  left_join(household_proj, by = "local_authority_code")

#Adding low-income households with children to income_adjusted_2024

# Read the CSV file
low_income_data <- read_excel("relative_perc_households_children.xlsx") %>%
  select(local_authority = local_authority, 
         num_low_income_households = `2023/24`) %>%
  mutate(num_low_income_households = parse_number(num_low_income_households))

# Join with income_adjusted_2024
income_adjusted_2024 <- income_adjusted_2024 %>%
  left_join(low_income_data, by = "local_authority")
#Some rows have missing low-incomre household data, so we will fill them with 
#an estimated value based on the 60% median of national median for 2024

national_median_ahc_income <- 29224 

poverty_threshold <- national_median_ahc_income * 0.6

income_adjusted_2024 <- income_adjusted_2024 %>%
  mutate(
    num_low_income_households = if_else(
      num_low_income_households == 0 & income_ahc_2024 < 29224,
      round(
        total_households * pmin(1, pmax(0, (29224 - income_ahc_2024) / (29224 - 13800)))
      ),
      num_low_income_households
    )
  )
income_adjusted_2024 <- income_adjusted_2024 %>%
  mutate(num_low_income_households = if_else(num_low_income_households == 0, NA_real_, num_low_income_households))

# Percentage of low income households
income_adjusted_2024 <- income_adjusted_2024 %>%
  mutate(
    perc_low_income_households = ifelse(
      !is.na(num_low_income_households) & !is.na(total_households) & total_households > 0,
      num_low_income_households / total_households,
      NA_real_
    )
  )

#Adding unemployment data
unemp_data <- read.csv("unemp_rate.csv") %>%
  select(local_authority, unemployment_rate) %>%
  mutate(unemployment_rate = as.numeric(unemployment_rate))

#Join with your existing data
income_adjusted_2024 <- income_adjusted_2024 %>%
  left_join(unemp_data, by = "local_authority")

#Add IMD data

imd_2019 <- read_excel("IMD2019.xlsx",sheet = "IMD") %>%
  select(
    local_authority_code = `Local Authority District code (2019)`,
    imd_avg_2019 = `IMD - Average score`
  )

income_adjusted_2024 <- income_adjusted_2024 %>%
  left_join(imd_2019, by = "local_authority_code")

#Add Urban/Rural Class

urban_rural_data <- read_excel("urban_rural.xlsx") %>%
  select(
    local_authority_code = LAD21CD,   # Rename during selection
    urban_rural = RUC21NM              # Keep classification
  )

income_adjusted_2024 <- income_adjusted_2024 %>%
  left_join(urban_rural_data, by = "local_authority_code")

income_adjusted_2024 <- income_adjusted_2024 %>%
  select(-urban_rural.x)  # or -urban_rural.y depending on which one you want to drop
