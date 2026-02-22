install.packages("readODS")
library(readODS)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(data.table)

winter_fuel_payment <- read_ods("winter_fuel_payments_last_winter.ods", sheet="2_Local_Authority")
winter_fuel_payment <- winter_fuel_payment %>%
  select(local_authority = `Local Authority`, 
         num_households_received_winter_fuel_payment_2023 = `Total`)

fuel_bills <- epc_master %>%
  select(LMK_KEY,
         local_authority_code = LOCAL_AUTHORITY, 
         energy_consumption_current = ENERGY_CONSUMPTION_CURRENT, 
         total_floor_area = TOTAL_FLOOR_AREA,
         main_fuel= MAIN_FUEL)

local_authority_to_region <- read_excel("Local_Authority_to_Region.xlsx") %>%
  select(local_authority_code = `LAD23CD`, 
         local_authority = `LAD23NM`,
         region_code=`RGN23CD`,
         region_name=`RGN23NM`)

# Join fuel bills with local authority to region mapping
fuel_bills_24 <- fuel_bills %>%
  left_join(local_authority_to_region, by = "local_authority_code")
rm(fuel_bills)

# Load average electricity and gas bills for 2024/25

avg_electricity_bill_24 <- read_excel("Average unit costs and fixed charges for electricity by UK regions.xlsx", 
                   sheet = "2.2.4 (Financial Year)") %>%
  filter(Year == "2024/25") %>%
  select(
    region_name = Region,
    electricity_avg_variable_per_kWh_price = `Overall: Average variable unit price (£/kWh)`,
    electricity_avg_fixed_cost_annual = `Overall: Average fixed cost (£/year)`
  )

avg_gas_bill_24 <- read_excel("Average unit costs and fixed charges for gas by GB regions.xlsx", 
                   sheet = "2.3.4 (Financial Year)") %>%
  filter(Year == "2024/25") %>%
  select(
    region_name = Region,
    gas_avg_variable_per_kWh_price = `Overall: Average variable unit price (£/kWh)[Note 1]`,
    gas_avg_fixed_cost_annual = `Overall: Average fixed cost (£/year)[Note 2]`
  )

# Join average electricity and gas bills with fuel bills
fuel_bills_24 <- fuel_bills_24 %>%
  left_join(avg_electricity_bill_24, by = "region_name") %>%
  left_join(avg_gas_bill_24, by = "region_name") %>%
  left_join(winter_fuel_payment, by = "local_authority")

sort(unique(fuel_bills_24$main_fuel))

fuel_bills_24 <- fuel_bills_24 %>%
  mutate(fuel_category = case_when(
    grepl("electricity", main_fuel, ignore.case = TRUE) ~ "Electricity",
    grepl("gas|lpg", main_fuel, ignore.case = TRUE) ~ "Gas",
    main_fuel %in% c("", "INVALID!", "NO DATA!") ~ NA_character_,
    TRUE ~ "Other"
  ))

# Calculate average annual fuel bills
fuel_bills_24 <- fuel_bills_24 %>%
  mutate(
    avg_fuel_bill_annual_individual_estimate = if_else(
      fuel_category == "Gas",
      (gas_avg_variable_per_kWh_price * energy_consumption_current * total_floor_area) + gas_avg_fixed_cost_annual,
      if_else(
        fuel_category == "Electricity",
        (electricity_avg_variable_per_kWh_price * energy_consumption_current * total_floor_area) + electricity_avg_fixed_cost_annual,
        NA_real_
      )
    )
  )

local_authority_average_electricity_consumption <- read_excel("Subnational_electricity_consumption_statistics_2005-2023.xlsx", 
                                                      sheet="2023") %>%
  select(local_authority_code = `Code`,
         local_authority_mean_domessic_electricity_consumption_kWh_per_household = `Mean_domestic_consumption_kWh_per_household`) 

local_authority_average_gas_consumption <- read_excel("Subnational_gas_consumption_statistics_2005-2023.xlsx",
                                                      sheet="2023") %>%
  select(local_authority_code = `Code`,
         local_authority_mean_domestic_gas_consumption_kWh_per_meter = `Mean_consumption_kWh_per_meter_Domestic`)

# Join average consumption data with fuel bills
fuel_bills_24 <- fuel_bills_24 %>%
  left_join(local_authority_average_electricity_consumption, by = "local_authority_code") %>%
  left_join(local_authority_average_gas_consumption, by = "local_authority_code")

fuel_bills_24 <- fuel_bills_24 %>%  mutate(
    avg_fuel_bill_annual_local_average_estimate = if_else(
      fuel_category == "Gas",
      (gas_avg_variable_per_kWh_price * local_authority_mean_domessic_electricity_consumption_kWh_per_household) + gas_avg_fixed_cost_annual,
      if_else(
        fuel_category == "Electricity",
        (electricity_avg_variable_per_kWh_price * local_authority_mean_domestic_gas_consumption_kWh_per_meter) + electricity_avg_fixed_cost_annual,
        NA_real_
      )
    )
  )

fuel_bills_24 <- fuel_bills_24 %>%
  mutate(
    fuel_bill_difference = avg_fuel_bill_annual_individual_estimate - avg_fuel_bill_annual_local_average_estimate
  )

# Filter to a reasonable range (e.g., -20,000 to 20,000)
diff_vals <- fuel_bills_24$fuel_bill_difference
diff_vals <- diff_vals[diff_vals >= -2000 & diff_vals <= 2000]

# Basic density plot
plot(
  density(diff_vals, na.rm = TRUE),
  main = "Distribution of Fuel Bill Difference",
  xlab = "Individual - Local Average (£)",
  col = "darkgreen",
  lwd = 2
)







