library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(data.table)

total_population <- read_excel("mid_2024_population.xlsx", sheet = "MYE2 - Persons")%>%
  select(local_authority = `Name`,local_authority_code = `Code`, population_2024 = `All ages`,child_population,senior_population,working_age_population)
                        
area <- read.csv("area_lad_2023.csv")%>%
  select(local_authority = `LAD23NM`,local_authority_code = `LAD23CD`, area_hectre = `AREALHECT`)

household_size <- read_excel("2018basedhhpsprincipalprojection.xlsx", 
                                               sheet = "427") %>%
  select(local_authority=`Area name`,local_authority_code = `Area code`, average_household_size_2023 = `Average household size 2023`)

population_data <- reduce(
  list(total_population,area,household_size),
  full_join,
  by = c("local_authority_code", "local_authority")
)

population_data <- population_data %>%
  mutate(
    area_km= area_hectre / 100,
    population_density = population_2024 / area_km,
    age_dependency_ratio = (child_population + senior_population) / working_age_population)

