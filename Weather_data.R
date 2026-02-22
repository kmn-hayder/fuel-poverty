library(readxl)
library(readr)
library(dplyr)
library(purrr)

hdd <-read.csv("hdd.csv") %>%
  select(local_authority=`NAME`,local_authority_code = `CODE`, hdd_01_20_median = `HDD.2001.2020.median`)

tas <-read.csv("tas.csv")%>%
  select(local_authority=`NAME`,local_authority_code = `CODE`, 
         tas_winter_01_20_median = `TAS.Winter.2001.2020.median`,
         tas_winter_1.5_median = `TAS.Winter.1.5.C.median`,
         tas_winter_2_median = `TAS.Winter.2.C.median`,
         tas_winter_2.5_median = `TAS.Winter.2.5.C.median`,
         tas_winter_5.5_median = `TAS.Winter.1.5.C.median`,
         tas_winter_3.5_median = `TAS.Winter.3.5.C.median`,
         tas_winter_4_median = `TAS.Winter.4.C.median`
         )
tasmin <-read.csv("TASMIN.csv") %>%
  select(local_authority=`NAME`,local_authority_code = `CODE`, min_temp_01_20_median = `TASMIN.Winter.2001.2020.median`)

weather_data <- reduce(
  list(hdd,tas,tasmin),
  full_join,
  by = c("local_authority_code", "local_authority")
)

