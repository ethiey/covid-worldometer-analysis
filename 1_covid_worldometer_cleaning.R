library(tidyverse) #load tidyverse
sessionInfo() #confirm loaded packages
library(readr) #load readr
worldometer_data <- read_csv("worldometer_data.csv") #import dataset
glimpse(worldometer_data) #look at dataset
head(worldometer_data) #check 1st few rows


library(janitor) #load janitor
worldometer_data <- worldometer_data |> clean_names() #clean column names
colnames(worldometer_data)

worldometer_data |> 
  summarise(across(everything(), ~ sum(is.na(.)))) #find NA values

worldometer_data <- worldometer_data |> #replace NA with 0 assuming no reported cases
  mutate(across(c(new_cases, new_deaths, new_recovered, total_recovered, 
                  active_cases, serious_critical), ~ replace_na(., 0)))

worldometer_data <- worldometer_data |> #replace with median for 1m_pop columns to prevent skewing data
  mutate(across(c(tot_cases_1m_pop, deaths_1m_pop, tests_1m_pop), 
                ~ replace_na(., median(., na.rm = TRUE))))

worldometer_data |> 
  summarise(across(everything(), ~ sum(is.na(.)))) #find remaining NA values

worldometer_data |>  #check for duplicate countries
  count(country_region, sort = TRUE) |> 
  filter(n > 1)

write_csv(worldometer_data, "worldometer_clean.csv") #save clean dataset
