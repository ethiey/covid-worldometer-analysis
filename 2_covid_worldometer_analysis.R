library(tidyverse) #load libraries
library(readr) #load readr
worldometer_clean <- read_csv("worldometer_clean.csv") #import dataset
glimpse(worldometer_clean) #view dataset

#summary statistics
summary(worldometer_clean)

#world summary statistics 
world_summary <- worldometer_clean |>
  summarise(
    total_new_cases = sum(new_cases, na.rm = TRUE),
    total_new_deaths = sum(new_deaths, na.rm = TRUE),
    total_new_recovered = sum(new_recovered, na.rm = TRUE),
    total_cases = sum(total_cases, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_recovered = sum(total_recovered, na.rm = TRUE),
    World_CFR = mean((total_deaths / total_cases) * 100, na.rm = TRUE), #Case fatality rate
  )

print(world_summary)

# Other population statistics for each country
# Mortality rate, recovery rate and active case ratio)
worldometer_clean <- worldometer_clean |> 
  mutate (CFR = (total_deaths / total_cases) * 100,  
          mortality_rate_per_million = (total_deaths / population) * 1e6,  
          recovery_rate = (total_recovered / total_cases) * 100,
          active_case_ratio = (active_cases / total_cases) * 100)

#Top 5 counties total cases per 1m pop cases
top_cases <- worldometer_clean |> arrange(desc(tot_cases_1m_pop)) |> 
  select(country_region, tot_cases_1m_pop, total_cases) |>
slice(1:5)

print(top_cases)

#top 5 countries testing per 1m pop
top_test <- worldometer_clean |> arrange(desc(tests_1m_pop)) |>
  select(country_region, tests_1m_pop, total_tests) |> slice(1:5)

print(top_test)

#correlation between testing and cases 
cor_case_test <- cor(worldometer_clean$tot_cases_1m_pop, 
                        worldometer_clean$tests_1m_pop, 
                        use = "complete.obs")
print(cor_case_test)
# r value is 0.305 = weak positive correlation, 
#testing somewhat associated with higher case detection

#countries with high increase of new cases relative to total cases (fast covid spread)
worldometer_clean <- worldometer_clean |> #growth rate
  mutate (growth_rate = (new_cases/total_cases) * 100 )
top_countries_spread <- worldometer_clean |> #top 5 rapid spread countries
  arrange(desc(growth_rate)) |> 
  select(country_region, total_cases, new_cases, growth_rate) |> 
  slice(1:5)

print(top_countries_spread)

#comparing recovery rate and CFR
cor_recovery_CFR <- cor(worldometer_clean$recovery_rate, worldometer_clean$CFR,
                        use = "complete.obs")

print(cor_recovery_CFR)
#r value is -o.189 = weak negative correlation
#as recovery rate increases, CFR decreases 
#however other factors are more likely to influence CFR and recovery


write_csv(worldometer_clean, "processed_worldometer_data.csv") #new dataset
