library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(haven)
library(data.table)
library(plyr)
library(ggplot2)
library(stringr)
library(xtable)
library(tibble)
library(tidyr)
library(sf)
library(collapse)
library(tidyverse)
library(writexl)
install.packages(c("rnaturalearth", "rnaturalearthdata"))
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(readr)


## Change direction to your local directory
setwd("C:/Users/CerenHüryolJIMFounda/OneDrive - Stichting Joint Impact Model/Documents/JIM Outputs")

input_file <- "member_project_output.xlsx"
member_name <- "Member A"



table_sheet <- readxl::read_excel(input_file, sheet = "Table")
general <- readxl::read_excel(input_file, sheet = "General")
ghg <- readxl::read_excel(input_file, sheet = "GHG")
employment <- readxl::read_excel(input_file, sheet = "Employment")
value_added <- readxl::read_excel(input_file, sheet = "Value Added")



## 1. Clean Data  


general <- general %>%
  clean_names()

general <- general %>%
  mutate(client_name_code = as.character(client_name_code))

general <- general %>%
  mutate(across(c(revenue, total, outstanding_amount, attributed_total_outstanding),
                 ~as.numeric(str_replace(., ",", "."))))


general <- general %>%
  filter(country_name != "Bangladesh")

general <- general %>%
  filter(economic_activity != "Paddy rice")


ghg_clean <- ghg %>%
  clean_names() %>%
  filter(
    !str_detect(str_to_lower(sub_indicator), "other"),
    !str_detect(str_to_lower(sub_indicator), "removal")
  )

ghg <- ghg %>%
   mutate(across(c(revenue, total, outstanding_amount, attributed_total_outstanding),
                 ~as.numeric(str_replace(., ",", "."))))


ghg <- ghg %>%
  mutate(client_name_code = as.character(client_name_code))

ghg <- ghg %>%
   filter(country_name != "Bangladesh")
 
 ghg <- ghg %>%
   filter(economic_activity != "Paddy rice")

employment <- employment %>%
  clean_names()

employment <- employment %>%
  mutate(client_name_code = as.character(client_name_code))

employment <- employment %>%
  mutate(across(c(revenue, total, outstanding_amount, attributed_total_outstanding),
                 ~as.numeric(str_replace(., ",", "."))))


employment <- employment %>%
  filter(country_name != "Bangladesh")

employment <- employment %>%
  filter(economic_activity != "Paddy rice")


value_added <- value_added %>%
  clean_names()

value_added <- value_added %>%
   mutate(across(c(revenue, total, outstanding_amount, attributed_total_outstanding),
                 ~as.numeric(str_replace(., ",", "."))))


value_added <- value_added %>%
  mutate(client_name_code = as.character(client_name_code))

value_added <- value_added %>%
   filter(country_name != "Bangladesh")
 
 value_added <- value_added %>%
   filter(economic_activity != "Paddy rice")




## 2. GHG

### Aggregate emissions by country

   country_emissions <- ghg %>%
   group_by(country_name, iso_alpha3_code) %>%
   summarise(total_emissions = sum(total, na.rm = TRUE), .groups = "drop") %>%
   arrange(desc(total_emissions))

 # world map
 world <- ne_countries(scale = "medium", returnclass = "sf")
 
 # join with map
 map_data <- world %>%
   left_join(country_emissions, by = c("name" = "country_name"))

# Africa map
# africa_map <- ne_countries(continent = "Africa", returnclass = "sf")
# map_data <- africa_map %>% 
# left_join(country_emissions, by = c("iso_a3" = "iso_alpha3_code"))
 
 # plot
 ggplot(map_data) +
   geom_sf(aes(fill = total_emissions)) +
   scale_fill_viridis_c(na.value = "grey90") +
   labs(title = "GHG Emissions by Country",
        fill = "Total emissions") 


 ghg <- ghg %>%
  mutate(
    gas_type = case_when(
      grepl("Non-CO2", sub_indicator, ignore.case = TRUE) ~ "Non-CO2",
      grepl("CO2", sub_indicator, ignore.case = TRUE) ~ "CO2"
    ),
    emission_category = case_when(
      grepl("Scope 1", sub_indicator, ignore.case = TRUE) ~ "Scope 1",
      grepl("Scope 2", sub_indicator, ignore.case = TRUE) ~ "Scope 2",
      grepl("Scope 3", sub_indicator, ignore.case = TRUE) ~ "Scope 3"
      )
    )



## 2.1  Aggregations 

ghg_by_scope <- ghg %>%
  group_by(scope, sub_scope, emission_category, gas_type) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop")

ghg_by_country <- ghg %>%
  group_by(country_name, iso_alpha3_code) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg))

ghg_by_sector <- ghg %>%
  group_by(economic_activity) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg))

top_contributors <- ghg %>%
  group_by(country_name, economic_activity, sub_indicator) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg)) %>%
  slice_head(n = 10)



kpis <- tibble::tibble(
  metric = c(
    "Total GHG",
    "Attributed GHG",
    "GHG per revenue",
    "GHG per outstanding amount"
  ),
  value = c(
    sum(ghg$total, na.rm = TRUE),
    sum(ghg$attributed_total, na.rm = TRUE),
    sum(ghg$total, na.rm = TRUE) / sum(ghg$revenue, na.rm = TRUE),
    sum(ghg$total, na.rm = TRUE) / sum(ghg$outstanding_amount, na.rm = TRUE)
  )
)


## 2.2 Visualize the prepared data - Totals 

ggplot(analysis$by_scope, aes(
  x = emission_scope,
  y = attributed_ghg,
  fill = gas_type
)) +
  geom_col() +
  labs(
    title = "Attributed GHG by Scope",
    x = NULL,
    y = "Attributed GHG",
    fill = "Gas type"
  ) +
  theme_minimal()

 ## Analysis by sub-scope and scope 

analysis$by_scope_subscope %>%
  mutate(scope_label = paste(scope, sub_scope, emission_scope, gas_type, sep = " | ")) %>%
  ggplot(aes(
    x = reorder(scope_label, attributed_ghg),
    y = attributed_ghg
  )) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Attributed GHG by Scope and Sub-scope",
    x = NULL,
    y = "Attributed GHG"
  ) +
  theme_minimal()



## 3. Employment 


employment_total <- employment %>%
  mutate(
    total = as.numeric(gsub(",", ".", total))
  ) %>%
  filter(sub_indicator == "Total")   # avoid double counting



  emp_summary <- employment_total %>%
  group_by(
    country_name,
    iso_alpha3_code,
    economic_activity,
    sub_scope
  ) %>%
  summarise(
    jobs = sum(total, na.rm = TRUE),
    .groups = "drop"
  )



