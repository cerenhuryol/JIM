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



setwd("C:/Users/CerenHüryolJIMFounda/OneDrive - Stichting Joint Impact Model/Documents/JIM Outputs")

input_file <- "member_project_output.xlsx"
member_name <- "Member A"



table_sheet <- readxl::read_excel(input_file, sheet = "Table")
general <- readxl::read_excel(input_file, sheet = "General")
ghg <- readxl::read_excel(input_file, sheet = "GHG")
employment <- readxl::read_excel(input_file, sheet = "Employment")
value_added <- readxl::read_excel(input_file, sheet = "Value Added")



## 1. Clean Data  

ghg <- ghg %>%
  clean_names()

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
   group_by(country_name) %>%
   summarise(total_emissions = sum(total, na.rm = TRUE), .groups = "drop") %>%
   arrange(desc(total_emissions))

 # world map
 world <- ne_countries(scale = "medium", returnclass = "sf")
 
 # join with map
 map_data <- world %>%
   left_join(country_emissions, by = c("name" = "country_name"))
 
 # plot
 ggplot(map_data) +
   geom_sf(aes(fill = total_emissions)) +
   scale_fill_viridis_c(na.value = "grey90") +
   labs(title = "GHG Emissions by Country",
        fill = "Total emissions") 
 


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



