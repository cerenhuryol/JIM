library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(haven)
library(data.table)
library(ggplot2)
library(stringr)
library(xtable)
library(tibble)
library(tidyr)
library(sf)
library(collapse)
library(tidyverse)
library(writexl)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(readr)
library(scales)

## Change direction to your local directory
setwd("C:/Users/CerenHüryolJIMFounda/OneDrive - Stichting Joint Impact Model/Documents/JIM Outputs")

input_file <- "member_project_output.xlsx"
member_name <- "Member A"

jim_colours <- c("#53C6B9", "#005A81", "#08306B")

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

ghg <- ghg %>%
  clean_names()

ghg_clean <- ghg %>%
  mutate(
    sub_indicator = as.character(sub_indicator),
    scope = as.character(scope),
    sub_scope = as.character(sub_scope),
    country_name = as.character(country_name),
    economic_activity = as.character(economic_activity),
    total = as.numeric(total)
  ) %>%
  filter(
    !str_detect(str_to_lower(sub_indicator), "other"),
    !str_detect(str_to_lower(sub_indicator), "removal")
  ) %>%
  mutate(
    emission_scope = case_when(
      str_detect(str_to_lower(sub_indicator), "scope 1") ~ "Scope 1",
      str_detect(str_to_lower(sub_indicator), "scope 2") ~ "Scope 2",
      str_detect(str_to_lower(sub_indicator), "scope 3") ~ "Scope 3",
      TRUE ~ "Unclassified"
    ),
    gas_type = case_when(
      str_detect(str_to_lower(sub_indicator), "non-co2") ~ "Non-CO2",
      str_detect(str_to_lower(sub_indicator), "co2") ~ "CO2",
      TRUE ~ "Unclassified"
    )
  )

ghg_clean <- ghg_clean %>%
   filter(country_name != "Bangladesh")
 
 ghg_clean <- ghg_clean %>%
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


## 2. General

  top_countries <- general %>%
  group_by(country_name) %>%
  summarise(total = sum(outstanding_amount, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 7) %>%
  pull(country_name)

top_sectors <- general %>%
  group_by(economic_activity) %>%
  summarise(total = sum(outstanding_amount, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 7) %>%
  pull(economic_activity)

## Sometimes you need shorter names!
general <- general %>%
  mutate(
    country_name = recode(
      country_name,
      "Democratic Republic of the Congo" = "DRC"
    )
  )

## Prep for graph
top_country_sector_g <- general %>%
  group_by(country_name) %>%
  mutate(country_total = sum(outstanding_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(country_name %in% (
    general %>%
      group_by(country_name) %>%
      summarise(country_total = sum(outstanding_amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(country_total)) %>%
      slice_head(n = 7) %>%
      pull(country_name)
  )) %>%
  group_by(country_name, economic_activity, country_total) %>%
  summarise(sector_amount = sum(outstanding_amount, na.rm = TRUE), .groups = "drop") %>%
  group_by(country_name) %>%
  slice_max(sector_amount, n = 1, with_ties = FALSE) %>%
  mutate(
    sector_share = sector_amount / country_total,
    label = paste0(
      economic_activity, "\n",
      percent(sector_share, accuracy = 1)
    )
  ) %>%
  ungroup()

## Visualize
ggplot(top_country_sector_g,
       aes(x = reorder(country_name, country_total), y = country_total, fill = economic_activity)) +
  geom_col() +
  geom_text(
    aes(label = label),
    hjust = -0.05,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.35))
  ) +
  scale_fill_manual(
    values = c("#53C6B9", "#005A81", "#08306B")
  )+
  labs(
    title = "Top 7 countries by exposure and their largest sector",
    x = NULL,
    y = "Outstanding amount",
    fill = "Largest sector"
  ) +
  theme_minimal()


  

## 3. GHG

### Aggregate emissions by country

   country_emissions <- ghg_clean %>%
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




## 2.1  Aggregations 

ghg_by_scope <- ghg_clean %>%
  group_by(scope, sub_scope, emission_category, gas_type) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop")

ghg_by_country <- ghg_clean %>%
  group_by(country_name, iso_alpha3_code) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg))

ghg_by_sector <- ghg_clean %>%
  group_by(economic_activity) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg))

top_contributors <- ghg_clean %>%
  group_by(country_name, economic_activity, sub_indicator) %>%
  summarise(total_ghg = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ghg)) %>%
  slice_head(n = 10)





top_7_countries_ghg <- ghg_clean %>%
  group_by(country) %>%
  summarise(
    total_ghg = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_ghg)) %>%
  slice_head(n = 7)

country_ghg_chart <- ggplot(top_7_countries_ghg, aes(
  x = reorder(country, total_ghg),
  y = total_ghg
)) +
  geom_col(fill = jim_colours[2]) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 7 Countries by GHG Emissions",
    x = NULL,
    y = "GHG emissions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = jim_colours[3]),
    axis.text = element_text(color = jim_colours[3]),
    axis.title = element_text(color = jim_colours[3])
  )

country_ghg_chart





top_7_sectors_ghg <- ghg_clean %>%
  group_by(economic_activity) %>%
  summarise(
    total_ghg = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_ghg)) %>%
  slice_head(n = 7)

sector_ghg_chart <- ggplot(top_7_sectors_ghg, aes(
  x = reorder(economic_activity, total_ghg),
  y = total_ghg
)) +
  geom_col(fill = jim_colours[1]) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 7 Sectors by GHG Emissions",
    x = NULL,
    y = "GHG emissions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = jim_colours[3]),
    axis.text = element_text(color = jim_colours[3]),
    axis.title = element_text(color = jim_colours[3])
  )

sector_ghg_chart

##################################################################################
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

##################################################################################################


scope_activity_summary <- ghg_clean %>%
  mutate(
    scope_group = case_when(
      emission_scope %in% c("Scope 1", "Scope 2") ~ "Scope 1 & 2",
      emission_scope == "Scope 3" ~ "Scope 3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(scope_group)) %>%
  group_by(economic_activity, scope_group) %>%
  summarise(
    total_ghg = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(economic_activity) %>%
  mutate(
    activity_total_ghg = sum(total_ghg, na.rm = TRUE),
    percentage_share = total_ghg / activity_total_ghg
  ) %>%
  ungroup() %>%
  arrange(desc(activity_total_ghg), economic_activity, scope_group)

scope_activity_summary


jim_colours_scope <- c(
  "Scope 1 & 2" = "#005A81",
  "Scope 3" = "#53C6B9"
)


## Visualize 
scope_activity_chart <- scope_activity_summary %>%
  ggplot(aes(
    x = reorder(economic_activity, activity_total_ghg),
    y = total_ghg,
    fill = scope_group
  )) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = jim_colours_scope) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Scope 1 & 2 vs Scope 3 Emissions by Economic Activity",
    x = NULL,
    y = "GHG emissions",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "#08306B"),
    axis.text = element_text(color = "#08306B"),
    axis.title = element_text(color = "#08306B")
  )

scope_activity_chart






## Stacked bar


scope_activity_share_chart <- scope_activity_summary %>%
  ggplot(aes(
    x = reorder(economic_activity, activity_total_ghg),
    y = percentage_share,
    fill = scope_group
  )) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = jim_colours_scope) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Percentage Share of Scope 1 & 2 vs Scope 3 by Economic Activity",
    x = NULL,
    y = "Percentage share",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "#08306B"),
    axis.text = element_text(color = "#08306B"),
    axis.title = element_text(color = "#08306B")
  )

scope_activity_share_chart




##

df_top10_scopes <- ghg_clean %>%
  mutate(
    scope_group = case_when(
      str_detect(str_to_lower(sub_indicator), "scope 1") ~ "Scope 1",
      str_detect(str_to_lower(sub_indicator), "scope 2") ~ "Scope 2",
      str_detect(str_to_lower(sub_indicator), "scope 3") &
        str_detect(str_to_lower(sub_indicator), "upstream") ~ "Scope 3 Upstream",
      str_detect(str_to_lower(sub_indicator), "scope 3") &
        str_detect(str_to_lower(sub_indicator), "downstream") ~ "Scope 3 Downstream",
      str_detect(str_to_lower(sub_indicator), "scope 3") &
        str_detect(str_to_lower(sub_indicator), "imports") ~ "Scope 3 Upstream",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(scope_group)) %>%
  group_by(economic_activity, scope_group) %>%
  summarise(
    emissions = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(economic_activity) %>%
  mutate(total_sector_emissions = sum(emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    economic_activity %in%
      unique(
        arrange(
          distinct(., economic_activity, total_sector_emissions),
          desc(total_sector_emissions)
        )$economic_activity
      )[1:10]
  )

ggplot(df_top10_scopes, aes(x = emissions, y = economic_activity, fill = scope_group)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Scope 1" = "#53C6B9",
      "Scope 2" = "#9ecae1",
      "Scope 3 Upstream" = "#005A81",
      "Scope 3 Downstream" = "#08306B"  # darker blue
    )
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(
    title = "Top 10 sectors by total GHG emissions",
    x = "Total emissions",
    y = "Sector",
    fill = "Scope"
  ) +
  theme_minimal()


## 4. Employment 


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



