library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(reactable)
library(plotly)
library(viridis)
library(shinythemes)
library(htmltools)

# Load the multiperiod data
CPI.Time <- readxl::read_excel(path="app/data/CPI2022_GlobalResultsTrends.xlsx", sheet=2, skip=2) %>% clean_names()
# create a cleaner function to clean the data
cleaner <- function(data, string) {
  # Start with the data
  data |>
    # Use the iso3 as ID and keep everything that starts with string
    select(iso3, starts_with(string)) |>
    # pivot those variables except iso3
    pivot_longer(cols=-iso3,
                 # names_prefix needs to remove string_
                 names_prefix = paste0(string,"_",sep=""),
                 # make what's left of the names the year -- it will be a four digit year
                 names_to = "year",
                 # make the values named string
                 values_to=string)
}
# Clean the panel data
CPI.TS.Tidy <- cleaner(CPI.Time,"cpi_score")
Sources.TS.Tidy <- cleaner(CPI.Time,"sources")
StdErr.TS.Tidy <- cleaner(CPI.Time,"standard_error")
Rank.TS.Tidy <- cleaner(CPI.Time, "rank")
# Join together the panel data
Panel.FJ <- left_join(CPI.TS.Tidy, Sources.TS.Tidy) |> 
  left_join(StdErr.TS.Tidy) |> 
  left_join(Rank.TS.Tidy) |>
  mutate(year = as.integer(year)) |>
  filter(year > 2016)
rm(CPI.TS.Tidy, Sources.TS.Tidy, StdErr.TS.Tidy, Rank.TS.Tidy)

# Map Stuff

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join the data to the map
Names.Merge <- CPI.Time |> select(iso3, country_territory, region_un)
Panel <- left_join(Panel.FJ, Names.Merge) |> 
  relocate(where(is.character)) # |>
Map.Data <- merge(world, Panel, by.x="iso_a3", by.y= "iso3")
Map.Data <- Map.Data |> 
  mutate(tooltip = paste0(sovereignt,"<br>",year,"<br>CPI: ",cpi_score, "<br>Rank: ",rank, sep="")) |>
  rename(Rank = rank) |> 
  rename(CPI = cpi_score) |>
  group_by(region_un, year) |> 
  mutate(sCPI = scale(CPI, scale = FALSE)) |>
  ungroup() |>
  mutate(standard_error = round(standard_error, 3)) |>
  select(iso_a3, country_territory, year, region_un, CPI, sCPI, Rank, standard_error, sources, tooltip, geometry) 
rm(Panel.FJ, Names.Merge)

Map.Data <- na.omit(Map.Data)
