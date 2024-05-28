
# load -------------------------------------------------------------------------

library(tidyverse)
library(highcharter)
library(sf)
library(geojsonsf)
library(rmapshaper)

source("R/functions/f_make_charts.R")
source("R/functions/f_round2.R")

gaelic <- readRDS("data/gaelic.rds")
scots <- readRDS("data/scots.rds")

# prep map data ----------------------------------------------------------------

# Countries (December 2023) Boundaries UK BGC
# shapefile from: 
# https://geoportal.statistics.gov.uk/maps/931a96f7c7d842dd924b214c3f4bf32f

sf <- "data/Countries_December_2023_Boundaries_UK_BGC_6982568813931010125/CTRY_DEC_2023_UK_BGC.shp"

# prepare SPR boundary map from shapefile
Scotland_map <- sf::st_read(sf) %>% 
  rename(Country = CTRY23NM) %>% 
  select(Country, geometry) %>% 
  filter(Country == "Scotland") %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  geojsonsf::sf_geojson() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE)

# prep Census data -------------------------------------------------------------

gaelic_skills <- gaelic %>% 
  mutate(rate = (All.people.aged.3.and.over - No.skills.in.Gaelic)/All.people.aged.3.and.over,
         rate = ifelse(rate < 0, 0, rate),
         rate = round2(100 * rate, 1)) %>% 
  select(x, y, rate) %>% 
  filter(!is.na(rate),
         rate > 0)

scots_skills <- scots %>% 
  mutate(rate = (All.people.aged.3.and.over - No.skills.in.Scots)/All.people.aged.3.and.over,
         rate = ifelse(rate < 0, 0, rate),
         rate = round2(100 * rate, 1)) %>% 
  select(x, y, rate) %>% 
  filter(!is.na(rate),
         rate > 0)

# make maps ---------------------------------------------------------------------

gaelic_map <- highchart(type = "map")  %>%
  hc_title(text = "More Gaelic skills in the North West") %>% 
  hc_subtitle(text = "Proportion of people who understand, speak, read, and/or write in Gaelic") %>% 
  hc_add_series(
    mapData = Scotland_map,
    showInLegend = FALSE
  ) %>%
  hc_add_series(name = "People with Gaelic skills",
                type = "mappoint",
                data = gaelic_skills,
                visible = TRUE) %>% 
  hc_plotOptions(
    mappoint = list(
      colorKey = "rate",
      marker = list(symbol = "circle",
                    radius = 1,
                    lineWidth = 1,
                    fillColor = NULL),
      tooltip = list(pointFormat = "{point.rate}%")
    )) %>% 
  hc_colorAxis(type = "linear",
               min = 0,
               max = 100,
               tickInterval = 50,
               minColor = unname(spcols["mustard"]),
               maxColor = unname(spcols["purple"]),
               labels = list(format = "{value}%")) %>% 
  hc_legend(enabled = TRUE,
            align = "left",
            verticalAlign = "top",
            floating = TRUE,
            y = 50) %>% 
  hc_add_theme(my_theme)

scots_map <- highchart(type = "map")  %>%
  hc_title(text = "Scots skills across Scotland") %>% 
  hc_subtitle(text = "Proportion of people who understand, speak, read, and/or write in Scots") %>% 
  hc_add_series(
    mapData = Scotland_map,
    showInLegend = FALSE
  ) %>%
  hc_add_series(name = "People with Scots skills",
                type = "mappoint",
                data = scots_skills,
                visible = TRUE) %>% 
  hc_plotOptions(
    mappoint = list(
      colorKey = "rate",
      marker = list(symbol = "circle",
                    radius = 1,
                    lineWidth = 1,
                    fillColor = NULL),
      tooltip = list(pointFormat = "{point.rate}%")
    )) %>% 
  hc_colorAxis(type = "linear",
               #startOnTick = FALSE,
               tickInterval = 50,
               min = 0,
               max = 100,
               minColor = unname(spcols["mustard"]),
               maxColor = unname(spcols["purple"]),
               labels = list(format = "{value}%")) %>% 
  hc_legend(enabled = TRUE,
            align = "left",
            verticalAlign = "top",
            floating = TRUE,
            y = 50) %>% 
  hc_add_theme(my_theme)


scots_map
gaelic_map
