
# load -------------------------------------------------------------------------

library(tidyverse)
library(highcharter)
library(sf)
library(geojsonsf)
library(rmapshaper)

source("R/functions/f_get_region.R")
source("R/functions/f_make_charts.R")

ethnic <- readRDS("data/ethnic.rds")

regions <- unique(ethnic$Region)

# local shares -----------------------------------------------------------------

r <- "Glasgow"
c <- ethnic %>% 
  filter(Region == r) %>% 
  select(Constituency) %>% 
  distinct() %>% 
  head(1L) %>% 
  pull()

# maps -------------------------------------------------------------------------

# Scottish Parliamentary Constituencies (December 2022) Boundaries SC BGC
# shapefiles from: 
# https://geoportal.statistics.gov.uk/search?q=BDY_SPC%20DEC_2022&sort=Title%7Ctitle%7Casc

const_sf <- "data/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_811473201121076359/SPC_DEC_2022_SC_BGC.shp"

# prepare SPC boundary map from shapefile
shp_SPC <- sf::st_read(const_sf) %>% 
  mutate(Constituency = const_code_to_name(SPC22CD),
         Region = const_name_to_region(Constituency)) %>% 
  select(Region, Constituency, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) 

local_map <- shp_SPC %>% 
  filter(Region == r) %>% 
  geojsonsf::sf_geojson() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE)

gypsy <- ethnic %>% 
  filter(Region == r) %>% 
  select(x, y, "White..Gypsy..Traveller") %>% 
  rename(z = 3) %>% 
  filter(!is.na(z)) %>% 
  uncount(z, .id = "id") %>% 
  # slightly move duplicate coordinates (=jitter); randomise direction
  mutate(rand1 = sample(-10:10, n(), replace=T),
         rand2 = sample(-10:10, n(), replace=T),
         x = ifelse(id != 1, x + rand1, x),
         y = ifelse(id != 1, y + rand2, y)) %>% 
  select(x, y)

polish <- ethnic %>% 
  filter(Region == r) %>% 
  select(x, y, "White..White.Polish") %>% 
  rename(z = 3) %>% 
  filter(!is.na(z)) %>% 
  uncount(z, .id = "id") %>% 
  # slightly move duplicate coordinates (=jitter); randomise direction
  mutate(rand1 = sample(-10:10, n(), replace=T),
         rand2 = sample(-10:10, n(), replace=T),
         x = ifelse(id != 1, x + rand1, x),
         y = ifelse(id != 1, y + rand2, y)) %>% 
  select(x, y)

asian <- ethnic %>% 
  filter(Region == r) %>% 
  select(x, y, "Asian..Asian.Scottish.or.Asian.British..Total") %>% 
  rename(z = 3) %>% 
  filter(!is.na(z))  %>% 
  uncount(z, .id = "id") %>% 
  # slightly move duplicate coordinates (=jitter); randomise direction
  mutate(rand1 = sample(-10:10, n(), replace=T),
         rand2 = sample(-10:10, n(), replace=T),
         x = ifelse(id != 1, x + rand1, x),
         y = ifelse(id != 1, y + rand2, y)) %>% 
  select(x, y)


highchart(type = "map")  %>%
  hc_add_theme(my_theme) %>% 
  hc_add_dependency("modules/marker-clusters.js") %>% 
  hc_add_series(
    mapData = local_map,
    showInLegend = FALSE
  ) %>%
  hc_add_series(name = "Gypsy Traveller",
                type = "mappoint",
                data = gypsy,
                visible = TRUE) %>% 
  hc_add_series(name = "White Polish",
                type = "mappoint",
                data = polish,
                visible = FALSE) %>%
  hc_add_series(name = "Asian or Asian-Scottish/-British",
                type = "mappoint",
                data = asian,
                visible = FALSE) %>%
  hc_plotOptions(
    mappoint = list(
      opacity = 0.5,
      cluster = list(enabled = TRUE,
                     #drillToCluster = TRUE, # chart freezes when zooming in too far
                     allowOverlap = FALSE,
                     layoutAlgorithm = list(type = "grid",
                                            gridSize = 100),
                     minimumClusterSize = 2,
                     zones = list(
                       list(from = 1, to = 50, marker = list(radius = 10)),
                       list(from = 51, to = 300, marker = list(radius = 15)),
                       list(from = 301, to = 1000, marker = list(radius = 20)),
                       list(from = 1001, to = 2000, marker = list(radius = 25)),
                       list(from = 2001, to = 1000000, marker = list(radius = 30)))
      ),
      marker = list(symbol = "circle",
                    lineWidth = 1,
                    fillColor = NULL),
      tooltip = list(pointFormat = NULL,
                     clusterFormat = "{point.clusterPointsAmount}")
    )) %>% 
  hc_mapNavigation(enabled = TRUE)

