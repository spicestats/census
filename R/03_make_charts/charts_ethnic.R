
# load -------------------------------------------------------------------------

library(tidyverse)
library(highcharter)
library(sf)
library(geojsonsf)
library(rmapshaper)

source("R/functions/f_make_charts.R")
source("R/functions/f_get_region.R")

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

ethnic_c <- ethnic %>% 
  filter(Region == r)


highchart(type = "map") %>% 
  hc_add_series(
    mapData = local_map,
    showInLegend = FALSE
  ) %>%
  hc_add_series(name = "Gypsy Traveller",
                type = "mapbubble",
                color = unname(spcols[1]),
                data = ethnic_c %>% 
                  select(x, y, "White..Gypsy..Traveller") %>% 
                  rename(z = 3)) %>% 
  hc_add_series(name = "White Polish",
                type = "mapbubble",
                color = unname(spcols)[3],
                data = ethnic_c  %>% 
                  select(x, y, "White..White.Polish") %>% 
                  rename(z = 3)) %>% 
  hc_plotOptions(mapbubble = list(maxSize = "5%",
                                  marker = list(fillColor = NULL))) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_mapNavigation(enabled = TRUE) %>% 
  hc_add_theme(my_theme)

