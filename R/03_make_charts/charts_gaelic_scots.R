
# load -------------------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(sf)
library(htmlwidgets)
library(geojsonsf)
library(rmapshaper)
library(htmltools)

source("R/functions/f_make_charts.R")
source("R/functions/f_get_region.R")
source("R/functions/f_round2.R")
source("R/functions/f_normalise.R")
source("R/functions/f_get_SP_boundaries.R")

gaelic <- readRDS("data/gaelic.rds")
scots <- readRDS("data/scots.rds")
regions <- unique(gaelic$Region)

# prep data --------------------------------------------------------------------

gaelic_prepped <- gaelic %>% 
  mutate(gaelic = All.people.aged.3.and.over - No.skills.in.Gaelic,
         gaelic = ifelse(gaelic < 0, NA, gaelic)) %>% 
  select(Region, Constituency, x, y, gaelic)

scots_prepped <- scots %>% 
  mutate(scots = All.people.aged.3.and.over - No.skills.in.Scots,
         scots = ifelse(scots < 0, NA, scots)) %>% 
  select(x, y, scots)
  
prepped <-  gaelic_prepped %>% 
  left_join(scots_prepped, by = c("x", "y")) %>% 
  rename("Has Gaelic language skills" = gaelic,
         "Has Scots language skills" = scots) %>% 
  # transform to long /lat
  sf::st_as_sf(coords = c("x", "y"), crs = 27700) %>%
  # transform to standard projection
  sf::st_transform(4326) %>% 
  pivot_longer(cols = where(is.numeric), names_to = "group") %>% 
  mutate(.by = c(Region, group),
         alpha = normalise(value, 0.4, 1),
         size = normalise(value, 2, 12)) %>% 
  filter(value > 0) %>% 
  # jitter points so overlapping data points can be seen better
  sf::st_jitter(factor = 0.001) %>%
  arrange(Region, group)

pal <- colorFactor(spcols, domain = NULL)

# make maps --------------------------------------------------------------------

maps <- lapply(regions, function(x) {
  
  bounds <- const_map %>% 
    filter(Region == x) %>% 
    sf::st_bbox() %>% 
    as.numeric()

  leaflet(data = prepped) %>%
    addPolygons(data = const_map %>% filter(Region == x),
                fill = TRUE,
                fillColor = "#E6E6E6",
                weight = 1,
                color = "#CCCCCC",
                label = ~htmltools::htmlEscape(Constituency),
                highlightOptions = highlightOptions(
                  color = unname(spcols["mustard"]),
                  fill = TRUE,
                  fillColor = unname(spcols["mustard"]), 
                  weight = 2,
                  bringToFront = TRUE,
                  sendToBack = TRUE)) %>%
    addCircleMarkers(data = prepped %>% filter(Region == x),
                     group = ~group,
                     radius = ~size,
                     stroke = FALSE, 
                     color = ~pal(group),
                     fillOpacity = ~alpha) %>%
    htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}")) %>%
    addLayersControl(
      overlayGroups = unique(prepped$group),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Has Scots language skills") %>% 
    fitBounds(bounds[1]+0.2, bounds[2]+0.2, bounds[3]-0.2, bounds[4]-0.2)
})

maps[[1]]

names(maps) <- regions
saveRDS(maps, "data/gaelicscots_maps.rds")
#rm(list = ls())


