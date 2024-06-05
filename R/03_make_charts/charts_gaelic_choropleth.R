
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
source("R/functions/f_get_DZ_boundaries.R")
source("R/functions/f_get_IZ_boundaries.R")

gaelic <- readRDS("data/gaelic.rds")
scots <- readRDS("data/scots.rds")
regions <- unique(gaelic$Region)

# prep datasets ----------------------------------------------------------------

gaelic_prepped <- gaelic %>% 
  mutate(all = All.people.aged.3.and.over,
         gaelic = all - No.skills.in.Gaelic,
         gaelic = ifelse(gaelic < 0, NA, gaelic)) %>% 
  select(Region, Constituency, DZ2011, IZ2011, IZ, OA2022, all, gaelic)

scots_prepped <- scots %>% 
  mutate(scots = All.people.aged.3.and.over - No.skills.in.Scots,
         scots = ifelse(scots < 0, NA, scots)) %>% 
  select(OA2022, scots)

prepped <-  gaelic_prepped %>% 
  left_join(scots_prepped, by = "OA2022")

# aggregate --------------------------------------------------------------------
# to different geographies and add area and geometry

prepped_IZ = IZ_map %>% 
  left_join(prepped %>%
              summarise(.by = c(IZ2011, IZ), 
                        across(where(is.numeric), sum, na.rm = TRUE)), 
            by = c("InterZone" = "IZ2011")) %>% 
  mutate(rate_g = gaelic / all,
         rate_s = scots / all,
         label_g = paste0("<strong>", IZ, "</strong><br>People with Gaelic skills: ", 100*round2(rate_g, 2), "%"),
         label_s = paste0("<strong>", IZ, "</strong><br>People with Scots skills: ", 100*round2(rate_s, 2), "%"))


prepped_const = const_map %>% 
  left_join(prepped %>% 
              summarise(.by = Constituency, 
                        across(where(is.numeric), sum, na.rm = TRUE)), 
            by = "Constituency") %>% 
  mutate(rate_g = gaelic / all,
         rate_s = scots / all,
         label_g = paste0("<strong>", Constituency, "</strong><br>People with Gaelic skills: ", 100*round2(rate_g, 2), "%"),
         label_s = paste0("<strong>", Constituency, "</strong><br>People with Scots skills: ", 100*round2(rate_s, 2), "%"))


# make Scotland map ------------------------------------------------------------

# Gaelic -----------------------------------------------------------------------

mincol <- floor(min(prepped_IZ$rate_g*100))
maxcol <- ceiling(max(prepped_IZ$rate_g*100))
pal <- colorNumeric(spcols[c(12, 1)], domain = mincol:maxcol)

gaelic_chor <-leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
  addTiles(group = "Basemap") %>% 
  addPolygons(group = "Gaelic1",
              data = prepped_const,
              fillColor = ~pal(rate_g*100),
              fillOpacity = ~normalise(rate_g, 0.6, 1),
              color = "white",
              opacity = 0.8,
              weight = 1,
              label = ~lapply(label_g, htmltools::HTML),
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.4,
                bringToFront = TRUE)) %>% 
  addPolygons(group = "Gaelic2",
              data = prepped_IZ,
              fillColor = ~pal(rate_g*100),
              fillOpacity = ~normalise(rate_g, 0.6, 1),
              color = "white",
              opacity = 0.8,
              weight = 1,
              label = ~lapply(label_g, htmltools::HTML),
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.4,
                bringToFront = TRUE)) %>%
  htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}")) %>%
  addLayersControl(overlayGroups = "Basemap",
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  groupOptions("Gaelic1", zoomLevels = 6:9) %>% 
  groupOptions("Gaelic2", zoomLevels = 10:16) %>% 
  addLegend(pal = pal,
            values = mincol:maxcol,
            labFormat = labelFormat(suffix = "%"),
            bins = 3,
            opacity = 1,
            position = "bottomright",
            title = "Share of people<br>with Gaelic<br>language skills")

# Scots -----------------------------------------------------------------------

mincol <- floor(min(prepped_IZ$rate_s*100))
maxcol <- ceiling(max(prepped_IZ$rate_s*100))
pal <- colorNumeric(spcols[c(12, 1)], domain = mincol:maxcol)

scots_chor <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
  addTiles(group = "Basemap") %>% 
  addPolygons(group = "Scots1",
              data = prepped_const,
              fillColor = ~pal(rate_s*100),
              fillOpacity = ~normalise(rate_s, 0.6, 1),
              color = "white",
              opacity = 0.8,
              weight = 1,
              label = ~lapply(label_s, htmltools::HTML),
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.4,
                bringToFront = TRUE)) %>% 
  addPolygons(group = "Scots2",
              data = prepped_IZ,
              fillColor = ~pal(rate_s*100),
              fillOpacity = ~normalise(rate_s, 0.6, 1),
              color = "white",
              opacity = 0.8,
              weight = 1,
              label = ~lapply(label_s, htmltools::HTML),
              highlightOptions = highlightOptions(
                weight = 2,
                fillOpacity = 0.4,
                bringToFront = TRUE)) %>%
  htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}")) %>%
  addLayersControl(overlayGroups = "Basemap",
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  groupOptions("Scots1", zoomLevels = 6:9) %>% 
  groupOptions("Scots2", zoomLevels = 10:16) %>% 
  addLegend(pal = pal,
            values = mincol:maxcol,
            labFormat = labelFormat(suffix = "%"),
            bins = 3,
            opacity = 1,
            position = "bottomright",
            title = "Share of people<br>with Scots<br>language skills")

saveRDS(list(gaelic_chor = gaelic_chor,
             scots_chor = scots_chor), "data/gaelicscots_chor.rds")

# both ----

# if plotting both in same map, then zoomLevels doesn't work

# prepped_const = const_map %>% 
#   left_join(prepped %>% 
#               summarise(.by = Constituency, 
#                         across(where(is.numeric), sum, na.rm = TRUE)), 
#             by = "Constituency") %>% 
#   pivot_longer(cols = c(gaelic, scots)) %>% 
#   mutate(rate = value / all,
#          label = paste0("<strong>", Constituency, "</strong><br>People with ", str_to_title(name), " skills: ", 100*round2(rate, 2), "%"))
# 
# 
# maxcol <- round2(max(prepped_const$rate*100), -1) + 10 
# pal <- colorNumeric(spcols[c(12, 1)], domain = 0:maxcol)
# 
# leaflet() %>%
#   addTiles(group = "Show map") %>% 
#   addPolygons(group = ~name,
#               data = prepped_const,
#               fillColor = ~pal(rate*100),
#               fillOpacity = ~normalise(rate, 0.4, 1),
#               color = "white",
#               opacity = 0.8,
#               weight = 1,
#               label = ~lapply(label, htmltools::HTML),
#               highlightOptions = highlightOptions(
#                 weight = 2,
#                 fillOpacity = 0.4,
#                 bringToFront = TRUE)) %>% 
#   htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}")) %>%
#   addLayersControl(overlayGroups = "Show map",
#                    baseGroups = unique(prepped_const$name),
#                    options = layersControlOptions(collapsed = FALSE)) %>% 
#   addLegend(pal = pal,
#             values = 0:maxcol,
#             labFormat = labelFormat(suffix = "%"),
#             bins = 3,
#             opacity = 1,
#             position = "bottomright",
#             title = "Share of people<br>with Scots<br>language skills")
