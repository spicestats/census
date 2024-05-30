
# load -------------------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(geojsonsf)
library(rmapshaper)
library(htmltools)

source("R/functions/f_make_charts.R")
source("R/functions/f_get_region.R")
source("R/functions/f_round2.R")
source("R/functions/f_normalise.R")

ethnic <- readRDS("data/ethnic.rds")
regions <- unique(ethnic$Region)

# constituency map -------------------------------------------------------------

# Scottish Parliamentary Constituencies (December 2022) Boundaries SC BGC
# shapefiles from: 
# https://geoportal.statistics.gov.uk/search?q=BDY_SPC%20DEC_2022&sort=Title%7Ctitle%7Casc

const_sf <- "data/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_811473201121076359/SPC_DEC_2022_SC_BGC.shp"

# prepare SPC boundary map from shapefile
const_map <- sf::st_read(const_sf) %>% 
  mutate(Constituency = const_code_to_name(SPC22CD),
         Region = const_name_to_region(Constituency)) %>% 
  select(Region, Constituency, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  sf::st_transform(4326)

# prep data --------------------------------------------------------------------

ethnic_prepped <- ethnic %>% 
  rename("White - total" = "White..Total",
         "White - Scottish" = "White..White.Scottish",
         "White - other British" = "White..Other.White.British",
         "White - Irish" = "White..White.Irish",                                                                              
         "White - Gypsy / Traveller" = "White..Gypsy..Traveller",                                                                       
         "White - Polish" = "White..White.Polish",                                                                          
         "White - other" = "Other.White",                                                                                     
         "Mixed / multiple" = "Mixed.or.multiple.ethnic.group", 
         "Asian - total" = "Asian..Asian.Scottish.or.Asian.British..Total",                                                   
         "Asian - Pakistani" = "Asian..Asian.Scottish.or.Asian.British..Pakistani..Pakistani.Scottish.or.Pakistani.British",      
         "Asian - Indian" = "Asian..Asian.Scottish.or.Asian.British..Indian..Indian.Scottish.or.Indian.British",               
         "Asian - Bangladeshi" = "Asian..Asian.Scottish.or.Asian.British..Bangladeshi..Bangladeshi.Scottish.or.Bangladeshi.British",
         "Asian - Chinese" = "Asian..Asian.Scottish.or.Asian.British..Chinese..Chinese.Scottish.or.Chinese.British",            
         "Asian - other" = "Asian..Asian.Scottish.or.Asian.British..Other.Asian",                                             
         "African" = "African..Total",                                                                                  
         "Caribbean / Black - total" = "Caribbean.or.Black..Total",                                                                       
         "Caribbean / Black - Caribbean" = "Caribbean.or.Black..Caribbean..Caribbean.Scottish.or.Caribbean.British",                          
         "Caribbean / Black - Black" = "Caribbean.or.Black..Black..Black.Scottish.or.Black.British",                                      
         "Caribbean / Black - other" = "Caribbean.or.Black..Other.Caribbean.or.Black",                                                    
         "Arab"  = "Other.ethnic.groups..Arab..Arab.Scottish.or.Arab.British",                                        
         "Other" = "Other.ethnic.groups..Other.ethnic.group") %>% 
  select(-"All.People",
         -"African..African..African.Scottish.or.African.British",
         -"African..Other.African",
         -"Other.ethnic.groups..Total") %>% 
  # transform to long /lat
  st_as_sf(coords = c("x", "y"), 
           crs = 27700) %>%
  # transform to standard projection
  st_transform(4326) %>% 
  pivot_longer(cols = where(is.numeric), names_to = "group", 
               names_transform = function(x) ordered(x, levels = 
                                                       c("African",                                                                                  
                                                         "Asian - total",                                                   
                                                         "Asian - Bangladeshi",
                                                         "Asian - Chinese",            
                                                         "Asian - Indian",               
                                                         "Asian - Pakistani",      
                                                         "Asian - other",                                             
                                                         "Arab",                                        
                                                         "Caribbean / Black - total",                                                                       
                                                         "Caribbean / Black - Black",                                      
                                                         "Caribbean / Black - Caribbean",                          
                                                         "Caribbean / Black - other",                                                    
                                                         "White - total",
                                                         "White - Scottish",
                                                         "White - other British",
                                                         "White - Irish",                                                                              
                                                         "White - Gypsy / Traveller",                                                                       
                                                         "White - Polish",                                                                          
                                                         "White - other",                                                                                     
                                                         "Mixed / multiple", 
                                                         "Other"))) %>% 
  mutate(.by = group, 
         alpha = normalise(value, 0.5, 0.8),
         size = normalise(value, 1.5, 8)) %>% 
  filter(!is.na(value)) %>% 
  arrange(group)

pal <- colorFactor(spcols, domain = NULL)

# make maps --------------------------------------------------------------------

ethnic_region <- lapply(regions, function(x) {
  leaflet(data = ethnic_prepped) %>%
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
    addCircleMarkers(data = ethnic_prepped %>% filter(Region == x),
                     group = ~group,
                     radius = ~size,
                     stroke = FALSE, 
                     color = ~pal(group),
                     fillOpacity = ~alpha) %>%
    htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}")) %>%
    addLayersControl(
      baseGroups = unique(ethnic_prepped$group),
      options = layersControlOptions(collapsed = FALSE)
    )
})

names(ethnic_region) <- regions

saveRDS(ethnic_region, "data/ethnic_maps.rds")
