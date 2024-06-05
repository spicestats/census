

# data zone map -------------------------------------------------------------

DZ_sf <- "data/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp"

# prepare SPC boundary map from shapefile
DZ_map <- sf::st_read(DZ_sf) %>% 
  select(DataZone, StdAreaKm2, geometry) %>% 
  rename(area = StdAreaKm2) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  sf::st_transform(4326)
