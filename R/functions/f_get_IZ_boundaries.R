

# intermediate zone map -------------------------------------------------------------

IZ_sf <- "data/SG_IntermediateZoneBdry_2011/SG_IntermediateZone_Bdry_2011.shp"

# prepare SPC boundary map from shapefile
IZ_map <- sf::st_read(IZ_sf) %>% 
  select(InterZone, StdAreaKm2, geometry) %>% 
  # reduce size of map by losing some detail
  rename(area = StdAreaKm2) %>% 
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  sf::st_transform(4326)
