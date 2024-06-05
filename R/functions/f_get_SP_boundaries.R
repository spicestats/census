

# constituency map -------------------------------------------------------------

# Scottish Parliamentary Constituencies (December 2022) Boundaries SC BGC
# shapefiles from: 
# https://geoportal.statistics.gov.uk/search?q=BDY_SPC%20DEC_2022&sort=Title%7Ctitle%7Casc

const_sf <- "data/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_811473201121076359/SPC_DEC_2022_SC_BGC.shp"

# get area from csv file

csv <- read_delim("data/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-7600346900927297545.csv") %>% 
  select(SPC22NM, Shape__Area)

# prepare SPC boundary map from shapefile
const_map <- sf::st_read(const_sf) %>% 
  left_join(csv, by = "SPC22NM") %>% 
  mutate(Constituency = const_code_to_name(SPC22CD),
         Region = const_name_to_region(Constituency),
         area = Shape__Area) %>% 
  select(Region, Constituency, area, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  sf::st_transform(4326)

# region map -------------------------------------------------------------------

region_sf <- "data/Scottish_Parliamentary_Regions_December_2022_SC_BGC_362369918693762846/SPR_DEC_2022_SC_BGC.shp"

# prepare SPR boundary map from shapefile
region_map <- sf::st_read(region_sf) %>% 
  rename(Region = SPR22NM) %>% 
  select(Region, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3) %>% 
  sf::st_transform(4326)