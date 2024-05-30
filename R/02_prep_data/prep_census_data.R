
# load -------------------------------------------------------------------------

library(tidyverse)
source("R/functions/f_get_region.R")

oa_lookup <- read_csv("data/OA_TO_HIGHER_AREAS.csv") %>% 
  select(OA2022, SPC2021, Easting, Northing) %>% 
  mutate(Constituency = const_code_to_name(SPC2021),
         Region = const_name_to_region(Constituency))

census_files <- list.files("data/Census-2022-Output-Area", full.names = TRUE)

# ethnic -----------------------------------------------------------------------

ethnic <- read_csv(census_files[grepl("UV201 ", census_files)], skip = 4, 
                   na = "-", name_repair = make.names) %>% 
  rename(OA2022 = 1) %>% 
  
  # remove notes below table
  filter(grepl("S00", OA2022)) %>% 
  
  left_join(oa_lookup, by = "OA2022") %>% 
  rename(x = Easting,
         y = Northing) %>% 
  arrange(Region, Constituency)

# gaelic -----------------------------------------------------------------------

gaelic <- read_csv(census_files[grepl("UV208 ", census_files)], skip = 4, 
                   na = "-", name_repair = make.names) %>% 
  rename(OA2022 = 1) %>% 
  
  # remove notes below table
  filter(grepl("S00", OA2022)) %>% 
  
  left_join(oa_lookup, by = "OA2022") %>% 
  rename(x = Easting,
         y = Northing) %>% 
  arrange(Region, Constituency)

# scots -----------------------------------------------------------------------

scots <- read_csv(census_files[grepl("UV209 ", census_files)], skip = 4, 
                   na = "-", name_repair = make.names) %>% 
  rename(OA2022 = 1) %>% 
  
  # remove notes below table
  filter(grepl("S00", OA2022)) %>% 
  
  left_join(oa_lookup, by = "OA2022") %>% 
  rename(x = Easting,
         y = Northing) %>% 
  arrange(Region, Constituency)

# save all ---------------------------------------------------------------------

saveRDS(ethnic, "data/ethnic.rds")
saveRDS(gaelic, "data/gaelic.rds")
saveRDS(scots, "data/scots.rds")

