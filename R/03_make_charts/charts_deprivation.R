
# load -------------------------------------------------------------------------

library(tidyverse)
library(highcharter)
library(sf)
library(geojsonsf)
library(rmapshaper)

source("R/functions/f_make_charts.R")
source("R/functions/f_get_region.R")

simd <- readRDS("data/tidy_simd_data.rds")

regions <- unique(simd$shares$Region)
regions <- regions[!is.na(regions)]

# local shares -----------------------------------------------------------------

charts_simd <- lapply(unique(simd$shares$simdDomain), function(x) {
  
  out <- lapply(regions, function(y) {
    
    title <- case_when(x == "SIMD" ~ paste0("SIMD deprivation in ", y, " constituencies, ", simd$ranks$refPeriod[1]),
                       x == "Education, Skills and Training" ~ paste0("Education domain deprivation in ", y, " constituencies, ", simd$ranks$refPeriod[1]),
                       x == "Access to Services" ~ paste0("Access domain deprivation in ", y, " constituencies, ", simd$ranks$refPeriod[1]),
                       TRUE ~ paste0(x, " domain deprivation in ", y, " constituencies, ", simd$ranks$refPeriod[1]))
    
    simd$shares %>% 
      filter(Region == y | Area_name == "Scotland",
             simdDomain == x) %>% 
      make_tenure_chart() %>% 
      hc_title(text = title) %>% 
      hc_subtitle(text = "Share of Data Zones in each deprivation band")
  })
  names(out) <- regions
  return(out)
})

names(charts_simd) <- unique(simd$shares$simdDomain)

# maps -------------------------------------------------------------------------

## Region maps -----------------------------------------------------------------

# Scottish Parliamentary Regions (December 2022) Boundaries SC BGC
# Scottish Parliamentary Constituencies (December 2022) Boundaries SC BGC

# shapefiles from: 
# https://geoportal.statistics.gov.uk/search?q=BDY_SPC%20DEC_2022&sort=Title%7Ctitle%7Casc

const_sf <- "data/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_811473201121076359/SPC_DEC_2022_SC_BGC.shp"
regions_sf <- "data/Scottish_Parliamentary_Regions_December_2022_SC_BGC_362369918693762846/SPR_DEC_2022_SC_BGC.shp"

# prepare SPC boundary map from shapefile
shp_SPC <- sf::st_read(const_sf) %>% 
  mutate(Constituency = const_code_to_name(SPC22CD),
         Region = const_name_to_region(Constituency)) %>% 
  select(Region, Constituency, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3)

# prepare SPR boundary map from shapefile
shp_SPR <- sf::st_read(regions_sf) %>% 
  rename(Region = SPR22NM) %>% 
  select(Region, geometry) %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.3)

charts_localshare <- lapply(unique(simd$shares$simdDomain), function(x) {
  
  # prepare SIMD data
  localshares <- simd$shares %>% 
    filter(simdDomain == x,
           Measure == "In most deprived fifth") %>% 
    rename(Constituency = Area_name) %>% 
    mutate(Data = Data * 100)
  
  
  out <- lapply(regions, function(y) {
    
    title <- case_when(x == "SIMD" ~ paste0("Where ", x, " deprivation is concentrated in ", y),
                       x == "Education, Skills and Training" ~ paste0("Where Education domain deprivation is concentrated in ", y),
                       x == "Access to Services" ~ paste0("Where Access domain deprivation is concentrated in ", y),
                       TRUE ~ paste0("Where ", x, " domain deprivation is concentrated in ", y))
    
    make_localshare_map(sf = shp_SPC %>% filter(Region == y),
                        df = localshares %>% filter(Region == y)) %>% 
      hc_title(text = title) %>% 
      hc_subtitle(text = "Constituencies' share of Scotland's 20% most deprived Data Zones")
  })
  names(out) <- regions
  return(out)
})

names(charts_localshare) <- unique(simd$shares$simdDomain)

## DZ map ----------------------------------------------------------------------

# DZ shapefile downloaded from:
# https://spatialdata.gov.scot/geonetwork/srv/api/records/7d3e8709-98fa-4d71-867c-d5c8293823f2

# DZ shapefile
shp_DZ <- sf::st_read("data/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp") %>% 
  mutate(Constituency = dz_code_to_const(DataZone),
         Region = const_name_to_region(Constituency),
         DataZoneName = dz_code_to_name(DataZone)) %>% 
  select(Region, Constituency, DataZone, DataZoneName, geometry) %>%
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.2)

# loop over domains
charts_deciles <- lapply(unique(simd$shares$simdDomain), function(d) {
  
  ranks <- simd$ranks %>% 
    filter(simdDomain == d) %>% 
    rename(DataZoneName = DataZone,
           DataZone = refArea)
  
  # loop over regions
  out <- lapply(regions, function(r) {
    
    constituencies <- unique(ranks %>% filter(Region == r) %>% select(Constituency) %>% pull)
    
    # loop over constituencies
    const <- lapply(constituencies, function(c) {
      
      title <- case_when(d == "SIMD" ~ paste0("Where SIMD deprivation is concentrated in ", c),
                         d == "Education, Skills and Training" ~ paste0("Where Education domain deprivation is concentrated in ", c),
                         d == "Access to Services" ~ paste0("Where Access domain deprivation is concentrated in ", c),
                         TRUE ~ paste0("Where ", d, " domain deprivation is concentrated in ", c))
      
      make_decile_map(sf = shp_DZ %>% filter(Region == r, Constituency == c), 
                      df = ranks %>%  filter(Region == r, Constituency == c)) %>% 
        hc_title(text = title) %>% 
        hc_subtitle(text = "Deprivation decile in each Data Zone (1 = most deprived)")
    })
    
    names(const) <- constituencies
    return(const)
  })
  
  names(out) <- regions
  return(out)
  
})

names(charts_deciles) <- unique(simd$shares$simdDomain)

# save all ---------------------------------------------------------------------

saveRDS(list(barchart = charts_simd,
             region_maps = charts_localshare,
             const_maps = charts_deciles), 
        "data/charts_deprivation.rds")

rm(list = ls())
