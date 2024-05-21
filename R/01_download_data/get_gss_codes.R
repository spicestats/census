
library(tidyverse)
library(polite)
library(rvest)


# gss codes --------------------------------------------------------------------

# GSS area codes to get from S16 codes (which change over time) to constituency 
# names (which don't change as much), downloaded from 
# https://www.gov.scot/publications/small-area-statistics-reference-materials/

url <- "https://www.gov.scot/publications/small-area-statistics-reference-materials/"

session <- polite::bow(url)
links_on_site <- polite::scrape(session) %>%
  html_nodes("a") %>% 
  html_attr("href")

download_link <- links_on_site[grepl("gss-codes", links_on_site)][1]

download.file(paste0("https://www.gov.scot", download_link), 
              "data/gss_codes.xlsx", mode = "wb")

# SP const to region lookup ----------------------------------------------------
#   Scottish_Parliamentary_Constituency_to_Region_lookup.xlsx
#   office-spice > documents > projects > statistics > tools > 
#   https://scottish4.sharepoint.com/:x:/r/sites/office-spice/Shared%20Documents/Projects/Statistics/Tools/Scottish_Parliamentary_Constituency_to_Region_lookup.xlsx?d=w593e4e1a23d94f77a73ad910b3f88299&csf=1&web=1&e=5lcSC2

url2 <- "C:/Users/s910140/OneDrive - The Scottish Parliament/Statistics/Tools/Scottish_Parliamentary_Constituency_to_Region_lookup.xlsx"

file.copy(url2, "data/region_lookup.xlsx")
