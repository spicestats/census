
# load -------------------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(geojsonsf)
library(rmapshaper)

source("R/functions/f_make_charts.R")
source("R/functions/f_round2.R")

gaelic <- readRDS("data/gaelic.rds")
scots <- readRDS("data/scots.rds")

# prep Census data -------------------------------------------------------------

gaelic_skills <- gaelic %>% 
  mutate(with_skills = All.people.aged.3.and.over - No.skills.in.Gaelic,
         all = All.people.aged.3.and.over,
         rate = with_skills/all,
         rate = ifelse(rate < 0, 0, rate),
         rate = round2(100 * rate, 1)) %>% 
  select(rate, x, y) %>% 
  filter(rate > 0) %>% 
  # transform to long /lat
  st_as_sf(coords = c("x", "y"), 
           crs = 27700) %>%
  # transform to standard projection
  st_transform(4326)

scots_skills <- scots %>% 
  mutate(rate = (All.people.aged.3.and.over - No.skills.in.Scots)/All.people.aged.3.and.over,
         rate = ifelse(rate < 0, 0, rate),
         rate = round2(100 * rate, 1)) %>% 
  select(x, y, rate) %>% 
  filter(!is.na(rate),
         rate > 0) %>% 
  # transform to long /lat
  st_as_sf(coords = c("x", "y"), 
           crs = 27700) %>%
  # transform to standard projection
  st_transform(4326)

# prep map data ----------------------------------------------------------------

# Countries (December 2023) Boundaries UK BGC
# shapefile from: 
# https://geoportal.statistics.gov.uk/maps/931a96f7c7d842dd924b214c3f4bf32f

sf <- "data/Countries_December_2023_Boundaries_UK_BGC_6982568813931010125/CTRY_DEC_2023_UK_BGC.shp"

# prepare SPR boundary map from shapefile
Scotland_map <- sf::st_read(sf) %>% 
  rename(Country = CTRY23NM) %>% 
  select(Country, geometry) %>% 
  filter(Country == "Scotland") %>% 
  # reduce size of map by losing some detail
  rmapshaper::ms_simplify(keep = 0.1) %>% 
  sf::st_transform(4326)

# make maps ---------------------------------------------------------------------

pal <- colorNumeric(colorRamp(c(unname(spcols["mustard"]), 
                                unname(spcols["purple"])), 
                              interpolate="spline"), 
                    domain = NULL)

gaelic_map <- leaflet(options = leafletOptions(zoomControl = FALSE),
                      data = gaelic_skills) %>%
  addPolygons(data = Scotland_map,
              fill = FALSE,
              weight = 2,
              color = "#636363") %>%
  addCircleMarkers(radius = ~round2(log(rate)),
                   stroke = FALSE, 
                   color = ~pal(rate),
                   fillOpacity = 0.7) %>%
  htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}"))

htmlwidgets::saveWidget(gaelic_map, "output/gaelic_map.html", selfcontained = TRUE)
webshot2::webshot("output/gaelic_map.html", file = "output/gaelic_map.png",
                  vwidth = 800,
                  vheight = 1500)

scots_map <- leaflet(data = scots_skills) %>%
  addPolygons(data = Scotland_map,
              fill = FALSE,
              weight = 2,
              color = "#636363") %>%
  addCircleMarkers(radius = ~round2(log(rate)),
                   stroke = FALSE, 
                   color = ~pal(rate),
                   fillOpacity = 0.7) %>%
  htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container {background: white;}"))

htmlwidgets::saveWidget(scots_map, "output/scots_map.html", selfcontained = TRUE)
webshot2::webshot("output/scots_map.html", file = "output/scots_map.png",
                  vwidth = 800,
                  vheight = 1500)

# aggregate data to constituency -----------------------------------------------

gaelic_const <- gaelic %>% 
  mutate(with_skills = All.people.aged.3.and.over - No.skills.in.Gaelic,
         all = All.people.aged.3.and.over) %>% 
  summarise(.by = c(Region, Constituency),
            with_skills = sum(with_skills, na.rm = TRUE), 
            all = sum(all, na.rm = TRUE)) %>% 
  mutate(rate = with_skills/all) %>% 
  arrange(desc(rate)) %>% 
  rowid_to_column() %>% 
  mutate(label = case_when(rowid %in% 1:3 ~ Constituency))

scots_const <- scots %>% 
  mutate(with_skills = All.people.aged.3.and.over - No.skills.in.Scots,
         all = All.people.aged.3.and.over) %>% 
  summarise(.by = c(Region, Constituency),
            with_skills = sum(with_skills, na.rm = TRUE), 
            all = sum(all, na.rm = TRUE)) %>% 
  mutate(rate = with_skills/all) %>% 
  arrange(desc(rate)) %>% 
  rowid_to_column() %>% 
  mutate(label = case_when(rowid %in% 1:3 ~ Constituency,
                           rowid == max(rowid) ~ Constituency))

# jitter charts ----------------------------------------------------------------

gaelic_jitter <- gaelic_const %>% 
  as.data.frame() %>% 
  ggplot(aes(x = 0, y = rate)) +
  geom_jitter(mapping = aes(color = rate, fill = rate),
              show.legend = FALSE,
              height = 0,
              shape = 21,
              size = 3, 
              alpha = 0.7) +
  geom_text(aes(label = label), nudge_x = 1.5, hjust = 0) +
  scale_x_discrete(expand = expansion(add = c(1, 10))) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  scale_fill_gradient(low = unname(spcols["mustard"]),
                      high = unname(spcols["purple"]),
                      aesthetics = c("fill", "colour")) +
  my_ggtheme() +
  theme(axis.line.y = element_line(),
        axis.text.y = element_text())

ggsave(plot = gaelic_jitter,
       filename = "output/gaelic_jitter.svg", width = 8, height = 15, units = "cm")

scots_jitter <- scots_const %>% 
  as.data.frame() %>% 
  ggplot(aes(x = 0, y = rate)) +
  geom_jitter(aes(color = rate, fill = rate),
              show.legend = FALSE,
              height = 0,
              shape = 21,
              size = 3, 
              alpha = 0.7) +
  geom_text(aes(label = label), nudge_x = 1.5, hjust = 0) +
  scale_x_discrete(expand = expansion(add = c(1, 10))) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  scale_fill_gradient(low = unname(spcols["mustard"]),
                      high = unname(spcols["purple"]),
                      aesthetics = c("fill", "colour")) +
  my_ggtheme() +
  theme(axis.line.y = element_line(),
        axis.text.y = element_text())

ggsave(plot = scots_jitter,
       filename = "output/scots_jitter.svg", width = 8, height = 15, units = "cm")

