# load -------------------------------------------------------------------------

library(tidyverse)

source("R/functions/f_get_region.R")

simd_data <- readRDS("data/simd_data.rds") %>% 
  mutate(simdDomain = factor(simdDomain,
                             levels = c("simd", "income", "employment", 
                                        "education-skills-and-training", 
                                        "health", "crime", "housing", 
                                        "access-to-services"),
                             labels = c("SIMD", "Income", "Employment", 
                                        "Education, Skills and Training", 
                                        "Health", "Crime", "Housing", 
                                        "Access to Services")))

# constituency shares ----------------------------------------------------------

shares <- simd_data %>% 
  filter(measureType == "decile") %>% 
  rename(decile = value) %>% 
  mutate(Area_name = dz_code_to_const(refArea),
         Area_name = ifelse(Area_name == "Perthshire South and Kinross-shire", 
                            "Perthshire South and Kinrossshire", Area_name),
         Region = const_name_to_region(Area_name),
         Measure = case_when(decile <= 2 ~ "In most deprived fifth",
                             decile <= 4 ~ "In 2nd most deprived fifth", 
                             decile <= 6 ~ "In middle fifth",
                             decile <= 8 ~ "In 2nd least deprived fifth",
                             decile <= 10 ~ "In least deprived fifth"),
         Measure = factor(Measure, levels = c("In most deprived fifth",
                                              "In 2nd most deprived fifth", 
                                              "In middle fifth",
                                              "In 2nd least deprived fifth",
                                              "In least deprived fifth"), ordered = TRUE)) %>% 
  summarise(dzs = n(),
            .by = c(Region, Area_name, simdDomain, Measure)) %>% 
  group_by(Region) %>% 
  
  # ensure all SIMD groups are in dataset, even those with no DZs
  complete(Area_name, simdDomain, Measure) %>% 
  ungroup() %>% 
  mutate(Data = dzs/sum(dzs, na.rm = TRUE),
         .by = c(Region, Area_name, simdDomain)) %>% 
  replace_na(list(Data = 0)) %>% 
  arrange(Region, Area_name, simdDomain, Measure)

shares_Scot <- simd_data %>% 
  filter(measureType == "decile") %>% 
  rename(decile = value) %>% 
  mutate(Measure = case_when(decile <= 2 ~ "In most deprived fifth",
                             decile <= 4 ~ "In 2nd most deprived fifth", 
                             decile <= 6 ~ "In middle fifth",
                             decile <= 8 ~ "In 2nd least deprived fifth",
                             decile <= 10 ~ "In least deprived fifth"),
         Measure = factor(Measure, levels = c("In most deprived fifth",
                                              "In 2nd most deprived fifth", 
                                              "In middle fifth",
                                              "In 2nd least deprived fifth",
                                              "In least deprived fifth"), ordered = TRUE)) %>% 
  summarise(dzs = n(),
            .by = c(simdDomain, Measure)) %>% 
  mutate(Data = dzs/sum(dzs),
         Area_name = "Scotland",
         Region = NA,
         .by = c(simdDomain)) %>% 
  arrange(simdDomain, Measure)

# most deprived DZs lists ------------------------------------------------------

ranks <- simd_data %>% 
  pivot_wider(names_from = measureType, values_from = value) %>% 
  mutate(DataZone = dz_code_to_name(refArea), 
         IntermediateZone = dz_code_to_iz(refArea),
         Constituency = dz_code_to_const(refArea),
         Region = const_name_to_region(Constituency)) %>% 
  arrange(Region, Constituency, simdDomain, rank)

# save -------------------------------------------------------------------------

saveRDS(list(shares = rbind(shares, shares_Scot), ranks = ranks), "data/tidy_simd_data.rds")

