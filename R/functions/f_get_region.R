# note this is a bit inelegant and slow - maybe refactor;

# main thing is to combine const. codes from lookup and gss file to cover all
# codes (including decommissioned ones)

# functions to convert the S16 codes for constituencies into the constituency
# names; and the names to regions

# 1 get lookup file from SPC to SPR
const_region_lookup <- readxl::read_excel("data/region_lookup.xlsx", skip = 1) %>% 
  rename(region = 2,
         const_code = 3,
         constituency = 4) %>% 
  select(region, const_code, constituency) %>% 
  arrange(const_code)

# 2 get full list of SPC codes and names from GSS file
gss_codes <- readxl::read_excel("data/gss_codes.xlsx",
                                sheet = "S16_SPC") %>% 
  select(InstanceCode, InstanceName) %>% 
  rename(const_code = InstanceCode,
         constituency = InstanceName)

# 3 add region name to it
lookup <- gss_codes %>% 
  left_join(const_region_lookup %>% 
              select(constituency, region),
            by = "constituency") %>% 
  mutate(constituency = case_when(constituency == "Perthshire South and Kinross-shire" 
                                  ~ "Perthshire South and Kinrossshire",
                                  TRUE ~ constituency))

# constituency S16 code to constituency name
const_code_to_name <- function(x) {
  
  data.frame(input = x) %>% 
    left_join(lookup %>% 
                select(constituency, const_code), 
              by = c(input = "const_code")) %>% 
    select(constituency) %>% 
    pull()

}

# constituency name to region
const_name_to_region <- function(x) {
  
  data.frame(input = x) %>% 
    left_join(lookup %>% 
                select(region, constituency) %>% 
                distinct() , 
              by = c(input = "constituency")) %>% 
    select(region) %>% 
    pull()

}

# OA code to const



