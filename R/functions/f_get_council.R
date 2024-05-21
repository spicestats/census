# function to convert S12 codes for councils to council names

# needs to cover all codes (including decommissioned ones; should all be in GSS file)

# import GSS area codes
gss_codes_la <- readxl::read_excel("data/gss_codes.xlsx",
                                   sheet = "S12_CA") %>% select(InstanceCode, InstanceName) %>% 
  rename(council_code = InstanceCode,
         council = InstanceName)


# constituency S16 code to constituency name
council_code_to_name <- function(x) {
  
  sapply(x, function(y) {
    gss_codes_la %>% 
      filter(tolower(council_code) == tolower(y)) %>% 
      pull(council)
  })
}

