# convert the GSS code for intermediate zones to names

IZ_code_to_name <- function(x) {
  
  gss_codes <- readxl::read_excel("data/gss_codes.xlsx",
                                  sheet = "S02_IZ") %>% 
    # get list of IZ codes and names from GSS file
    select(InstanceCode, InstanceName) %>% 
    rename(IZ_code = InstanceCode,
           IZ = InstanceName)
  
  data.frame(input = x) %>% 
    left_join(gss_codes, by = c(input = "IZ_code")) %>% 
    select(IZ) %>% 
    pull()
  
}
