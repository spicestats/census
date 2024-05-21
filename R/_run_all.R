
# 01 download data -------------------------------------------------------------

source("R/01_download_data/get_ASHE_data.R")
source("R/01_download_data/get_cilif_data.R")
source("R/01_download_data/get_gss_codes.R")
source("R/01_download_data/get_housing_data.R")
source("R/01_download_data/get_inflation_data.R")
source("R/01_download_data/get_labourmarket_data.R")
source("R/01_download_data/get_lifeexpectancy_data.R")
# source("R/01_download_data/get_PAYE_data.R")
source("R/01_download_data/get_population_data.R")
source("R/01_download_data/get_simd_data.R")
source("R/01_download_data/get_socialsecurity_data.R")

# 02 prep data -----------------------------------------------------------------

source("R/02_prep_data/prep_ASHE_data.R")
source("R/02_prep_data/prep_cilif_data.R")
source("R/02_prep_data/prep_housing_data.R")
source("R/02_prep_data/prep_labourmarket_data.R")
source("R/02_prep_data/prep_lifeexpectancy_data.R")
# source("R/02_prep_data/prep_PAYE_data.R")
source("R/02_prep_data/prep_population_data.R")
source("R/02_prep_data/prep_simd_data.R")
source("R/02_prep_data/prep_socialsecurity_data.R")

# 03 spreadsheet for powerBI dashboard -----------------------------------------

source("R/03_spreadsheet_powerbi.R")

# 03 present data --------------------------------------------------------------

source("R/03_make_charts/charts_earnings.R")
source("R/03_make_charts/charts_housing.R")
source("R/03_make_charts/charts_labourmarket.R")
source("R/03_make_charts/charts_poverty.R")

# 04 create website ------------------------------------------------------------

source("R/04_website.R")


