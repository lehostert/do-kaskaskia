library(tidyverse)

#Read in data for EPA sites, and the known IDNR basin surveys
epa <- readxl::read_excel("~/CREP/Data/IDNR_basin_surveys/il_epastation_2020_bmetzke.xlsx")
basin_stein <- 
basin_drake <-
  
# Read in local watershed landscape information

  
  
#Filter EPA Sites to include only kaskty sites
epa <- epa %>% 
  filter(str_detect(PUGAP_CODE, 'kasky') & TYPE != "LAKE")

Compare l