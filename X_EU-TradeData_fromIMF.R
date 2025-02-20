# #################################################################
# Project:  TRIAS Geopolitics
# Task:     Collect trade volumes between EU and rest of the world
#           from IMF Direction of Trade Statistics (DOTS)
# Authors:  Christian Rauh (20.02.2023)
###################################################################

# Ultimate goal is to get country-by-country trade dependence on the EU
# measured as GDP share of EU trade volume (imports + exports)
# This script collects and cleans the trade volume data from IMF DOTs via the imfr package


# Packages ####
library(tidyverse)
library(countrycode)

library(imfr)
# Not on CRAN anymore: https://github.com/christophergandrud/imfr
# library(devtools)
# devtools::install_github("christophergandrud/imfr", auth_token = "XXX")
# N-B. IMF API structure has also been updated in between, for now old calls still work as deprecated


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB



# Available databases in IMF API
imf_ids() # DOT | Direction of Trade Statistics (DOTS) is waht we want

# Dimensions of DOT
imf_codelist(database_id = "DOT")

# IMF area code lists
# Our target area here:	B0 | EU (Member States and Institutions of the European Union) changing composition
imf_ctry <- imf_codes(codelist = "CL_AREA_DOT")


# Indicators in DOT
imf_codes(codelist = "CL_INDICATOR_DOT")
# TXG_FOB_USD | Goods, Value of Exports, Free on board (FOB), US Dollars
# TMG_FOB_USD | Goods, Value of Imports, Free on board (FOB), US Dollars | THIS ONE RESULTS IN EMMPTY API CALLS
# TMG_CIF_USD | Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars
# TBG_USD | Goods, Value of Trade Balance, US Dollars | Not entirely sure what this captures, though

# NOTE:
# CIF: the seller takes responsibility for the costs and risks associated with the shipment
# FOB: puts the responsibility on the buyer from the moment the shipment leaves the port of origin

# I get outgoing FOB and incoming CIF - so its always on the cost side of target countries
# Cross-check this with the trade balance data below

# CHECK: I guess this is in million USD?



# EU exports to target country (FOB) ####

# IMF API CALL
eu_exports <- imf_data(database_id = "DOT" , 
                     indicator = c("TXG_FOB_USD") , 
                     country = "B0", 
                     start = 1970, end = 2024, 
                     return_raw = TRUE)

# Unnest time series
eu_exports_df <- data.frame(NULL)

for (i in 1:nrow(eu_exports)) {
  current <- eu_exports$Obs[i][[1]] %>% 
    rename(year = 1, eu_exports_to_ctry = 2)
  current$ctry <- eu_exports$`@COUNTERPART_AREA`[i]
  eu_exports_df <- rbind(eu_exports_df, current)
  rm(current)
}


# Clean up a bit
eu_exports_df$eu_exports_to_ctry <- eu_exports_df$eu_exports_to_ctry %>% as.numeric()




# EU imports from target country  (CIF) ####

eu_imports <- imf_data(database_id = "DOT" , 
                       indicator = c("TMG_CIF_USD") , 
                       country = "B0", 
                       start = 1970, end = 2024, 
                       return_raw = TRUE)

# Unnest time series
eu_imports_df <- data.frame(NULL)

for (i in 1:nrow(eu_imports)) {
  current <- eu_imports$Obs[i][[1]] %>% 
    rename(year = 1, eu_imports_from_ctry = 2)
  current$ctry <- eu_imports$`@COUNTERPART_AREA`[i]
  eu_imports_df <- rbind(eu_imports_df, current)
  rm(current)
}

# Clean up a bit
eu_imports_df$eu_imports_from_ctry <- eu_imports_df$eu_imports_from_ctry %>% as.numeric()




# EU trade balance to target country ####
eu_tb <- imf_data(database_id = "DOT" , 
                       indicator = c("TBG_USD") , 
                       country = "B0", 
                       start = 1970, end = 2024, 
                       return_raw = TRUE)

# Unnest time series
eu_tb_df <- data.frame(NULL)

for (i in 1:nrow(eu_tb)) {
  current <- eu_tb$Obs[i][[1]] %>% 
    rename(year = 1, eu_tb_with_ctry = 2)
  current$ctry <- eu_tb$`@COUNTERPART_AREA`[i]
  eu_tb_df <- rbind(eu_tb_df, current)
  rm(current)
}

# Clean up a bit
# N.B. countrycode solution has been inspected in dev script
eu_tb_df$eu_tb_with_ctry <- eu_tb_df$eu_tb_with_ctry %>% as.numeric()



# Merge data ###

eutrade <- eu_tb_df %>% # The longest one 
  left_join(eu_exports_df, by = c("ctry", "year")) %>% 
  left_join(eu_imports_df, by = c("ctry", "year"))

# Clean country names (cross-checked before)
eutrade$country <- countrycode(eutrade$ctry, origin = "iso2c", destination = "country.name")

write_rds(eutrade, paste0(data_path, "external_data/EU-Trade-By-Country.rds"))



# Inspect balance of trade ####
# How much is the CIF off?
eutrade <-
  eutrade  %>% 
  mutate(tb = eu_exports_to_ctry - eu_imports_from_ctry)
plot(eutrade$eu_tb_with_ctry, eutrade$tb, na.rm = T) # Perfect






