#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Collect country/year data (explanatory factors)
# Author:   @ChRauh (23.02.2025)
#########################################################################


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(countrycode)
library(readxl) 
library(igoR)
library(vdemdata) # https://github.com/vdeminstitute/vdemdata
library(desta) # https://github.com/pachadotdev/desta
library(imfr) # https://github.com/christophergandrud/imfr (no longer maintained ...)


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# Trade with EU ####
# From IMF / Direction of Trade Statistics (DOTS): https://data.imf.org/?sk=9D6028D4-F14A-464C-A2F2-59B2CD424B85&sId=1515614720959
# Assessed via Gandrud's imfr package
# # N-B. IMF API structure has also been updated in between, for now old calls still work as deprecated



# Available databases in IMF API
imf_ids() # DOT | Direction of Trade Statistics (DOTS) is waht we want

# Dimensions of DOT
# imf_codelist(database_id = "DOT")

# IMF area code lists
# Our target area here:	B0 | EU (Member States and Institutions of the European Union) changing composition
imf_ctry <- imf_codes(codelist = "CL_AREA_DOT")


# Indicators in DOT
# imf_codes(codelist = "CL_INDICATOR_DOT")
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


# EU exports to target country (FOB) 

eu_exports <- imf_data(database_id = "DOT" , # IMF API CALL
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

eu_exports_df$eu_exports_to_ctry <- eu_exports_df$eu_exports_to_ctry %>% as.numeric() # Clean up a bit



# EU imports from target country  (CIF)

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

eu_imports_df$eu_imports_from_ctry <- eu_imports_df$eu_imports_from_ctry %>% as.numeric() # Clean up a bit



# EU trade balance with target country
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

eu_tb_df$eu_tb_with_ctry <- eu_tb_df$eu_tb_with_ctry %>% as.numeric() # Clean up a bit



# Merge trade data 

eutrade <- eu_tb_df %>% # The longest one 
  left_join(eu_exports_df, by = c("ctry", "year")) %>% 
  left_join(eu_imports_df, by = c("ctry", "year")) %>% 
  mutate(year = as.numeric(year))

# Clean country names (cross-checked before)
# Note that this includes many non-state units 
eutrade$country <- countrycode(eutrade$ctry, origin = "iso2c", destination = "country.name")
eutrade <- eutrade %>% rename(iso2c = ctry)

# Export
write_rds(eutrade, paste0(data_path, "external_data/EU-Trade-By-Country.rds"))





# Regime type / liberal democracy index ####
# V-Dem Data

# Inspect what's in there
# cb <- codebook %>% 
#   arrange(metasection)
# cb %>% select(name, tag) %>% filter(str_detect(tag, "^v2x")) %>% arrange(tag) # Index variables
# v2x_polyarchy: Electoral democracy index (equals Prezworski's minimal definition of democracy)
# v2x_libdem: Liberal democracy index (Lührmann et al.)


# Extract data relevant for GP project
df.vdem <- vdem %>% 
  select(country_name, country_text_id, country_id, year, v2x_libdem, v2x_polyarchy) %>% 
  filter(year >= 1980) %>% 
  rename(vdem.libdem = v2x_libdem,
         vdem.elecdem = v2x_polyarchy)

# Clear country match variables

df.vdem$iso2c <- df.vdem$country_name %>% 
  str_replace("Türkiye", "Turkey") %>% 
  # str_replace("South Yemen", "Yemen") %>% # Tht seems to be off
  countrycode(origin = "country.name", destination = "iso2c")
# GDR, Kosovo, Somaliland, and Zanzibar not matched (the usual suspects)

df.vdem$iso2c[df.vdem$country_name == "German Democratic Republic"] <- "DD"
df.vdem$iso2c[df.vdem$country_name == "Kosovo"] <- "KV" # inofficial, Newsmap

# Palestine
# Vdem distinguishes Palestine/Gaza (country_text_id == PSG) from Palestine/West Bank (PSE) after 2007
# For completeness I go with the West Bank here ...
df.vdem <- df.vdem %>% filter(country_text_id != "PSG")


# Check for duplicates
df.vdem <- df.vdem %>% 
  mutate(id = paste0(country_text_id, "-", year)) %>% 
  mutate(dupl = duplicated(id))
sum(df.vdem$dupl) # Should be 0
df.vdem$dupl <- NULL

# Export
write_rds(df.vdem, paste0(data_path, "external_data/VDemIndicators.rds"))
gc()




# Countries with Free Trade agreements with the EU ####
# Design of Trade Agreements Database (DESTA) 

# Codebook: https://www.designoftradeagreements.org/media/filer_public/e2/46/e246b1d6-0992-4bdc-905d-26cb93a9f7fe/desta_codebook_02_00.pdf
# "EC is used throughout instead of EEC, EU ec."

# Check whether the EU occurs as an entity
countries <-
  treaties_dyads %>% 
  select(country1) %>% 
  unique() %>% 
  arrange(country1) # EU not in there
# Guess it intentioanlly stays on country level, while all EU ms should represent the EU
# Cf last figure at https://www.designoftradeagreements.org/visuals/
# So we can pick on country that was an EU member throughout (e.g. "Germany") and proxy the EU by that
# VERIFY!!!!


# Check time range
years <- 
  treaties_dyads %>% 
  select(year) %>% # Year of signature
  unique() %>% 
  arrange(year) # - ends in 2021
# most recent data? 
# Website says latest data version is December 2023, but maybe no agreements in the last two years?
# But EU-Mercosur?
# We probably should construct the data ourselves from the raw DESTA files for the final paper ...


# Conveniently, the packe contains a dyadic file listing every year


# Is it symmetric?
# If so, each set of countries should appear 2 times per year
test <- in_force_ftas_dyads %>% 
  filter(country1 %in% c("Germany", "Japan") & country2 %in% c("Germany", "Japan")) # Just one obs per year, sigh ...


# Extract all dyads including Germany

df.desta <- in_force_ftas_dyads %>% 
  filter(country1 == "Germany" | country2 == "Germany") %>% 
  mutate(country = ifelse(country1 == "Germany", country2, country1)) %>% # Keep always the non-Germany country in the dyad
  rename(eu.fta = in_force_fta) %>% 
  select(year, country, eu.fta) %>% 
  filter(year >= 1980)

table(df.desta$year) 
# Consistently only 95 obs per year, presumably all countries that had a Germany7eu FTA at least once
# In assembling the data downstream, all NA = 0
sum(df.desta$eu.fta)
sum(df.desta$eu.fta[df.desta$year == 2021]) # As expecetd as per https://www.designoftradeagreements.org/visuals/

# Crude LOCF

df.desta <- rbind(
  df.desta,
  df.desta %>% filter(year == 2021) %>% mutate(year = 2022),
  df.desta %>% filter(year == 2021) %>% mutate(year = 2023)
)

table(df.desta$year) 

# Iso2c code
df.desta <- df.desta %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))
df.desta$iso2c[df.desta$country == "Kosovo"] <- "KV" # Inoffical, newsmap

# Export
write_rds(df.desta, paste0(data_path, "external_data/EU-FTAs.rds"))





# Engagement in armed conflicts ####
# UCDP/PRIO Armed Conflict Dataset version 24.1: https://ucdp.uu.se/downloads/index.html#armedconflict

ucdp.raw <- read_rds(paste0(data_path, "external_data/ucdp-prio-acd-241.rds"))

# Data contains the following info relevant to us
# Primary and secondary conflict parties on both sides of the conflict in a given year
# Also encoded as Gleditsch/Ward country codes with are part of countrycode (here: the gwno variables), cf. http://ksgleditsch.com/data-4.html
# Note: Multiple, comma-separated entries per cell possible (sigh)
# intensity_level:  1 = Minor (1-999 battle deaths), 2 = War (> 1000 deaths)
# type_of_conflict: 1 extrasystemic, 2 = interstate, 3 = intrastate, 4 = internationalized intrastate
# incompatibility: 1 = about territory, 2 = about government, 3 = about both
# year 

ucdp <- ucdp.raw %>% 
  select(year, conflict_id, type_of_conflict, intensity_level, incompatibility, 
         gwno_a, gwno_a_2nd, gwno_b, gwno_b_2nd) %>% 
  mutate(across(starts_with("gwno"), as.character)) %>% # Some were numeric, some character (commas, see above ...)
  pivot_longer(cols = 6:9, names_to = "side", values_to = "ctry") %>% # For us its only important whther a country was involved in conflict in a given year (not which side it was)
  filter(!is.na(ctry)) %>% 
  mutate(side = str_remove(side, "gwno_")) %>% 
  separate_rows(ctry, sep = ", ") %>% # Split comma-separated values to one row per participating country
  mutate(ctry = as.numeric(ctry)) %>% 
  mutate(country = countrycode(ctry, origin = "gwn", destination = "country.name"),
         iso2c = countrycode(ctry, origin = "gwn", destination = "iso2c")) %>% 
  arrange(year, side)

# Some Failed matches .... 55, 751, 816, 972

# 816 - Vietnam, Democratic Republic of (as opposed to republic of ...)
# dict <- newsmap::data_dictionary_newsmap_en has only Vietnam entry
# ucdp$country[ucdp$ctry == 816] <- "Vietnam"
# ucdp$iso2c[ucdp$ctry == 972] <- "VN"
# We dpn't do this because this would duplicate obs during the Vietnam war (and only there ...)

# The others don't exist in the original list of states - http://ksgleditsch.com/data/iisystem.dat
# But some in the list of microstates: http://ksgleditsch.com/data/microstatessystem.dat
ucdp$country[ucdp$ctry == 55] <- "Grenada"
ucdp$iso2c[ucdp$ctry == 55] <- "GD"
ucdp$country[ucdp$ctry == 972] <- "Tonga"
ucdp$iso2c[ucdp$ctry == 972] <- "TO"

# Drop obs for which no country can be assigned
ucdp <- ucdp %>% filter(!is.na(country)) 


# Export different aggregations

ucdp %>% # All armed conflicts a country was engaged in
  group_by(year, country, iso2c) %>% 
  summarise(armed_conflicts = n()) %>% 
  ungroup() %>% 
  write_rds(paste0(data_path, "external_data/ArmedConflicts_By_CTRY_YEAR.rds"))

ucdp %>% 
  filter(intensity_level == 2) %>% # Wars only
  group_by(year, country, iso2c) %>% 
  summarise(wars = n()) %>% 
  ungroup() %>% 
  write_rds(paste0(data_path, "external_data/Wars_By_CTRY_YEAR.rds"))

ucdp %>% 
  filter(incompatibility != 2) %>% # Territorial conflicts only
  group_by(year, country, iso2c) %>% 
  summarise(terr_conflicts = n()) %>% 
  ungroup() %>% 
  write_rds(paste0(data_path, "external_data/TerritorialConflicts_By_CTRY_YEAR.rds"))


# Economic / GDP data ####
# From the IMF's world economic outlook
# https://www.imf.org/external/datamapper/NGDPD@WEO/OEMDC/ADVEC/WEOWORLD

# weo <- read_xls(paste0(data_path, "external_data/WEO.xls"), col_types = "text") # Fuckin Excel ...
weo <- read_delim(paste0(data_path, "external_data/WEO.csv"), delim = ";", locale = locale(decimal_mark = "."))


table(weo$`Subject Descriptor`)
table(weo$Units)

# GDP absolute (in current prices and US.Dollars !?)
gdp <- weo %>% 
  filter(`Subject Descriptor` == "Gross domestic product, current prices" &
           Units == 'U.S. dollars')
table(gdp$Scale) # Billions
gdp <- gdp %>% 
  select(-c(`WEO Country Code`, `Subject Descriptor`, Units, Scale, `Country/Series-specific Notes`, `Estimates Start After`)) %>% 
  rename(iso3 = ISO,
         country = Country) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "year", values_to = "gdp_billions") %>% 
  mutate(gdp_billions = na_if(gdp_billions, "n/a")) %>% 
  mutate(gdp_billions = str_remove_all(gdp_billions, fixed(","))) %>% # Watch out, excel ...
  mutate(year = as.numeric(year),
         gdp_billions = as.numeric(gdp_billions)) %>% 
  mutate(country = str_replace(country, "Türkiye", "Turkey")) %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))
gdp$iso2c[gdp$country == "Kosovo"] = "KV" # Inofficial, newsmap
gdp$iso2c[gdp$country == "Micronesia"] = "FM" 


# GDP per capita (in current prices and US.Dollars !?)
gdp_cap <- weo %>% 
  filter(`Subject Descriptor` == "Gross domestic product per capita, current prices" &
           Units == 'U.S. dollars')
table(gdp_cap$Scale) # Not specified
gdp_cap <- gdp_cap %>% 
  select(-c(`WEO Country Code`, `Subject Descriptor`, Units, Scale, `Country/Series-specific Notes`, `Estimates Start After`)) %>% 
  rename(iso3 = ISO,
         country = Country) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "year", values_to = "gdp_capita") %>% 
  mutate(gdp_capita = na_if(gdp_capita, "n/a")) %>% 
  mutate(gdp_capita = str_remove_all(gdp_capita, fixed(","))) %>% # Watch out, excel ...
  mutate(year = as.numeric(year),
         gdp_capita = as.numeric(gdp_capita)) %>% 
  mutate(country = str_replace(country, "Türkiye", "Turkey")) %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))
gdp_cap$iso2c[gdp_cap$country == "Kosovo"] = "KV" # Inofficial, newsmap
gdp_cap$iso2c[gdp_cap$country == "Micronesia"] = "FM" 


# GDP world share
gdp_world <- weo %>% 
  filter(`Subject Descriptor` == "Gross domestic product based on purchasing-power-parity (PPP) share of world total")
table(gdp_world$Scale) # Not specified
gdp_world <- gdp_world %>% 
  select(-c(`WEO Country Code`, `Subject Descriptor`, Units, Scale, `Country/Series-specific Notes`, `Estimates Start After`)) %>% 
  rename(iso3 = ISO,
         country = Country) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "year", values_to = "gdp_worldshare") %>% 
  mutate(gdp_worldshare = na_if(gdp_worldshare, "n/a")) %>% 
  mutate(gdp_worldshare = str_remove_all(gdp_worldshare, fixed(","))) %>% # Watch out, excel ...
  mutate(year = as.numeric(year),
         gdp_worldshare = as.numeric(gdp_worldshare)) %>% 
  mutate(country = str_replace(country, "Türkiye", "Turkey")) %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))
gdp_world$iso2c[gdp_world$country == "Kosovo"] = "KV" # Inofficial, newsmap
gdp_world$iso2c[gdp_world$country == "Micronesia"] = "FM" 


# GDP growth
gdp_growth <- weo %>% 
  filter(`Subject Descriptor` == "Gross domestic product, constant prices" &
           Units == "Percent change")
table(gdp_growth$Scale) # Not specified
gdp_growth <- gdp_growth %>% 
  select(-c(`WEO Country Code`, `Subject Descriptor`, Units, Scale, `Country/Series-specific Notes`, `Estimates Start After`)) %>% 
  rename(iso3 = ISO,
         country = Country) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "year", values_to = "gdp_growth") %>% 
  mutate(gdp_growth = na_if(gdp_growth, "n/a")) %>% 
  mutate(gdp_growth = str_remove_all(gdp_growth, fixed(","))) %>% # Watch out, excel ...
  mutate(year = as.numeric(year),
         gdp_growth = as.numeric(gdp_growth)) %>% 
  mutate(country = str_replace(country, "Türkiye", "Turkey")) %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))
gdp_growth$iso2c[gdp_growth$country == "Kosovo"] = "KV" # Inofficial, newsmap
gdp_growth$iso2c[gdp_growth$country == "Micronesia"] = "FM" 



# Assemble GDP data and export
econ <- gdp %>% 
  left_join(gdp_cap %>% select(iso2c, year, gdp_capita), by = c("iso2c", "year")) %>% 
  left_join(gdp_growth %>% select(iso2c, year, gdp_growth), by = c("iso2c", "year")) %>% 
  left_join(gdp_world %>% select(iso2c, year, gdp_worldshare), by = c("iso2c", "year"))
write_rds(econ, paste0(data_path, "external_data/WEO_GDP_By_CTRY_YEAR.rds"))




# Military Expenditure data ####
# SIPRI MilEx Data: https://www.sipri.org/databases/milex

sipri <- read_delim(paste0(data_path, "external_data/SIPRI-Milex-data-1948-2023_GDPshare_Cleaned.csv"), 
                    delim = ";", locale = locale(decimal_mark = ".")) %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "year", values_to = "milex_share") %>% 
  mutate(milex_share = milex_share %>% 
           str_replace_all(fixed(","), ".") %>% 
           str_remove_all(fixed("%")) %>% 
           na_if("...") %>% 
           as.numeric(),
         milex_share = milex_share / 100) %>% 
  rename(ctry = Country) %>% 
  mutate(ctry = str_replace(ctry, "Türkiye", "Turkey")) %>% 
  mutate(ctry = str_replace(ctry, "Czechoslovakia", "Czech Republic")) %>% 
  mutate(iso2c = countrycode(ctry, origin = "country.name", destination = "iso2c"),
         country = countrycode(iso2c, origin = "iso2c", destination = "country.name"))

# Clean unmatched cases
# "Yemen, North" is empty throughout

sipri$iso2c[sipri$ctry == "German Democratic Republic"] <- "DD"
sipri$country[sipri$ctry == "German Democratic Republic"] <- "German Democratic Republic"

sipri$iso2c[sipri$ctry == "Kosovo"] <- "KV" # Inofficial, nesmap
sipri$country[sipri$ctry == "Kosovo"] <- "Kosovo"

sipri$iso2c[sipri$ctry == "Yugoslavia"] <- "YU"
sipri$country[sipri$ctry == "Yugoslavia"] <- "Yugoslavia"

# There are two countries that create duplicates, both of which are completely empty, though
sipri <- sipri %>% 
  filter(ctry != "USSR") %>% # Russia with data from 1994 onwards
  filter(ctry != "Czech Republic") # Czechia with data from 1993 onwards





# Drop unidentified cases and export
sipri %>% 
  filter(!is.na(iso2c)) %>% 
  mutate(year = year %>% as.numeric()) %>% 
  write_rds(paste0(data_path, "external_data/MilitaryExpenditureShare_by_CTRY_YEAR.rds"))



# Build country/year panel ####


# Starting from the newsmap dictionary 
# as we use this to identify country mentions in Commission communication

# Extracting the country keys with a trick here ... 
# there might by smarter ways ...
nm.countries <- 
  newsmap::data_dictionary_newsmap_en %>% 
  unlist() %>% 
  names() %>% 
  as.data.frame() %>% 
  rename(ctry = 1) %>% 
  mutate(ctry = ctry %>% 
           str_extract("[A-Z0-9]*?$") %>% # LAst Digits are the ISO2 country
           str_remove_all("[0-9]")) %>% # KEys for individual country words removed here
  unique() %>% 
  arrange(ctry) %>% 
  mutate(country = countrycode(ctry, origin = "iso2c", destination = "country.name")) %>% 
  rename(iso2c = ctry)
nm.countries$country[nm.countries$iso2c == "KV"] <- "Kosovo" 

# The full range of years for which we have text data
# help <- read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% 
#   mutate(year = str_extract(date, "^[0-9]{4}"))
# min(help$year, na.rm = T) # 1985
# max(help$year, na.rm = T) # 2024
years <- seq(1985, 2024, 1)


# The full panel of newsmap countries over the range of years
panel <- data.frame(iso2c = rep(nm.countries$iso2c, length(years)),
                    country = rep(nm.countries$country, length(years)),
                    year = rep(years, nrow(nm.countries))) %>% 
  arrange(iso2c, year)


# Fill with the data collected above

cy <- 
  panel %>% 
  left_join(read_rds(paste0(data_path, "external_data/MilitaryExpenditureShare_by_CTRY_YEAR.rds")) %>% 
              select(iso2c, year, milex_share), 
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/WEO_GDP_By_CTRY_YEAR.rds")) %>% 
              select(iso2c, year, starts_with("gdp")), 
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/ArmedConflicts_By_CTRY_YEAR.rds")) %>% 
              select(iso2c, year, armed_conflicts),
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/Wars_By_CTRY_YEAR.rds")) %>% 
              select(iso2c, year, wars),
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/TerritorialConflicts_By_CTRY_YEAR.rds")) %>% 
              select(iso2c, year, terr_conflicts),
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/EU-FTAs.rds")) %>% 
              select(iso2c, year, eu.fta),
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/VDemIndicators.rds")) %>% 
              select(iso2c, year, starts_with("vdem")),
            by = c("iso2c", "year")) %>% 
  left_join(read_rds(paste0(data_path, "external_data/EU-Trade-By-Country.rds")) %>% 
              select(iso2c, year, starts_with("eu_")),
            by = c("iso2c", "year"))
  

# Check for duplicates
cy <- cy %>% 
  mutate(id = paste0(iso2c, "-", year),
         dupl = duplicated(id))

sum(cy$dupl) # Should be 0
cy$id <- NULL
cy$dupl <- NULL


# Check how many variables are missing by country/year
# 14 c/y variables in total 

cy <- cy %>%
  rowwise() %>% 
  mutate(missing_vars = sum(is.na(c_across(4:ncol(.))))) %>%
  ungroup()

max(cy$missing_vars)
min(cy$missing_vars)

test <- cy %>% filter(missing_vars == 0) # 115 ...

# Completely missing 'countries'
# N.B. newsmap goas quite low (Isle of Man, e.g)

cy$all_missing = cy$missing_vars == 14

missing_countries <- cy %>% 
  group_by(iso2c) %>% 
  summarise(missing_years = sum(all_missing)) %>% 
  ungroup() %>% 
  mutate(all_years_missing = missing_years == length(years)) %>% # Compare to full range
  arrange(desc(all_years_missing)) %>% 
  mutate(country = countrycode(iso2c, origin = "iso2c", "country.name"))

missing_countries$country[missing_countries$all_years_missing] # All small island states ...

cy <- cy %>% 
  filter(!(iso2c %in% missing_countries$iso2c[missing_countries$all_years_missing]))


# Completely missing years

n_countries_left <- length(unique(cy$iso2c))

missing_years <- cy %>% 
  group_by(year) %>% 
  summarise(missing_countries = sum(all_missing)) %>% 
  ungroup() %>% 
  mutate(all_countries_missing = missing_countries == n_countries_left) %>% # Compare to full range
  arrange(desc(all_countries_missing))
sum(missing_years$all_countries_missing) #0 

test <- cy %>% filter(year == 2024) # Only gdp data is available for 2024

# Drop 2024 from the country/year panel
cy <- cy %>% filter(year != 2024)


# Note that missings in the armed conflict data imply absence of conflicts (cf. above)
# Mark that accordingly here

cy <- cy %>% 
  mutate(armed_conflicts = replace_na(armed_conflicts, 0),
       wars = replace_na(wars, 0),
       terr_conflicts = replace_na(terr_conflicts, 0))




# Mark IGO (and EU) membership ####
# Based on COW/IGO data (which technically only goes through 2014), through igoR package
# https://cran.r-project.org/web/packages/igoR/vignettes/igoR.html
# https://correlatesofwar.org/data-sets/IGOs/
# https://correlatesofwar.org/wp-content/uploads/IGO-Codebook_v3_short-copy.pdf

# Maybe sifft through the list and filter more relavnt IGOs?

igom <- state_year_format3 %>% 
  filter(year >= 1985) %>% 
  select(ccode, year, state,
         eu, eec, nato, oecd, opec, osce, au, wto, who, mercosur, sco, asean, csto) %>%
  mutate(across(4:ncol(.), function(x){ifelse(x==1, 1, 0)})) %>% # Mutate IO membership so as to capture full membership only
  rename_with(~ paste0(., "_member"), .cols = 4:ncol(.)) %>% 
  mutate(eu_member = eu_member + eec_member) %>% # EEC and EU shouldn't overlap
  select(-eec_member) %>% 
  mutate(across(4:ncol(.), as.logical)) %>%
  mutate(iso2c = countrycode(ccode, "cown", "iso2c")) # COW codes here

# table(igom$eu, igom$year)

# Correct some non-matching COW-codes/ISOs
# unique(igom$state[is.na(igom$country)])
igom$iso2c[igom$state == "egermany"] <- "DD"
igom$iso2c[igom$state == "wgermany"] <- "DE"
igom$iso2c[igom$state == "czechoslovakia"] <- "CZ"
igom$iso2c[igom$state == "yugoslaviaserb"] <- "YU"
igom$iso2c[igom$state == "nyemen"] <- "YE"
igom$iso2c[igom$state == "kosovo"] <- "KV" 

test <- igom %>% filter(is.na(iso2c)) # Only south yemen

# Extrapolate to 2023 (strong assumption)
max(igom$year)
igom <- rbind(igom,
              igom %>% filter(year == 2014) %>% mutate(year = 2015),
              igom %>% filter(year == 2014) %>% mutate(year = 2016),
              igom %>% filter(year == 2014) %>% mutate(year = 2017),
              igom %>% filter(year == 2014) %>% mutate(year = 2018),
              igom %>% filter(year == 2014) %>% mutate(year = 2019),
              igom %>% filter(year == 2014) %>% mutate(year = 2020),
              igom %>% filter(year == 2014) %>% mutate(year = 2021),
              igom %>% filter(year == 2014) %>% mutate(year = 2022),
              igom %>% filter(year == 2014) %>% mutate(year = 2023))
igom$eu_member[igom$iso2c == "UK" & igom$year > 2016] <- F # Brexit


# Merge IGO membership data with the country/year panel

cy <- cy %>% 
  left_join(igom %>% select(-c(ccode, state)),
            by = c("year", "iso2c"))



# Export ####
cy  %>%  write_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds"))

