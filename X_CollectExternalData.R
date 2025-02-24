#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Collect country/year data on regime and free trade agreements
# Author:   @ChRauh (23.02.2025)
#########################################################################


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(countrycode)
library(vdemdata) # https://github.com/vdeminstitute/vdemdata
library(desta) # https://github.com/pachadotdev/desta
library(readxl)


# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


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

df.vdem$iso2c <- countrycode(df.vdem$country_name, origin = "country.name", destination = "iso2c")
# GDR, Kosovo, Somaliland, South Yemen, and Zanzibar not matched (the usual suspects)

df.vdem$iso2c[df.vdem$country_name == "German Democratic Republic"] <- "DD"
df.vdem$iso2c[df.vdem$country_name == "Kosovo"] <- "XK" # inofficial


# Export
write_rds(df.vdem, paste0(data_path, "external_data/VDemIndicators.rds"))
rm(df.vdem)
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
sum(df.desta$eu.fta[df.desta$year == 2021]) # As expetcetd as per https://www.designoftradeagreements.org/visuals/

# Crude LOCF

df.desta <- rbind(
  df.desta,
  df.desta %>% filter(year == 2021) %>% mutate(year = 2022),
  df.desta %>% filter(year == 2021) %>% mutate(year = 2023)
)

table(df.desta$year) 

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
gdp$iso2c[gdp$country == "Kosovo"] = "XK" # Inoffcial
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
gdp_cap$iso2c[gdp_cap$country == "Kosovo"] = "XK" # Inofficial
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
gdp_world$iso2c[gdp_world$country == "Kosovo"] = "XK" # Inofficial
gdp_world$iso2c[gdp_world$country == "Micronesia"] = "FM" 


# GDP growth
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
gdp_world$iso2c[gdp_world$country == "Kosovo"] = "XK" # Inofficial
gdp_world$iso2c[gdp_world$country == "Micronesia"] = "FM" 
