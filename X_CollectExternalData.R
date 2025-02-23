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


# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# V-Dem Data ####

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





# Design of Trade Agreements Database (DESTA).####

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

