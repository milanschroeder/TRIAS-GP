#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Plot Country Salience Maps over Time
# Author:   @milanschroeder (23.04.2025)
#########################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode)
library(GGally) # Extension to 'ggplot2' CRAN v2.1.2
library(grid)
library(gridExtra)
library(gtable)
library(patchwork)
library(ggtext)



# Paths ####
# Needed as big files currently not part of the repo

 data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Load the data #####

# Scale values of sentences
# Picking three - the two Glove Scale with pluasible face validity, and the LSX scale validated in Russia paper
glove <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_sentlevel.rds"))  %>% 
  mutate(gti = max(gti, na.rm = T) + min(gti, na.rm = T) - gti, # Get directionality right, more "hostile" language should have higher values (correct downstream)
         coop_confl = max(coop_confl, na.rm = T) + min(coop_confl, na.rm = T) - coop_confl,
         friend_foe = max(friend_foe, na.rm = T) + min(friend_foe, na.rm = T) - friend_foe) %>% 
  rename_with(~ paste0(., "_glove"), .cols = 2:last_col())

lsx <- read_rds(paste0(data_path, "cleaned_data/scaling_lsx_sentlevel.rds")) %>% 
  select(-doc_id) %>% 
  rename_with(~ paste0(., "_lsx"), .cols = 2:last_col())

# Sentence data
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  mutate(wordcount = str_count(text_sent, "\\s+")) %>% 
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  left_join(glove %>% select(sentence_id, coop_confl_glove, friend_foe_glove), by = "sentence_id") %>% 
  left_join(lsx %>% select(sentence_id, coop_confl_lsx), by = "sentence_id") %>% 
  filter(lang_sent == "en") # only English sentences

# Country mentions - long form, English sentences only
cm <- read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>% 
  left_join(glove %>% select(sentence_id, coop_confl_glove, friend_foe_glove), by = "sentence_id") %>% 
  left_join(lsx %>% select(sentence_id, coop_confl_lsx), by = "sentence_id") %>% 
  left_join(sent %>% select(sentence_id, doc_type, year))

# Filter country mentions 
# to include only those states in larger data sets
# and exclude EU member states

countries <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% 
  select(iso2c, year, eu_member)

cm <- cm %>% 
  filter(iso2c %in% unique(countries$iso2c)) %>% 
  left_join(countries, by = c("iso2c", "year")) %>% 
  filter(!eu_member) %>% 
  select(-eu_member)


# Filter document types
cm <- cm %>% 
  filter(doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement"))

# Clean up
rm(glove, lsx)
gc()



# Packages - for mapping
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Get a 'world' data frame from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_a3[world$iso_a3_eh == "FRA"] <- "FRA" # Missing ISO for some reason
world$iso_a3[world$iso_a3_eh == "NOR"] <- "NOR" # Missing ISO for some reason
world$iso_a2[world$iso_a3_eh == "FRA"] <- "FRA" # Missing ISO for some reason
world$iso_a2[world$sovereignt == "Kosovo"] <- "KV" # Match newsmap code
world$iso2c <- world$iso_a2 # For matching

# Calculate data for timeperiods

df85_89 <- cm %>% 
  filter(year >= 1985 & year <= 1989) %>% 
  group_by(iso2c) %>% 
  summarise(cc85_89 = mean(coop_confl_glove, na.rm = T),
            mentions85_89 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc85_89 = scales::rescale(mentions85_89, to = c(0, 1)),
         cc85_89 = mentions_sc85_89)
# mutate(mentions_sc85_89 = scale(mentions85_89)[,1]) # Centers and standardizes mentions

df90_00 <- cm %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(iso2c) %>% 
  summarise(cc90_00 = mean(coop_confl_glove, na.rm = T),
            mentions90_00 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc90_00 = scales::rescale(mentions90_00, to = c(0, 1)),
         cc90_00 = mentions_sc90_00)
# mutate(mentions_sc90_00 = scale(mentions90_00)[,1]) # Centers and standardizes mentions

df01_10 <- cm %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  group_by(iso2c) %>% 
  summarise(cc01_10 = mean(coop_confl_glove, na.rm = T),
            mentions01_10 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc01_10 = scales::rescale(mentions01_10, to = c(0, 1)),
         cc01_10 = mentions_sc01_10)
# mutate(mentions_sc01_10 = scale(mentions01_10)[,1]) # Centers and standardizes mentions

df11_23 <- cm %>% 
  filter(year >= 2011 & year <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(cc11_23 = mean(coop_confl_glove, na.rm = T),
            mentions11_23 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc11_23 = scales::rescale(mentions11_23, to = c(0, 1)),
         cc11_23 = mentions_sc11_23)
# mutate(mentions_sc11_23 = scale(mentions11_23)[,1]) # Centers and standardizes mentions

dfall <- cm %>% 
  group_by(iso2c) %>% 
  summarise(ccall = mean(coop_confl_glove, na.rm = T),
            mentionsall = n()) %>% 
  ungroup() %>% 
  mutate(mentions_scall = scales::rescale(mentionsall, to = c(0, 1)),
         ccall = mentions_scall)
#mutate(mentions_scall = scale(mentionsall)[,1]) # Centers and standardizes mentions


# Merge to world data
world <- world %>% 
  left_join(dfall, by = "iso2c") %>% 
  left_join(df85_89, by = "iso2c") %>% 
  left_join(df90_00, by = "iso2c") %>% 
  left_join(df01_10, by = "iso2c") %>% 
  left_join(df11_23, by = "iso2c")

# Individual maps per period

pl.all <-
  ggplot(data = world) +
  geom_sf(aes(fill = mentions_scall
              ), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
 # scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                   #    mid = "grey50",
                       na.value = "white",
                       # midpoint = mean(dfall$ccall, na.rm = T), 
                       # limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                       #            mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "Full period (1985-2023)",
       fill = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))

pl.85_89 <-
  ggplot(data = world) +
  geom_sf(aes(fill = mentions_sc85_89
              ), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
 # scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                     #  mid = "grey50",
                       na.value = "white",
                       # midpoint = mean(dfall$ccall, na.rm = T), 
                       # limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                       #            mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1985-1990",
       fill = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))


pl.90_00 <-
  ggplot(data = world) +
  geom_sf(aes(fill = mentions_sc90_00
              ), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                    #   mid = "grey50",
                       na.value = "white",
                       # midpoint = mean(dfall$ccall, na.rm = T), 
                       # limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                       #            mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1990-2000",
       fill = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))


pl.01_10 <-
  ggplot(data = world) +
  geom_sf(aes(fill = mentions_sc01_10
              ), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       na.value = "white",
                       # midpoint = mean(dfall$ccall, na.rm = T), 
                       # limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                       #            mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2001-2010",
       fill = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))

pl.11_23 <-
  ggplot(data = world) +
  geom_sf(aes(fill = mentions_sc11_23
              ), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
 # scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                     #  mid = "blue",
                       na.value = "white",
                       # midpoint = mean(dfall$ccall, na.rm = T), 
                       # limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                       #            mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2011-2023",
       fill = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))

# Combined plot
pl.comb <- 
  pl.all+((pl.85_89+pl.90_00)/(pl.01_10+pl.11_23))+
  plot_annotation(title = "Which Foreign Countries does the European Commission publicly communicate about over time?",
                #  subtitle = "Color indicates indicates the normalized frequency by which the Commission has meantioned the country.",
                  caption = "\nBased on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.\nCountries appearing fully white/invisible are not mentioned in the respective period.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5))) +  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        legend.box = "vertical") 


ggsave("./output/descriptive_plots/Salience_Maps_OverTime_withEU.png", pl.comb, height = 22, width = 36, units = "cm")


