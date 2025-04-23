#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Describe selected country/scale values
# Author:   @ChRauh (25.02.2025)
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


# Benchmarks ####

mean_cm_ccg <- mean(cm$coop_confl_glove, na.rm = T) # Means in foreign country mentions
mean_cm_ffg <- mean(cm$friend_foe_glove, na.rm = T)
mean_cm_ccl <- mean(cm$coop_confl_lsx, na.rm = T)

sd_cm_ccg <- sd(cm$coop_confl_glove, na.rm = T) # sds in foreign country mentions
sd_cm_ffg <- sd(cm$friend_foe_glove, na.rm = T)
sd_cm_ccl <- sd(cm$coop_confl_lsx, na.rm = T)

mean_sent_ccg <- mean(sent$coop_confl_glove, na.rm = T) # Means in all English sentences
mean_sent_ffg <- mean(sent$friend_foe_glove, na.rm = T)
mean_sent_ccl <- mean(sent$coop_confl_lsx, na.rm = T)

country_means <- cm %>% # Country means
  select(iso2c, country, coop_confl_glove, friend_foe_glove, coop_confl_lsx) %>% 
  group_by(iso2c, country) %>% 
  summarise(coop_confl_glove = mean(coop_confl_glove, na.rm = T),
            friend_foe_glove = mean(friend_foe_glove, na.rm = T),
            coop_confl_lsx = mean(coop_confl_lsx, na.rm = T)) %>% 
  ungroup()


# Over time ####

#ggplot(cm %>% sample_n(50000), aes(x = year, y = coop_confl_glove, label = country)) +
pl.all <- 
  ggplot(cm, aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "All foreign countries",
       y = "cooperative/conflictual\nlanguage\n",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.us <- 
  ggplot(cm %>% filter(iso2c == "US"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "United States",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.br <- 
  ggplot(cm %>% filter(iso2c == "BR"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "Brazil",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.ru <- 
  ggplot(cm %>% filter(iso2c == "RU"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "Russia",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.in <- 
  ggplot(cm %>% filter(iso2c == "IN"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "India",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.cn <- 
  ggplot(cm %>% filter(iso2c == "CN"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "China",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.za <- 
  ggplot(cm %>% filter(iso2c == "ZA"), aes(x = year, y = coop_confl_glove, label = country)) +
  geom_hline(yintercept = mean_cm_ccg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = coop_confl_glove), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ccg, 
                         limits = c(mean_cm_ccg-2*sd_cm_ccg,
                                    mean_cm_ccg+2*sd_cm_ccg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "South Africa",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

# Lower panel - US + brics

pl.lower <- (pl.us+pl.br)/(pl.ru+pl.in)/(pl.cn+pl.za)


# Combined plot
pl <- (pl.all+pl.lower)+
  # plot_layout(heights = c(.7,1))+
  plot_annotation(title = "How the European Commission publicly communicates about foreign countries",
                  subtitle = "Each dot represents one sentence in which a non-EU state is mentioned.<br>Y-axis and color indicate whether this sentence uses <span style='color:blue; font-weight:bold;'>cooperative</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:red; font-weight:bold;'>conflictual</span> language.",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  


# ggsave("./output/descriptive_plots/CoopConflictCountries_OverTime.png", pl, height = 32, width = 24, units = "cm")
ggsave("./output/descriptive_plots/CoopConflictCountries_OverTime.png", pl, height = 22, width = 36, units = "cm")

# Clean up
rm(list = ls(pattern = "^pl"))
gc()



# Maps ####

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
  mutate(mentions_sc85_89 = scales::rescale(mentions85_89, to = c(0, 1)))
# mutate(mentions_sc85_89 = scale(mentions85_89)[,1]) # Centers and standardizes mentions

df90_00 <- cm %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(iso2c) %>% 
  summarise(cc90_00 = mean(coop_confl_glove, na.rm = T),
            mentions90_00 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc90_00 = scales::rescale(mentions90_00, to = c(0, 1)))
# mutate(mentions_sc90_00 = scale(mentions90_00)[,1]) # Centers and standardizes mentions

df01_10 <- cm %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  group_by(iso2c) %>% 
  summarise(cc01_10 = mean(coop_confl_glove, na.rm = T),
            mentions01_10 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc01_10 = scales::rescale(mentions01_10, to = c(0, 1)))
# mutate(mentions_sc01_10 = scale(mentions01_10)[,1]) # Centers and standardizes mentions

df11_23 <- cm %>% 
  filter(year >= 2011 & year <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(cc11_23 = mean(coop_confl_glove, na.rm = T),
            mentions11_23 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc11_23 = scales::rescale(mentions11_23, to = c(0, 1)))
# mutate(mentions_sc11_23 = scale(mentions11_23)[,1]) # Centers and standardizes mentions

dfall <- cm %>% 
  group_by(iso2c) %>% 
  summarise(ccall = mean(coop_confl_glove, na.rm = T),
            mentionsall = n()) %>% 
  ungroup() %>% 
  mutate(mentions_scall = scales::rescale(mentionsall, to = c(0, 1)))
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
  geom_sf(aes(fill = ccall, alpha = mentions_scall), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ccall, na.rm = T), 
                       limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                                  mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "Full period (1985-2023)",
       fill = "Cooperative/conflictual language around country mentions: \n ",
       alpha = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))

pl.85_89 <-
  ggplot(data = world) +
  geom_sf(aes(fill = cc85_89, alpha = mentions_sc85_89), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ccall, na.rm = T), 
                       limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                                  mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1985-1990",
       fill = "Cooperative/conflictual language around country mentions: \n ",
       alpha = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))


pl.90_00 <-
  ggplot(data = world) +
  geom_sf(aes(fill = cc90_00, alpha = mentions_sc90_00), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ccall, na.rm = T), 
                       limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                                  mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1990-2000",
       fill = "Cooperative/conflictual language around country mentions: \n ",
       alpha = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))


pl.01_10 <-
  ggplot(data = world) +
  geom_sf(aes(fill = cc01_10, alpha = mentions_sc01_10), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ccall, na.rm = T), 
                       limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                                  mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2001-2010",
       fill = "Cooperative/conflictual language around country mentions: \n ",
       alpha = "Normalized frequency of country mentions:",
       x= "", y = "")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='white', color= "white"),
        plot.title = element_text(size = 10, face = "bold", hjust = .5))

pl.11_23 <-
  ggplot(data = world) +
  geom_sf(aes(fill = cc11_23, alpha = mentions_sc11_23), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ccall, na.rm = T), 
                       limits = c(mean(dfall$ccall, na.rm = T)-2*sd(dfall$ccall, na.rm = T),
                                  mean(dfall$ccall, na.rm = T)+2*sd(dfall$ccall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2011-2023",
       fill = "Cooperative/conflictual language around country mentions: \n ",
       alpha = "Normalized frequency of country mentions:",
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
  plot_annotation(title = "How the European Commission publicly communicates about foreign countries",
                  subtitle = "Color indicates whether sentences mentioning the country on average use <span style='color:blue; font-weight:bold;'>cooperative</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:red; font-weight:bold;'>conflictual</span> language.<br>Transparency indicates the normalized frequency by which the Commission has meantioned the country.<br>",
                  caption = "\nBased on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.\nCountries appearing fully white/invisible are either EU member states or not mentioned in the respective period.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5))) +  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        legend.box = "vertical") 


ggsave("./output/descriptive_plots/CoopConflictCountries_Maps_OverTime.png", pl.comb, height = 22, width = 36, units = "cm")





# Analyse salience of countries ####


# Document level data ####

# Load and filter (should be equvalently done for all other data)
docs <- read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% 
  # Language filter
  filter(main_lang_doc == "en") %>%
  # Doc type filter
  filter(doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement")) %>% 
  mutate(doc_type = ifelse(doc_type == "Daily news", "News", doc_type)) %>% 
  mutate(doc_type = factor(doc_type, levels = c("Press release", "Speech", "News", "Statement", "Read-out", "Country insights"))) %>% 
  # Year var
  mutate(year = str_extract(date, "^[0-9]{4}"))

# Data descriptives

doc_types <- docs %>% 
  group_by(doc_type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  rename(`Doc type` = 1,
         `Freq.` = 2)
doc_type_colors <- c("#1f78b4", "#33a02c", "#a6cee3", "#b2df8a", "#fd8d3c", "#fdbe85")
doc_type_table <- tableGrob(doc_types, rows = NULL)
for (i in seq_len(nrow(doc_types))) {
  bg <- gpar(fill = doc_type_colors[i], col = "black")  # Set fill and border color
  doc_type_table$grobs[doc_type_table$layout$t == i + 1] <- lapply(doc_type_table$grobs[doc_type_table$layout$t == i + 1], 
                                                                   function(g) { g$gp <- bg; g })
}
grid.draw(doc_type_table)

pl.annual <- docs %>% 
  group_by(year, doc_type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = count, fill = fct_rev(doc_type))) +
  geom_col(width = .7) +
  scale_fill_manual(values = rev(doc_type_colors)) +
  labs(x = "", y= "", title = "Document coverage in European Commission public communication corpus")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color ="black", size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5),
        plot.title = element_text(face = "bold", size = 14))

pl.annual + annotation_custom(grob = doc_type_table, 
                              xmin = 1985, xmax = 1995, 
                              ymin = 2000, ymax = 3000)

comp.pl <- pl.annual + annotation_custom(grob = doc_type_table, xmin = 5, xmax = 5 , ymin = 2000, ymax = 2900)
ggsave("./output/descriptive_plots/CorpusDescription.png", comp.pl, height = 15, width = 26, units = "cm")

rm(comp.pl, annual, pl.annual, bg, doc_types, doc_type_table, doc_type_colors, i)



# Country mentions - doc level ####

cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_doclevel.rds")) %>% 
  filter(doc_id %in% docs$doc_id) %>% # Filter as above
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  select(c(doc_id, year, BI:ncol(.))) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") %>%  # Easier to handle
  left_join(., countrycode::codelist %>% select(iso2c, continent), join_by(iso2c))


# External country/year panel ####

cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data states (= official states at some point)
length(unique(cp$iso2c)) # 213
# Newsmap dictionary is somewhat broader
length(unique(cm$iso2c)) # 241
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))



# Describe salience of foreign countries over time ####
# Share of docs (per year) that mention a country at least once as the most straightforward measure

# Only non-eu countries
cm.foreign <- cm %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(!eu_member) # %>% # Filter
  select(-eu_member)

cm.eu <- cm %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(eu_member) # %>% # Filter
  select(-eu_member)

  cm.all <- cm %>% 
    left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% 
    left_join(., countrycode::codelist %>% select(iso2c, region), join_by(iso2c)) %>% 
    mutate(us = (iso2c == "US" & mentions > 0), # Single out US + BRICS
           br = (iso2c == "BR" & mentions > 0),
           ru = (iso2c == "RU" & mentions > 0),
           ua = (iso2c == "UA" & mentions > 0),
           `in` = (iso2c == "IN" & mentions > 0),
           cn = (iso2c == "CN" & mentions > 0),
           za = (iso2c == "ZA" & mentions > 0),
           asia = (continent == "Asia" & mentions > 0),
           europe = (continent == "Europe" & mentions > 0),
           africa = (continent == "Africa" & mentions > 0),
           oceania = (continent == "Oceania" & mentions > 0),
           samerica = (region == "Latin America & Caribbean" & mentions > 0),
           namerica = (region == "North America" & mentions > 0),
           others = (!iso2c %in% c("ZA", "CN", "IN", # "UA", 
                                   "RU", "BR", "US") & mentions > 0), # only BRICS+US for now
           eu = (eu_member & mentions > 0),
           war = ((iso2c == "UA" | iso2c == "RU") & mentions > 0)
    ) 

# Share of docs with country


  
cm.all %>% filter(mentions > 0 & year < 2024 & !eu) %>% 
    mutate(
      iso2c = ifelse(eu, "EU countries", iso2c), 
      country = ifelse(iso2c %in% c("ZA", "CN", "IN", "RU", "BR", "US", "EU countries"), 
                            iso2c,
                            "other (non-EU)")) %>% 
#  filter(country != "other (non-EU)") %>% 
  
    select(year, country) %>% 
  ggplot(aes(x = year, fill = factor(country, levels = c("other (non-EU)", "BR", "IN", "ZA", "CN", "RU", "US"))))+
    geom_bar(position = "fill")+
  #viridis::scale_fill_viridis(discrete = T, direction = -1) +
  scale_fill_manual(values = c("ZA" = "#1f78b4", "CN" = "#33a02c", "IN" = "#a6cee3", "RU" = "#b2df8a", "BR"= "#fd8d3c", "US"= "#fdbe85","other (non-EU)" = "transparent"))+
  scale_y_continuous(labels = scales::percent_format())+
  theme_bw() + 
  labs(x = "", y= "", fill = "Country")+
  plot_annotation(
    title = "Does the Commission focus on major powers in its pubic communication?",
    subtitle = "Relative Country Focus among non-EU countries mentioned in European Commission Public Communication Documents, 1985-2023",
    caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
    theme = theme(plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, face = "bold")))


  
  ##########
  
shares <- 
 cm.all %>% 
  filter(!eu_member) # %>% # Filter
  select(-eu_member) %>% 
   
  group_by(doc_id, year) %>% 
  summarise(across(where(is.logical), sum, na.rm = T),
    fc = sum(mentions)
    #         us = sum(us),
    #         br = sum(br),
    #         ru = sum(ru),
    #         ua = sum(ua)
    #         `in` = sum(`in`),
    #         cn = sum(cn),
    #         za = sum(za)
    ) %>% 
  ungroup() %>% 
  select(-doc_id) %>% 
  group_by(year) %>% 
  summarise(across(where(is.numeric), ~mean(as.logical(.x), na.rm = T))
  # fc = mean(as.logical(mentions)), # Annual shares
  #           us = mean(as.logical(us)),
  #           br = mean(as.logical(br)),
  #           ru = mean(as.logical(ru)),
  #           ua = mean(as.logical(ua)),
  #           `in` = mean(as.logical(`in`)),
  #           cn = mean(as.logical(cn)),
  #           za = mean(as.logical(za))
  ) %>% 
  ungroup() 

plot_data = shares

# Individual plots

plot_theme <- theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 10, hjust = 0.5),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

pl.all <- 
  ggplot(plot_data, aes(x = year)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = fc), color = "lightblue", linewidth = 1) +
  geom_point(size = 4, color = "lightblue", aes(y = fc)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = others), color = "blue", linewidth = 1) +
  geom_point(size = 4, color = "blue", aes(y = others)) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Other countries</span> / <span style='color:lightblue; font-weight:bold;'>All foreign countries</span>", x = "", y = "") +
  plot_theme

pl.us <- 
  ggplot(plot_data, aes(x = year, y = us)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = namerica), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = namerica)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = us), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue") +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>US</span> / <span style='color:lightblue; font-weight:bold;'>North America</span>", x = "", y = "") +
  plot_theme

pl.br <- 
  ggplot(plot_data, aes(x = year, y = br)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = samerica), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = samerica)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = br), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue") +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Brazil</span> / <span style='color:lightblue; font-weight:bold;'>South America</span>", x = "", y = "") +
  plot_theme

pl.ru <- 
  ggplot(plot_data, aes(x = year, y = ru)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = war), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(x = year, y = war)) +
  geom_segment(aes(x = year+.2, xend = year+.2, y = 0, yend = ua), color = "yellow", linewidth = 1) +
  geom_point(size = 1, color = "yellow", aes(x = year+.2, y = ua)) +
  geom_segment(aes(x = year-.2, xend = year-.2, y = 0, yend = ru), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue", aes(x = year-.2)) +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Russia</span> <span style='color:lightblue; font-weight:bold;'>or</span> <span style='color:yellow; font-weight:bold;'>Ukraine</span>", x = "", y = "") +
  plot_theme

pl.ind <- 
  ggplot(plot_data, aes(x = year, y = `in`)) +
  geom_segment(aes(x = year-.1, xend = year-.1, y = 0, yend = oceania), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(x = year-.1, y = oceania)) +
  geom_segment(aes(x = year+.1, xend = year+.1, y = 0, yend = `in`), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue", aes(x = year+.1)) +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>India</span> / <span style='color:lightblue; font-weight:bold;'>Oceania</span>", x = "", y = "") +
  plot_theme

pl.cn <- 
  ggplot(plot_data, aes(x = year, y = cn)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = asia), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = asia)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = cn), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue" ) +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>China</span> / <span style='color:lightblue; font-weight:bold;'>Asia</span>", x = "", y = "") +
  plot_theme

pl.za <- 
  ggplot(plot_data, aes(x = year, y = za)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = africa), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = africa)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = za), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue" ) +
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>South Africa</span> / <span style='color:lightblue; font-weight:bold;'>Africa</span>", x = "", y = "") +
  plot_theme

# Lower panel - US + BRICS
pl.lower <- (pl.us + pl.br) / (pl.ru + pl.ind) / (pl.cn + pl.za)

# Combined plot
pl <- (pl.all + pl.lower) +
  plot_annotation(
    title = "How often the European Commission publicly communicates about foreign countries",
    subtitle = "Share of public communication documents mentioning non-EU states\n",
    caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
    theme = theme(plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, face = "bold"))
  )

pl

ggsave("./output/descriptive_plots/ForeignCountryRegionSalience_OverTime.png", pl, height = 20, width = 36, units = "cm")

# plot relative to all fc ####
shares_relative <- 
  shares %>% 
  mutate(
    across(us:war, ~.x / fc)
  )

plot_data = shares_relative

# Individual plots

plot_theme <- theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 10, hjust = 0.5),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

pl.all <- 
  ggplot(shares, aes(x = year)) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = fc), color = "lightblue", linewidth = 1) +
    geom_point(size = 4, color = "lightblue", aes(y = fc)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = others), color = "blue", linewidth = 1) +
  geom_point(size = 4, color = "blue", aes(y = others)) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>All other Countries (non US/BRICS)</span> / <span style='color:lightblue; font-weight:bold;'>All non-EU Countries</span>", x = "", y = "") +
  plot_theme

pl.us <- 
  ggplot(plot_data, aes(x = year, y = us)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = namerica), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = namerica)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = us), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue") +
 # geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>US</span> / <span style='color:lightblue; font-weight:bold;'>North America</span>", x = "", y = "") +
  plot_theme

pl.br <- 
  ggplot(plot_data, aes(x = year, y = br)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = samerica), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = samerica)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = br), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue") +
#  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Brazil</span> / <span style='color:lightblue; font-weight:bold;'>South America</span>", x = "", y = "") +
  plot_theme

pl.war <- 
  ggplot(plot_data, aes(x = year, y = ru)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = war), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(x = year, y = war)) +
  geom_segment(aes(x = year+.2, xend = year+.2, y = 0, yend = ua), color = "yellow", linewidth = 1) +
  geom_point(size = 1, color = "yellow", aes(x = year+.2, y = ua)) +
  geom_segment(aes(x = year-.2, xend = year-.2, y = 0, yend = ru), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue", aes(x = year-.2)) +
  #  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Russia</span> <span style='color:lightblue; font-weight:bold;'>or</span> <span style='color:yellow; font-weight:bold;'>Ukraine</span>", x = "", y = "") +
  plot_theme


pl.ru <- 
  ggplot(plot_data, aes(x = year, y = ru)) +
  # geom_segment(aes(x = year, xend = year, y = 0, yend = war), color = "lightblue", linewidth = 1) +
  # geom_point(size = 1, color = "lightblue", aes(x = year, y = war)) +
  # geom_segment(aes(x = year+.2, xend = year+.2, y = 0, yend = ua), color = "yellow", linewidth = 1) +
  # geom_point(size = 1, color = "yellow", aes(x = year+.2, y = ua)) +
  
  geom_segment(aes(x = year, xend = year, y = 0, yend = europe), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(x = year, y = europe)) +
  
  geom_segment(aes(x = year-.2, xend = year-.2, y = 0, yend = ru), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue", aes(x = year-.2)) +
#  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>Russia</span>  / <span style='color:lightblue; font-weight:bold;'>Europe (non-EU)</span>", x = "", y = "") +
  plot_theme

pl.ind <- 
  ggplot(plot_data, aes(x = year, y = `in`)) +
  geom_segment(aes(x = year-.1, xend = year-.1, y = 0, yend = asia), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(x = year-.1, y = asia)) +
  geom_segment(aes(x = year+.1, xend = year+.1, y = 0, yend = `in`), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue", aes(x = year+.1)) +
#  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>India</span> / <span style='color:lightblue; font-weight:bold;'>Asia</span>", x = "", y = "") +
  plot_theme

pl.cn <- 
  ggplot(plot_data, aes(x = year, y = cn)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = asia), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = asia)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = cn), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue" ) +
#  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>China</span> / <span style='color:lightblue; font-weight:bold;'>Asia</span>", x = "", y = "") +
  plot_theme

pl.za <- 
  ggplot(plot_data, aes(x = year, y = za)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = africa), color = "lightblue", linewidth = 1) +
  geom_point(size = 1, color = "lightblue", aes(y = africa)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = za), color = "blue", linewidth = .5) +
  geom_point(size = 1, color = "blue" ) +
#  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "<span style='color:blue; font-weight:bold;'>South Africa</span> / <span style='color:lightblue; font-weight:bold;'>Africa</span>", x = "", y = "") +
  plot_theme

# Lower panel - US + BRICS
#pl.lower <- (pl.us + pl.br) / (pl.ru + pl.ind) / (pl.cn + pl.za)
pl.lower <- (pl.br +  pl.ru) / (pl.ind + pl.cn) / (pl.za + pl.us)

# Combined plot
pl <- (pl.all + pl.lower) +
  plot_annotation(
    title = "On which foreign countries does the European Commission focus in its public communication?",
    subtitle = "Country/regional shares in public communication documents mentioning non-EU states\n",
    caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
    theme = theme(plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, face = "bold"))
  )

pl
ggsave("./output/descriptive_plots/ForeignCountryRegionRelativeFocus_OverTime.png", pl, height = 20, width = 36, units = "cm")


# Clean up ####
rm(list = ls(pattern = "^pl"), shares)
gc()







