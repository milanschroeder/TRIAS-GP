#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Plot Friend/Foe based on AspectPhrase Extraction
# Author:   @Milan Schröder (23.04.2025)
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

# scale aspect phrases ####
weights <- read_rds(paste0(data_path, "glove_models/SemSimilWeights-FriendFoe.rdsgz"))

countries <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% 
  select(iso2c, year, eu_member)

# cm <- cm %>% 
#   filter(iso2c %in% unique(countries$iso2c)) %>% 
#   left_join(countries, by = c("iso2c", "year")) %>% 
#   filter(!eu_member) %>% 
#   select(-eu_member)
# 
# 
# # Filter document types
# cm <- cm %>% 
#   filter(doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement"))
# 
# # Clean up
# rm(glove, lsx)
# gc()


phrases <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds")) %>% 
  mutate(country_phrase = ifelse(is.na(country_phrase), 
                                 text_sent %>% stringr::str_replace_all("[:punct:]| ", ","), 
                                 country_phrase)) %>% 
  separate_rows(., country_phrase, sep = ",") %>%
  mutate(token = country_phrase %>% str_squish() %>% str_to_lower()) %>% 
  select(-c(mentions:country_phrase)) %>% 
  left_join(., weights, join_by(token)) %>% 
  filter(iso2c %in% unique(countries$iso2c)) %>% 
  left_join(countries, by = c("iso2c", "year")) %>% 
  filter(!eu_member) %>% 
  select(-eu_member)

cm <- 
  phrases %>% 
  group_by(sentence_id, iso2c, year) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% 
  ungroup()

write_rds(country_phrases_scaled, paste0(data_path, "cleaned_data/scaling_glove_aspectPhrases_nonEU.rds"))

# Load the data #####

# Scale values of sentences
# Picking three - the two Glove Scale with pluasible face validity, and the LSX scale validated in Russia paper
# glove <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_sentlevel.rds"))  %>% 
#   mutate(gti = max(gti, na.rm = T) + min(gti, na.rm = T) - gti, # Get directionality right, more "hostile" language should have higher values (correct downstream)
#          coop_confl = max(coop_confl, na.rm = T) + min(coop_confl, na.rm = T) - coop_confl,
#          friend_foe = max(friend_foe, na.rm = T) + min(friend_foe, na.rm = T) - friend_foe) %>% 
#   rename_with(~ paste0(., "_glove"), .cols = 2:last_col())
# 
# lsx <- read_rds(paste0(data_path, "cleaned_data/scaling_lsx_sentlevel.rds")) %>% 
#   select(-doc_id) %>% 
#   rename_with(~ paste0(., "_lsx"), .cols = 2:last_col())
# 
# # Sentence data
# sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
#   mutate(wordcount = str_count(text_sent, "\\s+")) %>% 
#   mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
#   left_join(glove %>% select(sentence_id, friend_foe, friend_foe_glove), by = "sentence_id") %>% 
#   left_join(lsx %>% select(sentence_id, coop_confl_lsx), by = "sentence_id") %>% 
#   filter(lang_sent == "en") # only English sentences
# 
# # Country mentions - long form, English sentences only
# cm <- read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>% 
#   left_join(glove %>% select(sentence_id, friend_foe, friend_foe_glove), by = "sentence_id") %>% 
#   left_join(lsx %>% select(sentence_id, coop_confl_lsx), by = "sentence_id") %>% 
#   left_join(sent %>% select(sentence_id, doc_type, year))

# Filter country mentions 
# to include only those states in larger data sets
# and exclude EU member states


# Benchmarks ####


#mean_cm_ccg <- mean(cm$coop_confl_glove, na.rm = T) # Means in foreign country mentions
mean_cm_ffg <- mean(cm$friend_foe, na.rm = T)
#mean_cm_ccl <- mean(cm$coop_confl_lsx, na.rm = T)

#sd_cm_ccg <- sd(cm$coop_confl_glove, na.rm = T) # sds in foreign country mentions
sd_cm_ffg <- sd(cm$friend_foe, na.rm = T)
#sd_cm_ccl <- sd(cm$coop_confl_lsx, na.rm = T)

#mean_sent_ccg <- mean(sent$coop_confl_glove, na.rm = T) # Means in all English sentences
#mean_sent_ffg <- mean(sent$friend_foe, na.rm = T)
#mean_sent_ccl <- mean(sent$coop_confl_lsx, na.rm = T)

country_means <- cm %>% # Country means
  select(iso2c, friend_foe) %>% 
  group_by(iso2c) %>% 
  summarise(friend_foe = mean(friend_foe, na.rm = T)) %>% 
  ungroup()


# Over time ####

#ggplot(cm %>% sample_n(50000), aes(x = year, y = , label = country)) +
pl.all <- 
  ggplot(cm, aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "All foreign countries",
       y = "friendly/adversarial\nlanguage\n",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.all_no_war <- 
  ggplot(cm %>% filter(!iso2c %in% c("RU", "UA")), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "All foreign countries (- Russia/Ukraine)",
       y = "friendly/adversarial\nlanguage\n",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.us <- 
  ggplot(cm %>% filter(iso2c == "US"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "United States",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.br <- 
  ggplot(cm %>% filter(iso2c == "BR"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "Brazil",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.ru <- 
  ggplot(cm %>% filter(iso2c == "RU"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "Russia",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.in <- 
  ggplot(cm %>% filter(iso2c == "IN"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "India",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.cn <- 
  ggplot(cm %>% filter(iso2c == "CN"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "China",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.za <- 
  ggplot(cm %>% filter(iso2c == "ZA"), aes(x = year, y = friend_foe, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "red",
                         high = "blue",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.25, +.25)) +
  labs(title = "South Africa",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

# Lower panel - US + brics

#pl.lower <- (pl.us+pl.br)/(pl.ru+pl.in)/(pl.cn+pl.za)
pl.lower <- (pl.br +  pl.ru) / (pl.in + pl.cn) / (pl.za + pl.us)


# Combined plot
pl <- #(pl.all+pl.lower)+
  (pl.all_no_war+pl.lower)+
  # plot_layout(heights = c(.7,1))+
  plot_annotation(title = "How does the European Commission publicly communicate about foreign countries?",
                  subtitle = "Each dot represents a reference to a non-EU state.<br>Y-axis and color indicate whether extracted phrases discussing the country lean towards <span style='color:blue; font-weight:bold;'>friendly</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:red; font-weight:bold;'>adversarial</span> language.",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  


ggsave("./output/descriptive_plots/FriendFoeCountries_OverTime_AP.png", pl, height = 22, width = 36, units = "cm")

# Clean up
rm(list = ls(pattern = "^pl"))
gc()
