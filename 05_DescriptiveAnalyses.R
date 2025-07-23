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

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Load the data #####

# Scale values of sentences
# Picking three - the two Glove Scale with pluasible face validity, and the LSX scale validated in Russia paper
glove <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds"))  %>%
  mutate(year = as.numeric(year),
         gti = max(gti, na.rm = T) + min(gti, na.rm = T) - gti, # Get directionality right, more "hostile" language should have higher values (correct downstream)
         coop_confl = max(coop_confl, na.rm = T) + min(coop_confl, na.rm = T) - coop_confl) #%>% 
  #rename_with(~ paste0(., ""), .cols = 2:9)

# lsx <- read_rds(paste0(data_path, "cleaned_data/scaling_lsx_sentlevel.rds")) %>% 
#   select(-doc_id) %>% 
#   rename_with(~ paste0(., "_lsx"), .cols = 2:9)

# Sentence data
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  # mutate(wordcount = str_count(text_sent, "\\s+")) %>% 
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  left_join(glove %>% select(sent_id, coop_confl, friend_foe), by = join_by(sent_id)) %>% 
  #left_join(lsx %>% select(sent_id, coop_confl_lsx), join_by(sent_id)) %>% 
  filter(lang_sent == "en" &
           doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement") &
           !clearly_table & !mostly_numbers & !is_link) # only English sentences

# Country mentions - long form, English sentences only
cm <- read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>% 
  left_join(glove %>% select(sent_id, iso2c, coop_confl, friend_foe), join_by(sent_id, iso2c)) %>% 
  #left_join(lsx %>% select(sent_id, coop_confl_lsx), join_by("sent_id")) %>% 
  left_join(sent %>% select(sent_id, doc_type), join_by(sent_id))

# Filter country mentions 
# to include only those states in larger data sets
# and exclude EU member states

countries <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% 
  select(iso2c, country, year, eu_member)

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

mean_cm_ccg <- mean(cm$coop_confl, na.rm = T) # Means in foreign country mentions
mean_cm_ffg <- mean(cm$friend_foe, na.rm = T)
#mean_cm_ccl <- mean(cm$coop_confl_lsx, na.rm = T)

sd_cm_ccg <- sd(cm$coop_confl, na.rm = T) # sds in foreign country mentions
sd_cm_ffg <- sd(cm$friend_foe, na.rm = T)
#sd_cm_ccl <- sd(cm$coop_confl_lsx, na.rm = T)

mean_sent_ccg <- mean(sent$coop_confl, na.rm = T) # Means in all English sentences
mean_sent_ffg <- mean(sent$friend_foe, na.rm = T)
#mean_sent_ccl <- mean(sent$coop_confl_lsx, na.rm = T)

country_means <- cm %>% # Country means
  select(iso2c, country, coop_confl, friend_foe#, coop_confl_lsx
         ) %>% 
  group_by(iso2c, country) %>% 
  summarise(coop_confl = mean(coop_confl, na.rm = T),
            friend_foe = mean(friend_foe, na.rm = T)#,
  #          coop_confl_lsx = mean(coop_confl_lsx, na.rm = T)
            ) %>% 
  ungroup()


# Over time ####

#ggplot(cm %>% sample_n(50000), aes(x = year, y = friend_foe, label = country)) +
pl.all <- 
  ggplot(cm, aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(-.2, +.2)) +
  labs(title = "All foreign countries",
       y = "friendly/adversarial\nlanguage\n",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.us <- 
  ggplot(cm %>% filter(iso2c == "US"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
  ggplot(cm %>% filter(iso2c == "BR"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
  ggplot(cm %>% filter(iso2c == "RU"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
  ggplot(cm %>% filter(iso2c == "IN"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
  ggplot(cm %>% filter(iso2c == "CN"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
  ggplot(cm %>% filter(iso2c == "ZA"), aes(x = year, y = friend_foe, label = country)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = friend_foe), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-2*sd_cm_ffg,
                                    mean_cm_ffg+2*sd_cm_ffg),
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
                  subtitle = "Each dot represents one sentence in which a non-EU state is mentioned.<br>Y-axis and color indicate whether this sentence uses <span style='color:blue; font-weight:bold;'>friendly</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:red; font-weight:bold;'>adversarial</span> language.",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                        plot.subtitle = element_markdown(hjust = 0.5)))  


# ggsave("./output/descriptive_plots/new/FriendFoeCountries_OverTime.png", pl, height = 32, width = 24, units = "cm")
ggsave("./output/descriptive_plots/new/FriendFoeCountries_OverTime.png", pl, height = 22, width = 36, units = "cm")

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
  summarise(ff85_89 = mean(friend_foe, na.rm = T),
            mentions85_89 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc85_89 = scales::rescale(mentions85_89, to = c(0, 1)))
  # mutate(mentions_sc85_89 = scale(mentions85_89)[,1]) # Centers and standardizes mentions

df90_00 <- cm %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(iso2c) %>% 
  summarise(ff90_00 = mean(friend_foe, na.rm = T),
            mentions90_00 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc90_00 = scales::rescale(mentions90_00, to = c(0, 1)))
  # mutate(mentions_sc90_00 = scale(mentions90_00)[,1]) # Centers and standardizes mentions

df01_10 <- cm %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  group_by(iso2c) %>% 
  summarise(ff01_10 = mean(friend_foe, na.rm = T),
            mentions01_10 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc01_10 = scales::rescale(mentions01_10, to = c(0, 1)))
  # mutate(mentions_sc01_10 = scale(mentions01_10)[,1]) # Centers and standardizes mentions

df11_23 <- cm %>% 
  filter(year >= 2011 & year <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(ff11_23 = mean(friend_foe, na.rm = T),
            mentions11_23 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc11_23 = scales::rescale(mentions11_23, to = c(0, 1)))
  # mutate(mentions_sc11_23 = scale(mentions11_23)[,1]) # Centers and standardizes mentions

dfall <- cm %>% 
  group_by(iso2c) %>% 
  summarise(ffall = mean(friend_foe, na.rm = T),
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
  geom_sf(aes(fill = ffall, alpha = mentions_scall), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ffall, na.rm = T), 
                       limits = c(mean(dfall$ffall, na.rm = T)-2*sd(dfall$ffall, na.rm = T),
                                  mean(dfall$ffall, na.rm = T)+2*sd(dfall$ffall, na.rm = T)),
                         oob=scales::squish)+
  labs(title = "Full period (1985-2023)",
       fill = "friendly/adversarial language around country mentions: \n ",
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
  geom_sf(aes(fill = ff85_89, alpha = mentions_sc85_89), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ffall, na.rm = T), 
                       limits = c(mean(dfall$ffall, na.rm = T)-2*sd(dfall$ffall, na.rm = T),
                                  mean(dfall$ffall, na.rm = T)+2*sd(dfall$ffall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1985-1990",
       fill = "friendly/adversarial language around country mentions: \n ",
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
  geom_sf(aes(fill = ff90_00, alpha = mentions_sc90_00), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ffall, na.rm = T), 
                       limits = c(mean(dfall$ffall, na.rm = T)-2*sd(dfall$ffall, na.rm = T),
                                  mean(dfall$ffall, na.rm = T)+2*sd(dfall$ffall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "1990-2000",
       fill = "friendly/adversarial language around country mentions: \n ",
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
  geom_sf(aes(fill = ff01_10, alpha = mentions_sc01_10), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ffall, na.rm = T), 
                       limits = c(mean(dfall$ffall, na.rm = T)-2*sd(dfall$ffall, na.rm = T),
                                  mean(dfall$ffall, na.rm = T)+2*sd(dfall$ffall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2001-2010",
       fill = "friendly/adversarial language around country mentions: \n ",
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
  geom_sf(aes(fill = ff11_23, alpha = mentions_sc11_23), 
          color = NA, linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "grey50",
                       na.value = "white",
                       midpoint = mean(dfall$ffall, na.rm = T), 
                       limits = c(mean(dfall$ffall, na.rm = T)-2*sd(dfall$ffall, na.rm = T),
                                  mean(dfall$ffall, na.rm = T)+2*sd(dfall$ffall, na.rm = T)),
                       oob=scales::squish)+
  labs(title = "2011-2023",
       fill = "friendly/adversarial language around country mentions: \n ",
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
                    subtitle = "Color indicates whether sentences mentioning the country on average use <span style='color:blue; font-weight:bold;'>friendly</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:red; font-weight:bold;'>adversarial</span> language.<br>Transparency indicates the normalized frequency by which the Commission has meantioned the country.<br>",
                    caption = "\nBased on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.\nCountries appearing fully white/invisible are either EU member states or not mentioned in the respective period.",
                    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                  plot.subtitle = element_markdown(hjust = 0.5))) +  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        legend.box = "vertical") 


ggsave("./output/descriptive_plots/new/FriendFoeCountries_Maps_OverTime.png", pl.comb, height = 22, width = 36, units = "cm")





# Analyse salience of countries ####


# Analyse foreign country references over CAP classifications ####
cm_para_long <- read_rds(paste0(data_path, "CountryMentions/allCMs_paralevel.rds")) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% select(doc_key, date), join_by(doc_key)) %>% 
  mutate(year = year(date),
         cm_total = rowSums(select(., AD:YU))) %>% 
  select(where(~n_distinct(.)>1)) %>% 
  filter(cm_total > 0) %>% 
  pivot_longer(AD:YU, names_to = "iso2c", values_to = "mentions") %>% 
  filter(mentions > 0) %>% 
  left_join(., countries, join_by(iso2c, year)) #%>% 
  #select(text_id, iso2c, mentions, doc_id, date, CAP_code, CAP_name, eu_member)

write_rds(cm_para_long, "../data/CMs_paralevel_long_minimal.rds")

cm_external <- cm_para_long %>% 
  filter(eu_member == F | is.na(eu_member)) %>% 
  group_by(text_id) %>% 
  summarize(cm_external = sum(mentions),
            cm_external_any = cm_external > 0
  ) %>% 
  ungroup()

cm_internal <- cm_para_long %>% 
  filter(eu_member == T) %>% 
  group_by(text_id) %>% 
  summarize(cm_internal = sum(mentions),
            cm_internal_any = cm_internal > 0
  ) %>% 
  ungroup()

# join cm + cm_in/external
cm_para <- read_rds(paste0(data_path, "CountryMentions/allCMs_paralevel.rds")) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% select(doc_key, date), join_by(doc_key)) %>%
  #select(text_id, doc_id, date, CAP_code, CAP_name,  EU_expl:last_col()) %>% 
  left_join(., cm_internal, join_by(text_id)) %>% 
  left_join(., cm_external, join_by(text_id)) %>% 
  mutate(across(cm_internal:cm_external_any, ~ifelse(is.na(.x), 0, .x)),
         across(c(cm_external_any, cm_internal_any), ~as.logical(.x)))

# immediate context: para-level ####
para_plot_data <- 
  cm_para %>% 
  mutate(year = year(date),
         across(c(cm_external_any, cm_internal_any), ~as.numeric(.x))) %>% 
  group_by(year) %>% 
  mutate(N_total = n()) %>% 
  group_by(year, CAP_name) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T),
            N = n()) %>%
  ungroup() %>% 
  mutate(yearly_share = N / N_total) %>% 
filter(year < 2024)

para_plot_data %>%
  ggplot(aes(x = year, size = yearly_share#, alpha = N
  )) +
  #geom_point(aes(fill = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  geom_point(aes(colour = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(fill = cm_external, y = fct_reorder(CAP_name, 1-cm_external))) + # plot avg non-EU country mentions
  #geom_point(aes(fill = cm_BRICS, y = fct_reorder(CAP_name, 1-cm_BRICS))) + # plot avg mention of any BRICS country
  #geom_point(aes(fill = CN, y = fct_reorder(CAP_name, 1-CN))) + # plot avg China mentions
  #geom_point(aes(fill = US, y = fct_reorder(CAP_name, 1-US))) + # plot avg US mentions
  viridis::scale_fill_viridis(direction = -1) +
  viridis::scale_colour_viridis(direction = -1) +
  theme_minimal() + 
  labs(x = "", y= "", subtitle = "Share of paragraphs mentioning any foreign country by CAP classification", fill = "foreign country share", colour = "foreign country share", size = "CAP share p.a.")

ggsave("./output/descriptive_plots/new/ForeignCountry_CAP_OverTime.png", width = 26, height = 16, units = "cm")

para_plot_data %>%
  ggplot(aes(x = year, size = yearly_share#, alpha = N
  )) +
  #geom_point(aes(fill = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(colour = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(fill = cm_external, y = fct_reorder(CAP_name, 1-cm_external))) + # plot avg non-EU country mentions
  #geom_point(aes(fill = cm_BRICS, y = fct_reorder(CAP_name, 1-cm_BRICS))) + # plot avg mention of any BRICS country
  geom_point(aes(colour = CN, y = fct_reorder(CAP_name, 1-CN))) + # plot avg China mentions
  #geom_point(aes(fill = US, y = fct_reorder(CAP_name, 1-US))) + # plot avg US mentions
  viridis::scale_fill_viridis(direction = -1) +
  viridis::scale_colour_viridis(direction = -1) +
  theme_minimal() + 
  labs(x = "", y= "", subtitle = "Share of paragraphs mentioning China by CAP classification", fill = "country share", colour = "country share", size = "CAP share p.a.")

ggsave("./output/descriptive_plots/new/China_CAP_OverTime.png", width = 26, height = 16, units = "cm")

para_plot_data %>%
  ggplot(aes(x = year, size = yearly_share#, alpha = N
  )) +
  #geom_point(aes(fill = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(colour = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(fill = cm_external, y = fct_reorder(CAP_name, 1-cm_external))) + # plot avg non-EU country mentions
  #geom_point(aes(fill = cm_BRICS, y = fct_reorder(CAP_name, 1-cm_BRICS))) + # plot avg mention of any BRICS country
  #geom_point(aes(colour = CN, y = fct_reorder(CAP_name, 1-CN))) + # plot avg China mentions
  geom_point(aes(colour = US, y = fct_reorder(CAP_name, 1-US))) + # plot avg US mentions
  viridis::scale_fill_viridis(direction = -1) +
  viridis::scale_colour_viridis(direction = -1) +
  theme_minimal() + 
  labs(x = "", y= "", subtitle = "Share of paragraphs mentioning the US by CAP classification", fill = "country share", colour = "country share", size = "CAP share p.a.")

ggsave("./output/descriptive_plots/new/US_CAP_OverTime.png", width = 26, height = 16, units = "cm")

para_plot_data %>%
  ggplot(aes(x = year, size = yearly_share#, alpha = N
  )) +
  #geom_point(aes(fill = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(colour = cm_external_any, y = fct_reorder(CAP_name, 1-cm_external_any))) + # plot whether any non-EU country is mentioned
  #geom_point(aes(fill = cm_external, y = fct_reorder(CAP_name, 1-cm_external))) + # plot avg non-EU country mentions
  #geom_point(aes(fill = cm_BRICS, y = fct_reorder(CAP_name, 1-cm_BRICS))) + # plot avg mention of any BRICS country
  #geom_point(aes(colour = CN, y = fct_reorder(CAP_name, 1-CN))) + # plot avg China mentions
  geom_point(aes(colour = RU, y = fct_reorder(CAP_name, 1-RU))) + # plot avg US mentions
  viridis::scale_fill_viridis(direction = -1) +
  viridis::scale_colour_viridis(direction = -1) +
  theme_minimal() + 
  labs(x = "", y= "", subtitle = "Share of paragraphs mentioning Russia by CAP classification", fill = "country share", colour = "country share", size = "CAP share p.a.")

ggsave("./output/descriptive_plots/new/RU_CAP_OverTime.png", width = 26, height = 16, units = "cm")


# BRICS: basically trade only! (except maybe covid, slightly decreasing since ~2010)
# CN: same (but some interesting nuance to check)
# US almost everywhere (interesting: different trade peaks, defense around Irak/Afghanistan, energy peaking with climate movement, civil rights: slow increase after snowden, peak 2015/2016, social/labor only in new labour era)


# paragraph level descriptives ####

# para types over time:
# paras <- read_rds(paste0(data_path, "cleaned_data/data_paralevel.rds")) %>% 
#   left_join(., read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% select(doc_key, date), join_by(doc_key))
# 
# 
# paras %>% 
#   mutate(year = year(date),
#          para_type = ifelse(is.na(para_type), "paragraph", para_type)) %>% 
#   ggplot(aes(x = year)) +
#   geom_bar(aes(fill = para_type)) +
#   theme_minimal() +
#   labs(x = "", y = "", title = "Paragraph types used by European Commission, 1985-2024", fill = "")
# 
# paras %>% 
#   mutate(year = year(date),
#          para_type = ifelse(is.na(para_type), "paragraph", para_type)) %>% 
#   group_by(para_type, year) %>% 
#   summarise(N = n(),
#             length = mean(n_chars_para, na.rm = T)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = year, y = length, colour = para_type)) +
#   geom_point(aes(size = N)) +
#   geom_line() +
#   # coord_cartesian(ylim = c(0, 1000))+
#   theme_minimal() +
#   labs(x = "", y = "", title = "Paragraph length in European Commission Communication, 1985-2024", fill = "")
# 


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


# docs ver time
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
  select(c(doc_id, year, AD:YU)) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = AD:YU, names_to = "iso2c", values_to = "mentions") # Easier to handle


# External country/year panel ####

cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data states (= official states at some point)
length(unique(cp$iso2c)) # 241
# Newsmap dictionary is somewhat broader
length(unique(cm$iso2c)) # 257
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))



# Describe salience of foreign countries over time ####
# Share of docs (per year) that mention a country at least once as the most straightforward measure

# Only non-eu countries
cm.foreign <- cm %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(!eu_member) %>% # Filter
  select(-eu_member)


# Share of docs with country
shares <-
  cm.foreign %>% 
  mutate(us = (iso2c == "US" & mentions > 0), # Single out US + BRICS
         br = (iso2c == "BR" & mentions > 0),
         ru = (iso2c == "RU" & mentions > 0),
         `in` = (iso2c == "IN" & mentions > 0),
         cn = (iso2c == "CN" & mentions > 0),
         za = (iso2c == "ZA" & mentions > 0)) %>% 
  group_by(doc_id, year) %>% 
  summarise(mentions = sum(mentions),
            us = sum(us),
            br = sum(br),
            ru = sum(ru),
            `in` = sum(`in`),
            cn = sum(cn),
            za = sum(za)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(fc = mean(as.logical(mentions)), # Annual shares
            us = mean(as.logical(us)),
            br = mean(as.logical(br)),
            ru = mean(as.logical(ru)),
            `in` = mean(as.logical(`in`)),
            cn = mean(as.logical(cn)),
            za = mean(as.logical(za))) %>% 
  ungroup()


# Individual plots

pl.all <- 
  ggplot(shares, aes(x = year, y = fc)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = fc), color = "blue", linewidth = 1) +  # Line from 0 to point
  geom_point(size = 4, color = "blue") +  # Circular point
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "All foreign countries", x = "Years", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.us <- 
  ggplot(shares, aes(x = year, y = us)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = us), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +  
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "United States", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.br <- 
  ggplot(shares, aes(x = year, y = br)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = br), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Brazil", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.ru <- 
  ggplot(shares, aes(x = year, y = ru)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = ru), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Russia", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


pl.ind <- 
  ggplot(shares, aes(x = year, y = `in`)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = `in`), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "India", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.cn <- 
  ggplot(shares, aes(x = year, y = cn)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = cn), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "China", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.za <- 
  ggplot(shares, aes(x = year, y = za)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = za), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "South Africa", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Lower panel - US + brics
pl.lower <- (pl.us+pl.br)/(pl.ru+pl.ind)/(pl.cn+pl.za)


# Combined plot
pl <- (pl.all+pl.lower)+
  # plot_layout(heights = c(.7,1))+
  plot_annotation(title = "How often the European Commission publicly communicates about foreign countries",
                  subtitle = "Share of public communication documents mentioning non-EU states\n",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5, color = "blue", face = "bold")))  

ggsave("./output/descriptive_plots/new/ForeignCountrySalience_OverTime.png", pl, height = 20, width = 36, units = "cm")

# Clean up
rm(list = ls(pattern = "^pl"), shares)
gc()








  
