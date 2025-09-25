# CMs in DP context:
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode)
library(GGally) # Extension to 'ggplot2' CRAN v2.1.2
library(grid)
library(gridExtra)
library(gtable)
library(patchwork)
library(ggtext)
# DP scaled phrases: 

data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB

sents <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds"))

cm_all <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds")) %>% 
  bind_rows(., 
            read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrasesIO.rds")) %>% 
              mutate(year = as.numeric(year),
                     state = F,
                     iso2c = ifelse(iso2c == "AU", "AfrU", iso2c))
            ) %>% 
  
  left_join(., read_rds(paste0(data_path, "cleaned_data/scaling_glove_aspectPhrases_nonEU.rds")) %>% 
            select(iso2c, year) %>% 
            mutate(non_EU = T) %>% distinct(),
          join_by(iso2c, year)) %>% 
  mutate(non_EU = !is.na(non_EU),
         state = is.na(state)) %>% 
  
  left_join(., 
            bind_rows(read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds")),
                      read_rds(paste0(data_path, "CountryMentions/IOs_phrase-words_vec.rds")) %>%  
                        mutate(year = as.numeric(year),
                               iso2c = ifelse(iso2c == "AU", "AfrU", iso2c))
                      ) %>% select(-cregex, -year),
            join_by(sentence_id, iso2c)) 
  

cm <- cm_all %>% filter(year < 2024) #%>% filter(non_EU)


# Benchmarks ####


#mean_cm_ccg <- mean(cm$digi_sim, na.rm = T) # Means in foreign country mentions
mean_cm_ffg <- mean(cm$digi_sim, na.rm = T)
#mean_cm_ccl <- mean(cm$coop_confl_lsx, na.rm = T)

#sd_cm_ccg <- sd(cm$digi_sim, na.rm = T) # sds in foreign country mentions
sd_cm_ffg <- sd(cm$digi_sim, na.rm = T)
#sd_cm_ccl <- sd(cm$coop_confl_lsx, na.rm = T)

#mean_sent_ccg <- mean(sent$digi_sim, na.rm = T) # Means in all English sentences
#mean_sent_ffg <- mean(sent$digi_sim, na.rm = T)
#mean_sent_ccl <- mean(sent$coop_confl_lsx, na.rm = T)

country_means <- cm %>% # Country means
  select(iso2c, digi_sim) %>% 
  group_by(iso2c) %>% 
  summarise(digi_sim = mean(digi_sim, na.rm = T)) %>% 
  ungroup()


# Over time ####

#ggplot(cm %>% sample_n(50000), aes(x = year, y = , label = country)) +
pl.all <- 
  ggplot(cm %>% filter(non_EU & state & !iso2c %in% c("RU", "US", "BR", "IN", "CN", "ZA", "UA")), 
         aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "All other foreign countries (w/o US & BRICS)",
       y = "Digitality\nin Country References",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.all_no_war <- 
  ggplot(cm %>% filter(state & !iso2c %in% c("RU", "UA")), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  # stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = .2) + # Confidence bands not visible
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "All foreign countries (- Russia/Ukraine)",
       y = "friendly/adversarial\nlanguage\n",
       x = "Years")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.us <- 
  ggplot(cm %>% filter(iso2c == "US"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "United States",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.br <- 
  ggplot(cm %>% filter(iso2c == "BR"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "Brazil",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.ru <- 
  ggplot(cm %>% filter(iso2c == "RU"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "Russia",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.in <- 
  ggplot(cm %>% filter(iso2c == "IN"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "India",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.cn <- 
  ggplot(cm %>% filter(iso2c == "CN"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "China",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.za <- 
  ggplot(cm %>% filter(iso2c == "ZA"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg)) +
  labs(title = "South Africa",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

pl.AU <- 
  ggplot(cm %>% filter(iso2c == "AfrU" & year > 2001), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg), xlim = c(1985, 2023)) +
  labs(title = "African Union",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


pl.Africa <- 
  ggplot(cm %>% filter(iso2c == "AFR"), aes(x = year, y = digi_sim, label = iso2c)) +
  geom_hline(yintercept = mean_cm_ffg, linetype = "dotted")+
  geom_jitter(width = .3, alpha = .3, shape = 1, size = .2, aes(color = digi_sim), stroke = .2)+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "black", size = .7) + 
  scale_colour_gradient2(low = "blue",
                         high = "red",
                         mid = "grey50",
                         midpoint = mean_cm_ffg, 
                         limits = c(mean_cm_ffg-5*sd_cm_ffg,
                                    mean_cm_ffg+5*sd_cm_ffg),
                         oob=scales::squish)+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  coord_cartesian(ylim = c(mean_cm_ffg-3*sd_cm_ffg, mean_cm_ffg+5*sd_cm_ffg), xlim = c(1985, 2023)) +
  labs(title = "Africa",
       y = "",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())


# Lower panel - US + brics

pl.lower <- (pl.us+pl.br)/(pl.ru+pl.in)/(pl.cn+pl.za)
l#pl.lower <- (pl.br +  pl.ru) / (pl.in + pl.cn) / (pl.za + pl.us)


# Combined plot
pl <- (#pl.all+
         pl.lower)+
  #(pl.all_no_war+pl.lower)+
  # plot_layout(heights = c(.7,1))+
  plot_annotation(title = "Which role does Digital Policy play in the European Commission's Public Communication about Foreign Countries?",
                  subtitle = "Each dot represents a reference to a non-EU state.<br>Y-axis and color indicate whether extracted phrases discussing the country lean towards <span style='color:red; font-weight:bold;'>digital-related</span>, <span style='color:grey50; font-weight:bold;'>neutral</span>, or <span style='color:blue; font-weight:bold;'>non-digital</span> language.",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  


ggsave("./output/descriptive_plots/Digitality_over_time_BRICS.png", pl, height = 22, width = 36, units = "cm")

# Clean up
rm(list = ls(pattern = "^pl"))
gc()



###################################################

# plot maps #######
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

cm <- cm %>% filter(digi_sim > mean_cm_ffg + 2*sd_cm_ffg & state)

df85_89 <- cm %>% 
  filter(year >= 1985 & year <= 1989) %>% 
  group_by(iso2c) %>% 
  summarise(cc85_89 = mean(digi_sim, na.rm = T),
            mentions85_89 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc85_89 = scales::rescale(mentions85_89, to = c(0, 1)),
         cc85_89 = mentions_sc85_89)
# mutate(mentions_sc85_89 = scale(mentions85_89)[,1]) # Centers and standardizes mentions

df90_00 <- cm %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(iso2c) %>% 
  summarise(cc90_00 = mean(digi_sim, na.rm = T),
            mentions90_00 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc90_00 = scales::rescale(mentions90_00, to = c(0, 1)),
         cc90_00 = mentions_sc90_00)
# mutate(mentions_sc90_00 = scale(mentions90_00)[,1]) # Centers and standardizes mentions

df01_10 <- cm %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  group_by(iso2c) %>% 
  summarise(cc01_10 = mean(digi_sim, na.rm = T),
            mentions01_10 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc01_10 = scales::rescale(mentions01_10, to = c(0, 1)),
         cc01_10 = mentions_sc01_10)
# mutate(mentions_sc01_10 = scale(mentions01_10)[,1]) # Centers and standardizes mentions

df11_23 <- cm %>% 
  filter(year >= 2011 & year <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(cc11_23 = mean(digi_sim, na.rm = T),
            mentions11_23 = n()) %>% 
  ungroup() %>% 
  mutate(mentions_sc11_23 = scales::rescale(mentions11_23, to = c(0, 1)),
         cc11_23 = mentions_sc11_23)
# mutate(mentions_sc11_23 = scale(mentions11_23)[,1]) # Centers and standardizes mentions

dfall <- cm %>% 
  group_by(iso2c) %>% 
  summarise(ccall = mean(digi_sim, na.rm = T),
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

ggsave("./output/descriptive_plots/Digitality_worldmap.png", pl.all, height = 22, width = 36, units = "cm")
