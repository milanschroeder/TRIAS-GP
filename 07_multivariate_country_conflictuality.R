#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Multivariate, cross-sectional analysis of how cooperative/conflictive
#           the Commission speaks about types of countries over time
# Author:   @ChRauh (27.02.2025)
#################################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.4.0
library(broom) # Convert Statistical Objects into Tidy Tibbles CRAN v1.0.4
library(glue) # Interpreted String Literals CRAN v1.6.2
library(grid)
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics CRAN v2.3
library(gtable) # Arrange 'Grobs' in Tables CRAN v0.3.6
library(patchwork) # The Composer of Plots CRAN v1.2.0
library(ggtext) # Improved Text Rendering Support for 'ggplot2' CRAN v0.1.2



# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Sentence level data ####

# Load and filter (should be equivalently done for all other data)
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  # Language filter
  filter(lang_sent == "en" &
           doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement") &
            !clearly_table & !mostly_numbers & !is_link) %>% 
  # Year var
  mutate(year = str_extract(date, "^[0-9]{4}"))



# Country mentions - sentence level ####
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel.rds")) %>% 
  filter(sent_id %in% sent$sent_id) %>% # Filter as above
  left_join(., sent %>% select(sent_id, date)) %>% 
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  select(c(sent_id, year, AD:YU)) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") %>% # Easier to handle
  filter(mentions > 0) # Sentences w/out country mentions not needed here
gc()


# External country/year panel ####
cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data sets (= official states at some point)
length(unique(cp$iso2c)) # 241
# Newsmap dictionary should be somewhat broader, but isn't!? Cf. doc level salience analyses
length(unique(cm$iso2c)) # 233
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))


# Sentence-level language scales 
sc <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds")) %>% 
  bind_cols(
    ., read_rds(paste0(data_path, "cleaned_data/additive_scaling_glove_EntityPhrases.rds")) %>% select(friend_foe_additive = friend_foe)
  ) %>% 
  filter(sent_id %in% sent$sent_id) %>% # Filter as above
   select(sent_id, iso2c, security, economy, friend_foe, friend_foe_additive
          ) %>% 
  mutate(WI = 2 * (security * economy) / (security + economy) * 0.01) %>% # harmonized mean
  rename(ff = friend_foe,
         ff2 = friend_foe_additive
         ) #%>%  # Focusing on this one for now
    # only for coop_confl:
  # mutate(ff = max(ff, na.rm = T) + min(ff, na.rm = T) - ff,
  #        ff2 = max(ff2, na.rm = T) + min(ff2, na.rm = T) - ff2) # Get directionality right, more "hostile" language should have higher values (correct downstream)


# Merge scales to country mentions
cmsc <- cm %>% 
  left_join(sc, by = join_by(sent_id, iso2c))

# Only non-eu countries
cm.foreign <- cmsc %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(!eu_member) %>% # Filter
  select(-eu_member)  

focused_sents <- cm.foreign %>% 
  filter(mentions != 0) %>% 
  group_by(sent_id) %>% 
  #group_by(doc_id) %>% # or rather only sents from focused docs?!?
  filter(n() == 1) %>% 
  ungroup()
  
# friend/foe language by country - full period
ff.full <- cm.foreign %>% 
  group_by(iso2c) %>% 
  summarise(ff = mean(ff, na.rm = T)) %>% 
  ungroup()


# Coop/confl language by country and year
ff.ann <- cm.foreign %>% 
#   filter(sent_id %in% focused_sents$sent_id) %>%
   group_by(iso2c, year) %>% 
  summarise(ff = mean(ff, na.rm = T)) %>% 
  ungroup()




# Multivariate analyses ####
# Why does the Commission couch specific foreign countries in cooperative or conflictive language? ####


# Combine country language and country factor data

country_factors <- cp %>% select(iso2c, year,
                                 distance_eu,
                                 eu_import_dependency, eu_export_dependency,
                                 eu.fta,
                                 gdp_worldshare,
                                 vdem.libdem,
                                 nato_member, 
                                 milex_total, armed_conflicts) %>% 
  rename(eu_fta = eu.fta,
         libdem = vdem.libdem)

country_factors$eu_fta[is.na(country_factors$eu_fta)] <- 0 # Plausible assumption


df <- ff.ann %>% # Annual language around mentioning a specific foreign country
  left_join(country_factors, by = c("iso2c", "year"))

# Check completeness of explanatory variables
df$exp_complete <- complete.cases(df[, 4:ncol(df)])
sum(df$exp_complete) # 4325 of 5552, sigh


# NAs by variable
miss_vars <- colSums(is.na(df[, 4:ncol(df)])) %>% 
  as.data.frame() %>% 
  rename(nas = 1) %>% 
  arrange(desc(nas))

# Data available for modelling
reg.df_noWar <- df %>% 
  filter(!iso2c %in% c("RU", "UA")) %>% 
  filter(exp_complete) %>% 
  select(-exp_complete)

reg.df <- df %>% 
  # filter(!iso2c %in% c("RU", "UA")) %>% 
  filter(exp_complete) %>% 
  select(-exp_complete)

table(reg.df$year) # Available n of country obs per year


# Estimate salience models ####

# Function to estimate model by year and extract coefficents etc

ann_reg <- function(data, time.min, time.max) {
  
  # Select data and standardize
  data_std <- data %>% 
    filter(year >= time.min & year <= time.max) %>% 
    mutate(across(-c(iso2c, year), as.numeric)) %>% 
    mutate(across(-c(iso2c, year), scale)) 
  
  
  # De-select constants within this particular data set
  constants <- data_std %>% 
    select(-c(iso2c, year)) %>%
    pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
    group_by(var) %>% 
    summarise(sd = sd(value)) %>% 
    filter(#is.na(sd) | 
             sd == 0)
  data_reg <- data_std %>% 
    select(-c(constants$var))
  
  
  # Run regression
  est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
  
  # Extract coeff and cis
  results <- 
    tidy(est, conf.int = TRUE) %>%
    mutate(period = paste0(time.min, "-", time.max))
  
  # Return results
  return(results)
}


# Collect estimates across periods 

periods <- data.frame(time.min = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
                      time.max = c(1989, 1994, 1999, 2004, 2009, 2014, 2019, 2023))

coeff <- data.frame(NULL)
for (i in 1:nrow(periods)) {
  current <- ann_reg(reg.df, time.min = periods$time.min[i],time.max = periods$time.max[i])
  coeff <- rbind(coeff, current)
}


# Collect estimates for full period (one should cluster ses here ...)

data_reg <- reg.df %>% 
  mutate(across(-c(iso2c, year), as.numeric)) %>% 
  mutate(across(-c(iso2c, year), scale)) 
est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
coeff.full <- 
  tidy(est, conf.int = TRUE) %>%
  mutate(period = "Full\nperiod")
coeff <- rbind(coeff, coeff.full)
coeff$full <- coeff$period == "Full\nperiod"

# Significant on 95% level?
coeff$sig <- coeff$p.value < 0.05
sum(coeff$sig)

# Positively and negatively significant
coeff <- coeff %>% 
  mutate(direc = ifelse(sig & estimate < 0,
                        "neg", 
                        ifelse(sig & estimate > 0,
                               "pos",
                               "ns")) %>% 
           factor(levels = c("neg", "ns", "pos")))


max(abs(coeff$estimate)) # .45


# Nicer labels
coeff$labels <- coeff$term %>% 
  str_replace("distance_eu", "Distance") %>% 
  str_replace("libdem", "Liberal\ndemocracy\nindex") %>% 
  str_replace("eu_export_dependency", "EU's export\ndependency\non country") %>% 
  str_replace("eu_import_dependency", "EU's import\ndependency\non country") %>% 
  str_replace("eu_fta", "Free trade\nagreement\nwith the EU") %>% 
  str_replace("gdp_worldshare", "Global\nGDP\nshare") %>% 
  str_replace("milex_total", "Military\nspending\n(absolute)") %>% 
  str_replace("armed_conflicts", "Armed\nconflicts") %>% 
  str_replace("nato_member", "NATO\nmember") %>% 
  factor(levels = c("Distance",
                    "Liberal\ndemocracy\nindex",
                    "EU's export\ndependency\non country",
                    "EU's import\ndependency\non country",
                    "Free trade\nagreement\nwith the EU",
                    "Global\nGDP\nshare",
                    "Military\nspending\n(absolute)",
                    "Armed\nconflicts",
                    "NATO\nmember"))

# Plot

pl.period <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & !full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_line(aes(group =1), color = "black", linewidth = .3, linetype = "dotted")+
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "By 5-year periods",
       x = "", y ="")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),  
        # strip.background = element_blank(),             
        strip.placement = "outside",
        panel.grid.minor = element_blank())


pl.full <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.02, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "Average",
       x = "", y ="Standardized regression coefficients\n(with 95% confidence bands)\n")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y = element_blank(),  
        strip.background = element_blank(),
        panel.grid.minor = element_blank())

pl.comb <-
  pl.full+pl.period +
  plot_layout(widths = c(1,5))+
  plot_annotation(title = "Which characteristics of countries may explain why<br>the Commission presents them with <span style='color:blue; font-weight:bold;'>friendly</span> or <span style='color:red; font-weight:bold;'>adversarial</span> language?",
                  caption = "Multivariate linear regression models of the average, embedding-based friendly/adversarial language scales\nof sentences mentioning a country in Commission public communication documents.",
                  theme = theme(plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  

ggsave("./output/multivariate_plots/CountryFriendFoe_Explained.png", pl.comb, 
       width = 16, height = 20, units = "cm")

# without Russia / Ukraine: ####

coeff <- data.frame(NULL)
for (i in 1:nrow(periods)) {
  current <- ann_reg(reg.df_noWar, time.min = periods$time.min[i],time.max = periods$time.max[i])
  coeff <- rbind(coeff, current)
}


# Collect estimates for full period (one should cluster ses here ...)

data_reg <- reg.df_noWar %>% 
  mutate(across(-c(iso2c, year), as.numeric)) %>% 
  mutate(across(-c(iso2c, year), scale)) 
est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
coeff.full <- 
  tidy(est, conf.int = TRUE) %>%
  mutate(period = "Full\nperiod")
coeff <- rbind(coeff, coeff.full)
coeff$full <- coeff$period == "Full\nperiod"

# Significant on 95% level?
coeff$sig <- coeff$p.value < 0.05
sum(coeff$sig)

# Positively and negatively significant
coeff <- coeff %>% 
  mutate(direc = ifelse(sig & estimate < 0,
                        "neg", 
                        ifelse(sig & estimate > 0,
                               "pos",
                               "ns")) %>% 
           factor(levels = c("neg", "ns", "pos")))


max(abs(coeff$estimate)) # .45


# Nicer labels
coeff$labels <- coeff$term %>% 
  str_replace("distance_eu", "Distance") %>% 
  str_replace("libdem", "Liberal\ndemocracy\nindex") %>% 
  str_replace("eu_export_dependency", "EU's export\ndependency\non country") %>% 
  str_replace("eu_import_dependency", "EU's import\ndependency\non country") %>% 
  str_replace("eu_fta", "Free trade\nagreement\nwith the EU") %>% 
  str_replace("gdp_worldshare", "Global\nGDP\nshare") %>% 
  str_replace("milex_total", "Military\nspending\n(absolute)") %>% 
  str_replace("armed_conflicts", "Armed\nconflicts") %>% 
  str_replace("nato_member", "NATO\nmember") %>% 
  factor(levels = c("Distance",
                    "Liberal\ndemocracy\nindex",
                    "EU's export\ndependency\non country",
                    "EU's import\ndependency\non country",
                    "Free trade\nagreement\nwith the EU",
                    "Global\nGDP\nshare",
                    "Military\nspending\n(absolute)",
                    "Armed\nconflicts",
                    "NATO\nmember"))

# Plot

pl.period <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & !full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_line(aes(group =1), color = "black", linewidth = .3, linetype = "dotted")+
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "By 5-year periods",
       x = "", y ="")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),  
        # strip.background = element_blank(),             
        strip.placement = "outside",
        panel.grid.minor = element_blank())


pl.full <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.02, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "Average",
       x = "", y ="Standardized regression coefficients\n(with 95% confidence bands)\n")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y = element_blank(),  
        strip.background = element_blank(),
        panel.grid.minor = element_blank())

pl.comb <-
  pl.full+pl.period +
  plot_layout(widths = c(1,5))+
  plot_annotation(title = "Which characteristics of countries may explain why<br>the Commission presents them with <span style='color:blue; font-weight:bold;'>friendly</span> or <span style='color:red; font-weight:bold;'>adversarial</span> language?",
                  caption = "Multivariate linear regression models of the average, embedding-based friendly/adversarial language scales\nof sentences mentioning a country in Commission public communication documents. Russia & Ukraine excluded.",
                  theme = theme(plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  

ggsave("./output/multivariate_plots/CountryFriendFoe_Explained_noRUUA.png", pl.comb, 
       width = 16, height = 20, units = "cm")



# What is going on here?

ru22 <- cm.foreign %>% 
  filter(iso2c == "RU" & year == 2022) %>% 
  arrange(desc(ff)) %>% 
  left_join(sent %>% select(sent_id, text_sent), by = "sent_id")

# Many wrong attributions
# Russia should not benefit from European expertise and cooperation

#write_rds(ru22, paste0(data_path, "RussiaSentences2022.rds"))


# With alternative aditive aggregATION OF SCALINGS ########

# friend/foe language by country - full period
ff.full <- cm.foreign %>% 
  group_by(iso2c) %>% 
  summarise(ff = mean(ff2, na.rm = T)) %>% 
  ungroup()


# Coop/confl language by country and year
ff.ann <- cm.foreign %>% 
  #   filter(sent_id %in% focused_sents$sent_id) %>%
  group_by(iso2c, year) %>% 
  summarise(ff = mean(ff2, na.rm = T)) %>% 
  ungroup()




# Multivariate analyses ####
# Why does the Commission couch specific foreign countries in cooperative or conflictive language? ####


# Combine country language and country factor data

country_factors <- cp %>% select(iso2c, year,
                                 distance_eu,
                                 eu_import_dependency, eu_export_dependency,
                                 eu.fta,
                                 gdp_worldshare,
                                 vdem.libdem,
                                 nato_member, 
                                 milex_total, armed_conflicts) %>% 
  rename(eu_fta = eu.fta,
         libdem = vdem.libdem)

country_factors$eu_fta[is.na(country_factors$eu_fta)] <- 0 # Plausible assumption


df <- ff.ann %>% # Annual language around mentioning a specific foreign country
  left_join(country_factors, by = c("iso2c", "year"))

# Check completeness of explanatory variables
df$exp_complete <- complete.cases(df[, 4:ncol(df)])
sum(df$exp_complete) # 4325 of 5552, sigh


# NAs by variable
miss_vars <- colSums(is.na(df[, 4:ncol(df)])) %>% 
  as.data.frame() %>% 
  rename(nas = 1) %>% 
  arrange(desc(nas))

# Data available for modelling
reg.df_noWar <- df %>% 
  filter(!iso2c %in% c("RU", "UA")) %>% 
  filter(exp_complete) %>% 
  select(-exp_complete)

reg.df <- df %>% 
  # filter(!iso2c %in% c("RU", "UA")) %>% 
  filter(exp_complete) %>% 
  select(-exp_complete)

table(reg.df$year) # Available n of country obs per year


# Estimate salience models ####

# Function to estimate model by year and extract coefficents etc

ann_reg <- function(data, time.min, time.max) {
  
  # Select data and standardize
  data_std <- data %>% 
    filter(year >= time.min & year <= time.max) %>% 
    mutate(across(-c(iso2c, year), as.numeric)) %>% 
    mutate(across(-c(iso2c, year), scale)) 
  
  
  # De-select constants within this particular data set
  constants <- data_std %>% 
    select(-c(iso2c, year)) %>%
    pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
    group_by(var) %>% 
    summarise(sd = sd(value)) %>% 
    filter(#is.na(sd) | 
      sd == 0)
  data_reg <- data_std %>% 
    select(-c(constants$var))
  
  
  # Run regression
  est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
  
  # Extract coeff and cis
  results <- 
    tidy(est, conf.int = TRUE) %>%
    mutate(period = paste0(time.min, "-", time.max))
  
  # Return results
  return(results)
}


# Collect estimates across periods 

periods <- data.frame(time.min = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
                      time.max = c(1989, 1994, 1999, 2004, 2009, 2014, 2019, 2023))

coeff <- data.frame(NULL)
for (i in 1:nrow(periods)) {
  current <- ann_reg(reg.df, time.min = periods$time.min[i],time.max = periods$time.max[i])
  coeff <- rbind(coeff, current)
}


# Collect estimates for full period (one should cluster ses here ...)

data_reg <- reg.df %>% 
  mutate(across(-c(iso2c, year), as.numeric)) %>% 
  mutate(across(-c(iso2c, year), scale)) 
est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
coeff.full <- 
  tidy(est, conf.int = TRUE) %>%
  mutate(period = "Full\nperiod")
coeff <- rbind(coeff, coeff.full)
coeff$full <- coeff$period == "Full\nperiod"

# Significant on 95% level?
coeff$sig <- coeff$p.value < 0.05
sum(coeff$sig)

# Positively and negatively significant
coeff <- coeff %>% 
  mutate(direc = ifelse(sig & estimate < 0,
                        "neg", 
                        ifelse(sig & estimate > 0,
                               "pos",
                               "ns")) %>% 
           factor(levels = c("neg", "ns", "pos")))


max(abs(coeff$estimate)) # .45


# Nicer labels
coeff$labels <- coeff$term %>% 
  str_replace("distance_eu", "Distance") %>% 
  str_replace("libdem", "Liberal\ndemocracy\nindex") %>% 
  str_replace("eu_export_dependency", "EU's export\ndependency\non country") %>% 
  str_replace("eu_import_dependency", "EU's import\ndependency\non country") %>% 
  str_replace("eu_fta", "Free trade\nagreement\nwith the EU") %>% 
  str_replace("gdp_worldshare", "Global\nGDP\nshare") %>% 
  str_replace("milex_total", "Military\nspending\n(absolute)") %>% 
  str_replace("armed_conflicts", "Armed\nconflicts") %>% 
  str_replace("nato_member", "NATO\nmember") %>% 
  factor(levels = c("Distance",
                    "Liberal\ndemocracy\nindex",
                    "EU's export\ndependency\non country",
                    "EU's import\ndependency\non country",
                    "Free trade\nagreement\nwith the EU",
                    "Global\nGDP\nshare",
                    "Military\nspending\n(absolute)",
                    "Armed\nconflicts",
                    "NATO\nmember"))

# Plot

pl.period <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & !full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_line(aes(group =1), color = "black", linewidth = .3, linetype = "dotted")+
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "By 5-year periods",
       x = "", y ="")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),  
        # strip.background = element_blank(),             
        strip.placement = "outside",
        panel.grid.minor = element_blank())


pl.full <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.02, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "Average",
       x = "", y ="Standardized regression coefficients\n(with 95% confidence bands)\n")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y = element_blank(),  
        strip.background = element_blank(),
        panel.grid.minor = element_blank())

pl.comb <-
  pl.full+pl.period +
  plot_layout(widths = c(1,5))+
  plot_annotation(title = "Which characteristics of countries may explain why<br>the Commission presents them with <span style='color:blue; font-weight:bold;'>friendly</span> or <span style='color:red; font-weight:bold;'>adversarial</span> language?",
                  caption = "Multivariate linear regression models of the summed, embedding-based friendly/adversarial language scales\nof sentences mentioning a country in Commission public communication documents.",
                  theme = theme(plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  

ggsave("./output/multivariate_plots/AdditiveCountryFriendFoe_Explained.png", pl.comb, 
       width = 16, height = 20, units = "cm")

# without Russia / Ukraine: ####

coeff <- data.frame(NULL)
for (i in 1:nrow(periods)) {
  current <- ann_reg(reg.df_noWar, time.min = periods$time.min[i],time.max = periods$time.max[i])
  coeff <- rbind(coeff, current)
}


# Collect estimates for full period (one should cluster ses here ...)

data_reg <- reg.df_noWar %>% 
  mutate(across(-c(iso2c, year), as.numeric)) %>% 
  mutate(across(-c(iso2c, year), scale)) 
est <- lm(ff ~ ., data = data_reg %>%  select(-c(iso2c, year)))
coeff.full <- 
  tidy(est, conf.int = TRUE) %>%
  mutate(period = "Full\nperiod")
coeff <- rbind(coeff, coeff.full)
coeff$full <- coeff$period == "Full\nperiod"

# Significant on 95% level?
coeff$sig <- coeff$p.value < 0.05
sum(coeff$sig)

# Positively and negatively significant
coeff <- coeff %>% 
  mutate(direc = ifelse(sig & estimate < 0,
                        "neg", 
                        ifelse(sig & estimate > 0,
                               "pos",
                               "ns")) %>% 
           factor(levels = c("neg", "ns", "pos")))


max(abs(coeff$estimate)) # .45


# Nicer labels
coeff$labels <- coeff$term %>% 
  str_replace("distance_eu", "Distance") %>% 
  str_replace("libdem", "Liberal\ndemocracy\nindex") %>% 
  str_replace("eu_export_dependency", "EU's export\ndependency\non country") %>% 
  str_replace("eu_import_dependency", "EU's import\ndependency\non country") %>% 
  str_replace("eu_fta", "Free trade\nagreement\nwith the EU") %>% 
  str_replace("gdp_worldshare", "Global\nGDP\nshare") %>% 
  str_replace("milex_total", "Military\nspending\n(absolute)") %>% 
  str_replace("armed_conflicts", "Armed\nconflicts") %>% 
  str_replace("nato_member", "NATO\nmember") %>% 
  factor(levels = c("Distance",
                    "Liberal\ndemocracy\nindex",
                    "EU's export\ndependency\non country",
                    "EU's import\ndependency\non country",
                    "Free trade\nagreement\nwith the EU",
                    "Global\nGDP\nshare",
                    "Military\nspending\n(absolute)",
                    "Armed\nconflicts",
                    "NATO\nmember"))

# Plot

pl.period <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & !full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_line(aes(group =1), color = "black", linewidth = .3, linetype = "dotted")+
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "By 5-year periods",
       x = "", y ="")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),  
        # strip.background = element_blank(),             
        strip.placement = "outside",
        panel.grid.minor = element_blank())


pl.full <- 
  ggplot(coeff %>% filter(term != "(Intercept)" & full), 
         aes(x = period, y= estimate, ymin = conf.low, ymax=conf.high, color = direc))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .2) +
  geom_linerange(linewidth = .6)+
  geom_point(shape = 16, size = 2)+
  facet_grid(labels ~ .)+
  # scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.02, 0)) +
  scale_y_continuous(breaks = c(-.5,0,.5), expand = c(0,0)) +
  scale_color_manual(values = c("darkred", "grey70", "blue"))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(title = "Average",
       x = "", y ="Standardized regression coefficients\n(with 95% confidence bands)\n")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "italic", size = 10),
        axis.text = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y = element_blank(),  
        strip.background = element_blank(),
        panel.grid.minor = element_blank())

pl.comb <-
  pl.full+pl.period +
  plot_layout(widths = c(1,5))+
  plot_annotation(title = "Which characteristics of countries may explain why<br>the Commission presents them with <span style='color:blue; font-weight:bold;'>friendly</span> or <span style='color:red; font-weight:bold;'>adversarial</span> language?",
                  caption = "Multivariate linear regression models of the summed, embedding-based friendly/adversarial language scales\nof sentences mentioning a country in Commission public communication documents. Russia & Ukraine excluded.",
                  theme = theme(plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  

ggsave("./output/multivariate_plots/AdditiveCountryFriendFoe_Explained_noRUUA.png", pl.comb, 
       width = 16, height = 20, units = "cm")




# weaponized interdependence
# WI_plot <- cmsc %>% 
#   ggplot(aes(x = economy, y = security))+
#   geom_jitter()+ 
#   geom_abline(intercept = 0, slope = 1)+ 
#   theme_minimal()+
#   facet_wrap(vars(year))
# 
# ggsave("./output/multivariate_plots/WI.png", WI_plot, 
#        width = 16, height = 20, units = "cm")
