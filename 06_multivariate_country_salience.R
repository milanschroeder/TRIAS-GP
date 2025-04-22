#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Multivariate, cross-sectional analysis of how frequently 
#           the Commission speaks about types of countries over time
# Author:   @ChRauh (27.02.2025)
#########################################################################

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
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



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


# Country mentions - doc level ####
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_doclevel.rds")) %>% 
  filter(doc_id %in% docs$doc_id) %>% # Filter as above
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  select(c(doc_id, year, BI:ncol(.))) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") # Easier to handle


# External country/year panel ####
cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data states (= official states at some point)
length(unique(cp$iso2c)) # 213
# Newsmap dictionary is somewhat broader
length(unique(cm$iso2c)) # 241
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))

# Note that this is a complete country/year panel!
# We have every country in every year so that analyses below are consistent
# no matter whether country was actually mentioned or not!
# test <- cm %>% group_by(year) %>% summarise(countries = length(unique(iso2c)))
# table(test$countries)
# rm(test)




# Salience data ####
# Share of docs (per year) that mention a country at least once as the most straightforward measure

# Only non-eu countries
cm.foreign <- cm %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(!eu_member) %>% # Filter
  select(-eu_member)

# documents that specifically talk about one non-eu state (clearer interpretation of friend-foe etc.)
focused_docs <- cm.foreign %>% 
  filter(mentions != 0) %>% 
  group_by(doc_id) %>% 
  filter(n() == 1) %>% 
  ungroup()

# Share of documents mentioning each country - full period
mentions.full <- cm.foreign %>% 
  filter(!iso2c %in% c("RU", "UA")) %>%
  group_by(iso2c) %>% 
  summarise(doc_share = mean(as.logical(mentions))) %>% 
  ungroup()

# Share of documents mentioning each country - by year
mentions.ann <- 
  cm.foreign %>% 
  filter(!iso2c %in% c("RU", "UA")) %>% 
  #  filter(doc_id %in% focused_docs$doc_id) %>%
  group_by(iso2c, year) %>% 
  summarise(doc_share = mean(as.logical(mentions))) %>% 
  ungroup()



# Concentration measure ####
# How narrow or diversified is the Commission's focus on foreign countries?

sum_shares <- mentions.ann %>% 
  group_by(year) %>% 
  summarise(sum_share = sum(doc_share)) %>% 
  ungroup()

hhi <- mentions.ann %>% 
  left_join(sum_shares, by = "year") %>% 
  mutate(share_share = doc_share/sum_share) %>% # which share does country cover among all FC mentions?
  group_by(year) %>%
  summarise(hhi = sum(share_share^2)) %>% # Concentration index
  arrange(year)

pl.con <-
  ggplot(hhi, aes(x=year, y = hhi, group = 1))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  labs(title = "How diversified or concentrated is the Commission's communication about foreign countries?",
       subtitle = "<br>Values show the <span style='color:blue; font-weight:bold;'>Herfindahl-Hirschman-Index across mentions of non-EU states</span> within a year.<br>A value of 1 would mean that the Commission talks about one state only,<br>while 0 would indicate an equal distribution of mentions across foreign countries.<br>",
       x = "", y ="")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = .5),
        plot.subtitle = element_markdown(hjust = .5),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(color="blue", face = "bold"))

ggsave("./output/descriptive_plots/ConcentrationOfCountryMentions_OverTime.png", pl.con, height = 10, width = 25, units = "cm")
# ggsave("./output/descriptive_plots/ConcentrationOfCountryMentions_OverTime_noRU.png", pl.con, height = 10, width = 25, units = "cm")

rm(pl.con, hhi, sum_shares)  



# Multivariate analyses - why does the Commission mention specific foreign countries? ####


# (Crude) hypotheses, link to geopolitics (if positive effect), variable in country panel

# Territority / regional power (GP: +/-): distance_eu 

# Import dependency (GP: +/-): eu_import_dependency
# Export dependency (GP: +/-): eu_export_dependency

# Development status (GP: - ) gdp_capita
# World power (GP +): gdp_worldshare

# Free trade agreement (GP-): eu_fta
# World Trade Organisation (GP-): wto_member

# Alliances own (GP+): nato_member, osce_member, 
# Security alliances foreign (GP): csto_meber, sco_member, mercosur_member, asean_member

# Military power: milex_total
# Armed conflicts: armed_conflicts


# Combine country salience and country factor data

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


df <- mentions.ann %>% # Annual share of docs mentioning a specific foreign country
  left_join(country_factors, by = c("iso2c", "year"))


# Check completeness of explanatory variables
df$exp_complete <- complete.cases(df[, 4:ncol(df)])
sum(df$exp_complete) # 4601 of 6441, sigh

# NAs by variable
miss_vars <- colSums(is.na(df[, 4:ncol(df)])) %>% 
  as.data.frame() %>% 
  rename(nas = 1) %>% 
  arrange(desc(nas))

# Milex kills around 1500
# Vdem 700
# GDP data around 500
# Trade dependency another 261

# tough choice here - theoretical relevance vs coverage
# I go for theoretical relevance - discuss with Milan


# Data available for modelling
reg.df <- df %>% 
  filter(!iso2c %in% c("RU", "UA")) %>% 
  filter(exp_complete) %>% 
  select(-exp_complete)

table(reg.df$year) # Available n of country obs per year


# Estimate salience models ####

# The basic model
# reg.formula <- paste0("doc_share ~ ",
#                       paste(names(reg.df[4:ncol(reg.df)]), collapse = " + "))
# glue(reg.formula)

# Average model, full period
# fit <-
#   lm(glue(reg.formula),
#    reg.df)
# summary(fit)

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
    filter(is.na(sd) | sd == 0)
  data_reg <- data_std %>% 
    select(-c(constants$var))
    

  # Run regression
  est <- lm(doc_share ~ ., data = data_reg %>%  select(-c(iso2c, year)))

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
est <- lm(doc_share ~ ., data = data_reg %>%  select(-c(iso2c, year)))
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


max(abs(coeff$estimate))


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
  plot_annotation(title = "Which kind of foreign countries does the Commission speak about?",
                  subtitle = "<br>How different country characteristics relate to how frequently the Commission<br> mentions these countries in its public communication.<br>Statistically significant estimates (p<.05)<br>marked in <span style='color:blue; font-weight:bold;'>blue (positive)</span> or <span style='color:darkred; font-weight:bold;'>red (negative)</span>.<br>",
                  caption = "Multivariate linear regression models of the share of Commission documents mentioning a country.",
                  theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(hjust = 0.5)))  

ggsave("./output/multivariate_plots/CountrySalience_Explained.png", pl.comb, 
       width = 16, height = 20, units = "cm")

# full country year panel
# Fill shares
# Merge complete cp
# plot complete case over time to determine range

# Develop (and write down hypotheses)

# Estimate full model - lm with clustered se
# Estimate lm per year standardized 
# Coeff, cis, r2 into joitn data frame
# MArk significance

# Plot coeff with pointrange over time 



