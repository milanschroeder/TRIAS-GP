#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Calculate net friendliness scores (frequency weighted signals)
# Author:   @ChRauh (26.09.2025)
#########################################################################

# Intuition

# The average friendliness score is agnostic to the frequency of the signal, it asks:
# If a country gets mentioned at all, how friendly does the Comm present it?

# But given heavy frequency variation, one could argue that the signal is a different one
# if it is repeated, say, a hundred times (even though the average would be the same)

# I am thus playing around with a kind of "net tone" measure that weights the average by frequency
# This measure asks: How much total positive or negative signal did the Commission send about country x?

# Average friendliness ignores attention, treating low- and high-salience cases equally.
# Net friendliness (ideally in normalized form) explicitly fuses direction with salience, so it reflects both what is said and how much.

# May help in separating countries more clearly - in cross section and over time


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.6.1
library(GGally) # Extension to 'ggplot2' CRAN v2.1.2
library(grid)
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics CRAN v2.3
library(gtable) # Arrange 'Grobs' in Tables CRAN v0.3.6
library(patchwork) # The Composer of Plots CRAN v1.3.0
library(ggtext) # Improved Text Rendering Support for 'ggplot2' CRAN v0.1.2


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# Load the data #####
# Country mentions with scale values and meta information


# Scale values of country phrases (friend_foe scale only)
glove <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds"))  %>%
  select(c(1:5, friend_foe)) %>% 
  mutate(year = as.numeric(year))

# Sentence data
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  # left_join(glove %>% select(sent_id, friend_foe), by = join_by(sent_id)) %>% 
  filter(lang_sent == "en" & # only English sentences
           doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement") &
           !clearly_table & !mostly_numbers & !is_link) 

# Country mentions - long form, English sentences only
cm <- read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>% 
  left_join(glove %>% select(sent_id, iso2c, friend_foe), join_by(sent_id, iso2c)) %>% 
  #left_join(lsx %>% select(sent_id, coop_confl_lsx), join_by("sent_id")) %>% 
  left_join(sent %>% select(sent_id, doc_id, doc_type), join_by(sent_id))


# Filter country mentions 
# to include only those states in larger IR/IO data sets
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
rm(glove)
gc()



# A net tone function ####

# Arguments
#   data         : data frame
#   country_col  : name of country column (string)
#   year_col     : name of year column (string)
#   score_col    : name of score column (string)
#   doc_id_col   : name of document id column (string; required if dedupe=TRUE)
#   dedupe       : TRUE -> collapse to one value per (country,year,doc) first; FALSE -> use mention counts
#   per          : optional readability scaler (e.g., per 1000)

# Output (keeps original country/year column names):
#   <country>, <year>, N_units, net_direc, norm_net, norm_net_scaled

# dedupe = TRUE aggregates score to document level first (thus avoiding bias of overly long documents)
# With dedupe = TRUE, N_units is the number of documents mentioning the country that year; with dedupe = FALSE, it’s the number of mentions. The normalization adjusts accordingly.
# norm_net is comparable across years because it divides by the year’s total units (docs/mentions).
# Missing country-years (no mentions/docs) won’t appear—treat as missing rather than zero if you later build a balanced panel.


build_net_scores <- 
  function(data,
           country_col = "iso2c",
           year_col    = "year",
           score_col   = "friend_foe",
           doc_id_col  = "doc_id",
           dedupe      = TRUE,
           per         = 1000) 
  {
  stopifnot(all(c(country_col, year_col, score_col) %in% names(data)))
  if (dedupe) stopifnot(doc_id_col %in% names(data))
  
  library(dplyr)
  
  if (dedupe) {
    
    # 1) One row per (country, year, doc): average score within the doc
    doc_level <- data %>%
      group_by(.data[[country_col]], .data[[year_col]], .data[[doc_id_col]]) %>%
      summarise(doc_mean = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")
    
    # 2) Country-year aggregates (N_units = #docs)
    
    cy <- doc_level %>%
      group_by(.data[[country_col]], .data[[year_col]]) %>%
      summarise(
        N_units  = n(),                               # number of documents mentioning the country
        net_direc = sum(doc_mean, na.rm = TRUE),       # net tone volume across docs
        .groups  = "drop"
      )
    
  } else {
    
    # Mentions directly (N_units = #mentions)
    cy <- data %>%
      group_by(.data[[country_col]], .data[[year_col]]) %>%
      summarise(
        N_units  = n(),                               # number of mentions
        net_direc = sum(.data[[score_col]], na.rm = TRUE),  # net across mentions
        .groups  = "drop"
      )
  }
  
  # 3) Normalize by total units that year (docs if dedupe, mentions otherwise)
  year_totals <- cy %>%
    group_by(.data[[year_col]]) %>%
    summarise(total_units_y = sum(N_units, na.rm = TRUE), .groups = "drop")
  
  out <- cy %>%
    left_join(year_totals, by = setNames(year_col, year_col)) %>%
    mutate(
      norm_net        = ifelse(total_units_y > 0, net_direc / total_units_y, NA_real_),
      norm_net_scaled = per * norm_net
    ) %>%
    arrange(.data[[year_col]], .data[[country_col]])
  
  return(out)
}



# Get annual net direction scores ####

# Mean center the FF score first
# So that values below mean get subtracted ... ???
cm <- cm %>% 
  mutate(ff_centered = friend_foe - mean(friend_foe, na.rm = TRUE))

# Calculate net scoes
nets <- 
  build_net_scores(
  data        = cm,
  country_col = "iso2c",
  year_col    = "year",
  score_col   = "ff_centered", # ????
  doc_id_col  = "doc_id",
  dedupe      = TRUE   # normalize by documents
)

nets$country <- countrycode(nets$iso2c, origin = "iso2c", destination = "country.name")
nets$country[nets$iso2c =="KV"] <- "Kosovo"


baseline <- mean(nets$norm_net)

nets %>%
  filter(iso2c %in% c("US", "BR", "RU", "IN", "CN", "ZA")) %>%
  mutate(country = factor(country, levels = c("United States", "Brazil", "Russia", "India", "China", "South Africa"))) %>%
  ggplot(aes(x = year, y = norm_net, color = country)) +
  geom_line() +
  geom_hline(yintercept = baseline)+
  facet_wrap(~country, ncol = 2) +  
  theme_bw(base_size = 12) +
  theme(legend.position = "none") 


# Compare to means to see where it makes a diff ####

av <- cm %>% 
  group_by(iso2c, doc_id, year) %>% 
  summarise(ff = mean(friend_foe, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(iso2c, year) %>% 
  summarise(ff_mean = mean(ff, na.rm = T)) %>% 
  ungroup()

nets <- nets %>% 
  left_join(av, by = c("iso2c", "year"))

nets$cy <- paste0(nets$iso2c, "-", as.character(nets$year))

ggplot(nets, aes(x=ff_mean, y = norm_net))+
  geom_point() 

# Ok - the weight factor shrinks norm_net toward 0 for rarely mentioned countries, while the average tone can still be strongly positive or negative.
# But that norm_net is only reserved to the positive scale (except Russia an UA) is strange


# Look at the middle east

countrycodes <- countries %>% select(country, iso2c) %>% unique()

nets %>%
  filter(iso2c %in% c("IL", "PS", "SA", "SY", "IQ", "AF", "TR")) %>%
  # mutate(country = factor(country, levels = c("United States", "Brazil", "Russia", "India", "China", "South Africa"))) %>%
  ggplot(aes(x = year, y = norm_net, color = country)) +
  geom_line() +
  geom_hline(yintercept = baseline)+
  facet_wrap(~country, ncol = 2) +  
  theme_bw(base_size = 12) +
  theme(legend.position = "none") 




# Maps ####

# Packages - for mapping
library(sf) # Simple Features for R CRAN v1.0-21
library(rnaturalearth) # World Map Data from Natural Earth CRAN v1.0.1
library(rnaturalearthdata) # World Vector Map Data from Natural Earth Used in 'rnaturalearth' CRAN v1.0.0
library(ggspatial) # Spatial Data Framework for ggplot2 CRAN v1.1.9

# Get a 'world' data frame from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_a3[world$iso_a3_eh == "FRA"] <- "FRA" # Missing ISO for some reason
world$iso_a3[world$iso_a3_eh == "NOR"] <- "NOR" # Missing ISO for some reason
world$iso_a2[world$iso_a3_eh == "FRA"] <- "FRA" # Missing ISO for some reason
world$iso_a2[world$sovereignt == "Kosovo"] <- "KV" # Match newsmap code
world$iso2c <- world$iso_a2 # For matching

# Calculate data for timeperiods

df85_89 <- nets %>% 
  filter(year >= 1985 & year <= 1989) %>% 
  group_by(iso2c) %>% 
  summarise(ff85_89 = mean(norm_net, na.rm = T),
            mentions85_89 = n()) %>% 
  ungroup() 

df90_00 <- nets %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(iso2c) %>% 
  summarise(ff90_00 = mean(norm_net, na.rm = T),
            mentions90_00 = n()) %>% 
  ungroup() 

df01_10 <- nets %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  group_by(iso2c) %>% 
  summarise(ff01_10 = mean(norm_net, na.rm = T),
            mentions01_10 = n()) %>% 
  ungroup() 

df11_23 <- nets %>% 
  filter(year >= 2011 & year <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(ff11_23 = mean(norm_net, na.rm = T),
            mentions11_23 = n()) %>% 
  ungroup() 

dfall <- nets %>% 
  group_by(iso2c) %>% 
  summarise(ffall = mean(norm_net, na.rm = T),
            mentionsall = n()) %>% 
  ungroup() 

# Benchmarks for plotting - They do a lot!
# bench <-
#   dfall %>%
#   summarise(
#     mean = mean(ffall, na.rm = TRUE),
#     se   = sd(ffall, na.rm = TRUE) / sqrt(sum(!is.na(ffall))),
#     ci_low  = mean - 2.58 * se,
#     ci_high = mean + 2.58 * se
#   )

m <- mean(dfall$ffall, na.rm = TRUE)
qs <- quantile(dfall$ffall, c(0.05, 0.95), na.rm = TRUE)  # Drops highest and lowest X percent, adjust tails as needed
r  <- max(m - qs[1], qs[2] - m)                          # make it symmetric around mean


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
  geom_sf(aes(fill = ffall), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       mid = "grey",
                       na.value = "white",
                       midpoint = m,
                       limits   = m + c(-r, r),
                       oob=scales::squish)+
  labs(title = "Full period (1985-2023)",
       fill = "Adversarial vs. friendly language around country mentions: \n ",
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
  geom_sf(aes(fill = ff85_89), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       mid = "grey",
                       na.value = "white",
                       midpoint = m,
                       limits   = m + c(-r, r),
                       oob=scales::squish)+
  labs(title = "1985-1990",
       fill = "Adversarial vs. friendly language around country mentions: \n ",
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
  geom_sf(aes(fill = ff90_00), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       mid = "grey",
                       na.value = "white",
                       midpoint = m,
                       limits   = m + c(-r, r),
                       oob=scales::squish)+
  labs(title = "1990-2000",
       fill = "Adversarial vs. friendly language around country mentions: \n ",
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
  geom_sf(aes(fill = ff01_10), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       mid = "grey",
                       na.value = "white",
                       midpoint = m,
                       limits   = m + c(-r, r),
                       oob=scales::squish)+
  labs(title = "2001-2010",
       fill = "Adversarial vs. friendly language around country mentions: \n ",
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
  geom_sf(aes(fill = ff11_23), 
          color = "black", linewidth = .02)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_alpha_continuous(range = c(.1, 1), limits = c(0,1))+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       mid = "grey",
                       na.value = "white",
                       midpoint = m,
                       limits   = m + c(-r, r),
                       oob=scales::squish)+
  labs(title = "2011-2023",
       fill = "Adversarial vs. friendly language around country mentions: \n ",
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

pl.comb

ggsave("./output/descriptive_plots/new/FriendFoeCountries_Maps_OverTime_NETdirection.png", pl.comb, height = 22, width = 36, units = "cm")







