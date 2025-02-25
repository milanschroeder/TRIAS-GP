#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Compare differently derived text scales
# Author:   @ChRauh (25.02.2025)
#########################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(quanteda) # Quantitative Analysis of Textual Data CRAN v3.3.0
library(LSX) # Semi-Supervised Algorithm for Document Scaling CRAN v1.3.1
library(GGally) # Extension to 'ggplot2' CRAN v2.1.2
library(countrycode)


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Sentences for qualitative checks ####
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  mutate(wordcount = str_count(text_sent, "\\s+")) %>% 
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric())



# The scaled sentences ####


# GloVe 
glove <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_sentlevel.rds"))

# Directionality of the GloVe scales is off, my bad, currently the higher, the more friendly ...
# Re-run this later, invert here for now
glove <- glove %>% 
  mutate(gti = max(gti, na.rm = T) + min(gti, na.rm = T) - gti,
         coop_confl = max(coop_confl, na.rm = T) + min(coop_confl, na.rm = T) - coop_confl,
         friend_foe = max(friend_foe, na.rm = T) + min(friend_foe, na.rm = T) - friend_foe)

glove <- glove %>% 
  rename_with(~ paste0(., "_glove"), .cols = 2:9)

# LSX
lsx <- read_rds(paste0(data_path, "cleaned_data/scaling_lsx_sentlevel.rds")) %>% 
  select(-doc_id) %>% 
  rename_with(~ paste0(., "_lsx"), .cols = 2:9)


# Note the difference in N!
# A notable number of around 29k sentences dropped out in GloVe scaling, but not in LSX
test <- sent %>% 
  filter(!(sentence_id %in% glove$sentence_id))
# A very fine collection of stopwords, numbers, and Greek or Bulgraina text (do we need to worry that our LSX models is trained on that?)
rm(test)
gc()



# Scale correlations on sentence level  #####

comp <- glove %>% 
  select(sentence_id, gti_glove, coop_confl_glove, friend_foe_glove) %>% 
  left_join(lsx %>% 
              select(sentence_id, gti_lsx, coop_confl_lsx, friend_foe_lsx),
            by = "sentence_id") %>% 
  select(-sentence_id)

comp.pl <- 
  ggpairs(comp %>% sample_n(50000),
          lower = list(continuous = wrap("points", alpha = 0.05, shape = 16)),  # Set alpha and shape
          upper = list(continuous = wrap("cor", size = 6, color = "black")),
          columnLabels = c("GloVe: GTI (Hostility)", "GloVe: Coop-Conflict", "GloVe: Friend-Foe",
                           "LSX: GTI (Hostility)", "LSX: Coop-Conflict", "LSX: Friend-Foe"))+
  labs(title = "Comparing differently seeded scales on sentence level",
       subtitle = "Random sample of 50,000 sentences from Comm. corpus")+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size = 10)  
  )

ggsave("./output/scale_comp_plots/ComparingScalingWeigthsSentenceLevel_GloVe_LSX.png", comp.pl, width = 30, height = 30, units = "cm")


# All positively correlated, but some very weakly
# Within corpus scaling results in much more peaked distributions - less nuance, more noise
# Cross-correlations in the LSX cluster lower than in the GloVe Cluster

# Average cross-correlations
# FF/GLOVE: .5196
# COOP-CONF/GLOVE: .4934
# FF/LSX: .3898
# GTI/GLOVE: .3228
# GTI/LSX: .3056
# COOP-CONF/LSX: .3048

# => If we assume that all of these scales tap into the same semantic dimension,
# FF/FLOVE would be its most robust representation
# Recall: FF/GLOVE was similar on the word-vector space consistency metrics to GTI/GLOVE, and superior to CC/GloVe

rm(comp)
gc()



# Face validity checks: top-scoring sentences ####

sent <- sent %>% 
  left_join(glove, by = "sentence_id") %>% 
  left_join(lsx, by = "sentence_id")

# GloVe/FF - high
sent %>% filter(lang_sent == "en") %>% arrange(desc(friend_foe_glove)) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(desc(friend_foe_glove)) %>% select(text_sent) %>% head(20) %>% pull()
# Looks good - but faulty country attributions (RU/Ukraine examples) as in typical sentiment analysis

# GloVe/FF - low
sent %>% filter(lang_sent == "en") %>% arrange(friend_foe_glove) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(friend_foe_glove) %>% select(text_sent) %>% head(20) %>% pull()
# Looks good


# GloVe/GTI - high
sent %>% filter(lang_sent == "en") %>% arrange(desc(gti_glove)) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(desc(gti_glove)) %>% select(text_sent) %>% head(20) %>% pull()
# The longer ones look strange, very technical/scientific stuff - inferior to FF

# GloVe/GTI - low
sent %>% filter(lang_sent == "en") %>% arrange(gti_glove) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(gti_glove) %>% select(text_sent) %>% head(20) %>% pull()
# Not too bad, but strongly "aid"-driven - inferior to FF at first sight


# FF/LSX - high
sent %>% filter(lang_sent == "en") %>% arrange(desc(friend_foe_lsx)) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(desc(friend_foe_lsx)) %>% select(text_sent) %>% head(20) %>% pull()
# We seem to measure taxation issues here ... not good

# FF/LSX - low
sent %>% filter(lang_sent == "en") %>% arrange(friend_foe_lsx) %>% select(text_sent) %>% head(20) %>% pull()
sent %>% filter(lang_sent == "en") %>% filter(wordcount >= 12) %>% arrange(friend_foe_lsx) %>% select(text_sent) %>% head(20) %>% pull()
# Some good examples here, but also strange country lists where single terms beyond geo.entities seem to have a strong effect - not good



# Face validity check - most hostile countries in 2022 (year of Russion invasion of Ukraine) ####

# Country mentions on sentence level
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel.rds"))
names(cm)
cm <- cm %>% 
  select(-c(starts_with("doc_"), date, location, starts_with("n_"), lang_sent,
            ends_with("_expl"), ends_with("_total"),
            ASIA, EUROPE, OCEANIA, AFRICA, AMERICA))

# Only English sentences from 2022
sentences <- sent %>% filter(year == 2022 & lang_sent == "en") %>% select(sentence_id) %>% pull()
cm <- cm %>% 
  filter(sentence_id %in% sentences)
gc()

# Long format, dropping non-mentioned countries per sentence
cm2 <- cm %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "iso2c", values_to = "mentioned") %>% 
  filter(mentioned != 0) %>% 
  select(-mentioned) %>% 
  mutate(country = countrycode(iso2c, "iso2c", "country.name"))
cm2$country[cm2$iso2c == "KV"] <- "Kosovo"

# Merge sentence scales
cm2 <- cm2 %>% 
  left_join(glove, by = "sentence_id") %>% 
  left_join(lsx, by = "sentence_id")

# Aggregate to country level
# think about summing the values at some stage ...
cm3 <- cm2 %>% 
  group_by(iso2c, country) %>% 
  summarise(mentions = n(),
            gti_glove = mean(gti_glove, na.rm = T),
            friend_foe_glove = mean(friend_foe_glove, na.rm = T),
            coop_confl_glove = mean(coop_confl_glove, na.rm = T),
            gti_lsx = mean(gti_lsx, na.rm = T),
            friend_foe_lsx = mean(friend_foe_lsx, na.rm = T),
            coop_confl_lsx = mean(coop_confl_lsx, na.rm = T)) %>% 
  ungroup()


# Cross correlations on country level
comp <- cm3 %>% select(iso2c, 
                       gti_glove, coop_confl_glove, friend_foe_glove,
                       gti_lsx, coop_confl_lsx, friend_foe_lsx)

comp.pl <- 
  ggpairs(comp[, 2:7],
          lower = list(continuous = wrap("points", alpha = 0.1, shape = 16)),  # Set alpha and shape
          upper = list(continuous = wrap("cor", size = 6, color = "black")),
          columnLabels = c("GloVe: GTI (Hostility)", "GloVe: Coop-Conflict", "GloVe: Friend-Foe",
                           "LSX: GTI (Hostility)", "LSX: Coop-Conflict", "LSX: Friend-Foe"))+
  labs(title = "Comparing differently scaled country mentions during 2022",
       subtitle = "Average scale values across all mentions of an individual country during 2022 (n = 195)")+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size = 10)  
  )

ggsave("./output/scale_comp_plots/ComparingScalingWeigths_CountryLevel_2022.png", comp.pl, width = 30, height = 30, units = "cm")

# Similar picture as for the sample of sentences above
# GloVe scales more normally distributed and stronger correlated among each other than LSX values
# FF/Glove with highest cross-correlations overall


# Top25 hostile countries during 2022
comp <- rbind(
  cm3 %>% arrange(desc(gti_glove)) %>% head(25) %>% select(country, mentions, gti_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: GTI (Hostility)"),
  cm3 %>% arrange(desc(friend_foe_glove)) %>% head(25) %>% select(country, mentions, friend_foe_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Friend-Foe"),
  cm3 %>% arrange(desc(coop_confl_glove)) %>% head(25) %>% select(country, mentions, coop_confl_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Coop-Conflict"),
  cm3 %>% arrange(desc(gti_lsx)) %>% head(25) %>% select(country, mentions, gti_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: GTI (Hostility)"),
  cm3 %>% arrange(desc(friend_foe_lsx)) %>% head(25) %>% select(country, mentions, friend_foe_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Friend-Foe"),
  cm3 %>% arrange(desc(coop_confl_lsx)) %>% head(25) %>% select(country, mentions, coop_confl_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Coop-Conflict")
)


ggplot(comp, aes(y = tidytext::reorder_within(country, value, scale), x = value, alpha = mentions, color = (country == "Russia"))) +
  geom_col(fill = "blue", width = .7) +
  facet_wrap(.~scale, scales = "free") +  # Free x and y scales
  tidytext::scale_y_reordered() +  # Fix reordered factor display
  scale_color_manual(values = c("white", "darkred"))+
  guides(color = "none")+
  scale_alpha_continuous(range = c(.3, 1))+
  labs(title = "Face-validity check\nMost \"hostile\" countries in 2022 by average scale value", 
       subtitle = "Expectation is that Russia pops out!",
       x = "Average scale value across all sentences mentioning the country", 
       y = "Top-25 countries\non each scale", 
       alpha = "Mentions") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 6),
        panel.background = element_rect(fill = "white", color = NA),  # White plot area
        plot.background = element_rect(fill = "white", color = NA),   
        legend.background = element_rect(fill = "white", color = NA))

ggsave("./output/scale_comp_plots/MostHostileCountries_ByAverageValueOfScale.png", width = 28, height = 16, units = "cm")

# FF/GloVe clearly meets the expectation the best
# But overall small island states that get mentioned a few times only ... maybe summing is better than averaging



# Top25 hostile countries during 2022 by sum of scale values
cm4 <- cm2 %>% 
  group_by(iso2c, country) %>% 
  summarise(mentions = n(),
            gti_glove = sum(gti_glove, na.rm = T),
            friend_foe_glove = sum(friend_foe_glove, na.rm = T),
            coop_confl_glove = sum(coop_confl_glove, na.rm = T),
            gti_lsx = sum(gti_lsx, na.rm = T),
            friend_foe_lsx = sum(friend_foe_lsx, na.rm = T),
            coop_confl_lsx = sum(coop_confl_lsx, na.rm = T)) %>% 
  ungroup()


comp <- rbind(
  cm4 %>% arrange(desc(gti_glove)) %>% head(25) %>% select(country, mentions, gti_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: GTI (Hostility)"),
  cm4 %>% arrange(desc(friend_foe_glove)) %>% head(25) %>% select(country, mentions, friend_foe_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Friend-Foe"),
  cm4 %>% arrange(desc(coop_confl_glove)) %>% head(25) %>% select(country, mentions, coop_confl_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Coop-Conflict"),
  cm4 %>% arrange(desc(gti_lsx)) %>% head(25) %>% select(country, mentions, gti_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: GTI (Hostility)"),
  cm4 %>% arrange(desc(friend_foe_lsx)) %>% head(25) %>% select(country, mentions, friend_foe_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Friend-Foe"),
  cm4 %>% arrange(desc(coop_confl_lsx)) %>% head(25) %>% select(country, mentions, coop_confl_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Coop-Conflict")
)


ggplot(comp, aes(y = tidytext::reorder_within(country, value, scale), x = value, alpha = mentions, color = (country == "Russia"))) +
  geom_col(fill = "blue", width = .7) +
  facet_wrap(.~scale, scales = "free") +  # Free x and y scales
  tidytext::scale_y_reordered() +  # Fix reordered factor display
  scale_color_manual(values = c("white", "darkred"))+
  guides(color = "none")+
  scale_alpha_continuous(range = c(.3, 1))+
  labs(title = "Face-validity check\nMost \"hostile\" countries in 2022 by sum of scale values", 
       subtitle = "Expectation is that Russia pops out!",
       x = "Sum of scale values across all sentences mentioning the country", 
       y = "Top-25 countries\non each scale", 
       alpha = "Mentions") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 6),
        panel.background = element_rect(fill = "white", color = NA),  # White plot area
        plot.background = element_rect(fill = "white", color = NA),   
        legend.background = element_rect(fill = "white", color = NA))

ggsave("./output/scale_comp_plots/MostHostileCountries_BySumOfScaleValues.png", width = 28, height = 16, units = "cm")

# In this (implicitly frequency-weighted) perspective all scales point to Russia
# FF/GloVe looks actually bad - many EU countries score high here ...
# Both Coop-Conflict scales resemble my view on world politics during 2022 best (but recall, they score low on internal consistency)
# Ukraine example shows that we make wrong attributions (as it likely co-occures frequently with the russian aggressor)
# One way to approach the latter problem may be excluding co-mentions (= sentences in which more than one country is mentioned)


# Top25 hostile countries during 2022 by sum of scale values and excluding co-mentions

# Identify co-mentions (more than one country in sentence)
# rowwise takes a bit ...

comentions <- cm %>% 
  rowwise() %>% 
  mutate(country_mentions = sum(c_across(2:ncol(.)))) %>%
  ungroup() %>% 
  filter(country_mentions > 1) %>% 
  select(sentence_id) %>% 
  pull()

# Filter co_mentions from the country-level data
cm2_1 <- cm2 %>% 
  filter(!(sentence_id %in% comentions)) # Drops around 50% of sentences


# Re-run face validity check from above
cm5 <- cm2_1 %>% 
  group_by(iso2c, country) %>% 
  summarise(mentions = n(),
            gti_glove = sum(gti_glove, na.rm = T),
            friend_foe_glove = sum(friend_foe_glove, na.rm = T),
            coop_confl_glove = sum(coop_confl_glove, na.rm = T),
            gti_lsx = sum(gti_lsx, na.rm = T),
            friend_foe_lsx = sum(friend_foe_lsx, na.rm = T),
            coop_confl_lsx = sum(coop_confl_lsx, na.rm = T)) %>% 
  ungroup()


comp <- rbind(
  cm5 %>% arrange(desc(gti_glove)) %>% head(25) %>% select(country, mentions, gti_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: GTI (Hostility)"),
  cm5 %>% arrange(desc(friend_foe_glove)) %>% head(25) %>% select(country, mentions, friend_foe_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Friend-Foe"),
  cm5 %>% arrange(desc(coop_confl_glove)) %>% head(25) %>% select(country, mentions, coop_confl_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Coop-Conflict"),
  cm5 %>% arrange(desc(gti_lsx)) %>% head(25) %>% select(country, mentions, gti_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: GTI (Hostility)"),
  cm5 %>% arrange(desc(friend_foe_lsx)) %>% head(25) %>% select(country, mentions, friend_foe_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Friend-Foe"),
  cm5 %>% arrange(desc(coop_confl_lsx)) %>% head(25) %>% select(country, mentions, coop_confl_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Coop-Conflict")
)


ggplot(comp, aes(y = tidytext::reorder_within(country, value, scale), x = value, alpha = mentions, color = (country == "Russia"))) +
  geom_col(fill = "blue", width = .7) +
  facet_wrap(.~scale, scales = "free") +  # Free x and y scales
  tidytext::scale_y_reordered() +  # Fix reordered factor display
  scale_color_manual(values = c("white", "darkred"))+
  guides(color = "none")+
  scale_alpha_continuous(range = c(.3, 1))+
  labs(title = "Face-validity check\nMost \"hostile\" countries in 2022 by sum of scale values (excluding co-mentions)", 
       subtitle = "Expectation is that Russia pops out!",
       x = "Sum of scale value across all sentences mentioning the country", 
       y = "Top-25 countries\non each scale", 
       alpha = "Mentions") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 6),
        panel.background = element_rect(fill = "white", color = NA),  # White plot area
        plot.background = element_rect(fill = "white", color = NA),   
        legend.background = element_rect(fill = "white", color = NA))

ggsave("./output/scale_comp_plots/MostHostileCountries_BySumOfScaleValues_Wout_Comentions.png", width = 28, height = 16, units = "cm")

# looks largely similar to the picture including co-mentions

# Extremely skewed distribution of both GTI measures catches the eye (cf distributions seen above)
# FF/GloVe has the inverse problem - values in the middle add up quickly which might explain why we see so many EU mber states in this case

# Coop-Conflict resembles my view on world politic most again ...

# For all except GTI, "wrong" attributions to Ukraine are striking (aspect based-scaling!?)

# One alternative to the co-mention problem would be dividing the scale by the number of countries appearing in sentence
# Or is this effectively the same as the mean measure above?

# Balance between mean and sum might be log-weighted: summarise(weighted_mean = sum(scale_value) / log1p(n()), .groups = "drop")







# We can see country mentions conceptually in two ways
# Actor: Which country is fried of foe?
# Location: From where does the Comm see threats emerging?




# Top25 hostile countries during 2022 by log-weigthed average of scale values
cm6 <- cm2 %>% 
  group_by(iso2c, country) %>% 
  summarise(mentions = n(),
            gti_glove = sum(gti_glove, na.rm = T) / log1p(n()), 
            friend_foe_glove = sum(friend_foe_glove, na.rm = T)/ log1p(n()),
            coop_confl_glove = sum(coop_confl_glove, na.rm = T)/ log1p(n()),
            gti_lsx = sum(gti_lsx, na.rm = T)/ log1p(n()),
            friend_foe_lsx = sum(friend_foe_lsx, na.rm = T)/ log1p(n()),
            coop_confl_lsx = sum(coop_confl_lsx, na.rm = T)/ log1p(n())) %>% 
  ungroup()


comp <- rbind(
  cm6 %>% arrange(desc(gti_glove)) %>% head(25) %>% select(country, mentions, gti_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: GTI (Hostility)"),
  cm6 %>% arrange(desc(friend_foe_glove)) %>% head(25) %>% select(country, mentions, friend_foe_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Friend-Foe"),
  cm6 %>% arrange(desc(coop_confl_glove)) %>% head(25) %>% select(country, mentions, coop_confl_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Coop-Conflict"),
  cm6 %>% arrange(desc(gti_lsx)) %>% head(25) %>% select(country, mentions, gti_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: GTI (Hostility)"),
  cm6 %>% arrange(desc(friend_foe_lsx)) %>% head(25) %>% select(country, mentions, friend_foe_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Friend-Foe"),
  cm6 %>% arrange(desc(coop_confl_lsx)) %>% head(25) %>% select(country, mentions, coop_confl_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Coop-Conflict")
)


ggplot(comp, aes(y = tidytext::reorder_within(country, value, scale), x = value, alpha = mentions, color = (country == "Russia"))) +
  geom_col(fill = "blue", width = .7) +
  facet_wrap(.~scale, scales = "free") +  # Free x and y scales
  tidytext::scale_y_reordered() +  # Fix reordered factor display
  scale_color_manual(values = c("white", "darkred"))+
  guides(color = "none")+
  scale_alpha_continuous(range = c(.3, 1))+
  labs(title = "Face-validity check\nMost \"hostile\" countries in 2022 by sum of scale values (weighted by log frequency)", 
       subtitle = "Expectation is that Russia pops out!",
       x = "Weighted scale values across all sentences mentioning the country", 
       y = "Top-25 countries\non each scale", 
       alpha = "Mentions") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 6),
        panel.background = element_rect(fill = "white", color = NA),  # White plot area
        plot.background = element_rect(fill = "white", color = NA),   
        legend.background = element_rect(fill = "white", color = NA))

ggsave("./output/scale_comp_plots/MostHostileCountries_ByLogWeightedAverage.png", width = 28, height = 16, units = "cm")

# Flatter distributions, all other issues remain the same 



# Insights until here ####

# Within-corpus (LSX) scaling raises three fronts:
# 1 - low face validity in qualitative sentence checks
# 2 - narrow distributions (unless one assumes this is real)
# 3 - low cross-correlations

# FF/Glove and GTI/Glove on par with regad to internal scale consistence
# FF/Glove superior on cross-correlations, but weak in 2022 face validty check (picks Russia, but pulls up many EU mebers as well)
# CC/Glove weak on internal consistency, but plausible picture in 2022 example ...

# Wrong attributions of context to country is an issue in all - cautious interpretation (location) or grammatical filters (later)

# Balance of frequency and intensity is a cocneptual/methodological challenge



# Which countries are most hostile overall? ####

# Reload full country mentions, long format of all mentions (takes a bit)
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel.rds")) %>% 
  select(-c(starts_with("doc_"), date, location, starts_with("n_"), lang_sent,
            ends_with("_expl"), ends_with("_total"),
            ASIA, EUROPE, OCEANIA, AFRICA, AMERICA)) %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "iso2c", values_to = "mentioned") %>% 
  filter(mentioned != 0) %>% 
  select(-mentioned) %>% 
  mutate(country = countrycode(iso2c, "iso2c", "country.name"))
cm$country[cm$iso2c == "KV"] <- "Kosovo"

# Only English language sentences
ensent <- sent %>% filter(lang_sent == "en") %>% select(sentence_id) %>% pull()
cm <- cm %>% filter(sentence_id %in% ensent)
rm(ensent)

# Export for later
write_rds(cm, paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds"))
# cm <- read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds"))

# Merge scale values
cm <- cm %>% 
  left_join(glove, by = "sentence_id") %>% 
  left_join(lsx, by = "sentence_id")

# Filter out eu members
cm <- cm %>% 
  left_join(sent %>% select(sentence_id, year))

countries <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% 
  select(iso2c, year, eu_member)

cm <- cm %>% 
  left_join(countries, by = c("iso2c", "year")) %>% 
  filter(!eu_member)
  
  
# Aggregate scale values to country-level, log-weighted average
cm2 <- cm %>% 
  group_by(iso2c, country) %>% 
  summarise(mentions = n(),
            gti_glove = sum(gti_glove, na.rm = T)/log1p(n()), 
            friend_foe_glove = sum(friend_foe_glove, na.rm = T)/log1p(n()),
            coop_confl_glove = sum(coop_confl_glove, na.rm = T)/log1p(n()),
            gti_lsx = sum(gti_lsx, na.rm = T)/log1p(n()),
            friend_foe_lsx = sum(friend_foe_lsx, na.rm = T)/log1p(n()),
            coop_confl_lsx = sum(coop_confl_lsx, na.rm = T)/log1p(n())) %>% 
  ungroup()


# Plot top-25

comp <- rbind(
  cm2 %>% arrange(desc(gti_glove)) %>% head(25) %>% select(country, mentions, gti_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: GTI (Hostility)"),
  cm2 %>% arrange(desc(friend_foe_glove)) %>% head(25) %>% select(country, mentions, friend_foe_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Friend-Foe"),
  cm2 %>% arrange(desc(coop_confl_glove)) %>% head(25) %>% select(country, mentions, coop_confl_glove) %>% rename(value = 3) %>% mutate(scale = "GloVe: Coop-Conflict"),
  cm2 %>% arrange(desc(gti_lsx)) %>% head(25) %>% select(country, mentions, gti_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: GTI (Hostility)"),
  cm2 %>% arrange(desc(friend_foe_lsx)) %>% head(25) %>% select(country, mentions, friend_foe_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Friend-Foe"),
  cm2 %>% arrange(desc(coop_confl_lsx)) %>% head(25) %>% select(country, mentions, coop_confl_lsx) %>% rename(value = 3) %>% mutate(scale = "LSX: Coop-Conflict")
)


ggplot(comp, aes(y = tidytext::reorder_within(country, value, scale), x = value, alpha = mentions)) +
  geom_col(fill = "blue", width = .7) +
  facet_wrap(.~scale, scales = "free") +  # Free x and y scales
  tidytext::scale_y_reordered() +  # Fix reordered factor display
  scale_color_manual(values = c("white", "darkred"))+
  guides(color = "none")+
  scale_alpha_continuous(range = c(.3, 1))+
  labs(title = "Face-validity check\nMost \"hostile\" non-EU countries 1985-2024 by sum of scale values (weighted by log frequency)", 
       subtitle = "Expectation is that Russia pops out!",
       x = "Weighted scale values across all sentences mentioning the country", 
       y = "Top-25 countries\non each scale", 
       alpha = "Mentions") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 6),
        panel.background = element_rect(fill = "white", color = NA),  # White plot area
        plot.background = element_rect(fill = "white", color = NA),   
        legend.background = element_rect(fill = "white", color = NA))

ggsave("./output/scale_comp_plots/MostHostileForeignCountries_ByLogWeightedAverage_FullPeriod.png", width = 28, height = 16, units = "cm")

# Lessons:
# GTI/LSX, FF/LSX, and GTI/Glove are really out from that perspective - only small island states here and strange values for the LSX scales
# FF/Glove looks better in terms of the country list (few strange cases like Norway, though), but is strongly driven by frequency
# Both CC scales bring up plausible country lists (but lack internal consistency)

# Can we check internal consistency of CC/LSX?
