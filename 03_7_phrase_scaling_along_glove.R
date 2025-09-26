# Entity Phrase based scaling

library(tidyverse)
library(magrittr)

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Learned semantic similarity weights ####
# See 03_1_semantic_similarity_weights_glove.R

simil_files <- list.files(paste0(data_path, "glove_models/"), pattern = "SemSimilWeights")
simils <- read_rds(paste0(data_path, "glove_models/", simil_files[1])) 
for (i in 2:length(simil_files)) {
  varname <- str_extract(simil_files[i], "(?<=-)(.*?)(?=\\.)") %>% tolower()
  print(varname)
  current <- read_rds(paste0(data_path, "glove_models/", simil_files[i]))
  if ("sim.target" %in% names(current)) {
    simils %<>% 
      left_join(current %>%  
                  select(token, sim.target) %>% 
                  rename(!!varname := sim.target), 
                by = "token")  
  } else {
    simils %<>% 
      left_join(current, 
                by = "token") 
  }
}

# Clean up
simils <- simils %>% 
  #  select(-c(coop, conf, friend, foe, friendly, hostile)) %>% # Drop ind poles of the scales
  rename(digi_adv = digitalityadvanced,
         digi_sim = digitalitysimple)
rm(current, varname, i, simil_files)
gc()





# countries phrases ####
phrases <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds")) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
              select(doc_id, date, sent_id, text_id),
            join_by(sent_id)) %>% 
  mutate(country_phrase = ifelse(is.na(country_phrase), 
                                 text_sent %>% stringr::str_replace_all("[:punct:]| ", ","), 
                                 country_phrase)) %>% 
  separate_rows(., country_phrase, sep = ",") %>%
  mutate(token = country_phrase %>% str_squish() %>% str_to_lower()) %>% 
  select(-c(mentions:country_phrase)) %>% 
  left_join(., simils, join_by(token)) %>% 
  filter(!is.na(friend_foe)) # instead of na.rm = T -> much faster for some reason!


# country scaling sentence level ####

entity_scaling_sent <-
  phrases %>%
  group_by(sent_id, iso2c, year) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup()

write_rds(entity_scaling_sent, paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds"))

# country scaling document level ####
entity_scaling_doc <-
  phrases %>% 
  select(-c(text_id), sent_id) %>% 
  group_by(doc_id, iso2c, year) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup()

write_rds(entity_scaling_doc, paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases_doclevel.rds"))



# country scaling paragraph level ####
entity_scaling_para <-
  phrases %>% 
  select(-c(doc_id, sent_id)) %>% 
  group_by(text_id, iso2c, year) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup()

write_rds(entity_scaling_para, paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases_paralevel.rds"))


# additive scaling ####

# country scaling sentence level ####

entity_scaling_sent <-
  phrases %>%
  group_by(sent_id, iso2c, year) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()

write_rds(entity_scaling_sent, paste0(data_path, "cleaned_data/additive_scaling_glove_EntityPhrases.rds"))

# country scaling document level ####
entity_scaling_doc <-
  phrases %>% 
  select(-c(text_id), sent_id) %>% 
  group_by(doc_id, iso2c, year) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()

write_rds(entity_scaling_doc, paste0(data_path, "cleaned_data/additive_scaling_glove_EntityPhrases_doclevel.rds"))



# country scaling paragraph level ####
entity_scaling_para <-
  phrases %>% 
  select(-c(doc_id, sent_id)) %>% 
  group_by(text_id, iso2c, year) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()

write_rds(entity_scaling_para, paste0(data_path, "cleaned_data/additive_scaling_glove_EntityPhrases_paralevel.rds"))



# # same for IOs: ####
{
  # phrases <- read_rds(paste0(data_path, "CountryMentions/IOs_phrase-words_vec.rds")) %>% 
#   left_join(., read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% select(doc_id, date, sent_id),
#             join_by(sent_id)) %>% 
#   mutate(country_phrase = ifelse(is.na(country_phrase), 
#                                  text_sent %>% stringr::str_replace_all("[:punct:]| ", ","), 
#                                  country_phrase)) %>% 
#   separate_rows(., country_phrase, sep = ",") %>%
#   mutate(token = country_phrase %>% str_squish() %>% str_to_lower()) %>% 
#   select(-c(mentions:country_phrase)) %>% 
#   left_join(., simils, join_by(token))
# 
# 
# entity_scaling <-
#   phrases %>%
#   group_by(sent_id, iso2c, year) %>%
#   summarise(across(where(is.numeric), mean, na.rm = T)) %>%
#   ungroup()
# 
# write_rds(entity_scaling, paste0(data_path, "cleaned_data/scaling_glove_EntityPhrasesIO.rds"))
# 
# entity_scaling <-
#   phrases %>%
#   group_by(doc_id, iso2c, year) %>%
#   summarise(across(where(is.numeric), mean, na.rm = T)) %>%
#   ungroup()
# 
# write_rds(entity_scaling, paste0(data_path, "cleaned_data/scaling_glove_EntityPhrasesIO_doclevel.rds"))
# 
}