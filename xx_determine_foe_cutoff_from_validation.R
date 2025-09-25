#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Determine ideal friend-foe cutoff from validation sample  
# Author:   @MSchro (06.08.2025)
#########################################################################

library(tidyverse)
library(magrittr)

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


old_all_cm_sents <- 
  read_rds(paste0(data_path, "CountryMentions/OLD/OLD_CMs_sentlevel_EN_long.rds")) %>%    # all en sentence ids with CMs 
  left_join(., read_rds(paste0(data_path, "cleaned_data/OLD/OLD_data_sentlevel.rds")),    # add metadata
            join_by(sentence_id)) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/OLD/OLD_scaling_glove_EntityPhrases.rds")) %>% select(-doc_id), # add scaling for stratification
            join_by(sentence_id, iso2c)) %>% <- <- <- <- <- <- 
  mutate(year = year(date)) %>% 
  left_join(., read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% select(eu_member, year, iso2c),
            join_by(iso2c, year)) 

all_cm_sents <- 
  read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>%    # all en sentence ids with CMs 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")),    # add metadata
            join_by(sent_id)) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/additive_scaling_glove_EntityPhrases.rds")) %>% select(-doc_id, -year, -text_id), # add scaling for stratification
            join_by(sent_id, iso2c)) %>% 
  mutate(year = year(date)) %>% 
  left_join(., read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% select(eu_member, year, iso2c),
            join_by(iso2c, year)) %>% 
  left_join(., read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds")) %>% select(-c(year, mentions, text_sent)), join_by(sent_id, iso2c)) %>% 
  mutate(text = str_squish(text_sent))

dta <- rbind(
  read_csv("../TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder1.csv") %>% 
    mutate(coder = 1),
  read_csv("../TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder2.csv") %>% 
    mutate(coder = 2),
  read_csv("../TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder3.csv") %>% 
    mutate(coder = 3),
  read_csv("../TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_check_Christian.csv") %>% 
    mutate(coder = 4),
  read_csv("../TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_check_Milan.csv") %>% 
    mutate(coder = 5)) %>% 
  
  # Label 4-point human scales as ordered factor
  mutate(label = factor(label, levels = c("Very friendly","Rather friendly", "Neutral", "Rather adversarial", "Very adversarial")),
         iso = ifelse(is.na(iso), "NA", iso))  # Namibia read as NA from csv

dta %<>% left_join(., old_all_cm_sents %>% select(-any_of(names(dta))), join_by(id == sentence_id, iso == iso2c)) %>% 
  mutate(text = str_squish(text)) %>% 
  select(id:date)


# join new phrases & scalings, where applicable:
dta %<>% 
  left_join(
    all_cm_sents %>% select(cregex, country_phrase, friend_foe, friend_foe, text, doc_key, iso = iso2c),
    join_by(doc_key, iso, text)
  ) %>% 
  filter(!is.na(country_phrase)) # filter out mostly cities and other non-CM cases



# adfl <- dta %>% 
#   mutate(ind_code = paste0(id, "-", coder)) %>% # The individual coding step
#   mutate(coder = paste("Coder", coder),
#          label = as.numeric(label)) %>% # Human codes to numeric
#   select(-c(coop:foe, friendly:security)) %>% 
#   pivot_longer(
#     cols = c(friend_foe),  # All columns except 'ind_code'
#   ) 

# adfl_phrases <- adfl %>% filter(name == "friend_foe" & !is.na(country_phrase))
# adfl %<>% filter(name == "friend_foe")

data <- dta 


dta <- data %>% 
  mutate(
    target = as.numeric(label) > 3, # adversarial
    value = friend_foe
  )

# identify cutoff ####

cutoff_candidates <- tibble()

for (cutoff in seq(min(dta$value, na.rm = T), max(dta$value, na.rm = T), .001)) {
  
  dta %<>%
    # filter(label != 3) %>% 
    mutate(pred = value < cutoff) %>%  # change variable here
    filter(!is.na(pred))
  
  TP <- sum(dta$pred & dta$target)
  TN <- sum(!dta$pred & !dta$target)
  FP <- sum(dta$pred & !dta$target)
  FN <- sum(!dta$pred & dta$target)
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  cutoff_candidates %<>% bind_rows(., tibble_row(cutoff, precision, recall, TP, FP, TN, FN) %>% 
                                     mutate(F1 = 2 * ( (precision * recall) / (precision + recall) ) 
                                     ))
}

cutoff_candidates %>% 
  pivot_longer(c(TP:FN), names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = cutoff, y = value, colour = measure, alpha = TP)) + 
  geom_line() +
  geom_point()

# find ideal cutoff point:
cutoff <- max(cutoff_candidates$cutoff[cutoff_candidates$F1 == max(cutoff_candidates$F1, na.rm = T)], na.rm = T) # 0.3095 -> not sensible in additive case, also were not after ideal F1 but want catch most TPs
cutoff <- cutoff_candidates %>% filter(2*TP > FP) %>% pull(cutoff) %>% max() # -> better interpratability: 1:2 odds
dta %<>% mutate(pred = value < cutoff)
