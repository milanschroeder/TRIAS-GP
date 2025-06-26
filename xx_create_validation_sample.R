# Validation Sampling ####

#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Draw sample of country references for human validation 
# Author:   @MSchro (25.06.2025)
#################################################################################


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP

# Packages ####
library(tidyverse)

# filter condition #### 
## country mentions >= 1
all_cm_sents <- 
  read_rds(paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds")) %>%    # all en sentence ids with CMs 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")),    # add metadata
            join_by(sentence_id)) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds")) %>% select(-doc_id), # add scaling for stratification
            join_by(sentence_id, iso2c)) %>% 
  mutate(year = year(date)) %>% 
  left_join(., read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) %>% select(eu_member, year, iso2c),
            join_by(iso2c, year))

# stratification variables:
## year(bracket), friend-foe scaling

# check for potential overrpresentations


#######################################
# create sample ####

set.seed(1905)

overlap = .1
n_coder = 3

samplesize_percoder = 500
samplesize = NA
samplesize = n_coder * samplesize_percoder
samplesize_overlap = samplesize * overlap
samplesize_unique = samplesize_percoder - samplesize_overlap

n_quantiles = 5 # for stratification (quintiles bc neutral category reasonable)
year_bracket_size = 5
year_bracket_n = round((max(all_cm_sents$year, na.rm = T) - min(all_cm_sents$year, na.rm = T)) / year_bracket_size)
  

r = 0 # percentage random sample part
randomsamplesize = r/100 * samplesize # -> no fully random sample this time

# construct sample groups ##########

grouped_data <- all_cm_sents %>% 
  filter(., 
         !eu_member &
         !is.na(friend_foe)) %>%  # no value for stratification variable
  mutate(
    strata = ntile(friend_foe, n = n_quantiles),
    year_bracket = cut(year, breaks = year_bracket_n)
      ) %>% 
         # group = fct_lump_lowfreq(str_c(friend_foe, year))) %>% # to balance p(sampling), keep n per group as close as possible (N_max < 20 * N_min)
  group_by(year_bracket, strata)

# how many groups?
n_sample_splits <- 
  grouped_data %>% 
  n_groups()

# documentation of drawing prob
(N_per_group <- 
    grouped_data %>% 
    summarise(N = n()) %>% 
    arrange(N))

# overlap sample ####

sample_overlap <- 
  grouped_data %>% 
  sample_n(size = round(samplesize_overlap / n_sample_splits)) %>%  # round up, not down (default) to make up for meaningless paras
  ungroup()



for (coder in 1:n_coder) {
  sample <- 
    grouped_data %>% 
    sample_n(size = round(samplesize_unique / n_sample_splits)) %>% 
    ungroup() %>% 
    
    bind_rows(., sample_overlap) %>% 
    slice_sample(prop = 1) %>% 
    
    select(id = sentence_id, text = text_sent, country, iso = iso2c) %>% 
    mutate(label = NA_character_)
  
  write_csv(sample, paste0(data_path, "validation/validation_sample_coder", coder, ".csv"))
}



