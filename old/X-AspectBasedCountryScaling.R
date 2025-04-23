#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Scaling along aspect phrases around country mentions
# Author:   @ChRauh (22.04.2025)
#################################################################################


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.4.0
library(broom) # Convert Statistical Objects into Tidy Tibbles CRAN v1.0.4
library(glue) # Interpreted String Literals CRAN v1.6.2
library(spacyr)
library(rsyntax)
library(quanteda)


# Custom phrase extraction tools 
library(AspectPhraseEN)
source("EntityPhrase_dev.R")


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# Sentence level data ####

# Load and filter (should be equivalently done for all other data)
sent <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  # Language filter
  filter(lang_sent == "en") %>%
  # Doc type filter
  filter(doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement")) %>% 
  # Year var
  mutate(year = str_extract(date, "^[0-9]{4}"))



# Country mentions - sentence level ####
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel.rds")) %>% 
  filter(sentence_id %in% sent$sentence_id) %>% # Filter as above
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  select(c(sentence_id, year, BI:ncol(.))) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") %>% # Easier to handle
  filter(mentions > 0) # Sentences w/out country mentions not needed here
gc()


# External country/year panel ####
cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data sets (= official states at some point)
length(unique(cp$iso2c)) # 241
# Newsmap dictionary should be somewhat broader - but isn't, as it contains only the countries the Comm mentioned at least once
length(unique(cm$iso2c)) # 233
# I reduce the country mentions to the cp sample to ensure consistency throughout
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))



# The country mention dictionaries ####


cterms <- newsmap::data_dictionary_newsmap_en %>% 
  flatten() %>% # First level continent
  flatten() %>% # Second level orientation in continent
  enframe() %>% # Tibble
  as.data.frame() %>% 
  # Flatten the listed dictionary entries and turn them into a base R regex
  mutate(value = value %>% 
           flatten() %>% 
           map_chr(., ~ paste0(.x, collapse = "\\b)|(\\b")) %>% 
           str_replace_all(fixed("*"), "[a-z]{0,}")) %>% 
  mutate(value = paste0("(\\b", value, "\\b)")) %>% 
  mutate(value = str_replace_all(value, " ", "|")) %>% # Multiword dictionary entries  - create erros - TO DO!!!
  rename(iso2c = name,
         cregex = value)



# Enrich country mentions with country regex and full text of respective sentence ####
cm2 <- cm %>% 
  left_join(cterms, by = "iso2c") %>% 
  left_join(sent %>% select(sentence_id, text_sent), by = "sentence_id") %>% 
  mutate(text_sent = str_replace_all(text_sent, "\\s+", " ")) %>% # Multiple consecutive whtitespace to one (for correct dependency parsing)
  mutate(text_sent = str_remove(text_sent, "^IP/[0-9]{1,4}/[0-9]{1,4} ")) %>% # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS
  mutate(text_sent = str_remove(text_sent, "^Brussels, [0-9]{1,2} [A-Za-z]* [0-9]{1,4} ")) # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS





# Qualitative inspection ####
i <- sample(1:nrow(cm2), 1)
i
cm$sentence_id[i]
cm2$iso2c[i]
cm2$cregex[i]
cm2$text_sent[i]
entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = T)           
entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = F)$phrase_words
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = T)           
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = F)$phrase_words
# sent$text_sent[sent$sentence_id == cm$sentence_id[i]]

# Multiword matching creates errors (republic)
# Newsmap dictionaries sucks: "american"
# Doha and Uruguay rounbds



