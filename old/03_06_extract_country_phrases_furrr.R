#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Extract words related to specific countries mentioned in sentences
#           (for more targeted scaling downstream)
# Author:   @ChRauh (22.04.2025)
#################################################################################


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.4.0
library(glue) # Interpreted String Literals CRAN v1.6.2
library(spacyr)
library(rsyntax)
library(quanteda)

# Paralell processing
library(furrr)
plan(multisession)


# Custom phrase extraction tools 
# library(AspectPhraseEN)
source("EntityPhrase_dev_vec.R")


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



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
# here used to indetify the country/entity markers on the basis of which phrases are extracted

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
  mutate(value = str_replace_all(value, " ", "\\b|\\b")) %>% # Multiword dictionary entries  - create erros - TO DO!!!
  rename(iso2c = name,
         cregex = value)



# Enrich country mentions with country regex and full text of respective sentence ####
cm2 <- cm %>% 
  left_join(cterms, by = "iso2c") %>% 
  left_join(sent %>% select(sentence_id, text_sent), by = "sentence_id") %>% 
  mutate(text_sent = str_replace_all(text_sent, "\\s+", " ")) %>% # Multiple consecutive whtitespace to one (for correct dependency parsing)
  mutate(text_sent = str_remove(text_sent, "^IP/[0-9]{1,4}/[0-9]{1,4} ")) %>% # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS
  mutate(text_sent = str_remove(text_sent, "^Brussels, [0-9]{1,2} [A-Za-z]* [0-9]{1,4} ")) # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS



# Qualitative inspections ####

# i <- sample(1:nrow(cm2), 1)
# i
# cm$sentence_id[i]
# cm2$iso2c[i]
# cm2$cregex[i]
# cm2$text_sent[i]
# entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = T)
# entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = F)$phrase_words
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = T)
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = F)$phrase_words
# sent$text_sent[sent$sentence_id == cm$sentence_id[i]]

# Multiword matching creates errors (republic)
# Newsmap dictionaries sucks: "american"
# Doha and Uruguay rounds
# Lisbon treaty

# Clean up ####
rm(cp, cterms, sent)
gc()

# Benchmark phrase extraxtion ####
# spacy_initialize(model = "en_core_web_sm")
# 
# library(purrr)
# 
# test1 <- cm2 %>%
#   sample_n(size=100)
# 
# start <- Sys.time()
# phrases1 <- map2_chr(test1$text_sent, test1$cregex, ~ {
#   result <- entity_phrase(text = .x, entity = .y, plot = FALSE)
#   if (is.null(result) || nrow(result) == 0) return(NA_character_)
#   result$phrase_words[1]
# })
# duration1 <- Sys.time()-start # 45 seconds (~ 95 hours in total, unacceptable)
# 
# library(furrr)
# plan(multisession)
# 
# start <- Sys.time()
# phrases2 <- future_map2_chr(test1$text_sent, test1$cregex, ~ {
#   result <- entity_phrase(text = .x, entity = .y, plot = FALSE)
#   if (is.null(result) || nrow(result) == 0) return(NA_character_)
#   result$phrase_words[1]
# }, .progress = TRUE)
# duration2 <- Sys.time()-start # 15 secs (31 hours in total, puh ...)
# 
# 
# test1$phrases1 <- phrases1
# test1$phrases2 <- phrases2
# sum(test1$phrases1 != test1$phrases2, na.rm = T) # 0, good
# 
# i = 3
# test1$cregex[i]
# test1$phrases1[i]
# entity_phrase(test1$text_sent[i], entity = test1$cregex[i], plot = T)
# i = i+1



# Extract country phrases ####
# That runs a couple of hours ...

spacy_initialize(model = "en_core_web_sm")

start <- Sys.time()
phrases <- future_map2_chr(cm2$text_sent, cm2$cregex, ~ {
  result <- entity_phrase(text = .x, entity = .y, plot = FALSE)
  if (is.null(result) || nrow(result) == 0) return(NA_character_)
  result$phrase_words[1]
}, .progress = TRUE)
duration <- Sys.time()-start

cm2$country_phrase <- phrases

write_rds(cm2, paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds"))


# Sample checks
i <- sample(1:nrow(cm2), 1)
cm2$country_phrase[i]
entity_phrase(cm2$text_sent[i], entity = cm2$cregex[i], plot = T)


