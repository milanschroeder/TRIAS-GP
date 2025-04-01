#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Scaling along aspect phrases around country mentions
# Author:   @ChRauh (01.04.2025)
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

library(AspectPhraseEN)


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
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
# Newsmap dictionary should be somewhat broader, but isn't!? Cf. doc level salience analyses
length(unique(cm$iso2c)) # 233
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
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
           map_chr(., ~ paste0(.x, collapse = ")|(")) %>% 
           str_remove_all(fixed("*"))) %>% 
  mutate(value = paste0("(", value, ")")) %>% 
  mutate(value = str_replace_all(value, " ", "|")) %>% # Does this make sense??? multiword dictionary entries
  rename(iso2c = name,
         cregex = value)



# Enrich country mentions with country regex and full text of respective sentence ####
cm <- cm %>% 
  left_join(cterms, by = "iso2c") %>% 
  left_join(sent %>% select(sentence_id, text_sent), by = "sentence_id")





# Tests
i <- sample(1:nrow(cm), 1)
i
cm$iso2c[i]
cm$cregex[i]
cm$text_sent[i]
aspect_phrase(text = cm$text_sent[i], aspect = cm$cregex[i], plot = T)           
aspect_phrase(text = cm$text_sent[i], aspect = cm$cregex[i], plot = F)$phrase_words

# Word boundaries must be respected, at least in the beginning of the markers \\bamerican
# replace * with [a-z]{0,}

# Multiword matching creates errors (republic)
# Clean out duplicated whitespaces - sets off the dependency parser!


# Brussels ....
# Newsmap dictionaries such: "american"

# AP observations:
# needs an exclusion for passive "by" constructions to include the root at level 2
# amod <- nsubj <- ROOT would be also useful at level 2
# general - how much harm does including the Root at level 2 do?

# Downward filling of aspect must be stooped at conjuncts
# Upward conjunctions are also often too much (frequent: lists of countries)

# Filter aspect terms along lemmas/tokens, not on aspect label (otherwise downwards fill gets lost!)


