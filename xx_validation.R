#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Extract words related to specific countries mentioned in paragraphs (split by sentence)
#           (for more targeted scaling downstream)
# Author:   @mschro (11.07.2025)
#################################################################################


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.4.0
library(glue) # Interpreted String Literals CRAN v1.6.2
library(spacyr)
library(rsyntax)
library(quanteda)
library(magrittr)

# Custom phrase extraction tools 
# library(AspectPhraseEN)
source("EntityPhrase_dev_vec.R")


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP

adf <- 
  # Combine label sets from the three human coders
  rbind(
    read_csv("~/OneDrive - Hertie School/WZB/TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder1.csv") %>% 
      mutate(coder = 1),
    read_csv("~/OneDrive - Hertie School/WZB/TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder2.csv") %>% 
      mutate(coder = 2),
    read_csv("~/OneDrive - Hertie School/WZB/TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_coder3.csv") %>% 
      mutate(coder = 3),
    read_csv("~/OneDrive - Hertie School/WZB/TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_check_Christian.csv") %>% 
      mutate(coder = 4),
    read_csv("~/OneDrive - Hertie School/WZB/TRIAS-ValidationApps/CountryStatements-HumanCodedData/validation_sample_check_Milan.csv") %>% 
      mutate(coder = 5)) %>% 
  # Label 4-point human scales as ordered factor
  mutate(label = factor(label, levels = c("Very friendly","Rather friendly", "Neutral", "Rather adversarial", "Very adversarial")),
         iso = ifelse(is.na(iso), "NA", iso)) %>% 
         rename(text_codingtask = text) # Namibia read as NA from csv

adf %<>% 
  left_join(., read_rds(paste0(data_path, "CountryMentions/allCMs_sentlevel_phrase-words_vec.rds")) %>% 
              select(id = sentence_id, iso = iso2c, mentions, text_sent_phrase = text_sent, country_phrase, cregex),
            join_by(id, iso)) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")),    # add metadata
            join_by(id == sentence_id)) %>% 
  left_join(., read_rds(paste0(data_path, "cleaned_data/scaling_glove_EntityPhrases.rds")) %>% select(-doc_id), # add scaling for stratification
            join_by(id == sentence_id, iso == iso2c))
# Compare human assessment to semantic similarity from word embeddings ####

# Sentence level data ####

# Load and filter (should be equivalently done for all other data)
sent <- read_rds(paste0(data_path, "cleaned_data/data_paralevel.rds")) %>% 
  # Language filter
  filter(doc_key %in% unique(adf$doc_key)) 
  


# Country mentions - sentence level ####
cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_paralevel.rds")) %>% 
  filter(text_id %in% sent$text_id) %>% # Filter as above
  select(c(text_id, EU_expl, BI:ncol(.))) %>% # Only few meta and indvidual country indicators
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
cm %>% 
  filter(!iso2c %in% unique(cp$iso2c)) %>% count(iso2c)



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
  mutate(value = str_replace_all(value, " ", "|")) %>% # Multiword dictionary entries  - create erros - TO DO!!!
  rename(iso2c = name,
         cregex = value)



# Enrich country mentions with country regex and full text of respective sentence ####
cm2 <- cm %>% 
  left_join(cterms, by = "iso2c") %>% 
  left_join(sent %>% select(text_id, text_para, doc_key), by = "text_id") %>% 
  
  mutate(case = paste0(iso2c, "-", doc_key))

cm2 %<>% 
  filter(case %in% 
           c(adf %>% 
            mutate(case = paste0(iso, "-", doc_key)) %>% pull(case) %>% unique())
         )

#%>% 
  mutate(text_para = str_replace_all(text_para, "\\s+", " ")) %>% # Multiple consecutive whtitespace to one (for correct dependency parsing)
  mutate(text_para = str_remove(text_para, "^IP/[0-9]{1,4}/[0-9]{1,4} ")) %>% # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS
  mutate(text_para = str_remove(text_para, "^Brussels, [0-9]{1,2} [A-Za-z]* [0-9]{1,4} ")) # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS
  


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

# Clean up ####
rm(cp, cterms, sent)
gc()


# Extract country phrases ####
# That runs a couple of hours ...
# cm2 <- cm2 %>% 
#   mutate(country_phrase = entity_phrase(text = text_sent, entity = cregex, plot = F)$phrase_words)

spacy_initialize(model = "en_core_web_sm")

cm2$country_phrase = NA

for (i in 1:nrow(cm2)) {
  # Show progress
  print(i)
  # Check if phrase is already extracted in prior execution and skip if so
  if(!is.na(cm2$country_phrase[i])){next}
  # extract phrase words ...
  current <- entity_phrase(text = cm2$text_para[i], entity = cm2$cregex[i], plot = F)$phrase_words
  # ... and add to data if a phrase could be extracted
  if(length(current) == 0){next}
  cm2$country_phrase[i] <- current
}

cm <- cm2 %>% 
  select(-c(cregex, case)) %>% 
  right_join(., adf %>% select(-mentions), 
            join_by(iso2c == iso, country_phrase, doc_key)) %>% 
  distinct(., .keep_all = T)

# do phrases & scaling again, test different appproaces #####
adf2 <- adf %>% distinct(id, .keep_all = T) 

spacy_initialize(model = "en_core_web_sm")

adf2$cp = NA

for (i in 1:nrow(adf2)) {
  # Show progress
  print(i)
  # Check if phrase is already extracted in prior execution and skip if so
  if(!is.na(adf2$cp[i])){next}
  # extract phrase words ...
  current <- entity_phrase(text = adf2$text_sent_phrase[i], entity = adf2$cregex[i], plot = F)$phrase_words
  # ... and add to data if a phrase could be extracted
  if(length(current) == 0){next}
  adf2$cp[i] <- current
}


write_rds(cm2, paste0(data_path, "CountryMentions/CMs_paralevel_phrase-words.rds"))


