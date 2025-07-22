#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Extract words related to specific countries mentioned in sentences
#           (for more targeted scaling downstream)
# Author:   @ChRauh (21.07.2025)
#################################################################################


# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode) # Convert Country Names and Country Codes CRAN v1.4.0
library(glue) # Interpreted String Literals CRAN v1.6.2
library(spacyr)
library(rsyntax)
library(quanteda)

# Parallel processing
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
  filter(sent_id %in% sent$sent_id) %>% # Filter as above
  # mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  left_join(sent %>% select(sent_id, year), by = "sent_id") %>% 
  relocate(sent_id, year) %>% 
  select(c(sent_id, year, AD:ncol(.))) %>% # Only few meta and individual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") %>% # Easier to handle
  filter(mentions > 0) # Sentences w/out country mentions not needed here
gc()


# External country/year panel ####
cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data sets (= official states at some point)
length(unique(cp$iso2c)) # 241
# Used country dictionary should be somewhat broader
length(unique(cm$iso2c)) # 243
# I reduce the country mentions to the cp sample to ensure consistency throughout
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))




# The country mention dictionaries ####
# here used to indetify the country/entity markers on the basis of which phrases are extracted

cterms <- read_rds("data/country_dictionary.rds") %>% # CHECK: do i need to apply word boundaries as in ol version above?
  rename(cregex = regex)

cterms[cterms$iso2c == "CN", ]
cterms[cterms$iso2c == "DE", ]

test <- cterms %>% filter(str_detect(cregex, fixed("\\\\b")))


# The PERL regex used for identifying country mentions do not work with the entity_phrase function
# as rsyntax in the background only accepts base R regex (Kasper is at it, though ;))

# So we need to clean out PERL patterns such as case sensitivity and lookahead or lookbehinds
# Not a problem for the entity match, as false positives have been ruled out in construction of the country mentions already

cterms$cregex2 <- 
  cterms$cregex %>% 
  str_remove_all(fixed("(?i)")) %>% # case flags
  str_remove_all("\\(\\?.*?\\)") %>% #  remove lookaheads, lookbehinds, flags like (?i), and non-capturing groups (?:...) - can be of in case of nesting ...
  str_replace_all(fixed("\\\\b"), "\\b") %>% # double escaped literals
  str_trim(side = "both") %>% 
  str_replace_all(fixed(" "), "|") %>% # Multiword expression such as "united states", entity match happens on token level (parser output) only 
  str_replace_all(fixed("|||"), "| |") %>% 
  tolower() %>% # all lower case, matching the parser output
  str_trim(side = "both")

# Question: Are wild cards needed?
# entity_phrase(text = "Albanian forces attacked the city of Heron today.", entity = "albania", plot = T) # No! lemma_R in rsyntax seems to be quivalent to str_detect()
# entity_phrase(text = "We love the Canadian immigrants.", entity = "canad", plot = T) # No! lemma_R in rsyntax seems to be quivalent to str_detect()

  
# Some formerly complicated cases by hand
cterms$cregex2[cterms$iso2c == "AG"] <- "antigua|barbuda"
cterms$cregex2[cterms$iso2c == "BL"] <- "\\bsaint\\b|\\bst\\b|barth[eé]lem"
cterms$cregex2[cterms$iso2c == "CD"] <- "democratic|republic|congo|\\bdr\\b|\\bdrc\\b"
cterms$cregex2[cterms$iso2c == "CG"] <- "congo"
cterms$cregex2[cterms$iso2c == "CI"] <- "ivory|coast|c[ô|o]te|ivoire|ivorian"
cterms$cregex2[cterms$iso2c == "CZ"] <- "czech|\\brepublic\\b"
cterms$cregex2[cterms$iso2c == "DK"] <- "denmark|danish|\\bdanes\\b"
cterms$cregex2[cterms$iso2c == "GB"] <- "\\buk\\b|united|kingdom|great|brit|england"
cterms$cregex2[cterms$iso2c == "GW"] <- "guinea|bissau"
cterms$cregex2[cterms$iso2c == "HK"] <- "hong|kong"
cterms$cregex2[cterms$iso2c == "KN"] <- "\\bsaint\\b|\\bst\\b|kitts|nevis|kittitian"
cterms$cregex2[cterms$iso2c == "LC"] <- "\\bsaint\\b|\\bst\\b|lucia"
cterms$cregex2[cterms$iso2c == "MF"] <- "\\bsaint\\b|\\bst\\b|martin"
cterms$cregex2[cterms$iso2c == "PH"] <- "philip|filip"
cterms$cregex2[cterms$iso2c == "PL"] <- "poland|polish|poles"
cterms$cregex2[cterms$iso2c == "PM"] <- "\\bsaint\\b|\\bst\\b|pierr|miquelon"
cterms$cregex2[cterms$iso2c == "PR"] <- "\\bpuert|\\bric\\b"
cterms$cregex2[cterms$iso2c == "RE"] <- "r[ée]union|reunionese|reunionnais"
cterms$cregex2[cterms$iso2c == "RU"] <- "russia|so[wv][ji]et|\\bussr\\b"
cterms$cregex2[cterms$iso2c == "SH"] <- "\\bsaint\\b|\\bst\\b|helen"
cterms$cregex2[cterms$iso2c == "SX"] <- "\\bsaint\\b|\\bst\\b|maarten"
cterms$cregex2[cterms$iso2c == "TC"] <- "turks|cai[cr]os|islands"
cterms$cregex2[cterms$iso2c == "TR"] <- "turkey|t[üu]rkiye|turkish|turks\\b"
cterms$cregex2[cterms$iso2c == "TT"] <- "trinidad|tobago"
cterms$cregex2[cterms$iso2c == "US"] <- "\\bunited\\b|\\bstates\\b|us"
cterms$cregex2[cterms$iso2c == "VC"] <- "\\bsaint\\b|\\bst\\b|vincent|grenadines"
cterms$cregex2[cterms$iso2c == "VN"] <- "viet|\\bnam\\b"
cterms$cregex2[cterms$iso2c == "IO"] <- "british|indian|ocean"
cterms$cregex2[cterms$iso2c == "TF"] <- "french|southern|antarc"
cterms$cregex2[cterms$iso2c == "DD"] <- "german|democratic|republic"
cterms$cregex2[cterms$iso2c == "HM"] <- "heard|island|macdonald"
cterms$cregex2[cterms$iso2c == "AN"] <- "\\bnether|dutch|antil"
cterms$cregex2[cterms$iso2c == "GS"] <- "south|georgia|sandwich|island"
cterms$cregex2[cterms$iso2c == "UM"] <- "minor|outlying"
cterms$cregex2[cterms$iso2c == "YD"] <- "yemen"

# Errors in source dict
cterms$cregex2[cterms$iso2c == "OM"] <- "\\\\boman\\\\b|\\bomani" # omani w/out boundaries matches Romania
# cterms$cregex2[cterms$iso2c == "DE"] <- "\\bgerman|\\bgdr\\b" # Double escape removed
# cterms$cregex2[cterms$iso2c == "DE"] <- "\\bchina\\b|chinese" # Double escape removed




# Reduce columns 
cterms <- cterms %>% 
  select(iso2c, cregex2) %>% 
  rename(cregex = cregex2)

# Enrich country mentions with country regex and full text of respective sentence ####
cm2 <- cm %>% 
  left_join(cterms, by = "iso2c") %>% 
  left_join(sent %>% select(sent_id, text_sent), by = "sent_id") %>% 
  mutate(text_sent = str_replace_all(text_sent, "\\s+", " ")) %>% # Multiple consecutive whtitespace to one (for correct dependency parsing)
  mutate(text_sent = str_remove(text_sent, "^IP/[0-9]{1,4}/[0-9]{1,4} ")) %>% # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS
  mutate(text_sent = str_remove(text_sent, "^Brussels, [0-9]{1,2} [A-Za-z]* [0-9]{1,4} ")) # Clean headers - SHOULD BE DONE BEFORE EXTRACTING COUNTRY MENTIONS



# Qualitative inspections ####

i <- sample(1:nrow(cm2), 1)
i
cm$sent_id[i]
cm2$iso2c[i]
cm2$cregex[i]
cm2$text_sent[i]
entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = T)
entity_phrase(text = cm2$text_sent[i], entity = cm2$cregex[i], plot = F)$phrase_words
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = T)
# aspect_phrase(text = cm2$text_sent[i], aspect = cm2$cregex[i], plot = F)$phrase_words
sent$text_sent[sent$sent_id == cm$sent_id[i]]

# Multiword matching creates errors (republic)
# united kingdom not matched
# Newsmap dictionaries sucks: "american"


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
# That runs ~ 22 hours (TP) ...

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

# How many missing phrases?
sum(is.na(cm2$country_phrase)) # 230382
sum(is.na(cm2$country_phrase))/nrow(cm2) # .31

# Variation across countries?
mp <- cm2 %>% 
  group_by(iso2c) %>% 
  summarise(count = n(),
            nas = sum(is.na(country_phrase))) %>% 
  ungroup() %>% 
  mutate(na_share = nas/count) %>% 
  arrange(desc(na_share))

# Oman is an issue - corrected upstream but not yet applied here
# Liechtenstein, Hong Kong, woth a check - but regex appears ok

hist(mp$na_share) # reasonable- but everyting beyong .6 is abnormal
mean(mp$na_share) # . 33 - close to grand mean, good 




