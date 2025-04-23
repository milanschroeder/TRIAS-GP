# country mentions: ####
library(quanteda)
library(newsmap)
library(tidyverse)

# remove "us" from US dictionary, to avoid false positives:

extract_CMs <- function(df, id_var = df$doc_id # required vars: doc_id, text
                        ){
  
# fix newsmap dictionary:
  tmp <- list(newsmap::data_dictionary_newsmap_en) %>% flatten()
  tmp$AMERICA$NORTH$US[[1]] <- tmp$AMERICA$NORTH$US[[1]][c(1, 3:5)]
  data_dictionary_newsmap_en <- quanteda::dictionary(tmp)
  
# tokenize:
tok <- df %>% 
  mutate(doc_id = id_var) %>% 
  corpus() %>%  
  tokens(., remove_punct = T) %>% 
  # replace case sensitive "US" tokens with "USA" after removing punctuation ("U.S.") to avoid mixing up with "us"
  tokens_replace(pattern = "US", replacement = "USA", case_insensitive = F) %>%
  tokens_remove(pattern = c(stopwords("en")), 
                valuetype = "fixed", 
                padding = TRUE) 

## extract:

# countries:
toks_ctry <- tokens_lookup(tok, dictionary = data_dictionary_newsmap_en, 
                           levels = 3) # Countries on level 3 of the dict (1 is continents, 2 is region)

# toks_region <- tokens_lookup(tok, dictionary = data_dictionary_newsmap_en, 
#                              levels = 2) # Countries on level 3 of the dict (1 is continents, 2 is region)

toks_cont <- tokens_lookup(tok, dictionary = data_dictionary_newsmap_en, 
                           levels = 1) # Continents on level 1 of the dict


dict_conts <- set_names(names(data_dictionary_newsmap_en), names(data_dictionary_newsmap_en))

# dictionary for explicit continet names and major ROs:
toks_cont_explicit <- tokens_lookup(tok, 
                                    dictionary = dictionary(list(
                                      AFRICA = c("africa", "au", "ecowas", "sadc", "amu"),
                                      AMERICA = c("america", "mercosur", "nafta"),
                                      ASIA = c("asia", "asean"),
                                      EUROPE = c("europe", "eu", "coe", "eeas"),
                                      OCEANIA = c("oceania", "pif")
                                    )))

cont_hits_expl <- dfm(toks_cont_explicit, tolower = FALSE) %>% 
  convert(to = "data.frame") %>% 
  rename_with(~ paste0(.x, "_expl"), -doc_id)

# reg_hits <- dfm(toks_region, tolower = FALSE) %>% 
#   convert(to = "data.frame")

ctry_hits <- dfm(toks_ctry, tolower = FALSE) %>% 
  convert(to = "data.frame") 

cont_hits <- dfm(toks_cont, tolower = FALSE) %>% 
  convert(to = "data.frame") 

# merge:
cont_hits_total <- bind_cols(cont_hits %>% select(doc_id), 
                             cont_hits %>% select(-doc_id) + cont_hits_expl %>% select(-doc_id) 
) %>% 
  rename_with(~ paste0(.x, "_total"), -doc_id)

all_cms <- bind_cols(
  df %>% 
    mutate(EU_expl = str_count(text, "(?i)\bEU\b|European Union"),
           BRICS_expl = str_count(text, "(?i)\bBRICS\b"),
           UN_expl = str_count(text, "(?i)\bUN\b|United Nations|\bUNO\b"),
           US = str_count(text, "\\b(USA?|U\\.S\\.A?)[\\- \\.]|(?i:united states|washington|new york|(?<!(latino?|central|north|south|ibero|hispano)[\\- ])america)") 
           # based on:
            #newsmap::data_dictionary_newsmap_en$AMERICA$NORTH$US -> [1] "united states" "us" "american*" "washington" "new york"  
           ) %>% 
    select(-text),
  cont_hits %>% select(-doc_id),
  cont_hits_expl %>% select(-doc_id),
  cont_hits_total %>% select(-doc_id),
  ctry_hits %>% select(-doc_id, -US)
) %>% relocate(US, .after = PM)

return(all_cms)
}

# documnet level:
all_cms_doc <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_doclevel.rds") %>% rename(text = text_doc) %>% 
  extract_CMs()
write_rds(all_cms_doc, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_doclevel.rds")
rm(all_cms_doc)

# paragraph level:
all_cms_para <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_paralevel.rds") %>% rename(text = text_para) %>%
  extract_CMs(id_var = .$para_id)
write_rds(all_cms_para, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_paralevel.rds")
rm(all_cms_para)

# sentence level:
all_cms_sent <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_sentlevel.rds") %>% rename(text = text_sent) %>% 
  extract_CMs(id_var = .$sentence_id) 
write_rds(all_cms_sent, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_sentlevel.rds")
rm(all_cms_sent)

