library(tidyverse)
library(quanteda)

lookup <- read_rds("data/country_dictionary.rds")
lookup <- dictionary(setNames(as.list(lookup$regex), lookup$iso2c), tolower = F)

# avoid dictionary() interpreting regex as pure text...
lookup <- dictionary(setNames(as.list(str_replace_all(lookup %>% unlist(), "\\\\\\\\", "\\\\")), names(lookup)), tolower = F)


corpus <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_paralevel.rds") %>% 
  filter(lang_para == "en")  

text <- corpus %>% 
  mutate(id = str_c(text_id, "-")) %>% 
  corpus(docid_field = "id", text_field = "text_para") %>% 
  quanteda::tokenize_sentence() %>% 
  unlist(use.names = T)

#dfm_result <- dfm_lookup(x = dfm(text), dictionary = lookup, valuetype = "regex")

cm <- 
  tibble(
  id = names(text),  
  map_dfc(lookup, ~ str_count(text, .x))
)


cm %<>%
  separate(id, into = c("text_id", "para_pos"), sep = "-", remove = T) %>% 
  mutate(
    text_sent = text,
    para_pos = as.numeric(para_pos),
    text_id = as.numeric(text_id),
    para_pos = if_else(is.na(para_pos), 1, para_pos)) %>% 
  left_join(., 
            corpus, 
            join_by(text_id)) %>% 
  select(text_id, para_pos, text_sent, all_of(names(corpus)), everything())  


cm %<>% mutate(EU_expl = str_count(text_sent, "(?-i)\bEU\b|\bEE?C\b|(?i)European(Economic )? Community|European Union"),
               BRICS_expl = str_count(text_sent, "(?-i)\bBRICS\b(?i)"),
               UN_expl = str_count(text_sent, "(?-i)\bUN\b|\bUNO\b|(?i)United Nations")
               ) %>% 
  relocate(EU_expl:UN_expl, .after = doc_id)

write_rds(cm, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_sentlevel.rds")

rm(a, text, corpus)
gc()
