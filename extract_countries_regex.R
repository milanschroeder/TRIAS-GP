library(tidyverse)
library(quanteda)

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


lookup <- read_rds("data/country_dictionary.rds")
lookup <- dictionary(setNames(as.list(lookup$regex), lookup$iso2c), tolower = F)

# avoid dictionary() interpreting regex as pure text...
lookup <- dictionary(setNames(as.list(str_replace_all(lookup %>% unlist(), "\\\\\\\\", "\\\\")), names(lookup)), tolower = F)


corpus <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_sentlevel.rds") 

corpus %<>% 
#  filter(lang_sent == "en") %>% 
  # mutate(id = str_c(text_id, "-")) %>% 
  corpus(docid_field = "sent_id", text_field = "text_sent") %>% 
  quanteda::tokenize_sentence()
  # unlist(use.names = T)

#dfm_result <- dfm_lookup(x = dfm(text), dictionary = lookup, valuetype = "regex")
start <- Sys.time()

cm <- 
  tibble(
  id = names(corpus),  
  map_dfc(lookup, ~ str_count(corpus, .x))
)

(duration <- Sys.time()-start)

# save pure sentlevel CMs ####
cm %<>% rename(sent_id = id) %>% 
  mutate(sent_id = as.numeric(sent_id))

write_rds(cm, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_sentlevel.rds")

cm <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_sentlevel.rds")

cm %<>% left_join(., read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_sentlevel.rds", join_by(sent_id))) %>%
                    select(sent_id, text_sent, date, doc_type, text_id, para_pos, doc_key:has_link_sent, AD:YU)

cm %<>% mutate(EU_expl = str_count(text_sent, "(?-i)\\bEU\\b|\\bEE?C\\b|(?i)European(Economic )? Community|European Union"),
               BRICS_expl = str_count(text_sent, "(?-i)\\bBRICS\\b(?i)"),
               UN_expl = str_count(text_sent, "(?-i)\\bUN\\b|\\bUNO\\b|(?i)United Nations")
               ) %>% 
  relocate(EU_expl:UN_expl, .after = has_link_sent)

cm %<>% filter(lang_sent == "en") # implicitly excluding NA filters most trash! 
 
  


write_rds(cm, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/EN_CMs_sentlevel.rds")

rm(a, text, corpus)
gc()

# long format ####
cm_long <- read_rds(paste0(data_path, "CountryMentions/EN_CMs_sentlevel.rds")) %>%  # just read filtered version directly
  mutate(year = year(date)) %>% 
  select(sent_id, year, AD:last_col()) 

cm_long %<>% 
    mutate(any = rowSums(select(., AD:YU))) %>% 
  filter(any > 0) %>% 
  select(-any)

cm_long %<>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:last_col(), names_to = "iso2c", values_to = "mentions") %>% # Easier to handle
  filter(mentions > 0) # Sentences w/out country mentions not needed here

write_rds(cm_long, paste0(data_path, "CountryMentions/CMs_sentlevel_EN_long.rds"))

gc()


# aggregate doclevel ####

# get para/text & doc_id
cm <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/allCMs_sentlevel.rds") %>% 
  left_join(., read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_sentlevel.rds") %>% 
              select(sent_id, text_id, doc_key), 
            join_by(sent_id))

cm_doc <- cm %>% group_by(doc_key) %>% 
  summarise(across(AD:YU, sum)) %>% 
  ungroup()

docs <- read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")  # nothing changed here, just doclevel metadata
) %>% left_join(., cm_doc, join_by(doc_key))

allCMs_doclevel <- read_rds(paste0(data_path, "CountryMentions/old/OLD_allCMs_doclevel.rds")) # check & mimic structure

names(allCMs_doclevel)[!names(allCMs_doclevel) %in% names(docs)]
write_rds(docs, paste0(data_path, "CountryMentions/allCMs_doclevel.rds"))  

# aggregate paralevel ####

rm(allCMs_doclevel, cm_doc, docs)
gc()

cm_para <- cm %>% group_by(text_id) %>% 
  summarise(across(AD:YU, sum)) %>% 
  ungroup()


paras <- read_rds(paste0(data_path, "cleaned_data/data_paralevel.rds")  # nothing changed here, just doclevel metadata
) %>% left_join(., cm_para, join_by(text_id))

allCMs_paralevel <- read_rds(paste0(data_path, "CountryMentions/old/OLD_allCMs_paralevel.rds"))

names(allCMs_paralevel)[!names(allCMs_paralevel) %in% names(paras)]
write_rds(paras, paste0(data_path, "CountryMentions/allCMs_paralevel.rds"))  


