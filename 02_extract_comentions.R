### Co-Mentions ####

library(tidyverse)
library(quanteda)
library(magrittr)

data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/"

##########
all_cms <-  
  
  # add Date:
  right_join(read_rds("../TRIAS-paper1/data/all_meta.rds") %>% 
               mutate(year = year(date),
                      ym = zoo::as.yearmon(date)) %>% 
               select(year, ym, date, doc_key),
             read_rds("../TRIAS-paper1/data/all_texts.rds") %>% 
               select(id, doc_key, doc_type),
             join_by(doc_key)
  )  %>%
  
  # add EU mentions:
  left_join(.,
            read_rds("../TRIAS-paper1/data/EU_mentions") %>% 
              rename(EU = cm_EU),
            join_by(id)
  ) %>% 
  
  # add CMs:
  left_join(.,
            read_rds("../data/allCMs_EC.rds"),
            join_by(id == doc_id)
  )

write_rds(all_cms, "../data/parawise_CMs.rds")

# calculate country group mentions: ####
# cm_para %<>% 
#   mutate(
#     cm_total = rowSums(cm_para %>% select(BI:WS), na.rm = T),
#     cm_external = cm_total - cm_EU,
#     cm_CN_wider = rowSums(cm_para %>% select(CN, TW, HK, MO), na.rm = T),
#     cm_BRICS = rowSums(cm_para %>% select(BR, RU, IN, CN, ZA), na.rm = T),
#     cm_BRICSplus = rowSums(cm_para %>% select(IR, AE, ET, EG), na.rm = T) + cm_BRICS,
#   )

# docwise co-occurrence: ####



# docwise_cm <- 
#   right_join(
#     
#     all_cms %>% distinct(year, ym, date, doc_key),
#     
#     all_cms %>% 
#       group_by(doc_key) %>%
#       select(-c(id, year)) %>% 
#       summarise(across(where(is.numeric), sum)) %>% 
#       ungroup(),
#     
#     join_by(doc_key))

#write_rds(docwise_cm, "../data/docwise_CMs.rds")

# construct dyads on doclevel ####

relevant_docs <- 
  read_rds(paste0(data_path, "allCMs_doclevel.rds")) %>% 
  select(-c(title_short, title_long, location, doc_type, n_chars_doc, lang_tag, n_paras, main_lang_doc)) %>% 
  pivot_longer(EU_expl:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  # no ctry groups:
  filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
                      "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
                      "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")
  ) %>%  
  mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  group_by(doc_key) %>% 
  mutate(comentions = sum(mentions != 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions != 0) # only keep if more than 1 country mentioned

# rearrange:
relevant_docs %<>% 
  inner_join(., relevant_docs %>% 
               select(doc_key, ctry, mentions), join_by(doc_key)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_docs, paste0(data_path, "comentions_doc.rds"))
rm(relevant_docs)

# create dyads on paralevel ####

relevant_paras <- 
  read_rds(paste0(data_path, "allCMs_paralevel.rds")) %>% 
  select(para_id, doc_id, date, doc_pos_total, EU_expl:last_col()) %>% 
  pivot_longer(EU_expl:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  
  # no ctry groups:
  filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
                      "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
                      "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")) %>%  
  mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  
  # only docs >= 2 CM: 
  group_by(para_id) %>% 
  mutate(comentions = sum(mentions > 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions > 0)

relevant_paras %<>% 
  inner_join(., relevant_paras %>% 
               select(para_id, ctry, mentions), join_by(para_id)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_paras, paste0(data_path, "comentions_para.rds"))
rm(relevant_paras)

# create dyads on sentence level ####

relevant_sentences <- 
  read_rds(paste0(data_path, "allCMs_sentlevel.rds")) %>% 
  select(sentence_id, doc_id, date, EU_expl:last_col()) %>% 
  pivot_longer(EU_expl:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  
  # no ctry groups:
  filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
                      "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
                      "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")) %>%  
  mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  
  # only docs >= 2 CM: 
  group_by(sentence_id) %>% 
  mutate(comentions = sum(mentions > 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions > 0)

relevant_sentences %<>% 
  inner_join(., relevant_sentences %>% 
               select(sentence_id, ctry, mentions), join_by(sentence_id)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_sentences, paste0(data_path, "comentions_sent.rds"))
rm(relevant_sentences)
