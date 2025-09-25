### Co-Mentions ####

library(tidyverse)
library(quanteda)
library(magrittr)


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP
##########
all_cms <-  
  
  read_rds(paste0(data_path, "allCMs_paralevel.rds"))
#   
#   # add Date:
#   right_join(read_rds("../TRIAS-paper1/data/all_meta.rds") %>% 
#                mutate(year = year(date),
#                       ym = zoo::as.yearmon(date)) %>% 
#                select(year, ym, date, doc_key),
#              read_rds("../TRIAS-paper1/data/all_texts.rds") %>% 
#                select(id, doc_key, doc_type),
#              join_by(doc_key)
#   )  %>%
#   
#   # add EU mentions:
#   left_join(.,
#             read_rds("../TRIAS-paper1/data/EU_mentions") %>% 
#               rename(EU = cm_EU),
#             join_by(id)
#   ) %>% 
#   
#   # add CMs:
#   left_join(.,
#             read_rds("../data/allCMs_EC.rds"),
#             join_by(id == doc_id)
#   )
# 
# write_rds(all_cms, "../data/parawise_CMs.rds")

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
  select(doc_id, AD:last_col()) %>% 
  pivot_longer(AD:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  # no ctry groups:
  # filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
  #                     "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
  #                     "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")
  # ) %>%  
  # mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  group_by(doc_id) %>% 
  mutate(comentions = sum(mentions != 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions != 0) # only keep if more than 1 country mentioned

# rearrange:
relevant_docs %<>% 
  inner_join(., relevant_docs %>% 
               select(doc_id, ctry, mentions), join_by(doc_id)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_docs, paste0(data_path, "comentions/comentions_doc.rds"))
rm(relevant_docs)

# create dyads on paralevel ####

relevant_paras <- 
  read_rds(paste0(data_path, "allCMs_paralevel.rds")) %>% 
  left_join(., read_rds(paste0(data_path, "../cleaned_data/data_doclevel.rds")) %>% select(doc_id), join_by(doc_id)) %>% 
  select(text_id, doc_id, doc_pos, AD:last_col()) %>% 
  pivot_longer(AD:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  # no ctry groups:
  # filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
  #                     "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
  #                     "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")) %>%  
  # mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  
  # only docs >= 2 CM: 
  group_by(text_id) %>% 
  mutate(comentions = sum(mentions > 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions > 0)

relevant_paras %<>% 
  inner_join(., relevant_paras %>% 
               select(text_id, ctry, mentions), join_by(text_id)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_paras, paste0(data_path, "comentions/comentions_para.rds"))
rm(relevant_paras)

# create dyads on sentence level ####

relevant_sentences <- 
  read_rds(paste0(data_path, "allCMs_sentlevel.rds")) %>% 
  select(sent_id, AD:last_col()) %>% 
  pivot_longer(AD:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
  
  # no ctry groups:
  # filter(!ctry %in% c("EU", "AFRICA", "AMERICA", "ASIA", "EUROPE", "OCEANIA", 
  #                     "AFRICA_total", "AMERICA_total", "ASIA_total", "EUROPE_total", "OCEANIA_total", 
  #                     "AFRICA_expl", "AMERICA_expl", "ASIA_expl", "EUROPE_expl", "OCEANIA_expl")) %>%  
  # mutate(ctry = ifelse(ctry == "EU_expl", "EU", ctry)) %>% 
  
  # only docs >= 2 CM: 
  group_by(sent_id) %>% 
  mutate(comentions = sum(mentions > 0)) %>% 
  ungroup() %>% 
  filter(comentions > 1 & mentions > 0)

relevant_sentences %<>% 
  inner_join(., relevant_sentences %>% 
               select(sent_id, ctry, mentions), join_by(sent_id)) %>% 
  filter(ctry.x < ctry.y) %>% 
  relocate(ctry.y, mentions.y, .after = ctry.x)

write_rds(relevant_sentences, paste0(data_path, "comentions/comentions_sent.rds"))
rm(relevant_sentences)
