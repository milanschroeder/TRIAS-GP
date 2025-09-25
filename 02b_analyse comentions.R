# analyse comentions ####

library(tidyverse)
library(magrittr)

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/CountryMentions/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/CountryMentions/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/CountryMentions/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/CountryMentions/" # CR/TP


for (level in c("sent", "para", "doc")) {
 
  # get all single mentions:
  singlementions = 
    read_rds(paste0(data_path, "allCMs_", level, "level.rds")) %>% 
    rename(id = 1) %>%
    select(id, AD:last_col()) %>% 
    pivot_longer(AD:last_col(), names_to = "ctry", values_to = "mentions") %>% 
  
    group_by(id) %>% 
    mutate(comentions = sum(mentions > 0)) %>% 
    ungroup() %>% 
    filter(comentions == 1 & mentions > 0)
                  
  gc()               
  
  # avg comentions per country ####
  comentions_country =
    read_rds(paste0(data_path, "allCMs_", level, "level.rds")) %>% 
    rename(id = 1) %>%
    select(id, AD:last_col()) %>% 
    pivot_longer(AD:last_col(), names_to = "ctry", values_to = "mentions") %>% 
    
    group_by(id) %>% 
    mutate(comentions = sum(mentions > 0)) %>% 
    ungroup() %>%
    filter(mentions > 0) %>%  # only actual CMs!
    
    group_by(ctry) %>%
    summarise(avg_comentions = mean(comentions),
              total_mentions = sum(mentions)) %>% 
    ungroup()
  
  gc()
  
  comentions_country %>%
    arrange(total_mentions) %>%
    ggplot(aes(x = total_mentions, y = avg_comentions, label = ctry)) +
    geom_label(label.size = 0.05)
  
  ggsave(paste0(data_path, "comentions/CMs_comentions_", level, ".png"))
  
  
  # identify which other country is mentioned along each country most frequently ####
  
  comentions = read_rds(paste0(data_path, "comentions/comentions_", level, ".rds")) %>%
    rename(id = 1) # name id vars identical for processing (not needed for differentiation downstream)
  
  # comentions combinations two-way: ####
  full_cm_relations = 
    bind_rows(
      comentions %>% relocate(mentions.x, .before = mentions.y),
      comentions %>% 
        rename(ctry.x = ctry.y, ctry.y = ctry.x, mentions.x = mentions.y, mentions.y = mentions.x),
      singlementions %>%
        rename(ctry.x = ctry, mentions.x = mentions) %>%
        mutate(ctry.y = NA, mentions.y = NA)
    )
  
  rm(comentions)
  gc()
  
  cm_relation_count =
    full_cm_relations %>%
    count(ctry.x, ctry.y, sort = T, name = "n_combi") %>%
    
    # add overall counts for country.x for each orientation
    left_join(comentions_country, join_by(ctry.x == ctry)) %>%
    rename(total_mentions.x = total_mentions) %>%
    mutate(
      share_combi = n_combi / total_mentions.x
    ) %>%
    arrange(desc(share_combi))
  
  write_rds(cm_relation_count,
            paste0(data_path, "comentions/count_comentions_twoway_", level, ".rds"))
  

  rm(cm_relation_count)
  
  gc()
  
  
  # comentions combinations multi-way: ####
  
  cm_multi_count =
    full_cm_relations %>%
    distinct(ctry.x, id, .keep_all = T) %>%
    
    arrange(ctry.x) %>%
    
    group_by(id) %>%
    summarise(ctry_group = str_c(ctry.x, collapse = ", "), 
              n_ctry = n()) %>% # paste countries together
    ungroup() %>%
    
    count(ctry_group, n_ctry, sort = T) 
  
  write_rds(cm_multi_count, paste0(data_path, "comentions/count_comentions_groups_", level, ".rds"))

  rm(cm_multi_count, full_cm_relations)
  gc()
  
}
