library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS BrÃ¼ckenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee BrÃ¼ckenprojekt Ju-Chri/Daten/" # CR/HP
data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Document-level information ###
docs <- read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds"))

# Document types in corpus
doc_types <- docs %>% 
  mutate(
    year = lubridate::year(date),
    wc =  str_count(text_doc, "\\W+")+1
  ) %>% 
  # filter(year < 2024) %>% 
  group_by(doc_type) %>% 
  summarise(count = n(),
            mean_wc = round(mean(wc, na.rm = T), 2),
            mean_paras = round(mean(n_paras, na.rm = T), 2),
            min_date = min(date, na.rm = T),
            max_date = max(date, na.rm = T),
            en_share = round(mean(main_lang_doc == "en", na.rm = T), 4)*100) %>% # percentage
  filter(!is.na(doc_type)) %>% 
  arrange(desc(count))



# Sumnmary row

summ <- data.frame(doc_type = "Totals/Averages",
                  count = sum(doc_types$count),
                  mean_wc = mean(doc_types$mean_wc),
                  mean_paras = mean(doc_types$mean_paras),
                  min_date = min(doc_types$min_date),
                  max_date = max(doc_types$max_date),
                  en_share = mean(doc_types$en_share))

doc_types <- rbind(doc_types, summ)

# Export
write_csv2(doc_types, "./output/CorpusOverview.csv")












