# save new clean sentlevel data ####

library(tidyverse)
library(magrittr)
library(cld2)
library(ngram)

corpus <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_paralevel.rds")

text <- corpus %>% 
  mutate(id = str_c(text_id, "-")) %>% 
  corpus(docid_field = "id", text_field = "text_para") %>% 
  quanteda::tokenize_sentence() %>% 
  unlist(use.names = T)

sents <- 
  tibble(
    id = names(text),  
    text_sent = text
  ) %>% 
  separate(id, into = c("text_id", "para_pos"), sep = "-", remove = T) %>% 
  mutate(
    para_pos = as.numeric(para_pos),
    text_id = as.numeric(text_id),
    para_pos = if_else(is.na(para_pos), 1, para_pos)) %>% 
  left_join(., 
            corpus, 
            join_by(text_id)) %>% 
  select(text_id, para_pos, text_sent, all_of(names(corpus)), everything())  

sents %<>% 
  rename(CAP_name_para = CAP_name, 
         CAP_code_para = CAP_code) %>% 
  rowwise() %>% # for wordcount()
  mutate(legth_sent = str_length(text_sent),
         words_sent = ngram::wordcount(text_para) %>% as.integer(),
         lang_sent = cld2::detect_language(text_sent),
         mostly_numbers_sent = str_count(text_sent, "[:digit:]") > str_count(text_sent, "[:alpha:]"), # catches: dates, ids, phone, tables
         has_link_sent = str_detect(text_sent, "https?://|ftp://|www\\.|@")
         ) %>% ungroup() %>% 
  left_join(
    ., read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_doclevel.rds") %>% 
      select(doc_id, doc_type, date), 
    join_by(doc_id)
  )


sents %<>% 
  mutate(sent_id = row_number()) %>% 
  relocate(sent_id, .before = text_id) %>% 
  select(-text_para)

write_rds(sents, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_sentlevel.rds")


# save new clean doclevels data ####

library(cld2)
library(ngram)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

corpus <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_paralevel.rds")
docs_old <- read_rds("~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/OLD_data_doclevel.rds")

text <- corpus %>% 
  group_by(doc_id, doc_key) %>% 
  summarize(
    text_doc = str_c(text_para, collapse = "\n"),
    legth_doc = str_length(text_doc),
    words_doc = ngram::wordcount(text_doc) %>% as.integer(),
    lang_doc_main = cld2::detect_language(text_doc), 
    across(clearly_list:is_title, mean, .names = "share_{.col}"),
    main_CAP_doc = find_mode(CAP_name)
  ) %>% ungroup()


#write_rds(text, "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/data_doclevel.rds")


