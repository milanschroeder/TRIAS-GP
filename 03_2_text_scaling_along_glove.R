#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Scale Commission communication along semantic similarities
#           derived from pre-trained GloVe 
# Author:   @milanschroeder / @ChRauh (24.02.2025)
#########################################################################


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools CRAN v0.4.1
library(quanteda) # Quantitative Analysis of Textual Data CRAN v3.3.0
library(magrittr)
library(GGally)



# Paths ####
# Needed as big files currently not part of the repo

 data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
# data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
#data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# Learned semantic similarity weights ####
# See 03_1_semantic_similarity_weights_glove.R

simil_files <- list.files(paste0(data_path, "glove_models/"), pattern = "SemSimilWeights")
simils <- read_rds(paste0(data_path, "glove_models/", simil_files[1])) 
for (i in 2:length(simil_files)) {
  varname <- str_extract(simil_files[i], "(?<=-)(.*?)(?=\\.)") %>% tolower()
  print(varname)
  current <- read_rds(paste0(data_path, "glove_models/", simil_files[i]))
  if ("sim.target" %in% names(current)) {
    simils %<>% 
      left_join(current %>%  
                  select(token, sim.target) %>% 
                  rename(!!varname := sim.target), 
                by = "token")  
  } else {
    simils %<>% 
      left_join(current, 
                by = "token") 
  }
}

# Clean up
simils <- simils %>% 
#  select(-c(coop, conf, friend, foe, friendly, hostile)) %>% # Drop ind poles of the scales
  rename(digi_adv = digitalityadvanced,
         digi_sim = digitalitysimple)
rm(current, varname, i, simil_files)
gc()


# Tokenize sentences ####
# ~ 5 mins on HP
  
tokens_sents <- 
  read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  select(text = text_sent, sentence_id) %>% 
    mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relevant than www.ec.europa.eu
           text = str_replace_all(text, "_|-", " ")) %>% 
    unnest_tokens(input = text, # Text to tokenize
                  output = token, # Tokenizer output
                  token = "words", # tokenization level
                  to_lower = T) %>% 
    # remove meaningless tokens:
    filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
    filter(str_detect(token, "[a-z]")) %>% # Only tokens with letters in them
    filter(nchar(token) > 1)  



# Merge token level data with weights and aggregate to sentence level ####
# ~ 1.8 hours mins on TP

start <- Sys.time()
sent_weigths <- tokens_sents %>% 
  left_join(
  simils,
  join_by(token)) %>%
  group_by(sentence_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by sentences, unknown tokens excluded (grouping var should be automatically excluded)
  ungroup()
Sys.time()-start

# Export
write_rds(sent_weigths, paste0(data_path, "cleaned_data/scaling_glove_sentlevel.rds"))
rm(tokens_sents, sent_weigths)
gc()





# Tokenize paragraphs ####
# ~ XXX mins on HP
start <- Sys.time()
tokens_para <- 
    read_rds(paste0(data_path, "cleaned_data/data_paralevel.rds")) %>% select(text = text_para, text_id) %>% 
      mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relvant than www.ec.europa.eu
             text = str_replace_all(text, "_|-", " ")) %>% 
      unnest_tokens(input = text, # name of text var
                    output = token, # name of output var
                    token = "words",
                    to_lower = T) %>% 
      # remove meaningless tokens:
      filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
      filter(str_detect(token, "[a-z]")) %>% # Only tokens with letters in them
      filter(nchar(token) > 1) 
Sys.time()-start

# Merge token level data with weights and aggregate to paragraph level ####
# ~ 57 mins on TP

start <- Sys.time()
para_weights <- tokens_para %>% 
  left_join(
  simils,
  join_by(token)) %>%
  group_by(text_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by para, unknown tokens excluded (grouping var should be automatically excluded)
  ungroup()
Sys.time()-start

# Export
write_rds(para_weights, paste0(data_path, "cleaned_data/scaling_glove_paralevel.rds"))
rm(para_weights, tokens_para)




# Tokenize documents ####
# ~ XX mins on HP

tokens_doc <- 
  read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% select(text = text_doc, doc_id) %>% 
  mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relvant than www.ec.europa.eu
         text = str_replace_all(text, "_|-", " ")) %>% 
  unnest_tokens(input = text, # name of text var
                output = token, # name of output var
                token = "words",
                to_lower = T) %>% 
  # remove meaningless tokens:
  filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
  filter(str_detect(token, "[a-z]")) %>% # Only tokens with letters in them
  filter(nchar(token) > 1)


# Merge token level data with weights and aggregate to document level ####
# ~ 2.9 h on TP

start <- Sys.time()
doc_weights <-
  tokens_doc %>% 
  left_join(
      simils,
      join_by(token)
      ) %>%
  group_by(doc_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by para, unknown tokens excluded (grouping var automatically excluded)
  ungroup()
Sys.time()-start

# Export
write_rds(tokens_doc, paste0(data_path, "cleaned_data/scaling_glove_doclevel.rds"))
rm(tokens_doc)
gc()




# Compare different scales on sentence level ####

comp.df <- read_rds(paste0(data_path, "cleaned_data/scaling_glove_sentlevel.rds")) %>% 
  select(gti, coop_confl, friend_foe)

comp.pl <- 
  ggpairs(comp.df %>% sample_n(50000),
          lower = list(continuous = wrap("points", alpha = 0.05, shape = 16)),  # Set alpha and shape
          upper = list(continuous = wrap("cor", size = 6, color = "black")),
          columnLabels = c("GTI (Hostility)", "Coop-Conflict", "Friend-Foe"))+
  labs(title = "Comparing differently seeded scales on sentence level",
       subtitle = "GloVe-based scales, random sample of 50,000 sentences from Comm. corpus")+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size = 10)  
  )

ggsave("./output/glove_plots/ComparingScalingWeigthsSentenceLevel.png", comp.pl, width = 20, height = 20, units = "cm")








