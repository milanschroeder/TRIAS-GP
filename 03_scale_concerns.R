# latent semantic scaling ####

# libraries ####
library(tidytext)
library(quanteda)
library(tidyverse)
library(magrittr)

# data ####
data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/"

# docs <- read_rds(paste0(data_path, "data_doclevel.rds"))
# paras <- read_rds(paste0(data_path, "data_paralevel.rds"))
# sents <- read_rds(paste0(data_path, "data_sentlevel.rds"))


######
### Full Semantic scaling method

# prepare embedding model ####
{
  # glove.300 <- read_rds("../TRIAS-paper1/large_data/glove.6B.300d.rds") # from ../TRIAS-paper1/xx_process_glove.R
  # 
  # # Clean up the vocabulary a bit (lots of rare trash in there, exclude stopwords)
  # vocab <- names(glove.300) %>% 
  #   as.data.frame() %>% 
  #   rename(token = 1) %>% 
  #   filter(str_detect(token, "[a-z]")) %>% 
  #   filter(nchar(token) > 1) %>% 
  #   filter(!(token %in% stopwords("en"))) %>% 
  #   filter(!str_detect(token, "\\.")) %>% 
  #   filter(!str_detect(token, "[0-9]"))
  # glove.300 <- glove.300 %>% select(vocab$token)
  # 
  # write_rds(glove.300, "../TRIAS-paper1/large_data/glove_cleaned.rds")
}

# functions ####

# find_sim_wvs <- function(seed_dict, # c("seedterms")
#                          concept,  # "varname"
#                          embeddings = read_rds("../TRIAS-paper1/large_data/glove_cleaned.rds"), # or other in same format
#                          sim_measure = "cosine" # "cosine" or "dotproduct"
# ){
#   
#   seed_vector <- embeddings %>% 
#     select(all_of(seed_dict)) %>% # Probably needs a check whether all seeds exist in word vector
#     rowMeans() # Aggregation by mean
#   
#   # find similar terms: ####
#   
#   require(text2vec)
#   this_wv_mat <- matrix(seed_vector, ncol=length(seed_vector), nrow=1)
#   all_wvs_mat <- as.matrix(embeddings)
#   
#   if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
#     print("switching dimensions on the all_wvs_matrix")
#     all_wvs_mat <- t(all_wvs_mat)
#   }
#   
#   if(sim_measure == "cosine"){
#     # cosine smilarity:
#     cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
#     sim = cos_sim#[,1]
#   }
#   
#   if(sim_measure == "dotproduct"){
#     
#     dot_product_similarity <- function(matrix, vector) {
#       apply(matrix, 1, function(row) sum(row * vector))
#     }
#     
#     # dot product similarity:
#     dot_sim = dot_product_similarity(all_wvs_mat, this_wv_mat)
#     sim = dot_sim
#   }
#   
#   simils <-
#     sim %>% 
#     as.data.frame() %>% 
#     rename(!!concept := !!sym(names(.)[1])) %>% 
#     rownames_to_column("token") %>% 
#     mutate(seed = token %in% seed_dict) %>% 
#     filter(!(token %in% quanteda::stopwords("english"))) %>% 
#     arrange(desc(!!concept)) %>% 
#     mutate(rank.simil=row_number()) %>% 
#     relocate(rank.simil) # rank by similarity to seed vector
#   
#   return(simils)
# }

#sim_terms <- find_sim_wvs(seed_dict = seed, concept = concept)

# load learned simils: ####
# from 03_semantic_similarity_weights.R

# Merge semantic similarity weights to tokens data ####

simil_files <- list.files(paste0(data_path, "../large_data-paper1/"), pattern = "SemSimilWeights")

simils <- read_rds(paste0(data_path, "../large_data-paper1/", simil_files[1])) 
 
for (i in 2:length(simil_files)) {
  
  varname <- str_extract(simil_files[i], "(?<=-)(.*?)(?=\\.)")
  
  simils %<>% 
    left_join(read_rds(paste0(data_path, "../large_data-paper1/", simil_files[i])) %>% 
                select(token, sim.target) %>% 
                rename(!!varname := sim.target), 
              by = "token")  
}


# tokenize df ####

# scale_data <- function(df,
#                        id_var, # "para_id"
#                        text_var, # "text_para"
#                        concept_name, # "concept_name"
#                        seed # c("")
#                        ){
# 
#   # sim_terms <- find_sim_wvs(seed_dict = seed, concept = concept_name)
# 
#   df %<>% rename(text = text_var, id = id_var)
  
tokens_sents <- 
  read_rds(paste0(data_path, "data_sentlevel.rds")) %>% 
  select(text = text_sent, sentence_id) %>% 
    mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relvant than www.ec.europa.eu
           text = str_replace_all(text, "_|-", " ")) %>% 
    unnest_tokens(input = text, # name of text var
                  output = token, # name of output var
                  token = "words",
                  to_lower = T) %>% 
    # remove meaningless tokens:
    filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
    filter(str_detect(token, "[a-z????]")) %>% # Only tokens with letters in them
    filter(nchar(token) > 1)  %>% 

    # aggregate paras ####
left_join(
  .,
  simils,
  join_by(token)
) %>%
  group_by(sentence_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by para, unknown tokens excluded (grouping var should be automatically excluded)
  ungroup()

write_rds(tokens_sents, paste0(data_path, "scaling_glove_sentlevel.rds"))
rm(tokens_sents)
gc()

tokens_para <- 
    read_rds(paste0(data_path, "data_paralevel.rds")) %>% select(text = text_para, para_id) %>% 
      mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relvant than www.ec.europa.eu
             text = str_replace_all(text, "_|-", " ")) %>% 
      unnest_tokens(input = text, # name of text var
                    output = token, # name of output var
                    token = "words",
                    to_lower = T) %>% 
      # remove meaningless tokens:
      filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
      filter(str_detect(token, "[a-z????]")) %>% # Only tokens with letters in them
      filter(nchar(token) > 1)  %>% 

  # aggregate paras ####
left_join(
  .,
  simils,
  join_by(token)
) %>%
  group_by(para_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by para, unknown tokens excluded (grouping var should be automatically excluded)
  ungroup()

write_rds(tokens_para, paste0(data_path, "scaling_glove_paralevel.rds"))
rm(tokens_para)

  
tokens_doc <- 
  read_rds(paste0(data_path, "data_doclevel.rds")) %>% select(text = text_doc, doc_id) %>% 
  mutate(text = str_remove_all(text, "\'|’|#|\\.|[0-9]"), # choose to just remove . as u.s. much more relvant than www.ec.europa.eu
         text = str_replace_all(text, "_|-", " ")) %>% 
  unnest_tokens(input = text, # name of text var
                output = token, # name of output var
                token = "words",
                to_lower = T) %>% 
  # remove meaningless tokens:
  filter(!(token %in% quanteda::stopwords("en"))) %>% # Exclude en stopwords
  filter(str_detect(token, "[a-z????]")) %>% # Only tokens with letters in them
  filter(nchar(token) > 1)   %>% 
  
  # aggregate paras ####
    left_join(
      .,
      simils,
      join_by(token)
      ) %>%
  group_by(doc_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T)) %>% # Means of semantic simil weights by para, unknown tokens excluded (grouping var automatically excluded)
  ungroup()

write_rds(tokens_doc, paste0(data_path, "scaling_glove_doclevel.rds"))
rm(tokens_doc)

#   return(df_scaled)
# }

# paras_scaled <- scale_data(df = read_rds(paste0(data_path, "data_paralevel.rds")), 
#                            id_var = "para_id",
#                            text_var = "text_para"#,
#                            # concept = "coop", 
#                            # seed = c("cooperation", "agreement", "support", "collaboration", "unity")
#                            )
#                            


