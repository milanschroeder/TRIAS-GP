#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Scale sentences along LSX weights
# Author:   @ChRauh (24.02.2025)
#########################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(quanteda) # Quantitative Analysis of Textual Data CRAN v3.3.0
library(LSX) # Semi-Supervised Algorithm for Document Scaling CRAN v1.3.1
library(GGally) # Extension to 'ggplot2' CRAN v2.1.2


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP


# Commission Corpus ####
# Sentence level 
sents <- read_rds(paste0(data_path, "cleaned_data/data_sentlevel.rds")) %>% 
  select(sentence_id, doc_id, text = text_sent)


# Quanteda objects  ####
EC_corpus <- corpus(sents$text, docvars = sents[ ,c("sentence_id", "doc_id")])

# Tokenization and feature (de-)selection
# Takes some time bc of geo filter
toks_sent <- EC_corpus %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE, split_tags = T) %>% 
  tokens_remove(stopwords("en")) 
# %>% 
#   tokens_remove(pattern = geo_entities %>% paste(collapse = "|"), valuetype = "regex") # Remove country/continent names - they should not affect the scale

# Doc frequency matrix (sentences)
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

topfeatures(dfmat_sent, 20)



# Predict sentence weights from LSS models ####

# List model files
lss_files <- list.files(paste0(data_path, "LSX_models/"), pattern = "lss_model_")

# Target data - basd on docvars from DFM
df <- docvars(dfmat_sent)

# Loop through files and assemble LSS predictions per sentence
for (i in 1:length(lss_files)){
  varname <- lss_files[i] %>% str_remove("lss_model_") %>% str_remove(fixed(".rds"))
  print(varname)
  lss <- read_rds(paste0(data_path, "LSX_models/", lss_files[i])) 
  current <- predict(lss, newdata = dfmat_sent) %>% as.data.frame()
  names(current) <- varname
  df <- cbind(df, current) # Order is consistent, we are predicting from the same DFM
  rm(current)
}
names(df) <- tolower(names(df))


# Export results
df %>% write_rds(paste0(data_path, "cleaned_data/scaling_lsx_sentlevel.rds"))



# Compare different scales on sentence level ####


comp.df <- df %>% 
  select(gti, coop_confl, friend_foe)

comp.pl <- 
  ggpairs(comp.df %>% sample_n(50000),
          lower = list(continuous = wrap("points", alpha = 0.05, shape = 16)),  # Set alpha and shape
          upper = list(continuous = wrap("cor", size = 6, color = "black")),
          columnLabels = c("GTI (Hostility)", "Coop-Conflict", "Friend-Foe"))+
  labs(title = "Comparing differently seeded scales on sentence level",
       subtitle = "LSX-based scales, random sample of 50,000 sentences from Comm. corpus")+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size = 10)  
  )

ggsave("./output/LSX_plots/ComparingScalingWeigthsSentenceLevel.png", comp.pl, width = 20, height = 20, units = "cm")




