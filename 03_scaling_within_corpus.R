

# Packages #####
library(tidyverse)
library(quanteda)
library(countrycode)
library(LSX)
library(extrafont)

# key geographical entities to remove before scaling:
geo_entities <- c(c(newsmap::data_dictionary_newsmap_en %>% unlist(),
                  newsmap::data_dictionary_newsmap_ar %>% unlist(),
                  newsmap::data_dictionary_newsmap_de %>% unlist(),
                  newsmap::data_dictionary_newsmap_es %>% unlist(),
                  newsmap::data_dictionary_newsmap_fr %>% unlist(),
                  newsmap::data_dictionary_newsmap_he %>% unlist(),
                  newsmap::data_dictionary_newsmap_it %>% unlist(),
                  newsmap::data_dictionary_newsmap_ja %>% unlist(),
                  newsmap::data_dictionary_newsmap_pt %>% unlist(),
                  newsmap::data_dictionary_newsmap_pt %>% unlist(),
                  newsmap::data_dictionary_newsmap_ru %>% unlist(),
                  newsmap::data_dictionary_newsmap_zh_cn %>% unlist(),
                  newsmap::data_dictionary_newsmap_zh_tw %>% unlist()
                  ) %>% tibble(entities = .) %>% 
  mutate(entities = ifelse(str_length(entities %>% str_remove("\\*")) < 6 | str_detect(entities, "\\*"), 
                           paste0("\\b", entities %>% str_remove("\\*"), "\\b"),
                           entities %>% str_remove("\\*"))) %>% 
  filter(str_detect(entities, "\\bus\\b", negate = T)) %>%  # remove us bc false positives
  pull(entities) %>% unique(),
  "afri", "\\beurop", "americ", "oceania", "asian", "\\busa\\b")
  
# Illustrate text model ####

plot_tmod <- function(tmod = tmod_lss, seedterms, concept){
  
  set.seed(1905)
  
  df <- data.frame(words = names(tmod$beta),
                   polarity = tmod$beta,
                   freq = tmod$frequency)
  
  df.anchors <- df %>% 
    filter(words %in% seedterms) %>% 
    mutate(group = "Seedteerms set manually by researcher")
  
  df.learned <- 
      df %>% 
      #ToDo: maybe add frequency weight... 
      filter(words %in% names(head(coef(tmod), 100) %>% sample(size = 10))) %>% 
      mutate(group = "Examples for algorithmically identified terms")
  
  # add negative examples for bipolar scales:
  if (str_detect(concept, "_")) {
    df.learned <- 
      rbind(
        df.learned,
        df %>% 
            filter(words %in% names(tail(coef(tmod), 100) %>% sample(size = 10))) %>% 
            mutate(group = "Examples for algorithmically identified terms")
      )
  }
    
  df.highlight <- rbind(df.anchors, df.learned)
  df$highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Seedteerms set manually by researcher", "Examples for algorithmically identified terms")))
  
  # Plot textmodel illustration
  
  ggplot()+
    geom_text(data = df[sample(nrow(df), 5000, replace = F), ], alpha= .4, color = "grey60", aes(y=log(freq), x = polarity, label = words)) +
    geom_vline(xintercept = 0, linetype = "dashed")+
    geom_text(data = df.highlight, aes(y=log(freq), x = polarity, label = words, color = group), fontface = "bold")+
    scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
    labs(title = "Illustration of Scaling Model",
         subtitle = "Based on Latent Semantic Scaling Algorithm (Watanabe 2021),\nall Documents in EU Press Archive \nrandom sample of 5,000 words",
         y = "Word Frequency\nover all EU Documents (log)",
         x = paste("Word Polarity\non", concept, "scale"))+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text = element_text(color = "black"),
          text = element_text(family = "Dahrendorf"),
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 10))
  
  ggsave(paste0("./Output/LSX_plots/IllustrateScalingModel_", concept, ".png"), width = 26, height = 16, units = "cm")
}

# Corpus ####
data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/"

# docs <- read_rds(paste0(data_path, "data_doclevel.rds"))
# paras <- read_rds(paste0(data_path, "data_paralevel.rds"))
sents <- read_rds(paste0(data_path, "data_sentlevel.rds")) %>% 
  select(sentence_id, doc_id, text = text_sent)

# Quanteda objects  ###

# Corpus
EC_corpus <- corpus(sents$text, docvars = sents[ ,c("sentence_id", "doc_id")])

# Sentence tokenization
#corp_sent <- corpus_reshape(ungd_corpus, to =  "sentences")
toks_sent <- EC_corpus %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE, split_tags = T) %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(pattern = geo_entities %>% paste(collapse = "|"), valuetype = "regex") # Remove country/continent names - they should not affect the scale

# Doc frequency matrix (sentences)
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

topfeatures(dfmat_sent, 20)


# Seed dictionary ####
# Aim is to scale cooperative/conflictive language

vocabs <- 
list(
friend = list(friend = c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative")) %>% dictionary() %>% as.seedwords(),
foe = list(foe = c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")) %>% dictionary() %>% as.seedwords(),
friend_foe = list(
  positive = c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative"),
  negative = c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")
) %>% dictionary() %>% as.seedwords(),

DigitalitySimple = list(DigitalitySimple = c("digital", "online", "computer", "internet", "algorithm")) %>% dictionary() %>% as.seedwords(),
DigitalityAdvanced = list(DigitalityAdvanced = read_rds("../TRIAS-paper1/large_data/SemSimilWeights-DigitalityAdvancedFreqCorrection.rds") %>% filter(seed == T) %>% pull(token)) %>% dictionary() %>% as.seedwords(),

Economy = list(Economy = c("economy", "economic", "markets", "trade", "business")) %>% dictionary() %>% as.seedwords(),
Security = list(Security = c("security", "defense", "military", "espionage", "intelligence")) %>% dictionary() %>% as.seedwords(),
LibRights = list(LibRights = c("rights", "liberty", "freedom", "justice", "equality")) %>% dictionary() %>% as.seedwords(),

coop = list(coop = c("cooperation", "agreement",    "support",    "collaboration", "unity")) %>% dictionary() %>% as.seedwords(),
conf = list(confl = c("conflict",    "disagreement", "opposition", "confrontation", "hostility")) %>% dictionary() %>% as.seedwords(),
coop_conflict = list(
  positive = c("cooperation", "agreement",    "support",    "collaboration", "unity"),
  negative = c("conflict",    "disagreement", "opposition", "confrontation", "hostility")
) %>% dictionary() %>% as.seedwords()
)


# Run LSS model
# Intentionally no context words

for (i in 1:length(vocabs)) {
  tmod_lss <- textmodel_lss(dfmat_sent, seeds = vocabs[[i]], k = 300, cache = TRUE)
  write_rds(tmod_lss, paste0(data_path, "../LSX_models/lss_model_", names(vocabs[i]), ".rds"))
  
  cat("Scaled", names(vocabs[i]), ". Significant words:\npositive:")
  print(head(coef(tmod_lss),20))
  cat("negative:\n")
  print(tail(coef(tmod_lss),20))
  
  plot_tmod(tmod = tmod_lss, seedterms = names(vocabs[[i]]), concept = names(vocabs[i]))
  
  rm(tmod_lss)
  gc()
}
            
# Illustarte word weights
# textplot_terms(tmod_lss, unlist(conflict_terms), max_words = 5000)

# Maybe construct a nicer plot, highlighting random 'learned' words in different color
# Export model to that end



