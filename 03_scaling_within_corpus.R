

# Packages #####
library(tidyverse)
library(quanteda)
library(countrycode)
library(LSX)
#library(extrafont)


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
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(countrycode::codelist$country.name.en) # Remove english country names - they should not affect the scale

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

coop_vocab = list(coop = c("cooperation", "agreement",    "support",    "collaboration", "unity")) %>% dictionary() %>% as.seedwords(),
conf_vocab = list(confl = c("conflict",    "disagreement", "opposition", "confrontation", "hostility")) %>% dictionary() %>% as.seedwords(),
confl_coop = list(
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
  head(coef(tmod_lss),20)
  cat("negative:\n")
  tail(coef(tmod_lss),20)
  
  rm(tmod_lss)
  gc()
}
            
# Illustarte word weights
# textplot_terms(tmod_lss, unlist(conflict_terms), max_words = 5000)

# Maybe construct a nicer plot, highlighting random 'learned' words in different color
# Export mdoel to that end


# Illustrate text model ####


df <- data.frame(words = names(mod$beta),
                 polarity = mod$beta,
                 freq = mod$frequency)

df.anchors <- df %>% 
  filter(words %in% c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative",
                      "foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")) %>% 
  mutate(group = "Beispiele für vom Forscher gesetzte Ankerbegriffe")

df.learned <- df %>% 
  filter(words %in% c("coexistence", "harmonious", "respectful", "partnership", "ties", "win-win", "state-to-state", "legitimate", "stable",
                      "security", "community", "global", "international", "national", "unemployed", "geopolitics", "misperceptions", "issue", "conducive", 
                      "militaristic", "provocative", "propaganda", "irresponsible", "warmongering", "unacceptable", "military", "tension", "regret", "stop")) %>% 
  mutate(group = "Beispiele für vom Algorithmus automatisch skalierte Begriffe")

df.highlight <- rbind(df.anchors, df.learned)
df$highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Beispiele für vom Forscher gesetzte Ankerbegriffe", "Beispiele für vom Algorithmus automatisch skalierte Begriffe")))

# Plot textmodel illustration
set.seed(2147483647)
ggplot()+
  geom_text(data = df[sample(nrow(df), 6000, replace = F), ], alpha= .4, color = "grey60", aes(y=log(freq), x = polarity, label = words)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=log(freq), x = polarity, label = words, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Illustration des Skalierungsmodells",
       subtitle = "Basierend auf dem Latent Semantic Scaling Algorithmus (Watanabe 2021),\nallen Reden in der UN-Vollversammlung 1970-2020 (Baturo, Mikhaylov, und Dasandi, 2017)\nund einer Zufallsstichprobe von 6.000 Worten",
       y = "Häufigkeit der Worte\nüber alle UN-Reden (logarithmiert)",
       x = "Polarität des Wortes\nauf der Skala zwischen konfliktbetonter und kooperativer Sprache")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10))

#ggsave("./Plots/IllustrateScalingModel.png", width = 26, height = 16, units = "cm")
