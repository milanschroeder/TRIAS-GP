#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Extract semantic similarity token weigths from GloVe vectors
#           for scaling Commission Communication later on
# Author:   @ChRauh (21.02.2025)
#########################################################################

# Inputs: tokens.rds; glove.6B.300d.rds
# Output: semantic similarity weights (token-level) for various concept and respective visualizations


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(text2vec) # Modern Text Mining Framework for R CRAN v0.6.4 
library(newsmap) # Semi-Supervised Model for Geographical Document Classification CRAN v0.9.0 
library(ggwordcloud) # A Word Cloud Geom for 'ggplot2' CRAN v0.6.1 
library(quanteda) # Quantitative Analysis of Textual Data CRAN v4.0.2
library(GGally) # Extension to 'ggplot2' CRAN v2.2.1
library(patchwork)

# fix randomness
set.seed(1905)

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP


# Key geographical entities to remove before scaling ####

# As we want to scale the context in which countries are mentioned in Comm communication
# geography itself should not affect scaling weights

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



# Pre-trained word-embedding model ####

# Using the ready-made GLOVE word embeddings trained on Wikipedia and Gigaword
# Version with 400k vocabulary and 300 dimensions

# Get it here: https://nlp.stanford.edu/projects/glove/ # ! LARGE FILE !

# I have parsed this for other purposes already, if you start from the raw file see:
# https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

glove <- read_rds(paste0(data_path, "external_data/glove.6B.300d.rds"))

# Clean up the vocabulary a bit 
# lots of rare trash in there, exclude stopwords and geo entities
# takes some time ...
vocab <- names(glove) %>% 
  as.data.frame() %>% 
  rename(token = 1) %>% 
  filter(str_detect(token, "[a-z]")) %>% 
  filter(nchar(token) > 1) %>% 
  filter(!(token %in% stopwords("english"))) %>% 
  filter(!str_detect(token, "\\.")) %>% 
  filter(!str_detect(token, "[0-9]")) %>% 
  filter(!str_detect(token, geo_entities %>% paste(collapse = "|"))) # Remove country/continent names - they should not affect the scale
glove <- glove %>% select(vocab$token)
rm(vocab)
gc()

# Store (to save time in future iterations)
# write_rds(glove, paste0(data_path, "external_data/glove.6B.300d_GPfilter.rds"))
glove <- read_rds(paste0(data_path, "external_data/glove.6B.300d_GPfilter.rds"))



# Function to find nearest neighbors in word vectors model ####

# Taken from: https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9
# N.B.: Expects vectors with tokens in columns and dimensions in rows 
find_sim_wvs <- function(this_wv, all_wvs, top_n_res=40) {
  # this_wv will be a numeric vector; all_wvs will be a data.frame with words as columns and dimesions as rows
  require(text2vec)
  this_wv_mat <- matrix(this_wv, ncol=length(this_wv), nrow=1)
  all_wvs_mat <- as.matrix(all_wvs)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    print("switching dimensions on the all_wvs_matrix")
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  sorted_cos_sim <- sort(cos_sim[,1], decreasing = T) 
  return(head(sorted_cos_sim, top_n_res))
}




# Semantic similarity to ECONOMY terms ####

# Seed tokens
# Identical to the Rauh/EU-actorness paper as there is initial human validation
econ_vocab <- c("trade", "economy", "market", "business", "commerce")

# The average vector of these seeds in the pre.trained word embeddings
econ_vector <- glove %>% 
  select(all_of(econ_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
econ_simils <-
  find_sim_wvs(econ_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% econ_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
# Export semantic similarity dictionary ####
# write_rds(econ_simils, paste0(data_path, "glove_models/SemSimilWeights-Economy.rds", compress = "gz"))


# Visualize 
pl <-
  ggplot(econ_simils %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
         aes(label = token, size = sim.target, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'economy\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Economy.png", pl, width = 24, height = 12, units = "cm")





# Semantic similarity to SECURITY terms ####

# Seed tokens
# Identical to the Rauh/EU-actorness paper as there is initial human validation
sec_vocab <- c("security", "war", "military", "terrorism", "peace")

# The average vector of these seeds in the pre.trained word embeddings
sec_vector <- glove %>% 
  select(all_of(sec_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
sec_simils <-
  find_sim_wvs(sec_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% sec_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
# Export semantic similarity dictionary ####
# write_rds(sec_simils, paste0(data_path, "glove_models/SemSimilWeights-Security.rds", compress = "gz"))


# Visualize 
pl <-
  ggplot(sec_simils %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
         aes(label = token, size = sim.target, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'security\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Security.png", pl, width = 24, height = 12, units = "cm")




# Semantic similarity to LIBERAL DEMOCRACY terms ####

# Seed tokens
# Identical to the Rauh/EU-actorness paper as there is initial human validation
lib_vocab <- c("rights", "human", "freedom", "democracy", "law")

# The average vector of these seeds in the pre.trained word embeddings
lib_vector <- glove %>% 
  select(all_of(lib_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
lib_simils <-
  find_sim_wvs(lib_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% lib_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
# write_rds(lib_simils, paste0(data_path, "glove_models/SemSimilWeights-LibDem.rds", compress = "gz"))


# Visualize 
pl <-
  ggplot(lib_simils %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
         aes(label = token, size = sim.target, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'liberal rights\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_LibDem.png", pl, width = 24, height = 12, units = "cm")




# Semantic scaling COOPERATION/CONFLICT ####

# Seed tokens
coop_vocab <- c("cooperation", "agreement",    "support",    "collaboration", "unity")
conf_vocab <- c("conflict",    "disagreement", "opposition", "confrontation", "hostility")

# The average vectors of these seeds in the pre.trained word embeddings
coop_vector <- glove %>% 
  select(all_of(coop_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean
conf_vector <- glove %>% 
  select(all_of(conf_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
coop_simils <-
  find_sim_wvs(coop_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(coop = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% coop_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(coop)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector

conf_simils <-
  find_sim_wvs(conf_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(conf = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% conf_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(conf)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Scale
sc <- coop_simils %>% 
  select(token, coop) %>% 
  left_join(conf_simils %>% 
              select(token, conf),
            by = "token") %>% 
  mutate(coop_confl = coop-conf) %>% 
  arrange(desc(coop_confl))

# Export token weights
# write_rds(sc, paste0(data_path, "glove_models/SemSimilWeights-CooperationConflict.rds", compress = "gz"))


# Visualize (ind scales)

pl <-
  ggplot(coop_simils %>% ungroup() %>% arrange(desc(coop)) %>% head(250), 
         aes(label = token, size = coop, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'cooperation\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Cooperation.png", pl, width = 24, height = 12, units = "cm")

pl <-
  ggplot(conf_simils %>% ungroup() %>% arrange(desc(conf)) %>% head(250), 
         aes(label = token, size = conf, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'conflict\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Conflict.png", pl, width = 24, height = 12, units = "cm")


# Visualize scaling weights


# Stratified sample
df <- sc %>% 
  mutate(group = cut(coop_confl, 7)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
  sample_n(size = 28) %>% # n words from each interval
  ungroup()
  


# Selected terms for highlighting

df.anchors <- sc %>% 
  filter(token %in% c(coop_vocab, conf_vocab)) %>% 
  mutate(group = "Anchor terms defined by researcher")

df.learned <- sc %>% 
  filter(token %in% c("aggression", "mistrust", "anger", "tensions", "frustration", "escalation", "backlash", "insistence",
                      "understanding", "partnership", "solidarity", "negotiate", "compromise", "peaceful", "fiendship", "coalition",
                      "coexistence", "unaccaeptable", "stable", "stop", "normal", "responsible")) %>% 
  mutate(group = "Example terms scaled by the algorithm")

df.highlight <- rbind(df.anchors, df.learned)
df.highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Anchor terms defined by researcher", "Example terms scaled by the algorithm")))

df <- df %>% filter(!(token %in% df.highlight$token)) # Avoid duplicates


pl<- 
  ggplot()+
  #geom_text(data = df[sample(nrow(df), 2500, replace = F), ], alpha= .3, color = "grey60", aes(y=sample.int(100, size = 2500, replace =T), x = conf_coop, label = token)) +
  geom_text(data = df, alpha= .4, color = "grey60", aes(y=sample.int(100, size = nrow(df), replace =T), x = coop_confl, label = token)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=sample.int(100, size = nrow(df.highlight), replace =T), x = coop_confl, label = token, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Scaling conflictual vs cooperative language",
       subtitle = paste0("Based on Glove.6B.300d word vector model and a stratified random sample of ", nrow(df), " words from its vocabulary"),
       y = "",
       x = "Extracted word weights\nbetween conflictual and cooperative language")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        # text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

ggsave("./output/glove_plots/CoopConflictScalingWeigths.png", pl, width = 28, height = 14, units = "cm")




# Semantic scaling FRIEND/FOE ####

# Seed tokens
friend_vocab = c("friend", "partner",  "ally",  "peace", "peaceful",   "friendly", "cooperative")
foe_vocab =    c("foe",    "opponent", "enemy", "war",   "aggressive", "hostile",  "uncooperative")

# The average vectors of these seeds in the pre.trained word embeddings
friend_vector <- glove %>% 
  select(all_of(friend_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean
foe_vector <- glove %>% 
  select(all_of(foe_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
friend_simils <-
  find_sim_wvs(friend_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(friend = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% friend_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(friend)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector

foe_simils <-
  find_sim_wvs(foe_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(foe = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% foe_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(foe)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Scale
sc <- friend_simils %>% 
  select(token, friend) %>% 
  left_join(foe_simils %>% 
              select(token, foe),
            by = "token") %>% 
  mutate(friend_foe = friend-foe) %>% 
  arrange(desc(friend_foe))

# Export token weights
# write_rds(sc, paste0(data_path, "glove_models/SemSimilWeights-FriendFoe.rds", compress = "gz"))


# Visualize (ind scales)

pl <-
  ggplot(friend_simils %>% ungroup() %>% arrange(desc(friend)) %>% head(250), 
         aes(label = token, size = friend, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'friend\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Friend.png", pl, width = 24, height = 12, units = "cm")

pl <-
  ggplot(foe_simils %>% ungroup() %>% arrange(desc(foe)) %>% head(250), 
         aes(label = token, size = foe, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'foe\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Foe.png", pl, width = 24, height = 12, units = "cm")


# Visualize scaling weights


# Stratified sample
df <- sc %>% 
  mutate(group = cut(friend_foe, 5)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
  sample_n(size = 20) %>% # n words from each interval
  ungroup()



# Selected terms for highlighting

df.anchors <- sc %>% 
  filter(token %in% c(friend_vocab, foe_vocab)) %>% 
  mutate(group = "Anchor terms defined by researcher")

df.learned <- sc %>% 
  # ADAPT?!?
  filter(token %in% c("aggression", "mistrust", "anger", "tensions", "frustration", "escalation", "backlash", "insistence",
                      "understanding", "partnership", "solidarity", "negotiate", "compromise", "peaceful", "fiendship", "coalition",
                      "coexistence", "unaccaeptable", "stable", "stop", "normal", "responsible")) %>% 
  mutate(group = "Example terms scaled by the algorithm")

df.highlight <- rbind(df.anchors, df.learned)
df.highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Anchor terms defined by researcher", "Example terms scaled by the algorithm")))

df <- df %>% filter(!(token %in% df.highlight$token)) # Avoid duplicates


pl<- 
  ggplot()+
  #geom_text(data = df[sample(nrow(df), 2500, replace = F), ], alpha= .3, color = "grey60", aes(y=sample.int(100, size = 2500, replace =T), x = friend_foe, label = token)) +
  geom_text(data = df, alpha= .4, color = "grey60", aes(y=sample.int(100, size = nrow(df), replace =T), x = friend_foe, label = token)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=sample.int(100, size = nrow(df.highlight), replace =T), x = friend_foe, label = token, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Scaling friend vs foe",
       subtitle = paste0("Based on Glove.6B.300d word vector model and a stratified random sample of ", nrow(df), " words from its vocabulary"),
       y = "",
       x = "Extracted word weights\nbetween friend and foe")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        # text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

ggsave("./output/glove_plots/FriendFoeScalingWeigths.png", pl, width = 28, height = 14, units = "cm")





# Semantic scaling HOSTILITY/GTI ####

# Seed tokens
# As in the Geopolitical Threat Index paper (Trubowitz/Watanabe 20121): https://academic.oup.com/isq/article/65/3/852/6278490
# Dictionary loaded here is from their replication file

dict <- dictionary(file = paste0(data_path, 'external_data/GTI_keywords.yml'))
friendly_vocab = dict$seedwords$friendly
hostile_vocab = dict$seedwords$hostile

# The average vectors of these seeds in the pre.trained word embeddings
friendly_vector <- glove %>% 
  select(all_of(friendly_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean
hostile_vector <- glove %>% 
  select(all_of(hostile_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
friendly_simils <-
  find_sim_wvs(friendly_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(friendly = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% friendly_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(friendly)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector

hostile_simils <-
  find_sim_wvs(hostile_vector, glove, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(hostile = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% hostile_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(hostile)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Scale
sc <- friendly_simils %>% 
  select(token, friendly) %>% 
  left_join(hostile_simils %>% 
              select(token, hostile),
            by = "token") %>% 
  mutate(gti = friendly-hostile) %>% 
  arrange(desc(gti))

# Export token weights
# write_rds(sc, paste0(data_path, "glove_models/SemSimilWeights-GTI.rds", compress = "gz"))


# Visualize (ind scales)

pl <-
  ggplot(friendly_simils %>% ungroup() %>% arrange(desc(friendly)) %>% head(250), 
         aes(label = token, size = friendly, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'friendly\' seed terms (GTI)",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_GTI_friendly.png", pl, width = 24, height = 12, units = "cm")

pl <-
  ggplot(hostile_simils %>% ungroup() %>% arrange(desc(hostile)) %>% head(250), 
         aes(label = token, size = hostile, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'hostile\' seed terms (GTI)",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_GTI_hostile.png", pl, width = 24, height = 12, units = "cm")


# Visualize scaling weights


# Stratified sample
df <- sc %>% 
  mutate(group = cut(gti, 5)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
  sample_n(size = 25) %>% # n words from each interval
  ungroup()



# Selected terms for highlighting

df.anchors <- sc %>% 
  filter(token %in% c(friendly_vocab, hostile_vocab)) %>% 
  mutate(group = "Anchor terms defined by Trubowitz/Watanabe (2021)")

# Highlighting the same terms as in the GTI paper (interesting whther they occur in GloVe, though)
df.learned <- sc %>% 
  filter(token %in% dict$highlight) %>% 
  mutate(group = "Example terms scaled by the algorithm")

df.highlight <- rbind(df.anchors, df.learned)
df.highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Anchor terms defined by Trubowitz/Watanabe (2021)", "Example terms scaled by the algorithm")))

df <- df %>% filter(!(token %in% df.highlight$token)) # Avoid duplicates


pl<- 
  ggplot()+
  #geom_text(data = df[sample(nrow(df), 2500, replace = F), ], alpha= .3, color = "grey60", aes(y=sample.int(100, size = 2500, replace =T), x = friendly_hostile, label = token)) +
  geom_text(data = df, alpha= .4, color = "grey60", aes(y=sample.int(100, size = nrow(df), replace =T), x = gti, label = token)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=sample.int(100, size = nrow(df.highlight), replace =T), x = gti, label = token, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Scaling friendly vs hostile",
       subtitle = paste0("Based on Glove.6B.300d word vector model and a stratified random sample of ", nrow(df), " words from its vocabulary"),
       y = "",
       x = "Extracted word weights\nbetween friendly and hostile")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        # text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

ggsave("./output/glove_plots/GTI_ScalingWeigths.png", pl, width = 28, height = 14, units = "cm")




# # Semantic similarity to DIGITAL terms - radically simple dictionary ####
# # See respective markdown in TRIAS-Development for expanded reasoning
# 
# # Seed tokens
# dp_vocab <- c("digital", "online", "computer", "internet", "algorithm")
# 
# # The average vector of these seeds in the pre.trained word embeddings
# dp_vector <- glove %>% 
#   select(all_of(dp_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
#   rowMeans() # Aggregation by mean
# 
# # Extract cosine similarities to this average vector
# # (word weights that would semantically pull a text towards the seed)
# dp_simils <-
#   find_sim_wvs(dp_vector, glove, top_n_res = 400000) %>% 
#   as.data.frame() %>% 
#   rename(sim.target = 1) %>% 
#   rownames_to_column("token") %>% 
#   mutate(seed = token %in% dp_vocab) %>% 
#   filter(!(token %in% quanteda::stopwords("english"))) %>% 
#   arrange(desc(sim.target)) %>% 
#   mutate(rank.simil=row_number()) %>% 
#   relocate(rank.simil) # rank by similarity to seed vector
# 
# 
# # Export token weights
# # Export semantic similarity dictionary ####
# # write_rds(dp_simils, paste0(data_path, "glove_models/SemSimilWeights-DigitalitySimple.rds", compress = "gz"))
# 
# 
# # Visualize 
# pl <-
#   ggplot(dp_simils %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
#        aes(label = token, size = sim.target, color = seed)) +
#   geom_text_wordcloud() +
#   scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
#   scale_colour_manual(values = c("blue", "red"))+
#   labs(title = "Semantic similarity to vector of \'digital\' seed terms",
#        subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
#   theme_minimal()+
#   theme(plot.background = element_rect(color = "white"))
# ggsave("./output/glove_plots/SemanticallySimilarTerms_Digital_Simple.png", pl, width = 24, height = 12, units = "cm")



# # Semantic similarity to DIGITAL terms - intersubjective dictionary ####
# 
# # Seed tokens
# 
# # Five human coders received a list of the top-1500 terms semantically close to the radically simplified dictionary
# # Each coder rated each terms as "Clearly and exclusively referring to the digital technology?" [0|1]
# 
# dp_vocab2 = read_rds("../TRIAS-paper1/large_data/SemSimilWeights-DigitalityAdvancedFreqCorrection.rds") %>% filter(seed == T) %>% pull(token)
# 
# # ... and get back their average vector in the word embedding model  
# dp_vector2 <- glove %>% 
#   select(all_of(dp_vocab2)) %>% # Probably needs a check whether all seeds exist in word vector
#   rowMeans() # Aggregation by mean
# 
# # Extract cosine similarities to this average vector
# # (word weights that would semantically pull a text towards the seed)
# dp_simils2 <-
#   find_sim_wvs(dp_vector2, glove, top_n_res = 400000) %>% 
#   as.data.frame() %>% 
#   rename(sim.target = 1) %>% 
#   rownames_to_column("token") %>% 
#   mutate(seed = token %in% dp_vocab2) %>% 
#   filter(!(token %in% quanteda::stopwords("english"))) %>% 
#   arrange(desc(sim.target)) %>% 
#   mutate(rank.simil=row_number()) %>% 
#   relocate(rank.simil) # rank by similarity to seed vector
# 
# 
# # Export token weights
# # Export semantic similarity dictionary ####
# # write_rds(dp_simils2, paste0(data_path, "glove_models/SemSimilWeights-DigitalityAdvanced.rds", compress = "gz"))
# 
# # Visualize 
# pl <-
#   ggplot(dp_simils2 %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
#          aes(label = token, size = sim.target, color = seed)) +
#   geom_text_wordcloud() +
#   scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
#   scale_colour_manual(values = c("blue", "red"))+
#   labs(title = "Semantic similarity to vector of \'digital\' seed terms",
#        subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms (based on human coders)\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
#   theme_minimal()+
#   theme(plot.background = element_rect(color = "white"))
# ggsave("./output/glove_plots/SemanticallySimilarTerms_Digital_Advanced.png", pl, width = 24, height = 12, units = "cm")



# Compare the scales (token-level) ####


comp.df <- 
  read_rds(paste0(data_path, "glove_models/SemSimilWeights-GTI.rdsgz")) %>% select(token, gti) %>% 
  left_join(read_rds(paste0(data_path, "glove_models/SemSimilWeights-CooperationConflict.rdsgz")) 
            %>% select(token, coop_confl), 
            by = "token") %>% 
  left_join(read_rds(paste0(data_path, "glove_models/SemSimilWeights-FriendFoe.rdsgz")) 
            %>% select(token, friend_foe), 
            by = "token")


comp.pl <- 
  ggpairs(comp.df %>% select(gti, coop_confl, friend_foe) %>% sample_n(50000),
        lower = list(continuous = wrap("points", alpha = 0.05, shape = 16)),  # Set alpha and shape
        upper = list(continuous = wrap("cor", size = 6, color = "black")),
        columnLabels = c("GTI (Hostility)", "Coop-Conflict", "Friend-Foe"))+
  labs(title = "Comparing differently seeded scales on token level",
       subtitle = "GloVe vocabulary, random sample of 50,000 words")+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size = 10)  # Bold and increase facet label size
  )

ggsave("./output/glove_plots/ComparingScalingWeigthsTokenLevel.png", comp.pl, width = 20, height = 20, units = "cm")




# Internal consistency of the three friend/foe scales ####

# My key ideas here - If the respective seed terms form poles of a meaningful scale in the vector space
# defined by the pre-trained model two conditions should be conceptually met 

# A) The seed terms on one side of the scale should be more similar to each other, than to the words on the other side
# -> Can be assessed in two ways:
# 1. Cosine similarity of their vectors gives us their direction in the pre-trained space
# 2. Euclidean distance tells us how strongly the cluster (if vectors are similar in direction, but different in magnitude)

# B) The lines going from words on one side of the pole to the words on the other end shoudl be parallel (if they represent a semantic scale)
# -> Parrallelity means that the angle of these lines in the vectors space should be similar
# -> Can be asseses along the cosine similarity of the difference vectors between all pairs from the opposing side


# Given that the word vector space encodes numerous latent dimensions, these are not to be seen as absolute statements
# But in relative terms, these metrics should help to asses which scale is more consistent within the meaning space


# Function to collect the data for these scale consistency measures from pre-trained word vector model ###
# lo_seeds: character vector containing the seed word sfor the lower end of the scale
# hi_seeds: character vector containing the seed words for the upper end of the scale
# wordvectors: Vectors from pre-trained model with tokens/words as column names and values in rows

scale_consistency_data <- function(lo_seeds = character(NULL),
                                   hi_seeds = character(NULL),
                                   wordvectors = data.frame(NULL)) {
  
  # Function to calculate cosine similarity of two numerical vectors
  cosine_similarity <- function(a, b) {
    sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))}
  
  # Function to compute Euclidean distance between two vectors
  euclidean_distance <- function(vec1, vec2) {
    sqrt(sum((vec1 - vec2)^2))}
  
  # Build data frame with all possible combinations of seed words
  # indicating from which side of the scale they come from
  all_words <- unique(c(lo_seeds, hi_seeds))
  df <- expand.grid(a = all_words, b = all_words) %>% # All possible combinations of seed terms
    subset(a != b) %>% # Exclcude combinations of a seed term with itself
    mutate(lo_a = (a %in% lo_seeds), # Identify the seed pole the words come from
           lo_b = (b %in% lo_seeds),
           mixed_pair = (lo_a != lo_b),
           lo_pair = (lo_a & lo_b),
           hi_pair = (!lo_a & !lo_b)) %>% 
    # select(-c(lo_a, lo_b)) %>% 
    mutate(pair.sim = NA, # Columns to store the target values
           pair.dist = NA,
           vec_a = NA,
           vec_b = NA,
           diff.vec = NA)
  
  # Get cosine similarity and difference vector for each word pair
  for (i in 1:nrow(df)) {
    
    # Get and store vectors of each word in the pair from pre-trained model
    vec_a <- wordvectors[[df$a[i]]]
    vec_b <- wordvectors[[df$b[i]]]
    
    df$vec_a[i] <- list(vec_a)
    df$vec_b[i] <- list(vec_b)
    
    # Calculate cosine similarity of the two vectors
    df$pair.sim[i] = cosine_similarity(vec_a, vec_b)
    
    # Calculate Euclidean distance of the two vectors
    df$pair.dist[i] = euclidean_distance(vec_a, vec_b)    
    
    # Store difference vector (line combining both words in the vector space)
    diff_vec <- vec_b - vec_a
    df$diff.vec[i] <- list(diff_vec)
    
  }
  
  # Clean up
  rm(vec_a, vec_b, diff_vec)
  
  # Pair type variable
  df$pair.type <- ifelse(df$mixed_pair, "Seeds from opposing poles",
                         ifelse(df$lo_pair, "Seeds on lower pole",
                                "Seeds on upper pole"))
  df$pair.type <- factor(df$pair.type, levels = c("Seeds on lower pole",
                                                  "Seeds from opposing poles",
                                                  "Seeds on upper pole"))
  
  # Return the resulting data
  return(df)
  
}


# Get values for the GTI scale

dict <- dictionary(file = paste0(data_path, 'external_data/GTI_keywords.yml'))
friendly_vocab = dict$seedwords$friendly
hostile_vocab = dict$seedwords$hostile

df.gti <- scale_consistency_data(lo_seeds = friendly_vocab,
                                 hi_seeds = hostile_vocab,
                                 wordvectors = glove) %>% 
  mutate(scale = "GTI (Hostility)")



# Get Values for the Friend-Foe scale 

friend_vocab = c("friend", "partner",  "ally",  "peace", "peaceful",   "friendly", "cooperative")
foe_vocab =    c("foe",    "opponent", "enemy", "war",   "aggressive", "hostile",  "uncooperative")

df.ff <- scale_consistency_data(lo_seeds = friend_vocab,
                                 hi_seeds = foe_vocab,
                                 wordvectors = glove) %>% 
  mutate(scale = "Friend-Foe")


# Get Values for the Coop/Conflit scale 
coop_vocab <- c("cooperation", "agreement",    "support",    "collaboration", "unity")
conf_vocab <- c("conflict",    "disagreement", "opposition", "confrontation", "hostility")

df.cc <- scale_consistency_data(lo_seeds = coop_vocab,
                                hi_seeds = conf_vocab,
                                wordvectors = glove) %>% 
  mutate(scale = "Coop-Conflict")


# Get values for a sentiment scale (benchmarking)
pos_vocab <- c("good", "positive", "exellent",   "superior", "wonderful")
neg_vocab <- c("bad",  "negative",  "terrible", "inferior",  "awful")

df.sent <- scale_consistency_data(lo_seeds = pos_vocab,
                                hi_seeds = neg_vocab,
                                wordvectors = glove) %>% 
  mutate(scale = "Sentiment (benchmark)")



# Assess clustering of the seed poles in the vector space

comp.df <- rbind(
  df.gti %>% select(scale, pair.type, pair.sim, pair.dist),
  df.ff %>% select(scale, pair.type, pair.sim, pair.dist),
  df.cc %>% select(scale, pair.type, pair.sim, pair.dist),
  df.sent %>% select(scale, pair.type, pair.sim, pair.dist)) %>% 
  pivot_longer(cols = 3:4, names_to = "measure", values_to = "value") %>% 
  mutate(measure = ifelse(measure == "pair.sim", "Cosine similarity", "Euclidean distance"))

pl1 <- 
  ggplot(comp.df %>% filter(measure == "Cosine similarity"), aes(x =value, y = pair.type,)) + 
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) + 
  facet_grid(. ~ scale)+
  labs(title = "Cosine similarities",
       subtitle = "Seed word pairs representing opposing poles should have lower similarities than seed word pairs representing the same pole",
       y = "",
       x ="\nAverage cosine similarity across seed-word pairs\n(with bootstrapped 95% c.i.s)")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold.italic"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black"))
pl2 <-
  ggplot(comp.df %>% filter(measure == "Euclidean distance"), aes(x =value, y = pair.type,)) + 
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) + 
  facet_grid(. ~ scale)+
  labs(title = "Euclidean distances",
       subtitle = "Seed word pairs representing opposing poles should have higher distances (= cluster less) than seed word pairs representing the same pole",
       y = "",
       x ="\nAverage Euclidean distance across seed-word pairs\n(with bootstrapped 95% c.i.s)")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold.italic"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black"))

combined_plot <- (pl1/pl2) +
  plot_annotation(
    title = "How much do the seed words supplied for each scale\ncluster in the vector space?\n",
    caption = "\nBased on comparing the word vectors from pre-trained model for each pariwise combination of seed words for each scale.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1, color = "gray40")
    )
  )
combined_plot

ggsave("./output/glove_plots/SeedWordClustering.png", combined_plot, width = 30, height = 18, units = "cm")




# Parallelity ####

# Function to calculate cosine similarity of two numerical vectors
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))}


# GTI

df <- df.gti %>% 
  filter(lo_a & !lo_b) %>% # Only directed pairs from lo to hi end of scale
  mutate(pair = paste0(a, "-", b)) %>% 
  select(pair, diff.vec) # Only the difference vectors (lines connecting the pair in the space)

# Matrix with cosine sims for each combination of vectors
gti.p <- matrix(NA, nrow = nrow(df), ncol = nrow(df))
for (i in 1:nrow(df)) {
  for (j in 1:nrow(df)) {
    gti.p[i, j] <- cosine_similarity(df$diff.vec[[i]], df$diff.vec[[j]])
  }
}

# Long form of unique combinations
pa.gti <- as.data.frame(as.table(gti.p)) %>%
  mutate(Var1 = as.numeric(Var1),
         Var2 = as.numeric(Var2)) %>% 
  filter(Var1 < Var2) %>%  # Keep only unique (upper triangular) pairs
  filter(Var1 != Var2) %>% # Remove diagonal as well
  rename(sim = Freq) %>% 
  mutate(scale = "GTI (Hostility)") %>% 
  mutate(pair1 = df$pair[Var1],
         pair2 = df$pair[Var2])

# "aids" is the culprit here ...


# Friend-Foe

df <- df.ff %>% 
  filter(lo_a & !lo_b) %>% # Only directed pairs from lo to hi end of scale
  mutate(pair = paste0(a, "-", b)) %>% 
  select(pair, diff.vec) # Only the difference vectors (lines connecting the pair in the space)

# Matrix with cosine sims for each combination of vectors
ff.p <- matrix(NA, nrow = nrow(df), ncol = nrow(df))
for (i in 1:nrow(df)) {
  for (j in 1:nrow(df)) {
    ff.p[i, j] <- cosine_similarity(df$diff.vec[[i]], df$diff.vec[[j]])
  }
}

# Long form of unique combinations
pa.ff <- as.data.frame(as.table(ff.p)) %>%
  mutate(Var1 = as.numeric(Var1),
         Var2 = as.numeric(Var2)) %>% 
  filter(Var1 < Var2) %>%  # Keep only unique (upper triangular) pairs
  filter(Var1 != Var2) %>% # Remove diagonal as well
  rename(sim = Freq) %>% 
  mutate(scale = "Friend-Foe") %>% 
  mutate(pair1 = df$pair[Var1],
         pair2 = df$pair[Var2])

# adjective-noun combinations


# Cooperation- Conflict

df <- df.cc %>% 
  filter(lo_a & !lo_b) %>% # Only directed pairs from lo to hi end of scale
  mutate(pair = paste0(a, "-", b)) %>% 
  select(pair, diff.vec) # Only the difference vectors (lines connecting the pair in the space)

# Matrix with cosine sims for each combination of vectors
cc.p <- matrix(NA, nrow = nrow(df), ncol = nrow(df))
for (i in 1:nrow(df)) {
  for (j in 1:nrow(df)) {
    cc.p[i, j] <- cosine_similarity(df$diff.vec[[i]], df$diff.vec[[j]])
  }
}

# Long form of unique combinations
pa.cc <- as.data.frame(as.table(cc.p)) %>%
  mutate(Var1 = as.numeric(Var1),
         Var2 = as.numeric(Var2)) %>% 
  filter(Var1 < Var2) %>%  # Keep only unique (upper triangular) pairs
  filter(Var1 != Var2) %>% # Remove diagonal as well
  rename(sim = Freq) %>% 
  mutate(scale = "Coop-Conflict") %>% 
  mutate(pair1 = df$pair[Var1],
         pair2 = df$pair[Var2])


# Sentiment

df <- df.sent %>% 
  filter(lo_a & !lo_b) %>% # Only directed pairs from lo to hi end of scale
  mutate(pair = paste0(a, "-", b)) %>% 
  select(pair, diff.vec) # Only the difference vectors (lines connecting the pair in the space)

# Matrix with cosine sims for each combination of vectors
sent.p <- matrix(NA, nrow = nrow(df), ncol = nrow(df))
for (i in 1:nrow(df)) {
  for (j in 1:nrow(df)) {
    sent.p[i, j] <- cosine_similarity(df$diff.vec[[i]], df$diff.vec[[j]])
  }
}

# Long form of unique combinations
pa.sent <- as.data.frame(as.table(sent.p)) %>%
  mutate(Var1 = as.numeric(Var1),
         Var2 = as.numeric(Var2)) %>% 
  filter(Var1 < Var2) %>%  # Keep only unique (upper triangular) pairs
  filter(Var1 != Var2) %>% # Remove diagonal as well
  rename(sim = Freq) %>% 
  mutate(scale = "Sentiment (benchmark)") %>% 
  mutate(pair1 = df$pair[Var1],
         pair2 = df$pair[Var2])



# Compare average parrallelity in directed pairs per scale 

df <- rbind(
  pa.gti,
  pa.ff,
  pa.cc,
  pa.sent
)



pl.pa <-
  ggplot(df, aes(x = sim, y = scale)) + 
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) + 
  # facet_grid(. ~ scale)+
  labs(title = "Average parallelity of seed word pairs from opposing poles",
       subtitle = "The more the lines connecting any word in the lower seed set to any word in the upper set\nare parallel, the more consistent is the scale in the given vector space.",
       y = "",
       x ="\nAverage cosine similarity of difference vectors between opposing seeds\n(with bootstrapped 95% c.i.s)",
       caption = "\nIf all lines would be perfectly parallel in the vector space, the cosine similarity of the difference vectors would equal 1.")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold.italic"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black"))

ggsave("./output/glove_plots/SeedWordParallelity.png", pl.pa, width = 20, height = 8, units = "cm")



# Inspect pa data qualitatively
# Which pairs / words are particularly off?


