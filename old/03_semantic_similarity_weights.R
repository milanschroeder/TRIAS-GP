# Extract token weigths for semantic similarity to key concepts
# Author: ChRauh

# Inputs: large_data/tokens.rds; glove.6B.300d.rds
# Output: semantic similarity weights (token-level) for various concept and respective visualizations


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v2.0.0 
library(text2vec)
library(ggwordcloud)
library(quanteda) # Quantitative Analysis of Textual Data, CRAN v3.3.1
library(readxl)

set.seed(1905)
data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/"

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


# TO DOs / Ideas
# Filter out country markers from term weights - countries should not push into any substantial direction
# Aapproach: Nesmap seed dictionaries

# Filter out country leaders from wikidata



# Pre- trained word-embedding model ####

# TO DO: Make the model part of the repo

# Using the ready-made GLOVE word embeddings trained on Wikipedia and Gigaword
# Version with 400k vocabulary and 300 dimensions

# Get it here: https://nlp.stanford.edu/projects/glove/ # ! LARGE FILE !

# I have parsed this for other purposes already, if you start from the raw file see:
# https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9

# glove.300 <- read_rds("C:/Users/chris/Downloads/glove.6B.300d.rds") # HP path
# glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # ThinkPad path
# glove.300 <- read_rds("C:/Users/rauh/Downloads/glove.6B.300d.rds") # WZB path

glove.300 <- read_rds("../LargeData/glove.6B.300d.rds")

# Clean up the vocabulary a bit (lots of rare trash in there, exclude stopwords)
vocab <- names(glove.300) %>% 
  as.data.frame() %>% 
  rename(token = 1) %>% 
  filter(str_detect(token, "[a-z]")) %>% 
  filter(nchar(token) > 1) %>% 
  filter(!(token %in% stopwords("english"))) %>% 
  filter(!str_detect(token, "\\.")) %>% 
  filter(!str_detect(token, "[0-9]")) %>% 
  filter(!str_detect(token, geo_entities %>% paste(collapse = "|"))) # Remove country/continent names - they should not affect the scale
 # filter(token %in% geo_entities) 
glove.300 <- glove.300 %>% select(vocab$token)
rm(vocab)



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



# Semantic similarity to DIGITAL terms - radically simple dictionary ####
# See respective markdown in TRIAS-Development for expanded reasoning

# Seed tokens
dp_vocab <- c("digital", "online", "computer", "internet", "algorithm")

# The average vector of these seeds in the pre.trained word embeddings
dp_vector <- glove.300 %>% 
  select(all_of(dp_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
dp_simils <-
  find_sim_wvs(dp_vector, glove.300, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% dp_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
# Export semantic similarity dictionary ####
write_rds(dp_simils, paste0(data_path, "glove_models/SemSimilWeights-DigitalitySimple.rds", compress = "gz"))


# Visualize 
pl <-
  ggplot(dp_simils %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
       aes(label = token, size = sim.target, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'digital\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Digital_Simple.png", pl, width = 24, height = 12, units = "cm")





# Semantic similarity to DIGITAL terms - intersubjective dictionary ####

# Seed tokens

# Five human coders received a list of the top-1500 terms semantically close to the radically simplified dictionary
# Each coder rated each terms as "Clearly and exclusively referring to the digital technology?" [0|1]

dp_vocab2 = read_rds("../TRIAS-paper1/large_data/SemSimilWeights-DigitalityAdvancedFreqCorrection.rds") %>% filter(seed == T) %>% pull(token)

# ... and get back their average vector in the word embedding model  
dp_vector2 <- glove.300 %>% 
  select(all_of(dp_vocab2)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
dp_simils2 <-
  find_sim_wvs(dp_vector2, glove.300, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% dp_vocab2) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
# Export semantic similarity dictionary ####
write_rds(dp_simils2, paste0(data_path, "glove_models/SemSimilWeights-DigitalityAdvanced.rds", compress = "gz"))

# Visualize 
pl <-
  ggplot(dp_simils2 %>% ungroup() %>% arrange(desc(sim.target)) %>% head(250), 
         aes(label = token, size = sim.target, color = seed)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1.5, 6), limits = c(0, NA)) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title = "Semantic similarity to vector of \'digital\' seed terms",
       subtitle = "Based on Glove.6B.300d model; Top-250 words most similar to average word vector of the seed terms (based on human coders)\nWords in red are seed terms, words in blue are \'learned\' from the pre-trained word vector model.")+
  theme_minimal()+
  theme(plot.background = element_rect(color = "white"))
ggsave("./output/glove_plots/SemanticallySimilarTerms_Digital_Advanced.png", pl, width = 24, height = 12, units = "cm")



# Semantic similarity to ECONOMY terms ####

# Seed tokens
econ_vocab <- c("economy", "economic", "markets", "trade", "business")

# The average vector of these seeds in the pre.trained word embeddings
econ_vector <- glove.300 %>% 
  select(all_of(econ_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
econ_simils <-
  find_sim_wvs(econ_vector, glove.300, top_n_res = 400000) %>% 
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
write_rds(econ_simils, paste0(data_path, "glove_models/SemSimilWeights-Economy.rds", compress = "gz"))


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
sec_vocab <- c("security", "defense", "military", "espionage", "intelligence")

# The average vector of these seeds in the pre.trained word embeddings
sec_vector <- glove.300 %>% 
  select(all_of(sec_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
sec_simils <-
  find_sim_wvs(sec_vector, glove.300, top_n_res = 400000) %>% 
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
write_rds(sec_simils, paste0(data_path, "glove_models/SemSimilWeights-Security.rds", compress = "gz"))


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




# Semantic similarity to LIBERAL RIGHTS terms ####

# Seed tokens
lib_vocab <- c("rights", "liberty", "freedom", "justice", "equality")

# The average vector of these seeds in the pre.trained word embeddings
lib_vector <- glove.300 %>% 
  select(all_of(lib_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
lib_simils <-
  find_sim_wvs(lib_vector, glove.300, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(sim.target = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% lib_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(sim.target)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector


# Export token weights
write_rds(lib_simils, paste0(data_path, "glove_models/SemSimilWeights-LibRights.rds", compress = "gz"))


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
ggsave("./output/glove_plots/SemanticallySimilarTerms_Librights.png", pl, width = 24, height = 12, units = "cm")



# Semantic scaling COOPERATION/CONFLICT ####

# Seed tokens
coop_vocab <- c("cooperation", "agreement",    "support",    "collaboration", "unity")
conf_vocab <- c("conflict",    "disagreement", "opposition", "confrontation", "hostility")

# The average vectors of these seeds in the pre.trained word embeddings
coop_vector <- glove.300 %>% 
  select(all_of(coop_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean
conf_vector <- glove.300 %>% 
  select(all_of(conf_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
coop_simils <-
  find_sim_wvs(coop_vector, glove.300, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(coop = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% coop_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(coop)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector
conf_simils <-
  find_sim_wvs(conf_vector, glove.300, top_n_res = 400000) %>% 
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
write_rds(sc ,#%>% select(token, conf_coop), 
          paste0(data_path, "glove_models/SemSimilWeights-CooperationConflict.rds", compress = "gz"))


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



# Semantic scaling COOPERATION/CONFLICT ####

# Seed tokens
friend_vocab = c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative")
foe_vocab = c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")

# The average vectors of these seeds in the pre.trained word embeddings
friend_vector <- glove.300 %>% 
  select(all_of(friend_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean
foe_vector <- glove.300 %>% 
  select(all_of(foe_vocab)) %>% # Probably needs a check whether all seeds exist in word vector
  rowMeans() # Aggregation by mean

# Extract cosine similarities to this average vector
# (word weights that would semantically pull a text towards the seed)
friend_simils <-
  find_sim_wvs(friend_vector, glove.300, top_n_res = 400000) %>% 
  as.data.frame() %>% 
  rename(friend = 1) %>% 
  rownames_to_column("token") %>% 
  mutate(seed = token %in% friend_vocab) %>% 
  filter(!(token %in% quanteda::stopwords("english"))) %>% 
  arrange(desc(friend)) %>% 
  mutate(rank.simil=row_number()) %>% 
  relocate(rank.simil) # rank by similarity to seed vector

foe_simils <-
  find_sim_wvs(foe_vector, glove.300, top_n_res = 400000) %>% 
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
write_rds(sc, paste0(data_path, "glove_models/SemSimilWeights-FriendFoe.rds", compress = "gz"))


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
  mutate(group = cut(friend_foe, 7)) %>% # Cut range of scale into n intervals
  group_by(group) %>% 
  sample_n(size = 15) %>% # n words from each interval
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

