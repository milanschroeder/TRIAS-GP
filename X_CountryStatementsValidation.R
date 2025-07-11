#################################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Assess human-coded country statements
# Author:   @ChRauh (11.07.2025)
#################################################################################

# Packages
library(tidyverse)
library(ggdist)
library(ggridges)
library(patchwork)
library(GGally)
library(ggcorrplot)



# Load and prep data ####

# Overlap sample for intercoder reliability - THAT HOLDS A FUNCTION
# overlap <- read_rds("./data/CountryStatements-HumanCodedData/overlap_cases.rds")
# files <- list.files(path = "./data/CountryStatements-HumanCodedData/", pattern = ".csv")

df <- rbind(
  read_csv("./data/CountryStatements-HumanCodedData/validation_sample_coder1.csv") %>% mutate(coder = "Coder1"),
  read_csv("./data/CountryStatements-HumanCodedData/validation_sample_coder2.csv") %>% mutate(coder = "Coder2"),
  read_csv("./data/CountryStatements-HumanCodedData/validation_sample_coder3.csv") %>% mutate(coder = "Coder3"),
  read_csv("./data/CountryStatements-HumanCodedData/validation_sample_check_Milan.csv") %>% mutate(coder = "Coder4"),
  read_csv("./data/CountryStatements-HumanCodedData/validation_sample_check_Christian.csv") %>% mutate(coder = "Coder5")
  )

# Identify and mark overlap cases

ov_ids <- df %>% select(id) %>% group_by(id) %>% summarise(count = n())

table(ov_ids$count) # One seems to have been duplicated, take care of this below
ov_ids %>% filter(count == 6) # 1695234

ov_ids <- ov_ids %>% ungroup() %>% filter(count >= 5) %>% select(id) # n = 160


df$overlap <- (df$id %in% ov_ids$id)
sum(df$overlap) # 801

help <- df %>% filter(id == 1695234)
table(help$coder) 
help[help$coder == "Coder3",] # Coder 3 saw this case twice (but is consistent in its rating)

# We remove overlap marker for one of these obs
df$overlap[which(df$id == 1695234 & df$coder == "Coder3")[1]] <- FALSE

rm(help)
rm(ov_ids)


##############################
# Intercoder reliability #####


# Long perspective
ic <- 
  df %>% 
  filter(overlap) %>% 
  select(-overlap) %>% 
  mutate(
    # Coding as factor var
    f_label = factor(ic$label, levels = c("Very friendly", "Rather friendly", "Neutral", "Rather adversarial", "Very adversarial")),
    # Coding as numerical var (centered on 0 = neutral)
    n_label = f_label %>% as.numeric() - 3
    )

pl.distr <-
  ggplot(ic, aes(x = f_label, fill = coder)) +
  geom_bar(position = position_dodge(), color = "black")+
  scale_fill_manual(values = c("#1b9e77",  "#d95f02",  "#7570b3","#e7298a","#66a61e"))+
  labs(title = "Distribution of sentence ratings",
       y = "Frequency",
       x = "\nLabel chosen by coder")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold.italic"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(face = "bold"))

pl.means <- 
  ggplot(ic, aes(x = n_label, y = coder, color = coder))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot, size = 2.5)+
  scale_color_manual(values = c("#1b9e77",  "#d95f02",  "#7570b3","#e7298a","#66a61e"))+
  labs(title = "Average sentence rating",
       y = "",
       x = "\nAverage rating on 5-point scale\nfrom \"Rather friendly\" (-2) to \"Rather adversarial\" (+2)")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold.italic"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(face = "bold", size = 14))
  
pl.distr + pl.means+
  plot_layout(widths = c(2, 1))+
  plot_annotation(
    title = 'Coder responses in overlap sample',
    subtitle = "160 sentences coded by all 5 humans; 800 codings in total",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = .5),
                  plot.subtitle = element_text(size = 14, hjust = .5)))

ggsave("./output/CountryStatementsValidation/CoderRatingsInOverlap.png", width = 32, height = 16, units = "cm")



# Wide perspective 

icw <- ic %>% 
  select(id, n_label, coder) %>% 
  # Wide format - taking numerical variant of coder ratings
  pivot_wider(id_cols = "id", names_from = "coder", values_from = "n_label") %>% 
  # Add full text again
  left_join(ic %>% select(id, text, country) %>% unique(), by = "id") %>% 
  relocate(id, text, country) %>% 
  # Variation across coders per sentence
  rowwise() %>% 
  mutate(sd_coders = sd(c_across(starts_with("Coder")))) %>% 
  ungroup() %>% 
  mutate(full_agree = sd_coders == 0) %>% # All coders rated sentence equally on 5-point scale
  # Reduced 3-point scale
  mutate(
    Coder1_redux = case_when(
      Coder1 %in% c(-2, -1) ~ -1, 
      Coder1 == 0 ~  0, 
      Coder1 %in% c(1, 2)   ~  1),
    Coder2_redux = case_when(
      Coder2 %in% c(-2, -1) ~ -1, 
      Coder2 == 0 ~  0, 
      Coder2 %in% c(1, 2)   ~  1),
    Coder3_redux = case_when(
      Coder3 %in% c(-2, -1) ~ -1, 
      Coder3 == 0 ~  0, 
      Coder3 %in% c(1, 2)   ~  1),
    Coder4_redux = case_when(
      Coder4 %in% c(-2, -1) ~ -1, 
      Coder4 == 0 ~  0, 
      Coder4 %in% c(1, 2)   ~  1),
    Coder5_redux = case_when(
      Coder5 %in% c(-2, -1) ~ -1, 
      Coder5 == 0 ~  0, 
      Coder5 %in% c(1, 2)   ~  1),
    ) %>% 
  rowwise() %>% 
  mutate(sd_coders_redux = sd(c_across(ends_with("_redux")))) %>% 
  ungroup() %>% 
  mutate(redux_agree = sd_coders_redux == 0)  %>% # All coders rated sentence equally on 3-point scale
  # Reduced 2-point scale (seprating adversarial from neutral & friendly)
  mutate(
    Coder1_adver = ifelse(Coder1 > 0, 1, 0),
    Coder2_adver = ifelse(Coder2 > 0, 1, 0),
    Coder3_adver = ifelse(Coder3 > 0, 1, 0),
    Coder4_adver = ifelse(Coder4 > 0, 1, 0),
    Coder5_adver = ifelse(Coder5 > 0, 1, 0)
  ) %>%  
  rowwise() %>% 
  mutate(sd_coders_adver = sd(c_across(ends_with("_adver")))) %>% 
  ungroup() %>% 
  mutate(adver_agree = sd_coders_adver == 0) # All coders rated sentence equally on 2-point scale
  

# Percentage agreement 

pl.perc <-
  data.frame(
  scale = c("5-point", "3-point", "2-point"),
  agree = c(sum(icw$full_agree)/nrow(icw), sum(icw$redux_agree)/nrow(icw), sum(icw$adver_agree)/nrow(icw))
  ) %>% 
  mutate(text = paste0(round(agree*100,1), "%")) %>% 
  ggplot(aes(x = agree, y = scale))+
  geom_col(fill = "steelblue") +
  geom_text(aes(label = text), hjust = 1.1, color = "white", fontface = "bold") +  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title= "Percentage agreement of coders across different scale aggregations",
       subtitle = "Five coders rated the same 160 country staments each.\nOriginal 5-point scale: \"Very friendly\", \"Rather friendly\", \"Neutral\", \"Rather adversarial\", \"Very adversarial\", and \"Very friendly\".\nThe 3-point scale reduces this to \"Friendly\", \"Neutral\", and \"Adversarial\".\nThe 2-point scale distinguishes (\"Positive\ AND \"Neutral\") from \"Adversarial\".",
       x = "\nShare of cases in which coders agree\n(on the respective scale)",
       y = "")+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12))

pl.perc
ggsave("./output/CountryStatementsValidation/IntercoderPercentageAgreement.png", width = 30, height = 16, units = "cm")



# Intercoder-correlations
# ggpairs(icw %>% select(Coder1:Coder5))

cor_5 <- 
  icw %>% 
  select(Coder1:Coder5) %>% 
  cor(use = "pairwise.complete.obs")  
pl.5 <- 
  ggcorrplot(cor_5, lab = TRUE)+
  labs(title = "Original 5-point scale")+
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = .5, face = "bold.italic"))

cor_3 <- 
  icw %>% 
  select(Coder1_redux:Coder5_redux) %>% 
  cor(use = "pairwise.complete.obs")
colnames(cor_3) <- str_remove_all(colnames(cor_3), "_redux")
rownames(cor_3) <- str_remove_all(rownames(cor_3), "_redux")
pl.3 <- 
  ggcorrplot(cor_3, lab = TRUE)+
  labs(title = "Reduced 3-point scale")+
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = .5, face = "bold.italic"))

cor_2 <- 
  icw %>% 
  select(Coder1_adver:Coder5_adver) %>% 
  cor(use = "pairwise.complete.obs")
colnames(cor_2) <- str_remove_all(colnames(cor_2), "_adver")
rownames(cor_2) <- str_remove_all(rownames(cor_2), "_adver")
pl.2 <- 
  ggcorrplot(cor_2, lab = TRUE)+
  labs(title = "Reduced 2-point scale")+
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = .5, face = "bold.italic"))


pl.cor <-
  (pl.5 + pl.3 + pl.2)+
  plot_annotation(title = "Correlations among Coders' ratings",
                  subtitle = "Five coders rated the same 160 country staments each.\nOriginal 5-point scale: \"Very friendly\", \"Rather friendly\", \"Neutral\", \"Rather adversarial\", \"Very adversarial\", and \"Very friendly\".\nThe 3-point scale reduces this to \"Friendly\", \"Neutral\", and \"Adversarial\".\nThe 2-point scale distinguishes (\"Positive\ AND \"Neutral\") from \"Adversarial\".\n",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))

pl.cor

ggsave("./output/CountryStatementsValidation/IntercoderCorrelations.png", width = 30, height = 16, units = "cm")

