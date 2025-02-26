#########################################################################
# Project:  TRIAS - Geopolitics
# Tasks:    Multivariate,c ross-sectional analysis of how frequently 
#           the Commission speaks about types of countries over time
# Author:   @ChRauh (26.02.2025)
#########################################################################

# Packages #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(countrycode)
library(grid)
library(gridExtra)
library(gtable)
library(patchwork)
library(ggtext)


# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP



# Document level data ####

# Load and filter (should be equvalently done for all other data)
docs <- read_rds(paste0(data_path, "cleaned_data/data_doclevel.rds")) %>% 
  # Language filter
  filter(main_lang_doc == "en") %>%
  # Doc type filter
  filter(doc_type %in% c("Country insights", "Daily news", "News", "Press release", "Read-out", "Speech", "Statement")) %>% 
  mutate(doc_type = ifelse(doc_type == "Daily news", "News", doc_type)) %>% 
  mutate(doc_type = factor(doc_type, levels = c("Press release", "Speech", "News", "Statement", "Read-out", "Country insights"))) %>% 
  # Year var
  mutate(year = str_extract(date, "^[0-9]{4}"))

# Data descriptives

doc_types <- docs %>% 
  group_by(doc_type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  rename(`Doc type` = 1,
         `Freq.` = 2)
doc_type_colors <- c("#1f78b4", "#33a02c", "#a6cee3", "#b2df8a", "#fd8d3c", "#fdbe85")
doc_type_table <- tableGrob(doc_types, rows = NULL)
for (i in seq_len(nrow(doc_types))) {
  bg <- gpar(fill = doc_type_colors[i], col = "black")  # Set fill and border color
  doc_type_table$grobs[doc_type_table$layout$t == i + 1] <- lapply(doc_type_table$grobs[doc_type_table$layout$t == i + 1], 
                                                 function(g) { g$gp <- bg; g })
}
grid.draw(doc_type_table)

pl.annual <- docs %>% 
  group_by(year, doc_type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = count, fill = fct_rev(doc_type))) +
  geom_col(width = .7) +
  scale_fill_manual(values = rev(doc_type_colors)) +
  labs(x = "", y= "", title = "Document coverage in European Commission public communication corpus")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color ="black", size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5),
        plot.title = element_text(face = "bold", size = 14))

pl.annual + annotation_custom(grob = doc_type_table, 
                              xmin = 1985, xmax = 1995, 
                              ymin = 2000, ymax = 3000)

comp.pl <- pl.annual + annotation_custom(grob = doc_type_table, xmin = 5, xmax = 5 , ymin = 2000, ymax = 2900)
ggsave("./output/descriptive_plots/CorpusDescription.png", comp.pl, height = 15, width = 26, units = "cm")

rm(comp.pl, annual, pl.annual, bg, doc_types, doc_type_table, doc_type_colors, i)



# Country mentions - doc level ####

cm <- read_rds(paste0(data_path, "CountryMentions/allCMs_doclevel.rds")) %>% 
  filter(doc_id %in% docs$doc_id) %>% # Filter as above
  mutate(year = str_extract(date, "^[0-9]{4}") %>% as.numeric()) %>% 
  select(c(doc_id, year, BI:ncol(.))) %>% # Only few meta and indvidual country indicators
  pivot_longer(cols = 3:ncol(.), names_to = "iso2c", values_to = "mentions") # Easier to handle


# External country/year panel ####
  
cp <- read_rds(paste0(data_path, "external_data/CountryYearPanelComplete.rds")) 

# Panel includes only countries that appear in one of the big IR data states (= official states at some point)
length(unique(cp$iso2c)) # 213
# Newsmap dictionary is somewhat broader
length(unique(cm$iso2c)) # 241
# I reduce the country mentions to the cp sample to ensure consistency throughout - MILAN please check
cm <- cm %>% 
  filter(iso2c %in% unique(cp$iso2c))



# Describe salience of foreign countries over time ####
# Share of docs (per year) that mention a country at least once as the most straightforward measure

# Only non-eu countries
cm.foreign <- cm %>% 
  left_join(cp %>% select(year, iso2c, eu_member), by = c("year", "iso2c")) %>% # get EU membership (time sensitive)
  filter(!eu_member) %>% # Filter
  select(-eu_member)


# Share of docs with country
shares <-
  cm.foreign %>% 
  mutate(us = (iso2c == "US" & mentions > 0), # Single out US + BRICS
         br = (iso2c == "BR" & mentions > 0),
         ru = (iso2c == "RU" & mentions > 0),
         `in` = (iso2c == "IN" & mentions > 0),
         cn = (iso2c == "CN" & mentions > 0),
         za = (iso2c == "ZA" & mentions > 0)) %>% 
  group_by(doc_id, year) %>% 
  summarise(mentions = sum(mentions),
            us = sum(us),
            br = sum(br),
            ru = sum(ru),
            `in` = sum(`in`),
            cn = sum(cn),
            za = sum(za)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(fc = mean(as.logical(mentions)), # Anuual shares
            us = mean(as.logical(us)),
            br = mean(as.logical(br)),
            ru = mean(as.logical(ru)),
            `in` = mean(as.logical(`in`)),
            cn = mean(as.logical(cn)),
            za = mean(as.logical(za))) %>% 
  ungroup()
  
pl.all <- 
  ggplot(shares, aes(x = year, y = fc)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = fc), color = "blue", linewidth = 1) +  # Line from 0 to point
  geom_point(size = 4, color = "blue") +  # Circular point
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "All foreign countries", x = "Years", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.us <- 
  ggplot(shares, aes(x = year, y = us)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = us), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) +  
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "United States", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.br <- 
  ggplot(shares, aes(x = year, y = br)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = br), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Brazil", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
  
pl.ru <- 
  ggplot(shares, aes(x = year, y = ru)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = ru), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Russia", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


pl.ind <- 
  ggplot(shares, aes(x = year, y = `in`)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = `in`), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "India", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.cn <- 
  ggplot(shares, aes(x = year, y = cn)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = cn), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "China", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

pl.za <- 
  ggplot(shares, aes(x = year, y = za)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = za), color = "blue", linewidth = .5) +  # Line from 0 to point
  geom_point(size = 1, color = "blue") +  
  geom_point(aes(y = fc), size = 1, color = "grey", alpha = .5) + 
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.0, 0))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "South Africa", x = "", y = "")+
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 10, hjust = .5),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Lower panel - US + brics
pl.lower <- (pl.us+pl.br)/(pl.ru+pl.ind)/(pl.cn+pl.za)


# Combined plot
pl <- (pl.all+pl.lower)+
  # plot_layout(heights = c(.7,1))+
  plot_annotation(title = "How often the European Commission publicly communicates about foreign countries",
                  subtitle = "Share of public communication documents mentioning non-EU states\n",
                  caption = "Based on all country insights, daily news, press releases, read-outs, statements, and Commissioner speeches the Commission has published 1985-2023.",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5, color = "blue", face = "bold")))  

ggsave("./output/descriptive_plots/ForeignCountrySalience_OverTime.png", pl, height = 20, width = 36, units = "cm")

# Clean up
rm(list = ls(pattern = "^pl"), shares)
gc()


# Add distance to country data
# Military spendig total 

# full country year panel
# Fill shares
# Merge complete cp
# plot complete case over time to determine range

# Develop (and write down hypotheses)

# Estimate full model - lm with clustered se
# Estimate lm per year standardized 
# Coeff, cis, r2 into joitn data frame
# MArk significance

# Plot coeff with pointrange over time 


