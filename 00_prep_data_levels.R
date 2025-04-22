# create country mentions

# ToDo: change data path to nextcloud shared folder 

# prep ####
library(tidyverse)
library(magrittr)

data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/cleaned_data/"

# get data from DB: ####

source("../00_connectDB.R") # custom DB connection file in parent directory

data_paralevel <- DBI::dbReadTable(con, "all_texts") %>% 
  select(para_id = id, text_para, everything(), -doc_type) %>% 
  mutate(doc_key = str_sub(doc_key, start = 11),
         text_para = str_remove(text_para, "^o |^• |^- |^\\* |^· |^— |^– "), # EU uses weird bullets sometimes, that confuse sentence tokenizer
         text_para = ifelse(para_type == "title", str_c(text_para, "."), text_para),
         text_para = ifelse(para_type == "title", str_remove(text_para, "\\.+$"), text_para)
  ) %>% 
  filter(!str_detect(text_para, "^IP[^v]*?[0-9]$"))


data_doclevel <- DBI::dbReadTable(con, "all_meta") %>% 
  left_join(., 
            data_paralevel %>% 
              filter(para_type == "title") %>% 
              select(doc_key, title_clean = text_para),
            join_by(doc_key)
  ) %>% 
  mutate(title_long = coalesce(title_clean, title_long) %>% 
           str_remove(., "\\.+$")) %>% 
  select(doc_id, text_doc = text_full, everything(), -title_clean) %>% 
  group_by(doc_id) %>% 
  mutate(text_doc = str_c(title_long, text_doc, sep = ".\n\n") %>% 
           str_replace_all("\no |\n• |\n- |\n\\* |\n· |\n— |\n– ", "\n") # EU uses weird bullets sometimes, that confuse sentence tokenizer
  ) %>% 
  ungroup()


write_rds(data_doclevel, paste0(data_path, "data_doclevel.rds"))

dbDisconnect(con)
rm(con, data_paralevel)
gc()


# run on kalliope:
{
  # alt_paralevel <- data_doclevel %>% 
  #   tidyr::separate_rows(text_doc, sep = "\n") %>% # split at newline
  #   rename(text_para = text_doc)
  # 
  # alt_paralevel %<>%  
  #   # remove uninformative/service paragraphs (not fully exhaustive, but those are reliable):
  #   mutate(
  #     text_para = str_remove(text_para, "^IP.*? "), # IP num, doesn't capture anything about IP-address etc., as only on paragraph start
  #     text_para = str_remove(text_para, "^Brussels.*?[0-9]{4} ")
  #   )
  # alt_paralevel %<>% mutate(
  #     # remove "check against delivery" (variants) from speeches (incl typos):
  #     text_para = str_remove(text_para, 
  #                            "(?i)[[:print:]pleas]*check ag[ainstdwh delvry[:print:]]*")
  # )
  # alt_paralevel %<>% mutate(
  #     # some in different languages catched by hand:
  #     text_para = str_remove(text_para, "\\[check against\\]|\\[check as delivery\\]|\\[Só faz fé o texto proferido\\]|[:graph:]*muutokset mahdollisia puhuttaessa|[:graph:]*muutokset puhuttaessa mahdollisia|[:graph:]*Seul le prononc? fait foi[:graph:]*|[:graph:]*Seul le prononc? fait[:graph:]*|puhuttaessa mahdollisia|mahdollisia puhuttaessa")) %>% 
  # 
  #   # remove paragraphs without any meaningful content (styles etc.)
  #   filter(., str_detect(text_para, "PRV_ENDREF|Font Definitions|Style Definitions|view_mode|\\[if gte mso 9\\]", negate = T)) %>% 
  #   
  #   # remove html etc. from paragraphs with meaningful contents: 
  #   mutate(text_para = str_remove_all(text_para, '<br>|</b>|<b>|<sup>|</sup>|<sub>|</sub>|<p align=\"center\">|\\[if !supportLists\\]|\\[endif\\]|body p \\{ text-align: justify; \\}|<!-- PRV_REF[^>]*-->')) %>% 
  #   
  #   # some more basic cleaning:
  #   mutate(text_para = str_squish(text_para), # remove excessive whitespaces
  #          n_chars_para = str_length(text_para)) %>% # count characters per paragraph 
  #   filter(., text_para != "") %>% # remove empty paragraphs
  #   filter(., str_detect(text_para, "\\w"))  # remove paragraphs without any word characters
  }

alt_paralevel <- read_rds("M:/user/schroeder/alt_paralevel.rds")

alt_paralevel %<>% 
  filter(., str_detect(text_para, "(?i)check against delivery", negate = T)) %>% 
  
  group_by(doc_key) %>% 
  mutate(doc_pos = row_number()) %>% 
  ungroup()

alt_paralevel %<>% 
  mutate(text_para = ifelse(doc_pos == 1, 
                            str_remove(text_para, "\\.+$"), 
                            text_para),
         title_long = str_remove(title_long, "\\.+$"),
         n_chars_para = str_length(text_para)
  ) %>% 
  group_by(doc_id) %>% 
  mutate(n_chars_doc = sum(n_chars_para),
         n_paras = n()) %>% 
  ungroup()





# join by doc_key & text: (identical paragraphs in same text? probably..)
#...

##### paste alt_paralevel together (new id with order as in text) ####
# -> check if identical to original text

# if so: paste together, with {start-id} before new para
# assign any marker in sent
# if marker not at start of sent: also assign marker-1
# if no marker: assign last marker
# data_with_marker %<>%  mutate(
# p_min = case_when(str_detect(text_para, "^\\{start-\d+\\}") ~ str_extract(text_para, "(?<=\\{start-)\d+"),
#                   str_detect(text_para, "\\{start-\d+\\}") ~ str_extract(text_para, "(?<=\\{start-)\d+")-1,
#                   TRUE ~ lag(p_min)
#                   ) %>% 
#   as.numeric(),
# p_max = max(str_extract_all(text_para, "(?<=\\{start-)\d+") %>% as.numeric())
# ) %>% 
# 
# # remove marker:
# str_remove_all(text_para, "\\{start-\d+\\}")
# 
# data_doclevel %>% head() %>% distinct(doc_key) %>% left_join(., data_paralevel, join_by(doc_key))
# 
# cf <- 
#   left_join(alt_paralevel %>% count(doc_key, name = "n_alt"), 
#           data_paralevel %>% count(doc_key, name = "n_para"), 
#           join_by(doc_key)) %>% 
#   mutate(diff = n_alt - n_para) %>%
#   filter(diff != 0) %>% 
#   arrange(diff)

# all alt where text matches original 1-to-1: ####
match <- right_join(alt_paralevel %>% 
                      mutate(alt_id = row_number()-1) %>% 
                      rename(doc_pos_total = doc_pos), 
                    data_paralevel %>% select(-n_chars_para), 
                    join_by(text_para, doc_key),
                    multiple = "last"
) %>% 
  distinct(alt_id, .keep_all = T) %>% filter(!is.na(alt_id)) %>% 
  distinct(para_id, .keep_all = T) %>% filter(!is.na(para_id))

todo <- 1
i = 1
still_todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match$alt_id)))

while (still_todo != todo)  {
  
  todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match$alt_id)))
  i = i+1
  
  match <- 
    bind_rows(
      match,
      right_join(alt_paralevel %>% 
                   mutate(alt_id = row_number()-1) %>% 
                   rename(doc_pos_total = doc_pos) %>% 
                   filter(!alt_id %in% unique(match$alt_id)), 
                 data_paralevel %>% 
                   select(-n_chars_para) %>% 
                   filter(!para_id %in% unique(match$para_id)), 
                 join_by(text_para, doc_key),
                 multiple = "last"
      ) %>% 
        distinct(alt_id, .keep_all = T) %>% filter(!is.na(alt_id)) %>% 
        distinct(para_id, .keep_all = T) %>% filter(!is.na(para_id))
    )
  
  still_todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match$alt_id)))
  
  print(paste("Iteration", i, "- Still not matched:", still_todo))
}

# all original that don't have match in alt -> need to find match
#test_contr <- right_join(alt_paralevel, data_paralevel, join_by(text_para, doc_key)) %>% filter(is.na(doc_id))
nomatch <- data_paralevel %>% filter(!para_id %in% unique(match$para_id))

# nomatch <- alt_paralevel %>% 
#   mutate(alt_id = row_number()-1) %>% 
#   filter(!alt_id %in% (test %>% pull(alt_id)))


#fixit <- nomatch %>% select(text_para, doc_key, doc_pos, para_id, para_type)

### fix overcleaning issues:
pastein_titles <- nomatch %>% filter(para_type == "title") %>% 
  mutate(n_chars_para = str_length(text_para), lang = cld2::detect_language(text_para)) %>%  # paste in at beginning of text_full
  left_join(data_doclevel %>% select(any_of(names(match))), join_by(doc_key))

pastein_quotes <- nomatch %>% filter(para_type == "quote") %>% 
  mutate(n_chars_para = str_length(text_para), lang = cld2::detect_language(text_para)) %>%  # paste in at end of text_full
  left_join(data_doclevel %>% select(any_of(names(match))), join_by(doc_key))


# join  doclevelstuff
match <- bind_rows(pastein_titles, match, pastein_quotes)

# rest: paste together what belongs together, then join again:
fixit <- nomatch %>% 
  #  select(text_para, doc_key, doc_pos, para_id, para_type) %>% 
  filter(!para_type %in% c("title", "quote")) %>% 
  group_by(doc_key) %>% 
  arrange(para_id) %>%  # Ensure data is sorted by para_id
  mutate(
    group = cumsum(c(TRUE, diff(para_id) != 1))
  ) %>%
  group_by(group, doc_key) %>%
  summarise(
    para_id = paste(para_id, collapse = ","),
    # ifelse(
    # first(para_id) != last(para_id),
    # paste(first(para_id), last(para_id), sep = "-"),
    # as.character(first(para_id))),
    doc_pos = paste(doc_pos, collapse = ","),
    # ifelse(
    # first(doc_pos) != last(doc_pos),
    # paste(first(doc_pos), last(doc_pos), sep = "-"),
    # as.character(first(doc_pos))),
    text_para = paste(text_para, collapse = " "),
    para_type = {
      # Create a sorted table of para_type by decreasing frequency
      type_table <- sort(table(para_type), decreasing = TRUE)
      paste(names(type_table), collapse = ", ")
    },
    CAP_code = ifelse(length(unique(CAP_code)) == 1, first(CAP_code), NA),
    CAP_name = ifelse(length(unique(CAP_name)) == 1, first(CAP_name), NA)
  ) %>%
  ungroup() %>% 
  mutate(n_chars_para = str_length(text_para), 
         lang = cld2::detect_language(text_para)
  ) %>% 
  select(-group)


# use determine order by doc_pos instead of doc_id to distinguish different types better...
multi_types <- fixit %>% filter(str_detect(para_type, ","))

fixit_bytype <- nomatch %>% 
  #  select(text_para, doc_key, doc_pos, para_id, para_type) %>% 
  filter(!para_type %in% c("title", "quote")) %>% 
  group_by(doc_key) %>% 
  arrange(doc_pos) %>%  # Ensure data is sorted by para_id
  mutate(
    group = cumsum(c(TRUE, diff(doc_pos) != 1)) # paste together paragraphs that may belong together
  ) %>%
  group_by(group, doc_key) %>%
  summarise(
    para_id = paste(para_id, collapse = ","),
    # ifelse(
    #   first(para_id) != last(para_id),
    #   paste(first(para_id), last(para_id), sep = "-"),
    #   as.character(first(para_id))),
    doc_pos = paste(doc_pos, collapse = ","),
    # ifelse(
    #   first(doc_pos) != last(doc_pos),
    #   paste(first(doc_pos), last(doc_pos), sep = "-"),
    #   as.character(first(doc_pos))),
    text_para = paste(text_para, collapse = " "),
    para_type = {
      # Create a sorted table of para_type by decreasing frequency
      type_table <- sort(table(para_type), decreasing = TRUE)
      paste(names(type_table), collapse = ", ")},
    CAP_code = ifelse(length(unique(CAP_code)) == 1, first(CAP_code), NA),
    CAP_name = ifelse(length(unique(CAP_name)) == 1, first(CAP_name), NA)
  ) %>%
  ungroup() %>% 
  mutate(n_chars_para = str_length(text_para), 
         lang = cld2::detect_language(text_para)
  ) %>% 
  select(-group)

fix <- bind_rows(fixit, fixit_bytype) %>% distinct()
rm(fixit, fixit_bytype)

# try matching again now: ####
match_remaining <- 
  right_join(alt_paralevel %>% 
               mutate(alt_id = row_number()-1) %>% 
               rename(doc_pos_total = doc_pos) %>% 
               select(-c(n_chars_para)), 
             fix, 
             join_by(text_para, doc_key),
             multiple = "last"
  ) %>% 
  distinct(alt_id, .keep_all = T) %>% filter(!is.na(alt_id)) %>% 
  distinct(para_id, .keep_all = T) %>% filter(!is.na(para_id))

todo <- 1
i = 1
still_todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match_remaining$alt_id)))

while (still_todo != todo)  {
  
  todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match_remaining$alt_id)))
  i = i+1
  
  match_remaining <- 
    bind_rows(
      match_remaining,
      right_join(alt_paralevel %>% 
                   mutate(alt_id = row_number()-1) %>% 
                   rename(doc_pos_total = doc_pos) %>% 
                   select(-c(n_chars_para)) %>% 
                   filter(!alt_id %in% unique(match_remaining$alt_id)), 
                 fix %>% 
                   filter(!para_id %in% unique(match_remaining$para_id)), 
                 join_by(text_para, doc_key),
                 multiple = "last"
      ) %>% 
        distinct(alt_id, .keep_all = T) %>% filter(!is.na(alt_id)) %>% 
        distinct(para_id, .keep_all = T) %>% filter(!is.na(para_id))
    )
  
  still_todo <- nrow(alt_paralevel %>% mutate(alt_id = row_number()-1) %>% filter(!alt_id %in% unique(match_remaining$alt_id)))
  
  print(paste("Iteration", i, "- Still not matched:", still_todo))
}

fixed <- bind_rows(match %>% mutate(para_id = as.character(para_id), doc_pos = as.character(doc_pos)), match_remaining)


# fixed <- left_join(nomatch %>% select(-n_chars_para), fix, join_by(text_para, doc_key), suffix = c( "_new", "_old"), multiple = "last") #%>% filter(!is.na(para_id)) 
still_nomatch <- alt_paralevel %>% 
  mutate(alt_id = row_number()-1) %>% 
  filter(!alt_id %in% unique(fixed$alt_id))


matched_para_ids <- 
  str_c(fixed$para_id, collapse = ",") %>% 
  str_split_1(",") %>% 
  as.numeric() %>% 
  unique()

# select(doc_key, doc_pos para_id, text_para) %>% 
# mutate(para_id_min = as.numeric(str_extract(para_id, "^\\d+")),
#                            para_id_max = as.numeric(str_extract(para_id, "\\d+$")))
#  filter(para_id_min != para_id_max)


# para_ids <- c()
# for (i in 1:nrow(matched_para_ids)) {
#   para_ids <- c(para_ids, 
#                 seq(matched_para_ids$para_id_min[i], matched_para_ids$para_id_max[i]))
# }

still_notmatched <- data_paralevel %>% 
  filter(!para_id %in% matched_para_ids)
# fixed[duplicated(fixed$para_id),] %>% mutate(diff = alt_id - as.numeric(str_extract(para_id, "\\d+")))


### WHATS LEFT NOW IS TO MUCH EFORT JUST TO GET PARAGRAPH TYPES RIGHT... 
# just paste together matched, fixed, and remaining paragraphs (new split)  
all_paras <- 
  bind_rows(
    match,
    still_nomatch
  ) %>% 
  arrange(doc_id) %>% 
  mutate(para_id_old = para_id,
         lang = ifelse(is.na(lang), cld2::detect_language(text_para), lang),
         para_id = row_number()-1) %>% 
  group_by(doc_id) %>% 
  mutate(n_paras = n(),
         n_chars_doc = sum(n_chars_para)) %>% 
  ungroup()

write_rds(all_paras, paste0(data_path, "data_paralevel.rds"))



# tokenize sentences ####

data_sentlevel <-
  data_doclevel %>% 
  select(id = doc_id, text = text_doc) %>% 
  corpus() %>% 
  tokenize_sentence() %>% 
  tibble(text_sent = .) %>% 
  bind_cols(., data_doclevel %>% 
              select(-c(text_doc, title_short, title_long, n_paras, lang_tag, main_lang_doc))
              ) %>% 
  unnest(cols = c(text_sent)) %>% 
  mutate(sentence_id = row_number()-1,
         lang_sent = cld2::detect_language(text_sent),
         n_chars_sent = str_length(text_sent)) %>% 
  group_by(doc_id) %>% 
  mutate(n_sentence_doc = n()) %>% 
  ungroup()


write_rds(data_sentlevel, paste0(data_path, "data_sentlevel.rds"))


# prepare for CAP codings: ####
remaining_CAP <- read_rds(paste0(data_path, "data_paralevel.rds")) %>% filter(is.na(CAP_code)) %>% distinct(text_para, .keep_all = T) %>% select(id = para_id, text = text_para)
write_csv(remaining_CAP, "../data/remaining_CAP.csv")

CAP_remaining_results <- read.csv("~/OneDrive - Hertie School/WZB/data/results_remaining_CAP_HikVKPpOzl_with_pred.csv")
# CAP_remaining_softmax <- read.csv("~/OneDrive - Hertie School/WZB/data/softmax_remaining_CAP_HikVKPpOzl_softmax.csv") # top3 candidate labels with prob

data_paralevel <- 
  bind_rows(
    data_paralevel %>% 
    filter(!is.na(CAP_code)),
    data_paralevel %>%
      filter(is.na(CAP_code)) %>% 
      select(-c(CAP_code, CAP_name)) %>% 
      left_join(., CAP_remaining_results %>% select(-id, CAP_code = major_topic_pred, CAP_name = major_topic_pred_name), 
                join_by(text_para == text))
      ) %>% 
  arrange(para_id)

write_rds(data_paralevel, paste0(data_path, "data_paralevel.rds"))

CMs <-
  left_join(read_rds(paste0(data_path, "../CountryMentions/allCMs_paralevel.rds")),
            read_rds(paste0(data_path, "data_paralevel.rds")) %>% select(para_id, code = CAP_code, name = CAP_name),
            join_by(para_id)) %>% 
  mutate(CAP_code = code, CAP_name = name) %>% 
  select(-c(code, name))

write_rds(CMs, paste0(data_path, "../CountryMentions/allCMs_paralevel.rds"))
