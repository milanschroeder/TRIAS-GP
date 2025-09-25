library(tidyverse)
library(newsmap)
library(magrittr)



# fix newsmap dictionary:
test <- list(newsmap::data_dictionary_newsmap_en) %>% flatten() %>% unlist()

test <- tibble(names(test), test)
# xlsx::write.xlsx(test, "./data/country_names_toCheck.xlsx")

### qualitative inspection....


test <- xlsx::read.xlsx("./data/country_names_toCheck.xlsx", sheetIndex = 2) %>% 
  mutate(iso2c = str_extract(entity, "[A-Z]{2}(?=[0-9]?$)"),
         term = str_remove(term, "\\*"),
         regex = case_when(
           !is.na(regex) ~ regex,
           !is.na(full) ~ str_c("\\\\b", term, "\\\\b"),
           !is.na(partly) ~ term,
           T ~ NA
         ),
         regex_capture_full = case_when(
           !is.na(regex_full) ~ regex_full,
           !is.na(full) ~ str_c("\\\\b", term, "\\\\b"),
           !is.na(partly) ~ str_c("\\\\w*", term, "\\\\w*"),
           T ~ NA
         )) %>% 
           filter(!is.na(regex)) %>% 
  group_by(iso2c) %>% 
  summarise(regex = str_c("(?i)", str_c(regex, collapse = "|")),
            regex_capture_full = str_c("(?i)", str_c(regex_capture_full, collapse = "|"))) %>% 
  ungroup()
  


  # add further regexes from countrycode, without check them ######
countrynames <- 
  countrycode::codelist %>% 
  mutate(iso2c = if_else(is.na(iso2c), country.name.en, iso2c)) %>% 
  filter(!iso2c %in% test$iso2c &
         ! country.name.en %>% str_to_lower() 
           %in% c(unlist(newsmap::data_dictionary_newsmap_en %>% flatten()) %>% str_remove_all(., "\\*"))
) %>% select(c(regex = country.name.en.regex, iso2c))
# xlsx::write.xlsx(countrynames, "./data/cc_country_names_toCheck.xlsx")

# another qualitative inspection....

# add regexes from countrycodes after checks: ####
  test %<>%  
  bind_rows(., xlsx::read.xlsx("./data/cc_country_names_toCheck.xlsx", sheetIndex = 2, colIndex = 2:4))

 
   write_rds(test, "./data/country_dictionary.rds")
  
## add countrynames for all that are not in newsmap (?)


