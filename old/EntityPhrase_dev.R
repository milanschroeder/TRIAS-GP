entity_phrase <- function(text = character(NULL), 
                          entity = character(NULL), # Character/Regex (base R, perl = F) to provide lemmas for aspect identification (lower case!)
                          plot = F # Choose whether to output tree plot or aspect-phrase words
) {
  
  # Initialize spacy model 
  # Only done in the first instance, function automatically skips if already initialized
  # Note: presumes that user has locally already installed spacyr and the respective Python environment (see: https://spacyr.quanteda.io/)
  # spacy_initialize(model = "en_core_web_sm")
  
  
  # Get dependency tree(s) ####
  parsed <- spacy_parse(text, dependency = T, entity = F) %>% 
    mutate(lemma = tolower(lemma))
  
  
  # Rule 0
  # Collect children of the entity tokens (unless it starts a new subsentence)
  stop_fill0 <- custom_fill(BREAK(relation = c("conj"))) # More here?
  rule0 = tquery(lemma__R = entity,
                 label = "entity",
                 stop_fill0) 
  
  # Rule 1: Entity as subject of a verb
  # Then we collect the whole verbal phrase
  # Semantic Rationale: Actions undertaken by the entity in their entirety
  stop_fill1 <- custom_fill(BREAK(relation = c("ccomp"))) # More here?
  rule1 <- tquery(lemma__R = entity,
                  pos = "PROPN",
                  relation = "nsubj",
                  label = "entity",
                  parents(pos = c("VERB", "AUX"),
                          label = "rule1",
                          stop_fill1)
  )
  
  # Rule 2: Entity as adjective modifier of nominal subject and its verb
  # Then we collect the whole verbal phrase
  # Semantic Rationale: Actions of sub-entities associated with the entity (e.g., Chinese companies, China policy) - and what they do
  stop_fill2 <- custom_fill(BREAK(relation = c()))
  rule2 <- tquery(lemma__R = entity,
                  pos = "ADJ",
                  relation = "amod",
                  label = "entity",
                  parents(pos = c("NOUN", "PROPN"),
                          relation = "nsubj",
                          label = "rule2", 
                          parents(pos = c("VERB", "AUX"), 
                                  label = "rule2",
                                  stop_fill2)
                  )
  )
  
  
  # Rule 3: Entity's possessive relationships
  # Semantic Rationale: Entities possessed by the country (e.g., "China’s trade policy shifted") 
  # And what they potentially do
  stop_fill3 <- custom_fill(BREAK(relation = c("advcl", "relcl", "conj"))) # More here
  rule3 <- tquery(lemma__R = entity,
                  pos = c("PROPN", "NOUN"),
                  relation = "poss",
                  label = "entity",
                  parents(pos = c("NOUN", "PROPN"),
                          # relation = "nsubj",
                          label = "rule3", 
                          stop_fill3,
                          parents(pos = c("VERB"), # AUX intentionally excluded
                                  label = "rule3",
                                  req = F,               # Optional action!
                                  stop_fill3)            
                  )
  )
  
  
  # Rule 4: Entity in Apposition or Noun-Noun Constructions
  # Connects the entity to other entities it is describing or modifying (and their actions)
  # For example: "China tech exports surged", "Chinese companies expanded"
  stop_fill4 <- custom_fill(BREAK(relation = c("ccomp", "acl", "relcl", "appos"))) # More here?
  rule4 <- tquery(lemma__R = entity,
                  pos = c("PROPN", "NOUN", "ADJ"),
                  relation = c("compound", "appos", "amod", "nmod"), 
                  label = "entity",
                  parents(pos = c("NOUN", "PROPN"),
                          label = "rule4", 
                          stop_fill4,
                          parents(pos = c("VERB", "AUX"), # Optional action!
                                  label = "rule4",
                                  req = F,               
                                  stop_fill4)
                  )
  )
  
  
  # Rule 5: Passive by construction 1
  # Semantic rationale: Entity is agent of a verb similar to rule 1 above
  # Example: While the crisis has been created by Russia, the Ukraine suffers."
  stop_fill5 <- custom_fill(BREAK(relation = c()))
  rule5 <- tquery(lemma__R = entity,
                  pos = c("PROPN", "NOUN"),
                  relation = c("pobj"),
                  label = "entity",
                  parents(token = "by",
                          label = "rule5", 
                          parents(pos = c("VERB", "AUX"), 
                                  label = "rule5",
                                  req = T,
                                  stop_fill5)
                  )
  )
  
  
  # Rule 6: Passive by construction with entity adjective
  # Semantic rationale: Country is agent of a verb similar to rule 1 above
  # Example: While the crisis has been created by russian troops, the Ukraine suffers."
  stop_fill6 <- custom_fill(BREAK(relation = c()))
  rule6 <- tquery(lemma__R = entity,
                  pos = "ADJ",
                  relation = "amod",
                  label = "entity",
                  parents(pos = c("NOUN", "PRPON"),
                          label = "rule6",
                          parents(token = "by",
                                  label = "rule6", 
                                  parents(pos = c("VERB", "AUX"), 
                                          label = "rule6",
                                          req = T,
                                          stop_fill6)
                          ))
                  
  )
  
  
  # Rule 7: Noun or verbal phrase in which entity appears as prepositional object
  # Often indicates things associated with the entity (but sometimes also things the entity receives)
  stop_fill7 <- custom_fill(BREAK(relation = c("ccomp", "acl"))) # relcl - sometime with useful context
  rule7 <- tquery(lemma__R = entity,
                  pos = c("NOUN", "PROPN"),
                  relation = "pobj",
                  label = "entity",
                  parents(relation = "prep",
                          label = "rule7",
                          parents(pos = c("NOUN", "PROPN"),
                                  label = "rule7", 
                                  stop_fill7,
                                  parents(pos = c("VERB", "AUX"), # Optional action
                                          NOT(relation = "ROOT"),
                                          label = "rule7",
                                          req = F,
                                          stop_fill7)
                          ))
                  
  )
  
  # Rule 8: Noun or verbal phrase in which entity appears as modifier of prepositional object
  # Often indicates things associated with the entity (but sometimes also things the entity receives)
  stop_fill8 <- custom_fill(BREAK(relation = c()))
  rule8 <- tquery(lemma__R = entity,
                  relation = "amod",
                  label = "entity",
                  parents(pos = c("NOUN", "PROPN"),
                          relation = "pobj",
                          label = "rule8",
                          parents(relation = "prep",
                                  label = "rule8",
                                  parents(pos = c("NOUN", "PROPN"),
                                          label = "rule8", 
                                          parents(pos = c("VERB", "AUX"), # Optional action
                                                  label = "rule8",
                                                  req = F,
                                                  stop_fill8)
                                  )
                          )
                  )
                  
  )
  
  
  # Rule 9: Entity appears in noun phrase adverbial modifier
  # Similar to Rule 4, but more complex grammar
  # Example: "Sanctioned Russian-controlled entities
  stop_fill9 <- custom_fill(BREAK(relation = c()))
  rule9 <- tquery(lemma__R = entity,
                  relation = "npadvmod",
                  label = "entity",
                  parents(pos = "VERB",
                          label = "rule9",
                          parents(pos = c("NOUN", "PROPN"),
                                  label = "rule9",
                                      parents(pos = c("VERB", "AUX"), # Optional action
                                                  label = "rule9",
                                                  req = F,
                                                  stop_fill9)
                                  )
                          )
                  )
                  
  
  # Rule 10: Entity as conjunct of nominal subject and its verb
  # Then we collect the whole verbal phrase
  # Semantic Rationale: Actions attributed to the entity (and another one)
  # Example: "The EU and China are developing important co operation in these fields."
  stop_fill10 <- custom_fill(BREAK(relation = c()))
  rule10 <- tquery(lemma__R = entity,
                  pos = c("NOUN", "PROPN"),
                  relation = "conj",
                  label = "entity",
                  parents(pos = c("NOUN", "PROPN"),
                          relation = "nsubj",
                          label = "rule10", 
                          parents(pos = c("VERB", "AUX"), 
                                  label = "rule10",
                                  stop_fill10)
                  )
  )
  
  
  # Rule 11: Noun or verbal phrase in which entity appears as conjunct of prepositional object (expansion of Rule 7)
  # Often indicates things associated with the entity (but sometimes also things the entity receives)
  stop_fill11 <- custom_fill(BREAK(relation = c("ccomp", "acl"))) # relcl - sometime with useful context
  rule11 <- tquery(lemma__R = entity,
                  pos = c("NOUN", "PROPN"),
                  relation = "conj",
                  label = "entity",
                  parents(relation = "pobj",
                          label = "rule11",
                          parents(relation = "prep",
                                  label = "rule11",
                                  parents(pos = c("NOUN", "PROPN"),
                                          label = "rule11", 
                                          stop_fill11,
                                          parents(pos = c("VERB", "AUX"), # Optional action
                                                  NOT(relation = "ROOT"),
                                                  label = "rule11",
                                                  req = F,
                                                  stop_fill11)
                                  )
                                  )
                          )
  )
  
  
  
  # Annotate queries in dependency trees ####
  
  # Note: Technically rsyntax accepts a lists of queries to annotate them at once
  # But this creates errors if some nodes are already matched by an earlier rule (nested logics)
  # Thus is do this consecutively to get the most expansive phrase in such instances
  
  
  an <- 
    # Annotate the rules in the dependency tree
    annotate_tqueries(parsed, "rule0", r0=rule0) %>% 
    annotate_tqueries("rule1", r1=rule1) %>% 
    annotate_tqueries("rule2", r2=rule2) %>% 
    annotate_tqueries("rule3", r3=rule3) %>% 
    annotate_tqueries("rule4", r4=rule4) %>% 
    annotate_tqueries("rule5", r5=rule5) %>% 
    annotate_tqueries("rule6", r6=rule6) %>% 
    annotate_tqueries("rule7", r7=rule7) %>% 
    annotate_tqueries("rule8", r8=rule8) %>% 
    annotate_tqueries("rule9", r9=rule9) %>% 
    annotate_tqueries("rule10", r10=rule10) %>% 
    annotate_tqueries("rule11", r11=rule11) %>% 
    # Generate one variable identifying a phrase along the rules (nested)
    mutate(phrase = ifelse(rule0 == "entity", "entity", NA) %>% as.character(),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule1), "rule"), "Rule1", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule2), "rule"), "Rule2", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule3), "rule"), "Rule3", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule4), "rule"), "Rule4", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule5), "rule"), "Rule5", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule6), "rule"), "Rule6", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule7), "rule"), "Rule7", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule8), "rule"), "Rule8", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule9), "rule"), "Rule9", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule10), "rule"), "Rule10", phrase),
           phrase = ifelse(is.na(phrase) & str_detect(as.character(rule11), "rule"), "Rule11", phrase)) %>% 
    # Now we mark each full phrase with a consecutive number within each doc_id (sentence)
    group_by(doc_id) %>% 
    mutate(phrase_id = cumsum(!is.na(phrase) & !lag(!is.na(phrase), default = FALSE)), # increment at start of each phrase block
           phrase_id = paste0("Entity phrase ", phrase_id),
           phrase_id = ifelse(!is.na(phrase), phrase_id, NA)) %>%  # only assign phrase id to non-NA values of phrase
    ungroup() %>% 
    # Technical variable needed for the plot_tree function
    mutate(phrase_fill = NA) 
  # %>% 
  # # Filter out punctuation and stopwords
  # mutate(phrase = ifelse(pos == "PUNCT", NA, phrase),
  #        phrase = ifelse(lemma %in% stopwords("en"), NA, phrase))
  
  
  # Return results ####
  # Plot or phrase tokens
  if(plot == T) {
    plot_tree(an, annotation = "phrase")
  } else {return(an %>% 
                   filter(!is.na(phrase)) %>% # Only what is included in the phrase
                   filter(phrase != "entity") %>% 
                   filter(pos != "PUNCT") %>% # Drop punctuation
                   # filter(!(token %in% quanteda::stopwords("en"))) %>%  # Drop punctuation
                   group_by(doc_id, sentence) %>% 
                   summarise(phrase_words = paste0(token, collapse = ",")) %>% 
                   ungroup() %>%
                   ungroup() %>% 
                   mutate(entity_search = entity) %>% 
                   select(doc_id, sentence, entity_search, phrase_words)
  )}
}