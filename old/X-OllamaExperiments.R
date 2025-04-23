library(rollama)
library(httr2)
library(tidyverse)
library(jsonlite)
# pull_model() # downloads (current) default model — “llama3" /4.3 GB
show_model()
list_models()

# Paths ####
# Needed as big files currently not part of the repo

# data_path <- "~/Nextcloud/Shared/TRIAS Brückenprojekt/Daten/" # MS/WZB
# data_path <- "C:/Users/rauh/NextCloudSync/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/WZB
data_path <- "D:/WZB-Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/HP
# data_path <- "C:/Users/rauh/Nextcloud/Shared/Idee Brückenprojekt Ju-Chri/Daten/" # CR/TP

# Example: Sentences mentioning Russia in 2022
df <- read_rds(paste0(data_path, "RussiaSentences2022.rds")) %>% 
  slice(sample(n()))  # shuffle rows


# System prompt for the model

system_prompt <- paste("You are supposed to classify sentences which stem from statements of the European Commission.\n", 
                       "Classification instructions:\n",
                       "Just on the basis what the sentence says: does it imply that Russia is seen rather as an ally or an enemy?",
                       "Your answer should provide nothing but a well-formatted JSON string that contains the following three elements\n",
                       "REASONING: {A brief reasoning on why this text sees Russia as an ally or an enemy and why you may be sure or unsure.}\n",
                       "LIKELIHOOD: {A continuous estimation in the interval between 0 (= ALLY) and 1 (= ENEMY) that this sentence presents Russia as an ally (0) or as an enemy (1).}",
                       "CLASSIFICATION: {Your choice of whether this sentence presents Russia as an ally or an enemy in a single word. Answer only with ALLY or ENEMY}\n",
                       sep = " ")

options(rollama_config = system_prompt) # Set system prompt


df$model_response <- NA
df$model_time <- NA

start <- Sys.time()
for (i in 1:nrow(df)) {
  
  # Check whether information has already been collected
  if(!is.na(df$model_response[i])) {next}
  
  # Show the sentence
  print(df$text_sent[i])
  
  # Query the model
  response <- 
    query(df$text_sent[i],
          model = "llama3",
          model_params = list(
            seed = 42, # Sets the random number seed to use for generation. Setting this to a specific number will make the model generate the same text for the same prompt. (Default: 0)
            temperature = 0, # The temperature of the model. Increasing the temperature will make the model answer more creatively. (Default: 0.8)
            num_gpu = 0
          ))
  
  # Store the response
  df$model_response[i] <- response$message$content
  df$model_time[i] <- response$total_duration
  
}
end <- Sys.time() - start # 19 hours for 1802 sentences ;)




fromJSON(df$model_response)

write_rds(df, paste0(data_path, "RussiaSentences2022_llama.rds"))


start <- Sys.time()
response <- 
  query(df$text_sent[1],
        model = "phi3",
        model_params = list(
          seed = 42, # Sets the random number seed to use for generation. Setting this to a specific number will make the model generate the same text for the same prompt. (Default: 0)
          temperature = 0, # The temperature of the model. Increasing the temperature will make the model answer more creatively. (Default: 0.8)
          num_gpu = 0
        ))
Sys.time()-start 



