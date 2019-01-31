
# data cleaning

library(tidyverse)
library(tidytext)
library(stringi)

load("~/shiny.Rdata")

safeData <- trustData %>% 
  filter(is.na(Optout) | Optout == "No")

# remove the extraneous columns

use_data <- safeData %>% 
  select(Date, Time, formtype, Improve : BestCrit, Division2, Directorate2, Location)

# remove punctuation

use_data <- use_data %>% 
  mutate(Keep_Improve = Improve) %>% 
  mutate(Improve = str_to_lower(Improve)) %>% 
  mutate(Improve = gsub("[[:punct:]]", "", Improve)) %>% 
  mutate(Improve = str_squish(Improve))

# remove the single word comments

use_data <- use_data %>% 
  filter(!Improve %in% c("none", "nothing", "declined", "no", "blank")) %>% 
  filter(!Improve %in% c("", "0", "00", "100", "1010", "4444", "6", "6666", "7", "711", 
                         "9", "a", "a ok", "ai", "all", "dont", "idk",  
                         "n a", "n0", "na", "na9", "ni", "nil", "nine", 
                         "nne", "non", "nonw", "nope", "note", "nout", "nowt", "nr", "ok", 
                         "okay", "sw", "wa", "x", "x 56", "yes", "zero"))

use_data <- use_data %>% 
  filter(!is.na(Improve)) %>% 
  mutate(comment = row_number()) %>% 
  mutate(word_count = stri_count_words(Improve))

improve_words <- use_data %>%
  unnest_tokens(word, Improve, token = "words")

improve_words <- improve_words %>% 
  filter(!grepl('^\\d+$', word))

# this is just a check

# improve_words %>% 
#   count(word) %>% 
#   head(1000) %>% 
#   write.csv(file = "check")

save(improve_words, use_data, categoriesTable, file = "cleanData.Rdata")
