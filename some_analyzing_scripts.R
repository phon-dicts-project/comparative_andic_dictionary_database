library(tidyverse)
library(tidytext)

# get the phonological discription of every language ----------------------
df <- read_csv("andic_dicts.csv")

df %>% 
  distinct(glottocode) %>% 
  pull(glottocode) %>% 
  map(function(x){
    df %>% 
      filter(glottocode == x) %>% 
      unnest_tokens(sound, ipa, token = stringr::str_split, pattern = "-") %>% 
      distinct(sound) %>% 
      pull(sound) %>% 
      sort()
  }) ->
  sound_list

df %>% 
  distinct(glottocode) %>% 
  pull(glottocode) ->
  names(sound_list)

sound_list

# creation table for borrowing annotation ---------------------------------

df %>% 
  select(lang, id_word, id_meaning, id, ipa, meaning_ru, definition, bor, borrowing_source_word) %>% 
  rename(borrowing_source_language = borrowing_source_word) %>% 
  mutate(borrowing_source_transcription = "") %>% 
  write_csv("andic_dicts_version_for_borrowings_annotation.csv", na = "")


