library(tidyverse)
library(tidytext)

# get the phonological discription of every language ----------------------
df <- read_csv("https://raw.githubusercontent.com/phon-dicts-project/comparative_andic_dictionary_database/master/andic_dicts.csv")

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

