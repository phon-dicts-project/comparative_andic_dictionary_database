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

df %>% 
  mutate(ipa = str_replace_all(ipa, "eˌ", "ˌe"),
         ipa = str_replace_all(ipa, "aːˌ", "ˌaː"),
         ipa = str_replace_all(ipa, "iˌ", "ˌi"),
         ipa = str_replace_all(ipa, "ĩː",  "ĩː"),
         ipa = str_replace_all(ipa, "ĩː", "ĩː"),
         ipa = str_replace_all(ipa, "oˌ", "ˌo"),
         ipa = str_replace_all(ipa, "ũ", "ũ"),
         ipa = str_replace_all(ipa, "ũ", "ũ"),
         ipa = str_replace_all(ipa, "uˌ", "ˌu"),
         ipa = str_replace_all(ipa, "õ", "о̃"),
         ipa = str_replace_all(ipa, "õː","о̃ː" ),
         ipa = str_replace_all(ipa, "aˌ", "ˌa")) %>% 
  write_csv("andic_dicts.csv", na = "")
  

df %>% 
  select(glottocode, reference, ipa) %>% 
  unnest_tokens(sound, ipa, token = stringr::str_split, pattern = "-") 

# create a table of correspondences ---------------------------------------
library(tidyverse)
df <- read_csv("andic_dicts.csv")

df %>% 
  mutate(languages_source = str_c(language, " (", reference, ")")) %>% 
  filter(!is.na(meaning_ru)) %>% 
  select(languages_source, ipa, meaning_ru, borrowing_source_language, definition) %>% 
  mutate(borrowing_source_language = ifelse(is.na(borrowing_source_language),
                                            "native",
                                            borrowing_source_language)) %>% 
  select(languages_source, ipa, meaning_ru, borrowing_source_language) %>% 
  group_by(meaning_ru, languages_source) %>%
  mutate(id = 1:n()) %>% 
  sample_n(1) %>% 
  pivot_longer(names_to = "names", values_to = "value",  c(ipa, borrowing_source_language)) %>% 
  mutate(languages_source = ifelse(names != "ipa", str_c(languages_source, " bor"), languages_source)) %>%
  select(-names) %>% 
  pivot_wider(names_from = languages_source, values_from = value) ->
  result

result$n_languages <- 10-rowSums(is.na(result))/2

write_csv(result, "all_langs_with_borrowing.csv", na = "")


