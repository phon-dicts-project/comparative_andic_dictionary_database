library(tidyverse)
library(tidytext)

# get the phonological discription of every language ----------------------
read_csv("andic_dicts.csv") %>% 
  mutate(bor = ifelse(str_detect(ipa, "[ьыёюя]"), 1, bor),
         ipa = str_remove_all(ipa, "͡"),
         ipa = str_replace_all(ipa, "ɕ", "ʃ"),
         ipa = str_replace_all(ipa, "qχ", "q"),
         ipa = str_replace_all(ipa, "g", "ɡ"),
         ipa = str_replace_all(ipa, "u-ю", "u-j-u"),
         ipa = str_replace_all(ipa, "-Ю.", "-u"),
         ipa = str_replace_all(ipa, "-ю", "-u"),
         ipa = str_replace_all(ipa, "-Я.", "-a*"),
         ipa = str_replace_all(ipa, "А", "a"),
         ipa = str_replace_all(ipa, "о", "o"),
         ipa = str_replace_all(ipa, "Кь", "tɬ"),
         ipa = str_remove_all(ipa, "\\(тлис.\\)"),
         ipa = str_remove_all(ipa, "\\(гь.*$"),
         ipa = str_remove_all(ipa, "\\("),
         ipa = str_remove_all(ipa, "\\)"),
         ipa = str_remove_all(ipa, "<"),
         ipa = str_replace_all(ipa, "ы", "i"),
         ipa = str_replace_all(ipa, "-ё", "ʲ-o"),
         ipa = str_replace_all(ipa, "-ь", "ʲ"),
         ipa = str_replace_all(ipa, "ь", "ʲ"),
         ipa = str_replace_all(ipa, "ja", "j-a"),
         ipa = str_replace_all(ipa, "’", "'"),
         ipa = str_replace_all(ipa, "ʼ", "'"),
         ipa = str_remove_all(ipa, "^-"),
         ipa = str_replace_all(ipa, "[Бб]", "b"),
         ipa = ifelse(str_detect(lemma, ":"), ipa, str_replace_all(ipa, ":", "ː")),
         ipa = str_remove_all(ipa, " :.*$"),
         ipa = str_replace_all(ipa, "xx", "xː"),
         ipa = str_replace_all(ipa, "ʷː", "ːʷ"),
         ipa = str_replace_all(ipa, "ː'", "'ː"),
         ipa = str_replace_all(ipa, "'ʲ", "ʲ'"),
         ipa = str_replace_all(ipa, "ːʲ", "ʲː"),
         ipa = str_remove_all(ipa, "-\\sI{1,}"),
         ipa = str_replace_all(ipa, "ç", "x"),
         ipa = str_replace_all(ipa, "aːᴴ", "ãː"),
         ipa = str_remove_all(ipa, "- –.*$"),
         ipa = str_remove_all(ipa, "1\\.?$"),
         ipa = str_remove_all(ipa, "2$"),
         ipa = str_replace_all(ipa, "χI", "ħ"),
         ipa = str_replace_all(ipa, "aː́", "a`ː"),
         ipa = str_replace_all(ipa, "ũ", "ũ"),
         ipa = str_replace_all(ipa, "ã", "ã"),
         ipa = str_replace_all(ipa, "ĩ", "ĩ"),
         ipa = str_replace_all(ipa, "õ", "õ"),
         ipa = str_replace_all(ipa, "ẽ", "ẽ"),
         ipa = str_replace_all(ipa, "í", "'i"),
         ipa = str_replace_all(ipa, "l̄", "l:"),
         ipa = str_replace_all(ipa, "-`-", "`-"),
         ipa = str_replace_all(ipa, "([aeiou])̃?ː?(['\\*`ˌ])", "\\2\\1"),
         ipa = str_replace_all(ipa, "[`*]", "'")
         ) %>% 
  write_csv("andic_dicts.csv", na = "")

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

