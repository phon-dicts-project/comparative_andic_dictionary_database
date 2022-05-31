library(tidyverse)
dicts <- read_csv("andic_dicts.csv")

library(tidyverse)
dicts %>% 
  select(id_word, glottocode, definition) %>% 
  mutate(definition = str_replace_all(definition, "³", "3"),
         definition = str_replace_all(definition, "\\s{2,}", " "),
         definition = str_remove(definition, "^\\s{1,}"),
         analyze = str_split(definition, ";")) %>% 
  unnest_longer(analyze) %>% 
  mutate(analyze = str_remove_all(analyze, "\\s[\\dабвгдI][\\d]?[\\)\\.].*"),
         analyze = str_remove(analyze, "\\s{1,}$")) %>% 
  filter(str_detect(analyze, "^ "),
         str_count(analyze, " ") > 1,
         !str_detect(analyze, "^ \\*?_?ратл\\*?\\._?"),
         !str_detect(analyze, "^ \\*?_?цег\\._?"),
         !str_detect(analyze, "^ \\*?_?тлян\\._?"),
         !str_detect(analyze, "^ \\*?_?тл\\._?"),
         !str_detect(analyze, "^ миарс\\."),
         !str_detect(analyze, "^ \\(?тлис\\.\\)?"),
         !str_detect(analyze, "^ тлон\\."),
         !str_detect(analyze, "^ тук\\."),
         !str_detect(analyze, "^ \\(?кван\\."),
         !str_detect(analyze, "^ анг\\."),
         !str_detect(analyze, "^ \\(?гим\\."),
         !str_detect(analyze, "^ цум\\."),
         !str_detect(analyze, "^ зиб\\."),
         !str_detect(analyze, "^ awe\\."),
         !str_detect(analyze, "^ ewe\\."),
         !str_detect(analyze, "^ гиг\\."),
         !str_detect(analyze, "^ гги\\."),
         !str_detect(analyze, "^ пгук\\."),
         !str_detect(analyze, "^ мук\\."),
         !str_detect(analyze, "^1ук\\."),
         !str_detect(analyze, "^кан\\."),
         !str_detect(analyze, "^гитм\\."),
         !str_detect(analyze, "^эчед\\."),
         !str_detect(analyze, "^числ\\."),
         !str_detect(analyze, "^межд\\."),
         !str_detect(analyze, "^нареч\\."),
         !str_detect(analyze, "^мест\\."),
         !str_detect(analyze, "^указ\\."),
         !str_detect(analyze, "^мн\\."),
         !str_detect(analyze, "^род\\. п\\."),
         !str_detect(analyze, "^масд\\. гл\\."),
         !str_detect(analyze, "^анат\\."),
         !str_detect(analyze, "^_?\\(?букв\\._?"),
         !str_detect(analyze, "^_?лат\\._?"),
         !str_detect(analyze, "^_?см\\. ?тж\\._?"),
         !str_detect(analyze, "^_?ср\\._?"),
         !str_detect(analyze, "^_?\\(?см\\._?")) %>%
  distinct() %>% 
  mutate(analyze = str_remove(analyze, "^\\s{1,}")) %>% 
  write_csv("examples.csv")