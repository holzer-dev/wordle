rm(list = ls())

library(tidyverse)

# Nur Grundformen (Nomen, Adjektive, Verben), keine deklinierten/konjugierten Wörter.
# Die Listen sind lückenhaft, fehlende Wörter (z. B. "stern") werden von Hand ergänzt.
base_forms <- c("Input/Nomen.txt", "Input/Adjektive.txt", "Input/Verben.txt") %>%
  map(read_lines) %>%
  unlist()

missing_words <- c("stern")

# Nur Wörter mit genau 5 Buchstaben, Kleinschreibung und Duplikate entfernen
words <- tibble(word = c(base_forms, missing_words)) %>%
  filter(str_detect(word, "^[[:alpha:]]{5}$")) %>%
  mutate(word = str_to_lower(word)) %>%
  distinct(word)

# Ein Buchstabe pro Zeile, mit Position im Wort (1-5)
letters_long <- words %>%
  mutate(letter = str_split(word, "")) %>%
  unnest_longer(letter, indices_to = "position")

# 1. Alle Buchstaben zählen (Mehrfachvorkommen in einem Wort zählen mehrfach)
letter_counts <- letters_long %>%
  count(letter, name = "n_total")

# 2. Anzahl Wörter, die den Buchstaben mindestens einmal enthalten
letter_word_counts <- letters_long %>%
  distinct(word, letter) %>%
  count(letter, name = "n_words")

# 3. Häufigkeit des ersten Buchstabens
first_letter_counts <- letters_long %>%
  filter(position == 1) %>%
  count(letter, name = "n_first")

# 4. Häufigkeit des letzten Buchstabens
last_letter_counts <- letters_long %>%
  filter(position == 5) %>%
  count(letter, name = "n_last")

# Alles in einer Tabelle zusammenführen
letter_stats <- list(letter_counts, letter_word_counts, first_letter_counts, last_letter_counts) %>%
  reduce(full_join, by = "letter") %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0L))) %>%
  arrange(desc(n_total))


# Zweites Wort nach "stern": Für jeden Buchstaben aus "stern" wird angenommen,
# dass genau dieser ein Treffer ist (Position egal) und die anderen vier fehlen.
first_word <- "stern"
first_letters <- str_split_1(first_word, "")

second_words <- map(first_letters, function(hit) {
  excluded <- setdiff(first_letters, hit)

  # Mögliche Lösungen: enthalten den Treffer, aber keinen der anderen Buchstaben
  candidates <- letters_long %>%
    group_by(word) %>%
    filter(any(letter == hit), !any(letter %in% excluded)) %>%
    ungroup() %>%
    distinct(word, letter)

  # Wichtigkeit neuer Buchstaben: in wie vielen Kandidaten kommen sie vor?
  new_letter_scores <- candidates %>%
    filter(!letter %in% first_letters) %>%
    count(letter, name = "score")

  # Wort-Score: Summe der Scores seiner neuen Buchstaben (jeder nur einmal)
  candidates %>%
    inner_join(new_letter_scores, by = "letter") %>%
    group_by(word) %>%
    summarise(new_letters = str_c(letter, collapse = ""), score = sum(score)) %>%
    mutate(hit = hit, n_candidates = n_distinct(candidates$word), .before = 1) %>%
    arrange(desc(score))
}) %>%
  bind_rows()

# Die besten 5 Wörter je Treffer-Buchstabe
best_second_words <- second_words %>%
  group_by(hit) %>%
  slice_max(score, n = 5) %>%
  ungroup()
