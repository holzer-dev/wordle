library(tidyverse)

# Wortliste einlesen (ein Wort pro Zeile)
words <- read_lines("Input/all_words.md") %>%
  tibble(word = .)

# Nur Wörter mit genau 5 Buchstaben behalten
five_letter_words <- words %>%
  filter(str_detect(word, "^[[:alpha:]]{5}$"))

write_csv(five_letter_words, "Input/5_digit_words.csv")

# Gefilterte Liste wieder in die Session laden
five_digit_words <- read_csv("Input/5_digit_words.csv")
