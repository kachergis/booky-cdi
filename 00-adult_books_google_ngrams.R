require(tidyverse)
require(ngramr) # access to Google Books ngrams to get bigrams (e.g., 'have to')

bigram_definitions = c("a lot","all gone", "buttocks/bottom*", "yum yum", "turn around", "uh oh", "french fries", "gonna/going to", "night night", "hafta/have to", "green beans", "high chair", "ice cream", "inside/in", "lemme/let me", "next to", "on top of", "peanut butter", "quack quack", "rocking chair", "shh/shush/hush", "teddybear", "thank you", "wanna/want to", "woof woof")
bigrams_to_search = c("a lot", "all gone", "butt", "yum", "turn around", "uh oh", "french fries", "going to", "good night", "have to", "green beans", "high chair", "ice cream", "inside", "let me", "next to", "on top of", "peanut butter", "quack", "rocking chair", "hush", "teddy bear", "thank you", "want to", "woof")

# Google book frequencies (743.8 billion words) min freq = 100005
adult_books_uni <- read.csv(here("data/google-books-common-words.txt"), sep='\t') %>%
  mutate(word = tolower(word),
         word = ifelse(word=="i", "I", word),
         word_count_norm = word_count * (1e6 / sum(word_count)), # count per million tokens
         prob = word_count / sum(word_count),
         source = "Adult Books") %>%
  arrange(desc(word_count)) #%>%
#  mutate(rank = 1:n(),
#         on_cdi = ifelse(is.element(word, cdi_voc), 1, 0)) # 643


goog_baseline <- ngram(c("the", "of", "and", "to")) %>% # Google books P("the") = 0.07138255
  group_by(Phrase) %>% summarise(prob=mean(Frequency)) %>%
  mutate(unigram_probs = c(0.03042581, 0.04162986, 0.07138255, 0.02601006), # and, of, the, to
         ratio = unigram_probs / prob)
goog_baseline_ratio = mean(goog_baseline$ratio) # 1.236
goog_bigrams <- bind_rows(ngram(bigrams_to_search[1:10]),
                          ngram(bigrams_to_search[11:20]),
                          ngram(bigrams_to_search[20:25])) %>%
  group_by(Phrase) %>%
  summarise(prob = goog_baseline_ratio * mean(Frequency)) %>%
  rename(word = Phrase) %>%
  mutate(word_count_norm = 1e6 * prob)

goog_bigrams$definition = bigram_definitions

adult_books <- adult_books_uni %>% select(-word_count) %>%
  left_join(cdi_items) %>%
  bind_rows(goog_bigrams %>% left_join(cdi_items)) %>%
  arrange(desc(prob)) %>%
  mutate(rank = 1:n(),
         on_cdi = ifelse(is.element(definition, cdi_items$definition), 1, 0))

saveRDS(adult_books, file="data/Google_books_frequencies.rds")