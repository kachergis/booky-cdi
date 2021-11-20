# re-save Charlesworth corpus
require(tidyverse)
require(here)
require(tokenizers)

process_text <- function(path, source_name) {
  words = tokenize_words(read_file(path))
  tab <- table(words[[1]])
  wf <- data_frame(word = names(tab), word_count = as.numeric(tab)) %>%
    mutate(source = source_name,
           word_count_norm = word_count * (1e6 / sum(word_count)), # count per million tokens
           prob = word_count / sum(word_count)) %>%
    mutate(word = ifelse(word=="i", "I", ifelse(word=="i'll", "I'll", word))) %>%
    arrange(desc(word_count_norm)) %>%
    mutate(rank = 1:n())
  print(paste0("loaded ", nrow(wf), " word types (", sum(wf$word_count)," tokens)"))
  return(wf)
}

data_dir = "data/Charlesworth-Clean-data/"
adult_dat <- bind_rows(process_text(here(paste0(data_dir, "Adult_Books/gutenberg_corpus.txt")), "books"),
                       process_text(here(paste0(data_dir, "Adult_Speech/adult_speech_corpus.txt")), "speech")) 
# or lemmatized_adult_speech_corpus.txt
# loaded 827414 word types (35143617 tokens)
# loaded 27536 word types (3104328 tokens)


# combine the TV corpora
tv1 <- process_text(here(paste0(data_dir, "Adult_TV/pbs_adults_corpus.txt")), "tv1")
# loaded 45351 word types (4175570 tokens)
tv2 <- process_text(here(paste0(data_dir, "Adult_TV/simply_scripts_corpus.txt")), "tv2")
# loaded 103079 word types (2056384 tokens)
#tv1 %>% select(word, word_count) 

adult_tv <- full_join(tv1 %>% select(word, word_count), 
                      tv2 %>% select(word, word_count), by="word") %>%
  mutate(word_count.x = replace_na(word_count.x, 0),
         word_count.y = replace_na(word_count.y, 0),
         word_count = word_count.x + word_count.y) %>%
  select(word, word_count) %>%
  filter(word_count>1) %>% # hapaxes are mostly junk--phrases with spaces/punctuation removed
  mutate(source = "TV",
         word_count_norm = word_count * (1e6 / sum(word_count)), # count per million tokens
         prob = word_count / sum(word_count)) %>%
  mutate(word = ifelse(word=="i", "I", ifelse(word=="i'll", "I'll", word))) %>% # I'd I've..
  arrange(desc(word_count_norm)) %>%
  mutate(rank = 1:n())

adult_dat = bind_rows(adult_dat, adult_tv)

child_dat <- bind_rows(
  process_text(here(paste0(data_dir, "Child_Books/unlemmatized_child_books.txt")), "books"),
  process_text(here(paste0(data_dir, "Child_TV/childrens_tv_combined_corpus.txt")), "tv"),
  process_text(here(paste0(data_dir, "CHILDES clean text/corpus_parents.txt")), "speech")
) %>%
  filter(!is.element(word, c("d","m","s","t", "xxx", "ve", "ll", "n't", "rrb", "unc", "re"))) 
# loaded 42652 word types (4730867 tokens)
# loaded 84341 word types (6759928 tokens)
# loaded 38770 word types (5495648 tokens)


save(adult_dat, child_dat, file="data/Charlesworth-unlemmatized-counts.Rdata")