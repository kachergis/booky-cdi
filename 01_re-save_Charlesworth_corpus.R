# re-save Charlesworth corpus
require(tidyverse)
require(here)
require(tokenizers)

source("tricky_matches.R")

# grep -o "gotta" Child_Books/unlemmatized_child_books.txt | wc -l
ch_child_book_counts = c(290, 55, 22, 0, 6, 0, 0, 2506, 91, 1393, 0, 1, 26, 643, 840, 71, 54, 0, 0, 8, 151, 0, 515, 1333, 0, 550)
# do n't = 6510; ca n't = 2404; I 'll = 3330

#raw <- read_file(here(paste0(data_dir, "CHILDES clean text/corpus_parents.txt")))
# grep -o "butt" corpus_parents.txt | wc -l
ch_childes_counts = c(1519, 212, 0, 0, 349, 1613, 89, 29322, 257, 985, 77, 101, 708, 1944, 4734, 311, 574, 138, 0, 92, 630, 397, 2532, 21986, 0, 2859)
# going to = 7953; gonna = 21369
# have to = 491; hafta = 494
# let me = 4058; lemme = 676
# want to = 8120; wanna = 13866
# got to = 316; gotta = 2543

extra_cdi_books = data.frame(word = tricky_matches$simple_word, 
                               word_count = ch_child_book_counts) %>%
   bind_rows(tibble(word=c("don't", "can't", "I'll"), word_count=c(6510, 2404, 3330)))

extra_cdi_childes = data.frame(word = tricky_matches$simple_word, 
                               word_count = ch_childes_counts)

process_text <- function(path, source_name, extra_cdi_matches=data.frame()) {
  words = tokenize_words(read_file(path))
  tab <- table(words[[1]])
  wf <- data_frame(word = names(tab), word_count = as.numeric(tab))
  
  if(nrow(extra_cdi_matches)>0) {
    wf <- wf %>% bind_rows(extra_cdi_matches) %>%
      group_by(word) %>% summarise(word_count = max(word_count)) # in case we got duplicates, use larger
  }
  
  wf <- wf %>%
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
  process_text(here(paste0(data_dir, "Child_Books/unlemmatized_child_books.txt")), "books", extra_cdi_books),
  process_text(here(paste0(data_dir, "Child_TV/childrens_tv_combined_corpus.txt")), "tv"),
  process_text(here(paste0(data_dir, "CHILDES clean text/corpus_parents.txt")), "speech", extra_cdi_childes)
) %>%
  filter(!is.element(word, c("d","m","s","t", "xxx", "ve", "ll", "n't", "rrb", "unc", "re"))) 
# loaded 42677 word types (4751006 tokens)
# loaded 84341 word types (6759928 tokens)
# loaded 38789 word types (5562189 tokens)


save(adult_dat, child_dat, file="data/Charlesworth-unlemmatized-counts.Rdata")