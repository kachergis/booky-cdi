library(ggpubr)
library(glue)
library(wordbankr)
install.packages("remotes")
remotes::install_github("langcog/childesr")
library(childesr)
library(SnowballC)

normalize_language <- function(language) {
  language %>% str_replace(" ", "_") %>% str_to_lower()
}

walk(list.files("scripts", pattern = "*.R$", full.names = TRUE), source)

target_langs <-c("French (Quebecois)")


fr_wg <- create_inst_data("French (Quebecois)", "WG")
fr_ws <- create_inst_data("French (Quebecois)", "WS")
fr_wg_summary <- collapse_inst_data(fr_wg)
fr_ws_summary <- collapse_inst_data(fr_ws)
fr_comb_summary <- combine_form_data(list(fr_wg_summary, fr_ws_summary))

uni_lemmas <- get_uni_lemmas(fr_comb_summary)#(wb_data)
uni_lemma_map <- build_uni_lemma_map(uni_lemmas)

childes_speech_corpora<- c("Champaud", "Geneva","Goadrose", "Leveille", "Paris", "Pauline", "Yamaguchi", "Hunkeler", "Lyon", "Paris", "Palasis",  "York")

metric_funs <- list(compute_count, compute_positions)
corpus_args <- list(corpus = childes_speech_corpora, role = NULL, role_exclude = "Target_Child",
                    age = NULL, sex = NULL, part_of_speech = NULL, token = "*")

get_childes_metrics("French (Quebecois)", metric_funs, corpus_args)

fc<-get_uni_lemma_metrics("French (Quebecois)", uni_lemma_map) %>%
  select(language, uni_lemma, tokens, count, sumcount, freq_raw)

cdi_items <- unique(fr_comb_summary$uni_lemma) #609 items

cdi_matched = intersect(cdi_items, fc$uni_lemma) #553/609

cdi_unmatched = setdiff(cdi_items, fc$uni_lemma)

#childse_bookreading_corpora<- c()?






