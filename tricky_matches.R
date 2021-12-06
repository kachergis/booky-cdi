# add unmatched CDI words/bigrams
tricky_matches <- tibble(
  cdi_definition = c("a lot","all gone", "buttocks/bottom*", "yum yum", "turn around", "uh oh", "french fries", "gonna/going to", "night night", "hafta/have to", "green beans", "high chair", "ice cream", "inside/in", "lemme/let me", "next to", "on top of", "peanut butter", "quack quack", "rocking chair", "shh/shush/hush", "teddybear", "thank you", "wanna/want to", "woof woof", "gotta/got to"),
  bigram_to_search = c("a lot", "all gone", "butt", "yum", "turn around", "uh oh", "french fries", "going to", "good night", "have to", "green beans", "high chair", "ice cream", "inside", "let me", "next to", "on top of", "peanut butter", "quack", "rocking chair", "hush", "teddy bear", "thank you", "want to", "woof", "got to"),
  simple_word = c("a lot", "all gone", "butt", "yum", "turn around", "uh oh", "french fries", "gonna", "night night", "hafta", "green beans", "high chair", "ice cream", "inside", "lemme/let me", "next to", "on top of", "peanut butter", "quack", "rocking chair", "shh", "teddy bear", "thank you", "wanna", "woof", "gotta"))

