require(tidyverse)


# get CDI data
load("data/en_ws_production.Rdata")
cdi_items <- d_en_ws %>% distinct(definition, lexical_class, category) %>%
  mutate(word = case_when(definition=="a lot" ~ "lot",
                          definition=="all gone" ~ "gone",
                          definition=="buttocks/bottom*" ~ "butt",
                          definition=="call (on phone)" ~ "call",
                          definition=="can (auxiliary)" ~ "can",
                          definition=="can (object)" ~ "can",
                          definition=="chicken (animal)" ~ "chicken",
                          definition=="chicken (food)" ~ "chicken",
                          definition=="clean (action)" ~ "clean",
                          definition=="clean (description)" ~ "clean",
                          definition=="church*" ~ "church", # add temple, synagogue..?
                          definition=="daddy*" ~ "daddy",
                          definition=="mommy*" ~ "mommy",
                          definition=="grandma*" ~ "grandma",
                          definition=="grandpa*" ~ "grandpa",
                          definition=="vagina*" ~ "vagina",
                          definition=="penis*" ~ "penis",
                          definition=="drink (beverage)" ~ "drink",
                          definition=="drink (action)" ~ "drink",
                          definition=="did/did ya" ~ "did",
                          definition=="dress (object)" ~ "dress",
                          definition=="dry (action)" ~ "dry",
                          definition=="dry (description)" ~ "dry",
                          definition=="baa baa" ~ "baa",
                          definition=="choo choo" ~ "choo",
                          definition=="woof woof" ~ "woof",
                          definition=="yum yum" ~ "yum",
                          definition=="fish (animal)" ~ "fish",
                          definition=="fish (food)" ~ "fish",
                          definition=="gas station" ~ "gas", # or station? or average?
                          definition=="work (action)" ~ "work",
                          definition=="work (place)" ~ "work",
                          definition=="water (beverage)" ~ "water",
                          definition=="water (not beverage)" ~ "water",
                          definition=="wanna/want to" ~ "wanna", # or want
                          definition=="watch (action)" ~ "watch",
                          definition=="watch (object)" ~ "watch",
                          definition=="TV" ~ "tv",
                          definition=="washing machine" ~ "washing",
                          definition=="lawn mower" ~ "mower",
                          definition=="soda/pop" ~ "soda",
                          definition=="swing (action)" ~ "swing",
                          definition=="swing (object)" ~ "swing",
                          definition=="go potty" ~ "potty",
                          definition=="gonna/going to" ~ "gonna", # search CHILDES
                          definition=="gotta/got to" ~ "gotta",
                          definition=="little (description)" ~ "little",
                          definition=="next to" ~ "next",
                          definition=="hafta/have to" ~ "hafta",
                          definition=="french fries" ~ "fries",
                          definition=="need/need to" ~ "need",
                          definition=="inside/in" ~ "in", # also inside
                          definition=="orange (description)" ~ "orange",
                          definition=="orange (food)" ~ "orange",
                          definition=="on top of" ~ "top",
                          definition=="owie/boo boo" ~ "ouch",
                          definition=="potato chip" ~ "chip",
                          definition=="quack quack" ~ "quack",
                          definition=="tissue/kleenex" ~ "tissue",
                          definition=="shh/shush/hush" ~ "shh",
                          definition=="slide (action)" ~ "slide",
                          definition=="slide (object)" ~ "slide",
                          definition=="teddybear" ~ "teddy",
                          definition=="thank you" ~ "thanks",
                          definition=="toy (object)" ~ "toy",
                          definition=="try/try to" ~ "try",
                          definition=="turn around" ~ "turn",
                          definition=="TV" ~ "tv", # or television
                          definition=="uh oh" ~ "uhoh",
                          TRUE ~ definition)) %>%
  filter(!is.element(definition, c("babysitter's name", "child's own name", "gonna get you!", "give me five!", "pet's name", "so big!", "this little piggy"))) 
# remove some phrases we can't possibly match

# sum(cdi_items$word==cdi_items$definition) # need to match $word in corpora, or definition (Montag)

saveRDS(cdi_items, "data/CDI-WS-American-EN.rds")