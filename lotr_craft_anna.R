library(dplyr)
library(tm)
library(ggplot2)

lotr <- read.csv("lotr_words_locations.csv")

lotr_locs <- lotr %>% group_by(location, character) %>% 
  summarize(wordsmax = max(words))

lotrs_filt <- lotr_locs %>% filter(wordsmax > 25)
TheShire <- filter(lotr, location == "The Shire")

theshireplot <- ggplot(TheShire)
theshireplot <- theshireplot + geom_histogram(aes(wordsmax))
theshireplot

character_ma <- c("Aragorn", "Frodo", "Gandalf", "Boromir", "Gimli", "Legolas", "Pippin", "Merry", "Sam")
location_ma <- c("Moria", "Rivendell")
character_three <- c("Sam", "Pippin", "Frodo", "Merry")

class(lotr$character)
class(lotr)

lotr$charactern <- lotr$character
lotr$locationon <- lotr$location

lotr_main <- subset(lotr, charactern %in% character_three) %>% subset(locationon %in% location_ma)
lotrplot <- ggplot(lotr)

ggplot(lotr_main, aes(x = words)) + geom_histogram(bins = 10, colours = factor(~ charactern)) + facet_wrap( ~ locationon, nrow = 3)


TheShireplot <- ggplot(TheShire, aes(x = words)) + geom_histogram(bins = 5) + facet_wrap(~ detailed_location, nrow = 3)
TheShireplot


lotrcolore <- c("#E7D9D7", "#C5B6BA", "#C49790", "#53453C", "#1C110F",
                "#DAD9C8", "#DEE8EB", "#AFB675", "#3A5B33", "#1C2516", "#FCD882", 
                "#E8A577", "#DF7B74", "#4E2729", "#180109",
                "#CCC8C0", "#C1C2CE", "#953155", "#391A26", "#160A11", "#D2F6FA")


ggplot(TheShire, aes(detailed_location, fill = character) ) +
  geom_bar(position = "stack") + scale_fill_manual(values = lotrcolore, name = "Some Characters") + theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Locations in and around The Shire", y = "Number of Words spoken at one time", 
       title = "A Basic-Ass Plot of Words Spoken By Characters in the LOTR Movies, The Shire")


for (i in lotrcolore){
  print(i)
  print(col2rgb(i))
}
