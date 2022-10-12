library(tidyverse)
plays <- read.csv("plays.csv")
pff <- read.csv("pffScoutingData.csv")
plays$pff_passCoverage <- as.factor(plays$pff_passCoverage)
levels(plays$pff_passCoverage)
table(plays$pff_passCoverage)
play_subset_list<- plays %>% 
  group_by(pff_passCoverage) %>%
  summarise(n = n()) %>% filter(n >100) %>% select(pff_passCoverage)
play_subset <- plays[plays$pff_passCoverage %in% play_subset_list$pff_passCoverage,]
plays_and_pff <- merge(play_subset, pff, on = "playId")
