library(tidyverse)
plays <- read.csv("plays.csv")
pff <- read.csv("pffScoutingData.csv")
week1 <- read.csv("week1.csv")
week2 <- read.csv("week2.csv")
week3 <- read.csv("week3.csv")
week4 <- read.csv("week4.csv")
week5 <- read.csv("week5.csv")
week6 <- read.csv("week6.csv")
week7 <- read.csv("week7.csv")
week8 <- read.csv("week8.csv")
tracking_dirty <- rbind(week1,week2,week3,week4,week5,week6,week7,week8)
rm(week1,week2,week3,week4,week5,week6,week7,week8)
#preprocess tracking data
pre_processing_function <- function(df_input){
  dataframe <- df_input %>% 
    mutate(toLeft = playDirection == "left",
           x_std = ifelse(toLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(toLeft, 160/3-y, y) ## Standardized Y
    )   ## Standardized Dir
  
  
  return(dataframe)
}
tracking_clean <-pre_processing_function(tracking_dirty)
rm(tracking_dirty)
#merge tracking and pff to attach roles to tracking
tracking_pff <- merge(tracking_clean, pff, how = "left",on = c(gameId, playId, nflId))
tracking_just_line_players <- tracking_pff %>% 
  filter(pff_role == "Pass Rush" | pff_role == "Pass Block" | pff_role == "Pass")
tracking_pff$event <- as.factor(tracking_pff$event)
tracking_pff$comb_id <- as.factor(paste0(as.character(tracking_pff$gameId), " ", as.character(tracking_pff$playId)))
#get start and end frame of "action
snap <- c("ball_snap")
play_over <- c("pass_forward", "qb_sack", "qb_strip_sack", "lateral", "fumble_offense_recovered", "fumble", "run", "handoff", "tackle", "out_of_bounds")

df_start_end <- tracking_pff %>% mutate(play_type = case_when(
  event %in% snap ~ "snapped",
  event %in% play_over ~ "play_over"))
df_in_play_long <-distinct(df_start_end, comb_id, frameId, .keep_all = TRUE)%>% 
  dplyr::select(comb_id, frameId, play_type )
df_in_play <- df_in_play_long %>% group_by(comb_id) %>%
  pivot_wider(id_cols = c(comb_id),names_from = play_type, values_from = frameId, values_fn = min)

#remove instances without both a reception and an end event
df_in_play <- df_in_play[rowSums(is.na(df_in_play))<1,]
levels(df_in_play$comb_id) <- droplevels(df_in_play$comb_id)
just_action_tracking<- data.frame()
result <- data.frame()
#subset tracking to just action frames
for(i in 1:length(levels(df_in_play$comb_id))){
    df_in_play_just_combid <- df_in_play[df_in_play$comb_id == levels(df_in_play$comb_id)[i],] 
    result <- tracking_pff %>% 
      filter(comb_id == df_in_play_just_combid$comb_id[1] & frameId >= df_in_play_just_combid$snapped[1] & frameId <= df_in_play_just_combid$play_over[1])
    just_action_tracking <- rbind(just_action_tracking, result)
}
levels(just_action_tracking$comb_id) <- droplevels(just_action_tracking$comb_id)
group_by_pressure <- just_action_tracking %>% group_by(gameId, playId) %>% summarise(pressures = max(pff_hit,pff_hurry,pff_sack, na.rm = TRUE))

N<-length(levels(just_action_tracking$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.70)))
nottestset<-setdiff(1:N,trainset)
validset<-sort(sample(nottestset,size=length(nottestset)/2))
testset<-sort(setdiff(nottestset,validset))
