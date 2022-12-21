library(tidyverse)
set.seed(1234)
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
    )   
  
  
  return(dataframe)
}
tracking_clean <-pre_processing_function(tracking_dirty)
rm(tracking_dirty)
#merge tracking and pff to attach roles to tracking
play_clean <- plays %>% select(c(gameId,playId,quarter, down,yardsToGo))
play_track <- merge(tracking_clean, play_clean, how = "left",on = c(gameId, playId))
tracking_pff <- merge(play_track, pff, how = "left",on = c(gameId, playId, nflId))
tracking_pff <- tracking_pff %>% 
  filter(pff_role == "Pass Rush" | pff_role == "Pass Block" | pff_role == "Pass")
tracking_pff$event <- as.factor(tracking_pff$event)
#I always make comb_id, giving each play a unique ID
tracking_pff$comb_id <- paste0(as.character(tracking_pff$gameId)," ", as.character(tracking_pff$playId))
tracking_pff$comb_id <- as.factor(tracking_pff$comb_id)
tracking_pff$gameId <- as.factor(tracking_pff$gameId)
tracking_pff$playId <- as.factor(tracking_pff$playId)

# get start and end frame of "action" so I can delete irrelevant play

#events that will indicate start and end of relevant play
snap <- c("ball_snap")
play_over <- c("pass_forward", "qb_sack", "qb_strip_sack", "lateral", "fumble_offense_recovered", "fumble", "run", "handoff", "tackle", "out_of_bounds")
#for each play get the start and end of play
df_start_end <- tracking_pff %>% mutate(play_type = case_when(
  event %in% snap ~ "snapped",
  event %in% play_over ~ "play_over"))

df_in_play_long <-distinct(df_start_end, comb_id, frameId, .keep_all = TRUE)%>% 
  dplyr::select(comb_id, frameId, play_type )
#group by play so each play has a column indicating when relevant play started and ended
df_in_play <- df_in_play_long %>% group_by(comb_id) %>%
  pivot_wider(id_cols = c(comb_id),names_from = play_type, values_from = frameId, values_fn = min)

#remove instances without both a reception and an end event
df_in_play <- df_in_play[rowSums(is.na(df_in_play))<1,]
df_in_play$comb_id <-droplevels(df_in_play$comb_id)


just_action_tracking<- data.frame()
result <- data.frame()
#subset tracking to just frames between snap and pass/sack
for(i in 1:length(levels(df_in_play$comb_id))){
  df_in_play_just_combid <- df_in_play[df_in_play$comb_id == levels(df_in_play$comb_id)[i],] 
  result <- tracking_pff %>% 
    filter(comb_id == levels(df_in_play$comb_id)[i] & frameId >= df_in_play_just_combid$snapped[1] & frameId <= df_in_play_just_combid$play_over[1])
  just_action_tracking <- rbind(just_action_tracking, result)
}
just_action_tracking$pff_role <- as.factor(just_action_tracking$pff_role)
just_action_tracking <- just_action_tracking %>% filter(pff_role == "Pass Rush" | pff_role == "Pass Block" | pff_role == "Pass")
just_action_tracking$comb_id <- droplevels(just_action_tracking$comb_id)
#create unique ID for each frame
just_action_tracking <- just_action_tracking %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
  mutate(comb_and_frame = as.factor(comb_and_frame))
just_action_tracking <- just_action_tracking %>% filter(!is.na(comb_and_frame))
just_action_tracking$comb_and_frame <- as.factor(just_action_tracking$comb_and_frame)




#train val split and 1 0 split
just_sack_frame <- just_action_tracking %>% filter(event == "qb_sack"|event == "qb_strip_sack") %>% select(c(comb_id,frameId)) %>% distinct
no_sack_frame <- levels(just_action_tracking$comb_id)[!(levels(just_action_tracking$comb_id) %in% just_sack_frame$comb_id)]


#SUBSETTING
#subset based on play, so we aren't training and validating on the same play
N<-length(levels(just_action_tracking$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.80)))
validset<-setdiff(1:N,trainset)


#separating test/val data start data
#sample 5 frames within selected plays, to avoid longer plays being over represented
train_val_subset_function <- function(subset){
  frames <- just_action_tracking %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[subset])
  chosen_frames <- data.frame(matrix(, ncol = ncol(frames)))
  names(chosen_frames) <- names(frames)
  for(i in 1:length(subset)){
    df_in_play_down <- df_in_play %>% filter(comb_id == levels(just_action_tracking$comb_id)[subset[i]])
    frames_down <- frames %>% filter(comb_id == levels(just_action_tracking$comb_id)[subset[i]])
    set_1 <- frames_down %>% filter(frameId == df_in_play_down$sample_frame1[1])
    set_2 <- frames_down %>% filter(frameId == df_in_play_down$sample_frame2[1])
    set_3 <- frames_down %>% filter(frameId == df_in_play_down$sample_frame3[1])
    set_4 <- frames_down %>% filter(frameId == df_in_play_down$sample_frame4[1])
    set_5 <- frames_down %>% filter(frameId == df_in_play_down$sample_frame5[1])
    chosen_frames <- rbind(chosen_frames, set_1,set_2,set_3,set_4,set_5)
  }
  chosen_frames$gameId <- as.factor(chosen_frames$gameId)
  chosen_frames$playId <- as.factor(chosen_frames$playId)
  chosen_frames$pff_role <- as.factor(chosen_frames$pff_role)
  return(chosen_frames)
}



