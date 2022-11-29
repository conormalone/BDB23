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
    )   ## Standardized Dir
  
  
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
tracking_pff$comb_id <- paste0(as.character(tracking_pff$gameId)," ", as.character(tracking_pff$playId))
tracking_pff$comb_id <- as.factor(tracking_pff$comb_id)
tracking_pff$gameId <- as.factor(tracking_pff$gameId)
tracking_pff$playId <- as.factor(tracking_pff$playId)
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
df_in_play$comb_id <-droplevels(df_in_play$comb_id)
#add sampled plays 
df_in_play <- df_in_play %>% rowwise() %>% mutate(sample_frame1 = sample(snapped:play_over,1), sample_frame2 = sample(snapped:play_over,1),
                                                  sample_frame3 = sample(snapped:play_over,1),sample_frame4 = sample(snapped:play_over,1),
                                                  sample_frame5 = sample(snapped:play_over,1),
)%>% ungroup()


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
just_action_tracking <- just_action_tracking %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
  mutate(comb_and_frame = as.factor(comb_and_frame))
just_action_tracking <- just_action_tracking %>% filter(!is.na(comb_and_frame))
just_action_tracking$comb_and_frame <- as.factor(just_action_tracking$comb_and_frame)




#train val split and 1 0 split
just_sack_frame <- just_action_tracking %>% filter(event == "qb_sack"|event == "qb_strip_sack") %>% select(c(comb_id,frameId)) %>% distinct
no_sack_frame <- levels(just_action_tracking$comb_id)[!(levels(just_action_tracking$comb_id) %in% just_sack_frame$comb_id)]
#just_action_zero <- just_action_tracking %>% filter(!(comb_id %in% just_sack_frame$comb_id))
#just_action_one <- just_action_tracking %>% filter(comb_id %in% just_sack_frame$comb_id)

#subset start
N_start<-length(levels(just_action_tracking$comb_id))
trainset_start<-sort(sample(1:N_start,size=floor(N_start*0.80)))
validset_start<-setdiff(1:N_start,trainset_start)
#validset<-sort(sample(nottestset,size=length(nottestset)/2))
#testset<-sort(setdiff(nottestset,validset))
#subset end - sack plays
N_end_1<-length(just_sack_frame$comb_id)
trainset_end_1<-sort(sample(1:N_end_1,size=floor(N_end_1*0.90)))
validset_end_1<-setdiff(1:N_end_1,trainset_end_1)
#subset end - not sacked plays
N_end_0<-length(no_sack_frame)
trainset_end_0<-sort(sample(1:N_end_0,size=floor(N_end_1*0.90*20),replace = T))
validset_end_0<-setdiff(1:N_end_0,trainset_end_0)


rm(tracking_just_line_players)
rm(tracking_pff)
rm(tracking_clean)
rm(df_start_end)

#separating test/val data start data
train_val_subset_function_start <- function(subset){
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
val_data_start <- train_val_subset_function_start(validset_start)
val_data_start <- val_data_start[-1,]
train_data_start  <- train_val_subset_function_start(trainset_start)
train_data_start <- train_data_start[-1,]
#test_data <- just_action_tracking %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[testset])

#subset chosen data test/val data YES SACK
train_val_subset_function_end_1 <- function(subset){
  frames <- just_action_tracking %>% filter(comb_id %in% just_sack_frame$comb_id[subset])
  chosen_frames <- data.frame(matrix(, ncol = ncol(frames)))
  names(chosen_frames) <- names(frames)
  for(i in 1:length(subset)){
    df_in_play_down <- df_in_play %>% filter(comb_id == just_sack_frame$comb_id[subset[i]])
    frames_down <- frames %>% filter(comb_id == just_sack_frame$comb_id[subset[i]])
    set_1 <- frames_down %>% filter(frameId >= (df_in_play_down$play_over[1]-20))
    chosen_frames <- rbind(chosen_frames, set_1)#,set_2,set_3,set_4,set_5)
  }
  chosen_frames$gameId <- as.factor(chosen_frames$gameId)
  chosen_frames$playId <- as.factor(chosen_frames$playId)
  chosen_frames$pff_role <- as.factor(chosen_frames$pff_role)
  return(chosen_frames)
}
#subset chosen data test/val data NO SACK
train_val_subset_function_end_0 <- function(subset){
  frames <- just_action_tracking %>% filter(comb_id %in% no_sack_frame[subset])
  chosen_frames <- data.frame(matrix(, ncol = ncol(frames)))
  names(chosen_frames) <- names(frames)
  for(i in 1:length(subset)){
    sample_choice <- sample(1:20,1)
    df_in_play_down <- df_in_play %>% filter(comb_id == no_sack_frame[subset[i]])
    frames_down <- frames %>% filter(comb_id == no_sack_frame[subset[i]])
    set_1 <- frames_down %>% filter(frameId == (df_in_play_down$play_over[1]-sample_choice))
    chosen_frames <- rbind(chosen_frames, set_1)#,set_2,set_3,set_4,set_5)
  }
  chosen_frames$gameId <- as.factor(chosen_frames$gameId)
  chosen_frames$playId <- as.factor(chosen_frames$playId)
  chosen_frames$pff_role <- as.factor(chosen_frames$pff_role)
  return(chosen_frames)
}
val_data_sack <- train_val_subset_function_end_1(validset_end_1)
train_data_sack  <- train_val_subset_function_end_1(trainset_end_1)
val_data_no_sack <- train_val_subset_function_end_0(validset_end_0)
train_data_no_sack  <- train_val_subset_function_end_0(trainset_end_0)

val_data <- rbind(val_data_sack,val_data_no_sack)
train_data <- rbind(train_data_no_sack,train_data_sack)

just_sack_frame <- just_sack_frame %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
  mutate(comb_and_frame = as.factor(comb_and_frame))
just_sack_frame$comb_and_frame <- as.factor(just_sack_frame$comb_and_frame)
##############
####graph function from 2022
####graph function from 2022
library(mltools)
library(data.table)
graph_processing_function <- function(data, end = 0){
  features <- list()
  adj_matrix <- list()
  y_list <-list()
  data$comb_and_frame <- as.factor(data$comb_and_frame)
  for(j in 1:length(levels(data$comb_and_frame))){
    data$comb_and_frame <- droplevels(data$comb_and_frame)
    dataframe <- data %>% filter(comb_and_frame == levels(data$comb_and_frame)[j])
    #dataframe <- dataframe %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
    # mutate(comb_and_frame = as.factor(comb_and_frame)) %>%  distinct
    #get ball location for each play
    ball_location <-dataframe %>% filter(pff_role == "Pass") %>% dplyr::select(c(comb_and_frame, x_std, y_std)) %>%  distinct
    #join ball location
    df <-  dataframe %>%  merge(ball_location,by = "comb_and_frame")
    
    #get distance from ball to player on each frame
    linesstore<-apply(df, 1, function(x) dist(rbind(c(x["x_std.x"], x["y_std.x"]), c(x["x_std.y"], x["y_std.y"])), method = "euclidean" )[1])
    #store distance from ball as new variable
    df$linelength<-linesstore
    rm(linesstore) 
    rm(ball_location)
    #group combandframe get node feature details
    function_graph_list <- list()
    #if loo is selected subset on chosen frames, otherwise no need
    to_loop <- df %>% 
      group_by(comb_and_frame) %>% 
      mutate(node_type = pff_role, row = rank(linelength, ties.method= "random"))%>% 
      arrange(row) %>% 
      dplyr::select(comb_and_frame, node_type, frameId,s, x, y, row, quarter, down,yardsToGo)
    to_loop$comb_and_frame <- as.factor(to_loop$comb_and_frame)
    to_loop$node_type <- as.factor(to_loop$node_type)
    
    to_loop <-one_hot(data.table(to_loop),cols = c("node_type"))
    
    
    
    #get y values
    if(end ==1){
      group_by_pressure <- ifelse(df$comb_id %in% just_sack_frame$comb_id,1,0)
      #group_by_pressure <- df %>% select(c(pff_hit,pff_hurry,pff_sack)) %>% mutate_all(~replace(., is.na(.), 0)) %>% summarise(pressures = max(pff_hit,pff_hurry,pff_sack))
    }
    else{
      df_this_play <- df_in_play %>% filter(comb_id == df$comb_id[1])
      group_by_pressure <- ifelse(df$frameId >=(df_this_play$play_over[1]-20),1,0)
    }
    
    #get node features(graph.x)
    #get for every play (2 node features)
    features[[j]] <- to_loop %>%  dplyr::select(x,c("s","node_type_Pass","node_type_Pass Block","node_type_Pass Rush","frameId", "quarter", "down","yardsToGo", "row"))
    
    #loop to get distances between all points (adjacency matrix) (graph.a)
    dist_subset <- to_loop %>% select(-row) %>% 
      distinct %>% 
      select(c(x,y))
    adj_matrix[[j]] <- as.matrix(dist(dist_subset,method= "euclidean",diag=T, upper=T))
    
    #features <- c(features, training_features_list)
    #adj_matrix <- c(adj_matrix, train_matrix)
    y_list <- c(y_list, group_by_pressure[1])
  }
  
  function_graph_list <-list( train_x = features, train_a = adj_matrix, y = y_list)
  
  return(function_graph_list)
} 
#use function on start (all) and end data
training_data_start <- graph_processing_function(train_data_start,0)
validation_data_start <- graph_processing_function(val_data_start,0)
training_data_end <- graph_processing_function(train_data,1)
validation_data_end <- graph_processing_function(val_data,1)

all_data <- graph_processing_function(just_action_tracking)


library(reticulate)
source_python("GNN_Functionsv3.py")
start_predictions <- py$predictions_start
end_predictions <- py$predictions_end
#all_predictions <- py$predictions
mean(start_predictions)
#leave out last level, it took 2 days to run, not doing it again
#just_action_tracking <- just_action_tracking %>% filter(comb_and_frame != "2021110100 917 9")
just_action_tracking$comb_and_frame <- droplevels(just_action_tracking$comb_and_frame)
#

length(levels(just_action_tracking$comb_and_frame))
merged_predictions <- cbind(levels(just_action_tracking$comb_and_frame),as.numeric(start_predictions),as.numeric(end_predictions))
colnames(merged_predictions) <- c("comb_and_frame","start_predictions","end_predictions")
new_df <- merge(just_action_tracking,merged_predictions, on= "comb_and_frame")
new_df$end_predictions <-as.numeric(new_df$end_predictions)
new_df$start_predictions <-as.numeric(new_df$start_predictions)
new_df$combined_predictions <- new_df$start_predictions *new_df$end_predictions
level_chk <-new_df %>% group_by(comb_id) %>% select(c("comb_id","combined_predictions")) %>% 
  summarise(max_rate= max(combined_predictions), min_rate = min(combined_predictions), diff = max_rate-min_rate) %>% 
  arrange(desc(diff))
just_action_tracking$pred <- new_df$combined_predictions
