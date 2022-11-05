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

#test train split
N<-length(levels(just_action_tracking$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.80)))

nottestset<-setdiff(1:N,trainset)
validset<-sort(sample(nottestset,size=length(nottestset)/2))
testset<-sort(setdiff(nottestset,validset))
rm(tracking_just_line_players)
rm(tracking_pff)
rm(tracking_clean)
rm(df_start_end)
#just_action_trackingv2 <- just_action_tracking %>% dplyr::select(gameId, playId, comb_id, pff_role, frameId,s, x, y,x_std,y_std)
#separating test/train data
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
val_data <- train_val_subset_function(validset)
train_data <- train_val_subset_function(trainset)
test_data <- just_action_tracking %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[testset])
##############
####graph function from 2022
####graph function from 2022
graph_processing_function <- function(data){
  features <- list()
  adj_matrix <- list()
  for(j in 1:length(levels(data$gameId))){
    dataframe <- data %>% filter(gameId == levels(data$gameId)[j])
    dataframe <- dataframe %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
      mutate(comb_and_frame = as.factor(comb_and_frame)) %>%  distinct
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
      #filter(comb_and_frame %in% packed_frames_list) %>% 
      dplyr::select(comb_and_frame, node_type, frameId,s, x, y, row)
    to_loop$comb_and_frame <- as.factor(to_loop$comb_and_frame)
    to_loop$node_type <- as.factor(to_loop$node_type)
    library(mltools)
    library(data.table)
    to_loop <-one_hot(data.table(to_loop),cols = c("node_type"))
    
    rm(df)  
    
    #get y values
    
    
    
    
    #get node features(graph.x)
    #get for every play (2 node features)
    training_features_list_by <- by(to_loop, to_loop$comb_and_frame, function(x) dplyr::select(x,c("s","node_type_Pass","node_type_Pass Block","node_type_Pass Rush")))
    training_features_list <- list()
    for(i in 1:nrow(froot_loop)){
      training_features_list[[i]] <-training_features_list_by[[i]]
    }  
    #loop to get distances between all points (adjacency matrix) (graph.a)
    train_matrix<-list()
    for(i in 1:length(levels(to_loop$comb_and_frame))){
      train_matrix[[i]] <-list()
      dist_subset <- to_loop %>% select(-row) %>% filter(comb_and_frame == levels(to_loop$comb_and_frame)[i]) %>% 
        distinct %>% 
        select(c(x,y))
      train_matrix[[i]] <- dist(dist_subset,method= "euclidean",diag=T, upper=T)
    }
    features <- append(features, training_features_list)
    adj_matrix <- append(adj_matrix, train_matrix)}
  
  
  
  function_graph_list <-list( train_x = features, train_a = adj_matrix)
  
  return(function_graph_list)
} 
training_data <- function_graph_list(train_data)
validation_data <- function_graph_list(val_data)
training_data <- function_graph_list(test_data)
library(reticulate)

#write.csv(predictions_test, "predictions_test.csv")
