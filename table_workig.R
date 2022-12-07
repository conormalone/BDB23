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
play_clean <- plays %>% select(c(gameId,playId,quarter, down,yardsToGo, offenseFormation, defendersInBox,
                                 pff_playAction, pff_passCoverage))
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

just_sack_frame <- just_action_tracking %>% filter(event == "qb_sack"|event == "qb_strip_sack") %>% select(c(comb_id,frameId)) %>% unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE) %>% 
  mutate(comb_and_frame = as.factor(comb_and_frame))%>% distinct

rm(tracking_just_line_players)
rm(tracking_pff)
rm(tracking_clean)
rm(df_start_end)
#write.csv(just_action_tracking, "just_action.csv")
#get dist, yardstogo, quarter, down, scorediff, offensive formation (factor), defendersinbox, passcoverage, 
library(foreach)
library(doParallel)
library(tidyverse)
my.cluster <- parallel::makeCluster(
  11, 
  type = "PSOCK"
)

#check cluster definition (optional)
print(my.cluster)
## socket cluster with 7 nodes on host 'localhost'
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

just_action_tracking$comb_and_frame <- as.factor(just_action_tracking$comb_and_frame)
just_action_tracking <- just_action_tracking %>% filter(comb_id !="2021101700 2372")
just_action_tracking$comb_and_frame <- droplevels(just_action_tracking$comb_and_frame)

table_processing_function2 <- function(data){



  data <- just_action_tracking
result <- data.frame(matrix(,ncol=17))
names(result) <- c("comb_and_frame","comb_id" ,  "frameId","quarter", "down","yardsToGo", "defendersInBox",  "pff_passCoverage", "offenseFormation", "pff_playAction","rusher_1", "rusher_2",       
                   "rusher_3","blocker_1","blocker_2","blocker_3", "y"  )

for(j in 181588:length(levels(just_action_tracking$comb_and_frame))){
  
 
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
  library(reshape2)
  #dcast(df, comb_and_frame ~ row, value.var = "linelength")
  Def_dist <- df %>% filter(pff_role == "Pass Rush") %>% 
  mutate(row = rank(linelength, ties.method= "random"))%>% 
    arrange(row)  %>% 
    dcast(., comb_and_frame ~ row, value.var = "linelength") %>% select(any_of(2:4)) 
  Def_dist[c(1:3)[!(c(1:3) %in% colnames(Def_dist))]] = 20 
  names(Def_dist) <- c("rusher_1","rusher_2","rusher_3")
  
  Block_dist <- df %>% filter(pff_role == "Pass Block") %>% 
  mutate(row = rank(linelength, ties.method= "random"))%>% 
    arrange(row)  %>% 
    dcast(., comb_and_frame ~ row, value.var = "linelength") %>% select(any_of(2:4)) 
  Block_dist[c(1:3)[!(c(1:3) %in% colnames(Block_dist))]] = 20
  names(Block_dist) <- c("blocker_1","blocker_2","blocker_3")

    keepers <- dataframe %>% 
    dplyr::select(comb_and_frame, comb_id, frameId, quarter, down,yardsToGo, defendersInBox, pff_passCoverage, offenseFormation, pff_playAction) %>% head(1)
  
  y <- ifelse(df$comb_and_frame[1]  %in% just_sack_frame$comb_and_frame,1,0)
iteration <- cbind(keepers, Def_dist,Block_dist,y)
result <- rbind(result, iteration)
#return(iteration)
}
y <- ifelse(result$comb_and_frame  %in% just_sack_frame$comb_and_frame,1,0)
result$y <- y
#make some factors and onehot them
result$pff_passCoverage <- as.factor(result$pff_passCoverage)
result$offenseFormation <- as.factor(result$offenseFormation)
library(fastDummies)
result <- dummy_cols(result, 
                        select_columns = c("pff_passCoverage","offenseFormation"), remove_selected_columns = TRUE)

####test val split

just_action_tracking$comb_id <- as.factor(just_action_tracking$comb_id)
N<-length(levels(just_action_tracking$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.80)))
validset<-setdiff(1:N,trainset)
train_data <- result %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[trainset]) %>% select(-c(comb_and_frame,comb_id))
val_data <- result %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[validset])%>% select(-c(comb_and_frame,comb_id))
#lets find a working model

glm_model <- glm(formula = y ~ ., data = train_data, family = "binomial")
summary(glm_model)
prediction <- predict(glm_model, newdata = val_data)

library(xgboost)
train_no_y <- train_data %>% select(-y)
val_no_y <- val_data %>% select(-y)
result_no_y <- result %>% select(-c(y, comb_and_frame,comb_id))
bstSparse <-xgboost(data = as.matrix(train_no_y), label = train_data$y, max.depth = 10, nrounds = 10, objective = "binary:logistic", verbose = 2)
pred <- predict(bstSparse, as.matrix(result_no_y))
pred
val_data$pred <- pred
#write.csv(val_data,"validation.csv")

result$pred <- pred
write.csv(result,"result_check.csv")

bartfit <- lbart(x.train = train_no_y,y.train = train_data$y,sparse = TRUE,ndpost = 500,nskip = 2500,keepevery = 5,printevery = 500)
bart_pred <- predict(bartfit, result_no_y)
