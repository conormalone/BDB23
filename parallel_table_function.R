library(mltools)
library(data.table)
library(foreach)
library(doParallel)
library(tidyverse)

my.cluster <- parallel::makeCluster(
  parallel::detectCores(), 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)


table_processing_function <- function(data){
  data$comb_and_frame <- droplevels(data$comb_and_frame)
  dataframe <- data %>% filter(comb_and_frame == levels(data$comb_and_frame)[j])
  #get ball location for each play
  ball_location <-dataframe %>% filter(pff_role == "Pass") %>% dplyr::select(c(comb_and_frame, x_std, y_std)) %>%  distinct
  #join ball location
  df <-  dataframe %>%  merge(ball_location,by = "comb_and_frame")
  #get distance from ball to player on each frame
  linesstore<-apply(df, 1, function(x) dist(rbind(c(x["x_std.x"], x["y_std.x"]), c(x["x_std.y"], x["y_std.y"])), method = "euclidean" )[1])
  #store distance from ball as new variable
  df$linelength<-linesstore
  
  #widen players by distance from QB and select 3 closed
  #first select Pass Rush
  Def_dist <- df %>% filter(pff_role == "Pass Rush") %>% 
    mutate(row = rank(linelength, ties.method= "random"))%>% 
    arrange(row)  %>% 
    dcast(., comb_and_frame ~ row, value.var = "linelength") %>% select(any_of(2:4)) 
  Def_dist[c(1:3)[!(c(1:3) %in% colnames(Def_dist))]] = 20 
  names(Def_dist) <- c("rusher_1","rusher_2","rusher_3")
  #Do the same for Pass Block
  Block_dist <- df %>% filter(pff_role == "Pass Block") %>% 
    mutate(row = rank(linelength, ties.method= "random"))%>% 
    arrange(row)  %>% 
    dcast(., comb_and_frame ~ row, value.var = "linelength") %>% select(any_of(2:4)) 
  Block_dist[c(1:3)[!(c(1:3) %in% colnames(Block_dist))]] = 20
  names(Block_dist) <- c("blocker_1","blocker_2","blocker_3")
  
  to_loop <- dataframe %>% 
    dplyr::select(comb_and_frame, comb_id, frameId, quarter, down,yardsToGo, defendersInBox, pff_passCoverage, offenseFormation, pff_playAction) %>% head(1)
  
  y <- ifelse(df$comb_and_frame[1]  %in% just_sack_frame$comb_and_frame,1,0)
  result <- cbind(to_loop, Def_dist,Block_dist,y)
  return(result)
}