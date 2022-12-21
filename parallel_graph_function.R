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

graph_processing_function <- function(data){
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
  
  to_loop <- df %>% 
    group_by(comb_and_frame) %>% 
    #add variable rank, ranking distance to QB, 1 being closed
    mutate(node_type = pff_role, row = rank(linelength, ties.method= "random"))%>% 
    arrange(row) %>% 
    dplyr::select(comb_and_frame, node_type, frameId,s, x, y, row, quarter, down,yardsToGo)
  to_loop$comb_and_frame <- as.factor(to_loop$comb_and_frame)
  to_loop$node_type <- as.factor(to_loop$node_type)
  #one hot encode node type, so Passer, Pass Rush and Pass Block are Binary variables
  to_loop <-one_hot(data.table(to_loop),cols = c("node_type"))
  
  #filter results to get response variable (y)
  df_this_play <- df_in_play %>% filter(comb_id == df$comb_id[1])
  group_by_pressure <- ifelse(df$frameId >=(df_this_play$play_over[1]-20),abs(df$frameId -df_this_play$play_over[1]),0)
  
  
  #get node features(graph.x)
  #get for every play (2 node features)
  features <- to_loop %>%  dplyr::select(x,c("s","node_type_Pass","node_type_Pass Block","node_type_Pass Rush","frameId", "quarter", "down","yardsToGo", "row"))
  
  #loop to get distances between all points (adjacency matrix) (graph.a)
  dist_subset <- to_loop %>% select(-row) %>% 
    distinct %>% 
    select(c(x,y))
  adj_matrix <- as.matrix(dist(dist_subset,method= "euclidean",diag=T, upper=T))
  #return in list
  function_graph_list <-list( train_x = features, train_a = adj_matrix, y = group_by_pressure[1])
  return(function_graph_list)
}

