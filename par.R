library(mltools)
library(data.table)
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


all_data_2 <-foreach::foreach(
  j = 1:length(levels(just_action_tracking$comb_and_frame)), .packages = c("mltools", "data.table", "tidyverse")
) %dopar% {graph_processing_function2(just_action_tracking)
  }

graph_processing_function2 <- function(data){
    features <- list()
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
    
    
    
    #get node features(graph.x)
    #get for every play (2 node features)
    features<- to_loop %>%  dplyr::select(x,c("s","node_type_Pass","node_type_Pass Block","node_type_Pass Rush","frameId", "quarter", "down","yardsToGo", "row"))
    #features = append(features, features_list)
    print(j)
  
  
  return(features)
} 
all_data_v2 <- graph_processing_function2(just_action_tracking)