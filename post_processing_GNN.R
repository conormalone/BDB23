library(tidyverse)
#functions from pre_processing.R
val_data <- train_val_subset_function(validset)
train_data <- train_val_subset_function(trainset)

training_data<- foreach::foreach(
  j = 1:length(levels(train_data$comb_and_frame)), .packages = c("mltools", "data.table", "tidyverse")
) %dopar% {graph_processing_function(train_data)}
  
validation_data <- foreach::foreach(
  j = 1:length(levels(val_data$comb_and_frame)), .packages = c("mltools", "data.table", "tidyverse")
) %dopar% {graph_processing_function(val_data)}

all_data <- foreach::foreach(
  j = 1:length(levels(just_action_tracking$comb_and_frame)), .packages = c("mltools", "data.table", "tidyverse")
) %dopar% {graph_processing_function(just_action_tracking)}


library(reticulate)
#can also use binary version, will need to rejig the processing function.
#source_python("GNN_Functions_Binary.py")
source_python("GNN_Functions_Categorical.py")

all_predictions <- py$predictions

#compare prediction and actual for categorical
all_eval <- data.frame(matrix(ncol = 0, nrow = 0))
for(i in 1:nrow(all_predictions)){
  comb_and_frame <- levels(just_action_tracking$comb_and_frame)[i]
  predict <- max(which(all_predictions[i,] > 0.5))
  actual <-all_data$y[i][[1]]
  difference <- actual - predict
  all_eval <- rbind(all_eval, c(comb_and_frame, predict,actual, difference))
}
colnames(all_eval) <- c("comb_and_frame",  "predict","actual", "difference")