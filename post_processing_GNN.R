library(tidyverse)
#functions from pre_processing.R

all_data <- foreach::foreach(
  j = 1:length(levels(just_action_tracking$comb_and_frame)), .packages = c("mltools", "data.table", "tidyverse","fastDummies")
) %dopar% {graph_processing_function(just_action_tracking)}


just_c_and_f_train <-unique(just_action_tracking[just_action_tracking$comb_id %in% levels(just_action_tracking$comb_id)[trainset],"comb_and_frame"])
just_c_and_f_val <-unique(just_action_tracking[just_action_tracking$comb_id %in% levels(just_action_tracking$comb_id)[validset],"comb_and_frame"])

training_data <- all_data[levels(just_action_tracking$comb_and_frame) %in% just_c_and_f_train]
validation_data <- all_data[levels(just_action_tracking$comb_and_frame) %in% just_c_and_f_val]

library(reticulate)
#can also use binary version, will need to rejig the processing function.
source_python("GNN_Functions_Binary.py")

#source_python("GNN_Functions_Categorical.py")

all_predictions <- py$predictions

#compare prediction and actual for categorical
merged_predictions <- cbind(levels(just_action_tracking$comb_and_frame),as.numeric(all_predictions))
colnames(merged_predictions) <- c("comb_and_frame","predictions")
new_df <- merge(just_action_tracking,merged_predictions, on= "comb_and_frame")

write.csv(new_df, "new_df.csv")
training_data[1]
training_data[230006]
