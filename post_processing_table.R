library(tidyverse)
  
all_data <-foreach::foreach(
  j = 1:length(levels(just_action_tracking$comb_and_frame)), .packages = c("reshape2","mltools", "data.table", "tidyverse")
) %dopar% {table_graph_function(just_action_tracking)
}

#set as factor so it can one hot
all_data$pff_passCoverage <- as.factor(all_data$pff_passCoverage)
all_data$offenseFormation <- as.factor(all_data$offenseFormation)

library(fastDummies)
#one hot encode factors so each level gets a column, required for xgboost
all_data <- dummy_cols(all_data, 
                     select_columns = c("pff_passCoverage","offenseFormation"), remove_selected_columns = TRUE)

#filter data
train_data <- result %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[trainset]) %>% select(-c(comb_and_frame,comb_id))
val_data <- result %>% filter(comb_id %in% levels(just_action_tracking$comb_id)[validset])%>% select(-c(comb_and_frame,comb_id))


######XG BOOST
library(xgboost)
train_no_y <- train_data %>% select(-y)
val_no_y <- val_data %>% select(-y)
result_no_y <- result %>% select(-c(y, comb_and_frame,comb_id))
bstSparse <-xgboost(data = as.matrix(train_no_y), label = train_data$y, max.depth = 10, eta = .1,nrounds = 20, objective = "binary:logistic", verbose = 2)
pred <- predict(bstSparse, as.matrix(result_no_y))

val_data$pred <- pred

######### BAYESIAN ADDITIVE REGRESSION SOMETHING
library(BART)
bartfit <- lbart(x.train = train_no_y,y.train = train_data$y)
bartpred <- predict(bartfit, result_no_y)
result$bartpred <- bartpred$prob.test.mean