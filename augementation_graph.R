#file to augment the training data, bringing 501 sacks to 400 for training (80%) then up to 20 times that.
just_sacks_tracking <- just_action_tracking %>% filter(comb_and_frame %in% just_sack_frame$comb_and_frame) #%>% droplevels

just_sacks <- all_data[levels(just_action_tracking$comb_and_frame) %in% just_sacks_tracking$comb_and_frame]
just_sacks <- just_sacks[sample(1:501, size=400)]
result <- list()
library(rlist)
#create augmented data by randomly removing rows (not 1 or 2)
for(i in 1:400){
  aug <- just_sacks[[i]]
  for(j in 1:30){
  #SAMPLE rows to delete from adjacency and feature sets
  #excludes closed and 2nd closest to ball, as thats fundamental to what a sack is
  sample_1 <- sample(3:dim(aug$train_x)[1],sample(1:(dim(aug$train_x)[1]-2),1))
  #change closest and second closest by small amount, this samples that amount
  sample_2 <- sample(0.95:1.05,1)
  aug_1 <- list()
  #multiply first and second players of adj matrix by sampled value
  #to change value slightly
  aug$train_a[c(1,2),] <- aug$train_a[c(1,2),] * sample_2
  aug$train_a[,c(1,2)] <- aug$train_a[,c(1,2)] * sample_2
  aug$train_x$s <- aug$train_x$s *sample_2
  aug_1$train_a <- aug$train_a[-sample_1,-sample_1]
  aug_1$train_x <- aug$train_x[-sample_1,]
  aug_1$y <- 1
  iteration <- list(train_a = aug_1$train_a, train_x = aug_1$train_x, y = aug_1$y)
  result <- list.append(result,iteration)
  }
  
}
training_data <- append(training_data, result)
