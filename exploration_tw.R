## ---------------------------
##
## Script name: exploration_tw.R
##
## Purpose of script: Explore potential models
##
## Author: Tyler Wiederich
##
## Date Created: 2022-08-22
##
## ---------------------------
##
## Notes:
##   > Clean missing values with mean
##   > Use model from exploration
##
## ---------------------------

library(tidyverse)
library(lubridate)

#Arguments
split <- 0.75
iterations <- 1000

#Reading in training file
orig = read.csv('data/train.csv')

#Filling missing values with the mean
fill_mean = function(x){
  temp_mean = suppressWarnings(mean(x, na.rm = T))
  dat = x
  new_dat = c()
  for(i in 1:length(x)){
    if(is.na(dat[i]))(new_dat[i] <- ifelse(is.integer(dat), round(temp_mean), temp_mean))
    else(new_dat[i] <- dat[i])
  }
  return(new_dat)
}

orig_fill_means = as.data.frame(sapply(orig, fill_mean, simplify = F))

#Checking for missing values
sum(flatten_int(lapply(orig_fill_means, function(x)(sum(is.na(x))))) != 0)





#Training/test set on training data set
set.seed(101)
to_sample = sample(1:dim(orig_fill_means)[1],
                       size = round(split*dim(orig_fill_means[1])))

train = orig_fill_means[to_sample,-1]
test = orig_fill_means[!(1:dim(orig_fill_means)[1] %in% to_sample),-1]

#Full main effects model
mod = glm(failure~., data = train[,-1], family = 'binomial')
summary(mod)

#This model fit well with 0.75 split
mod2 = glm(failure ~ loading + attribute_0 + attribute_2 +
             measurement_2 + measurement_4 + measurement_17,
           data = train,
           family = 'binomial')
summary(mod2)

#Predictions with >50% classified as a failure
prediction = predict(mod2, test[,-25], type = 'response')

answer = 1:length(prediction)
answer[prediction>=0.5] <- 1
answer[prediction<0.5] <- 0

#Single trial success
mean(answer == test$failure)



#Testing accuracy with different partitionings of data
accuracy = c()
for(i in 1:iterations){
  start_time = now()
  set.seed(i)
  to_sample = sample(1:dim(orig_fill_means)[1],
                     size = round(split*dim(orig_fill_means[1])))
  
  sim_train = orig_fill_means[to_sample,-1]
  sim_test = orig_fill_means[!(1:dim(orig_fill_means)[1] %in% to_sample),-1]
  
  mod2 = glm(failure ~ loading + attribute_0 + attribute_2 +
               measurement_2 + measurement_4 + measurement_17,
             data = train,
             family = 'binomial')
  summary(mod2)
  
  prediction = predict(mod2, sim_test[,-25], type = 'response')
  
  answer = 1:length(prediction)
  answer[prediction>=0.5] <- 1
  answer[prediction<0.5] <- 0
  
  accuracy[i] <- mean(answer==sim_test$failure)
  end_time = now()
  cat('Iteration', i, 'completed in:', round(end_time - start_time,2), 'seconds\n')
  
}

hist(accuracy)
