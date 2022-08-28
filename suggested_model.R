## ---------------------------
##
## Script name: suggested_model.R
##
## Purpose of script: Model for Kaggle Competition
##
## Author: Tyler Wiederich
##
## Date Created: 2022-08-27
##
## ---------------------------
##
## Notes:
##   > Clean missing values with mean
##   > Use model from exploration
##
## ---------------------------

library(tidyverse)
train = read.csv('data/train.csv')
test = read.csv('data/test.csv')

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

train_fill_means = as.data.frame(sapply(train, fill_mean, simplify = F))
test_fill_means = as.data.frame(sapply(test, fill_mean, simplify = F))


mod = glm(failure ~ loading + attribute_0 + attribute_2 +
             measurement_2 + measurement_4 + measurement_17,
           data = train_fill_means,
           family = 'binomial')
summary(mod)

prediction = predict(mod, newdata = test_fill_means, type = 'response')

answer = 1:length(prediction)
answer[prediction>=0.5] <- 1
answer[prediction<0.5] <- 0

result = data.frame(id = test$id, failure = prediction)

write.csv(result, file = 'data/submission_tw.csv')
