library(readr)
library(tidyverse)
library(lubridate)
library(gbm)
library(e1071)
library(fastDummies)
library(class)
library(randomForest)

setwd("~/GitHub/Allegheny-County-Real-Estate2")

data = read_csv('final_data.csv') %>%
  select(-SCHOOLDESC) %>%
  mutate(MUNIDESC = ifelse(grepl("PITTSBURGH", MUNIDESC), "Pittsburgh", MUNIDESC))

# test = data %>%
#   mutate(MUNIDESC2 = ifelse(grepl("PITTSBURGH", MUNIDESC), "Pittsburgh", MUNIDESC)) %>%
#   group_by(MUNIDESC, MUNIDESC2) %>%
#   summarise(count = n())

data$MUNIDESC = as.factor(data$MUNIDESC)

set.seed(420)

smp_size <- floor(0.8 * nrow(data))
n = dim(data)[1]

flag = sort(sample(1:n, smp_size));
datatrain = data[flag,];
datatest  = data[-flag,];

baseline_train = sum(datatrain$assessed) / nrow(datatrain)
baseline_test = sum(datatest$assessed) / nrow(datatest)

LOG_REGS = NULL
thresholds = seq(0, 1, by = 0.1)


lr = glm(assessed ~ MUNIDESC + 
           relevant_saleprice + SALEPRICE_MINUS_ASSESSED, 
         family = binomial, data = datatrain)

lr.2 = glm(assessed ~ relevant_saleprice + SALEPRICE_MINUS_ASSESSED, 
           family = binomial, data = datatrain)

for (t in 1:length(thresholds)){
  lr.pred = predict(lr, datatest[, 1:3], type = 'response')
  lr.pred[lr.pred >= thresholds[t]] = '1'
  lr.pred[lr.pred < thresholds[t]] = '0'
  lr.te = mean(lr.pred != datatest[, 4])
  print(t)
  
  lr.2.pred = predict(lr.2, datatest[, 2:3], type = 'response')
  lr.2.pred[lr.2.pred >= thresholds[t]] = '1'
  lr.2.pred[lr.2.pred < thresholds[t]] = '0'
  lr.2.te = mean(lr.2.pred != datatest[, 4])
  print(t)
  
  LOG_REGS = c(LOG_REGS, lr.te, lr.2.te)
}

#Does CV on its own.
gbm.model = gbm(assessed ~ ., data = datatrain, 
                distribution = 'bernoulli', 
                n.trees = 7000, 
                shrinkage = 0.01, 
                interaction.depth = 3, 
                cv.folds = 10)

optimal_gbm = gbm.perf(gbm.model, method = "cv")
optimal_gbm

summary(gbm.model)

pred.gbm = predict(gbm.model, newdata = datatest[, -4], n.trees = optimal_gbm, 
                   type = 'response')

yhat = ifelse(pred.gbm <0.5, 0, 1)
sum(yhat!=datatest[4]) / nrow(datatest)

#Naives Bayes
dummytrain = dummy_columns(datatrain) %>%
  select(assessed, everything()) %>%
  select(-MUNIDESC)

dummytest = dummy_columns(datatrain) %>%
  select(assessed, everything()) %>%
  select(-MUNIDESC)

dummytrain = data.frame(lapply(dummytrain, 
                               function(x) scale(x, center=FALSE, 
                                                 scale = max(x, na.rm = TRUE)/1)))

dummytest = data.frame(lapply(dummytest, 
                              function(x) scale(x, center=FALSE, 
                                                scale = max(x, na.rm = TRUE)/1)))


nb = naiveBayes(datatrain[,2:3], datatrain[,1])
nb.pred <- predict(nb,datatest[,2:3]);
nb.te <- mean(nb.pred != datatest[,1])
nb.te

#KNN
KNNs = NULL
k_values = c(1, 3, 7, 25)
for (kk in 1:length(k_values)){
  print(kk)
  knn_model = knn(dummytrain[, 2:3], dummytest[, 2:3], dummytrain[, 1], k=k_values[kk])
  knn.te = mean(knn_model != dummytest[, 1])
  
  KNNs = c(KNNs, knn.te)
}

#Forest
rf1 <- randomForest(as.factor(datatrain$assessed) ~ relevant_saleprice + 
                      SALEPRICE_MINUS_ASSESSED, data=datatrain, 
                    importance=TRUE)

rf.pred = predict(rf1, datatest, type='class')

rf.te = sum(rf.pred != datatest$assessed) / nrow(datatest)