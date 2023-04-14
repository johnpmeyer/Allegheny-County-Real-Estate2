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

data$MUNIDESC = as.factor(data$MUNIDESC)

#set.seed(420)

smp_size <- floor(0.8 * nrow(data))
n = dim(data)[1]

flag = sort(sample(1:n, smp_size));
datatrain = data[flag,];
datatest  = data[-flag,];

baseline_train = sum(datatrain$assessed) / nrow(datatrain)
baseline_test = sum(datatest$assessed) / nrow(datatest)

TEALL_FINAL = NULL
TEALL = NULL

B = 100

for (b in 1:B){
  flag = sort(sample(1:n, smp_size))
  datatrain = data[flag,]
  datatest  = data[-flag,]
  
  
  LOG_REGS = NULL
  thresholds = seq(0, 1, by = 0.1)
  
  print("Logistic Regression")
  
  
  lr = glm(assessed ~ MUNIDESC + 
             relevant_saleprice + SALEPRICE_MINUS_ASSESSED, 
           family = binomial, data = datatrain)
  
  lr.2 = glm(assessed ~ relevant_saleprice + SALEPRICE_MINUS_ASSESSED, 
             family = binomial, data = datatrain)
  
  for (t in 1:length(thresholds)){
    print(t)
    
    tryCatch(expr = {
      lr.pred = predict(lr, datatest[, 1:3], type = 'response')
      lr.pred[lr.pred >= thresholds[t]] = '1'
      lr.pred[lr.pred < thresholds[t]] = '0'
      lr.te = mean(lr.pred != datatest[, 4])
      
      lr.2.pred = predict(lr.2, datatest[, 2:3], type = 'response')
      lr.2.pred[lr.2.pred >= thresholds[t]] = '1'
      lr.2.pred[lr.2.pred < thresholds[t]] = '0'
      lr.2.te = mean(lr.2.pred != datatest[, 4])
      
      LOG_REGS = c(LOG_REGS, lr.te, lr.2.te)
    }, 
    error = function(e) {
      print(e)
      LOG_REGS = c(LOG_REGS, NA, NA)
    })
  }
  
  
  #Naives Bayes
  print("Bayes")
  dummytrain = dummy_columns(datatrain) %>%
    select(assessed, everything()) %>%
    select(-MUNIDESC)
  
  dummytest = dummy_columns(datatest) %>%
    select(assessed, everything()) %>%
    select(-MUNIDESC)
  
  dummytrain = data.frame(lapply(dummytrain, 
                                 function(x) scale(x, center=FALSE, 
                                                   scale = max(x, na.rm = TRUE)/1)))
  
  dummytest = data.frame(lapply(dummytest, 
                                function(x) scale(x, center=FALSE, 
                                                  scale = max(x, na.rm = TRUE)/1)))
  
  #Bayes
  nb = naiveBayes(as.numeric(assessed) ~ relevant_saleprice + SALEPRICE_MINUS_ASSESSED,
                  data = datatrain)
  nb.pred = predict(nb,datatest[,2:3], type = 'class')
  nb.pred = as.numeric(nb.pred)
  nb.pred = replace(nb.pred, nb.pred==1, 0)
  nb.pred = replace(nb.pred, nb.pred==2, 1)
  nb.te <- mean(nb.pred != datatest[,4])
  
  #KNN
  print("KNN")
  KNNs = NULL
  k_values = c(1, 3, 7, 25)
  for (kk in 1:length(k_values)){
    print(kk)
    knn_model = knn(dummytrain[, 2:3], dummytest[, 2:3], dummytrain[, 1], k=k_values[kk])
    knn.te = mean(knn_model != dummytest[, 1])
    
    # #testing
    # dummytest$pred = knn_model
    # dummytest = dummytest %>%
    #   select(assessed, relevant_saleprice, SALEPRICE_MINUS_ASSESSED, pred) %>%
    #   mutate(pred_type = case_when(
    #     pred == assessed & pred == 1 ~ 'Pred_Assessed_Correct', 
    #     pred == assessed & pred == 0 ~ 'Pred_NonAssessed_Correct', 
    #     pred != assessed & pred == 1 ~ 'Pred_Assessed_Wrong', 
    #     pred != assessed & pred == 0 ~ 'Pred_NonAssessed_Wrong', 
    #   ))
    # 
    # ggplot(dummytest, aes(x=relevant_saleprice, y = SALEPRICE_MINUS_ASSESSED, 
    #                         colour = pred_type)) +
    #   geom_point()
    # 
    # ggplot(dummytrain, aes(x=relevant_saleprice, y = SALEPRICE_MINUS_ASSESSED, 
    #                       colour = assessed)) +
    #   geom_point()
    # 
    KNNs = c(KNNs, knn.te)
  }
  
  #Forest
  print("Forest")
  rf1 <- randomForest(as.factor(datatrain$assessed) ~ relevant_saleprice + 
                        SALEPRICE_MINUS_ASSESSED, data=datatrain, 
                      importance=TRUE)
  
  rf.pred = predict(rf1, datatest, type='class')
  
  rf.te = sum(rf.pred != datatest$assessed) / nrow(datatest)
  
  
  
  TEALL = c(LOG_REGS, nb.te, KNNs, rf.te)
  TEALL_FINAL = rbind(TEALL, TEALL_FINAL)
}

gbm.model = gbm(assessed ~ relevant_saleprice + 
                  SALEPRICE_MINUS_ASSESSED, data = datatrain, 
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

column_labels = c("LR.1, 0.0", "LR.2, 0.0",
                  "LR.1, 0.1", "LR.2, 0.1",
                  "LR.1, 0.2", "LR.2, 0.2",
                  "LR.1, 0.3", "LR.2, 0.3",
                  "LR.1, 0.4", "LR.2, 0.4",
                  "LR.1, 0.5", "LR.2, 0.5",
                  "LR.1, 0.6", "LR.2, 0.6",
                  "LR.1, 0.7", "LR.2, 0.7",
                  "LR.1, 0.8", "LR.2, 0.8",
                  "LR.1, 0.9", "LR.2, 0.9",
                  "LR.1, 1.0", "LR.2, 1.0",
                  "Bayes",
                  "KNN.1, 1",
                  "KNN.1, 3", 
                  "KNN.1, 7",
                  "KNN.1, 25", 
                  "Random Forest"
)

dim(TEALL_FINAL); 
colnames(TEALL_FINAL) = column_labels

clipr::write_clip(TEALL_FINAL)
