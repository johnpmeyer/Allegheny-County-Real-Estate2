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


smp_size <- floor(0.10 * nrow(data))
n = dim(data)[1]

flag = sort(sample(1:n, smp_size))
data_small = data[flag,]


ggplot(data_small, aes(x=relevant_saleprice, y = SALEPRICE_MINUS_ASSESSED, 
                 colour = assessed)) +
  geom_point()

data_expensive = data %>%
  filter(relevant_saleprice > 500000)

ggplot(data_expensive, aes(x=relevant_saleprice, y = SALEPRICE_MINUS_ASSESSED, 
                       colour = assessed)) +
  geom_point()


data_expensive = data %>%
  filter(relevant_saleprice > 500000)

data_cheap = data_small %>%
  filter(relevant_saleprice < 250000)

ggplot(data_cheap, aes(x=relevant_saleprice, y = SALEPRICE_MINUS_ASSESSED, 
                           colour = assessed)) +
  geom_point()


smp_size <- floor(0.8 * nrow(data_small))
n = dim(data_small)[1]

flag = sort(sample(1:n, smp_size))
data_small_train = data_small[flag,]
data_small_test = data_small[-flag,]


print("KNN")
knn_1 = knn(data_small_train[, 2:3], data_small_test[, 2:3], train_labels, k=1)
knn_1 = as.numeric(knn_1)
knn_1 = replace(knn_1, knn_1==1, 0)
knn_1 = replace(knn_1, knn_1==2, 1)
knn_1.te <- mean(knn_1 != data_small_test[,4])

knn_1.te


smp_size <- floor(0.8 * nrow(data))
n = dim(data)[1]

flag = sort(sample(1:n, smp_size))
data_train = data[flag,]
data_test = data[-flag,]

train_labels = as.vector(data_train$assessed)

print("KNN")
knn_1 = knn(data_train[, 2:3], data_test[, 2:3], train_labels, k=1)
knn_1 = as.numeric(knn_1)
knn_1 = replace(knn_1, knn_1==1, 0)
knn_1 = replace(knn_1, knn_1==2, 1)
knn_1.te <- mean(knn_1 != data_test[,4])

knn_1.te


for (kk in 1:length(k_values)){
  print(kk)
  knn_model = knn(dummytrain[, 2:3], dummytest[, 2:3], dummytrain[, 1], k=k_values[kk])
  knn.te = mean(knn_model != dummytest[, 1])
  
  KNNs = c(KNNs, knn.te)
}