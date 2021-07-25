rm(list = ls())
#memory.limit(size=20000)
library(tidyverse)
library(lubridate)
library(caret)
library(survival)
library(Hmisc)
#library(rms)
setwd("C:/Users/ngocdung/Dropbox/Front-end developer/src/data-science-path/udacity-nanodegree/Project 3/Assignment")
train <- read.csv("./Creditworthiness/credit-data-training.csv", stringsAsFactors=F)
test <- read.csv("./Creditworthiness/customers-to-score.csv", stringsAsFactors=F)
colnames(train) <- str_replace_all(colnames(train), '\\.', '_')
colnames(test) <- str_replace_all(colnames(test), '\\.', '_')
glimpse(train)
glimpse(test)

## 1. Building the training set
# 1.1. Identifying and imputing missing
isna = vector(length = length(train))
i=1
for(col in train){
  isna[i] = sum(is.na(col))
  i = i + 1 }
print(data.frame(colnames(train), isna))  #Duration address - 344, Age years - 12

# Impute Age_years
median_age <- median(train$Age_years[!is.na(train$Age_years)])
train$Age_years[is.na(train$Age_years)] <- median_age
#View(train[is.na(train$Age_years),c("Credit_Application_Result","Age_years")])

# Drop Duration_in_Current_address
train = train[,!(colnames(train) %in% c('Duration_in_Current_address'))]

# 1.2. Identifying highly-correlate fields (at least 0.7)
sapply(train, unique)
num_cols = c('Duration_of_Credit_Month', 'Credit_Amount', 'Age_years') #exclude outcome and potential factors
factor = c('Instalment_per_cent', 
           #'Duration_in_Current_address', 
           'Most_valuable_available_asset',
           'Type_of_apartment',
           'Occupation',
           'No_of_dependents',
           'Telephone',
           'Foreign_Worker')  #?
cor(train[c(num_cols,factor)])
