rm(list = ls())
#memory.limit(size=20000)
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(bst)
library(pROC)
#library(Hmisc)
#library(rms)
setwd("C:/Users/ngocdung/Dropbox/Front-end developer/src/data-science-path/udacity-nanodegree/Project 3/Assignment")
raw <- read.csv("./Creditworthiness/credit-data-training.csv", stringsAsFactors=F)
validation <- read.csv("./Creditworthiness/customers-to-score.csv", stringsAsFactors=F)
colnames(raw) <- str_replace_all(colnames(raw), '\\.', '_')
colnames(validation) <- str_replace_all(colnames(validation), '\\.', '_')
glimpse(raw)
glimpse(validation)

## 1. Building the dataing set
# 1.1. Identifying and imputing missing data
isna = vector(length = length(raw))
i=1
for(col in raw){
  isna[i] = sum(is.na(col))
  i = i + 1 }
print(data.frame(colnames(raw), isna))  #Duration address - 344, Age years - 12

# Impute Age_years
median_age <- median(raw$Age_years[!is.na(raw$Age_years)])
raw$Age_years[is.na(raw$Age_years)] <- median_age
#View(raw[is.na(raw$Age_years),c("Credit_Application_Result","Age_years")])

# Drop Duration_in_Current_address
raw = raw[,!(colnames(raw) %in% c('Duration_in_Current_address'))]

# 1.2. Identifying low-variability fields and removing them
sapply(raw, unique)
num_cols <-  c('Duration_of_Credit_Month', 
               'Credit_Amount', 
               'Age_years') #exclude outcome and potential factors
factor <-  c('Instalment_per_cent', 
           #'Duration_in_Current_address', 
           'Most_valuable_available_asset',
           'Type_of_apartment',
           'Occupation',
           'No_of_dependents',
           'Telephone',
           'Foreign_Worker')  
remain <- colnames(raw[!colnames(raw) %in% c(num_cols, factor)])

count(raw, Instalment_per_cent)
count(raw, Most_valuable_available_asset)
count(raw, Type_of_apartment)
count(raw, Occupation)  #1-500
count(raw, No_of_dependents)  #1-427, 2-73
count(raw, Telephone)  
count(raw, Foreign_Worker)  #1-481, 2-19

count(raw, Credit_Application_Result)
count(raw, Account_Balance)
count(raw, Payment_Status_of_Previous_Credit)
count(raw, Purpose)  #Draw histo - temporarily accepted
count(raw, Value_Savings_Stocks)
count(raw, Length_of_current_employment)
count(raw, Guarantors)  #1-457, 2-43
count(raw, Concurrent_Credits)  #1-500
count(raw, No_of_Credits_at_this_Bank)

# data_scale <- scale(raw[c(num_cols, factor)], center = T, scale = T)
# apply(data_scale,2, var)

# 1.3. Identifying highly-correlate fields (at least 0.7)
factor1 <- c('Instalment_per_cent',    #? Can treat as ordered numeric
           'Most_valuable_available_asset',
           #'Telephone',
           'Type_of_apartment')
remain1 <- c("Credit_Application_Result",
             "Account_Balance",
             "Payment_Status_of_Previous_Credit",
             "Purpose",
             "Value_Savings_Stocks",
             "Length_of_current_employment",
             "No_of_Credits_at_this_Bank")
raw <- raw %>% mutate(Credit_Application_Result= as.factor(Credit_Application_Result),
                           Account_Balance= as.factor(Account_Balance),
                           Payment_Status_of_Previous_Credit=as.factor(Payment_Status_of_Previous_Credit),
                           Purpose=as.factor(Purpose),
                           Value_Savings_Stocks=as.factor(Value_Savings_Stocks),
                           Length_of_current_employment=as.factor(Length_of_current_employment),
                           No_of_Credits_at_this_Bank=as.factor(No_of_Credits_at_this_Bank))
                        
a <- cor(raw[c(num_cols,factor1,remain1)])
# a[a==1] <- 0
# max(a) #0.5739797
cor(raw[c(num_cols)])

data <- raw[c(num_cols,factor1,remain1)]
colnames(data)  #13

## 2. Train your Classification Models
#Bias/Fairness: Are Opportunity/Odds/Accuracy same across different groups ?
#Prediction bias = average of predictions - average of lables in dataset
#Is under predict = bias?
data$worthy <- as.factor(ifelse(data$Credit_Application_Result == 'Creditworthy', 1, 0))
data$nonworthy <- as.factor(ifelse(data$Credit_Application_Result == 'Non-Creditworthy', 1, 0))

set.seed(1)
train.index <- createDataPartition(data$Credit_Application_Result, p = .7, list = FALSE)  
train <- data[train.index,]  #351
test <- data[-train.index,]  #149

# Logistic regression
lr <- vector()
model1 <- train(Credit_Application_Result~., 
                data=train[!colnames(train) %in% c('worthy', 'nonworthy')], 
                method='glm')  #check
y_hat1 <- predict(model1, test)
y_hat10 <- predict(model1, train)
lr['Accuracy_train'] <- confusionMatrix(data = y_hat10, reference = train$Credit_Application_Result)$overall["Accuracy"] #Bias
lr['Accuracy_test'] <- confusionMatrix(data = y_hat1, reference = test$Credit_Application_Result)$overall["Accuracy"]

model1w <- train(worthy~., 
                  data=train[!colnames(train) %in% c('Credit_Application_Result', 'nonworthy')], 
                  method='glm')
y_hat1w <- predict(model1w, test)
lr['Accyracy_worthy'] <- confusionMatrix(data = y_hat1w, reference = test$worthy)$overall["Accuracy"]

model1n <- train(nonworthy~., 
                  data=train[!colnames(train) %in% c('Credit_Application_Result', 'worthy')], 
                  method='glm')
y_hat1n <- predict(model1n, test)
lr['Accuracy_nonworthy'] <- confusionMatrix(data = y_hat1n, reference = test$nonworthy)$overall["Accuracy"]

vi1 <- varImp(model1, scale = F)   #pvalue
plot1 <- ggplot(vi1, top = dim(vi1$importance)[1]) + 
  ggtitle('Variable Importance - Logistic Regression')

# Decision tree
tree <- vector()
model2 <- train(Credit_Application_Result~., 
                data=train[!colnames(train) %in% c('worthy', 'nonworthy')],
                method='rpart')
y_hat2 <- predict(model2, test)
y_hat20 <- predict(model2, train)
tree['Accuracy_train'] <- confusionMatrix(data = y_hat20, reference = train$Credit_Application_Result)$overall["Accuracy"] 
tree['Accuracy_test'] <- confusionMatrix(data = y_hat2, reference = test$Credit_Application_Result)$overall["Accuracy"]            

model2w <- train(worthy~., 
                 data=train[!colnames(train) %in% c('Credit_Application_Result', 'nonworthy')], 
                 method='rpart')
y_hat2w <- predict(model2w, test)
tree['Accuracy_worthy'] <- confusionMatrix(data = y_hat2w, reference = test$worthy)$overall["Accuracy"]

model2n <- train(nonworthy~., 
                 data=train[!colnames(train) %in% c('Credit_Application_Result', 'worthy')], 
                 method='rpart')
y_hat2n <- predict(model2n, test)
tree['Accuracy_nonworthy'] <- confusionMatrix(data = y_hat2n, reference = test$nonworthy)$overall["Accuracy"]

vi2 <- varImp(model2, scale = F)   #pvalue
plot2 <- ggplot(vi2, top = dim(vi2$importance)[1]) + 
  ggtitle('Variable Importance - Decision Tree')

# Forest model
forest <- vector()
set.seed(1)
model3 <- train(Credit_Application_Result~., 
                data=train[!colnames(train) %in% c('worthy', 'nonworthy')], 
                method='rf',
                ntree=500)
y_hat3 <- predict(model3, test)
y_hat30 <- predict(model3, train)
forest['Accuracy_train'] <- confusionMatrix(data = y_hat30, reference = train$Credit_Application_Result)$overall["Accuracy"]
forest['Accuracy_test'] <- confusionMatrix(data = y_hat3, reference = test$Credit_Application_Result)$overall["Accuracy"]   

model3w <- train(worthy~., 
                 data=train[!colnames(train) %in% c('Credit_Application_Result', 'nonworthy')], 
                 method='rf',
                 ntree=500)
y_hat3w <- predict(model3w, test)
forest['Accuracy_worthy'] <- confusionMatrix(data = y_hat3w, reference = test$worthy)$overall["Accuracy"]

model3n <- train(nonworthy~., 
                 data=train[!colnames(train) %in% c('Credit_Application_Result', 'worthy')], 
                 method='rf',
                 ntree=500)
y_hat3n <- predict(model3n, test)
forest['Accuracy_nonworthy'] <- confusionMatrix(data = y_hat3n, reference = test$nonworthy)$overall["Accuracy"]

vi3 <- varImp(model3, scale = F)   #pvalue
plot3 <- ggplot(vi3, top = dim(vi3$importance)[1]) + 
  ggtitle('Variable Importance - Random Forest')
  
# Boosted model - Boosted Classification Trees
# In Alteryx default, max number of trees 4000, method to determine the final number of trees: Cross validation (5 folds, 1 machine core) 
# Must modify the data so that factor variablies -> numeric factor variables?

# model4 <- bst(train[-1], 
#                 as.numeric(train$Credit_Application_Result), 
#                 learner='tree')  
boosted <- vector()
train4 <- train %>% mutate(#Credit_Application_Result= as.numeric(Credit_Application_Result),
  Account_Balance= as.factor(as.numeric(Account_Balance)),
  Payment_Status_of_Previous_Credit=as.factor(as.numeric(Payment_Status_of_Previous_Credit)),
  Purpose=as.factor(as.numeric(Purpose)),
  Value_Savings_Stocks=as.factor(as.numeric(Value_Savings_Stocks)),
  Length_of_current_employment=as.factor(as.numeric(Length_of_current_employment)),
  No_of_Credits_at_this_Bank=as.factor(as.numeric(No_of_Credits_at_this_Bank)),
  # Instalment_per_cent = as.factor(Instalment_per_cent),
  # Most_valuable_available_asset = as.factor(Most_valuable_available_asset),
  # Type_of_apartment = as.factor(Type_of_apartment)
  )

test4 <- test %>% mutate(#Credit_Application_Result= as.numeric(Credit_Application_Result),
  Account_Balance= as.factor(as.numeric(Account_Balance)),
  Payment_Status_of_Previous_Credit=as.factor(as.numeric(Payment_Status_of_Previous_Credit)),
  Purpose=as.factor(as.numeric(Purpose)),
  Value_Savings_Stocks=as.factor(as.numeric(Value_Savings_Stocks)),
  Length_of_current_employment=as.factor(as.numeric(Length_of_current_employment)),
  No_of_Credits_at_this_Bank=as.factor(as.numeric(No_of_Credits_at_this_Bank)),
  # Instalment_per_cent = as.factor(Instalment_per_cent),
  # Most_valuable_available_asset = as.factor(Most_valuable_available_asset),
  # Type_of_apartment = as.factor(Type_of_apartment)
  )
control <- trainControl(method = "cv", number = 5)
set.seed(1)
model4 <- train(Credit_Application_Result~., 
                data=train4[!colnames(train) %in% c('worthy', 'nonworthy')], 
                method="bstTree", 
                trControl = control)
y_hat4 <- predict(model4, test4)
y_hat40 <- predict(model4, train4)
boosted['Accuracy_train'] <- confusionMatrix(data = y_hat40, reference = train4$Credit_Application_Result)$overall["Accuracy"]  
boosted['Accuracy_test'] <- confusionMatrix(data = y_hat4, reference = test4$Credit_Application_Result)$overall["Accuracy"]  

model4w <- train(worthy~., 
                 data=train4[!colnames(train) %in% c('Credit_Application_Result', 'nonworthy')], 
                 method='bstTree',
                 trControl = control)
y_hat4w <- predict(model4w, test4)
boosted['Accuracy_worthy'] <- confusionMatrix(data = y_hat4w, reference = test4$worthy)$overall["Accuracy"]

model4n <- train(nonworthy~., 
                 data=train4[!colnames(train) %in% c('Credit_Application_Result', 'worthy')], 
                 method='bstTree',
                 trControl = control)
y_hat4n <- predict(model4n, test4)
boosted['Accuracy_nonworthy'] <- confusionMatrix(data = y_hat4n, reference = test4$nonworthy)$overall["Accuracy"]

vi4 <- varImp(model4, scale = F)   #pvalue
plot4 <- ggplot(vi4, top = dim(vi4$importance)[1]) + 
  ggtitle('Variable Importance - Boosted Tree')

plot_grid(plot1, plot2, plot3, plot4)

# ROCs
roc(as.numeric(test$Credit_Application_Result), 
    as.numeric(y_hat1), plot = TRUE, AUC=TRUE)$auc   #3c_2

x <- predict(optimal_rf_fit_log, test_obs[, c("Group", all_metabolites)], type = "raw")
y <- ROC(as.numeric(x), stat = test_obs$Group, AUC = TRUE, ) 


roc(as.numeric(test$Credit_Application_Result), 
    as.numeric(y_hat1), plot=TRUE,
    legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", 
    ylab="True Postive Percentage", 
    print.auc=TRUE,
    print.auc.x=45,
    # col="#377eb8", 
    # lwd=4, partial.auc=c(100, 90), auc.polygon = TRUE, 
    # auc.polygon.col = "#377eb822"
    ) 
legend("bottomright", legend=c("Logisitic Regression"))

plot.roc(as.numeric(test$Credit_Application_Result), 
         as.numeric(y_hat3), 
         legacy.axes=TRUE, percent=TRUE, 
         xlab="False Positive Percentage", 
         ylab="True Postive Percentage", 
         print.auc=TRUE,
         print.auc.x=45,
         col="#4daf4a", 
         # lwd=4, partial.auc=c(100, 90), auc.polygon = TRUE, 
         # auc.polygon.col = "#377eb822"
) 
legend("bottomright", legend=c("Random Forest"), 
       col=c("#377eb8", "#4daf4a"), lwd=4)



