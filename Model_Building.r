# Building model to predict engagement category of a tweet

install.packages("xgboost")
install.packages('multiROC')

library(xgboost)
library(caret)
library(nnet)
library(pROC)
library(multiROC)
setwd("C:/Users/promaninfante/OneDrive - IESEG/SM Analytics/Social-Media-Analytics/data")

base_table <- read.csv("final_base_table.csv", row.names = 1)
target <- base_table$engagement_categ
label <- as.integer(target) - 1 

set.seed(123)

base_table$engagement_categ <- as.factor(base_table$engagement_categ)


train_index<- createDataPartition(base_table$engagement_categ, p = .8, list = FALSE)

training_set <- base_table[ train_index,]
test_set  <- base_table[-train_index,]

train_data <- as.matrix(training_set[, -(1)])
test_data <- as.matrix(test_set[,-(1)])

train_label <- label[train_index]
test_label <- label[-(train_index)]

###############################################################
#
# CODE REFERENCE: https://www.rdocumentation.org/packages/nnet/versions/7.3-12/topics/multinom?
#
###############################################################

mn.net <- nnet::multinom(engagement_categ ~ ., training_set)
pred <- predict(mn.net, newdata=test_set, type="prob")
#max_pred = apply(pred,1,function(x) colnames(pred)[which.max(x)])
pred<- data.frame(pred)

colnames(pred) <- paste(colnames(pred), "_pred_MN")

true_label <- dummies::dummy(test_label)
true_label <- data.frame(true_label)

colnames(true_label) <- paste(colnames(true_label), "_true")

final_df <- cbind(true_label, pred)

predictions <- final_df[,(5:8)]
labels <- final_df[,(1:4)]
# Calculate auc
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; library("ROCR")
predML <- prediction(predictions,labels)
# ROC curve
perfML <- performance(predML,"tpr","fpr")
plot(perfML)

## auc
auc.perfML = performance(predML, measure = "auc")
auc.perfML@y.values

plot(perfML,
     colorize=TRUE,
     lwd=2,
     main='ROC curves')
abline(0,1)
