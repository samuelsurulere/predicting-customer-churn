
# Loading necessary packages

# install.packages('caret')
# install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(pROC)

# Reading the dataset into a data.frame
data <- read.csv('Customer Churn.csv')

# Viewing important information regarding the data
str(data)
summary(data)
glimpse(data)

# Dropping irrelevant columns that makes no sense
data$FN <- NULL
data$FP <- NULL
data$Age <- NULL

# Rename the column names
colnames(data) <- c("call.failure","complains","subscription.len",
                    "charge.cost","usage.secs","freq.of.use",
                    "freq.of.sms","numbers.dialed","age.group",
                    "tariff.plan","status","customer.value","churn")

colSums(is.na(data))

head(data)

# Converting categorical features disguised as numerical features
# to categorical features

# data$complains <- as.factor(data$complains)
# data$charge.cost <- as.factor(data$charge.cost)
# data$usage.secs <- as.factor(data$usage.secs)
# data$tariff.plan <- as.factor(data$tariff.plan)
# data$status <- as.factor(data$status)
data$churn <- factor(data$churn)


levels(data$churn)


data$churn <- factor(data$churn, 
                     levels=c(0, 1), 
                     labels=c("active", "non.active"))

glimpse(data)

# list types for each attribute
sapply(data, class)


# Bar charts for categorical features
bar1 <- ggplot(data=data) + geom_bar(aes(x=complains, fill=churn)) + 
  scale_fill_manual(values = c("#ff66b2", "#c77dff")) 
bar2 <- ggplot(data=data) + geom_bar(aes(x=charge.cost, fill=churn)) + 
  scale_fill_manual(values = c("#ff66b2", "#c77dff")) 
bar3 <- ggplot(data=data) + geom_bar(aes(x=age.group, fill=churn)) + 
  scale_fill_manual(values = c("#ff66b2", "#c77dff"))
bar4 <- ggplot(data=data) + geom_bar(aes(x=tariff.plan, fill=churn)) + 
  scale_fill_manual(values = c("#ff66b2", "#c77dff")) 
bar5 <- ggplot(data=data) + geom_bar(aes(x=status, fill=churn)) + 
  scale_fill_manual(values = c("#ff66b2", "#c77dff")) 

grid.arrange(bar1, bar2, bar3, bar4, bar5, ncol=2)


# Box plots for numerical features
box1 <- ggplot(data=data) + geom_boxplot(aes(x=call.failure, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none") 
box2 <- ggplot(data=data) + geom_boxplot(aes(x=subscription.len, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none")
box3 <- ggplot(data=data) + geom_boxplot(aes(x=charge.cost, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none")
box4 <- ggplot(data=data) + geom_boxplot(aes(x=usage.secs, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none")
box5 <- ggplot(data=data) + geom_boxplot(aes(x=customer.value, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none") 
box6 <- ggplot(data=data) + geom_boxplot(aes(x=freq.of.use, fill=churn)) + 
  scale_fill_manual(values = c("#00ff80", "#9999ff")) +
  theme(legend.position="none") 

grid.arrange(box1, box2, box3, box4, box5, box6, ncol=2)


# Dealing with outliers by clipping using the lower and upper quantiles.
clipping <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

# Applying defined functions to numerical features containing outliers
data$call.failure <- clipping(data$call.failure)
data$subscription.len <- clipping(data$subscription.len)
data$charge.cost <- clipping(data$charge.cost)
data$usage.secs <- clipping(data$usage.secs)
data$customer.value <- clipping(data$customer.value)
data$freq.of.use <- clipping(data$freq.of.use)
data$freq.of.sms <- clipping(data$freq.of.sms)



# Create a partitioned list of 80% of the rows in the original data
index <- createDataPartition(data$churn, p=0.80, list=FALSE)
# 20% of the data assigned for testing and evaluating the models
test_data <- data[-index,]
# 80% of the data assigned to training the models
train_data <- data[index,]

# Checking dimensions of the split data
dim(train_data)
dim(test_data)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", search="grid", 
                        summaryFunction=twoClassSummary, 
                        classProbs=T)
metric <- "ROC"


# Training on different models
# LDA
set.seed(42)
fit.lda <- train(churn~., data=train_data, 
                 method="lda", metric=metric, 
                 trControl=control)
# CART
set.seed(42)
fit.cart <- train(churn~., data=train_data, 
                  method="rpart", metric=metric, 
                  trControl=control)
# kNN
set.seed(42)
fit.knn <- train(churn~., data=train_data, 
                 method="knn", metric=metric, 
                 trControl=control)
# SVM
set.seed(42)
fit.svm <- train(churn~., data=train_data, 
                 method="svmRadial", metric=metric, 
                 trControl=control)
# Random Forest
set.seed(42)
fit.rf <- train(churn~., data=train_data, 
                method="rf", metric=metric, 
                trControl=control)


# Display the summary of the accuracy of the trained models
summary(resamples(list(lda=fit.lda, cart=fit.cart, 
                       knn=fit.knn, svm=fit.svm, rf=fit.rf)))

# Summarize the best performing model
print(fit.rf)
plot(fit.rf)

# Estimate the efficiency of Random Forest model on the test data
predictions <- predict(fit.rf, test_data)
confusionMatrix(predictions, test_data$churn, positive="non.active")

# variable importance evaluation for the Random Forest
plot(varImp(fit.rf), top=10, 
     main='Variable Importance Plot')


## Fine-tuning the model
# Define the parameter grid
control <- trainControl(method="LGOCV", 
                        summaryFunction=twoClassSummary,
                        classProbs=TRUE)

grid <- expand.grid(.mtry=c(1:sqrt(ncol(data))),
                    .splitrule=c("gini", "extratrees", "hellinger"),
                    .min.node.size=c(0.5, 1, 5, 10))

# Perform grid search cross-validation
optimizedModel <- train(churn~., 
                        data=train_data,
                        method="ranger", metric="ROC",
                        trControl=control,
                        tuneGrid=grid)

print(optimizedModel)
plot(optimizedModel)

# Estimate the efficiency of Random Forest model on the test data
predictions <- predict(optimizedModel, test_data)
confusionMatrix(predictions, test_data$churn, positive="non.active")

# Construct the ROC Curve and then calculate the Area Under the Curve (AUC)
# for the initial simple RF (with 500 trees).

RFprob <- predict(optimizedModel, test_data, type="prob")
ROC_RF <- roc(test_data$churn, RFprob[,"non.active"],
              levels=rev(levels(test_data$churn)))

# display the ROC curve just constructed
plot(ROC_RF, col="blue", lwd=3, 
     main="ROC curve for optimized model")

# calculate the Area Under the ROC
cat('Area under the ROC curve', 
    round(auc(ROC_RF) * 100, 1), '\n')






