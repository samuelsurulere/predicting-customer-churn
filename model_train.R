
#################### Loading necessary packages and data ###################

# install.packages('caret')
# install.packages("caret", dependencies=c("Depends", "Suggests"))
# install.packages("cvms")
# install.packages("yardstick")
library(caret)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(pROC)
library(yardstick)

# Reading the dataset into a data.frame
data <- read.csv('Customer Churn.csv')

# Viewing important information regarding the data
str(data)
summary(data)
glimpse(data)


########################### Data Preprocessing ############################

# Dropping irrelevant columns that makes no sense
# The Age column was dropped because, the values were categorical and not 
# continuous
data$FN <- NULL
data$FP <- NULL
data$Age <- NULL


# Renaming the column names
colnames(data) <- c("call.failure","complains","subscription.len",
                    "charge.cost","usage.secs","freq.of.use",
                    "freq.of.sms","numbers.dialed","age.group",
                    "tariff.plan","status","customer.value","churn")

# Checking for missing values in the dataset
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

# Checking the defined levels of Churn column
levels(data$churn)

# Reassigning readable description to the Churn levels
data$churn <- factor(data$churn, 
                     levels=c(0, 1), 
                     labels=c("non.churn", "churn"))

glimpse(data)

# List the types for each feature
sapply(data, class)



######################### Some EDA visualizations ##########################

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



########################### Modeling process ############################

# Creating a partitioned list of 80% of the rows in the original data
index <- createDataPartition(data$churn, p=0.80, list=FALSE)
# 20% of the data assigned for testing and evaluating the models
test_data <- data[-index,]
# 80% of the data assigned to training the models
train_data <- data[index,]

# Checking dimensions of the split data
dim(train_data)
dim(test_data)

# Running algorithms using 10-fold cross validation
control <- trainControl(method="cv", search="grid", 
                        summaryFunction=twoClassSummary, 
                        classProbs=T)
# Since the Churn feature is quite imbalanced, Accuracy will not be an 
# appropriate measure of model performance
metric <- "ROC"


## Comparing training on different models
# LDA
set.seed(42) # Setting seed for reproducibility
fit.lda <- train(churn~., data=train_data, 
                 method="lda", metric=metric, 
                 trControl=control)
# CART
set.seed(42) # Setting seed for reproducibility
fit.cart <- train(churn~., data=train_data, 
                  method="rpart", metric=metric, 
                  trControl=control)
# kNN
set.seed(42) # Setting seed for reproducibility
fit.knn <- train(churn~., data=train_data, 
                 method="knn", metric=metric, 
                 trControl=control)
# SVM
set.seed(42) # Setting seed for reproducibility
fit.svm <- train(churn~., data=train_data, 
                 method="svmRadial", metric=metric, 
                 trControl=control)
# Random Forest
set.seed(42) # Setting seed for reproducibility
fit.rf <- train(churn~., data=train_data, 
                method="rf", metric=metric, 
                trControl=control)


# Display the summary of the accuracy of the trained models
summary(resamples(list(lda=fit.lda, cart=fit.cart, 
                       knn=fit.knn, svm=fit.svm, rf=fit.rf)))

## The RandomForest model was the best performing model
# Summarize the best performing model
print(fit.rf)

# Plot the model
plot(fit.rf)

# Estimate the efficiency of Random Forest model on the test data
predictions <- predict(fit.rf, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$churn, positive="churn")
print(conf_matrix)


# Attempting to plot the confusion matrix but failed
autoplot(conf_mat(test_data$churn, .predictions), type="heatmap") +
  scale_fill_gradient(low="pink", high="cyan")


# Feature importance evaluation for the best performing model
plot(varImp(fit.rf), top=10, 
     main='Variable Importance Plot')

ggplot(varImp(fit.rf))


####################### Optimizing model performance ########################

## Fine-tuning the model
# Defining the parameter grid
control <- trainControl(method="LGOCV", 
                        summaryFunction=twoClassSummary,
                        classProbs=TRUE)

grid <- expand.grid(.mtry=c(1:sqrt(ncol(data))),
                    .splitrule=c("gini", "extratrees", "hellinger"),
                    .min.node.size=c(0.5, 1, 5, 10))

# Performing grid search cross-validation
optimizedModel <- train(churn~., 
                        data=train_data,
                        method="ranger", metric="ROC",
                        trControl=control,
                        tuneGrid=grid)

print(optimizedModel)
plot(optimizedModel)



########################### Model validation ############################

# Estimate the efficiency of Random Forest model on the test data
predictions <- predict(optimizedModel, test_data)
confusionMatrix(predictions, test_data$churn, positive="churn")


## Construct the ROC Curve and then calculate the Area Under the Curve (AUC)
# Calculating probabilities for test data
RFprob <- predict(optimizedModel, test_data, type="prob")

# Calculating the ROC Curve values
ROC_RF <- roc(test_data$churn, RFprob[,"churn"],
              levels=rev(levels(test_data$churn)))

names(ROC_RF)

# Calculating the Area Under the ROC Curve
auc <- auc(ROC_RF)
ci <- ci.auc(ROC_RF)
ci_lower <- round(ci[1] * 100, 1)
ci_upper <- round(ci[3] * 100, 1)

# Display the ROC curve
ggroc(ROC_RF, col="blue", size=1.) +
  ggtitle(paste0('Receiver Operating Characteristic Curve ')) + 
  geom_segment(
    aes(x=1, xend=0, y=0, yend=1), color="black", linetype="dashed"
  ) + 
  annotate("text", x=0.3, y=0.05, label=paste0(
    "AUC = ", round(auc * 100, 1),"%", " (95% CI = ", ci_lower, "% - ", ci_upper, "%)"
  ))


# Printing the Area Under the ROC
cat('Area under the ROC curve is', 
    round(auc * 100, 1),"%", '\n')






