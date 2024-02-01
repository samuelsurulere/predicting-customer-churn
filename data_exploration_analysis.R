
######################## Loading libraries and data ##########################

# install.packages("ggvis")
# install.packages("rstatix")
# install.packages("lares")
# install.packages("DataExplorer")
# install.packages("tableone")
# install.packages("viridis")
# library('ggvis')
# library(tableone)
library(rstatix)
library(corrplot)
library(ggplot2)
library(lares)
library(DataExplorer)
library(dplyr)
library(viridis)


# Reading the dataset into a data.frame
# Dataset was downloaded from 
# https://archive.ics.uci.edu/dataset/563/iranian+churn+dataset

data <- read.csv('Customer Churn.csv')

# Viewing details of the dataset
head(data)
str(data)
glimpse(data)

###################### Data cleaning and preprocessing ######################

# Information about columns

# Call Failures: number of call failures
# Complains: binary (0: No complaint, 1: complaint)
# Subscription Length: total months of subscription
# Charge Amount: Ordinal attribute (0: lowest amount, 9: highest amount)
# Seconds of Use: total seconds of calls
# Frequency of use: total number of calls
# Frequency of SMS: total number of text messages
# Distinct Called Numbers: total number of distinct phone calls 
# Age Group: ordinal attribute (1: younger age, 5: older age)
# Tariff Plan: binary (1: Pay as you go, 2: contractual)
# Status: binary (1: active, 2: non-active)
# Churn: binary (1: churn, 0: non-churn) - Class label
# Customer Value: The calculated value of customer


# FN and FP columns will be deleted as their is no information about them 
# from the origin of the dataset. The Age column is also deleted because 
# Age is meant to be a continuous variable. But Age here only has 5 
# distinct discrete values.
data$FN <- NULL
data$FP <- NULL
data$Age <- NULL

# Renaming the column names
colnames(data) <- c("callFailure","complains","subscriptionLen",
                    "chargeCost","usageSecs","freqOfUse",
                    "freqOfSMS","numbersDialed","ageGroup",
                    "tariffPlan","status","customerValue","churn")
head(data)

# Checking for missing values
sum(is.na(data))

colSums(is.na(data))

##############################################################################
##############################################################################
#
# Arising questions this analysis aims to seek answers to:
#
# 1. What could be the main causes of customer churn?
# 2. What is the time interval for customers to churn?
# 3. From insights generated from the EDA process, is it possible to discover 
# how customers can be retained?
# 4. What recommendations observed from the EDA process can avoid customer
# churn?
# 5. What other insights and interesting information can be observed from the 
# data?
#
##############################################################################
##############################################################################

# Decoding the numbers assigned to categorical variables
data.eda <- data %>%
  mutate_at(c("ageGroup"), ~ recode(.,
                                     "1"="youngest age",
                                     "2"="young age",
                                     "3"="middle age",
                                     "4"="old age",
                                     "5"="older age")
  ) %>%
  mutate_at(c("complains"), ~ recode(.,
                                     "0"="No complaint",
                                     "1"="complaint")
  ) %>%
  mutate_at(c("status"), ~ recode(.,
                                  "1"="active",
                                  "2"="non-active")
  ) %>%
  mutate_at(c("tariffPlan"), ~ recode(.,
                                       "1"="Pay as you go",
                                       "2"="contractual")
  ) %>%
  mutate_at(c("churn"), ~ recode(.,
                                 "0"="non-churn",
                                 "1"="churn")
  )


# Converting categorical features disguised as numerical features to 
# categorical and/or ordinal features
data.eda$complains <- factor(data.eda$complains)
data.eda$chargeCost <- factor(data.eda$chargeCost)
data.eda$ageGroup <- factor(data.eda$ageGroup, 
                             levels=c("youngest age", "young age", 
                                        "middle age", "old age",
                                        "older age"))
data.eda$tariffPlan <- factor(data.eda$tariffPlan)
data.eda$status <- factor(data.eda$status)
data.eda$churn <- factor(data.eda$churn, 
                         levels=c("non-churn", "churn"))

head(data.eda)
str(data.eda)

levels(data.eda$churn)
levels(data$churn)

data$churn <- factor(data$churn)
head(data)

# PS. There are two data.frames in this script. The first dataset (data) only
# contains numerical features. The categorical features are hidden as numbers.
# The second dataset (data.eda) properly represents both categorical as well as
# numerical features. Depending on the task, either of both will be used.


######################## Exploratory Data Analysis ##########################

# Bar plots for categorical features
barPlot <- function(Data, col, custom_colors=NULL){
  ggplot(data=Data, aes(x=.data[[col]], fill=churn)) +
    geom_bar(position="dodge", width=0.7, stat="count") +
    scale_fill_manual(values=custom_colors)
}
# Defining custom colours for aesthetics
my_custom_colors <- c("purple", "pink", "#3366FF", "#FF33B2",
                      "cyan", "magenta", "maroon")

barPlot(Data=data.eda, col="churn", 
        custom_colors=my_custom_colors)
barPlot(Data=data.eda, col="status", 
        custom_colors=my_custom_colors)
barPlot(Data=data.eda, col="complains", 
        custom_colors=my_custom_colors)
barPlot(Data=data.eda, col="ageGroup", 
        custom_colors=my_custom_colors)
barPlot(Data=data.eda, col="tariffPlan", 
        custom_colors=my_custom_colors)
barPlot(Data=data.eda, col="chargeCost", 
        custom_colors=my_custom_colors)


# Pie chart for analyzes of Age.Group 
ggplot(data=data.eda, aes(x="", fill=ageGroup)) + 
  geom_bar(width=1, stat="count", colour="black") +
  coord_polar(theta="y") +
  scale_fill_manual(values=my_custom_colors) + 
  labs(x=NULL, y=NULL, fill=NULL, title="Age group distribution")

#data.eda %>%
#  count(Age.Group) %>%
#  ggplot(aes(x="", y=Age.Group, fill=Age.Group)) + 
#  geom_bar(width=1, stat="identity", colour="black") +
#  coord_polar(theta="y") +
#  scale_fill_manual(values=my_custom_colors) +
#  geom_text(aes(label=paste0(round((n/sum(n)) * 100, 1), "%")), 
#            position=position_stack(vjust=0.5), stat="identity") + 
#  labs(x=NULL, y=NULL, fill=NULL, title="Age group distribution")


# Histogram plots for numerical features
histPlot <- function(Data, col, custom_colors=NULL) {
  ggplot(data=Data, aes(x=.data[[col]], fill=churn)) +
    geom_histogram(binwidth=7, color='black', position='identity') +
    scale_fill_manual(values=custom_colors)
}

churnData <- filter(data.eda, churn=="churn")

# churnData <- data.eda[data.eda$Churn=="churn", 13]


histPlot(Data=data.eda, col="subscriptionLen", 
         custom_colors=my_custom_colors)
histPlot(Data=churnData, col="subscriptionLen", 
         custom_colors=my_custom_colors)
# The Subscription...Length column seems to be left-skewed

histPlot(Data=data.eda, col="numbersDialed",
         custom_colors=my_custom_colors)
# The Distinct.Called.Numbers column is right-skewed 

histPlot(Data=data.eda, col="callFailure", 
         custom_colors=my_custom_colors)
# Call failures does not seem to be a determinant of customers who churned 
# The no call failures bar (0) had the highest churned customers

histPlot(Data=data.eda, col="freqOfUse", 
         custom_colors=my_custom_colors)
# The distribution of the Frequency of use column is right-skewed

histPlot(Data=data.eda, col="freqOfSMS", 
         custom_colors=my_custom_colors)
# The distribution of the Frequency of SMS column is right-skewed


# Scatter plots for comparing numerical features
scatterPlot <- function(xcol, ycol, sizeCol=NULL, custom_colors=NULL){
  if (!is.null(sizeCol)){
    ggplot(data=data.eda, aes(x=.data[[xcol]], y=.data[[ycol]], 
                          color=churn, size=.data[[sizeCol]])) +
      geom_point() + scale_color_manual(values=custom_colors) +
      scale_size()
  } else {
    ggplot(data=data.eda, aes(x=.data[[xcol]], y=.data[[ycol]], 
                              color=churn)) +
      geom_point() + scale_color_manual(values=custom_colors)
  }
}

my_custom_colors <- c("seagreen", "blue")

scatterPlot(xcol="subscriptionLen", ycol="freqOfUse",
            custom_colors=my_custom_colors)
scatterPlot(xcol="usageSecs", ycol="freqOfUse",
            custom_colors=my_custom_colors)
scatterPlot(xcol="subscriptionLen", ycol="numbersDialed",
            custom_colors=my_custom_colors)
scatterPlot(xcol="freqOfSMS", ycol="customerValue",
            custom_colors=my_custom_colors)
scatterPlot(xcol="freqOfUse", ycol="numbersDialed",
            custom_colors=my_custom_colors)
scatterPlot(xcol="callFailure", ycol="chargeCost",
            custom_colors=my_custom_colors)
scatterPlot(xcol="callFailure", ycol="numbersDialed",
            custom_colors=my_custom_colors)
scatterPlot(xcol="callFailure", ycol="usageSecs",
            custom_colors=my_custom_colors)
scatterPlot(xcol="usageSecs", ycol="chargeAmount",
            custom_colors=my_custom_colors)
scatterPlot(xcol="subscriptionLen", ycol="callFailure",
            custom_colors=my_custom_colors)
scatterPlot(xcol="subscriptionLen", ycol="customerValue",
            custom_colors=my_custom_colors)



# Box plots for detecting outliers
boxPlot <- function(xcol, ycol){
  ggplot(data=data.eda, 
         aes(x=.data[[xcol]], y=.data[[ycol]], colour=.data[[xcol]])) + 
    geom_jitter(position=position_jitter(0.3)) +
    geom_boxplot(alpha=0.5, size=0.5)
}

boxPlot(xcol="churn", ycol="subscriptionLen")
boxPlot(xcol="complains", ycol="subscriptionLen")
boxPlot(xcol="status", ycol="subscriptionLen")
boxPlot(xcol="ageGroup", ycol="subscriptionLen")
boxPlot(xcol="ageGroup", ycol="callFailure")
boxPlot(xcol="ageGroup", ycol="usageSecs")
boxPlot(xcol="ageGroup", ycol="numbersDialed")
boxPlot(xcol="chargeCost", ycol="numbersDialed")
boxPlot(xcol="chargeCost", ycol="callFailure")


# Violin plots for observing the distribution of data
violinPlot <- function(xcol, ycol){
  ggplot(data=data.eda, 
         aes(x=.data[[xcol]], y=.data[[ycol]], fill=.data[[xcol]])) +
    geom_violin() +
    geom_boxplot(width=.1, 
                 fill="orange",
                 outlier.color="orange",
                 outlier.size=1)
}

violinPlot(xcol="ageGroup", ycol="usageSecs")
# The width of the youngest age KDE means it contains more points around the 
# median. While the rest of the categories have an elongated distribution.
violinPlot(xcol="ageGroup", ycol="subscriptionLen")
# The distribution of all the categories are quite similar in this plot.
violinPlot(xcol="ageGroup", ycol="callFailure")
# Quite similar distributions again excluding the older age which has contains
# lesser points.
violinPlot(xcol="chargeCost", ycol="callFailure")
# Charge cost 0 to 5 are quite similar (elongated distributions). 6 to 10 have
# more points around their median.
violinPlot(xcol="churn", ycol="callFailure")
# Similar distribution.
violinPlot(xcol="churn", ycol="numbersDialed")
# non-churn has an elongated distribution while churn has a wider spread 
# (contains more points).


##############################################################################
##############################################################################
# Answers to arising questions:
#
# 1. What could be the main causes of customer churn?
# Answer: The data points to two main features as the cause of customer churn:
# - Complains (customers who have complains)
# - Status (non-active customers have the highest churn)
# It seems mostly likely that the customer complains are not attended
# to. Many of customers that had complains churned. Also the most majority of
# customers churned because they were no longer active on the platform. It was
# also observed that customers on the "pay-as-you-go" tariff plan churned the
# most compared to those on a contract.
# 2. What is the time interval for customers to churn?
# Answer: Customers begin to churn right from the time they subscribe to the 
# network. The highest churn rate (almost 300 customers) was between 30 and 40.
# 3. From insights generated from the EDA process, is it possible to discover 
# how customers can be retained?
# Answer: A major issue seems to be that the network is not attending to the 
# complains of the customers. A large number of customers churn due to this.
# 4. What recommendations observed from the EDA process can avoid customer
# churn?
# Answer: The data reveals that most of the customers that leave are subscribed
# in the "pay-as-you-go" tariff plan. Probably make the contractual plan the 
# gold standard. Customers on the contractual plan rarely churn.
# 5. What other insights and interesting information can be observed from the 
# data?
# - Customers that churned have less than 100 frequency of use and around 600
# seconds of use.
# - Customers that have customer value of 5000 and above almost never churn.
# - Customers that churn are the young age, middle age and old age. The oldest
# and youngest age do not churn.
# - The network charges does not influence customers' decision to churn.
# - The middle age are the largest age group in the dataset and they also have
# the highest churn rate.
# - The majority of customers that churn have a long subscription with the 
# network. There is something they are probably not happy about.
#
#
##############################################################################
##############################################################################


########################## Statistical Analysis #############################

data$churn <- as.numeric(data$churn)

# Correlation plots
corrplot(cor(data[,1:12]), method='square', order='AOE', 
         tl.col="black", tl.cex=0.8, tl.srt=70, 
         col=colorRampPalette(c("maroon", "pink", "seagreen"))(100))

cor_test <- cor_mat(data[,1:12])
cor_test

p_value <- cor_pmat(data[,1:12])
p_value


# Further Statistical analysis
corr_cross(data, max_pvalue=0.05, top=20, grid=T)

# ANOVA test to check how features are statistically significant with churn
two.way <- aov(churn~complains+status+tariffPlan+customerValue+subscriptionLen, 
               data=data)
summary(two.way)

# Combined plots for numerical and categorical features
plot_bar(data[,1:12], by="churn")
plot_prcomp(data[,1:12]) #, variance_cap=0.9, nrow=2L, ncol=2L
plot_qq(data.eda)
plot_histogram(data.eda)
plot_boxplot(data.eda, by="churn")

