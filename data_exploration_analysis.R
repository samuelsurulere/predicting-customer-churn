
# Loading packages

# install.packages("ggvis")
# install.packages("rstatix")
# install.packages("lares")
# install.packages("DataExplorer")
# install.packages("tableone")
# library('ggvis')
#library(tableone)
library(rstatix)
library(corrplot)
library(ggplot2)
library(lares)
library(DataExplorer)


# Reading the dataset into a data.frame
data <- read.csv('Customer Churn.csv', 
                 fileEncoding="UTF-8-BOM", 
                 na.strings = '..')

# Viewing details of the data.frame
head(data)

str(data)


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
# from the origin of the dataset
# The Age column is also deleted because Age is meant to be a continuous 
# variable. But Age here only has 5 distinct values

data$FN <- NULL
data$FP <- NULL

head(data)

# Checking for missing values
sum(is.na(data))

colSums(is.na(data))

##############################################################################
##############################################################################
# Arising questions:
#
# 1. What features are the main cause of customer churn? 
# 2. What is the longest time before customers churn?
# 3. Through EDA process, is it possible to discover how customers
# can be retained?
# 4. What recommendations observed from the EDA process can avoid customer
# churn?
# 5. What insights do interesting information from the data point towards?
# 6. 
#
#
#
#
#
#
#
##############################################################################
##############################################################################

# Decoding the numbers assigned to categorical variables
data.eda <- data %>%
  # modify factor levels
   mutate_at(c("Age.Group"), ~ recode(.,
                                      "1" = "youngest age",
                                      "2" = "young age",
                                      "3" = "middle age",
                                      "4" = "old age",
                                      "5" = "older age")
   ) %>%
   mutate_at(c("Complains"), ~ recode(.,
                                      "0" = "No complaint",
                                      "1" = "complaint")
   ) %>%
   mutate_at(c("Status"), ~ recode(.,
                                   "1" = "active",
                                   "2" = "non-active")
   ) %>%
   mutate_at(c("Tariff.Plan"), ~ recode(.,
                                        "1" = "Pay as you go",
                                        "2" = "contractual")
   )

# Converting some numerical values to categorical and ordinal variables 
data.eda$Complains <- factor(data$Complains)
data.eda$Charge..Amount <- factor(data$Charge..Amount)
data.eda$Age.Group <- factor(data$Age.Group)
data.eda$Tariff.Plan <- factor(data$Tariff.Plan)
data.eda$Status <- factor(data$Status)
data.eda$Churn <- factor(data$Churn)

head(data.eda)
str(data.eda)


data$Churn <- factor(data$Churn)
head(data)

ggplot(data=data, aes(x=Subscription..Length)) + 
  geom_histogram(binwidth=5, aes(fill=Churn), colour='Black')
# The Subscription...Length column seems to be left-skewed

ggplot(data=data, aes(x=Distinct.Called.Numbers)) + 
  geom_histogram(binwidth=10, aes(fill=Churn), colour='Black')
# The Distinct.Called.Numbers column is right-skewed

ggplot(data=data, aes(x=Call..Failure)) +
  geom_histogram(binwidth=5, aes(fill=Churn), colour='Black')
# Call failures does not seem to be a determinant of customers who churned 
# The no call failures bar (0) had the highest churned customers

ggplot(data=data, aes(x=Subscription..Length, y=Frequency.of.use, 
                      colour=Churn)) + geom_point()
# The customers that churned have less than 100 frequency of use and quite a
# number of customers churned early after subscribing to the network




ggplot(data=data, aes(x=Seconds.of.Use, y=Frequency.of.use, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Subscription..Length, y=Distinct.Called.Numbers, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Frequency.of.SMS, y=Customer.Value, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Frequency.of.use, y=Distinct.Called.Numbers, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Seconds.of.Use, y=Distinct.Called.Numbers, 
                      colour=Churn)) + geom_point()

ggplot(data=data.eda, aes(x=Call..Failure, y=Charge..Amount, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Call..Failure, y=Distinct.Called.Numbers, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Call..Failure, y=Seconds.of.Use, 
                      colour=Churn)) + geom_point()

ggplot(data=data, aes(x=Charge..Amount, y=Seconds.of.Use, 
                      colour=Churn)) + geom_point()






ggplot(data=data, aes(x=Subscription..Length, y=Customer.Value, 
                      colour=Churn)) + geom_point()
# The customers who churned have a value less than 500, except for two that had
# a value less than 1000

ggplot(data=data, aes(x=Subscription..Length, y=Seconds.of.Use, 
                      colour=Churn)) + geom_point()
# Most of the Customers that churned did so before clocking 5000 seconds of use
# The remaining few left after clocking between 5000 and 7500 seconds


ggplot(data=data, aes(x=Churn, y=Subscription..Length, 
                      color=Churn)) + 
  geom_jitter() + geom_boxplot(alpha=0.5, size=0.1)

ggplot(data=data.eda, aes(x=Age.Group, y=Subscription..Length, 
                          color=Age.Group)) + 
  geom_jitter() + geom_boxplot(alpha=0.5, size=0.1)

ggplot(data=data, aes(x=Distinct.Called.Numbers)) + 
  geom_bar(aes(fill=Churn), colour='Black') +
  coord_flip()



# Correlation plots
corrplot(cor(data[,1:12]))

cor_test <- cor_mat(data[,1:12])
cor_test

p_value <- cor_pmat(data[,1:12])
p_value


# Further statistical analysis
corr_cross(data, max_pvalue=0.05, top=20, grid=T)


plot_bar(data[,1:12], by="Churn")
plot_prcomp(data[,1:12]) #, variance_cap=0.9, nrow=2L, ncol=2L
plot_qq(data)
plot_histogram(data)
plot_boxplot(data, by="Churn")
plot_scatterplot(data, by="Churn") #, sampled_rows = 1000L

