rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv",  stringsAsFactors = T)
L <- as.data.frame(c(1:49))
set.seed(29506751) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

library(tidyverse)
library(dplyr)

## Cleaning missing data
## Date
WAUS$Date <- as.Date(with(WAUS, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

# 1. Exploring data

# clearing missing data for max temp
WAUS_temp <- select(WAUS, Date,MaxTemp) %>% arrange(Date)  %>% filter(, complete.cases(.))

# temperature difference between a day and the previous day
WAUS_temp$temp_diff <- ave(WAUS_temp$MaxTemp, FUN=function(x) c(NA,diff(x)))
# if the difference is negative, that means that the 'next' day was cooler than
# the previous day. Otherwise, vice-versa
WAUS_temp$warm_cool = ifelse(WAUS_temp$temp_diff > 0, "Warmer", "Cooler")

# calculating percent of days that are warmer or cooler
warm_days <- table(WAUS_temp$warm_cool)[2] # number of warmer days
warm_days <- round((warm_days/count(WAUS_temp))*100, digits = 2) # percentage
cooler_days <- table(WAUS_temp$warm_cool)[1] # number of cooler days
cooler_days <- round((cooler_days/count(WAUS_temp))*100,  digits = 2) # percentage

# 2 Pre-processing data
# removing data that has NA values
# ommitting rows
WAUS <- WAUS %>% filter(, complete.cases(.))
WAUS_data <- select(WAUS, Date, Rainfall, Evaporation, WarmerTomorrow,Sunshine)
#WAUS_data$WindSpeed <-apply(WAUS[,14:15],1,mean)
WAUS_data$Humidity <-apply(WAUS[,16:17],1,mean)
#WAUS_data$Pressure <-apply(WAUS[,18:19],1,mean)
WAUS_data$Cloud <-apply(WAUS[,20:21],1,mean)
WAUS_data$AverageTemp <-apply(WAUS[,22:23],1,mean)
WarmerTomorrow = ifelse(WAUS$WarmerTomorrow == 1, "yes", "no")
WarmerTomorrow = as.factor(WarmerTomorrow)
WAUS_data$WarmerTomorrow <- WarmerTomorrow

# 3 dividing data into training and testing set
set.seed(29506751) #Student ID as random seed
train.row = sample(1:nrow(WAUS_data), 0.7*nrow(WAUS_data))
WAUS_data.train = WAUS_data[train.row,]
WAUS_data.test = WAUS_data[-train.row,]


# get larger training data set
# set.seed(9999)
# WAUS_data.train  = train[sample(nrow(train ), 100, replace = TRUE),]
# fit tree model
# 4 Decission Tree
# fit decision tree model to predict price_level
library(tree)
library(adabag)
library(neuralnet)
WAUS_data.fit = tree(WarmerTomorrow ~.-Date, WAUS_data.train)
summary(WAUS_data.fit)
plot(WAUS_data.fit)
text(WAUS_data.fit, pretty = 0,cex= 0.5)

#pruning to remove overfitting
testptfit = cv.tree(WAUS_data.fit, FUN = prune.misclass)
prune.ptfit = prune.misclass(WAUS_data.fit, best = 3)
summary(prune.ptfit)
plot(prune.ptfit)
text(prune.ptfit, pretty = 0)


#####
set.seed(9999) #random seed
# resampling with replacement to create a larger training set
pttrain = WAUS_data.train[sample(nrow(WAUS_data.train), 100, replace = TRUE),]
# fitting the model
ptfit = tree::tree(WarmerTomorrow ~.-Date, data=pttrain)
ptfit
summary(ptfit)
plot(ptfit)
text(ptfit, pretty = 0,cex= 0.5)
# making predictions from test data
tpredict = predict(ptfit, WAUS_data.test, type = "class")
tpredict
WAUS_data.test$WarmerTomorrow
table(actual = WAUS_data.test$WarmerTomorrow, predicted = tpredict)

# now do a cross validation test at different tree sizes

# Note that CV is not working properly on Version 3.6.3 for JB

testptfit = cv.tree(ptfit, FUN = prune.misclass, K = 10)

testptfit
prune.ptfit = prune.misclass(ptfit, best = 3)
summary(prune.ptfit)
plot(prune.ptfit)
text(prune.ptfit, pretty = 0)


ppredict = predict(prune.ptfit, pttest, type = "class")
table(actual = pttest$Play, predicted = ppredict)
ppredict = predict(prune.ptfit, pttest, type = "vector")
ppredict

####
## Naive Bayers
library(e1071)
WAUS_data.bayes = naiveBayes(WarmerTomorrow ~.-Date, WAUS_data.train)
WAUS_data.predbayes = predict(WAUS_data.bayes, WAUS_data.test)
table(actual = WAUS_data.test$WarmerTomorrow, predicted = tbpredict)
tbpredict.r = predict(tmodel, WAUS_data.test, type = 'raw')

####
WAUS_data.bayes = naiveBayes(WarmerTomorrow ~.-Date, WAUS_data.train)
WAUS_data.predbayes = predict(WAUS_data.bayes, WAUS_data.test)

## Bagging
install.packages("adabag")
library(adabag)
library(rpart)
WAUS_data.bag <- bagging(WarmerTomorrow ~.-Date, WAUS_data.train,mfinal=5)
WAUS_data_pred.bag <- predict.bagging(WAUS_data.bag, WAUS_data.test)


# Boosting
WAUS_data.boost <- boosting(WarmerTomorrow ~.-Date,  data=WAUS_data, mfinal=10)

# Random Forest
install.packages("randomForest") 
library(randomForest)
WAUS_data.rf <- randomForest(WarmerTomorrow ~.-Date,  data=WAUS_data, na.action = na.exclude)
plot(randomForest(WarmerTomorrow ~ ., data=WAUS_data[sub,], keep.forest=FALSE, ntree=100))
