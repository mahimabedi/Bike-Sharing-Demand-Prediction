#reading the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#formatting date variable
train$datetime <- strptime(train$datetime, "%Y-%m-%d %H:%M:%S")
test$datetime <- strptime(test$datetime, "%Y-%m-%d %H:%M:%S")

# creating date variables
train$hour<-train$datetime$hour
test$hour<-test$datetime$hour

train$month<-train$datetime$mon
test$month<-test$datetime$mon

# coverting to factors
train$season<-as.factor(train$season)
train$holiday<-as.factor(train$holiday)
train$workingday<-as.factor(train$workingday)
train$weather<-as.factor(train$weather)
train$hour<-as.factor(train$hour)
train$month<-as.factor(train$month)

test$season<-as.factor(test$season)
test$holiday<-as.factor(test$holiday)
test$workingday<-as.factor(test$workingday)
test$weather<-as.factor(test$weather)
test$hour<-as.factor(test$hour)
test$month<-as.factor(test$month)

# realigning factors variables
levels(train$season)<-levels(test$season)
levels(train$holiday)<-levels(test$holiday)
levels(train$workingday)<-levels(test$workingday)
levels(train$weather)<-levels(test$weather)
levels(train$hour)<-levels(test$hour)
levels(train$month)<-levels(test$month)

str(train)

# random forest models
library(randomForest)
library(ROCR)

# model 1
M1<-randomForest(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hour+month,data=train,mtry=5,nodesize=5,ntree=1000,importance=TRUE)
varImpPlot(M1,main="Model 1 Variable Importance",col="blue",pch=19)


predM1train<-predict(M1,newdata=train)
SSE<-sum((predM1train-train$count)^2)
SST<-sum((mean(train$count)-train$count)^2)
Rsq1<-1-(SSE/SST)
Rsq1
#R squared=0.0.9639838

predM1<-predict(M1,newdata=test)
#RMSLE=0.55284

# model 2: removing least important varibales
M2<-randomForest(count~workingday+weather+temp+atemp+humidity+hour+month,data=train,mtry=5,nodesize=5,ntree=1000,importance=TRUE)
varImpPlot(M2,main="Model 2 Variable Importance",col="blue",pch=19)

predM2train<-predict(M2,newdata=train)
SSE<-sum((predM2train-train$count)^2)
SST<-sum((mean(train$count)-train$count)^2)
Rsq2<-1-(SSE/SST)
Rsq2
#R squared= 0.967955

predM2<-predict(M2,newdata=test)
# RMSLE= 0.52538

submissionM2<- data.frame(datetime=test$datetime, count = predM2)
write.csv(submissionM2, "Prediction.csv", row.names=FALSE)

