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
test$pred<-predM2

submissionM2<- data.frame(datetime=test$datetime, count = predM2)
write.csv(submissionM2, "Prediction.csv", row.names=FALSE)

library(ggplot2)

train$day <- as.factor(train$datetime$wday)
summary(train$day)
levels(train$day)<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                     "Friday", "Saturday")
levels(train$month)<-c("Jan", "Feb", "Mar", "Apr", "May", 
                       "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
str(train)

test$day <- as.factor(test$datetime$wday)
levels(test$day)<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                     "Friday", "Saturday")
levels(test$month)<-c("Jan", "Feb", "Mar", "Apr", "May", 
                       "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")

#plot 1: Monthly Average Demand-Current and Prediction
monthlyavg<-tapply(train$count,train$month,mean)
monthlyavg<-as.data.frame(monthlyavg)
monthlyavg$month<-c("Jan", "Feb", "Mar", "Apr", "May", 
                    "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
monthlyavg$month<-factor(monthlyavg$month,c("Jan", "Feb", "Mar", "Apr", "May", 
                                            "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
monthlyavg$DataType="Train"
Data1<-monthlyavg

monthlyavg<-tapply(test$pred,test$month,mean)
monthlyavg<-as.data.frame(monthlyavg)
monthlyavg$month<-c("Jan", "Feb", "Mar", "Apr", "May", 
                    "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
monthlyavg$month<-factor(monthlyavg$month,c("Jan", "Feb", "Mar", "Apr", "May", 
                                            "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
monthlyavg$DataType="Test"
Data2<-monthlyavg
Monthly=rbind(Data1,Data2)
Monthly
ggplot(Monthly,aes(x=month,y=monthlyavg,group=DataType))+
  geom_point(aes(colour=DataType,size=monthlyavg))+
  ylab("Average demand") + 
  xlab("Month of the Year") +
  ggtitle("Average Monthly Demand")

#plot2: Weekday distribution of demand
cols=c("white","lightblue","blue")
ggplot(train, aes(x=day,y=month))+
  geom_tile(aes(fill=(log(count+0.0001))))+
  scale_fill_gradientn(colours=cols)+
  xlab("Day of the week") + 
  ylab("Month of the Year") +
  ggtitle("Weekday Distribution of Demand")+
  guides(fill=guide_legend(title="Log of count"))

#plot3: Monthly and Hourly
cols=c("lightblue","darkblue")
ggplot(train, aes(x=hour,y=month))+
  geom_tile(aes(fill=count))+
  scale_fill_gradientn(colours=cols) +
  xlab("Hour of the Day") + 
  ylab("Month of the Year") +
  ggtitle("Monthly and Hourly Breakdown of Demand: Increased Demand in May Afternoons")

#plot4: Non-workday bike rentals
ggplot(train[train$workingday==0,], aes(x=hour,y=count,color=temp))+
  geom_point(aes(size=4))+
  scale_colour_gradient(low = "yellow",high="red")+
  xlab("Hour of the Day") + 
  ylab("Number of Bike Rentals") +
  ggtitle("Non-workday Bike Rentals: Highest During Warm Afternoons")

#plot5: Workday bike rentals
ggplot(train[train$workingday==1,], aes(x=hour,y=count,color=temp))+
  geom_point(aes(size=4))+
  scale_colour_gradient(low = "yellow",high="red")+
  xlab("Hour of the Day") + 
  ylab("Number of Bike Rentals") +
  ggtitle("Workday Bike Rentals: Highest During Morning and Evenings")
