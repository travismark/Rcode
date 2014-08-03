# July 2014
# Kaggle Bike Share competition

require('lubridate') # for handling datetimes
require(MASS) # for fitting

####### LOAD THE DATA
setwd("C:/Users/tmbae_000/Desktop/programming/kaggle/bikeshare")
testallbike<-read.csv("test.csv",stringsAsFactors=FALSE)
trainallbike<-read.csv("train.csv",stringsAsFactors=FALSE)
# consolidate test and train for data manipulation
testallbike["casual"]=NA;testallbike["registered"]=NA;testallbike["count"]=NA
testallbike["test"]=1
trainallbike["test"]=0
alldatabike<-rbind(testallbike,trainallbike)
# add date/time field(s)
alldatabike$datetime<-ymd_hms(alldatabike$datetime, tz='UTC')
sum(minute(alldatabike$datetime)) # =0; prove all times are on the hour
alldatabike$hour<-hour(alldatabike$datetime)
alldatabike$year<-year(alldatabike$datetime)
alldatabike$month<-month(alldatabike$datetime)
alldatabike$day<-day(alldatabike$datetime)
# make season factor
alldatabike$season<-as.factor(alldatabike$season)

trainallbike<-alldatabike[which(alldatabike$test==0),]
testallbike<-alldatabike[which(alldatabike$test==1),]
#explore
aggregate(count ~ hour + workingday,data=trainallbike, FUN=mean)
aggregate(count ~ weather,data=trainallbike, FUN=mean)
hist(trainallbike$count)
ce<-fitdistr(trainallbike$count, "exponential")
cp<-fitdistr(trainallbike$count, "Poisson")
pairs(alldatabike)

#first model using all data (cheating)
a1<-lm(count~atemp,data=trainallbike)
abline(a1)
a2<-lm(count ~ atemp + windspeed + humidity +
         hour + workingday + holiday + season, data=trainallbike)
summary(a1)
summary(a2)
p1<-predict(a1,newdata=testallbike)
p2<-predict(a2,newdata=testallbike)

# ridge
library(ridge)
a3<-linearRidge(count ~ atemp + windspeed + humidity +
         hour + workingday + holiday + season, data=trainallbike)
summary(a3)
p3<-predict(a3,newdata=testallbike)

# poisson regression for count data
a4<-glm(count ~ atemp + windspeed + humidity +
          hour + workingday + holiday + season, data=trainallbike,
        family='poisson')

xTrain<-trainallbike
xTrain$count<-NULL;xTrain$registered<-NULL;xTrain$casual<-NULL # remove y
xTrain$datetime<-NULL # remove datetime, which is in year/month/day fields
yTrain<-trainallbike[,c('count','casual','registered')]
# gbm
require(gbm)
a5<-gbm.fit(y=yTrain,x=xTrain, distribution='poisson')
