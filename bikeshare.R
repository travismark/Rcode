# July 2014
# Kaggle Bike Share competition

require('lubridate') # for handling datetimes
require(MASS) # for fitting
require(metrics) # for RMSLE

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
xTrain$test<-NULL # remove test classifier
yTrain<-trainallbike[,c('count','casual','registered')]
# gbm
require(gbm)
a5<-gbm.fit(y=trainallbike$casual,x=xTrain, distribution='poisson',n.trees=1000)
a51<-gbm.fit(y=trainallbike$casual,x=xTrain, distribution='gaussian',n.trees=1000)
a5<-gbm(casual~atemp + windspeed + humidity +
          hour + workingday + holiday + season,
        data=trainallbike,distribution="poisson")
tP1<-predict.gbm(object=a5,g.newdata=xTrain,n.trees=1000)
# gbm is being weird, try random forest
a6<-randomForest(casual~atemp + windspeed + humidity +
                   hour + workingday + holiday + season,
                 data=trainallbike,distribution="poisson")

# split up the data to make a prediction - 24 months
table(alldatabike$month,alldatabike$year)
for(ii in unique(alldatabike$year) {
  for(jj in unique(alldatabike$month)) {
    # ? need to make the name of the file here on the fly
  }
}
## brute force (cascading size)
trainbike201101<-trainallbike[which(trainallbike$month==1),][which(trainallbike[which(trainallbike$month==1),]$year==2011),]
trainbike201102<-rbind(trainbike201101,trainallbike[which(trainallbike$month==2),][which(trainallbike[which(trainallbike$month==2),]$year==2011),])
trainbike201103<-rbind(trainbike201102,trainallbike[which(trainallbike$month==3),][which(trainallbike[which(trainallbike$month==3),]$year==2011),])
trainbike201104<-rbind(trainbike201103,trainallbike[which(trainallbike$month==4),][which(trainallbike[which(trainallbike$month==4),]$year==2011),])
trainbike201105<-rbind(trainbike201104,trainallbike[which(trainallbike$month==5),][which(trainallbike[which(trainallbike$month==5),]$year==2011),])
trainbike201106<-rbind(trainbike201105,trainallbike[which(trainallbike$month==6),][which(trainallbike[which(trainallbike$month==6),]$year==2011),])
trainbike201107<-rbind(trainbike201106,trainallbike[which(trainallbike$month==7),][which(trainallbike[which(trainallbike$month==7),]$year==2011),])
trainbike201108<-rbind(trainbike201107,trainallbike[which(trainallbike$month==8),][which(trainallbike[which(trainallbike$month==8),]$year==2011),])
trainbike201109<-rbind(trainbike201108,trainallbike[which(trainallbike$month==9),][which(trainallbike[which(trainallbike$month==9),]$year==2011),])
trainbike201110<-rbind(trainbike201109,trainallbike[which(trainallbike$month==10),][which(trainallbike[which(trainallbike$month==10),]$year==2011),])
trainbike201111<-rbind(trainbike201110,trainallbike[which(trainallbike$month==11),][which(trainallbike[which(trainallbike$month==11),]$year==2011),])
trainbike201112<-rbind(trainbike201111,trainallbike[which(trainallbike$month==12),][which(trainallbike[which(trainallbike$month==12),]$year==2011),])
trainbike201201<-rbind(trainbike201112,trainallbike[which(trainallbike$month==1),][which(trainallbike[which(trainallbike$month==1),]$year==2012),])
trainbike201202<-rbind(trainbike201201,trainallbike[which(trainallbike$month==2),][which(trainallbike[which(trainallbike$month==2),]$year==2012),])
trainbike201203<-rbind(trainbike201202,trainallbike[which(trainallbike$month==3),][which(trainallbike[which(trainallbike$month==3),]$year==2012),])
trainbike201204<-rbind(trainbike201203,trainallbike[which(trainallbike$month==4),][which(trainallbike[which(trainallbike$month==4),]$year==2012),])
trainbike201205<-rbind(trainbike201204,trainallbike[which(trainallbike$month==5),][which(trainallbike[which(trainallbike$month==5),]$year==2012),])
trainbike201206<-rbind(trainbike201205,trainallbike[which(trainallbike$month==6),][which(trainallbike[which(trainallbike$month==6),]$year==2012),])
trainbike201207<-rbind(trainbike201206,trainallbike[which(trainallbike$month==7),][which(trainallbike[which(trainallbike$month==7),]$year==2012),])
trainbike201208<-rbind(trainbike201207,trainallbike[which(trainallbike$month==8),][which(trainallbike[which(trainallbike$month==8),]$year==2012),])
trainbike201209<-rbind(trainbike201208,trainallbike[which(trainallbike$month==9),][which(trainallbike[which(trainallbike$month==9),]$year==2012),])
trainbike201210<-rbind(trainbike201209,trainallbike[which(trainallbike$month==10),][which(trainallbike[which(trainallbike$month==10),]$year==2012),])
trainbike201211<-rbind(trainbike201210,trainallbike[which(trainallbike$month==11),][which(trainallbike[which(trainallbike$month==11),]$year==2012),])
trainbike201212<-rbind(trainbike201211,trainallbike[which(trainallbike$month==12),][which(trainallbike[which(trainallbike$month==12),]$year==2012),])
# test data (keep as separate months)
testbike201101<-testallbike[which(testallbike$month==1),][which(testallbike[which(testallbike$month==1),]$year==2011),]
testbike201102<-testallbike[which(testallbike$month==2),][which(testallbike[which(testallbike$month==2),]$year==2011),]
testbike201103<-testallbike[which(testallbike$month==3),][which(testallbike[which(testallbike$month==3),]$year==2011),]
testbike201104<-testallbike[which(testallbike$month==4),][which(testallbike[which(testallbike$month==4),]$year==2011),]
testbike201105<-testallbike[which(testallbike$month==5),][which(testallbike[which(testallbike$month==5),]$year==2011),]
testbike201106<-testallbike[which(testallbike$month==6),][which(testallbike[which(testallbike$month==6),]$year==2011),]
testbike201107<-testallbike[which(testallbike$month==7),][which(testallbike[which(testallbike$month==7),]$year==2011),]
testbike201108<-testallbike[which(testallbike$month==8),][which(testallbike[which(testallbike$month==8),]$year==2011),]
testbike201109<-testallbike[which(testallbike$month==9),][which(testallbike[which(testallbike$month==9),]$year==2011),]
testbike201110<-testallbike[which(testallbike$month==10),][which(testallbike[which(testallbike$month==10),]$year==2011),]
testbike201111<-testallbike[which(testallbike$month==11),][which(testallbike[which(testallbike$month==11),]$year==2011),]
testbike201112<-testallbike[which(testallbike$month==12),][which(testallbike[which(testallbike$month==12),]$year==2011),]
testbike201201<-testallbike[which(testallbike$month==1),][which(testallbike[which(testallbike$month==1),]$year==2012),]
testbike201202<-testallbike[which(testallbike$month==2),][which(testallbike[which(testallbike$month==2),]$year==2012),]
testbike201203<-testallbike[which(testallbike$month==3),][which(testallbike[which(testallbike$month==3),]$year==2012),]
testbike201204<-testallbike[which(testallbike$month==4),][which(testallbike[which(testallbike$month==4),]$year==2012),]
testbike201205<-testallbike[which(testallbike$month==5),][which(testallbike[which(testallbike$month==5),]$year==2012),]
testbike201206<-testallbike[which(testallbike$month==6),][which(testallbike[which(testallbike$month==6),]$year==2012),]
testbike201207<-testallbike[which(testallbike$month==7),][which(testallbike[which(testallbike$month==7),]$year==2012),]
testbike201208<-testallbike[which(testallbike$month==8),][which(testallbike[which(testallbike$month==8),]$year==2012),]
testbike201209<-testallbike[which(testallbike$month==9),][which(testallbike[which(testallbike$month==9),]$year==2012),]
testbike201210<-testallbike[which(testallbike$month==10),][which(testallbike[which(testallbike$month==10),]$year==2012),]
testbike201211<-testallbike[which(testallbike$month==11),][which(testallbike[which(testallbike$month==11),]$year==2012),]
testbike201212<-testallbike[which(testallbike$month==12),][which(testallbike[which(testallbike$month==12),]$year==2012),]

## poisson forrest for first month
set.seed(1044)
pf012011<-randomForest(casual~atemp + windspeed + humidity +
                   hour + workingday + holiday + season,
                 data=trainbike201101,distribution="poisson")
set.seed(1044)
gf012011<-randomForest(casual~atemp + windspeed + humidity +
                         hour + workingday + holiday + season,
                       data=trainbike201101,distribution="gaussian")
pf201101predtrain<-predict(pf012011)
pf201101pred<-predict(pf012011,newdata=testbike201101)
pf201101pred<-data.frame(testbike201101$datetime,pf012011pred)
result<-pf201101pred

### can ddply work?

trainIncData<-list(trainbike201101,trainbike201102,trainbike201103,
                   trainbike201104,trainbike201105,trainbike201106,
                   trainbike201107,trainbike201108,trainbike201109,
                   trainbike201110,trainbike201111,trainbike201112,
                   trainbike201201,trainbike201202,trainbike201203,
                   trainbike201204,trainbike201205,trainbike201206,
                   trainbike201207,trainbike201208,trainbike201209,
                   trainbike201210,trainbike201211,trainbike201212)
testIncData<-list(testbike201101,testbike201102,testbike201103,
                   testbike201104,testbike201105,testbike201106,
                   testbike201107,testbike201108,testbike201109,
                   testbike201110,testbike201111,testbike201112,
                   testbike201201,testbike201202,testbike201203,
                   testbike201204,testbike201205,testbike201206,
                   testbike201207,testbike201208,testbike201209,
                   testbike201210,testbike201211,testbike201212)

#set.seed(1044)
## submission 1
#for(ii in seq(length(trainIncData)){
for(ii in seq(length(trainIncData))){
  bikeModel<-randomForest(count~atemp + windspeed + humidity +
                           hour + workingday + holiday + season,
                         data=trainIncData[[ii]],distribution="poisson")
  bikePred<-predict(bikeModel,newdata=testIncData[[ii]])##how do I get this) just do all the subsetting,etc in this for-loop
  bikePpred<-data.frame(testIncData[[ii]]$datetime,bikePred)
  ifelse(ii==1,result<-bikePpred,result<-rbind(result,bikePpred))
  print(ii)
}
colnames(result)<-c("datetime","count")
write.csv(result,"submit01.csv",row.names=FALSE,quote=FALSE)

### submission 2 (forgot to set seed)
# try again with a few more factors I left out
for(ii in seq(length(trainIncData))){
  bikeModel<-randomForest(count~atemp + temp + windspeed + humidity +
                            hour + workingday + holiday + season + weather,
                          data=trainIncData[[ii]],distribution="poisson")
  bikePred<-predict(bikeModel,newdata=testIncData[[ii]])##how do I get this) just do all the subsetting,etc in this for-loop
  bikePpred<-data.frame(testIncData[[ii]]$datetime,bikePred)
  ifelse(ii==1,result<-bikePpred,result<-rbind(result,bikePpred))
  print(ii)
}
colnames(result)<-c("datetime","count")
write.csv(result,"submit02.csv",row.names=FALSE,quote=FALSE)

# try predicted casual and registered first
bikeModelCas<-randomForest(casual~atemp + temp + windspeed + humidity +
                          hour + workingday + holiday + season + weather,
                        data=trainIncData[[ii]],distribution="poisson")

##### notes
# should I use casual and registered information?
# https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
##### submission log
# 1) default randomForest tree leaving  out a few variables (temp,season)
# 2) default randomForest tree leaving  out only casual and registered variables