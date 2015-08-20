# July 2014
# Kaggle Bike Share competition

require('lubridate') # for handling datetimes
require(MASS) # for fitting
require(Metrics) # for RMSLE

####### LOAD THE DATA
#setwd("C:/Users/tmbae_000/Desktop/programming/kaggle/bikeshare")
setwd("C:/Users/tbaer/Desktop/product_development/kaggle/Bike_Sharing_Demand")
testallbike<-read.csv("test.csv",stringsAsFactors=FALSE)
trainallbike<-read.csv("train.csv",stringsAsFactors=FALSE)
# consolidate test and train for data manipulation
testallbike["casual"]=NA;testallbike["registered"]=NA;testallbike["count"]=NA
testallbike["test"]=1
trainallbike["test"]=0
alldatabike<-rbind(testallbike,trainallbike)
# add date/time field(s) and make factors
alldatabike$datetime<-ymd_hms(alldatabike$datetime, tz='UTC')
sum(minute(alldatabike$datetime)) # =0; prove all times are on the hour
alldatabike$hour<-as.factor(hour(alldatabike$datetime))
alldatabike$year<-as.factor(year(alldatabike$datetime))
alldatabike$month<-as.factor(month(alldatabike$datetime))
alldatabike$dayOfMonth<-as.factor(day(alldatabike$datetime))
alldatabike$dayOfWeek<-as.factor(weekdays(alldatabike$datetime))
# make season, working day, holiday, and weather factors
alldatabike$season<-as.factor(alldatabike$season)
alldatabike$holiday<-as.factor(alldatabike$holiday)
alldatabike$workingday<-as.factor(alldatabike$workingday)
alldatabike$weather<-as.factor(alldatabike$workingday)
# split hour data into some parts
alldatabike$hour<-as.numeric(alldatabike$hour)
alldatabike$dayPart<-4
alldatabike[alldatabike$hour <= 9 & alldatabike$hour >= 4 ,"dayPart"] <- 1
alldatabike[alldatabike$hour <= 15 & alldatabike$hour >= 10 , "dayPart"] <- 2
alldatabike[alldatabike$hour <= 21 & alldatabike$hour >= 16 , "dayPart"] <- 3
alldatabike$hour<-as.factor(alldatabike$hour)
alldatabike$dayPart<-as.factor(alldatabike$dayPart)
# re-split data into train and test
trainallbike<-alldatabike[which(alldatabike$test==0),]
testallbike<-alldatabike[which(alldatabike$test==1),]
#explore
aggregate(count ~ hour + workingday,data=trainallbike, FUN=mean)
aggregate(count ~ weather,data=trainallbike, FUN=mean)
aggregate(count ~ dayOfMonth, data=trainallbike, FUN=mean)
aggregate(count ~ dayOfWeek, data=trainallbike, FUN=mean)
hist(trainallbike$count)
ce<-fitdistr(trainallbike$count, "exponential")
cp<-fitdistr(trainallbike$count, "Poisson")
#pairs(alldatabike)

#first model using all data (cheating)
a1<-lm(count~atemp,data=trainallbike)
plot(a1)
a2<-lm(count ~ atemp + windspeed + humidity +
         hour + workingday + holiday + season, data=trainallbike)
summary(a1)
summary(a2)
p1<-predict(a1,newdata=testallbike)
p2<-predict(a2,newdata=testallbike)

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

require(randomForest)
# some defaults
allTime1<-system.time(a1<-randomForest(casual~atemp + windspeed + humidity + 
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=500, mytry=3))  # 500 trees is default
rmsle(trainallbike$casual,predict(a1,trainallbike[,attr(a1$terms,'term.labels')]))
# fewer trees
allTime2<-system.time(a2<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100, mtry=3))
rmsle(trainallbike$casual,predict(a2,trainallbike[,attr(a2$terms,'term.labels')]))
# temp instead of atemp
allTime3<-system.time(a3<-randomForest(casual~temp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100, mtry=3))
rmsle(trainallbike$casual,predict(a3,trainallbike[,attr(a3$terms,'term.labels')]))
# does daypart help at all
allTime4<-system.time(a4<-randomForest(casual~temp + windspeed + humidity + dayPart +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100, mtry=3))
rmsle(trainallbike$casual,predict(a4,trainallbike[,attr(a4$terms,'term.labels')]))
# more variables to (randomly) try at each split
allTime5<-system.time(a5<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=4)) # default mtry is p/3 
rmsle(trainallbike$casual,predict(a5,trainallbike[,attr(a5$terms,'term.labels')]))
# try dayPart again
allTime6<-system.time(a6<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=4)) # default mtry is p/3 
rmsle(trainallbike$casual,predict(a6,trainallbike[,attr(a6$terms,'term.labels')]))
# try one more variable to try
allTime7<-system.time(a7<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=5)) #default mtry is p/3, or 3 
rmsle(trainallbike$casual,predict(a7,trainallbike[,attr(a7$terms,'term.labels')]))
# try 6 at a time
allTime8<-system.time(a8<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=6)) #default mtry is p/3, or 3 
rmsle(trainallbike$casual,predict(a8,trainallbike[,attr(a8$terms,'term.labels')]))
# try 7 at a time
allTime9<-system.time(a9<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=7)) #default mtry is p/3, or 3 
rmsle(trainallbike$casual,predict(a9,trainallbike[,attr(a9$terms,'term.labels')]))
# try (all) 8 at a time
allTime10<-system.time(a10<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=100,mtry=8)) #default mtry is p/3, or 3 
rmsle(trainallbike$casual,predict(a10,trainallbike[,attr(a10$terms,'term.labels')]))
#  try 50% more trees
allTime11<-system.time(a11<-randomForest(casual~atemp + windspeed + humidity +
                                         hour + workingday + holiday + season + dayOfWeek,
                                       data=trainallbike,ntree=150,mtry=8)) #default mtry is p/3, or 3 
rmsle(trainallbike$casual,predict(a11,trainallbike[,attr(a10$terms,'term.labels')]))
importance(a11)
# cross validation?
allTime11CV<-system.time(a11CV<-rfcv(trainallbike[,attr(a10$terms,'term.labels')],trainallbike[,"casual"]))
allTime11CV2<-system.time(a11CV2<-rfcv(trainallbike[,attr(a10$terms,'term.labels')],trainallbike[,"casual"],step=0.25))

extractFeatures <- function(df){ # predictors of choice
  return(df[,c("atemp","windspeed","humidity",
               "hour","workingday","holiday","season","dayOfWeek")])
}
# split up the data to make a prediction - 24 months
submission1 <- data.frame(datetime=testallbike$datetime, count=NA)
submission2 <- data.frame(datetime=testallbike$datetime, count=NA)
# We only use past data to make predictions on the test set, 
# so we train a new model for each test set cutoff point
for (i_year in unique(year(ymd_hms(testallbike$datetime)))) {
  for (i_month in unique(month(ymd_hms(testallbike$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(testallbike$datetime))==i_year & month(ymd_hms(testallbike$datetime))==i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(trainallbike$datetime) <= min(ymd_hms(testSubset$datetime))
    rfc <- randomForest(x=extractFeatures(trainallbike[trainLocs,]),y=trainallbike[trainLocs,"casual"], ntree=150,mtry=8)
    rfr <- randomForest(x=extractFeatures(trainallbike[trainLocs,]),y=trainallbike[trainLocs,"registered"], ntree=150,mtry=8)
    rfb <- randomForest(x=extractFeatures(trainallbike[trainLocs,]),y=trainallbike[trainLocs,"count"], ntree=150,mtry=8)
    submission1[testLocs, "count"] <- predict(rfc, testSubset) + predict(rfr, testSubset)
    submission2[testLocs, "count"] <- predict(rfb, testSubset)
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
pf201101pred<-data.frame(testbike201101$datetime,pf201101pred)
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

### submission 3 (ran with 500 trees, the default)
set.seed(14152)
# try predicted casual and registered first
trees2try<-500
for(ii in seq(length(trainIncData))){
  treeTimeC<-system.time(bikeModelC<-randomForest(casual~atemp + temp + windspeed + humidity +
                            hour + workingday + holiday + season + weather,
                          data=trainIncData[[ii]],distribution="poisson",ntree=trees2try))
  treeTimeR<-system.time(bikeModelR<-randomForest(registered~atemp + temp + windspeed + humidity +
                            hour + workingday + holiday + season + weather,
                          data=trainIncData[[ii]],distribution="poisson",ntree=trees2try))
  treeMseC<-bikeModelC$mse[trees2try]
  treeMseR<-bikeModelR$mse[trees2try]
  bikePredC<-predict(bikeModelC,newdata=testIncData[[ii]])
  bikePredR<-predict(bikeModelR,newdata=testIncData[[ii]])
  bikePpred<-data.frame(testIncData[[ii]]$datetime,bikePredC+bikePredR)
  ifelse(ii==1,result<-bikePpred,result<-rbind(result,bikePpred))
  print(ii)
}
colnames(result)<-c("datetime","count")
write.csv(result,"submit03.csv",row.names=FALSE,quote=FALSE)

# try to find a better loss function
system.time(bikeModelR<-randomForest(registered~atemp + temp + windspeed + humidity +
                           hour + workingday + holiday + season + weather,
                         data=trainIncData[[ii]],distribution="poisson",ntree=300))
system.time(bikeModelC<-randomForest(casual~atemp + temp + windspeed + humidity +
                                       hour + workingday + holiday + season + weather,
                                     data=trainIncData[[ii]],distribution="poisson",ntree=300))

#try 4 - fewer trees (300)
set.seed(14152)
# try predicted casual and registered first
trees2try<-300
treeTimeC<-0
treeTimeR<-0
treeMseC<-0
treeMseR<-0
treeRmsleC<-0
treeRmsleR<-0
for(ii in seq(length(trainIncData))){
  treeTimeC[ii]<-system.time(bikeModelC<-randomForest(casual~atemp + temp + windspeed + humidity +
                                                    hour + workingday + holiday + season + weather,
                                                  data=trainIncData[[ii]],distribution="poisson",ntree=trees2try))
  treeTimeR[ii]<-system.time(bikeModelR<-randomForest(registered~atemp + temp + windspeed + humidity +
                                                    hour + workingday + holiday + season + weather,
                                                  data=trainIncData[[ii]],distribution="poisson",ntree=trees2try))
  (treeMseC[ii]<-bikeModelC$mse[trees2try])
  (treeMseR[ii]<-bikeModelR$mse[trees2try])
  (treeRmsleC[ii]<-sqrt(sum(log(bikeModelC$predicted+1)-log(trainIncData[[ii]]$casual+1))/length(bikeModelC$predicted)))
  (treeRmsleR[ii]<-sqrt(sum(log(bikeModelR$predicted+1)-log(trainIncData[[ii]]$registered+1))/length(bikeModelR$predicted)))
  bikePredC<-predict(bikeModelC,newdata=testIncData[[ii]])
  bikePredR<-predict(bikeModelR,newdata=testIncData[[ii]])
  bikePpred<-data.frame(testIncData[[ii]]$datetime,bikePredC+bikePredR)
  ifelse(ii==1,result<-bikePpred,result<-rbind(result,bikePpred))
  print(ii)
}

##### notes
# should I use casual and registered information?
# https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
# MSE is calculated as: sum((bikeModelR$predicted-trainIncData[[ii]]$registered)^2)/length(bikeModelR$predicted)
# RMSLE is calcualted as sqrt(sum(log(bikeModelR$predicted+1)-log(trainIncData[[ii]]$registered+1))/length(bikeModelR$predicted))
# try some time-series approach?
# there is missing data to take care of in test set

##### submission log
# 1) default randomForest tree leaving  out a few variables (temp,season)
# 2) default randomForest tree leaving  out only casual and registered variables
# 3) default randomForest tree - predict casual and registered then add together