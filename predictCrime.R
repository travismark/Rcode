##
require(dplyr);require(ggplot2);require(lubridate);require(randomForest);require(reshape2)
#setwd("C:/Users/tbaer/Desktop/product_development/kaggle/SFcrime2015/")
setwd("C:/Users/tmbae_000/Desktop/programming/kaggle/SFcrime/")
cD<-read.csv("train/train.csv")
cDtest<-read.csv("test/test.csv")
## Feature engineering
originalCategoryNames<-levels(cD$Category)
# add date information
cD$Dates<-ymd_hms(cD$Dates);cDtest$Dates<-ymd_hms(cDtest$Dates)
cD$hour<-hour(cD$Dates); cDtest$hour<-hour(cDtest$Dates)
cD$year<-year(cD$Dates); cDtest$year<-year(cDtest$Dates)
# intersection or not
cD$intersection <- FALSE; cDtest$intersection <- FALSE 
cD[grep("/",cD$Address,fixed=TRUE),"intersection"]<-TRUE; cDtest[grep("/",cDtest$Address,fixed=TRUE),"intersection"]<-TRUE

#s<-sample_frac(cD,0.1,replace=FALSE) # down-sample
## Cleaning
# 67 samples have the wrong longitude 
#nrow(cD[cD$Y==90,])
# and longitude
#nrow(cD[cD$X==-120.5,])
# impute by police district
cD[cD$X==-120.5,c("X","Y")]<-NA # first NA these in full dataset
# average x & y by PD
avgCoordByPD<-cD[,c("PdDistrict","X","Y")] %>% group_by(PdDistrict) %>% summarise(meanX = mean(X,na.rm=TRUE), meanY = mean(Y,na.rm=TRUE))
cD<-left_join(cD,avgCoordByPD,by="PdDistrict")
cD$X[which(is.na(cD$X))]<-cD$meanX[which(is.na(cD$X))] # assign x
cD$X[which(is.na(cD$X))]<-cD$meanX[which(is.na(cD$X))] # assign Y
# no problem in test data
set.seed(5)
s<-sample_frac(cD,0.1,replace=FALSE) # and re-down-sample


### Plots
plot(s$X,s$Y,col=s$Category)
# g <- ggplot(s) + geom_point(aes(x=X,y=Y,colour=format(Dates,format="%Y")))
g <- ggplot(s) + geom_point(aes(x=X,y=Y,colour=intersection))
g

plot(prop.table(table(s$hour,s$Category)))
plot(prop.table(table(s$DayOfWeek,s$Category)))
plot(prop.table(table(s$year,s$Category)))

## correlations
# table
countByCategoryDayOfWeek<-cD %>% group_by(DayOfWeek, Category) %>% summarise(count = n())
# plot
g <- ggplot(countByCategoryDayOfWeek, aes(x=Category, y=count)) +
  geom_bar(stat = "identity") +
  facet_grid(.~DayOfWeek) +
  coord_flip()
g

## submission data structure
# thanks to https://stackoverflow.com/questions/10689055/create-an-empty-data-frame
baseSub<-data.frame(matrix(0,nrow(cDtest),1+length(levels(cD$Category)),dimnames=list(c(),c("Id",levels(cD$Category)))))
baseSub$Id<-row(baseSub)[,1]-1; # id starts at zero

## trying to get evaluation metric
#sum(log(pmax(pmin(SubsTry06[1,],1-1e-15),1e-15))*ifelse("ASSAULT"==colnames(SubsTry06),1,0))

# first try: highest % by hour and year - 100% probability to a single class - just use all data instead of cross-validation
#maxByHourAndYearSample<-s %>% group_by(hour,year,Category) %>% summarise(ct = n()) %>% top_n(1,ct) # returns a few duplicates
maxByHourAndYearSample<-s %>% group_by(hour,year,Category) %>% summarise(ct = n()) %>% slice(which.max(ct)) # does not return duplicates
maxByHourAndYear<-cD %>% group_by(hour,year,Category) %>% summarise(ct = n()) %>% slice(which.max(ct)) # does not return duplicates
# training error from using this estimate
# first rename the estimate's field
colnames(maxByHourAndYearSample)<-c("hour","year","estimate","ct")
colnames(maxByHourAndYear)<-c("hour","year","estimate","ct")
# on sample
sampleTry01<- left_join(s,maxByHourAndYearSample, by=c("hour","year"))
sum(sampleTry01$Category==sampleTry01$estimate)/nrow(sampleTry01) # 0.2092592 
# on full data
trainTry01<- left_join(cD,maxByHourAndYear, by=c("hour","year"))
sum(trainTry01$Category==trainTry01$estimate)/nrow(trainTry01) # 0.205304
# make a submission
SubsTry01<- left_join(cDtest,maxByHourAndYear, by=c("hour","year"))[,c("Id","estimate")]
SubsTry01$colNum <- 3+as.numeric(SubsTry01$estimate)
SubsTry01<-left_join(SubsTry01,baseSub,by="Id")
#apply(head(SubsTry01),1,function(abc){abc})
# what's a better way to do this?
# loop over rows
#system.time(for(ii in seq(nrow(SubsTry01))) {SubsTry01[ii,SubsTry01$colNum[ii]]<-1}) # looping through test set takes 4330.6 seconds, or 72 minutes
# loop over columns
system.time(for(ii in seq(length(SubsTry01))) {SubsTry01[,ii]<-ifelse(SubsTry01$colNum==ii,1,SubsTry01[,ii])}) # 4 seconds 
# remove a few unnecessary fields for submission
SubsTry01$colNum<-NULL; SubsTry01$estimate<-NULL
system.time(write.table(x=SubsTry01,file="submission01.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 47 seconds
#a<- baseSub[,1+which(levels(SubsTry01$estimate)==SubsTry01$estimate)] #<- left_join(SubsTry01,cDbaseSub

# second try: proportional voting by hour and year - relative probability to a a class based on its count within that hour/year bucket
countByHourAndYear<-cD %>% group_by(hour,year,Category) %>% summarise(ct = n()) #%>% slice(which.max(ct)) # does not return duplicates
totalByHourAndYear<-cD %>% group_by(hour,year) %>% summarise(totCt = n())
countByHourAndYear<- left_join(countByHourAndYear,totalByHourAndYear,by=c("hour","year"))
countByHourAndYear$catProp<-countByHourAndYear$ct/countByHourAndYear$totCt
#SubsTry02<-left_join(baseSub,cDtest[,c("Id","hour","year")],by="Id")
#system.time(for(ii in seq(from=2,to=length(SubsTry02))) {SubsTry02[,ii]<-countByHourAndYearSample[
 # countByHourAndYearSample$hour==SubsTry02$hour & countByHourAndYearSample$year == SubsTry02$year & 
  #  countByHourAndYearSample$Category == "TRESPASS",]$catProp})
# reg ex "( |\\/)+" matches all spaces and forward slashes
# gsub(pattern="( |\\/)+",x=levels(cD$Category),replacement=".")
fieldPerCat<-dcast(data=countByHourAndYear,formula = hour + year ~ Category,value.var="catProp",fill=0)
SubsTry02<-left_join(cDtest[,c("Id","hour","year")],fieldPerCat)
SubsTry02$hour<-NULL;SubsTry02$year<-NULL
system.time(write.table(x=SubsTry02,file="submission02.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 91 seconds

# third try: proportional voting by hour and year and day-of-week
countByHourAndYear<-cD %>% group_by(hour,year,DayOfWeek,Category) %>% summarise(ct = n()) #%>% slice(which.max(ct)) # does not return duplicates
totalByHourAndYear<-cD %>% group_by(hour,year,DayOfWeek) %>% summarise(totCt = n())
countByHourAndYear<- left_join(countByHourAndYear,totalByHourAndYear,by=c("hour","year","DayOfWeek"))
countByHourAndYear$catProp<-countByHourAndYear$ct/countByHourAndYear$totCt
fieldPerCat<-dcast(data=countByHourAndYear,formula = hour + year + DayOfWeek ~ Category,value.var="catProp",fill=0)
SubsTry03<-left_join(cDtest[,c("Id","hour","year","DayOfWeek")],fieldPerCat)
SubsTry03$hour<-NULL;SubsTry03$year<-NULL;SubsTry03$DayOfWeek<-NULL
system.time(write.table(x=SubsTry03,file="submission03.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 82 seconds

# fourth try: proportional voting by hour and year and day-of-week and precinct
countByHourAndYear<-cD %>% group_by(hour,year,DayOfWeek,PdDistrict,Category) %>% summarise(ct = n()) #%>% slice(which.max(ct)) # does not return duplicates
totalByHourAndYear<-cD %>% group_by(hour,year,DayOfWeek,PdDistrict) %>% summarise(totCt = n())
countByHourAndYear<- left_join(countByHourAndYear,totalByHourAndYear,by=c("hour","year","DayOfWeek","PdDistrict"))
countByHourAndYear$catProp<-countByHourAndYear$ct/countByHourAndYear$totCt
fieldPerCat<-dcast(data=countByHourAndYear,formula = hour + year + DayOfWeek + PdDistrict ~ Category,value.var="catProp",fill=0)
SubsTry04<-left_join(cDtest[,c("Id","hour","year","DayOfWeek","PdDistrict")],fieldPerCat)
SubsTry04$hour<-NULL;SubsTry04$year<-NULL;SubsTry04$DayOfWeek<-NULL;SubsTry04$PdDistrict<-NULL
system.time(write.table(x=SubsTry04,file="submission04.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 59 seconds

# seventh try: - no DayOfWeek, which leads to some NAs - add 0.001 to all zeros
countByHourYearDist<-cD %>% group_by(hour,year,PdDistrict,Category) %>% summarise(ct = n()) #%>% slice(which.max(ct)) # does not return duplicates
totalByHourYearDist<-cD %>% group_by(hour,year,PdDistrict) %>% summarise(totCt = n())
countByHourYearDist<-left_join(countByHourYearDist,totalByHourYearDist,by=c("hour","year","PdDistrict"))
countByHourAndYear$catProp<-countByHourYearDist$ct/countByHourYearDist$totCt
fieldPerCat<-dcast(data=countByHourAndYear,formula = hour + year + PdDistrict ~ Category,value.var="catProp",fill=0)
SubsTry07<-left_join(cDtest[,c("Id","hour","year","PdDistrict")],fieldPerCat)

# eight try - try replacing zeros with 0.01
SubsTry08<-SubsTry07
for (ii in seq(from=2,to=length(SubsTry08))) {
  SubsTry08[,ii] = ifelse(SubsTry08[,ii]==0.001,0.01,SubsTry08[,ii])
}
system.time(write.table(x=SubsTry08,file="submission08.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 77 seconds

# ninth try - try replacing zeros with 0.001
SubsTry09<-SubsTry07
for (ii in seq(from=2,to=length(SubsTry09))) {
  SubsTry09[,ii] = ifelse(SubsTry09[,ii]==0,0.001,SubsTry09[,ii])
}
system.time(write.table(x=SubsTry09,file="submission09.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 109 seconds
# need to deal with NAs in the "fieldPerCat" where some combination has no outcomes in training data


## now a more complicated model - first random forest
# because of large size of data I need to train a model on only a subset
# sample might not have all the factors, so must refactor
system.time(firstRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek,mtry=2,ntree=200,data=s,importance=TRUE))
format(object.size(firstRFsample),units="Mb")
#system.time(sfirstRFfull<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek,mtry=1,ntree=200,data=cD,importance=TRUE))
SubsTry05<-as.data.frame(predict(firstRFsample,type="prob",newdata=cDtest[,c("Id","hour","year","DayOfWeek")]))
# add missing fields - Id and any Categories that were left out of the sample
# missing one
mis<- levels(cD$Category)[which(!(levels(cD$Category)) %in% (levels(factor(s$Category))))]
SubsTry05$TREA<-0
SubsTry05$Id<-seq(from=0,to=nrow(SubsTry05)-1)
SubsTry05<-SubsTry05[,c("Id",levels(cD$Category))]
system.time(write.table(x=SubsTry05,file="submission05.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 59 seconds

# another RF - include police district
system.time(secondRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek + PdDistrict,mtry=2,ntree=50,data=s,importance=TRUE)) # 67 s
secondRFsampleTrainPred<-as.data.frame(predict(secondRFsample,type="prob"))
secondRFsampleTrainPred$TREA<-0
secondRFsampleTrainPred$Id<-seq(from=0,to=nrow(secondRFsampleTrainPred)-1)
secondRFsampleTrainPred<-secondRFsampleTrainPred[,c("Id",levels(cD$Category))]
# Calculate the training logloss
a = 0
for (ii in seq(from=2,to=length(secondRFsampleTrainPred))) {
  a = a + sum(log(pmax(pmin(secondRFsampleTrainPred[,ii],1-1e-15),1e-15))*ifelse(s$Category==colnames(secondRFsampleTrainPred[ii]),1,0))
}
-a/nrow(secondRFsampleTrainPred) # training logloss

SubsTry06<-as.data.frame(predict(secondRFsample,type="prob",newdata=cDtest[,c("Id","hour","year","DayOfWeek","PdDistrict")]))
mis<- levels(cD$Category)[which(!(levels(cD$Category)) %in% (levels(factor(s$Category))))]
SubsTry06$TREA<-0
SubsTry06$Id<-seq(from=0,to=nrow(SubsTry06)-1)
SubsTry06<-SubsTry06[,c("Id",levels(cD$Category))]
system.time(write.table(x=SubsTry06,file="submission06.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 59 seconds

# another RF - use 20% of the training data
set.seed(5)
s<-sample_frac(cD,0.2,replace=FALSE) # re-down-sample - not missing any categories
system.time(thirdRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek + PdDistrict,mtry=2,ntree=50,data=s,importance=TRUE))
thirdRFsampleTrainPred<-as.data.frame(predict(thirdRFsample,type="prob"))
thirdRFsampleTrainPred$Id<-seq(from=0,to=nrow(thirdRFsampleTrainPred)-1)
thirdRFsampleTrainPred<-thirdRFsampleTrainPred[,c("Id",levels(cD$Category))]
a3 = 0
for (ii in seq(from=2,to=length(thirdRFsampleTrainPred))) {
  a3 = a3 + sum(log(pmax(pmin(thirdRFsampleTrainPred[,ii],1-1e-15),1e-15))*ifelse(s$Category==colnames(thirdRFsampleTrainPred[ii]),1,0))
}
-a3/nrow(thirdRFsampleTrainPred) # training logloss

# another RF - use 10% of the training data and add X & Y
set.seed(5)
s<-sample_frac(cD,0.1,replace=FALSE) # re-down-sample - not missing any categories
system.time(fourthRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek + PdDistrict + X + Y,ntree=50,data=s[!is.na(sX),],importance=TRUE))
fourthRFsampleTrainPred<-as.data.frame(predict(fourthRFsample,type="prob"))
fourthRFsampleTrainPred$TREA<-0
fourthRFsampleTrainPred$Id<-seq(from=0,to=nrow(fourthRFsampleTrainPred)-1)
fourthRFsampleTrainPred<-fourthRFsampleTrainPred[,c("Id",levels(cD$Category))]
a4 = 0
for (ii in seq(from=2,to=length(fourthRFsampleTrainPred))) {
  a4 = a4 + sum(log(pmax(pmin(fourthRFsampleTrainPred[,ii],1-1e-15),1e-15))*ifelse(s$Category==colnames(fourthRFsampleTrainPred[ii]),1,0))
}
-a4/nrow(fourthRFsampleTrainPred) # training logloss
