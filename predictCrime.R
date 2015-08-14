##
require(dplyr);require(ggplot2);require(lubridate)
setwd("C:/Users/tbaer/Desktop/product_development/kaggle/SFcrime2015/")
cD<-read.csv("train/train.csv")
cDtest<-read.csv("test/test.csv")
## Feature engineering
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
cD[cD$X==-120.5,c("X","Y")]<-NA # so NA them in full dataset
s<-sample_frac(cD,0.1,replace=FALSE) # and re-down-sample


### Plots
plot(s$X,s$Y,col=s$Category)
g <- ggplot(s) + geom_point(aes(x=X,y=Y,colour=format(Dates,format="%Y")))
g

## submission data structure
# thanks to https://stackoverflow.com/questions/10689055/create-an-empty-data-frame
baseSub<-data.frame(matrix(0,nrow(cDtest),1+length(levels(cD$Category)),dimnames=list(c(),c("Id",levels(cD$Category)))))
baseSub$Id<-row(baseSub)[,1]-1; # id starts at zero

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

# third try: proportional voting by hour and year and day-of-week
