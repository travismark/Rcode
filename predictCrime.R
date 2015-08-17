##
require(dplyr);require(ggplot2);require(lubridate)
setwd("C:/Users/tbaer/Desktop/product_development/kaggle/SFcrime2015/")
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

## now a more complicated model - first random forest
require(randomForest)
# sample doesn't have all categories so must re-factor
system.time(firstRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek,mtry=1,ntree=200,data=s,importance=TRUE))
format(object.size(sfirstRFsample),units="Mb")
#system.time(sfirstRFfull<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek,mtry=1,ntree=200,data=cD,importance=TRUE))
SubsTry05<-as.data.frame(predict(firstRFsample,type="prob",newdata=cDtest[,c("Id","hour","year","DayOfWeek")]))
endNames<-colnames(SubsTry05)
SubsTry05$Id<-seq(from=0,to=nrow(SubsTry05)-1)
SubsTry05<-SubsTry05[,c("Id",endNames)]
system.time(write.table(x=SubsTry05,file="submission05.csv",col.names=c("Id",levels(cD$Category)),sep=",",row.names=FALSE)) # 59 seconds
# another RF
system.time(secondRFsample<- randomForest(formula=factor(Category) ~ hour + year + DayOfWeek + PdDistrict,mtry=3,ntrees=200,data=s,importance=TRUE))