# contour plots like those in JMP
# February 2015

require(dplyr)
require(lattice)

# what's the right function?


# try filled.conour
# examples
filled.contour(volcano,color=terrain.colors)
filled.contour(z=matrix(c(2,1,0.9,0.2,6,5,5,4,9,9,7,5,5,5,6,7),nrow=4,ncol=4,byrow=TRUE))
filled.contour(z=matrix(c(2,1,0.9,0.2,6,5,5,4,9,NA,7,5,5,5,6,7),nrow=4,ncol=4,byrow=TRUE)) # NA handling
# make 5% of volcano's data 
abc<-volcano
abc[sample(seq(length(volcano)),floor(length(volcano)*0.05))]<-NA
filled.contour(abc,color=terrain.colors) # NA handling
## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
## rather, the key *should* be labeled:
filled.contour(cos(r^2)*exp(-r/(2*pi)), frame.plot = FALSE,
               plot.axes = {})
# bigger volcano
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                             axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"),
               key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

# try sample data
cz<-matrix(c(2,1,4,3),nrow=2,ncol=2)
cx<-c(1,2)
cy<-c(1,2)
filled.contour(cx,cy,cz)

# verdit - can't handle NAs or gaps in data.  seems to require equally spaced measurements across the range
#### 

# try contour/level plot from lattice
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)
# set 5% of data to NA
grid$z[sample(seq(length(grid$z)),floor(length(grid$z)*0.05))]<-NA
levelplot(z~x*y,grid,cuts=50,scales=list(log="e"))

setwd("C:/Users/tmbae_000/Desktop/programming/contours")
imbal<-read.csv("MaintSupport.csv")
# the data is a data frame - make into a matrix
with(imbal[which(imbal$GeoLoc=="Okinawa"),],levelplot(UtilDiff~OrgPerc*NumPend))
# well that sucks
# try the strategy here: locally weighted regression
# http://www.wekaleamstudios.co.uk/posts/displaying-data-using-level-plots/

# fit separate loess for each Geographic Location
OkiLwr1<-loess(UtilDiff~OrgPerc*NumPend,data=imbal[which(imbal$GeoLoc=="Okinawa"),],degree=1,span=0.25) # span controls smoothing - default is 0.75
OkiLwr2<-loess(UtilDiff~OrgPerc*NumPend,data=imbal[which(imbal$GeoLoc=="Okinawa"),],degree=2,span=0.25)
OkiConPts<-expand.grid(list("OrgPerc"=seq(min(filter(imbal,GeoLoc=="Okinawa")$OrgPerc),max(filter(imbal,GeoLoc=="Okinawa")$OrgPerc),
                               (max(filter(imbal,GeoLoc=="Okinawa")$OrgPerc)-min(filter(imbal,GeoLoc=="Okinawa")$OrgPerc))/200),
                         "NumPend"=seq(min(filter(imbal,GeoLoc=="Okinawa")$NumPend),max(filter(imbal,GeoLoc=="Okinawa")$NumPend),
                               (max(filter(imbal,GeoLoc=="Okinawa")$NumPend)-min(filter(imbal,GeoLoc=="Okinawa")$NumPend))/200))) # 201 on each axis 
# predict the z at each and attach to Points for Plotting
Okiz1<-predict(OkiLwr1,newdata=OkiConPts)
OkiConPts$Okiz1<-as.numeric(Okiz1)
Okiz2<-predict(OkiLwr2,newdata=OkiConPts)
OkiConPts$Okiz2<-as.numeric(Okiz2)

# now try the lattice levelplot again
levelplot(Okiz1~OrgPerc*NumPend,data=OkiConPts) # first order
levelplot(Okiz2~OrgPerc*NumPend,data=OkiConPts) # second order



# find minimum and maximum % org for each Pendleton
imbalBound<-data.frame(group_by(imbal,NumPend) %>% summarise("minOrgP"=min(OrgPerc),"maxorgp"=max(OrgPerc)))

# other notes
# use expand.grid to build out a matrix
expand.grid("%Org"=c(1,2,3),"Pen"=c(4,5,6))
str(expand.grid("%Org"=c(1,2,3),"Pen"=c(4,5,6)))
