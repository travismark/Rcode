# contour plots like those in JMP
# February 2015

require(dplyr)
require(lattice);require(latticeExtra)
require(spatstat)
require(spdep)
require(GeoXp)
require(ggplot2)
require(tripack)
require(deldir)

# what's the right function? for plotting, for interpolating between points


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
# level plots place a box around each point the same color as the point
#     box sizes are as small as the two closest data points, so there is high potential for sparsity (no color)
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

#setwd("C:/Users/tmbae_000/Desktop/programming/contours")
setwd("c:/users/tbaer/desktop/product_development/kaggle/Rcode")
imbal<-read.csv("MaintSupport.csv")
# the data is a data frame - make into a matrix
with(imbal[which(imbal$GeoLoc=="Pendleton"),],levelplot(UtilDiff~OrgPerc*NumPend))
# well that sucks
# try the strategy here: locally weighted regression
# http://www.wekaleamstudios.co.uk/posts/displaying-data-using-level-plots/

# fit separate loess for each Geographic Location
PenLwr1<-loess(UtilDiff~OrgPerc*NumPend,data=imbal[which(imbal$GeoLoc=="Pendleton"),],degree=1,span=0.25) # span controls smoothing - default is 0.75
PenLwr2<-loess(UtilDiff~OrgPerc*NumPend,data=imbal[which(imbal$GeoLoc=="Pendleton"),],degree=2,span=0.25)
PenConPts<-expand.grid(list("OrgPerc"=seq(min(filter(imbal,GeoLoc=="Pendleton")$OrgPerc),max(filter(imbal,GeoLoc=="Pendleton")$OrgPerc),
                               (max(filter(imbal,GeoLoc=="Pendleton")$OrgPerc)-min(filter(imbal,GeoLoc=="Pendleton")$OrgPerc))/200),
                         "NumPend"=seq(min(filter(imbal,GeoLoc=="Pendleton")$NumPend),max(filter(imbal,GeoLoc=="Pendleton")$NumPend),
                               (max(filter(imbal,GeoLoc=="Pendleton")$NumPend)-min(filter(imbal,GeoLoc=="Pendleton")$NumPend))/200))) # 201 on each axis 
# predict the z at each and attach to Points for Plotting
Penz1<-predict(PenLwr1,newdata=PenConPts)
PenConPts$Penz1<-as.numeric(Penz1)
Penz2<-predict(PenLwr2,newdata=PenConPts)
PenConPts$Penz2<-as.numeric(Penz2)

# now try the lattice levelplot again
levelplot(Penz1~OrgPerc*NumPend,data=PenConPts) # first order
p1<-levelplot(Penz2~OrgPerc*NumPend,data=PenConPts) # second order
#levelplot(Penz2~OrgPerc*NumPend,data=PenConPts, at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) # explict contours
p1 + xyplot(PenConPts$NumPend~PenConPts$OrgPerc,cex=.5) # der
# plot the points using another panel function ?
levelplot(Penz2~OrgPerc*NumPend,data=PenConPts, at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
          panel = function(x,y,z){
          panel.levelplot(x,y,z,subscripts=seq(length(z)))
          panel.points(x,y)
          }) # unsure here

# try simple example here:
tx<-c(1,1,2,2,3,3) # c(1,1,3,3)
ty<-c(1,2,1,3.5,1,3) # c(1,2,1,3)
tz<-c(1,2,2.5,4,3,4) # c(1,2,3,4)
levelplot(tz~tx*ty) # boring - no interpolation between points
# try interpolation
simpleLWR1<-loess(tz~tx*ty,degree=2,span=2) # span controls smoothing - default is 0.75
tConPts<-expand.grid(list(tx=seq(1,3,((3-1)/20)),ty=seq(1,3,((3-1)/20))))
tz1<-predict(simpleLWR1,newdata=tConPts)
# it seems loess has a difficult time with only four points - it has the same problem with 6

# try ggplot2
g1 <- ggplot(data.frame(tx,ty,tz),aes(tx,ty,fill=tz))
g1 + geom_tile() # gives the same boring plot with two points missing and no interpolation
g2<-ggplot(PenConPts, aes(x=OrgPerc,y=NumPend,fill=Penz2))
g2 + geom_tile()
# eliminate parts of contour outside the boundaries
# find minimum and maximum % org for each Pendleton
imbalBound<-data.frame(group_by(imbal,NumPend) %>% summarise("minOrgP"=min(OrgPerc),"maxorgp"=max(OrgPerc)))


#### try GeoXp and spdep
abc<-tri2nb()

##### Make Delaunay triangles with the tripack package
# first three data points must not be collinear
randomorder<-sample(length(imbal[imbal$GeoLoc=="Pendleton","OrgPerc"]))
edgesPend<-tri.mesh(x=imbal[imbal$GeoLoc=="Pendleton","OrgPerc"][randomorder],
                    y=imbal[imbal$GeoLoc=="Pendleton","NumPend"][randomorder])
voronPend<-voronoi.mosaic(x=imbal[imbal$GeoLoc=="Pendleton","OrgPerc"][randomorder],
                    y=imbal[imbal$GeoLoc=="Pendleton","NumPend"][randomorder])
par(mfrow=c(1,2))
plot(edgesPend);plot(voronPend)
par(mfrow=c(1,1))
##### Make Delaunay triangles with the deldir package
edgesPendDD<-deldir(x=imbal[imbal$GeoLoc=="Pendleton","OrgPerc"][randomorder],   ### these two methods give the same connections - maybe JMP 9 is wrong or isn't using Delaunay triangles
                    y=imbal[imbal$GeoLoc=="Pendleton","NumPend"][randomorder],
                    z=imbal[imbal$GeoLoc=="Pendleton","UtilDiff"][randomorder],
                    plotit=TRUE)
# Overall
randomorder<-sample(length(imbal[imbal$GeoLoc=="Overall","OrgPerc"]))
edgesOverallDD<-deldir(x=imbal[imbal$GeoLoc=="Overall","OrgPerc"][randomorder],   ### these two methods give the same connections - maybe JMP 9 is wrong or isn't using Delaunay triangles
                    y=imbal[imbal$GeoLoc=="Overall","NumPend"][randomorder],
                    z=imbal[imbal$GeoLoc=="Overall","UtilDiff"][randomorder],
                    plotit=TRUE)

# other notes
# use expand.grid to build out a matrix
expand.grid("%Org"=c(1,2,3),"Pen"=c(4,5,6))
str(expand.grid("%Org"=c(1,2,3),"Pen"=c(4,5,6)))
