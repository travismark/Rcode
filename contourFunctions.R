############
# Algorithm to make contour plots similar to those in JMP
# Travis Baer
# Clockwork
# Spring 2015
############

#### Five Steps
# Calculate and draw all Delaunay triangle edges
# Calculate the slope of each edge
# Find any points on the edges that equal one of the contour values with straight lines
# Connect the points found in Step 3 to create the contours
# Fill the areas between the contours
####

# 1) Calculate and draw all Delaunay triangle edges

# for now I'll use deldir pakcage
# create all the required starting data with a call to deldir() function, passing in x, y, and z
# giving the deldir function the z seems pointless - it attaches it to the tessalations, which are wrong anyway
edges<-edgesPendDD$delsgs
z<-edgesPendDD$summary$z
####
# 2) Calculate the slope of each edge
calcEdgeSlope<-function(edges,z){
# Calculates the length and slope (in third-variable z) of each edge
#
# Args:  
#   edges: an output of deldir$delsgs that gives points of edges
#   z: the z value for each point
#  
# Returns: edges: data frame with all edge points, slopes (in z), and lengths
#   
#   i) combine both point z's to all the edges
# z is a vector of values - turn it into a data frame give it an index
z<- as.data.frame(z)
z$ind=as.numeric(row.names(z))
# use "merge" instead of dplyr's joins for fewer package requirements
edges<-merge(edges,z,by.x="ind1",by.y="ind")
colnames(edges)[which(colnames(edges)=="z")]<-"z1" # rename
edges<-merge(edges,z,by.x="ind2",by.y="ind")
colnames(edges)[which(colnames(edges)=="z")]<-"z2" # rename
#  ii) calculate length of each edge
edges$len<-sqrt((edges$x1-edges$x2)^2+(edges$y1-edges$y2)^2)
# iii) calculate the slope of each edge
edges$slp<-(edges$z1-edges$z2)/edges$len
return edges
}

cv<-c(0.3,0.5,0.4)

# 3) Find any points on the edges that equal one of the contour values with straight lines
calcContourPts<-function(edges,cv) {
# Finds each point on an edge that equals one of the contour values
#
# Args:  
#   edges: an output of deldir$delsgs that gives points, lengths, and slopes of edges
#   cv: the z value for each contour
#  
# Returns: contours: data frame with all points on lines
#   
# Re-order contour values so they're in ascending order
cv<-cv[order(cv)]
# Remove duplicates
cv<-unique(cv)
# maybe do some other checks
# for each contour value - find the edges that include it, make a df big enough, then loop through these
cvPts<-data.frame("cv"=double(0),"x"=double(0),"y"=double(0))
for (ii in cv) {
  indicies<-union(which(cv>=edges$z1 & cv<=edges$z2),which(cv>=edges$z2 & cv<=edges$z1))
  cvPtsOne<-data.frame("cv"=ii,x=0,y=0) # initialize this df, then apply through the matching rows
  sapply(findConInt,cv,ii)
  rbind(cvPts,cvPtsOne)
}
    
}

findConInt<-function(row,cv,indx){
# Finds an x and y coordinate for where cv is on the edge in the row
#
# Args:  
#   row: a row from the edges table
#   cv: one z value for this contour
#  indx: index to place this x and y coordinate  
#
# Returns: nothing
#  
  a<-(row$z1-cv)/(row$z2-row$z1)
  cvPts[ii,"cv"]<-cv
  cvPts[ii,"x"]<-row$x1+a*(row$x2-row$x1)
  cvPts[ii,"y"]<-row$y1+a*(row$y2-row$y1) 
}