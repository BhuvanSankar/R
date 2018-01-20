


















setwd("~/../Desktop/INFS4203 Pracs/P3/")


# Data Source: https://data.brisbane.qld.gov.au/data/dataset/bicycle-racks
bikeRacks = read.csv("CBD-bike-racks.csv")

win.graph()
plot(bikeRacks$Longitude, bikeRacks$Latitude)


#A little hack I created for plotting with map image
#install.packages("png") #if needed
source("Tute3Helper.R")

plotSetupCBD()
points(bikeRacks$Longitude, bikeRacks$Latitude)





## Do the K means clustering

bikeLocations = bikeRacks[,c("Longitude","Latitude")]

nClusters = 2L
#Optional
set.seed(0) #Optionaly set the seed, to enusre same results
bikeClusters = kmeans(bikeLocations, nClusters)


## Plot cluster results

plotSetupCBD(main=sprintf("Kmeans - %d clusters", nClusters))
points(bikeLocations,
       pch = 21, col="Black",
       bg = bikeClusters$cluster)

points(bikeClusters$centers, pch=7, cex=2)


#fun
segments(bikeLocations[,1],bikeLocations[,2],
         bikeClusters$centers[bikeClusters$cluster,1],bikeClusters$centers[bikeClusters$cluster,2])





## Put together into nice function
clusterAndPlot = function(data, nClusters, centroids = TRUE, lineSegs = FALSE, setSeed = NA){
  
  if(ncol(data)!=2 ) stop("Data should have 2 columns (e.g. x and y)")
  if(!is.na(setSeed)) set.seed(setSeed)
  
  clusters = kmeans(data, nClusters)
  
  plotSetupCBD(main=sprintf("Kmeans - %d clusters", nClusters))
  
  if(centroids) { #plot centroids
    points(clusters$centers, pch=7, cex=2)
  }
  if(lineSegs) { #add lines between data point and centroid
    segments(data[,1],data[,2],
             clusters$centers[clusters$cluster,1],clusters$centers[clusters$cluster,2])
  }
  
  #Plot the data points
  points(data, pch=21, col="Black", bg = clusters$cluster)
}


clusterAndPlot(bikeLocations, 3)

clusterAndPlot(bikeLocations, 10, lineSegs = TRUE)
clusterAndPlot(bikeLocations, 10, centroids = FALSE)



