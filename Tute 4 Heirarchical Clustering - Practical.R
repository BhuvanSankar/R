setwd("~/../Desktop/INFS4203 Pracs/P4")

#Let's use the same bike data set, but do hierarchical clustering


# Data Source: https://data.brisbane.qld.gov.au/data/dataset/bicycle-racks
bikeRacks = read.csv("../P3/CBD-bike-racks.csv")
bikeLocations = bikeRacks[,c("Longitude","Latitude")]

plot(bikeLocations)

source("Tute3Helper.R") #Ensure you have this+picture avaiable in same folder, otherwise change the plotSetupCBD/points -> plot(...)


plotSetupCBD()
points(bikeLocations)




## Do the Heirarchical clustering

bike_dist = dist(bikeLocations)

bike_HClust_tree_complete <- hclust(bike_dist) #Max/Complete linkage
bike_HClust_tree_single <- hclust(bike_dist, method = "single") #Min/Single linkage


#Show the cluster tree
win.graph()
plot(bike_HClust_tree_single, main="Bike Rack Cluster Tree - Single Linkage")
win.graph()
plot(bike_HClust_tree_complete, main="Bike Rack Clusters - Complete Linkage")


## Let's use the cluster tree to create our clusters
#3 clusters
bike_HClusters_3C <- cutree(bike_HClust_tree_complete, 3) #3 Complete
bike_HClusters_3S <- cutree(bike_HClust_tree_single, 3) #3 Single

plotSetupCBD(main = "Bike Rack Clusters - 3, Complete Linkage")
points(bikeLocations, pch=21, bg = bike_HClusters_3C)

plotSetupCBD(main = "Bike Rack Clusters - 3, Single Linkage")
points(bikeLocations, pch=21, bg = bike_HClusters_3S)


#Try 5 clusters
bike_HClusters_5C <- cutree(bike_HClust_tree_complete, 5) #5 Complete
bike_HClusters_5S <- cutree(bike_HClust_tree_single, 5) #5 Single

plotSetupCBD(main = "Bike Rack Clusters - 5, Complete Linkage")
points(bikeLocations, pch=21, bg = bike_HClusters_5C)

plotSetupCBD(main = "Bike Rack Clusters - 5, Single Linkage")
points(bikeLocations, pch=21, bg = bike_HClusters_5S)


#Try clustering by distance apart
KM_TO_DEG = 1/100 #Approx
bike_HClusters_H005 <- cutree(bike_HClust_tree_complete, h = 0.5 * KM_TO_DEG) #500m Complete

plotSetupCBD(main = "Bike Rack Clusters - 500m, Complete Linkage")
points(bikeLocations, pch=21, bg = bike_HClusters_H005)