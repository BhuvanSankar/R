p0 = c(1,2,3,4,5)
p1 = c(2,0,1,5,-1)

#L1 Dist
sum( abs(p0-p1) )

#L2 Dist
sqrt( sum( (p1-p0)^2 ) )



points = matrix(
  c(
    1,1,
    1,4,
    5,4
  ),
  ncol=2,
  byrow = TRUE #Default is to treat matricies by column / 'downwards' first
)
# e.g. points = matrix(c(1,1,1,4,4,5),ncol=2,byrow=TRUE) #short version
# Technical asise, setting points to a matrix masks the function graphics::points. Call as written if needed

plot(points, col="red") #uses col1 as x, col2 as y

#Distance matrix
x= dist(points)
as.matrix( dist(x) )






distMatrix = matrix(nrow=5,ncol=5)

distMatrix[,3] = distMatrix[3,] = c(3,4,0,1,7)
distMatrix[,4] = distMatrix[4,] = c(2,5,1,0,7)


#Look at the set up matrix
distMatrix


#min-linkage, joining 3 and 4
pmin(distMatrix[,3], distMatrix[,4])[-4] #parallel (here, pairwise) minimum, but get rid of result[4].

#max-linkage, joining 3 and 4
pmax(distMatrix[,3], distMatrix[,4])[-4] #parallel (here, pairwise) maximum.
#nb note [3]==1, error below

##Update the dist matrix. We'll join 4 onto 3:

#min linkage
updatedMinLDistMatrix = distMatrix
updatedMinLDistMatrix[,3] = updatedMinLDistMatrix[3,] = pmin(updatedMinLDistMatrix[,3], updatedMinLDistMatrix[,4]) #4 merge-> 3
updatedMinLDistMatrix = updatedMinLDistMatrix[-4,-4] #remove 4 (eg joined to 3)

updatedMinLDistMatrix

#max linkage
updatedMaxLDistMatrix = distMatrix
updatedMaxLDistMatrix[,3] = updatedMaxLDistMatrix[3,] = pmax(updatedMaxLDistMatrix[,3], updatedMaxLDistMatrix[,4]) #4 merge-> 3
updatedMaxLDistMatrix[3,3] = 0 # Be careful with max linkage - max(0,1)!=0.
updatedMaxLDistMatrix = updatedMaxLDistMatrix[-4,-4] #remove 4 (eg joined to 3)

updatedMaxLDistMatrix
