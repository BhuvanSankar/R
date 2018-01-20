

#HITS:

pages = c("index.html", "step1.html", "step2.html", "step3.html", "done.html")


pageLinksList = 
  list(
    index = c("step1.html", "step2.html", "step3.html", "done.html"),
    step1 = c("index.html", "step2.html"),
    step2 = c("index.html", "step1.html", "step3.html"),
    step3 = c("index.html", "step2.html", "done.html"),
    done = c("index.html", "step3.html", "step1.html")
  )


linkMatrix = t( sapply( pageLinksList, function(links){setNames(pages %in% links, pages)} ) )




##HITS:

authorities = numeric( length(pages) )+1
hubs = numeric( length(pages) )+1


for(i in 1:100){
  
  #I-step
  #hubFactorPassed = linkMatrix * hubs
  #authorities = colSums( hubFactorPassed )
  
  authorities = c( hubs %*% linkMatrix )
  #normalise
  authorities = authorities / dist(rbind(0,authorities))
  
  
  #O-step
  #authFactorPassed = t( t(linkMatrix) * authorities )
  #hubs = rowSums( authFactorPassed )
  
  hubs = c( linkMatrix %*% authorities )
  hubs = hubs / dist(rbind(0,hubs))
  
  print( list(steps=i, authorities=authorities, hubs=hubs) )
}


##Page Rank:
pageRanks = numeric( length(pages) ) + 1/length(pages)

linkMatrixPr = linkMatrix / rowSums(linkMatrix)

for(i in 1:100){
  pageRanks = pageRanks %*% linkMatrixPr #Works provided the limiting distribution exists
}

#damped
pageRanksDamped = numeric( length(pages) ) + 1/length(pages)
dampFactor = 0.9

for(i in 1:100){
  pageRanksDamped = dampFactor * (pageRanksDamped %*% linkMatrixPr) + #Chance we follow a link
    (1-dampFactor) * (1/length(pages)) #chance we jump anywhere randomly
}

#damped equivalent:
linkMatrixPrDamped = linkMatrixPr*dampFactor + (1-dampFactor)/length(pages)

pageRanksDamped2 = numeric( length(pages) ) + 1/length(pages)
for(i in 1:100){
  pageRanksDamped2 = pageRanksDamped2 %*% linkMatrixPrDamped
}


#fancy maths:
linkMatrixMinusDiag = linkMatrixPr - diag(1, nrow=nrow(linkMatrixPr))

linkMatrixForSolve = rbind( t( linkMatrixMinusDiag[,-ncol(linkMatrixMinusDiag)]  ), 1)
solve(linkMatrixForSolve, c( rep(0,nrow(linkMatrixForSolve)-1), 1) )

#or, more nicely
eiganSolved = eigen( t(linkMatrixPr) )
eiganSolved$values # find which value equals 1? I got index == 1
#Remember that there is some imprecision in calculating these


stationaryDist = as.numeric( eiganSolved$vectors[,1] / sum(eiganSolved$vectors[,1]) )

stationaryDist
stationaryDist %*% linkMatrixPr
