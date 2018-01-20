brisCBDLat = c(-27.482719,-27.460718)
brisCBDLong = c(153.012146,153.036705)

#Plot Images
require("png")
brisCBDIm = readPNG("CBD Map.png")
brisCBDIm = as.raster(brisCBDIm)

plotSetupCBD = function(...){
  xlabPlot = if(!hasArg(xlab)) "Longitude"
  ylabPlot = if(!hasArg(ylab)) "Latitude"
  win.graph()
  plot(c(), c(), xlim = brisCBDLong, ylim = brisCBDLat, xaxs="i", yaxs="i",
       xlab=xlabPlot,ylab=ylabPlot,...)
  
  rasterImage(brisCBDIm, brisCBDLong[1], brisCBDLat[1], brisCBDLong[2], brisCBDLat[2])
}