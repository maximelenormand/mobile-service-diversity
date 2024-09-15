myImagePlot <- function(x1, rowNames, colNames,m,M){
  
  min=m 
  max=M 
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=1)
  
  # Colors
  ColorRamp=colorRampPalette(c("#CC6666","white","steelblue3"))(100)
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x1) : 1
  rowNames <- rowNames[reverse]
  x1 <- x1[reverse,]

  # Data Map
  par(mar = c(15,15,2,2))
  image(1:length(colNames), 1:length(rowNames), t(x1), col=ColorRamp, xlab="",ylab="", axes=FALSE, zlim=c(min,max),las=1)
  axis(1, at=(1:length(colNames)-0.1), las=2, labels=colNames,cex.axis=2,tick=FALSE)
  axis(2, at=(1:length(rowNames)), labels=rowNames, las=1,cex.axis=2,tick=FALSE)
  
  # Color Scale
  par(mar = c(20, 5, 9, 5))
  image(1, ColorLevels,matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),col=ColorRamp,xlab="",ylab="", axes=FALSE)
  axis(2, las=1,cex.axis=1.5)
  #title("AMI", line=2, font=1, cex.main=2.5)
  
  layout(1)
}
