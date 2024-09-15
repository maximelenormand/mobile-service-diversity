# Option
options(scipen=10000)

# Working directory
#setwd("")

# Volumes per week
vol1 = read.csv2("Data/Tables/Volume_1.csv")
vol2 = read.csv2("Data/Tables/Volume_2.csv")
vol3 = read.csv2("Data/Tables/Volume_3.csv")

vol1 = as.matrix(vol1[,-1])
vol2 = as.matrix(vol2[,-1])
vol3 = as.matrix(vol3[,-1])

vol=(vol1 + vol2 + vol3)/3
vol=apply(vol[,-1],2,sum)

# Ranking services
rank=cbind(1:68,vol)
rank=rank[order(rank[,2],decreasing=TRUE),]

# Selected MS
indMS=c(1:4,6,8,12,15:18,21,23,25,28,31,32) 
length(indMS)

MSin=rownames(rank)
MSin[-indMS]=""
MSout=rownames(rank)
MSout[indMS]=""

# Figure S1
pdf("FigS1.pdf", width=14.281250, height=6.742673, useDingbats=FALSE)

  colo="steelblue3"
  
  par(mar=c(11,6,1,0))
  barplot(rank[,2]/10^12, beside = TRUE, 
          axes = FALSE, xlab = "", ylab = "", names.arg = rep("",68),
          xlim = c(2,80), ylim = c(0,14),
          col = colo, border = NA)
  
  axis(1, at=seq(0.6,81,length.out=68), labels = MSout, 
       las=2, cex.axis=1, tick=FALSE)
  axis(1, at=seq(0.6,81,length.out=68), labels = MSin, 
       las=2, cex.axis=1, tick=FALSE, font=2)
  axis(2, las = 1, cex.axis=1)
  mtext(expression(paste("Traffic (x ",10^12,")",sep="")), 2,
        line=3.25, cex=1.5)
  
dev.off()

# Index MS
as.numeric(rank[indMS,1])



