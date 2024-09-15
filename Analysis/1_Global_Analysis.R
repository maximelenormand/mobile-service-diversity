# Option
options(scipen=10000)

# Working directories
#setwd("")

# H
H = read.csv2("Data/Tables/H.csv")
idcit = H[,1]
h = as.matrix(H[,-c(1,2)])

# Cities
cits = idcit[!duplicated(idcit)]
ncits = length(cits)

# Parameters (id north and souch cities)
north = c(3,5,7,9,11,12,14,15,16,18,20)
south = c(1,2,4,6,8,10,13,17,19)

# Figure 3 
muh=apply(h,2,mean)
sdh=apply(h,2,sd)

print(c(mean(muh),mean(sdh)))
print(cbind(c(rep(seq(0.5,23.5),4)),muh))
print(c(mean(muh[1:48]),mean(muh[49:96])))

pdf("Fig3.pdf",width=12.510417,height=6.285542,useDingbats=FALSE)

  colo="steelblue3"
  
  par(mar=c(6,6,1,1))
  matplot(seq(0.5,95.5,1),cbind(muh,muh-sdh,muh+sdh),
          type="l",
          lty=c(1,3,3),
          lwd=c(3,2,2),
          col=rep(colo,3),
          xlim=c(0,96), ylim=c(0.65,0.85),
          xlab=" ", ylab=" ", axes=FALSE, xaxs="i")
  axis(1,at=seq(0,96,6),labels=c(seq(0,18,6),seq(0,18,6),seq(0,18,6),seq(0,18,6),0),cex.axis=1.4,las=1)
  axis(2,cex.axis=1.4,las=1)
  mtext("Time of day (in hours)", side=1,line=3.5,cex=2)
  mtext("H",side=2,line=4.5,las=1,cex=2)
  abline(v=24)
  abline(v=48)
  abline(v=72)
  abline(v=96)
  box(lwd=1.5)
  
  text(0,0.85,"Thursday",font=4,pos=4,col="#525252",cex=1.5)
  text(24,0.85,"Friday",font=4,pos=4,col="#525252",cex=1.5)
  text(48,0.85,"Saturday",font=4,pos=4,col="#525252",cex=1.5)
  text(72,0.85,"Sunday",font=4,pos=4,col="#525252",cex=1.5)

  legend("bottomleft",inset=c(0.1,0),col=colo,lwd=c(3,2),lty=c(1,3),legend=c("Mean","SD"),bty="n",cex=1.5,xpd=TRUE)

dev.off() 

# Figure 4
cith=aggregate(h,list(idcit),mean)
cith=apply(cith[,-1],1,mean)
cith=data.frame(cits,cith)

cith$cits[cith$cits=="Mans"]="Le Mans"
cith$cits[cith$cits=="Orleans"]="Orléans"

colo=rep("#80CDC1",ncits)
colo[south]="#DE77AE"
colo=colo[order(cith[,2])]

cith=cith[order(cith[,2]),]

pdf("Fig4.pdf",width=8.083333,height=6.995698,useDingbats=FALSE)

  par(mar = c(5, 14, 1, 2))
  plot(cith[,2], 1:ncits, type = "n", 
       col = colo, pch = 16, cex = 3,
       axes = FALSE, xlab = "", ylab = "", xlim = c(0.73, 0.78))
  for (g in 1:ncits) {
    abline(g, 0, col = "grey", lty = 3)
  }
  par(new = TRUE)
  plot(cith[,2], 1:ncits, 
       col = colo, pch = 16, cex = 3,
       axes = FALSE, xlab = "", ylab = "", xlim = c(0.73, 0.78), xaxs = "i")
  
  axis(1, las = 1, cex.axis = 1.7)
  axis(2, at = 1:ncits, labels = cith[,1], las = 2, tick = FALSE, cex.axis = 1.7, font = 2, padj = 0.5)
  mtext("H", 1, line = 3.5, cex = 2)
  box(lwd = 1.5)
  
  legend("topleft", inset = c(0, 0), pch=16, col = c("#80CDC1","#DE77AE"), 
         legend = c("North (11 cities)", "South (9 cities)"), border = NA, 
         bty = "n", cex = 2, xpd = NA, pt.cex = 3)

dev.off() 

# Figure S2
cith$cits[cith$cits=="Le Mans"]="Mans"
cith$cits[cith$cits=="Orléans"]="Orleans"

pval=matrix(-1,20,20)
for(i in 1:ncits){
  
   print(c(i,ncits))

   Hi=H[H$City==cith$cits[i],-c(1,2)]

   for(j in 1:ncits){

     Hj=H[H$City==cith$cits[j],-c(1,2)]

     test=0
     for(k in 1:96){
        test=test+(wilcox.test(Hi[,k],Hj[,k], alternative = "greater")$p.<0.05)
     }
     pval[i,j]=test

   }
}

cith$cits[cith$cits=="Mans"]="Le Mans"
cith$cits[cith$cits=="Orleans"]="Orléans"

source("Analysis/myImagePlot.R")
pdf("FigS2.pdf", width=15, height=12, useDingbats=FALSE)

  myImagePlot(pval/96, 
              cith[,1], 
              cith[,1], 
              0, 1)
  
dev.off()




