# Option
options(scipen=10000)

# Working directories
#setwd("")

# Load data
H = read.csv2("Data/Tables/H.csv")
LU = read.csv2("Data/Tables/LU.csv")
clu = read.csv2("Data/Tables/Clusters.csv")

# Format data
idcit=H$City
h= as.matrix(H[,-c(1,2)])
hclu = as.numeric(clu$Cluster_H)
lu = as.matrix(LU[,-c(1,2)])
lu = lu/apply(lu,1,sum)
luclu = as.numeric(clu$Cluster_LU)

# Figure 5
nbh=as.numeric(table(hclu))
nbh=100*nbh/sum(nbh)
print(round(nbh))
print(sum(round(nbh)))

muh=aggregate(h,list(hclu),mean) 
sdh=aggregate(h,list(hclu),sd) 

print(apply(muh[,-1],1,mean))
print(apply(sdh[,-1],1,mean))

pdf("Fig5.pdf",width=12.510417,height=6.285542,useDingbats=FALSE)
  
  colo=c("#EA4335","#FBBC05","#34A853","#4285F4")
  
  par(mar=c(6,6,1,1))
  matplot(seq(0.5,95.5,1),t(muh[,-1]),
          type="l",
          lty=c(1,1,1,1),
          lwd=c(3,3,3,3),
          col=colo,
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
  
  legend("bottomleft",inset=c(0.315,0),col=colo,lwd=3,lty=1,
         legend=c("TD1 (21%)",
                  "TD2 (16%)",
                  "TD3 (40%)",
                  "TD4 (23%)"),
         bty="n",cex=1.4,xpd=TRUE)
  
dev.off() 

# Figure 6
idlu=as.numeric(substr(colnames(lu),2,nchar(colnames(lu))))
nblu=as.numeric(table(luclu))
nblu=100*nblu/sum(nblu)
print(nblu)
print(round(nblu))
print(sum(round(nblu)))

mulu=rbind(aggregate(lu,list(luclu),mean)[,-1],apply(lu,2,mean)) 
sdlu=rbind(aggregate(lu,list(luclu),sd)[,-1],apply(lu,2,sd)) 

pdf("Fig6.pdf",width=19.114583,height=6.649169,useDingbats=FALSE)

  colo=c("#EA4335","#FBBC05","#34A853","#4285F4","grey")

  par(mar=c(5,6,1,1))
  barplot(as.matrix(mulu), beside=TRUE, names.arg = idlu, 
          col = colo, border = colo, axes =FALSE, xlim=c(5,155), ylim=c(0,0.7))
  #axis(1,at=seq(0,96,6),labels=c(seq(0,18,6),seq(0,18,6),seq(0,18,6),seq(0,18,6),0),cex.axis=1.4,las=1)
  axis(2,cex.axis=1.4,las=1)
  mtext("Land use type", side=1,line=3.5,cex=2)
  mtext("Fraction of surface area",side=2,line=4,cex=2)
  
  legend("topright",inset=c(0,0),fill=colo,border=colo,
         legend=c("LU1 (32%)",
                  "LU2 (33%)",
                  "LU3 (23%)",
                  "LU4 (12%)",
                  "All (100%)"),
         bty="n",cex=1.4,xpd=TRUE)

dev.off()

# Table S1
tex <- paste0(idlu, " & ", 
              round(as.numeric(mulu[5,]), digits=2), " (", round(as.numeric(sdlu[5,]), digits=2), ") & ",
              round(as.numeric(mulu[1,]), digits=2), " (", round(as.numeric(sdlu[1,]), digits=2), ") & ",
              round(as.numeric(mulu[2,]), digits=2), " (", round(as.numeric(sdlu[2,]), digits=2), ") & ",
              round(as.numeric(mulu[3,]), digits=2), " (", round(as.numeric(sdlu[3,]), digits=2), ") & ",
              round(as.numeric(mulu[4,]), digits=2), " (", round(as.numeric(sdlu[4,]), digits=2), ") \\") 
              
print(data.frame(tex), row.names = FALSE)
  
# Table 2
njg=as.matrix(table(luclu,hclu))

chisq.test(njg)

nj=replicate(dim(njg)[2], apply(njg, 1, sum))  
ng=t(replicate(dim(njg)[1], apply(njg, 2, sum)))        
n=sum(njg)

chisq.test((nj*ng)/n)

num=njg-((nj*ng)/n)
den=sqrt((n-ng)/(n-1)*(1-(nj/n))*((nj*ng)/n))
Vjg=num/den

Vjg

tex <- paste0(paste0("\textbf{LU",1:4, "}"), " & ", 
              round(Vjg[,1], digits=2), " & ",
              round(Vjg[,2], digits=2), " & ",
              round(Vjg[,3], digits=2), " & ",
              round(Vjg[,4], digits=2), " \\") 

print(data.frame(tex), row.names = FALSE)

# Figure 7
north = c(3,5,7,9,11,12,14,15,16,18,20)
south = c(1,2,4,6,8,10,13,17,19)

tab=table(idcit,hclu)
tab=as.matrix(tab/apply(tab,1,sum))
tab1=tab[north,] # North
tab2=tab[south,] # South

box <- list() # List of values for the boxplots
box[[1]] <- tab2[,4]
box[[2]] <- tab1[,4]
box[[3]] <- tab2[,3]
box[[4]] <- tab1[,3]
box[[5]] <- tab2[,2]
box[[6]] <- tab1[,2]
box[[7]] <- tab2[,1]
box[[8]] <- tab1[,1]

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { 
  b$stats[1, i] <- min(box[[i]])
  b$stats[5, i] <- max(box[[i]])
}
ba=b

tab=table(idcit,luclu)
tab=as.matrix(tab/apply(tab,1,sum))
tab1=tab[north,] # North
tab2=tab[south,] # South

box <- list() # List of values for the boxplots
box[[1]] <- tab2[,4]
box[[2]] <- tab1[,4]
box[[3]] <- tab2[,3]
box[[4]] <- tab1[,3]
box[[5]] <- tab2[,2]
box[[6]] <- tab1[,2]
box[[7]] <- tab2[,1]
box[[8]] <- tab1[,1]

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { 
  b$stats[1, i] <- min(box[[i]])
  b$stats[5, i] <- max(box[[i]])
}
bb=b

pdf("Fig7.pdf",width=14.895833,height=5.049213,useDingbats=FALSE)

  colo <- c("#DE77AE","#80CDC1")
  
  #par(mfrow=c(1,2))
  layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE), width = c(1, 1, 0.4))
  
  #a
  par(mar=c(7,5,1,1))
  bxp(ba, notch=FALSE, at = 1:8, outline = FALSE, boxcol = colo, whiskcol = colo, 
      whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.75, 
      staplecol = colo, medbg = colo, boxfill = colo, cex.axis = 1.25, las = 1, 
      axes = FALSE, xlab = "", ylab = "", horizontal=TRUE, ylim=c(0,0.8))
  axis(1, at=seq(0,0.8,0.1), las = 1, cex.axis = 1.75)
  axis(2, at=c(1.5,3.5,5.5,7.5), labels=c("TD4","TD3","TD2","TD1"), 
       las = 1, cex.axis = 2, tick = FALSE, font = 2, padj = 0.5)
  mtext("Fraction of IRIS", 1, line=4.5, cex=1.75)

  #b
  par(mar=c(7,5,1,1))
  bxp(bb, notch=FALSE, at = 1:8, outline = FALSE, boxcol = colo, whiskcol = colo, 
      whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.75, 
      staplecol = colo, medbg = colo, boxfill = colo, cex.axis = 1.25, las = 1, 
      axes = FALSE, xlab = "", ylab = "", horizontal=TRUE, ylim=c(0,0.6))
  axis(1, at=seq(0,0.6,0.1), las = 1, cex.axis = 1.75)
  axis(2, at=c(1.5,3.5,5.5,7.5), labels=c("LU4","LU3","LU2","LU1"), 
       las = 1, cex.axis = 2, tick = FALSE, font = 2, padj = 0.5)
  mtext("Fraction of IRIS", 1, line=4.5, cex=1.75)

  #Legend
  plot(1, type="n", axes=FALSE, xlab="",ylab="")
  legend("right", inset = c(-0.1, 0), fill = rev(colo), 
         legend = c("North (11 cities)", "South (9 cities)"), border = NA, 
         bty = "n", cex = 2.5, xpd = NA)
  
dev.off()  
  
  

