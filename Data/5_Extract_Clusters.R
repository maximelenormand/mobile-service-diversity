# Option
options(scipen=10000)

# Working directory
#setwd("")

# Load data
H = read.csv2("Data/Tables/H.csv")
LU = read.csv2("Data/Tables/LU.csv")

# Format data
h= as.matrix(H[,-c(1,2)])
lu = as.matrix(LU[,-c(1,2)])
lu = lu/apply(lu,1,sum)

# WARD H
set.seed(1)
d <- as.matrix(dist(h)) # Dissimilarity matrix based on the Euclidean distance
w <- hclust(as.dist(d), method = "ward.D2") # Clustering Ward D2

# Choose number of clusters based on the ration vaintra/vartot
n <- dim(d)[1]
nclustmax <- 40
vars <- rep(0, nclustmax)
vartot <- sum(d^2) / (2 * (n^2)) # Variance total
for (i in 1:nclustmax) {
  clu <- cutree(w, i)
  varintra <- rep(0, i)
  for (k in 1:i) {
    lg <- which(clu == k)
    varintra[k] <- (sum(d[lg, lg]^2) / (2 * sum(clu == k)^2)) * (sum(clu == k) / n) # Variance intra per cluster
  }
  vars[i] <- sum(varintra) / vartot # Ratio
}

pdf("FigS3.pdf", width = 8.322917, height = 5.811220, useDingbats = FALSE)
  par(mar = c(5, 7, 1, 1))
  par(mfrow = c(1, 1))
  plot(1:nclustmax, vars, typ = "b", 
       cex = 2, lwd = 3, pch = 16, col = "steelblue3", 
       axes = FALSE, xlab = "", ylab = "", 
       xlim = c(1, nclustmax), ylim = c(0.5, 1))
  box(lwd = 1.5)
  axis(1, las = 1, cex.axis = 1.7, lwd = 1.5, padj = 0.2)
  axis(2, las = 1, cex.axis = 1.75, lwd = 1.5, at = seq(0, 1, 0.2))
  mtext("Number of clusters", 1, line = 3.5, cex = 2)
  mtext("Within / total variance", 2, line = 5, cex = 2)
  abline(v = 4, col = "#CC6666", lwd = 3)
dev.off()

# Four clusters identified
com <- cutree(w, 4)
temp = com
com[temp==1]=3
com[temp==3]=1

table(com)
table(com) / sum(table(com))
  
sig=aggregate(h,list(com),mean)  

#x11()
#matplot(t(sig[,-1]),type="b")

hclu = com

# WARD LU
set.seed(1)
d <- as.matrix(dist(lu)) # Dissimilarity matrix based on the Euclidean distance
w <- hclust(as.dist(d), method = "ward.D2") # Clustering Ward D2

# Choose number of clusters based on the ration vaintra/vartot
n <- dim(d)[1]
nclustmax <- 40
vars <- rep(0, nclustmax)
vartot <- sum(d^2) / (2 * (n^2)) # Variance total
for (i in 1:nclustmax) {
  clu <- cutree(w, i)
  varintra <- rep(0, i)
  for (k in 1:i) {
    lg <- which(clu == k)
    varintra[k] <- (sum(d[lg, lg]^2) / (2 * sum(clu == k)^2)) * (sum(clu == k) / n) # Variance intra per cluster
  }
  vars[i] <- sum(varintra) / vartot # Ratio
}

pdf("FigS4.pdf", width = 8.322917, height = 5.811220, useDingbats = FALSE)
  par(mar = c(5, 7, 1, 1))
  par(mfrow = c(1, 1))
  plot(1:nclustmax, vars, typ = "b", cex = 2, lwd = 3, pch = 16, col = "steelblue3", axes = FALSE, xlab = "", ylab = "", xlim = c(1, nclustmax), ylim = c(0, 1))
  box(lwd = 1.5)
  axis(1, las = 1, cex.axis = 1.7, lwd = 1.5, padj = 0.2)
  axis(2, las = 1, cex.axis = 1.75, lwd = 1.5, at = seq(0, 1, 0.2))
  mtext("Number of clusters", 1, line = 3.5, cex = 2)
  mtext("Within / total variance", 2, line = 5, cex = 2)
  abline(v = 4, col = "#CC6666", lwd = 3)
dev.off()

# Four clusters identified
com <- cutree(w, 4)
temp = com
com[temp==1]=4
com[temp==4]=1

table(com)
table(com) / sum(table(com))

sig=aggregate(lu,list(com),mean)  

#x11()
#matplot(t(sig[,-1]),type="b")

luclu = com

# Export Clusters
clu=data.frame(City=H$City, IRIS=H$IRIS, Cluster_H=hclu, Cluster_LU=luclu)
write.csv2(clu, "Data/Tables/Clusters.csv", row.names = FALSE)









  
  
  
  



        

