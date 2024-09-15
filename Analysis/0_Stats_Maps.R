# Packages
library(sf)
library(imager)

# Option
options(scipen=10000)

# Working directories
#setwd("")

# Load data
shp = st_read("Data/SHPs/IRIS.shp")
north = c(3,5,7,9,11,12,14,15,16,18,20)
south = c(1,2,4,6,8,10,13,17,19)

# Table 1
tab=cbind(aggregate(shp$Area, list(shp$City), sum),
          aggregate(shp$Area, list(shp$City), length)[,2], 
          aggregate(shp$Area, list(shp$City), mean)[,2])
colnames(tab)=c("City","Area","Nb_IRIS","Area_IRIS")

tab$City[tab$City=="Mans"]="Le Mans"
tab$City[tab$City=="Orleans"]="Orléans"

tex <- paste0(tab[,1], " & ", 
              round(as.numeric(tab[,2]), digits=1), " & ",
              round(as.numeric(tab[,3]), digits=1), " & ",
              round(as.numeric(tab[,4]), digits=1), " \\") 

print(data.frame(tex), row.names = FALSE)

# Figure 1
shpcit = aggregate(shp, list(shp$City), mean)
shpcit = st_transform(shpcit, 27572) # Europe Lambert Conformal Conic
shpcit1 = shpcit[north,]
shpcit2 = shpcit[south,]

cent=st_coordinates(st_centroid(shpcit)) # Centroids coordinates

AND <- st_read("Data/SHPs/Administratives Boundaries/gadm36_AND_0.shp") # Andorra's administrative boundaries
AUT <- st_read("Data/SHPs/Administratives Boundaries/gadm36_AUT_0.shp") # Austria's administrative boundaries
BEL <- st_read("Data/SHPs/Administratives Boundaries/gadm36_BEL_0.shp") # Belgium's administrative boundaries
CHE <- st_read("Data/SHPs/Administratives Boundaries/gadm36_CHE_0.shp") # Switzerland's administrative boundaries
DEU <- st_read("Data/SHPs/Administratives Boundaries/gadm36_DEU_0.shp") # Germany's administrative boundaries
ESP <- st_read("Data/SHPs/Administratives Boundaries/gadm36_ESP_0.shp") # Spain's administrative boundaries
FRA <- st_read("Data/SHPs/Administratives Boundaries/gadm36_FRA_0.shp") # France's administrative boundaries
GBR <- st_read("Data/SHPs/Administratives Boundaries/gadm36_GBR_0.shp") # United Kingdom's administrative boundaries
ITA <- st_read("Data/SHPs/Administratives Boundaries/gadm36_ITA_0.shp") # Italy's administrative boundaries
LUX <- st_read("Data/SHPs/Administratives Boundaries/gadm36_LUX_0.shp") # Luxembourg's administrative boundaries
NLD <- st_read("Data/SHPs/Administratives Boundaries/gadm36_NLD_0.shp") # Netherlands' administrative boundaries

AND <- st_transform(AND, 27572) # Europe Lambert Conformal Conic
AUT <- st_transform(AUT, 27572)
BEL <- st_transform(BEL, 27572)
CHE <- st_transform(CHE, 27572)
DEU <- st_transform(DEU, 27572)
ESP <- st_transform(ESP, 27572)
FRA <- st_transform(FRA, 27572)
GBR <- st_transform(GBR, 27572)
ITA <- st_transform(ITA, 27572)
LUX <- st_transform(LUX, 27572)
NLD <- st_transform(NLD, 27572)

REG <- st_union(AND, BEL) # Surrounding lands
REG <- st_union(AUT, BEL)
REG <- st_union(REG, CHE)
REG <- st_union(REG, DEU)
REG <- st_union(REG, ESP)
REG <- st_union(REG, GBR)
REG <- st_union(REG, ITA)
REG <- st_union(REG, LUX)
REG <- st_union(REG, NLD)

box <- st_bbox(FRA) # Box
box[1] <- 56634.41 - 170000
box[2] <- 1780889.85 - 200000
box[3] <- 1031641.60 + 200000
box[4] <- 2677385.03 + 50000
box <- st_as_sfc(box)
boxmet <- box

REG <- st_intersection(REG, boxmet) # Intersect Surrounding lands and Box Metropolitan

coloboxmet <- "#D4EDFF"
coloreg <- "#EDECEC"
colofra <- "#BCC5D5"

colo1 <- "#80CDC1"
colo2 <- "#DE77AE"

pdf("Fig1.pdf", width = 7, height = 6, useDingbats = FALSE)
  
  # France
  par(mar = c(0, 0, 0, 0))
  plot(boxmet, col = coloboxmet, border = coloboxmet)
  plot(st_geometry(REG), col = coloreg, border = coloreg, add = TRUE)
  plot(st_geometry(FRA), col = colofra, border = colofra, add = TRUE)
  plot(st_geometry(shpcit1), col = colo1, border = colo1, add = TRUE)
  plot(st_geometry(shpcit2), col = colo2, border = colo2, add = TRUE)
  
  # Legend zone
  legend("topright", inset = c(0.04, 0.03), fill = c(colo1, colo2), 
         legend = c("North (11 cities)", "South (9 cities)"), 
         border = NA, bty = "n", cex = 1.1, xpd = NA)
  
  # Legend 200km
  segments(st_bbox(boxmet)[1] + 100000, st_bbox(boxmet)[2] + 100000, st_bbox(boxmet)[1] + 300000, st_bbox(boxmet)[2] + 100000, lwd = 3, col = "grey2")
  text(st_bbox(boxmet)[1] + 200000, st_bbox(boxmet)[2] + 60000, labels = "200 km", cex = 1.25, font = 2, col = "grey2")
  
  # Cities
  for(i in 1:length(tab$City)){
    text(cent[i, 1]+0, cent[i, 2]+0, labels = tab$City[i], cex = 0.75, col="black",pos=3)
  }
  
dev.off()

# Figure 2
shpi = shp[shp$City == tab$City[1],] # Bordeaux
shpi = st_transform(shpi, 3035)

unzip("Data/SHPs/RAW_LU/FR007L2_BORDEAUX_UA2018_v013.zip", 
      files = "FR007L2_BORDEAUX_UA2018_v013/Data/FR007L2_BORDEAUX_UA2018_v013.gpkg",
      junkpaths = TRUE,
      exdir = "Data/SHPs/RAW_LU/temp")
shplu = st_read("Data/SHPs/RAW_LU/temp/FR007L2_BORDEAUX_UA2018_v013.gpkg", 
                quiet = TRUE)
unlink("Data/SHPs/RAW_LU/temp", recursive = TRUE) 

int = st_intersection(shpi, shplu)

shplui = aggregate(int, list(int$code_2018), length)
shplui = shplui[dim(shplui)[1]:1,1]

cololu=c("#800000","#BE0000","#FE4040","#FE8081","#FFBFBF","#CC6566",
         "#CC4DF2","#959595","#B3B3B3","#595959","#E7CCCC","#E6CCE7",
         "#734D38","#B9A66E","#874546",
         "#8CDC01","#AED2A4",
         "#FFFFA7","#F2A64E","#E6E74D","#FFE64C","#F2CC81",
         "#008C00","#CCF24D","#CDFFCC",
         "#A6A6FE","#80F3E6")
cololu=cololu[-c(22,25)]
cololu=rev(cololu)
      
luleg=load.image("Data/SHPs/RAW_LU/LU.png") 

#x11(width = 9.791667, height = 5.288167)
png("Fig2.png", width = 9.791667, height = 5.288167, units="in", res = 300) 
#pdf("Fig2.pdf", width = 9.791667, height = 5.288167, useDingbats = FALSE)

  layout(matrix(c(1, 2), 1, 2, byrow = TRUE), width = c(1, 0.6))

  par(mar = c(0,0,0,0))
  plot(st_geometry(shplui), col=cololu, border=cololu)
  plot(st_geometry(shpi), add=TRUE)
  
  par(mar=c(2,0,2,0))
  plot(luleg,xlab="",ylab="",axes=FALSE)

dev.off()











