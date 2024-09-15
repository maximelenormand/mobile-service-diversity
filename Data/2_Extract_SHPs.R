# Packages
library(sf)

# Option
options(scipen=10000)

# Working directory
#setwd("")

# Cities 
cits = list.files("Data/SHPs/RAW_NB")    # List cities in json files (NetMob2023 dataset)
cits =  cits[cits!="source"]
cits = substr(cits, 1, nchar(cits)-8)
ncits = length(cits)                     # Number of cities

# IRIS 
iris = st_read("Data/SHPs/RAW_IRIS/CONTOURS-IRIS_2-1__SHP__FRA_2021-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2021-06-00217/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2021/CONTOURS-IRIS.shp", quiet = TRUE)
iris = cbind(iris, as.numeric(st_area(iris)/1000000))
iris = iris[,c(4,6,7)]
colnames(iris)[1:3]=c("IRIS","Type","Area")

iris$IRIS=as.numeric(iris$IRIS)
iris=iris[!is.na(iris$IRIS),]

# Loop cities 
int=NULL
for(i in 1:ncits){

  print(c(i,ncits))
  
  # Original grid in NetMob2023 dataset
  nbi = st_read(paste0("Data/SHPs/RAW_NB/", cits[i], ".geojson"), quiet = TRUE)
  nbi = st_transform(nbi,2154)
  
  # Boundaries City & IRIS inside
  boui = st_sf(st_union(nbi))
  inti = st_intersection(boui, iris, sparse = FALSE)
  fracarea = as.numeric(st_area(inti)/1000000)/inti$Area 
  irisi = cbind(City=cits[i],iris[match(inti$IRIS[fracarea>0.75], iris$IRIS),])
  if(i == 1){
    shp = irisi
  }else{
    shp = rbind(shp, irisi)
  }
  
  #x11()
  #plot(st_geometry(boui), border="red")
  #plot(st_geometry(irisi), add=TRUE)
  
  # Intersection between the original grid cell and the IRIS
  inti = st_intersection(nbi, irisi)
  inti = cbind(City=cits[i],inti[,c(1,3)],as.numeric(st_area(inti)/1000000))
  inti = inti[, c(1,3,2,4), drop=TRUE]
  colnames(inti)=cbind("City","IRIS","ID","Area")
  
  int = rbind(int, inti)
  
}

# Export IRIS and intersection between IRIS and origanal grid
st_write(shp, "Data/SHPs/IRIS.shp", append = FALSE)
write.csv2(int, "Data/SHPs/IRIS_NB.csv", row.names = FALSE)


