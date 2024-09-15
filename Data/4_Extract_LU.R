# Packages
library(sf)
library(stringr)
library(DescTools)

# Option
options(scipen=10000)

# Working directory
#setwd("")

# Cities
cits = list.files("Data/SHPs/RAW_NB")  # Extract the list of cities
cits =  cits[cits!="source"]
cits = substr(cits, 1, nchar(cits)-8)
ncits = length(cits)                   # Number of cities

# IRIS
shp = st_read("Data/SHPs/IRIS.shp")
shp = st_transform(shp, 3035)

# LU 
flus = list.files("Data/SHPs/RAW_LU")
flus =  flus[flus!="source" & flus!="LU.png"]
flus = substr(flus, 1, nchar(flus)-4)
temp = as.character(str_split_fixed(flus, "_", 4)[,2])
temp[18] = "Mans"
flus = flus[order(temp)]

idlu=c(11100,11210,11220,11230,11240,11300,
       12100,12210,12220,12230,12300,12400,
       13100,13300,13400,
       14100,14200,
       21000,22000,23000,24000,25000,
       31000,32000,33000,
       40000,50000)

# Extract LU
lu = NULL
for(i in 1:ncits){ # Loop cities
  
  print(c(i,ncits))

  # Unzip data
  unzip(paste0("Data/SHPs/RAW_LU/", flus[i], ".zip"), 
        files = paste0(flus[i], "/Data/", flus[i], ".gpkg"),
        junkpaths = TRUE,
        exdir = "Data/SHPs/RAW_LU/temp")
  
  # Import shp & remove temp
  shplu = st_read(paste0("Data/SHPs/RAW_LU/temp/",  flus[i], ".gpkg"), 
               quiet = TRUE)
  unlink("Data/SHPs/RAW_LU/temp", recursive = TRUE) 
  
  # Extract shp city
  shpi = shp[shp$City==cits[i],]

  # Intersection
  int = st_intersection(shpi, shplu)
  int = data.frame(IRIS=int$IRIS, 
                   LU = int$code_2018, 
                   Area = as.numeric(st_area(int)/1000000))
  temp = data.frame(IRIS = int$IRIS[1], LU=idlu, Area=0)
  int=rbind(int,temp)
  lui = as.matrix.xtabs(xtabs(Area ~ IRIS + LU, data = int))
  lui = data.frame(City=cits[i], IRIS = as.numeric(rownames(lui)), lui)
  
  print(dim(lui))  
  
  lu = rbind(lu,lui)
  
}

# Export LU
write.csv2(lu, "Data/Tables/LU.csv", row.names=FALSE)

