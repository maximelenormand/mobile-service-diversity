# Packages
library(readr)
library(DescTools)

# Option
options(scipen=10000)

# Working directory
#setwd("")

# Parameters (index of the 17 selected mobile services)
indMS = c(27,16,68,36,17,43,13,28,50,49,12,64,35,37,39,40,45)

# Cities
cits = list.files("Data/SHPs/RAW_NB")  # Extract the list of cities
cits =  cits[cits!="source"]
cits = substr(cits, 1, nchar(cits)-8)
ncits = length(cits)                   # Number of cities

# Load file containing the intersection between IRIS and original grid cell
nbiris = read.csv2("Data/SHPs/IRIS_NB.csv")

# Extract H
res=NULL
for(i in 1:ncits){ # Loop cities
  
  print(c(i,ncits))
  
  # Transition matrix from CELL to IRIS based on the intersecting surface area
  nbirisi = nbiris[nbiris$City == cits[i],]
  P = as.matrix.xtabs(xtabs(Area ~ ID + IRIS, data = nbirisi))
  P = t(P/apply(P,1,sum))
  
  # Loops days and hours
  resi=data.frame(City=cits[i], IRIS=as.numeric(rownames(P)))
  
  for(j in 1:4){ 
    for(k in 1:24){
      
      # Rep 1
      mat1=read_delim(paste0("Data/NB/", cits[i], "_1", 
                            "/WD_", j, "_", k, ".csv"), 
                     delim=";", 
                     col_name=TRUE,
                     show_col_types = FALSE)
      
      match1=match(as.numeric(colnames(P)),as.numeric(as.matrix(mat1[,1])))
      mat1=as.matrix(mat1[,-1])
      mat1=mat1[match1,indMS]
      
      # Rep 2
      mat2=read_delim(paste0("Data/NB/", cits[i], "_2", 
                             "/WD_", j, "_", k, ".csv"), 
                      delim=";", 
                      col_name=TRUE,
                      show_col_types = FALSE)
      match2=match(as.numeric(colnames(P)),as.numeric(as.matrix(mat2[,1])))
      mat2=as.matrix(mat2[,-1])
      mat2=mat2[match2,indMS]
      
      # Rep 3
      mat3=read_delim(paste0("Data/NB/", cits[i], "_3", 
                             "/WD_", j, "_", k, ".csv"), 
                      delim=";", 
                      col_name=TRUE,
                      show_col_types = FALSE)
      match3=match(as.numeric(colnames(P)),as.numeric(as.matrix(mat3[,1])))
      mat3=as.matrix(mat3[,-1])
      mat3=mat3[match3,indMS]
    
      # H IRIS
      mat=(mat1 + mat2 + mat3)/3
      mat=P%*%mat
      mat=mat/apply(mat,1,sum)
      mat=mat*log(mat)
      mat[is.na(mat)]=0
      H=apply(mat,1,sum)
      H=-H/log(dim(mat)[2])
      
      resi=cbind(resi,H)
      colnames(resi)[dim(resi)[2]]=paste0("H_",j,"_",k)
      
    }
  }
  
  res = rbind(res, resi)
  
  gc()
  
}

# Export H
write.csv2(res, "Data/Tables/H.csv", row.names = FALSE)
        
        
   