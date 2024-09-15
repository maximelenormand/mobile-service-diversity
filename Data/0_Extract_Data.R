# Packages
library(readr)

# Working directories
wdataraw="/media/maxime/DD/NetMob2023/Data/"    # NetMob2023 dataset
wd="/home/maxime/Desktop/GIT"                   # Local working directory
setwd(wd)

# Create NetMob directory (NB) in local
if(!dir.exists("Data/NB")){
  dir.create("Data/NB")
}

# Function to aggregate 4 15-minutes slot into 1-hour slot
extract_hour = function(tab, h){
  ind = (h-1)*4+1
  res = tab[,ind] + tab[,(ind+1)] + tab[,(ind+2)] + tab[,(ind+3)] 
  return(res)
}

# Parameters
rep = 1       # Replication ID [manually change from 1 to 3] corresponds to the
              # three selected weeks

# Cities
cits = list.files(paste0(wdataraw, "Downlink")) # Extract the list of cities
ncits = length(cits)                            # Number of cities

# Initialize count volume per city and service & number of cells per city
vol = matrix(0, ncits, 68)
cel = NULL

# Loop cities
for(i in 1:ncits){
  
  # City
  cit = cits[i]

  # Extract days
  days = read.csv("Data/Days.csv")
  days = days[days$Selected == rep, 1:4]
  
  year = as.character(days$Year)
  month = paste0("0", days$Month)
  day = as.character(days$Day)
  day[nchar(day)==1]=paste0("0",day[nchar(day)==1])
  days = paste0(year, month, day)
  
  print(c(cit, rep, days))
  
  # Loop days
  for(j in 1:4){ 
    
    # Day
    day = days[j]
    
    # Initialize output 
    res = list()
    length(res) = 24
      
    # List and number of services
    servs = list.files(paste0(wdataraw, "Downlink/", cit))
    nservs = length(servs)
      
    # Downlink for service 1
    dl = read_delim(paste0(wdataraw, "Downlink/", cit, "/", 
                              servs[1], "/", 
                              day, "/", 
                              cit, "_", servs[1], "_", day, "_DL.txt"),
                      col_names = FALSE,
                      show_col_types = FALSE) 
    iddl = as.numeric(as.matrix(dl)[,1])
    dl = as.matrix(dl[,-1])
    
    # Uplink for service 1
    ul = read_delim(paste0(wdataraw, "Uplink/", cit, "/", 
                           servs[1], "/", 
                           day, "/", 
                           cit, "_", servs[1], "_", day, "_UL.txt"),
                    col_names = FALSE,
                    show_col_types = FALSE) 
    idul = as.numeric(as.matrix(ul)[,1])
    ul = as.matrix(ul[,-1])
    
    if(sum(idul == iddl) != length(iddl)){
      print("WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
    
    # Cell id & uplink and downlink addition
    id = iddl
    tab = dl + ul
    
    # Update volume and number of cells
    vol[i,1] = vol[i,1] + sum(tab)
    if(j == 1){
      cel = c(cel, dim(tab)[1])
    }
    
    # Loop hours (sum 15-minutes slots)
    for(h in 1:24){
      res[[h]] = matrix(0, length(id), 68)
      res[[h]][,1] = extract_hour(tab, h)
    }
      
    # Loop services
    for(s in 2:nservs){ 
        
      # Service s
      serv = servs[s]
        
      # Import tables
      dl = read_delim(paste0(wdataraw, "Downlink/", cit, "/", 
                             serv, "/", 
                             day, "/", 
                             cit, "_", serv, "_", day, "_DL.txt"),
                      col_names = FALSE,
                      show_col_types = FALSE) 
      iddl = as.numeric(as.matrix(dl)[,1])
      dl = as.matrix(dl[,-1])
      
      ul = read_delim(paste0(wdataraw, "Uplink/", cit, "/", 
                             serv, "/", 
                             day, "/", 
                             cit, "_", serv, "_", day, "_UL.txt"),
                      col_names = FALSE,
                      show_col_types = FALSE) 
      idul = as.numeric(as.matrix(ul)[,1])
      ul = as.matrix(ul[,-1])
      
      if(sum(idul == iddl) != length(iddl)){
        print("WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      }
      
      # Cell id & uplink and downlink addition
      id2 = iddl
      tab = dl + ul
      
      # Update volume
      vol[i,s] = vol[i,s] + sum(tab)
        
      if(sum(id == id2) != length(id)){
        print("WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      }
        
      # Loop hours (sum 15-minutes slots)
      for(h in 1:24){
        res[[h]][,s] = extract_hour(tab, h)
      }
        
    }
    
    # Export output for each week, weekday and hour
    for(h in 1:24){
    
      mat = cbind(id, res[[h]])
      colnames(mat) = c("ID", servs)
    
      write_delim(data.frame(mat), 
                    paste0("Data/NB/", cit, "_", rep, "/WD_", j, "_", h, ".csv"), 
                    delim=";", 
                    col_name=TRUE)
      
    } 
    
    rm(res, mat)
    gc()
    
  }
}

# Export volume in Tables folder
count = data.frame(City = cits, NbCells = cel, vol)
colnames(count)[-c(1,2)]=servs

if(!dir.exists("Data/Tables")){
  dir.create("Data/Tables")
}
write.csv2(count, paste0("Data/Tables/Volume_", rep, ".csv"), row.names = FALSE)






        
        
   