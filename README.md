## Mapping mobile service usage diversity in cities

## Description

This repository contains all the material needed to process the data, perform 
the analysis and produce the figures that can be found in 
[[1]](https://arxiv.org/abs/2311.06269). This study analysed the diversity of 
mobile service usage and its relation to land use distribution within and 
between cities at different scales.

The repository consists of two folders: the **Data** folder, which contains the 
six R scripts used to process the data and extract the intermediate results and
the **Analysis** folder containing three R scripts developed to analyze the data
and produce the various figures and tables presented in the paper. 

## Data

The **Data** folder contains six scripts that should be run in order to 
process the data located in the subfolders **NB** and 
**SHPs** and produce the intermediate results stored in the **Tables** folder.
The datasets are not available in this repository, they should be  
downloaded separately (see **source** files in each folder). However, the 
intermediate files stored in the **Tables** are available to ensure 
reproducibility of most of the results, tables and figures.

***0_Extract_Data.R*** takes as input the raw data provided as 
part of the
[NetMob 2023 Data Challenge](https://netmob2023challenge.networks.imdea.org/) 
and extract/reformat/aggregate a subsample of the original dataset that is 
stored in the **NB** folder. Some intermediate files are also stored in the 
**Tables** folder during this first step.   

***1_Select_MS.R*** takes as input the intermediate files generated at the
previous step and stored in the **Tables** folder to produce the Figure S1.

***2_Extract_SHPs.R*** takes as inputs the original spatial data provided as 
part of the
[NetMob 2023 Data Challenge](https://netmob2023challenge.networks.imdea.org/) 
stored in **SHPs/RAW_NB** folder and the IRIS spatial data stored in 
**SHPs/RAW_IRIS** to intersect the two datasets and produce a shapefile 
***IRIS.shp*** and a file ***IRIS_NB.csv*** stored in the **SHPs** folder.

***3_Extract_H.R*** takes as inputs the file ***IRIS_NB.csv*** produced at the 
previous step and the data stored in the **NB** folder to compute the Shannon 
diversity index by IRIS and hour. The resulting file **H.csv** is stored in the 
**Tables** folder and available in this repository as intermediate result.

***4_Extract_LU.R*** takes as inputs the shapefile ***IRIS.shp*** and land use 
data stored in the **SHPs/RAW_LU** folder to compute 
the surface area by land use type and IRIS. The resulting file **LU.csv** is 
stored in the **Tables** folder and available in this repository as intermediate
result.

***5_Extract_Clusters.R*** takes as inputs the files **H.csv*** and **LU.csv** 
produced at the two previous steps to cluster the IRIS based on their temporal
diversity and land use distribution separately. The resulting file 
**Clusters.csv** is stored in the **Tables** folder and available in this 
repository as intermediate result. This script is also used to produce the 
Figure S2 and Figure S3.

## Analysis

The **Analysis** folder contains three scripts developed to analyze the 
intermediate files stored in the **Tables** folder.

***0_Stats_Maps.R*** is used to produce the Figure 1, Figure 2 and Table 1. 
Three datasets are needed to run this script.

***1_Global_Analysis.R*** is used to produce the Figure 3, Figure 4 and 
Figure S2.

***2_Clustering_Analysis.R*** is used to produce the Figure 5, Figure 6, 
Table S1, Table 2 and Figure 7.

## References

[1] Lenormand M (2023) [Mapping mobile service usage diversity in cities](https://arxiv.org/abs/2311.06269). 
*NetMob 2023*, Madrid, Spain.  

## Citation

If you use this code, please cite:

[1] Lenormand M (2023) [Mapping mobile service usage diversity in cities](https://arxiv.org/abs/2311.06269). 
*NetMob 2023*, Madrid, Spain.  

If you need help, find a bug, want to give me advice or feedback, please contact me!

## Repository mirrors

This repository is mirrored on both GitLab and GitHub. You can access it via the following links:

- **GitLab**: [https://gitlab.com/maximelenormand/mobile-service-diversity](https://gitlab.com/maximelenormand/mobile-service-diversity)  
- **GitHub**: [https://github.com/maximelenormand/mobile-service-diversity](https://github.com/maximelenormand/mobile-service-diversity)  

The repository is archived in Software Heritage:

[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/maximelenormand/mobile-service-diversity/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/maximelenormand/mobile-service-diversity)

