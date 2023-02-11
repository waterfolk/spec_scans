  #
  # useful reference for sentinel 2 bloom calcs https://www.mdpi.com/2071-1050/13/15/8570
  #https://www.frontiersin.org/articles/10.3389/fenvs.2021.612934/full

  timestart<-Sys.time()

  
  library(reticulate)
  library(rgee)
  
  #  ee_install()#py_env = "rgee") # It is just necessary once!
  
#  ee_Initialize(user = 'st3powers@gmail.com')
  
  ee_Initialize()
  #####################
  
  # load packages
  #library(mapedit) # (OPTIONAL) Interactive editing of vector data
  library(raster)  # Manipulate raster data
  library(scales)  # Scale functions for visualization
  library(cptcity)  # cptcity color gradients!
  #library(tmap)    # Thematic Map Visualization <3
  library(geojsonio)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(sf)
  library(reshape2)
  library(lubridate)
  library(googledrive)
  library(mapedit)
  library(leaflet)
  library(leaflet.extras)
  library(ggrepel)
  #library(leaflet.extras2)
  #library(devtools)
  
  library(rgeeExtra)
  #library(leafpm)
  #remotes::install_github("r-earthengine/rgeeExtra")
  #######################
  
  # 1. Load packages --------------------------------------------------------
  library(janitor)
  library(lubridate)
  #library(readxl)
  library(plyr)
  library(tidyverse)
  library(reshape2)
  library(data.table)
  #library(stringr)
  library(ggplot2)
  library(rgdal)
  library(sf)
  library(OpenStreetMap)
  library(leaflet)
  library(leaflet.extras)
  library(grid)
  library(gridExtra)
  library(maptools)
  library(maps)
  library(ggsn)
  library(ggspatial)
  library(factoextra)
  library(automap)
  library(sp)
  library(lattice)
  #library(FreqProf)
  #library(DecomposeR)
  library(rioja)
  library(zoo)
  #library(ecp)
  #library(changepoint.mv)
  #library(kcpRS)
  #library(depmixS4)
  #library('quantmod')
  #library('zoo')
  #library(ppclust)
  #library(factoextra)
  #library(cluster)
  #library(fclust)
  
  getUTMzone <- function(lon) {
    (floor((lon + 180)/6) %% 60) + 1
  }
  
  # set inputs
  
  outdir<-"arrowhead_2022Aug12"
  s_date_set<-"220812"
  
  #collectionchoice<-"COPERNICUS/S2"
  collectionchoice<-"COPERNICUS/S2_SR_HARMONIZED"
  #collectionchoice<-"MODIS/061/MOD09A1"
  #collectionchoice<-"LANDSAT/LT05/C02/T1_L2"
  #collectionchoice<-"LANDSAT/LT04/C02/T1_L2"
  
  startdate<-"2022-08-12"
  startdate_sat<-"2022-08-12"
  enddate_sat<-"2022-08-13"
  
  # set this higher to subsample data (5 means every 5th point in time) and run code faster
  # set this to 1 to run all data
#  keepevery<-20
#  keepevery<-5
#  keepevery<-2
#  keepevery<-1
#  keepevery<-2
  
  B1_max<-1000 #250 #260#250 #275# 300#250
  MSK_CLDPRB_max<-0 #1
  AOT_max<-137#137
  
    
  minval<-0 # replace sensor values below this to NA
  
  map0 <- openmap(upperLeft = c(33.775, -98.3), #(31.91, -90.106)
                  lowerRight = c(33.63, -98.49), # (31.7977, -99.152)
                  type = 'stamen-terrain',zoom=12)
  
  sitenames <- tribble(
    ~site, ~sitename, ~siteabbr,
    "AH1", "arm1", "A1",
    "AH2", "upper", "U",
    "AH3", "lower", "L",
    "AH4", "middle", "M",
    "AH5", "arm2", "A2"
  )
  

  # 2. Read in data files ------------------------------------------
  
  datafiles<-list.files(path="./data")
  datafiles<-datafiles[grep(".asc",datafiles,ignore.case=TRUE)]
  datapaths<-paste("./data/",datafiles,sep="")
  datalist<-lapply(datapaths,FUN="read.csv",header=FALSE,skip=6)
  nrows<-length(datalist[[1]][,1])
  
  df<-as.data.frame(do.call(rbind, datalist))
  df$wvl<-substr(df[,1],3,10) %>% as.numeric()
  df$abs<-substr(df[,1],16,22) %>% as.numeric()
  
  df<-df %>% dplyr::select(wvl,abs)
  df$file<-unlist(lapply(datafiles,FUN="rep",nrows))
  df$label<-gsub(".ASC","",df$file)
  df$labelshort<-substr(df$label,nchar(df$label)-1,nchar(df$label))
  
  
  dataSabine0<-df[grep("SAB",df$label),]
  dataSabine<-dataSabine0 %>% dplyr::filter(!labelshort %in% c("18","19"))
  
  dataSabine_DI<-dataSabine0 %>% dplyr::filter(labelshort=="18")
  dataSabine_labdup<-dataSabine0 %>% dplyr::filter(labelshort=="19")
  
  dataplot<-dataSabine
  
  plotit<-dataplot %>%
    ggplot(aes(x=wvl,y=abs,color=labelshort)) +
    geom_point()+
    theme_bw()
  plotit
  
  plotit<-dataplot %>%
    ggplot(aes(x=wvl,y=abs,color=labelshort)) +
    geom_point()+
    facet_wrap(~labelshort)+
    theme_bw()
  plotit
  
###################################################################  
  
  
  