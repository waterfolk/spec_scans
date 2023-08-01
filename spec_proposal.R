  #
  # useful reference for sentinel 2 bloom calcs https://www.mdpi.com/2071-1050/13/15/8570
  #https://www.frontiersin.org/articles/10.3389/fenvs.2021.612934/full

  timestart<-Sys.time()


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
  df$scanlabel<-gsub(".ASC","",df$file)
  df$scanlabelshort<-substr(df$scanlabel,nchar(df$scanlabel)-1,nchar(df$scanlabel))
  
  dataplot<-df %>% filter(scanlabel %in% c("LWNOV001","LWNOV002",
                                           "LWNOV003","LWNOV004",
                                           "LWNOV005","LWNOV006"))
  latlon<-tribble(
    ~site,~sitename,~lat,~lon,
    "LWNOV001","Dam",31.580661,-97.203177,
    "LWNOV002","North arm middle",31.6056,	-97.27216,
    "LWNOV003","North arm lower",31.59498,	-97.25371,
    "LWNOV004","North arm upper",31.60024,	-97.29351,
    "LWNOV005","South arm lower",31.54366,	-97.22294,
    "LWNOV006","South arm upper",31.52896,	-97.23724
  )
  dataplot<-merge(dataplot,latlon,by.x="scanlabel",by.y="site")
#  dataplot$sitename[which(dataplot$sitename=="Dam")]<"Main body"

  dataplot<-dataplot %>% filter(sitename %in% c("Dam","South arm upper","North arm middle"))
  dataplot<-dataplot %>% mutate(site=sitename)
  dataplot$site[which(dataplot$site=="North arm middle")]<-"North arm"
  dataplot$site[which(dataplot$site=="South arm upper")]<-"South arm"
  dataplot$site<-factor(dataplot$site,c("North arm","South arm","Dam"))

  
#  dataplot$site<-as.factor(c("North arm","South arm","Dam"))
#  dataplot$site<-factor(dataplot$site,relevel(dataplot$site,"North arm"))
  
  
  plotit<-dataplot %>% filter(wvl>=220 & wvl<=750) %>%
#    ggplot(aes(x=wvl,y=log10(abs),color=site)) +
   ggplot(aes(x=wvl,y=(abs/0.01),color=site)) +
    scale_color_viridis_d()+
    geom_point()+
#    xlim(220,750)+
    xlab("Wavelength (nm)")+
    ylab(expression(paste("Absorption coefficient (", m^-1,")")))+
#    ylab("log10 absorbance")+
#    facet_wrap(~sitename)+
    theme_bw()
  plotit
  
  png(filename="LW.png", width=4,height=2.5,units="in",res=300)
  plotit
  dev.off()
  
###################################################################  
  
  
  