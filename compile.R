
library(tidyverse)
  

  # 2. Read in data files ------------------------------------------
  
  specscans_key0 <- read_csv("specscans_key.csv")
  specscans_key <- specscans_key0

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
  
###############################################################
  
  scandata <- merge(specscans_key,df,by.x="fileprefix",by.y="scanlabel")
  scandata <- scandata %>% arrange(dateran,bottlecode,file,wvl)
  
  scandata
  
  scandata$index<-1:length(scandata[,1])
  
  scandata_trim<-scandata %>% select(index,bottlecode,wvl,abs)
  write.csv(scandata_trim,file="scans.csv",row.names=FALSE)
  
  
  
  