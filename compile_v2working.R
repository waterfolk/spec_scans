
library(tidyverse)
  

  # 2. Read in data files ------------------------------------------
  
  specscans_key0 <- read_csv("specscans_key.csv")
  specscans_key <- specscans_key0

  datafiles<-list.files(path="./data")
  datafiles<-datafiles[grep(".asc",datafiles,ignore.case=TRUE)]
  datapaths<-paste("./data/",datafiles,sep="")
  datalist<-lapply(datapaths,FUN="read.csv",header=FALSE,skip=6)
  nrows<-length(datalist[[1]][,1])
  
  
  # new code chunk to handle newer spec data
  datafiles<-list.files(path="./data")
  datafiles<-datafiles[grep("agilent.*\\.csv",datafiles,ignore.case=TRUE)]
  datapaths<-paste("./data/",datafiles,sep="")
  

  read_wide<-function(data){
    lines<-readLines(datapaths[1])
    first_empty_row <- which(trimws(lines) == "")[1] -1
    items<-names(read_csv(data,n_max=1))
    items<-items[seq(1,length(items)-1,by=2)]
    nitems<-length(items)
    print(items)
    for (i in 1:nitems){
      print(i)
      col1<-(2*i)-1
      col2<-2*1
      datai<-read_csv(data,col_select=c(col1,col2), skip=1,n_max =first_empty_row)
      datai<-data.frame(datai)
      names(datai)<-c("wvl","abs")
      itemi<-items[i]
      datai$item<-itemi
      datai<-data.frame(item=datai$item,wvl=datai$wvl,abs=datai$abs)
      #      print(datai)
      if(i==1){data_bound<-datai}
      if(i>1){data_bound<-rbind(data_bound,datai)}
    }
    return(na.omit(data_bound))
  }
  
  test<-read_wide(data=datapaths[1])


  
  read_wide(data=datapaths[1],nitems=6)
  
  df <- read_csv(datapaths[1], skip=1, col_select=c(1,2))        # Read the 4th column
  
  # Read the CSV file, skipping the first row
  data_tmp<-read_csv(datapaths[1],skip=1)
  
  # Extract column names from the second row
  names(data_tmp) <- names(data_tmp[1, ])  # Assign second row as column names
  data_tmp <- data_tmp[-1, ]  # Remove the second row from the data
  
  # Pivot the table to long format
  df_long <- data_tmp %>%
    pivot_longer(
      cols = c("Wavelength","Abs"),  # Keep 'item' as is, pivot the rest
      names_to = c(".value", "group"),
      names_pattern = "(Wavelength|Abs)(.*)"
    ) %>%
    rename(Wavelength = Wavelength, Abs = Abs)
  
  
  cols<-names(data_tmp)[grep("-",names(data_tmp))]
  data_tmp %>% pivot_longer(names_to=cols)
  
  data_melted<-melt(data_tmp)
  
  
  datalist<-lapply(datapaths,FUN="read.csv",header=FALSE,skip=6)
  nrows<-length(datalist[[1]][,1])
  
  # end new code chunck
  
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
  
  
  
  