rm(list=ls())

library(tidyverse)
  
  # 2. Read in data files ------------------------------------------
  
specscans_key0 <- read_csv("specscans_key.csv")
specscans_key <- specscans_key0

# first compile Beckman files

datafiles<-list.files(path="./data")
datafiles<-datafiles[grep(".asc",datafiles,ignore.case=TRUE)]
datapaths<-paste("./data/",datafiles,sep="")

# Create a list of data frames with file names added
datalist <- lapply(datapaths, function(file) {
  df <- read.csv(file, header = FALSE, skip = 6, stringsAsFactors = FALSE)
  df[, 1] <- as.character(df[, 1])
  
  # Extract wvl and abs
  df <- df %>%
    mutate(
      wvl = as.numeric(substr(V1, 3, 10)),
      abs = as.numeric(substr(V1, 16, 22)),
      file = basename(file)  # Extract just the filename without the path
    ) %>%
    select(wvl, abs, file)
  
  return(df)
})

# Combine all data frames
df <- bind_rows(datalist)

df$scanlabel<-gsub(".ASC","",df$file)
df$scanlabelshort<-substr(df$scanlabel,nchar(df$scanlabel)-1,nchar(df$scanlabel))

scandata <- merge(specscans_key,df,by.x="fileprefix",by.y="scanlabel")
scandata <- scandata %>% arrange(dateran,bottlecode,file,wvl)

scandata$index<-1:length(scandata[,1])
scandata_trim<-scandata %>% select(index,bottlecode,wvl,abs)


######################################################

# now do agilent files
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

agilent_combined<-bind_rows(lapply(datapaths,read_wide))
agilent_combined$wvl<-round(as.numeric(agilent_combined$wvl),0)
agilent_combined$abs<-signif(as.numeric(agilent_combined$abs),4)

agilent_combined <- data.frame(bottlecode=agilent_combined$item,wvl=agilent_combined$wvl,abs=agilent_combined$abs)
agilent_combined$index<-(1+max(scandata_trim$index)):(max(scandata_trim$index)+length(agilent_combined[,1]))
agilent_combined_trim<-agilent_combined %>% select(index,bottlecode,wvl,abs)
#####################################
# combine Backman and Aglient data

scans_combined<-rbind(scandata_trim,agilent_combined_trim)
write.csv(scans_combined,file="scans.csv",row.names=FALSE)

