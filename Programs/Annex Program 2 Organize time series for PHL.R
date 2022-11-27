
library(maptools)
library(raster)
library(sp)

rm(list=ls())


NameVec<-c("F101992", "F101993", "F101994", "F121994", "F121995", "F121996", "F121997", "F121998", "F121999", 
           "F141997", "F141998", "F141999", "F142000", "F142001", "F142002", "F142003", 
           "F152000", "F152001", "F152002", "F152003", "F152004", "F152005", "F152006", "F152007", 
           "F162004", "F162005", "F162006", "F162007", "F162008", "F162009", "F182010", "F182011", "F182012", "F182013")

for (i in 1:34)
{
  temp<-paste("PHL ",NameVec[i],".intercal.stable_lights.avg_vis.tif",sep="")
  
  data<-raster(temp)
  
  #plot(data1)
  data1<-as.data.frame(data,xy=TRUE)
  data2<-data1[!is.na(data1[,3]),]
  data2$Year<-as.numeric(substr(names(data2)[3],8,11))
  data2$SateliteNo<-as.numeric(substr(names(data2)[3],6,7))
  names(data2)[3]<-"Intercal Stable Lights Mean"
  
  if (i==1) {finaldata<-data2} else {finaldata<-rbind(finaldata,data2)}
}

finaldata<-as.data.frame(finaldata)

##write.csv(finaldata, "PHL Intercal stable lights ave vis 1992 to 2013.csv", row.names = FALSE)