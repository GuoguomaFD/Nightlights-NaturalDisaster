
## Using the inter-calibrated light data

rm(list = ls())

library(maptools)
library(raster)
library(sp)

dataproj<-read.csv("proj Geo Philippines with disbursement Roads and Highway.csv")

## different from the regular stable light data
## the inter-calibrated stable light is much larger in size
## because the previously NA data is now available
## Therefore, instead merge the 1992 to 2013 in one single CSV (3G +), we call in tif for each year and Satellite to do the matching

dataproj$longitude<-round(dataproj$longitude,digits=4)
dataproj$latitude<-round(dataproj$latitude,digits=5)

NameVec<-c("F101992", "F101993", "F101994", "F121994", "F121995", "F121996", "F121997", "F121998", "F121999", 
           "F141997", "F141998", "F141999", "F142000", "F142001", "F142002", "F142003", 
           "F152000", "F152001", "F152002", "F152003", "F152004", "F152005", "F152006", "F152007", 
           "F162004", "F162005", "F162006", "F162007", "F162008", "F162009", "F182010", "F182011", "F182012", "F182013")


for (counter in 1:34)
{
  temp<-paste("PHL ",NameVec[counter],".intercal.stable_lights.avg_vis.tif",sep="")
  
  data<-raster(temp)
  
 
  data1<-as.data.frame(data,xy=TRUE)
  data2<-data1[!is.na(data1[,3]),]
  data2$Year<-as.numeric(substr(names(data2)[3],8,11))
  data2$SateliteNo<-as.numeric(substr(names(data2)[3],6,7))
  names(data2)[3]<-"Intercal Stable Lights Mean"
  names(data2)[1]<-"Long"
  names(data2)[2]<-"Lat"
  
lightdata<-data2

lightdata$Long<-round(lightdata$Long,digits=4)
lightdata$Lat<-round(lightdata$Lat,digits=5)

lightdata$Count<-0
lightdata$Disburs<-0
lightdata$Cost<-0
lightdata$SectorMa<-NA

targetData<-lightdata[1,]
targetData$Count<-0
targetData[1,1:2]<-0

## available lat and long in the data
LongVec<-as.numeric(row.names(table(lightdata[,1])))
LatVec<-as.numeric(row.names(table(lightdata[,2])))

for (i in 1:length(dataproj[,1]))
{
  
  lightcell<-lightdata[lightdata$Long%in%dataproj$longitude[i] &
                         lightdata$Lat%in%dataproj$latitude[i],]
  
  ## if no exact location available, then the surrounding available 2*2 cells
  if (length(lightcell[,1])==0)
  {
  ## the closest 4 cells
  aa<-LongVec-dataproj$longitude[i]
  aaright<-min(aa[aa>0])
  aaleft<-max(aa[aa<0])
  bb<-LatVec-dataproj$latitude[i]
  bbright<-min(bb[bb>0])
  bbleft<-max(bb[bb<0])
  lightcell<-lightdata[lightdata$Long<=dataproj$longitude[i]+aaright & lightdata$Long>=dataproj$longitude[i]+aaleft &
                        lightdata$Lat<=dataproj$latitude[i]+bbright & lightdata$Lat>=dataproj$latitude[i]+bbleft,]
 
  ## if four grids are reported, select the one with the smallest distance
  ## and assign the long and lat as the same as dataproj[i] to generate the consistent panel
  
  dist<-sqrt((lightcell$Long-dataproj$longitude[i])^2+(lightcell$Lat-dataproj$latitude[i])^2)
  mindis<-min(dist)
  lightcell$dist<-dist
  lightcell<-lightcell[lightcell$dist==mindis,]
  lightcell$Long<-dataproj$longitude[i]
  lightcell$Lat<-dataproj$latitude[i]
  ## remove the newly added distance column
  lightcell<-lightcell[,-10]
   }
  
  
  lightcell$Count[lightcell$Year>=dataproj$comYear[i]]<-1
  ## since both sides are levels - light and expenditures, we keep the amount 
  lightcell$Disburs[lightcell$Year>=dataproj$StartYear[i]&lightcell$Year<=dataproj$EndYear[i]]<-dataproj$AnnualDisbur[i]
  lightcell$Cost[lightcell$Year>=dataproj$StartYear[i]&lightcell$Year<=dataproj$EndYear[i]]<-dataproj$AnnualprojCost[i]
  lightcell$SectorMa[lightcell$Year>=dataproj$StartYear[i]&lightcell$Year<=dataproj$EndYear[i]]<-as.character(dataproj$SectorMa[i])
    
  ## There might be double counting of cells
  ## will do the cleaning when assigning unique cell numbers in later program
  
  targetData<-rbind(targetData,lightcell) 
     
remove(lightcell)
}

targetData<-targetData[-1,]

if (counter==1) {finaldata<-targetData} else {finaldata<-rbind(finaldata,targetData)}

remove(data)
remove(data1)
remove(data2)
remove(lightdata)
print(counter)
}

finaldata<-as.data.frame(finaldata)

#write.csv(finaldata, "Cubic cells with intercalibrated light and projects disbursement Roads and Highways.csv", row.names=FALSE)

