 
rm(list = ls())

dataproj<-read.csv("proj Geo Philippines with disbursement Roads and Highway.csv")

lightdata<-read.csv("NL PHL OLS Year 1992 to 2013 Stable Lights Mean All grid cells.csv")

dataproj$longitude<-round(dataproj$longitude,digits=4)
dataproj$latitude<-round(dataproj$latitude,digits=5)

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

OverlapVec<-0

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
  }
  
  lightcell$Count[lightcell$year>=dataproj$comYear[i]]<-1
  ## since both sides are levels - light and expenditures, we keep the amount 
  lightcell$Disburs[lightcell$year>=dataproj$StartYear[i]&lightcell$year<=dataproj$EndYear[i]]<-dataproj$AnnualDisbur[i]
  lightcell$Cost[lightcell$year>=dataproj$StartYear[i]&lightcell$year<=dataproj$EndYear[i]]<-dataproj$AnnualprojCost[i]
  lightcell$SectorMa[lightcell$year>=dataproj$StartYear[i]&lightcell$year<=dataproj$EndYear[i]]<-as.character(dataproj$SectorMa[i])
    
  ## There might be double counting of cells
  ## will do the cleaning when assigning unique cell numbers in later program
  
  targetData<-rbind(targetData,lightcell) 
     
print(i)
remove(lightcell)
}

targetData<-targetData[-1,]


##write.csv(targetData, "Cubic cells with light and projects disbursement Roads and Highways.csv", row.names=FALSE)

