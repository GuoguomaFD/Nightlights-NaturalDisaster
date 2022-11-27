

rm(list = ls())

dataproj<-read.csv("proj Geo Philippines with disbursement Roads and Highway.csv")

lightdata<-read.csv("NL PHL OLS Year 1992 to 2013 Stable Lights Mean All grid cells.csv")

dataproj$longitude<-round(dataproj$longitude,digits=4)
dataproj$latitude<-round(dataproj$latitude,digits=5)

lightdata$Long<-round(lightdata$Long,digits=4)
lightdata$Lat<-round(lightdata$Lat,digits=5)

lightdata$Count<-0

targetData<-lightdata[1,]
targetData$Count<-0
targetData[1,1:2]<-0


for (i in 1:length(dataproj[,1]))
{
  ## for the adjacent 9 cells 
  lightcell<-lightdata[lightdata$Long<dataproj$longitude[i]+0.009 & lightdata$Long>dataproj$longitude[i]-0.009 &
                    lightdata$Lat<dataproj$latitude[i]+0.009 & lightdata$Lat>dataproj$latitude[i]-0.009,]
  
  ## any year after completing counted as 1
  lightcell$Count[lightcell$year>=dataproj$comYear[i]]<-1
  
  ## there might be double countring of cells and will do cleaning in program 4
  targetData<-rbind(targetData,lightcell) 

print(i)
}

targetData<-targetData[-1,]
 

#write.csv(targetData, "Cubic cells 3km times 3km with light and projects.csv", row.names=FALSE)

