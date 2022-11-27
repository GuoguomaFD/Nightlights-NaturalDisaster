## The natural diasters data is downloaded from AidData web as:
## https://www.aiddata.org/datasets
## Entitled "EM-DAT Philippines Geocoded Dataset,1980-2012,Version 1.0"

rm(list = ls())

data<-read.csv("Cubic cells 3km times 3km with light and projects.csv")

DAT<-read.csv("disasters_locations_merged.csv")

DAT<-DAT[DAT$year>=1992,]

DAT$latitude<-round(DAT$latitude, digits=5)
DAT$longitude<-round(DAT$longitude, digits=4)

data$Lat<-round(data$Lat,digits=5)
data$Long<-round(data$Long, digits=4)

table(DAT$disaster_type)

#Drought Earthquake (seismic activity)                      Epidemic                         Flood            Insect infestation             Mass movement dry 
#11                            16                            41                           423                             1                             3 
#Mass movement wet                         Storm                       Volcano                      Wildfire 
#42                           956                            30                             2 

DATflood<-DAT[DAT$disaster_type%in%"Flood",]

data$disaster<-0

for (i in 1:length(DATflood[,1]))
{
#  select<-data$Long%in%DATflood$longitude[i] & data$Lat%in%DATflood$latitude[i] & data$year%in%DATflood$year[i]
  
  select<-(data$Long<DATflood$longitude[i]+0.009 & data$Long>DATflood$longitude[i]-0.009 &
    data$Lat<DATflood$latitude[i]+0.009 & data$Lat>DATflood$latitude[i]-0.009) & data$year%in%DATflood$year[i]
  if (length(data[select,1])!=0)
  {data$disaster[select]<-1
  print(i)}
 ## alternatively if the natural disaster hit cells no project, add the cells in
  
}

## remove the year 2013 because no disaster data

data<-data[data$year<=2012,]

#write.csv(data, "grid cell data 3km times 3km with light project and disaster Road and Highway.csv", row.names=FALSE)



