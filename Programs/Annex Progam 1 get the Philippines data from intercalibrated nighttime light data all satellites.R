
## The first step is to download tif images of all statellites from the EOG website as below:
## https://eogdata.mines.edu/products/dmsp/
## Save them in the local folder entilted "Global intercal stable light avg vis TIFF"
## Read the nightlights from the downloaded raster tif 
## Useful GIF command can be found: https://www.gis-blog.com/r-raster-data-acquisition/


library(maptools)
library(raster)
library(sp)

rm(list=ls())


PH0 <- getData('GADM', country='PHL', level=3)

## list the names of all satellites

NameVec<-c("F101992", "F101993", "F101994", "F121994", "F121995", "F121996", "F121997", "F121998", "F121999", 
            "F141997", "F141998", "F141999", "F142000", "F142001", "F142002", "F142003", 
            "F152000", "F152001", "F152002", "F152003", "F152004", "F152005", "F152006", "F152007", 
            "F162004", "F162005", "F162006", "F162007", "F162008", "F162009", "F182010", "F182011", "F182012", "F182013")

## Define the latitude and longitude scope for the Philippines
## PH0
#extent      : 116.9283, 126.6053, 4.58694, 21.07014  (xmin, xmax, ymin, ymax)


aa<-c(116.93, 126.6053, 4.58694, 21.07014) 

for (i in 1:30)
{
  temp<-paste("/Global intercal stable light avg vis TIFF/",NameVec[i],".v4b.intercal.stable_lights.avg_vis.tif",sep="")
  
  data<-raster(temp)
  ## such cutting would yield a raster of 1162 column for F162009, F182010, and F182011
  ## which is inconsistent with the deblur raster
  ## therefore, we munually adjust the min x to get 1161 column
  ## 
  data1<-crop(data,PH0)
  
  if (i==30) 
  {data1<-crop(data,aa)
    print(ncol(data1))}
  
  temp1<-paste("PHL ",NameVec[i],".intercal.stable_lights.avg_vis.tif",sep="")
  writeRaster(data1,temp1)
}

for (i in 31:34)
{
  temp<-paste("/Global intercal stable light avg vis TIFF/",NameVec[i],".v4c.intercal.stable_lights.avg_vis.tif",sep="")
  
  data<-raster(temp)
  
  data1<-crop(data,PH0)
  if (i%in%c(31,32)) 
  {data1<-crop(data,aa)
  print(ncol(data1))}
  
  temp1<-paste("PHL ",NameVec[i],".intercal.stable_lights.avg_vis.tif",sep="")
  writeRaster(data1,temp1)
 
}

finaldata<-as.data.frame(finaldata)

#write.csv(finaldata, "PHL Intercal stable lights ave vis 1992 to 2013.csv", row.names = FALSE)


