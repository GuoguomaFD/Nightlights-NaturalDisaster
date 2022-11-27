## Read the stable nightlights data from the downloaded raster tif 
## The data is zipped in Stable Lights Annual Philippines 1992 to 2013.zip

## An alternative way to do is to download the data from EOG website at https://eogdata.mines.edu/products/dmsp/#v4_dmsp_download
## And then get the Philippines using the corresponding GADM 
## Corresponding codes refer to Annex Program 1*.R

library(maptools)
library(raster)
library(sp)

rm(list=ls())

data<-raster('~/NL_PHL_OLS.Y_1992_STABLE_LIGHTS-MTS1-MEAN-RGFT_GADM-3.6-SHPZIP.tif')

data1<-as.data.frame(data,xy=TRUE)
data2<-data1[!is.na(data1[,3]),]
names(data2)[3]<-"Stable Lights Mean"
data2$year<-1992

totaldata<-data2

data<-raster('~/NL_PHL_OLS.Y_1993_STABLE_LIGHTS-MTS1-MEAN-RGFT_GADM-3.6-SHPZIP.tif')

data1<-as.data.frame(data,xy=TRUE)
data2<-data1[!is.na(data1[,3]),]
names(data2)[3]<-"Stable Lights Mean"
data2$year<-1993

totaldata<-rbind(totaldata, data2)


for (t in 1994:2007)
{
data<-raster(paste("~/NL_PHL_OLS.Y_",t,"_STABLE_LIGHTS-MTSLAST-MEAN-RGFT_GADM-3.6-SHPZIP.tif",sep=""))
data1<-as.data.frame(data,xy=TRUE)
data2<-data1[!is.na(data1[,3]),]
names(data2)[3]<-"Stable Lights Mean"
data2$year<-t
totaldata<-rbind(totaldata, data2)
}


for (t in 2008:2013)
{
data<-raster(paste("~/NL_PHL_OLS.Y_",t,"_STABLE_LIGHTS-MTS1-MEAN-RGFT_GADM-3.6-SHPZIP.tif",sep=""))
data1<-as.data.frame(data,xy=TRUE)
data2<-data1[!is.na(data1[,3]),]
names(data2)[3]<-"Stable Lights Mean"
data2$year<-t
totaldata<-rbind(totaldata, data2)
}

names(totaldata)[1:2]=c("Long", "Lat")

write.csv(totaldata, "NL PHL OLS Year 1992 to 2013 Stable Lights Mean All grid cells", row.names=FALSE)

