
rm(list = ls())

data<-read.csv("grid cell data 3km times 3km with light project and disaster Road and Highway.csv")

## label cell names

data$cellnum<-NA
data$cellnum[1]<-1

NameCounter<-2

Rec<-0

for (i in 2:length(data[,1]))
{
  
    select<-data$Long[1:(i-1)]%in%data$Long[i]&data$Lat[1:(i-1)]%in%data$Lat[i]
    if (any(select))
    ## there might be more than one repeating cell number
    ## such as i=6293, therefore only get the first one
    {data$cellnum[i]<-data$cellnum[1:(i-1)][select][1]} else
    {data$cellnum[i]<-NameCounter
    NameCounter<-NameCounter+1}
    print(i)
}

## 2660 cells

##write.csv(data,"Grid cell data with cell names coded Road and Highway.csv", row.names=FALSE)

###########################################################################
## remove the replications of the same cell for the same year


rm(list = ls())

data<-read.csv("Grid cell data with cell names coded Road and Highway.csv")

N=max(data$cellnum)

temp<-data[data$cellnum==1,]

## for the temp, there are two different projects happen for the same cell 
## we add the effect into one, but leave the sector as first occurance 


finaldata<-temp[1:21,]

remove(temp)

for (i in 2:N)
{
  temp<-data[data$cellnum==i,]
  temp1<-temp[1:21,]
  temp1[,5]<-tapply(temp[,5],INDEX = temp$year, FUN = sum)  
  
  finaldata<-as.data.frame(rbind(finaldata, temp1))
  print(i)
  remove(temp,temp1)
}

#write.csv(finaldata,"Grid cell data 3km times 3km with cell names coded Road and Highway Cleaned.csv", row.names=FALSE)

############################################################

rm(list = ls())

data<-read.csv("Grid cell data 3km times 3km with cell names coded Road and Highway Cleaned.csv")


## revise the zero light to 1 to avoid inf value for changes
data$Stable.Lights.Mean[data$Stable.Lights.Mean==0]<-1

## alternatively remove zeros and 63
## data<-data[data$Stable.Lights.Mean!=0 & data$Stable.Lights.Mean!=63,]
data<-data[data$Stable.Lights.Mean!=63,]

## for project dummy count first 
## dummy count counted after completion of the years

dataCount<-data

library(fastDummies)

RegCount<-dummy_cols(dataCount, select_columns = c("cellnum", "year"), remove_first_dummy = TRUE)

Left1<-as.matrix(log(RegCount[,3]))
tempD<-RegCount$Count
# If put the original count in the interacting dummy
## the coef is -0.07 but insignificant
tempD[tempD>0]<-1
CountIntDisaster<-as.data.frame(tempD*RegCount[,6])
Right1<-as.matrix(cbind(tempD,RegCount[,c(6)], CountIntDisaster,RegCount[,c(8:length(RegCount[1,]))]))
#Right1<-as.matrix(cbind(RegCount[,c(5:6)], CountIntDisaster,RegCount[,c(8:length(RegCount[1,]))]))
aa<-summary(lm(Left1~Right1))
aa

## there are 2659 cells
## number of varialbes: 2678
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -2.840e-01  7.313e-02  -3.883 0.000103 ***
#  Right1tempD                  1.106e-01  8.228e-03  13.444  < 2e-16 ***
#  Right1RegCount[, c(6)]      -3.222e-02  1.262e-02  -2.554 0.010655 *  
#  Right1tempD * RegCount[, 6] -7.185e-02  2.142e-02  -3.355 0.000795 ***

# Residual standard error: 0.3337 on 52737 degrees of freedom
# Multiple R-squared:  0.8961,	Adjusted R-squared:  0.8909 
# F-statistic: 169.9 on 2678 and 52737 DF,  p-value: < 2.2e-16
