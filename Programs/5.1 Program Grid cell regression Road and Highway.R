
rm(list = ls())

data<-read.csv("grid cell data with light project and disaster Road and Highway.csv")

## label cell names

data$cellnum<-NA
data$cellnum[1]<-1

NameCounter<-2

Rec<-0
## i is 6293
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

## obs 17304/ year 21 = 824
##  748 unique cell grids

##write.csv(data,"Grid cell data with cell names coded Road and Highway.csv", row.names=FALSE)

###########################################################################
## remove the replications of the same cell for the same year

rm(list = ls())

data<-read.csv("Grid cell data with cell names coded Road and Highway.csv")

N=max(data$cellnum)

temp<-data[data$cellnum==1,]

## for the temp, there are two different projects happen for the same cell 
## we add the effect into one, but leave the sector as first occurrence 


finaldata<-temp[1:21,]

remove(temp)

for (i in 2:N)
{
  temp<-data[data$cellnum==i,]
  temp1<-temp[1:21,]
  temp1[,5]<-tapply(temp[,5],INDEX = temp$year, FUN = sum)  
  temp1[,6]<-tapply(temp[,6],INDEX = temp$year, FUN = sum)  
  temp1[,7]<-tapply(temp[,7],INDEX = temp$year, FUN = sum)  
  
  finaldata<-as.data.frame(rbind(finaldata, temp1))
  print(i)
  remove(temp,temp1)
}

##write.csv(finaldata,"Grid cell data with cell names coded Road and Highway Cleaned.csv", row.names=FALSE)

############################################################

rm(list = ls())

data<-read.csv("Grid cell data with cell names coded Road and Highway Cleaned.csv")


## revise the zero light to 1 to avoid inf value for changes
data$Stable.Lights.Mean[data$Stable.Lights.Mean==0]<-1

## alternatively remove zeros and 63
## data<-data[data$Stable.Lights.Mean!=0 & data$Stable.Lights.Mean!=63,]
data<-data[data$Stable.Lights.Mean!=63,]

## for project dummy count first 
## dummy count counted after completion of the years

dataCount<-data[,c(1:5,9:10)]

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

##
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 -3.322e-01  7.692e-02  -4.319 1.57e-05 ***
#  Right1tempD                  1.110e-01  1.343e-02   8.269  < 2e-16 ***
#  Right1RegCount[, c(6)]      -1.958e-02  4.175e-02  -0.469 0.639023    
#Right1tempD * RegCount[, 6] -1.131e-01  6.687e-02  -1.691 0.090784 .  
## Residual standard error: 0.3474 on 14600 degrees of freedom
## Multiple R-squared:  0.9267,	Adjusted R-squared:  0.9229 

###############################################
## using the cost and disbursement of the project

dataCost<-data
dataCost$CostCom<-0
N<-max(dataCost$cellnum)
temp<-dataCost[dataCost$cellnum==1,]
temp$CostCom[temp$Count>0]<-sum(temp$Cost)
totaldata<-temp
remove(temp)

for (i in 2:N)
{
 
  temp<-dataCost[dataCost$cellnum==i,]
  temp$CostCom[temp$Count>0]<-sum(temp$Cost)
  totaldata<-rbind(totaldata,temp)
  remove(temp)
}

dataCost<-totaldata[,c(1:4,9:11)]


library(fastDummies)

RegCost<-dummy_cols(dataCost, select_columns = c("cellnum", "year"), remove_first_dummy = TRUE)

Left1<-as.matrix(log(RegCost[,3]))
## change the amount to ln
RegCost$CostCom[RegCost$CostCom!=0]<-log(RegCost$CostCom[RegCost$CostCom!=0])
CostIntDisaster<-as.data.frame(RegCost[,5]*RegCost[,7])
Right1<-as.matrix(cbind(RegCost[,c(7,5)], CostIntDisaster,RegCost[,c(8:length(RegCost[1,]))]))
aa<-summary(lm(Left1~Right1))
aa

## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                       -3.343e-01  7.693e-02  -4.345 1.40e-05 ***
##  Right1CostCom                      7.296e-03  8.862e-04   8.232  < 2e-16 ***
##  Right1disaster                    -2.071e-02  4.167e-02  -0.497 0.619177    
## Right1RegCost[, 5] * RegCost[, 7] -7.114e-03  4.279e-03  -1.663 0.096418 .  

## Residual standard error: 0.3474 on 14600 degrees of freedom
## Multiple R-squared:  0.9267,	Adjusted R-squared:  0.9229 
