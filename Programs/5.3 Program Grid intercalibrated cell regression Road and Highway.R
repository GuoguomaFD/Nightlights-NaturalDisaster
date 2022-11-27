
rm(list = ls())

data<-read.csv("grid cell data with intercalibrated light project and disaster Road and Highway.csv")

## label cell names

data$cellnum<-NA
data$cellnum[1]<-1

NameCounter<-2

Rec<-0
## stable light i is 6293
## intercalibrated light i is 14112

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

## name counter 371
##  748 unique cell grids

#write.csv(data,"Grid cell intercalibrated data with cell names coded Road and Highway.csv", row.names=FALSE)


###########################################################################
## remove the replications of the same cell for the same year

rm(list = ls())

data<-read.csv("Grid cell intercalibrated data with cell names coded Road and Highway.csv")

N=max(data$cellnum)

## for the temp, there are two different projects happen for the same cell 
## we add the effect into one, but leave the sector as first occurrence 

for (i in 1:N)
{
  temp<-data[data$cellnum%in%i,]
  temp1<-temp
  ## to count number of satelites for each year in record
  SateAdj<-colSums(table(temp[,5],INDEX = temp$Year))  
  
  ## accumulate count, disbursement, and cost
  ## also adjust double counting by different satelite 
  
  tempcount<-tapply(temp[,6],INDEX = temp$Year, FUN = sum)  
  tempcount<-tempcount/SateAdj
  for (t in 1:length(temp1$Year)) {temp1[t,6]<-tempcount[names(tempcount)%in%temp1$Year[t]]}
  
  tempDisburse<-tapply(temp[,7],INDEX = temp$Year, FUN = sum)  
  tempDisburse<-tempDisburse/SateAdj
  for (t in 1:length(temp1$Year)) {temp1[t,7]<-tempDisburse[names(tempDisburse)%in%temp1$Year[t]]}
  
  tempCost<-tapply(temp[,8],INDEX = temp$Year, FUN = sum)  
  tempCost<-tempCost/SateAdj
  for (t in 1:length(temp1$Year)) {temp1[t,8]<-tempCost[names(tempCost)%in%temp1$Year[t]]}
  
  if (i==1) {finaldata<-as.data.frame(temp1)} else {finaldata<-as.data.frame(rbind(finaldata, temp1))}
  print(i)
  remove(temp,temp1)
}

#write.csv(finaldata,"Grid cell intercalibrated data with cell names coded Road and Highway Cleaned.csv", row.names=FALSE)

############################################################

rm(list = ls())

data<-read.csv("Grid cell intercalibrated data with cell names coded Road and Highway Cleaned.csv")


## revise the zero light to 1 to avoid inf value for changes
data$Intercal.Stable.Lights.Mean[data$Intercal.Stable.Lights.Mean%in%0]<-1

## alternatively remove zeros and 63
## data<-data[data$Stable.Lights.Mean!=0 & data$Stable.Lights.Mean!=63,]
data<-data[data$Intercal.Stable.Lights.Mean!=63,]

## for project dummy count first 
## dummy count counted after completion of the years

dataCount<-data[,c(1:6,10:11)]

library(fastDummies)

RegCount<-dummy_cols(dataCount, select_columns = c("SateliteNo","Year","cellnum"), remove_first_dummy = TRUE)

Left1<-as.matrix(log(RegCount[,3]))
tempD<-RegCount$Count
# If put the original count in the interacting dummy
## the coef is -0.07 but insignificant
tempD[tempD>0]<-1
CountIntDisaster<-as.data.frame(tempD*RegCount[,7])
Right1<-as.matrix(cbind(tempD,RegCount[,c(7)], CountIntDisaster,RegCount[,c(9:length(RegCount[1,]))]))
#Right1<-as.matrix(cbind(RegCount[,c(5:6)], CountIntDisaster,RegCount[,c(8:length(RegCount[1,]))]))
aa<-summary(lm(Left1~Right1))
aa

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 -0.3058747  0.0640248  -4.777 1.79e-06 ***
#  Right1tempD                  0.0821912  0.0148543   5.533 3.20e-08 ***
#  Right1RegCount[, c(7)]      -0.0095534  0.0254161  -0.376 0.707012    
# Right1tempD * RegCount[, 7] -0.1281847  0.0404530  -3.169 0.001534 ** 
#  Right1SateliteNo_12         -0.1429532  0.0247232  -5.782 7.54e-09 ***
#  Right1SateliteNo_14         -0.1423167  0.0285632  -4.983 6.35e-07 ***
#  Right1SateliteNo_15         -0.1205303  0.0311334  -3.871 0.000109 ***
#  Right1SateliteNo_16         -0.1127903  0.0335064  -3.366 0.000764 ***
#  Right1SateliteNo_18          0.3239271  0.0249539  12.981  < 2e-16 ***

# Residual standard error: 0.354 on 13584 degrees of freedom
# Multiple R-squared:  0.904,	Adjusted R-squared:  0.9012 
# F-statistic: 322.8 on 396 and 13584 DF,  p-value: < 2.2e-16


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

