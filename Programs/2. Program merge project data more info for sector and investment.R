
## The World Bank projects data is downlowded from Aiddata website: https://www.aiddata.org/datasets
## This program is to get the projects details for the Philippines



rm(list = ls())

projDetail<-read.csv('projects_ancillary.csv')

projDetailPH<-projDetail[projDetail$COUNTRY=="Philippines",]

remove(projDetail)

projGeo<-read.csv("WB projects in Philippines.csv")


leveldata<-read.csv('level_1a.csv')

leveldataPH<-leveldata[leveldata$recipients=="Philippines",]

remove(leveldata)

## Both projGeo and leveldata have several obs for the same project ID
## projGeo has already listed all the geocodes of the locations of the same projects
## The net commitment amount in GeoDetail is different from leveldata commitment and disbursement


######################################################
##
aa<-table(projDetailPH$LENDING.INSTRUMENT)
aa[aa!=0]

##  Adaptable Program Loan             Development Policy Lending                Emergency Recovery Loan 
## 9                                      8                                      1 
## Financial Intermediary Loan           Investment Project Financing           Learning and Innovation Loan 
## 1                                      4                                      1 
## Sector Investment and Maintenance Loan               Specific Investment Loan             Structural Adjustment Loan 
##     6                                     35                                      1 
## Technical Assistance Loan 
## 1 

##

aa<-table(projDetailPH$SECTOR.1)
aa[aa!=0]

##Agro-industry, marketing, and trade                                     Animal production                                               Banking 
#1                                                     1                                                     1 
#Central government administration      General agriculture, fishing and forestry sector                              General education sector 
#6                                                     3                                                     1 
#General finance sector                  General public administration sector General water, sanitation and flood protection sector 
#4                                                     2                                                     7 
#Health                               Irrigation and drainage                                       Law and justice 
#4                                                     3                                                     1 
#Other Renewable Energy                                 Other social services                                                 Power 
#2                                                     5                                                     1 
#Primary education          Public administration- Other social services                                    Roads and highways 
#4                                                     1                                                     6 
#Rural and Inter-Urban Roads and Highways                                              Sewerage                Sub-national government administration 
#2                                                     1                                                     3 
#Telecommunications          Transmission and Distribution of Electricity                                       Urban Transport 
#1                                                     1                                                     1 
#Wastewater Collection and Transportation                     Wastewater Treatment and Disposal                                          Water supply 
#1                                                     1                                                     3 


##################################################################

## for adaptation return

projIDSelect<-as.character(projDetailPH$PROJECT.ID[projDetailPH$SECTOR.1%in%c("Roads and highways","Rural and Inter-Urban Roads and Highways","Urban Transport")])

projGeo<-projGeo[projGeo$project_id%in%projIDSelect,]


projGeo$SectorMa<-NA
projGeo$SectorMaPCT<-NA
projGeo$SectorMi<-NA
projGeo$SectorMiPCT<-NA
projGeo$StartYear<-NA
projGeo$EndYear<-NA
projGeo$AnnualDisbur<-NA
projGeo$AnnualprojCost<-NA
projGeo$WBPCT<-NA
projGeo$IEGOutcome<-NA

## the amount of each is assumed evenly distributed across the geo locations of even_split_disbursements
## we further assume it is evenly disbursed across years

N=length(projGeo[,1])

## check
## i<-404
## as.data.frame(cbind(as.character(t(temp1[seq(24, 44,by=5)])),t(temp1[seq(27,47,by=5)])))
## V1 1603
## SECTOR.1.PCT                                    Roads and highways   22
## SECTOR.2.PCT General water, sanitation and flood protection sector   20
## SECTOR.3.PCT                               Irrigation and drainage   20
## SECTOR.4.PCT                              General education sector   20
## SECTOR.5.PCT                                 Other social services   18

for (i in 1:N)
{ 
  
  temp1<-projDetailPH[projDetailPH$PROJECT.ID%in%projGeo$project_id[i],]
  PctVec<-as.data.frame(cbind(as.character(t(temp1[seq(24, 44,by=5)])),t(temp1[seq(27,47,by=5)])))
  PctVec[,2]<-as.numeric(as.character(PctVec[,2]))
  
  projGeo$SectorMa[i]<-as.character(PctVec[PctVec[,2]==max(PctVec[,2]),1])
  projGeo$SectorMaPCT[i]<-PctVec[PctVec[,2]==max(PctVec[,2]),2]
  projGeo$SectorMi[i]<-as.character(PctVec[PctVec[,2]==max(PctVec[PctVec[,1]!=projGeo$SectorMa[i],2]),1])[1]
  projGeo$SectorMiPCT[i]<-PctVec[PctVec[,2]==max(PctVec[PctVec[,1]!=projGeo$SectorMa[i],2]),2][1]
  projGeo$IEGOutcome[i]<-as.character(temp1$IEG_Outcome)
  
  temp2<-leveldataPH[leveldataPH$project_location_id%in%projGeo$project_location_id[i],]
  
  projGeo$StartYear[i]<-temp2$transactions_start_year
  projGeo$EndYear[i]<-temp2$transactions_end_year
   projGeo$AnnualDisbur[i]<-temp2$even_split_disbursements/(temp2$transactions_end_year-temp2$transactions_start_year)
   projGeo$AnnualprojCost[i]<-temp1$LENDING.PROJECT.COST*10^6/(temp2$total_disbursements/temp2$even_split_disbursements)/(temp2$transactions_end_year-temp2$transactions_start_year)
   projGeo$WBPCT[i]<-projGeo$AnnualDisbur[i]/projGeo$AnnualprojCost[i]*100
   
   print(i)
   
   remove(temp1)
   remove(temp2)
}

  

projGeoPH<-projGeo                    

write.csv(projGeoPH,"proj Geo Philippines with disbursement Roads and Highway.csv",row.names=FALSE)
                  
