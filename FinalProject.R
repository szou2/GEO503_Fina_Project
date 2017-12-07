#GEO503 Fall2017 Final Project 
#Shengyuan Zou
library(rgdal)
library(rgeos)

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maptools)
library(tigris)
library(censusapi)

#Input shapefile-  2015 census block group boundary, Erie, NY
#Download from TIGER 029 at block_group level for Erie County using tigris package
shpfile<-block_groups('NY',county='Erie',year='2015')
#shp = gSimplify(shpfile)
plot(shpfile,col=1:100)

head(shpfile)
#Download vacancy status, Median Household Income and Race Data from CensusAPI for 2013-2015
#Income2013=getCensus(name='acs5',vintage =2013,key='062204a2874cf2aa0310ac7ac2444243b0d6360e',vars =c("RACE") ,region='block group:*',
#                   regionin = '"state:36+county:29')

#However, no block-group level data available in census API
#We have to download from census website.
Income2013=read.csv(file = 'Data/Income2013.csv')

Income2014=read.csv(file = 'Data/Income2014.csv')

Income2015=read.csv(file = 'Data/Income2015.csv')

VR2013=read.csv(file = 'Data/VacancyRate2013.csv')

VR2014=read.csv(file = 'Data/VacancyRate2014.csv')

VR2015=read.csv(file = 'Data/VacancyRate2015.csv')

Race2013=read.csv(file = 'Data/RaceRatio2013.csv')

Race2014=read.csv(file = 'Data/RaceRatio2014.csv')

Race2015=read.csv(file = 'Data/RaceRatio2015.csv')


##new_shp=geo_join(shpfile,Income2013,'GEOID','GEO.id2')
#plot(new_shp,col=new_shp$HD01_VD01)

#Join tables and calculate the difference
Income1=merge(Income2013,Income2014,by.x = 'GEO.id2',by.y = 'GEO.id2')

Income1$Change1=as.numeric(Income1$HD01_VD01.y)-as.numeric(Income1$HD01_VD01.x)

Income2=merge(Income2014,Income2015,by.x = 'GEO.id2',by.y = 'GEO.id2')

Income2$Change2=as.numeric(Income2$HD01_VD01.y)-as.numeric(Income2$HD01_VD01.x)

VR1=merge(VR2013,VR2014,by.x = 'GEO.id2',by.y = 'GEO.id2')

VR1$Change1=as.numeric(VR1$HD01_VD03.y)/as.numeric(VR1$HD01_VD01.y)-as.numeric(VR1$HD01_VD03.x)/as.numeric(VR1$HD01_VD01.x)

VR2=merge(VR2014,VR2015,by.x = 'GEO.id2',by.y = 'GEO.id2')

VR2$Change2=as.numeric(VR2$HD01_VD03.y)/as.numeric(VR2$HD01_VD01.y)-as.numeric(VR2$HD01_VD03.x)/as.numeric(VR2$HD01_VD01.x)


Race1=merge(Race2013,Race2014,by.x = 'GEO.id2',by.y = 'GEO.id2')

Race1$Change1=as.numeric(Race1$HD01_VD02.y)/as.numeric(Race1$HD01_VD01.y)-as.numeric(Race1$HD01_VD02.x)/as.numeric(Race1$HD01_VD01.x)

Race2=merge(Race2014,Race2015,by.x = 'GEO.id2',by.y = 'GEO.id2')

Race2$Change2=as.numeric(Race2$HD01_VD02.y)/as.numeric(Race2$HD01_VD01.y)-as.numeric(Race2$HD01_VD02.x)/as.numeric(Race2$HD01_VD01.x)

#Join change to geometric
Income1_Join=geo_join(shpfile,Income1,'GEOID','GEO.id2',how='inner')
Income2_Join=geo_join(shpfile,Income2,'GEOID','GEO.id2',how='inner')
VR1_Join=geo_join(shpfile,VR1,'GEOID','GEO.id2',how='inner')
VR2_Join=geo_join(shpfile,VR2,'GEOID','GEO.id2',how='inner')
Race1_Join=geo_join(shpfile,Race1,'GEOID','GEO.id2',how='inner')
Race2_Join=geo_join(shpfile,Race2,'GEOID','GEO.id2',how='inner')

#Visualize the change in house vacancy, income and demographic made up
plot(Income2_Join,axes=TRUE)
plot(Income2_Join[Income2_Join$Change2>0,],add=T,col='red')
plot(Income2_Join[Income2_Join$Change2<0,],add=T,col='blue')

plot(VR2_Join,axes=TRUE)
plot(VR2_Join[VR2_Join$Change2>0,],add=T,col='red')
plot(VR2_Join[VR2_Join$Change2<0,],add=T,col='blue')

plot(Race2_Join,axes=TRUE)
plot(Race2_Join[Race2_Join$Change2>0,],add=T,col='red')
plot(Race2_Join[Race2_Join$Change2<0,],add=T,col='blue')
#plot(Income1_Join[Income1_Join$Change1<-22,],add=T,col='blue')
#plot(Income1_Join[Income1_Join$Change1>-22&&Income1_Join$Change1<5,],add=T,col='yellow')
#plot(Income1_Join[Income1_Join$Change1>5&&Income1_Join$Change1<31,],add=T,col='green')
#plot(Income1_Join[Income1_Join$Change1>31,],add=T,col='red')



plot(Income2_Join,axes=TRUE)
plot(Income2_Join[Income2_Join$Change2>0,],add=T,col='red')
plot(Income2_Join[Income2_Join$Change2<0,],add=T,col='blue')

plot(VR2_Join,axes=TRUE)
plot(VR2_Join[VR2_Join$Change2>0,],add=T,col='red')
plot(VR2_Join[VR2_Join$Change2<0,],add=T,col='blue')

plot(Race2_Join,axes=TRUE)
plot(Race2_Join[Race2_Join$Change2>0,],add=T,col='red')
plot(Race2_Join[Race2_Join$Change2<0,],add=T,col='blue')

#merge 2 years change data and summarize
Income_Total=merge(Income1,Income2,by="GEO.id2")
Income_Total$ChangeInTwoYears<-Income_Total$Change1+Income_Total$Change2
summary(Income_Total$ChangeInTwoYears)

VR_Total=merge(VR1,VR2,by="GEO.id2")
VR_Total$ChangeInTwoYears<-VR_Total$Change1+VR_Total$Change2
summary(VR_Total$ChangeInTwoYears)

Race_Total=merge(Race1,Race2,by="GEO.id2")
Race_Total$ChangeInTwoYears<-Race_Total$Change1+Race_Total$Change2
summary(Race_Total$ChangeInTwoYears)

#Pearson's Correlation coefficient
#Join all 2year change 

Change_Join=merge(Income_Total,VR_Total,by="GEO.id2")
Change_Join_All=merge(Change_Join,Race_Total,by="GEO.id2")
head(Change_Join_All)
Change_Income=Change_Join_All$ChangeInTwoYears.x
Change_VR=Change_Join_All$ChangeInTwoYears.y
Change_Race=Change_Join_All$ChangeInTwoYears
#cor between vr and income
cor.test(Change_Income,Change_VR)
cor.test(Change_Income,Change_Race)
cor.test(Change_Race,Change_VR)