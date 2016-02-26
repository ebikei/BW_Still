x<-c("dplyr","ggplot2","data.table","haven",'readxl','sp','rgdal','gridExtra',"grid")
lapply(x, require, character.only=T)

drive=c("M:\\Atesgis\\Calfine_Data_Casecrossover")
setwd(drive)

## Read EPA Monitor Place and data ###
EPA_Gas=read_sas('Exposure\\sw_gases_1999_2009.sas7bdat') %>%
	data.frame() %>%
	select(DATE,stn_id,maxno2,maxo3,maxeighthro3)

EPA_Gas=mutate(EPA_Gas,FIPS=sprintf("%09d",as.numeric(as.character(stn_id)))) %>%
	select(DATE,FIPS,maxno2,maxo3,maxeighthro3)

EPA_Mon=read_excel('GIS\\Shared Information\\Gas_1999_thru_2009\\gaslocs_1999_thru_2009.xls') %>%
	data.frame() %>%
	mutate(FIPS=sprintf("%09d",as.numeric(as.character(stn_id)))) %>%
	select(FIPS,finallat,finallong,coorddatum)
## Change datum 
target=c("WGS84","UNKNOWN")
EPA_Mon2_1=filter(EPA_Mon,coorddatum %in% target)
coordinates(EPA_Mon2_1)=c('finallong','finallat')
proj4string(EPA_Mon2_1)=CRS("+proj=longlat +datum=WGS84")

EPA_Mon2_2=filter(EPA_Mon,coorddatum=='NAD27')
coordinates(EPA_Mon2_2)=c('finallong','finallat')
proj4string(EPA_Mon2_2)=CRS("+proj=longlat +datum=NAD27")
EPA_Mon2_2=spTransform(EPA_Mon2_2,proj4string(EPA_Mon2_1))

EPA_Mon2_3=filter(EPA_Mon,coorddatum=='NAD83')
coordinates(EPA_Mon2_3)=c('finallong','finallat')
proj4string(EPA_Mon2_3)=CRS("+proj=longlat +datum=NAD83")
EPA_Mon2_3=spTransform(EPA_Mon2_3,proj4string(EPA_Mon2_1))

EPA_Mon3=rbind(EPA_Mon2_1,EPA_Mon2_2,EPA_Mon2_3)

## Load Grid files (Note: I only used sjv_secondary; sjv_primary was not used)

Grid_scb4=readOGR("K:\\AirData\\UCDavisData\\GIS_Kleeman_Grids", "grid_scb4km")
Grid_scb4=spTransform(Grid_scb4,proj4string(EPA_Mon3))

Grid_sjv4_secondary=readOGR("K:\\AirData\\UCDavisData\\GIS_Kleeman_Grids", "grid_sjv4km_secondary")
Grid_sjv4_secondary=spTransform(Grid_sjv4_secondary,proj4string(EPA_Mon3))


##Overlay

output=data.frame()
for (i in 1:dim(EPA_Mon3)[1]){
	tryCatch({
	test=over(EPA_Mon3[i,],Grid_scb4) %>% 
		na.omit() %>%
		data.frame(.,EPA_Mon3[i,]) %>%
		mutate(area='scb',ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep='')) 
	output=rbind(output,test)
	rm(test)
	}, error=function(e){})
}
for (i in 1:dim(EPA_Mon3)[1]){
	tryCatch({
	test=over(EPA_Mon3[i,],Grid_sjv4_secondary) %>% 
		na.omit() %>%
		data.frame(.,EPA_Mon3[i,]) %>%
		mutate(area='sjv',ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep='')) 
	output=rbind(output,test)
	rm(test)
	}, error=function(e){})
}
output$FIPS=as.character(output$FIPS)


##NO2 

EPA_Gas2=filter(EPA_Gas,!is.na(maxno2)) %>%
	mutate(YM=substr(DATE,1,7)) 
EPA_Gas3=aggregate(maxno2~FIPS+YM,EPA_Gas2,mean,na.rm=TRUE) %>%
	inner_join(output,by='FIPS')

#Load NO2 Chemical Transport Model
Davis_NO2=fread('M:\\Atesgis\\Calfine_Data\\Exposure\\Davis Data\\all_no2_soa_4km.csv') %>%
	data.frame() %>%
	mutate(ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep=''),
	YM=as.character(paste(year,'-',sprintf("%02d",month),sep=''))) %>%
	select(ID,YM,area,NO2)

## Merge files
data2=merge(EPA_Gas3,Davis_NO2,by=c('ID','YM')) #%>%
	#filter(area.y=='scb')
cor(data2$maxno2,data2$NO2)
cor_val=cor(data2$maxno2,data2$NO2)
## GG plot
xlegend=substitute(paste('EPA ',NO[2],' Observed Value'))
ylegend=substitute(paste('UC Davis ',NO[2],' Estimated Value'))
title='Scatter plot betwen EPA Observed value and UCD Estimated Value'
#Overall Scatterplot
sca_1=ggplot(data2, aes(x=maxno2,y=NO2)) +
	geom_point(shape=16) +  
	#geom_smooth(method=lm,se=FALSE) +
	geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
	coord_cartesian(ylim=c(0,0.15),xlim=c(0,0.15))+
	scale_x_continuous(xlegend)+
	scale_y_continuous(ylegend)+
	ggtitle(title)+
	geom_text(x=0.10,y=0.025,label=paste("r=",round(cor_val,2),sep=''),size=6,fontface=3)+
	theme_bw()
sca_1

## Ozone 1hr
EPA_Gas2=filter(EPA_Gas,!is.na(maxo3)) %>%
	mutate(YM=substr(DATE,1,7)) 
EPA_Gas3=aggregate(maxo3~FIPS+YM,EPA_Gas2,mean,na.rm=TRUE) %>%
	mutate(maxo3=maxo3*1000) %>%
	inner_join(output,by='FIPS')

#Load O3 Chemical Transport Model
Davis_O3=fread('M:\\Atesgis\\Calfine_Data\\Exposure\\Davis Data\\peako3_daily_4km.csv') %>%
	data.frame() %>%
	mutate(ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep=''),
	YM=as.character(paste(year,'-',sprintf("%02d",month),sep=''))) %>%
	select(ID,YM,area,meanPeakO31h,meanPeakO38h)
data3=merge(EPA_Gas3,Davis_O3,by=c('ID','YM')) #%>%
	#filter(area.y=='scb')
cor(data3$maxo3,data3$meanPeakO31h)
cor_val=cor(data3$maxo3,data3$meanPeakO31h)
## GG plot
xlegend=substitute(paste('EPA 1hr peak ',O[3],' Observed Value'))
ylegend=substitute(paste('UC Davis 1hr peak ',O[3],' Estimated Value'))
title='Scatter plot betwen EPA Observed value and UCD Estimated Value'
#Overall Scatterplot
sca_2=ggplot(data3, aes(x=maxo3,y=meanPeakO31h)) +
	geom_point(shape=16) +  
	#geom_smooth(method=lm,se=FALSE) +
	geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
	coord_cartesian(ylim=c(0,125),xlim=c(0,125))+
	scale_x_continuous(xlegend)+
	scale_y_continuous(ylegend)+
	ggtitle(title)+
	geom_text(x=70,y=5,label=paste("r=",round(cor_val,2),sep=''),size=6,fontface=3)+
	theme_bw()
sca_2

## Ozone 8hr
EPA_Gas2=filter(EPA_Gas,!is.na(maxeighthro3)) %>%
	mutate(YM=substr(DATE,1,7)) 
EPA_Gas3=aggregate(maxeighthro3~FIPS+YM,EPA_Gas2,mean,na.rm=TRUE) %>%
	mutate(maxeighthro3=maxeighthro3*1000) %>%
	inner_join(output,by='FIPS')

#Load O3 Chemical Transport Model
Davis_O3=fread('M:\\Atesgis\\Calfine_Data\\Exposure\\Davis Data\\peako3_daily_4km.csv') %>%
	data.frame() %>%
	mutate(ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep=''),
	YM=as.character(paste(year,'-',sprintf("%02d",month),sep=''))) %>%
	select(ID,YM,area,meanPeakO31h,meanPeakO38h)
data3=merge(EPA_Gas3,Davis_O3,by=c('ID','YM')) #%>%
	#filter(area.y=='scb')
cor(data3$maxeighthro3,data3$meanPeakO38h)
cor_val=cor(data3$maxeighthro3,data3$meanPeakO38h)
## GG plot
xlegend=substitute(paste('EPA 8hr peak ',O[3],' Observed Value'))
ylegend=substitute(paste('UC Davis 8hr peak ',O[3],' Estimated Value'))
title='Scatter plot betwen EPA Observed value and UCD Estimated Value'
#Overall Scatterplot
sca_3=ggplot(data3, aes(x=maxeighthro3,y=meanPeakO38h)) +
	geom_point(shape=16) +  
	#geom_smooth(method=lm,se=FALSE) +
	geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
	coord_cartesian(ylim=c(0,120),xlim=c(0,120))+
	scale_x_continuous(xlegend)+
	scale_y_continuous(ylegend)+
	ggtitle(title)+
	geom_text(x=70,y=5,label=paste("r=",round(cor_val,2),sep=''),size=6,fontface=3)+
	theme_bw()
sca_3

do.call(grid.arrange,list(sca_1,sca_2,sca_3))

