x<-c("dplyr",'ggplot2','gridExtra',"grid",'stringr','readxl')
lapply(x, require, character.only=T)
setwd('K:\\AirData\\UCDavisData')

load('OrigPMFine_Data.RData') #OrigPMFine_Data
load('EightMonitor_Grid.RData')#EightMonitor_Grid


data1=mutate(OrigPMFine_Data,ID=paste(area,'_',sprintf("%03d",GridX),'_',sprintf("%03d",GridY),sep=''),
			YearMonth=as.Date(paste(year,'-',sprintf("%02d",month),'-15',sep=''),format="%Y-%m-%d")) %>%
	filter(ID %in% EightMonitor_Grid$ID) %>%
	data.frame() %>%
	select(ID,YearMonth,area,PM2.5MASS,PM2.5Cu,PM2.5Fe,PM2.5Mn,PM2.5N.V.,PM2.5N..III.,PM2.5Na.,PM2.5EC,
		PM2.5OC,PM2.5Other,PM2.5Metals,PM2.5Tracer1,PM2.5Tracer2,PM2.5Tracer3,PM2.5Tracer4,PM2.5Tracer5,
		PM2.5Tracer6,PM2.5Tracer7,PM2.5Tracer8,PM2.5Tracer9,PM2.5AALK,PM2.5AXYL,PM2.5ATOL,PM2.5ABNZ,
		PM2.5ABIO,PM2.5AOLG) %>%
	arrange(ID,YearMonth) %>%
	mutate(Sum_Chemi=rowSums(.[5:14],na.rm =TRUE),Diff1=PM2.5MASS-Sum_Chemi,Sum_Source=rowSums(.[15:29],na.rm =TRUE),Diff2=PM2.5MASS-Sum_Source) %>%
	inner_join(EightMonitor_Grid,by='ID')

summary(data1$Diff1)
summary(data1$Diff2)

rm(OrigPMFine_Data)


#### Combine with Total PM2.5 total mass
	
TotPM25_USC=read.csv("K:\\Research\\PMSource_Still\\Data\\totpm25.csv") %>%
	mutate(date2=paste('0',date,sep=''),date=as.Date(paste(str_sub(date2,start=-4),str_sub(date2,-8,-7),str_sub(date2,-6,-5),sep=''),format="%Y%m%d"),YM=substr(date,1,7)) %>%
	select(-date2)
TotPM25_USC_2=aggregate(totpm25~loca+YM,TotPM25_USC,mean,na.rm=TRUE) %>%
	rename(USC_totpm25=totpm25) %>%
	mutate(YearMonth=as.Date(paste(substr(YM,1,4),'-',substr(YM,6,7),'-15',sep=''),format="%Y-%m-%d")) %>%
	select(-YM)


#Overall Correlation
warm=c('May','Jun','Jul','Aug','Sep','Oct')
data2=inner_join(data1,TotPM25_USC_2,by=c('YearMonth','loca')) %>%
	mutate(Year=substr(YearMonth,1,4),Season=ifelse(format(YearMonth,"%b") %in% warm,'Warm','Cold'))

#Overall correlation
cor(data2$PM2.5MASS,data2$USC_totpm25)
cor_val=cor(data2$PM2.5MASS,data2$USC_totpm25) %>% data.frame
##GG plot
xlegend=substitute(paste('USC ',PM[2.5],' Observed Value'))
ylegend=substitute(paste('UC Davis ',PM[2.5],' Estimated Value'))
title='Scatter plot betwen USC Observed value and UCD Estimated Value'
#Overall Scatterplot
sca_1=ggplot(data2, aes(x=USC_totpm25,y=PM2.5MASS)) +
	geom_point(shape=16) +  
	#geom_smooth(method=lm,se=FALSE) +
	geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
	coord_cartesian(ylim=c(0,70))+
	coord_cartesian(xlim=c(0,70))+
	scale_x_continuous(xlegend)+
	scale_y_continuous(ylegend)+
	ggtitle(title)+
	geom_text(x=5,y=45,label=paste("r=",round(cor_val,2),sep=''),size=6,fontface=3)+
	theme_bw()
sca_1

#Corerlation by Site
gp=group_by(data2,SiteName)
dplyr::summarize(gp,cor(PM2.5MASS,USC_totpm25))

gp=group_by(data2,area.x)
dplyr::summarize(gp,cor(PM2.5MASS,USC_totpm25))

gp=group_by(data2,Year)
dplyr::summarize(gp,cor(PM2.5MASS,USC_totpm25))

gp=group_by(data2,Season)
dplyr::summarize(gp,cor(PM2.5MASS,USC_totpm25))

## GG plot by Site
Sca_Site=function(ii){
	data3=filter(data2,loca==ii)
	cor_val=cor(data3$PM2.5MASS,data3$USC_totpm25) %>% data.frame

	xlegend=substitute(paste('USC ',PM[2.5],' Observed Value'))
	ylegend=substitute(paste('UC Davis ',PM[2.5],' Estimated Value'))
	title=paste('Scatter plot in ',unique(data3$SiteName),' (',unique(data3$area.x),')',sep='')

	gplo=ggplot(data3,aes(x=USC_totpm25,y=PM2.5MASS)) +
	geom_point(shape=16)+
	geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
	coord_cartesian(ylim=c(0,70),xlim=c(0,70))+
	scale_x_continuous(xlegend)+
	scale_y_continuous(ylegend)+
	ggtitle(title)+
	geom_text(x=5,y=45,label=paste("r=",round(cor_val,2),sep=''),size=6,fontface=3)+
	theme_bw()

	return(gplo)
}


OutList=list(Sca_Site(1),Sca_Site(2),Sca_Site(3),Sca_Site(4),Sca_Site(5),Sca_Site(6),Sca_Site(7),Sca_Site(8))
Scatter_Out=do.call(arrangeGrob,c(OutList,nrow=3))
#grid.draw(Scatter_Out)

pdf('C:\\Users\\kebisu\\Downloads\\USC_UCD_ScatterPlot.pdf',15,10)
	grid.draw(sca_1)
	grid.newpage()
	grid.draw(Scatter_Out)
dev.off()

############################################
##Combine with PM 2.5 chemical species data
############################################

#Import Species data (Since SAC data are a bit different order, I cleaned it up)

SpePM25_BAK_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Bakersfield (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='BAK')
SpePM25_ELC_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\El Cajon (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='ELC')
SpePM25_FRE_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Fresno (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='FRE')
SpePM25_LAX_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Los Angeles (2002-2013).xlsx",sheet="Input conc.") %>%
	mutate(SiteName='LAX')
SpePM25_RUB_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Rubidoux (2002-2013).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='RUB')
SpePM25_SAC_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Sacramento (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='SAC')
SpePM25_SJO_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\San Jose (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='SJO')
SpePM25_SVY_USC=read_excel("K:\\Research\\PMSource_Still\\Data\\PM species used by USC\\Simi Valley (2002-2009).xlsx",sheet="Input Conc.") %>%
	mutate(SiteName='SVY')

names(SpePM25_SAC_USC)[names(SpePM25_SAC_USC)=="OC "]="OC"
names(SpePM25_SAC_USC)[names(SpePM25_SAC_USC)=="Zinc (Zn)"]="Zn"
SpePM25_SAC_USC=mutate(SpePM25_SAC_USC,Se=NA,Sr=NA,Zr=NA) %>%
	mutate(Se=as.numeric(Se),Sr=as.numeric(Sr),Zr=as.numeric(Zr))
SpePM25_SAC_USC=SpePM25_SAC_USC[names(SpePM25_LAX_USC)]

SpePM25_USC=bind_rows(SpePM25_BAK_USC,SpePM25_ELC_USC,SpePM25_FRE_USC,SpePM25_LAX_USC,SpePM25_RUB_USC,SpePM25_SAC_USC,SpePM25_SJO_USC,SpePM25_SVY_USC) %>%
	mutate(YearMonth=as.Date(Date,format="%Y-%m%-d"),YM=substr(YearMonth,1,7),SiteName=as.character(SiteName)) %>%
	mutate(sumrow=rowSums(.[3:42],na.rm=TRUE),Diff=PM2.5-sumrow) %>%
	select(-Date) %>%
	data.frame()
summary(SpePM25_USC$Diff)

warm=c('May','Jun','Jul','Aug','Sep','Oct')


sca_spe=function(species_USC,species_UCD){
	SpePM25_USC_2=aggregate(SpePM25_USC[,species_USC]~SiteName+YM,SpePM25_USC,mean,na.rm=TRUE)
	names(SpePM25_USC_2)[3]=species_USC
	SpePM25_USC_2=mutate(SpePM25_USC_2,YearMonth=as.Date(paste(substr(YM,1,4),'-',substr(YM,6,7),'-15',sep=''),format="%Y-%m-%d")) %>%
		select(-YM)
	SpePM25_USC_3=merge(data1,SpePM25_USC_2,by=c('YearMonth','SiteName')) %>%
		mutate(Year=substr(YearMonth,1,4),Season=ifelse(format(YearMonth,"%b") %in% warm,'Warm','Cold')) 

	ii=c(which(colnames(SpePM25_USC_3)==species_USC),which(colnames(SpePM25_USC_3)==species_UCD))
	SpePM25_USC_4=SpePM25_USC_3[,c(1,2,42,43,ii)]
	names(SpePM25_USC_4)=c('YearMOnth','SiteName','Year','Season','USC','UCD')
	cor_all=cor(SpePM25_USC_4$USC,SpePM25_USC_4$UCD) %>% data.frame
	print(cor_all)
##GG plot
	xlegend=substitute(paste('USC ',PM[2.5],' ',nn1,' Observed Value'),list(nn1=species_USC))
	ylegend=substitute(paste('UC Davis ',PM[2.5],' ',nn1,' Estimated Value'),list(nn1=species_USC))
	#title='Scatter plot betwen USC Observed value and UCD Estimated Value'
	title=substitute(paste('Scatter plot for ', nn1),list(nn1=species_USC))
	limit=max(max(SpePM25_USC_4$USC),max(SpePM25_USC_4$UCD))
#Overall Scatterplot
	sca_1=ggplot(SpePM25_USC_4, aes(x=USC,y=UCD)) +
		geom_point(shape=16) +  
		#geom_smooth(method=lm,se=FALSE) +
		geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
		coord_cartesian(ylim=c(0,limit),xlim=c(0,limit))+
		scale_x_continuous(xlegend)+
		scale_y_continuous(ylegend)+
		ggtitle(title)+
		geom_text(x=limit/2,y=0,label=paste("r=",round(cor_all,2),sep=''),size=6,fontface=3)+
		theme_bw()
	return(sca_1)
}

OutList=list(
sca_spe('Cu','PM2.5Cu'),
sca_spe('Fe','PM2.5Fe'),
sca_spe('Mn','PM2.5Mn'),
sca_spe('NO3.','PM2.5N.V.'), #nitrate
sca_spe('NH4.','PM2.5N..III.'),
sca_spe('NA.','PM2.5Na.'),
sca_spe('EC','PM2.5EC'),
sca_spe('OC','PM2.5OC'),
sca_spe('PM2.5','PM2.5MASS')
)
Scatter_Out=do.call(arrangeGrob,c(OutList,nrow=3))


pdf('C:\\Users\\kebisu\\Downloads\\USC_UCD_ScatterPlot_Species.pdf',15,10)
	grid.draw(Scatter_Out)
dev.off()


############################################
##Stratify by City
##Combine with PM 2.5 chemical species data
############################################


species_USC='Cu'
species_UCD='PM2.5Cu'

sca_spe_city=function(species_USC,species_UCD){
	SpePM25_USC_2=aggregate(SpePM25_USC[,species_USC]~SiteName+YM,SpePM25_USC,mean,na.rm=TRUE)
	names(SpePM25_USC_2)[3]=species_USC
	SpePM25_USC_2=mutate(SpePM25_USC_2,YearMonth=as.Date(paste(substr(YM,1,4),'-',substr(YM,6,7),'-15',sep=''),format="%Y-%m-%d")) %>%
		select(-YM)
	SpePM25_USC_3=merge(data1,SpePM25_USC_2,by=c('YearMonth','SiteName')) %>%
		mutate(Year=substr(YearMonth,1,4),Season=ifelse(format(YearMonth,"%b") %in% warm,'Warm','Cold')) 

	ii=c(which(colnames(SpePM25_USC_3)==species_USC),which(colnames(SpePM25_USC_3)==species_UCD))
	SpePM25_USC_4=SpePM25_USC_3[,c(1,2,42,43,ii)]
	names(SpePM25_USC_4)=c('YearMOnth','SiteName','Year','Season','USC','UCD')

	list2=as.character(unique(SpePM25_USC_4$SiteName))

p=list()
for (i in 1:length(list2)){
	SpePM25_USC_5=filter(SpePM25_USC_4,SiteName==list2[i])
	cor_all=cor(SpePM25_USC_5[,5],SpePM25_USC_5[,6]) %>% data.frame

##GG plot
	xlegend=substitute(paste('USC ',PM[2.5],' ',nn1,' Observed Value'),list(nn1=species_USC))
	ylegend=substitute(paste('UC Davis ',PM[2.5],' ',nn1,' Estimated Value'),list(nn1=species_USC))
	title=substitute(paste('Scatter plot for ', nn1,' in ',nn2),list(nn1=species_USC,nn2=list2[i]))
	limit=max(max(SpePM25_USC_5[,5]),max(SpePM25_USC_5[,6]))
#Overall Scatterplot
	sca_1=ggplot(SpePM25_USC_5, aes(x=SpePM25_USC_5[,5],y=SpePM25_USC_5[,6])) +
		geom_point(shape=16) +  
		#geom_smooth(method=lm,se=FALSE) +
		geom_abline(intercept=0,slope=1,lwd=1,col='blue')+
		coord_cartesian(ylim=c(0,limit),xlim=c(0,limit))+
		scale_x_continuous(xlegend)+
		scale_y_continuous(ylegend)+
		ggtitle(title)+
		geom_text(x=limit/2,y=0,label=paste("r=",round(cor_all,2),sep=''),size=6,fontface=3)+
		theme_bw()
	p[[i]]=sca_1
}
do.call(grid.arrange,p)
test=do.call(arrangeGrob,c(p,nrow=3))
grid.draw(test)

}


group_by(SpePM25_USC_3,SiteName) %>%
summarize(CU_UCD=mean(PM2.5Cu),CU_USC=mean(Cu))



rm(list=ls())
