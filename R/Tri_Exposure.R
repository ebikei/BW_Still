#jhpce01.jhsph.edu
#qrsh -l mf=16G,h_vmem=32G 

x<-c('dplyr','data.table','zoo')
lapply(x,require,character.only=T)
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
output_location=c('/home/bst/other/kebisu/BW_PMCoarse/Result/')

load('PM25_DF.RData') #PM25_DF
load('PM25Mon_CountyPWC.RData') #PM25Mon_CountyPWC

PM25_DF2=filter(PM25_DF,FIPS_Monitor %in% unique(PM25Mon_CountyPWC$FIPS_Monitor)) %>%
	data.table()
setkey(PM25_DF2,FIPS_Monitor,Date.Local)
PM25_DF3=PM25_DF2[CJ(unique(FIPS_Monitor),seq(min(Date.Local),max(Date.Local),by=1))]
PM25_DF3[,PM25_Week:=rollapply(PM25_level,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=FIPS_Monitor]
PM25_DF3$PM25_Week[is.nan(PM25_DF3$PM25_Week)]=NA

load('BirthData/LMP_FIPS_Cobination.RData') #Combination_List
#setkey(Combination_List,FIPS_County,LMPDate2)

MonitorList=PM25_DF3[!duplicated(PM25_DF3$FIPS_Monitor),list(FIPS_Monitor)] %>%
	mutate(FIPS_County=substr(FIPS_Monitor,1,5)) %>%
	arrange(FIPS_County,FIPS_Monitor)

Combination=inner_join(Combination_List,MonitorList,by='FIPS_County')
rm(Combination_List,PM25_DF2)
output=select(Combination,FIPS_Monitor,LMPDate2,FIPS_County) %>% data.table()
setkey(output,FIPS_Monitor,LMPDate2)
DataMatrix=matrix(0,nrow=dim(Combination)[1],ncol=50)

for (i in 1:50){
	BirthList3=mutate(Combination,Date.Local=LMPDate2+i*7-1)
	setkey(BirthList3,FIPS_Monitor,Date.Local)
	test=PM25_DF3[BirthList3] %>% select(FIPS_Monitor,LMPDate2,Date.Local,PM25_Week)
	setkey(test,FIPS_Monitor,LMPDate2)
	output=cbind(output,test$PM25_Week)
	rm(BirthList3,test)
}

BirthList4=data.frame(output)
names=paste(rep('Week',50),1:50,sep='')
colnames(BirthList4)[4:53]=names

pb=txtProgressBar(min=30,max=50, style = 3)
for (i in 30:50){
	Sys.sleep(0.1)
	BirthList4[[paste0('GestWeek_',i)]]=rowMeans(BirthList4[,c(4:(3+i))],na.rm=TRUE)
	setTxtProgressBar(pb,i)
}

BirthList4=BirthList4[rowSums(!is.na(BirthList4[,4:29]))>0,]
BirthList4=BirthList4[rowSums(!is.na(BirthList4[,4:16]))>9,]
BirthList4=BirthList4[rowSums(!is.na(BirthList4[,17:29]))>9,]

BirthList5=BirthList4
pb=txtProgressBar(min=30,max=50, style = 3)
for (i in 30:50){
	Sys.sleep(0.1)
	BirthList5[,c(i+24)]=ifelse(rowSums(!is.na(BirthList5[,30:(i+3)]))<((i-26)*0.75),NA,BirthList5[,c(i+24)])
	setTxtProgressBar(pb,i)
}

BirthList5$PM25_TriFirst=rowMeans(BirthList5[,c(4:16)],na.rm=TRUE)
BirthList5$PM25_TriSecond=rowMeans(BirthList5[,c(17:29)],na.rm=TRUE)
pb=txtProgressBar(min=30,max=50,style=3)
for (i in 30:50){
	Sys.sleep(0.1)
	BirthList5[[paste0('PM25_TriThird_Week_',i)]]=rowMeans(BirthList5[,c(30:(3+i))],na.rm=TRUE)
	setTxtProgressBar(pb,i)
}

BirthList6_PM25=BirthList5[,c(1:3,54:97)]
BirthList6_PM25=BirthList6_PM25[rowSums(!is.na(BirthList6_PM25[,4:24]))>0,]

save(BirthList6_PM25,file='PM25_Exposure_20150318.RData')

rm(list=ls())


test=BirthList6_PM25[rowSums(!is.na(BirthList6_PM25[,4:24]))==0,]
