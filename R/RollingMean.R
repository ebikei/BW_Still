#jhpce01.jhsph.edu
#qrsh -l mf=16G,h_vmem=32G 

x<-c('dplyr','data.table','zoo')
lapply(x,require,character.only=T)
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
output_location=c('/home/bst/other/kebisu/BW_PMCoarse/Result/')

load('PM10_DF.RData') #PM10_DF
load('PM10Mon_CountyPWC_temp.RData') #PM10Mon_CountyPWC
load('PM25_DF.RData') #PM25_DF
load('PM25Mon_CountyPWC_temp.RData') #PM25Mon_CountyPWC

PMC_DF=inner_join(PM10_DF,PM25_DF,by=c('FIPS_Monitor','Date.Local'))
PMC_DF2=filter(PMC_DF,FIPS_Monitor %in% unique(PM25Mon_CountyPWC$FIPS_Monitor),FIPS_Monitor %in% unique(PM10Mon_CountyPWC$FIPS_Monitor)) %>%
	mutate(PMC_level=PM10_level-PM25_level) 
PMC_Cut=quantile(PMC_DF2$PMC_level,0.99)
#PMC_DF2=mutate(PMC_DF2,ifelse(PMC_level<0,))
PMC_DF2$PMC_level[PMC_DF2$PMC_level<0]=0
#PMC_DF2=filter(PMC_DF2,PMC_level>=0&PMC_level<PMC_Cut) %>% 
PMC_DF2=filter(PMC_DF2,PMC_level<PMC_Cut) %>% 
	select(FIPS_Monitor,Date.Local,PMC_level) %>%
	data.table()
setkey(PMC_DF2,FIPS_Monitor,Date.Local)
PMC_DF3=PMC_DF2[CJ(unique(FIPS_Monitor),seq(min(Date.Local),max(Date.Local),by=1))]
PMC_DF3[,PMC_Week:=rollapply(PMC_level,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=FIPS_Monitor]
PMC_DF3$PMC_Week[is.nan(PMC_DF3$PMC_Week)]=NA

load('BirthData/LMP_FIPS_Cobination.RData') #Combination_List
#setkey(Combination_List,FIPS_County,LMPDate2)

MonitorList=PMC_DF3[!duplicated(PMC_DF3$FIPS_Monitor),list(FIPS_Monitor)] %>%
	mutate(FIPS_County=substr(FIPS_Monitor,1,5)) %>%
	arrange(FIPS_County,FIPS_Monitor)

Combination=inner_join(Combination_List,MonitorList,by='FIPS_County')
rm(Combination_List,PMC_DF2)
output=select(Combination,FIPS_Monitor,LMPDate2,FIPS_County) %>% data.table()
setkey(output,FIPS_Monitor,LMPDate2)
DataMatrix=matrix(0,nrow=dim(Combination)[1],ncol=50)

pb=txtProgressBar(min=1,max=50, style = 3)
for (i in 1:50){
	Sys.sleep(0.1)
	BirthList3=mutate(Combination,Date.Local=LMPDate2+i*7-1)
	setkey(BirthList3,FIPS_Monitor,Date.Local)
	test=PMC_DF3[BirthList3] %>% select(FIPS_Monitor,LMPDate2,Date.Local,PMC_Week)
	setkey(test,FIPS_Monitor,LMPDate2)
	output=cbind(output,test$PMC_Week)
	rm(BirthList3,test)
	setTxtProgressBar(pb,i)
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

BirthList5$PMC_TriFirst=rowMeans(BirthList5[,c(4:16)],na.rm=TRUE)
BirthList5$PMC_TriSecond=rowMeans(BirthList5[,c(17:29)],na.rm=TRUE)
pb=txtProgressBar(min=30,max=50,style=3)
for (i in 30:50){
	Sys.sleep(0.1)
	BirthList5[[paste0('PMC_TriThird_Week_',i)]]=rowMeans(BirthList5[,c(30:(3+i))],na.rm=TRUE)
	setTxtProgressBar(pb,i)
}

BirthList6_PMC=BirthList5[,c(1:3,54:97)]
BirthList6_PMC=BirthList6_PMC[rowSums(!is.na(BirthList6_PMC[,4:24]))>0,]

save(BirthList6_PMC,file='PMC_Col_Exposure_20160422.RData')

rm(list=ls())
