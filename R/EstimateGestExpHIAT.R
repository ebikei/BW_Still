x<-c("dplyr","ggplot2","data.table","zoo")
lapply(x, require, character.only=T)

drive=c("K:\\Research\\PMSource_Still\\Data")
setwd(drive)

load('WeatherData_20160317.RData') #Weather_Data
load('combination_20160303.RData') #combination

combination2=mutate(combination,
		LMP=as.Date(paste(paste('20',substr(lmp,6,7),sep=''),substr(lmp,3,5),substr(lmp,1,2),sep=''),format="%Y%b%d"),
		DelivDate=as.Date(paste(paste('20',substr(delivDate,6,7),sep=''),substr(delivDate,3,5),substr(delivDate,1,2),sep=''),format="%Y%b%d")) %>%
	select(-c(lmp,delivDate)) 

#Pick out Unique combination
combination3=combination2 %>%
	distinct(SITE_NAME,LMP) %>%
	arrange(SITE_NAME,LMP) %>%
	data.table()
setkey(combination3,SITE_NAME,LMP)

## Gestational Exposure for HI
Weather=select(Weather_Data,Weather_FIPS,SITE,Date,HI) %>%
		rename(FIPS=Weather_FIPS,date=Date) %>%
		data.table()
setkey(Weather,SITE,date)

Weather2=Weather[CJ(unique(SITE),seq(min(date),max(date),by=1))]
Weather2[,HI_Week:=rollapply(HI,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=SITE]
Weather2$HI[is.nan(Weather2$HI)]=NA

output=select(combination3,SITE,LMP,SITE) %>% data.table()
setkey(output,SITE,LMP)

for (i in 1:50){
	temp1=mutate(combination3,date=LMP+i*7-1)
	setkey(temp1,SITE,date)
	temp2=Weather2[temp1] %>% select(SITE,LMP,date,HI_Week)
	setkey(temp2,SITE,LMP)
	output=cbind(output,temp2$HI_Week)
	rm(temp1,temp2)
}

df1=data.frame(output)
names=paste(rep('Week',50),1:50,sep='')
colnames(df1)[3:52]=names

pb=txtProgressBar(min=20,max=50, style = 3)
for (i in 20:50){
	Sys.sleep(0.1)
	df1[[paste0('GestWeek_',i)]]=ifelse(rowSums(!is.na(df1[,3:(2+i)]))>i*0.75,rowMeans(df1[,c(3:(2+i))],na.rm=TRUE),NA)
	setTxtProgressBar(pb,i)
}	

df2=df1[rowSums(!is.na(df1[,3:28]))>0,]
df2=df2[rowSums(!is.na(df2[,3:15]))>9,]
Weather_Var='HI'
df3=cbind(Weather_Var,df2[,c(1:2,53:83)])

rm(Weather,Weather2,df1,df2)

#################################
## Gestational Exposure for AT
#################################

Weather=select(Weather_Data,Weather_FIPS,SITE,Date,AT) %>%
		rename(FIPS=Weather_FIPS,date=Date) %>%
		data.table()
setkey(Weather,SITE,date)

Weather2=Weather[CJ(unique(SITE),seq(min(date),max(date),by=1))]
Weather2[,AT_Week:=rollapply(AT,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=SITE]
Weather2$AT[is.nan(Weather2$AT)]=NA

output=select(combination3,SITE,LMP,SITE) %>% data.table()
setkey(output,SITE,LMP)

for (i in 1:50){
	temp1=mutate(combination3,date=LMP+i*7-1)
	setkey(temp1,SITE,date)
	temp2=Weather2[temp1] %>% select(SITE,LMP,date,AT_Week)
	setkey(temp2,SITE,LMP)
	output=cbind(output,temp2$AT_Week)
	rm(temp1,temp2)
}

df1=data.frame(output)
names=paste(rep('Week',50),1:50,sep='')
colnames(df1)[3:52]=names

pb=txtProgressBar(min=20,max=50, style = 3)
for (i in 20:50){
	Sys.sleep(0.1)
	df1[[paste0('GestWeek_',i)]]=ifelse(rowSums(!is.na(df1[,3:(2+i)]))>i*0.75,rowMeans(df1[,c(3:(2+i))],na.rm=TRUE),NA)
	setTxtProgressBar(pb,i)
}	

df2=df1[rowSums(!is.na(df1[,3:28]))>0,]
df2=df2[rowSums(!is.na(df2[,3:15]))>9,]
Weather_Var='AT'
df2_AT=cbind(Weather_Var,df2[,c(1:2,53:83)])

GestWeatherData=rbind(df3,df2_AT)
save(GestWeatherData,file='GestWeatherData.RData')

rm(list=ls())
