x<-c("stringr","dplyr",'data.table',"readxl","ggplot2","gridExtra","grid","zoo")
lapply(x, require, character.only=T)

drive=c('K:\\Research\\PMSource_Still\\Data')
setwd(drive)


load('AirPolData_20160304.RData') #AirPol_Data
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
setkey(Combination3,SITE_NAME,LMP)


## Specify Pollutant ##
Air=select(AirPol_Data,SITE_NAME=Name,SITE=Site,date,totpm25) %>%
	data.table()
setkey(Air,SITE_NAME,date)
Air2=Air[CJ(unique(SITE_NAME),seq(min(date),max(date),by=1))]
Air2[,PM25_Week:=rollapply(totpm25,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=SITE_NAME]
Air2$PM25_Week[is.nan(Air2$PM25_Week)]=NA

#DataMatrix=matrix(0,nrow=dim(Combination3)[1],ncol=50)
output=select(combination3,SITE_NAME,LMP,SITE) %>% data.table()
setkey(output,SITE_NAME,LMP)

for (i in 1:50){
	temp1=mutate(combination3,date=LMP+i*7-1)
	setkey(temp1,SITE_NAME,date)
	temp2=Air2[temp1] %>% select(SITE_NAME,LMP,date,PM25_Week)
	setkey(temp2,SITE_NAME,LMP)
	output=cbind(output,temp2$PM25_Week)
	rm(temp1,temp2)
}
df1=data.frame(output)
names=paste(rep('Week',50),1:50,sep='')
colnames(df1)[4:53]=names

pb=txtProgressBar(min=20,max=50, style = 3)
for (i in 20:50){
	Sys.sleep(0.1)
#	df1[[paste0('GestWeek_',i)]]=rowMeans(df1[,c(4:(3+i))],na.rm=TRUE)
	df1[[paste0('GestWeek_',i)]]=ifelse(rowSums(!is.na(df1[,4:(3+i)]))>i*0.75,rowMeans(df1[,c(4:(3+i))],na.rm=TRUE),NA)
	setTxtProgressBar(pb,i)
}


df2=df1[rowSums(!is.na(df1[,4:29]))>0,]
df2=df2[rowSums(!is.na(df2[,4:16]))>9,]
df3=df2[,c(1:3,54:84)]
