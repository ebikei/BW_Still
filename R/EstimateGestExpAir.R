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
setkey(combination3,SITE_NAME,LMP)

Pol_Names=names(AirPol_Data)
Pol_Names=Pol_Names[-c(1:4,13,16)]
df=data.frame()

##########
## Loop ##
##########

for (i in 1:length(Pol_Names)){

	polname=Pol_Names[i]
	Air=select_(AirPol_Data,"Name","Site","date",polname) %>%
		rename(SITE_NAME=Name,SITE=Site) %>%
		rename_(Pol=polname) %>%
		data.table()
	setkey(Air,SITE_NAME,date)
	Air2=Air[CJ(unique(SITE_NAME),seq(min(date),max(date),by=1))]
	Air2[,Pol_Week:=rollapply(Pol,7,mean,align=c("right"),fill=NA,na.rm=TRUE,partial=TRUE),by=SITE_NAME]
	Air2$Pol_Week[is.nan(Air2$Pol_Week)]=NA

	output=select(combination3,SITE_NAME,LMP,SITE) %>% data.table()
	setkey(output,SITE_NAME,LMP)

	for (i in 1:50){
		temp1=mutate(combination3,date=LMP+i*7-1)
		setkey(temp1,SITE_NAME,date)
		temp2=Air2[temp1] %>% select(SITE_NAME,LMP,date,Pol_Week)
		setkey(temp2,SITE_NAME,LMP)
		output=cbind(output,temp2$Pol_Week)
		rm(temp1,temp2)
	}
	df1=data.frame(output)
	names=paste(rep('Week',50),1:50,sep='')
	colnames(df1)[4:53]=names

	pb=txtProgressBar(min=20,max=50, style = 3)
	for (i in 20:50){
		Sys.sleep(0.1)
		df1[[paste0('GestWeek_',i)]]=ifelse(rowSums(!is.na(df1[,4:(3+i)]))>i*0.75,rowMeans(df1[,c(4:(3+i))],na.rm=TRUE),NA)
		setTxtProgressBar(pb,i)
	}	

	df2=df1[rowSums(!is.na(df1[,4:29]))>0,]
	df2=df2[rowSums(!is.na(df2[,4:16]))>9,]
	df3=cbind(polname,df2[,c(1:3,54:84)])
	df=rbind(df,df3)
	rm(Air,Air2,output,df1,df2,df3)
}


test=group_by(df,SITE_NAME,polname) %>%
	summarize(mean(GestWeek_39,na.rm=TRUE)) %>%
	data.frame() %>%
	arrange(polname,SITE_NAME)

filter(df,polname=='Mn',SITE_NAME=='SJO',LMP=='2005-01-02')

GestAirData=df
save(GestAirData,file='GestAirData.RData')

rm(list=ls())

