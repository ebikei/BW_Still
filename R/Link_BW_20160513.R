x<-c('dplyr','data.table','ggplot2','corrplot')
lapply(x, require, character.only=T)

setwd('C:\\Users\\kebisu\\Documents\\Research\\PMSource_Still\\Data')

load('BWData_20160309.RData') #BWData3
#BWData3=sample_n(BWData3,1000)
#save(BWData3,file='C:\\Users\\kebisu\\Desktop\\smp.RData')

bwdata=filter(BWData3,gestweeks>=37,gestweeks<45,FetalDeath!=1,!is.na(sex),!is.na(clean)) %>%
	select(-complicpreg1,-complicpreg2,-complicpreg3,-complicpreg4,-complicpreg5,-complicpreg6,
		-complicpreg7,-complicpreg8,-complicpreg9,-complicpreg10,-complicpreg11,-complicpreg12,
		-complicpreg13,-complicpreg14,-complicpreg15,-complicpreg16,-compliclabor1,-compliclabor2,
		-compliclabor3,-compliclabor4,-compliclabor5,-compliclabor6,-compliclabor7,-compliclabor8,
		-compliclabor9,-condition1,-condition2,-condition3,-condition4,-condition5,-condition6,-condition7,
		-condition8,-condition9,-condition10)
rm(BWData3)
bwdata$SGA10=0
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==22&bwdata$birthWeight<393]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==23&bwdata$birthWeight<453]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==24&bwdata$birthWeight<498]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==25&bwdata$birthWeight<554]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==26&bwdata$birthWeight<594]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==27&bwdata$birthWeight<674]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==28&bwdata$birthWeight<766]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==29&bwdata$birthWeight<906]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==30&bwdata$birthWeight<1044]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==31&bwdata$birthWeight<1241]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==32&bwdata$birthWeight<1475]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==33&bwdata$birthWeight<1712]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==34&bwdata$birthWeight<1957]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==35&bwdata$birthWeight<2192]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==36&bwdata$birthWeight<2410]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==37&bwdata$birthWeight<2609]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==38&bwdata$birthWeight<2807]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==39&bwdata$birthWeight<2947]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==40&bwdata$birthWeight<3029]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==41&bwdata$birthWeight<3063]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==42&bwdata$birthWeight<2979]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==43&bwdata$birthWeight<2949]=1
bwdata$SGA10[bwdata$sex==1&bwdata$gestweeks==44&bwdata$birthWeight<2954]=1

bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==22&bwdata$birthWeight<362]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==23&bwdata$birthWeight<416]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==24&bwdata$birthWeight<470]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==25&bwdata$birthWeight<504]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==26&bwdata$birthWeight<556]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==27&bwdata$birthWeight<622]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==28&bwdata$birthWeight<693]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==29&bwdata$birthWeight<845]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==30&bwdata$birthWeight<965]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==31&bwdata$birthWeight<1180]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==32&bwdata$birthWeight<1390]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==33&bwdata$birthWeight<1638]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==34&bwdata$birthWeight<1872]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==35&bwdata$birthWeight<2099]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==36&bwdata$birthWeight<2299]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==37&bwdata$birthWeight<2495]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==38&bwdata$birthWeight<2694]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==39&bwdata$birthWeight<2834]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==40&bwdata$birthWeight<2919]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==41&bwdata$birthWeight<2949]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==42&bwdata$birthWeight<2893]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==43&bwdata$birthWeight<2849]=1
bwdata$SGA10[bwdata$sex==2&bwdata$gestweeks==44&bwdata$birthWeight<2863]=1


load('K:\\Research\\PMSource_LBW\\Data\\GestAirData.RData') #GestAirData
load('K:\\Research\\PMSource_LBW\\Data\\GestWeatherData.RData') #GestWeatherData

df=rbind(GestAirData,GestWeatherData)
rownames(df)=c(1:dim(df)[1])
#df2=filter(df,polname %in% c('totpm25','AT'))
#save(df2,file='C:\\Users\\kebisu\\Desktop\\smp_air.RData')

BWData=BWData3[,c(1:114)]
