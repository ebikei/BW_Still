x<-c("dplyr","ggplot2","data.table","weathermetrics","readxl")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)


load("Temp_Data_20160120.RData") 
load("Temp_Monitor_20160120.RData") 
load("RH_Data_20160120.RData") 
load("RH_Monitor_20160120.RData") 
load("Dew_Data_20160120.RData") 
load("Dew_Monitor_20160120.RData")


Temp_Monitor=rename(Temp_Monitor,Temp_Latitude=Latitude,Temp_Longitude=Longitude)
RH_Monitor=rename(RH_Monitor,RH_Latitude=Latitude,RH_Longitude=Longitude)
Dew_Monitor=rename(Dew_Monitor,Dew_Latitude=Latitude,Dew_Longitude=Longitude)

# Check whether monitor location is idential among RH, Dewp, and Temp monitor ID. (Indeed, it is)
temp1=full_join(Temp_Monitor,RH_Monitor,by='FIPSPOC') %>%
	full_join(Dew_Monitor,by='FIPSPOC')
filter(temp1,RH_Latitude!=Dew_Latitude)
rm(temp1)

## Extract CA data
yr_range=c(2002:2009)
Temp_CA_AQS=filter(Temp_AQS,substr(FIPSPOC,1,2)=='06',substr(Date,1,4) %in% yr_range)
RH_CA_AQS=filter(RH_AQS,substr(FIPSPOC,1,2)=='06',substr(Date,1,4) %in% yr_range)
Dew_CA_AQS=filter(Dew_AQS,substr(FIPSPOC,1,2)=='06',substr(Date,1,4) %in% yr_range)

## Merge Files
temp1=full_join(Temp_CA_AQS,RH_CA_AQS,by=c('FIPSPOC','Date')) %>%
	full_join(Dew_CA_AQS,by=c('FIPSPOC','Date')) %>%
	filter(!is.na(RH_Value|Dew_Value),!is.na(Temp_Value)) %>%
	filter(Temp_Value>0)

temp2=mutate(temp1,
	RH_Value=ifelse(!is.na(RH_Value),RH_Value,dewpoint.to.humidity(t=Temp_Value,dp=Dew_Value,temperature.metric="fahrenheit")),
	Dew_Value=ifelse(!is.na(Dew_Value),Dew_Value,humidity.to.dewpoint(t=Temp_Value,rh=RH_Value,temperature.metric="fahrenheit")))


temp3=mutate(temp2,Temp_Value_C=fahrenheit.to.celsius(Temp_Value),
		Dew_Value_C=fahrenheit.to.celsius(Dew_Value),
		AT=-2.653+0.994*Temp_Value_C+0.368*Dew_Value_C*Dew_Value_C,
		HI=heat.index(t=Temp_Value,rh=RH_Value,temperature.metric='fahrenheit',output.metric='celsius'))


##Check Frequency of each monitor 
## Criteria is first obs year is 2002 and last obs year is 2009
temp3$n=sequence(rle(temp3$FIPSPOC)$lengths)
First_Date=data.frame(temp3[!duplicated(temp3$FIPSPOC),c('FIPSPOC','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(temp3[!duplicated(temp3$FIPSPOC,fromLast=TRUE),c('FIPSPOC','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPSPOC') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
	filter(substr(FirstObsDate,1,4)=='2002',substr(LastObsDate,1,4)=='2009',Freq<1.25)


#Choose the closest monitor from 8 soure monitors
Monitor_Loc=filter(Temp_Monitor,FIPSPOC %in% Obs_List$FIPSPOC)

AllMon=read_excel("M:\\Atesgis\\Calfine_Data\\Exposure\\Monitor\\Raw Data\\PM25_not_speciated\\ARB website extract January 2014\\location.xls") %>%
	data.frame %>%
	filter(!is.na(Latitude),!is.na(Long.Degrees),!is.na(County),!is.na(Site)) %>%
	select(SITE=Site,Latitude,Longitude) 

MonList=read_excel("K:\\Research\\PMSource_Still\\Data\\species monitor IDs.xlsx") %>% 
	data.frame() %>%
	inner_join(AllMon,by='SITE')

earthDist <- function (lon1, lat1, lon2, lat2){
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- lon1 * rad
    b1 <- lat2 * rad
    b2 <- lon2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}

df=data.frame()
for (i in 1:8){
temp=MonList[i,] %>%
	merge(Monitor_Loc) %>%
	mutate(distance=earthDist(Longitude,Latitude,Temp_Longitude,Temp_Latitude)) %>%
	select(FIPSPOC,SITE,distance)
df=rbind(df,temp)
rm(temp)
}

df2=arrange(df,SITE,distance) %>%
	distinct(SITE) %>%
	filter(distance<=120)



Weather_Data=filter(temp3,FIPSPOC %in% df2$FIPSPOC) %>%
	inner_join(df2,by='FIPSPOC') %>%
	select(Weather_FIPS=FIPSPOC,SITE,Date,AT,HI,Temp_Value_C,Dew_Value_C,RH_Value,distance) %>%
	arrange(SITE,Date)

save(Weather_Data,file='K:\\Research\\PMSource_Still\\Data\\WeatherData_20160317.RData')

