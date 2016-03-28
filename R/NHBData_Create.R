
x<-c('dplyr','data.table')
lapply(x, require, character.only=T)

setwd('K:\\Research\\PMSource_Still\\Data')

df=read.csv('ACS_11_5YR_S2201_with_ann.csv',skip=1,header=FALSE, stringsAsFactors=FALSE)
df2=df[-1,]
df3=select(df2,V2,V4,V6,V22,V46,V82,V94,V106) %>%
	mutate_each(funs(as.numeric), c(V4,V6,V22,V46,V82,V94,V106))

## V2: ZCTA5
## V4: Total Household Number
## V6: Household Number receiving Food Stamps
## V22: Below Poeverty Line
## V46: Rate of African American
## V82: Rate of Hispanic 
## V94: Median Income
## V106: No Work in past 12 months

colnames(df3)=c('ZCTA5','N_TotalHouse','N_FoodStamp','Bewlo_Pov','NBH_AA_Perc','NBH_His_Perc','NBH_MedIncome','NBH_NoWork')


df4=read.csv('ACS_11_5YR_S1501_with_ann.csv',skip=1,header=FALSE, stringsAsFactors=FALSE)
df5=df4[-1,]
df6=select(df5,V2,V40,V46,V52,V58,V64,V70,V76) %>%
	mutate_each(funs(as.numeric), c(V40,V46,V52,V58,V64,V70,V76))
## V2: ZCTA5
## V40: Less than 9th grade (%)
## V46: 9-12 no diploma (%)
## V52  HS (%)
## V58  Some college (%)
## V64  Associate Degree (%)
## V70  College Degree (%)
## V76  Grad Degree (%)
colnames(df6)=c('ZCTA5','Per_LT9','Per_9to12','Per_HS','Per_SC','Per_AD','Per_Col','Per_Grad')

df7=merge(df3,df6,by='ZCTA5') %>%
	mutate(FoodStamp_Rate=N_FoodStamp/N_TotalHouse,Bewlo_Pov_Rate=Bewlo_Pov/100,NBH_AA_Rate=NBH_AA_Perc/100,
		NBH_His_Rate=NBH_His_Perc/100,NBH_NoWork_Rate=NBH_NoWork/100,NBH_LHS_Rate=(Per_LT9+Per_9to12)/100,
		NBH_HS_Rate=Per_HS/100) %>%
	select(ZCTA5,NBH_MedIncome,FoodStamp_Rate,Bewlo_Pov_Rate,NBH_NoWork_Rate,NBH_LHS_Rate,NBH_HS_Rate,NBH_AA_Rate,NBH_His_Rate)

save(df7,file='NBH_Data.RData')
 
