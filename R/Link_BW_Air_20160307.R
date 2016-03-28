
x<-c('dplyr','data.table','ggplot2','corrplot')
lapply(x, require, character.only=T)

setwd('C:\\temp\\fetaldeath')

load('GestAirData.RData') #GestAirData
load('GestWeatherData.RData') #GestWeatherData
GestData=rbind(GestAirData,GestWeatherData)

NameList=distinct(GestData,SITE_NAME) %>%
	select(SITE,SITE_NAME)

BWData=fread('BWData_20160307.csv') 
BWData=mutate(BWData,ID=seq(1:dim(BWData)[1]),
		LMP=as.Date(paste(paste('20',substr(lmp,6,7),sep=''),substr(lmp,3,5),substr(lmp,1,2),sep=''),format="%Y%b%d"))
setkey(BWData,SITE)

NameList=data.table(NameList)
setkey(NameList,SITE)
BWData=merge(BWData,NameList,by=c('SITE')) %>%
	arrange(SITE,ID,LMP,gestweeks)

BWData2=select(BWData,ID,SITE_NAME,LMP,gestweeks) %>%
	arrange(SITE_NAME,LMP,ID,gestweeks)
setkey(BWData2,SITE_NAME,LMP)

pol_list=as.character(unique(GestData$polname))

output=data.frame(unique((BWData2$ID))) 
colnames(output)='ID'
output=arrange(output,ID)

ptm=proc.time()
pb=txtProgressBar(min=1,max=length(pol_list), style = 3)
for (i in 1:length(pol_list)){
	tryCatch({
	Sys.sleep(0.1)
	Air_DF=filter(GestData,polname==pol_list[i]) %>%
		data.table()
	setkey(Air_DF,SITE_NAME,LMP)
	DF1=inner_join(BWData2,Air_DF,by=c('SITE_NAME','LMP'))
	DF2=mutate(DF1,temp=ifelse(gestweeks==20,GestWeek_20,
			ifelse(gestweeks==21,GestWeek_21,
			ifelse(gestweeks==22,GestWeek_22,
			ifelse(gestweeks==23,GestWeek_23,
			ifelse(gestweeks==24,GestWeek_24,
			ifelse(gestweeks==25,GestWeek_25,
			ifelse(gestweeks==26,GestWeek_26,
			ifelse(gestweeks==27,GestWeek_27,
			ifelse(gestweeks==28,GestWeek_28,
			ifelse(gestweeks==29,GestWeek_29,
			ifelse(gestweeks==30,GestWeek_30,
			ifelse(gestweeks==31,GestWeek_31,
			ifelse(gestweeks==32,GestWeek_32,
			ifelse(gestweeks==33,GestWeek_33,
			ifelse(gestweeks==34,GestWeek_34,
			ifelse(gestweeks==35,GestWeek_35,
			ifelse(gestweeks==36,GestWeek_36,
			ifelse(gestweeks==37,GestWeek_37,
			ifelse(gestweeks==38,GestWeek_38,
			ifelse(gestweeks==39,GestWeek_39,
			ifelse(gestweeks==40,GestWeek_40,
			ifelse(gestweeks==41,GestWeek_41,
			ifelse(gestweeks==42,GestWeek_42,
			ifelse(gestweeks==43,GestWeek_43,
			ifelse(gestweeks==44,GestWeek_44,
			ifelse(gestweeks==45,GestWeek_45,
			ifelse(gestweeks==46,GestWeek_46,
			ifelse(gestweeks==47,GestWeek_47,
			ifelse(gestweeks==48,GestWeek_48,
			ifelse(gestweeks==49,GestWeek_49,
			ifelse(gestweeks==50,GestWeek_50,
			NA)))))))))))))))))))))))))))))))) %>%
	arrange(ID) %>%
	select(ID,temp) %>%
	data.frame()
	colnames(DF2)[which(colnames(DF2)=='temp')]=pol_list[i]
	output=full_join(output,DF2,by=c('ID'))
	rm(DF1,DF2)
	setTxtProgressBar(pb,i)
	},error=function(e){})
}
proc.time()-ptm

output=data.table(output)
setkey(output,ID)
BWData3=inner_join(BWData,output,by='ID') 

save(BWData3,file='BWData_20160309.RData')


## Correlation Plot Note this is a case for simpson paradox
col1=colorRampPalette(c("blue","#007FFF","cyan", "white","yellow", "#FF7F00","red"))
temp=filter(BWData3,!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic)
M=cor(temp) 
corrplot(M,type="upper",tl.pos="tp",col=col1(10))
corrplot(M,add=TRUE, type="lower", method="number",col="black",diag=FALSE,tl.pos="n", cl.pos="n")

## One more Correlaiton including salt components
temp2=filter(BWData3,!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt,fsalt)
M2=cor(temp2) #Simpson Paradox
corrplot(M2,type="upper",tl.pos="tp",col=col1(10))
corrplot(M2,add=TRUE, type="lower",method="number",col="black",diag=FALSE,tl.pos="n", cl.pos="n")

test=group_by(BWData3,SITE) 
dplyr::summarize(test,cor(totpm25,biomass,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,secsulf,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,sufroad,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,secnit,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,vehic,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,soil,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,fsalt,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,asalt,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,niind,use='pairwise.complete.obs'))
dplyr::summarize(test,cor(totpm25,ind,use='pairwise.complete.obs'))


## Scatter plot (Show Simpson Paradox)
test=filter(BWData3,!is.na(totpm25),!is.na(biomass)) %>%
	sample_n(10000) %>%
	data.frame() 

title=substitute(paste('Correlation between ',PM[2.5], ' and Biomass Source'))
Y_title=substitute(paste(PM[2.5]))

ggplot(test,aes(x=biomass,y=totpm25))+
	geom_point(aes(colour=factor(SITE_NAME)))+
	ggtitle(title)+
	scale_y_continuous(Y_title)+
	#theme_bw()+
	theme(panel.background=element_rect(fill='black'))



rm(list=ls())
