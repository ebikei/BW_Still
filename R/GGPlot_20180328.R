x<-c('dplyr','ggplot2')
lapply(x, require, character.only=T)


setwd('C:\\temp\\fetaldeath')

result2=read.csv('Logistic_Result_20160322.csv')

ggplot(data=result2,aes(x=Pol,y=PE)) +
	geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.667)+
	geom_point(aes(),color='red')+
	geom_hline(yintercept=1,lty=2,color='Black')+
	scale_x_discrete(name="Pollutant")+
	scale_y_continuous(name="Effect Estimate")+
	coord_flip()+
	ggtitle('Estimated RR per IQR increment')+	
	theme_bw()		

Source=c("totpm25","secsulf","secnit","vehic","biomass","soil","cln","fsalt",
"asalt","cusmelt","niind","ind")
Source2=c("totpm25","secsulf","secnit","vehic","biomass","soil","cln","fsalt",
"asalt")

result3=filter(result2,Pol %in% Source2)  
result3$Pol=factor(result3$Pol,levels=unique(as.character(result3$Pol)))

ggplot(data=result3,aes(x=Pol,y=PE)) +
	geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.667)+
	geom_point(aes(),color='red',size=3)+
	geom_hline(yintercept=1,lty=2,color='Black')+
	scale_x_discrete(name="Pollutant")+
	scale_y_continuous(name="RR")+
	coord_flip()+
	ggtitle('Estimated RR per IQR increment')+		
	theme_bw()		

result4=filter(result2,!(Pol %in% Source))  
result4$Pol=factor(result4$Pol,levels=unique(as.character(result4$Pol)))

ggplot(data=result4,aes(x=Pol,y=PE)) +
	geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.667)+
	geom_point(aes(),color='red')+
	geom_hline(yintercept=1,lty=2,color='Black')+
	scale_x_discrete(name="Pollutant")+
	scale_y_continuous(name="RR")+
	coord_flip()+
	ggtitle('Estimated RR per IQR increment')+		
	theme_bw()		


#############################################
#############################################
#############################################

result5=filter(result2,Pol %in% Source2)  
result5$index=c(3,2,7,6,9,1,4,5,8)
result5=arrange(result5,index)
yr=c(1,3:10)
PE=result5$PE
LCI=result5$LCI
UCI=result5$UCI

plot(yr,PE,xlim=c(1,11),ylim=c(0.95,1.15),type='n',main='')
points(yr,PE,pch=16)
segments(yr,LCI,yr,UCI,lwd=2)
axis(2)
abline(h=1)

plot()
