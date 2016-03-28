
x<-c('dplyr','data.table','ggplot2','corrplot')
lapply(x, require, character.only=T)

setwd('C:\\temp\\fetaldeath')

load('BWData_20160309.RData')

#Check Missing Variable
table(BWData3$sex,useNA='always')
table(BWData3$momsEthnic,useNA='always')
table(BWData3$ageOfMom,useNA='always')
table(BWData3$momsEducatn,useNA='always')
table(BWData3$tobacco,useNA='always')

BWData4=filter(BWData3,!is.na(ageOfMom),!is.na(sex)) # %>%
#	filter(gestweeks>33)

BWData4$sex=factor(BWData4$sex,levels=c(1,2),labels=c('male','female'))
BWData4$momsEthnic[is.na(BWData4$momsEthnic)]=6
BWData4$MotherRace=factor(BWData4$momsEthnic,levels=c(1,2,3,4,5,6),labels=c('White','Black','Hispanic','Asian','Other','Missing'))
BWData4$MotherAge=factor(cut(BWData4$ageOfMom,c(10,20,25,30,35,40,60),right=FALSE,include.lowest=TRUE),
	labels=c('<20','20-24','25-29','30-34','35-39','Over 40'))
BWData4$momsEducatn[is.na(BWData4$momsEducatn)]=99
BWData4$MotherEduc=factor(cut(BWData4$momsEducatn,c(0,12,13,16,26,99),right=FALSE,include.lowest=TRUE),
	labels=c('Less than HS','HS','Some College','College','Missing'))
BWData4$Smoking=factor(BWData4$tobacco,levels=c(0,1),labels=c('No_Smoke','Smoke'))
BWData4$delivyear=as.factor(BWData4$delivyear)

# Other things to put ; Order, delivery method,care begun, gest length.

pol_list=names(BWData4)[107:159]
pol_list=pol_list[-3]

temp2=list()

pb=txtProgressBar(min=1,max=length(pol_list),style=3)
ptm=proc.time()

for (i in 1:length(pol_list)){
	Sys.sleep(0.1)
	pol=pol_list[i]
	model=as.formula(paste('FetalDeath~',pol_list[i],'+sex+MotherRace+MotherAge+MotherEduc+Smoking+HI+delivyear',sep=''))
	model_out=summary(glm(model,data=BWData4,family=binomial(link='logit')))
	temp=select_(BWData4,pol)
	iqr=as.numeric(quantile(temp,0.75,na.rm=TRUE)-quantile(temp,0.25,na.rm=TRUE))
	N=model_out$df.null
	temp2[[i]]=c(model_out$coefficients[2,],N,iqr)
	setTxtProgressBar(pb,i)
}
proc.time()-ptm

result=do.call('rbind',temp2) %>%
	data.frame()
result$pol=pol_list
colnames(result)=c('Est','SE','Z_val','P_val','N','IQR','Pol')

result2=result %>%
	mutate(PE=round(exp(Est*IQR),3),LCI=round(exp((Est-1.96*SE)*IQR),3),UCI=round(exp((Est+1.96*SE)*IQR),3),CI=paste(' (',LCI,',',UCI,')',sep='')) %>%
	select(Pol,N,IQR,PE,CI,round(P_val,5),LCI,UCI) %>%
	arrange(PE)
result2
write.csv(result2,file='Logistic_Result_20160322.csv')
