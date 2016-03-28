x<-c('dplyr','data.table','ggplot2','corrplot')
lapply(x, require, character.only=T)

setwd('C:\\temp\\fetaldeath')
load('BWData_20160309.RData') #BWData3

col1=colorRampPalette(c("blue","#007FFF","cyan", "white","yellow", "#FF7F00","red"))

temp=filter(BWData3,!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic)

M=cor(temp) #Simpson Paradox
corrplot(M,type="upper",tl.pos="tp",col=col1(10))
corrplot(M,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")


# Include a few more sources even some of them are missing in some sites
temp2=filter(BWData3,!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt,fsalt)
M2=cor(temp2) #Simpson Paradox
corrplot(M2,type="upper",tl.pos="tp",col=col1(10))
corrplot(M2,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

### By Site
temp_BAK=filter(BWData3,SITE=='3146') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,fsalt) %>%
	cor()

temp_FRE=filter(BWData3,SITE=='3009') %>% 
	filter(!is.na(totpm25),!is.na(secnit),!is.na(vehic),!is.na(biomass)) %>%
	select(totpm25,biomass,secnit,vehic) %>%
	cor()

temp_ELC=filter(BWData3,SITE=='2327') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt,fsalt) %>%
	cor()

temp_LAX=filter(BWData3,SITE=='2899') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt,fsalt) %>%
	cor()

temp_RUB=filter(BWData3,SITE=='2596') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(fsalt),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt,fsalt) %>%
	cor()

temp_SAC=filter(BWData3,SITE=='2731') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt) %>%
	cor()

temp_SJO=filter(BWData3,SITE=='3661') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt) %>%
	cor()

temp_SVY=filter(BWData3,SITE=='2880') %>% 
	filter(!is.na(totpm25),!is.na(secsulf),!is.na(secnit),!is.na(vehic),!is.na(biomass),!is.na(soil),!is.na(asalt)) %>%
	select(totpm25,biomass,secnit,secsulf,soil,vehic,asalt) %>%
	cor()

par(mfrow=c(3,3))

corrplot(M,type="upper",tl.pos="tp",col=col1(10),title='All Site',mar = c(0,0,0.85,0))
corrplot(M,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_BAK,type="upper",tl.pos="tp",col=col1(10),title='BAK',mar = c(0,0,0.85,0))
corrplot(temp_BAK,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_FRE,type="upper",tl.pos="tp",col=col1(10),title='FRE',mar = c(0,0,0.85,0))
corrplot(temp_FRE,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_ELC,type="upper",tl.pos="tp",col=col1(10),title='ELC',mar = c(0,0,0.85,0))
corrplot(temp_ELC,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_LAX,type="upper",tl.pos="tp",col=col1(10),title='LAX',mar = c(0,0,0.85,0))
corrplot(temp_LAX,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_RUB,type="upper",tl.pos="tp",col=col1(10),title='RUB',mar = c(0,0,0.85,0))
corrplot(temp_RUB,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_SAC,type="upper",tl.pos="tp",col=col1(10),title='SAC',mar = c(0,0,0.85,0))
corrplot(temp_SAC,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_SJO,type="upper",tl.pos="tp",col=col1(10),title='SJO',mar = c(0,0,0.85,0))
corrplot(temp_SJO,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")

corrplot(temp_SVY,type="upper",tl.pos="tp",col=col1(10),title='SVY',mar = c(0,0,0.85,0))
corrplot(temp_SVY,add=TRUE, type="lower", method="number", col="black",
	diag=FALSE,tl.pos="n", cl.pos="n")
