par(mar=c(5, 4, 4, 2) + 0.1)
rm(list=ls())

library(gdata)
library(epicalc)
library(grDevices)
library(lattice)
library(grid)
library(scatterplot3d)
library(arm)
library(reldist)
library(ineq)
library(Hmisc)
library(lawstat)
library(car)
library(gplots)
library(latticeExtra)
library(VIM)
library(reshape)
library(psych)
library(optimx)

#### Cargando bases de datos 
inequality<-read.csv("pahowbdfinal.csv")
##health_inequality<-read.csv("Healthindicators.csv")

#inequality1<-subset(inequality,inequality$country!="Suriname" & inequality$country!="Cuba" & inequality$country!="Guyana" & inequality$year>1994)
inequality1<-subset(inequality,inequality$country!="Suriname" & inequality$country!="Cuba" & inequality$country!="Guyana" & inequality$year>1994)
inequality1<-subset(inequality1,inequality1$year>1994 & inequality1$year<2012)
inequality1$country <- factor(inequality1$country,labels = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Dominican Republic",
                                                             "Ecuador","El Salvador","Guatemala ","Haiti","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru","Trinidad and Tobago","Uruguay","Venezuela"))


df=data.frame(id=1:nrow(inequality1),year=inequality1$year,country=inequality1$country,country_code=inequality1$country_code,population=inequality1$poblacion_total,
              wb_income_group=inequality1$wb_income_group,wb_income_group_code=inequality1$wb_income_group_code,
              gdp=inequality1$GDP_per_capita_PPP_constant_2005_international, gni=inequality1$GNI_percapita_PPP_constant_2005_international,p.hex.gdp=(inequality1$Health_expenditure_percapita_PPP_constant_2005_international/inequality1$GDP_per_capita_PPP_constant_2005_international),
              hexp=inequality1$Health_expenditure_percapita_PPP_constant_2005_international,
              ir_tb=inequality1$tb_incidence,t_mort=inequality1$rate_deaths_hiv_neg_tb_cases,t_jail=inequality1$tasa_encarcelamiento_100k,
              urbgrow=inequality1$Urban_population_growth,phiv=inequality1$vih_prevalence,
              impsfac=inequality1$esperanza_vida_nacer_anos,impswt=inequality1$Proporcion_poblacion_usa_fuentes_mejoradas_agua_potable,
              impsfac=inequality1$proporcion_poblacion_utiliza_instalaciones_mejoradas_saneamiento,
              bcg=inequality1$p_poblacion_menores_1ano_inmunizada_tuberculosis,unemploy=inequality1$prop_desem_fuerza_trabajo,
              AIDS=inequality1$tasa_incidencia_SIDA_por_100000_hab,p_hexp_gdp=inequality1$Health_expenditure_percapita_PPP_constant_2005_international/inequality1$GDP_per_capita_PPP_constant_2005_international,
              pnotified=inequality1$captacion, num_tb_cases=inequality1$num_incid_tb_case)

legend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Dominican Republic",
           "Ecuador","El Salvador","Guatemala ","Haiti","Honduras","Mexico","Nicaragua",
           "Panama","Paraguay","Peru","Trinidad and Tobago","Uruguay","Venezuela")


##### Health inequality for GDP #####

gdp<-df[, c("country","year","population","gdp","num_tb_cases","ir_tb")]
gdp2010<-as.data.frame(gdp[gdp$year=="2010",])
gdp2010sort<-gdp2010[order(gdp2010$gdp),]
totalp=sum(gdp2010sort$population)
totaltb=sum(gdp2010sort$num_tb_cases)
gdp2010sort$Wpop<-gdp2010sort$population/totalp
gdp2010sort$CWpop<-cumsum(gdp2010sort$Wpop)
gdp2010sort$ridit<-c((0+gdp2010sort$CWpop[1])/2,(gdp2010sort$CWpop[1]+gdp2010sort$CWpop[2])/2,
                      (gdp2010sort$CWpop[2]+gdp2010sort$CWpop[3])/2,(gdp2010sort$CWpop[3]+gdp2010sort$CWpop[4])/2,
                      (gdp2010sort$CWpop[4]+gdp2010sort$CWpop[5])/2,(gdp2010sort$CWpop[5]+gdp2010sort$CWpop[6])/2,
                      (gdp2010sort$CWpop[6]+gdp2010sort$CWpop[7])/2,(gdp2010sort$CWpop[7]+gdp2010sort$CWpop[8])/2,
                      (gdp2010sort$CWpop[8]+gdp2010sort$CWpop[9])/2,(gdp2010sort$CWpop[9]+gdp2010sort$CWpop[10])/2,
                      (gdp2010sort$CWpop[10]+gdp2010sort$CWpop[11])/2,(gdp2010sort$CWpop[11]+gdp2010sort$CWpop[12])/2,
                      (gdp2010sort$CWpop[12]+gdp2010sort$CWpop[13])/2,(gdp2010sort$CWpop[13]+gdp2010sort$CWpop[14])/2,
                      (gdp2010sort$CWpop[14]+gdp2010sort$CWpop[15])/2,(gdp2010sort$CWpop[15]+gdp2010sort$CWpop[16])/2,
                      (gdp2010sort$CWpop[16]+gdp2010sort$CWpop[17])/2,(gdp2010sort$CWpop[17]+gdp2010sort$CWpop[18])/2,
                      (gdp2010sort$CWpop[18]+gdp2010sort$CWpop[19])/2,(gdp2010sort$CWpop[19]+gdp2010sort$CWpop[20])/2)
gdp2010sort$Whealth<-gdp2010sort$num_tb_cases/totaltb
gdp2010sort$CWhealth<-cumsum(gdp2010sort$Whealth)
gdp2010sort$logridit<-log10(gdp2010sort$ridit)
gdp2010sort$Wi<-sqrt(gdp2010sort$population)
gdp2010sort$XiWi<-gdp2010sort$Wi*gdp2010sort$logridit
gdp2010sort$YiWi<-gdp2010sort$Wi*gdp2010sort$ir_tb
fit2010<-lm(gdp2010sort$YiWi~gdp2010sort$Wi + gdp2010sort$XiWi + 0)
summary(fit2010)
gdp2010sort$predict2010<-coef(summary(fit2010))[1,1] + coef(summary(fit2010))[2,1]*gdp2010sort$logridit

gdp2005<-as.data.frame(gdp[gdp$year=="2005",])
gdp2005sort<-gdp2005[order(gdp2005$gdp),]
totalp=sum(gdp2005sort$population)
totaltb=sum(gdp2005sort$num_tb_cases)
gdp2005sort$Wpop<-gdp2005sort$population/totalp
gdp2005sort$CWpop<-cumsum(gdp2005sort$Wpop)
gdp2005sort$ridit<-c((0+gdp2005sort$CWpop[1])/2,(gdp2005sort$CWpop[1]+gdp2005sort$CWpop[2])/2,
                      (gdp2005sort$CWpop[2]+gdp2005sort$CWpop[3])/2,(gdp2005sort$CWpop[3]+gdp2005sort$CWpop[4])/2,
                      (gdp2005sort$CWpop[4]+gdp2005sort$CWpop[5])/2,(gdp2005sort$CWpop[5]+gdp2005sort$CWpop[6])/2,
                      (gdp2005sort$CWpop[6]+gdp2005sort$CWpop[7])/2,(gdp2005sort$CWpop[7]+gdp2005sort$CWpop[8])/2,
                      (gdp2005sort$CWpop[8]+gdp2005sort$CWpop[9])/2,(gdp2005sort$CWpop[9]+gdp2005sort$CWpop[10])/2,
                      (gdp2005sort$CWpop[10]+gdp2005sort$CWpop[11])/2,(gdp2005sort$CWpop[11]+gdp2005sort$CWpop[12])/2,
                      (gdp2005sort$CWpop[12]+gdp2005sort$CWpop[13])/2,(gdp2005sort$CWpop[13]+gdp2005sort$CWpop[14])/2,
                      (gdp2005sort$CWpop[14]+gdp2005sort$CWpop[15])/2,(gdp2005sort$CWpop[15]+gdp2005sort$CWpop[16])/2,
                      (gdp2005sort$CWpop[16]+gdp2005sort$CWpop[17])/2,(gdp2005sort$CWpop[17]+gdp2005sort$CWpop[18])/2,
                      (gdp2005sort$CWpop[18]+gdp2005sort$CWpop[19])/2,(gdp2005sort$CWpop[19]+gdp2005sort$CWpop[20])/2)
gdp2005sort$Whealth<-gdp2005sort$num_tb_cases/totaltb
gdp2005sort$CWhealth<-cumsum(gdp2005sort$Whealth)
gdp2005sort$logridit<-log10(gdp2005sort$ridit)
gdp2005sort$Wi<-sqrt(gdp2005sort$population)
gdp2005sort$XiWi<-gdp2005sort$Wi*gdp2005sort$logridit
gdp2005sort$YiWi<-gdp2005sort$Wi*gdp2005sort$ir_tb
fit2005<-lm(gdp2005sort$YiWi~gdp2005sort$Wi + gdp2005sort$XiWi + 0)
summary(fit2005)
gdp2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*gdp2005sort$logridit

gdp2000<-as.data.frame(gdp[gdp$year=="2000",])
gdp2000sort<-gdp2000[order(gdp2000$gdp),]
totalp=sum(gdp2000sort$population)
totaltb=sum(gdp2000sort$num_tb_cases)
gdp2000sort$Wpop<-gdp2000sort$population/totalp
gdp2000sort$CWpop<-cumsum(gdp2000sort$Wpop)
gdp2000sort$ridit<-c((0+gdp2000sort$CWpop[1])/2,(gdp2000sort$CWpop[1]+gdp2000sort$CWpop[2])/2,
                      (gdp2000sort$CWpop[2]+gdp2000sort$CWpop[3])/2,(gdp2000sort$CWpop[3]+gdp2000sort$CWpop[4])/2,
                      (gdp2000sort$CWpop[4]+gdp2000sort$CWpop[5])/2,(gdp2000sort$CWpop[5]+gdp2000sort$CWpop[6])/2,
                      (gdp2000sort$CWpop[6]+gdp2000sort$CWpop[7])/2,(gdp2000sort$CWpop[7]+gdp2000sort$CWpop[8])/2,
                      (gdp2000sort$CWpop[8]+gdp2000sort$CWpop[9])/2,(gdp2000sort$CWpop[9]+gdp2000sort$CWpop[10])/2,
                      (gdp2000sort$CWpop[10]+gdp2000sort$CWpop[11])/2,(gdp2000sort$CWpop[11]+gdp2000sort$CWpop[12])/2,
                      (gdp2000sort$CWpop[12]+gdp2000sort$CWpop[13])/2,(gdp2000sort$CWpop[13]+gdp2000sort$CWpop[14])/2,
                      (gdp2000sort$CWpop[14]+gdp2000sort$CWpop[15])/2,(gdp2000sort$CWpop[15]+gdp2000sort$CWpop[16])/2,
                      (gdp2000sort$CWpop[16]+gdp2000sort$CWpop[17])/2,(gdp2000sort$CWpop[17]+gdp2000sort$CWpop[18])/2,
                      (gdp2000sort$CWpop[18]+gdp2000sort$CWpop[19])/2,(gdp2000sort$CWpop[19]+gdp2000sort$CWpop[20])/2)

gdp2000sort$Whealth<-gdp2000sort$num_tb_cases/totaltb
gdp2000sort$CWhealth<-cumsum(gdp2000sort$Whealth)
gdp2000sort$logridit<-log10(gdp2000sort$ridit)
gdp2000sort$Wi<-sqrt(gdp2000sort$population)
gdp2000sort$XiWi<-gdp2000sort$Wi*gdp2000sort$logridit
gdp2000sort$YiWi<-gdp2000sort$Wi*gdp2000sort$ir_tb
fit2000<-lm(gdp2000sort$YiWi~gdp2000sort$Wi + gdp2000sort$XiWi + 0)
summary(fit2000)
gdp2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*gdp2000sort$logridit

quartz(width=10, height=6, pointsize=10)
plot(gdp2010sort$ridit,gdp2010sort$ir_tb, col="black",pch=0,
     ylab="Tuberculosis incidence rates per 100,000 population", 
     xlab="Country-level population gradient defined by GDP")
points(gdp2005sort$ridit,gdp2005sort$ir_tb, col="black",pch=1,
       ylab="", 
       xlab="")
points(gdp2000sort$ridit,gdp2000sort$ir_tb, col="black",pch=2,
       ylab="", 
       xlab="")
lines(gdp2010sort$ridit,gdp2010sort$predict2010, col="black", lty=1,
      ylab="", 
      xlab="")
lines(gdp2005sort$ridit,gdp2005sort$predict2005, col="black", lty=2,
      ylab="", 
      xlab="")
lines(gdp2000sort$ridit,gdp2000sort$predict2000, col="black", lty=3,
      ylab="", 
      xlab="")
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)

# Social gradient
slope_index_of_inequality_gdp2000<-fit2000$coefficients[2]
slope_index_of_inequality_gdp2005<-fit2005$coefficients[2]
slope_index_of_inequality_gdp2010<-fit2010$coefficients[2]
round(slope_index_of_inequality_gdp2000,2)
round(slope_index_of_inequality_gdp2005,2)
round(slope_index_of_inequality_gdp2010,2)



##### Health inequality for Health expenditure per capita#####

hexp<-df[, c("country","year","population","hexp","num_tb_cases","ir_tb")]
hexp2010<-as.data.frame(hexp[hexp$year=="2010",])
hexp2010sort<-hexp2010[order(hexp2010$hexp),]
totalp=sum(hexp2010sort$population)
totaltb=sum(hexp2010sort$num_tb_cases)
hexp2010sort$Wpop<-hexp2010sort$population/totalp
hexp2010sort$CWpop<-cumsum(hexp2010sort$Wpop)
hexp2010sort$ridit<-c((0+hexp2010sort$CWpop[1])/2,(hexp2010sort$CWpop[1]+hexp2010sort$CWpop[2])/2,
											(hexp2010sort$CWpop[2]+hexp2010sort$CWpop[3])/2,(hexp2010sort$CWpop[3]+hexp2010sort$CWpop[4])/2,
											(hexp2010sort$CWpop[4]+hexp2010sort$CWpop[5])/2,(hexp2010sort$CWpop[5]+hexp2010sort$CWpop[6])/2,
											(hexp2010sort$CWpop[6]+hexp2010sort$CWpop[7])/2,(hexp2010sort$CWpop[7]+hexp2010sort$CWpop[8])/2,
											(hexp2010sort$CWpop[8]+hexp2010sort$CWpop[9])/2,(hexp2010sort$CWpop[9]+hexp2010sort$CWpop[10])/2,
											(hexp2010sort$CWpop[10]+hexp2010sort$CWpop[11])/2,(hexp2010sort$CWpop[11]+hexp2010sort$CWpop[12])/2,
											(hexp2010sort$CWpop[12]+hexp2010sort$CWpop[13])/2,(hexp2010sort$CWpop[13]+hexp2010sort$CWpop[14])/2,
											(hexp2010sort$CWpop[14]+hexp2010sort$CWpop[15])/2,(hexp2010sort$CWpop[15]+hexp2010sort$CWpop[16])/2,
											(hexp2010sort$CWpop[16]+hexp2010sort$CWpop[17])/2,(hexp2010sort$CWpop[17]+hexp2010sort$CWpop[18])/2,
											(hexp2010sort$CWpop[18]+hexp2010sort$CWpop[19])/2,(hexp2010sort$CWpop[19]+hexp2010sort$CWpop[20])/2)
hexp2010sort$Whealth<-hexp2010sort$num_tb_cases/totaltb
hexp2010sort$CWhealth<-cumsum(hexp2010sort$Whealth)
hexp2010sort$logridit<-log10(hexp2010sort$ridit)
hexp2010sort$Wi<-sqrt(hexp2010sort$population)
hexp2010sort$XiWi<-hexp2010sort$Wi*hexp2010sort$logridit
hexp2010sort$YiWi<-hexp2010sort$Wi*hexp2010sort$ir_tb
fit2010<-lm(hexp2010sort$YiWi~hexp2010sort$Wi + hexp2010sort$XiWi + 0)
summary(fit2010)
hexp2010sort$predict2010<-coef(summary(fit2010))[1,1] + coef(summary(fit2010))[2,1]*hexp2010sort$logridit

hexp2005<-as.data.frame(hexp[hexp$year=="2005",])
hexp2005sort<-hexp2005[order(hexp2005$hexp),]
totalp=sum(hexp2005sort$population)
totaltb=sum(hexp2005sort$num_tb_cases)
hexp2005sort$Wpop<-hexp2005sort$population/totalp
hexp2005sort$CWpop<-cumsum(hexp2005sort$Wpop)
hexp2005sort$ridit<-c((0+hexp2005sort$CWpop[1])/2,(hexp2005sort$CWpop[1]+hexp2005sort$CWpop[2])/2,
											(hexp2005sort$CWpop[2]+hexp2005sort$CWpop[3])/2,(hexp2005sort$CWpop[3]+hexp2005sort$CWpop[4])/2,
											(hexp2005sort$CWpop[4]+hexp2005sort$CWpop[5])/2,(hexp2005sort$CWpop[5]+hexp2005sort$CWpop[6])/2,
											(hexp2005sort$CWpop[6]+hexp2005sort$CWpop[7])/2,(hexp2005sort$CWpop[7]+hexp2005sort$CWpop[8])/2,
											(hexp2005sort$CWpop[8]+hexp2005sort$CWpop[9])/2,(hexp2005sort$CWpop[9]+hexp2005sort$CWpop[10])/2,
											(hexp2005sort$CWpop[10]+hexp2005sort$CWpop[11])/2,(hexp2005sort$CWpop[11]+hexp2005sort$CWpop[12])/2,
											(hexp2005sort$CWpop[12]+hexp2005sort$CWpop[13])/2,(hexp2005sort$CWpop[13]+hexp2005sort$CWpop[14])/2,
											(hexp2005sort$CWpop[14]+hexp2005sort$CWpop[15])/2,(hexp2005sort$CWpop[15]+hexp2005sort$CWpop[16])/2,
											(hexp2005sort$CWpop[16]+hexp2005sort$CWpop[17])/2,(hexp2005sort$CWpop[17]+hexp2005sort$CWpop[18])/2,
											(hexp2005sort$CWpop[18]+hexp2005sort$CWpop[19])/2,(hexp2005sort$CWpop[19]+hexp2005sort$CWpop[20])/2)
hexp2005sort$Whealth<-hexp2005sort$num_tb_cases/totaltb
hexp2005sort$CWhealth<-cumsum(hexp2005sort$Whealth)
hexp2005sort$logridit<-log10(hexp2005sort$ridit)
hexp2005sort$Wi<-sqrt(hexp2005sort$population)
hexp2005sort$XiWi<-hexp2005sort$Wi*hexp2005sort$logridit
hexp2005sort$YiWi<-hexp2005sort$Wi*hexp2005sort$ir_tb
fit2005<-lm(hexp2005sort$YiWi~hexp2005sort$Wi + hexp2005sort$XiWi + 0)
summary(fit2005)
hexp2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*hexp2005sort$logridit

hexp2000<-as.data.frame(hexp[hexp$year=="2000",])
hexp2000sort<-hexp2000[order(hexp2000$hexp),]
totalp=sum(hexp2000sort$population)
totaltb=sum(hexp2000sort$num_tb_cases)
hexp2000sort$Wpop<-hexp2000sort$population/totalp
hexp2000sort$CWpop<-cumsum(hexp2000sort$Wpop)
hexp2000sort$ridit<-c((0+hexp2000sort$CWpop[1])/2,(hexp2000sort$CWpop[1]+hexp2000sort$CWpop[2])/2,
											(hexp2000sort$CWpop[2]+hexp2000sort$CWpop[3])/2,(hexp2000sort$CWpop[3]+hexp2000sort$CWpop[4])/2,
											(hexp2000sort$CWpop[4]+hexp2000sort$CWpop[5])/2,(hexp2000sort$CWpop[5]+hexp2000sort$CWpop[6])/2,
											(hexp2000sort$CWpop[6]+hexp2000sort$CWpop[7])/2,(hexp2000sort$CWpop[7]+hexp2000sort$CWpop[8])/2,
											(hexp2000sort$CWpop[8]+hexp2000sort$CWpop[9])/2,(hexp2000sort$CWpop[9]+hexp2000sort$CWpop[10])/2,
											(hexp2000sort$CWpop[10]+hexp2000sort$CWpop[11])/2,(hexp2000sort$CWpop[11]+hexp2000sort$CWpop[12])/2,
											(hexp2000sort$CWpop[12]+hexp2000sort$CWpop[13])/2,(hexp2000sort$CWpop[13]+hexp2000sort$CWpop[14])/2,
											(hexp2000sort$CWpop[14]+hexp2000sort$CWpop[15])/2,(hexp2000sort$CWpop[15]+hexp2000sort$CWpop[16])/2,
											(hexp2000sort$CWpop[16]+hexp2000sort$CWpop[17])/2,(hexp2000sort$CWpop[17]+hexp2000sort$CWpop[18])/2,
											(hexp2000sort$CWpop[18]+hexp2000sort$CWpop[19])/2,(hexp2000sort$CWpop[19]+hexp2000sort$CWpop[20])/2)

hexp2000sort$Whealth<-hexp2000sort$num_tb_cases/totaltb
hexp2000sort$CWhealth<-cumsum(hexp2000sort$Whealth)
hexp2000sort$logridit<-log10(hexp2000sort$ridit)
hexp2000sort$Wi<-sqrt(hexp2000sort$population)
hexp2000sort$XiWi<-hexp2000sort$Wi*hexp2000sort$logridit
hexp2000sort$YiWi<-hexp2000sort$Wi*hexp2000sort$ir_tb
fit2000<-lm(hexp2000sort$YiWi~hexp2000sort$Wi + hexp2000sort$XiWi + 0)
summary(fit2000)
hexp2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*hexp2000sort$logridit

quartz(width=10, height=6, pointsize=10)
plot(hexp2010sort$ridit,hexp2010sort$ir_tb, col="black",pch=0,
		 ylab="Tuberculosis incidence rates per 100,000 population", 
		 xlab="Country-level population gradient defined by total health expenditure per capita")
points(hexp2005sort$ridit,hexp2005sort$ir_tb, col="black",pch=1,
		 ylab="", 
		 xlab="")
points(hexp2000sort$ridit,hexp2000sort$ir_tb, col="black",pch=2,
		 ylab="", 
		 xlab="")
lines(hexp2010sort$ridit,hexp2010sort$predict2010, col="black", lty=1,
		 ylab="", 
		 xlab="")
lines(hexp2005sort$ridit,hexp2005sort$predict2005, col="black", lty=2,
		 ylab="", 
		 xlab="")
lines(hexp2000sort$ridit,hexp2000sort$predict2000, col="black", lty=3,
		 ylab="", 
		 xlab="")
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)

# Social gradient
slope_index_of_inequality_hexp2000<-fit2000$coefficients[2]
slope_index_of_inequality_hexp2005<-fit2005$coefficients[2]
slope_index_of_inequality_hexp2010<-fit2010$coefficients[2]
round(slope_index_of_inequality_hexp2000,2)
round(slope_index_of_inequality_hexp2005,2)
round(slope_index_of_inequality_hexp2010,2)

##### Health inequality for Access to improved sanitation facilities  #####

impsfac<-df[, c("country","year","population","impsfac","num_tb_cases","ir_tb")]
impsfac2010<-as.data.frame(impsfac[impsfac$year=="2010",])
impsfac2010sort<-impsfac2010[order(impsfac2010$impsfac),]
totalp=sum(impsfac2010sort$population)
totaltb=sum(impsfac2010sort$num_tb_cases)
impsfac2010sort$Wpop<-impsfac2010sort$population/totalp
impsfac2010sort$CWpop<-cumsum(impsfac2010sort$Wpop)
impsfac2010sort$ridit<-c((0+impsfac2010sort$CWpop[1])/2,(impsfac2010sort$CWpop[1]+impsfac2010sort$CWpop[2])/2,
											(impsfac2010sort$CWpop[2]+impsfac2010sort$CWpop[3])/2,(impsfac2010sort$CWpop[3]+impsfac2010sort$CWpop[4])/2,
											(impsfac2010sort$CWpop[4]+impsfac2010sort$CWpop[5])/2,(impsfac2010sort$CWpop[5]+impsfac2010sort$CWpop[6])/2,
											(impsfac2010sort$CWpop[6]+impsfac2010sort$CWpop[7])/2,(impsfac2010sort$CWpop[7]+impsfac2010sort$CWpop[8])/2,
											(impsfac2010sort$CWpop[8]+impsfac2010sort$CWpop[9])/2,(impsfac2010sort$CWpop[9]+impsfac2010sort$CWpop[10])/2,
											(impsfac2010sort$CWpop[10]+impsfac2010sort$CWpop[11])/2,(impsfac2010sort$CWpop[11]+impsfac2010sort$CWpop[12])/2,
											(impsfac2010sort$CWpop[12]+impsfac2010sort$CWpop[13])/2,(impsfac2010sort$CWpop[13]+impsfac2010sort$CWpop[14])/2,
											(impsfac2010sort$CWpop[14]+impsfac2010sort$CWpop[15])/2,(impsfac2010sort$CWpop[15]+impsfac2010sort$CWpop[16])/2,
											(impsfac2010sort$CWpop[16]+impsfac2010sort$CWpop[17])/2,(impsfac2010sort$CWpop[17]+impsfac2010sort$CWpop[18])/2,
											(impsfac2010sort$CWpop[18]+impsfac2010sort$CWpop[19])/2,(impsfac2010sort$CWpop[19]+impsfac2010sort$CWpop[20])/2)
impsfac2010sort$Whealth<-impsfac2010sort$num_tb_cases/totaltb
impsfac2010sort$CWhealth<-cumsum(impsfac2010sort$Whealth)
impsfac2010sort$logridit<-log10(impsfac2010sort$ridit)
impsfac2010sort$Wi<-sqrt(impsfac2010sort$population)
impsfac2010sort$XiWi<-impsfac2010sort$Wi*impsfac2010sort$logridit
impsfac2010sort$YiWi<-impsfac2010sort$Wi*impsfac2010sort$ir_tb
fit2010<-lm(impsfac2010sort$YiWi~impsfac2010sort$Wi + impsfac2010sort$XiWi + 0)
summary(fit2010)
impsfac2010sort$predict2010<-coef(summary(fit2010))[1,1] + coef(summary(fit2010))[2,1]*impsfac2010sort$logridit

impsfac2005<-as.data.frame(impsfac[impsfac$year=="2005",])
impsfac2005sort<-impsfac2005[order(impsfac2005$impsfac),]
totalp=sum(impsfac2005sort$population)
totaltb=sum(impsfac2005sort$num_tb_cases)
impsfac2005sort$Wpop<-impsfac2005sort$population/totalp
impsfac2005sort$CWpop<-cumsum(impsfac2005sort$Wpop)
impsfac2005sort$ridit<-c((0+impsfac2005sort$CWpop[1])/2,(impsfac2005sort$CWpop[1]+impsfac2005sort$CWpop[2])/2,
											(impsfac2005sort$CWpop[2]+impsfac2005sort$CWpop[3])/2,(impsfac2005sort$CWpop[3]+impsfac2005sort$CWpop[4])/2,
											(impsfac2005sort$CWpop[4]+impsfac2005sort$CWpop[5])/2,(impsfac2005sort$CWpop[5]+impsfac2005sort$CWpop[6])/2,
											(impsfac2005sort$CWpop[6]+impsfac2005sort$CWpop[7])/2,(impsfac2005sort$CWpop[7]+impsfac2005sort$CWpop[8])/2,
											(impsfac2005sort$CWpop[8]+impsfac2005sort$CWpop[9])/2,(impsfac2005sort$CWpop[9]+impsfac2005sort$CWpop[10])/2,
											(impsfac2005sort$CWpop[10]+impsfac2005sort$CWpop[11])/2,(impsfac2005sort$CWpop[11]+impsfac2005sort$CWpop[12])/2,
											(impsfac2005sort$CWpop[12]+impsfac2005sort$CWpop[13])/2,(impsfac2005sort$CWpop[13]+impsfac2005sort$CWpop[14])/2,
											(impsfac2005sort$CWpop[14]+impsfac2005sort$CWpop[15])/2,(impsfac2005sort$CWpop[15]+impsfac2005sort$CWpop[16])/2,
											(impsfac2005sort$CWpop[16]+impsfac2005sort$CWpop[17])/2,(impsfac2005sort$CWpop[17]+impsfac2005sort$CWpop[18])/2,
											(impsfac2005sort$CWpop[18]+impsfac2005sort$CWpop[19])/2,(impsfac2005sort$CWpop[19]+impsfac2005sort$CWpop[20])/2)
impsfac2005sort$Whealth<-impsfac2005sort$num_tb_cases/totaltb
impsfac2005sort$CWhealth<-cumsum(impsfac2005sort$Whealth)
impsfac2005sort$logridit<-log10(impsfac2005sort$ridit)
impsfac2005sort$Wi<-sqrt(impsfac2005sort$population)
impsfac2005sort$XiWi<-impsfac2005sort$Wi*impsfac2005sort$logridit
impsfac2005sort$YiWi<-impsfac2005sort$Wi*impsfac2005sort$ir_tb
fit2005<-lm(impsfac2005sort$YiWi~impsfac2005sort$Wi + impsfac2005sort$XiWi + 0)
summary(fit2005)
impsfac2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*impsfac2005sort$logridit

impsfac2000<-as.data.frame(impsfac[impsfac$year=="2000",])
impsfac2000sort<-impsfac2000[order(impsfac2000$impsfac),]
totalp=sum(impsfac2000sort$population)
totaltb=sum(impsfac2000sort$num_tb_cases)
impsfac2000sort$Wpop<-impsfac2000sort$population/totalp
impsfac2000sort$CWpop<-cumsum(impsfac2000sort$Wpop)
impsfac2000sort$ridit<-c((0+impsfac2000sort$CWpop[1])/2,(impsfac2000sort$CWpop[1]+impsfac2000sort$CWpop[2])/2,
											(impsfac2000sort$CWpop[2]+impsfac2000sort$CWpop[3])/2,(impsfac2000sort$CWpop[3]+impsfac2000sort$CWpop[4])/2,
											(impsfac2000sort$CWpop[4]+impsfac2000sort$CWpop[5])/2,(impsfac2000sort$CWpop[5]+impsfac2000sort$CWpop[6])/2,
											(impsfac2000sort$CWpop[6]+impsfac2000sort$CWpop[7])/2,(impsfac2000sort$CWpop[7]+impsfac2000sort$CWpop[8])/2,
											(impsfac2000sort$CWpop[8]+impsfac2000sort$CWpop[9])/2,(impsfac2000sort$CWpop[9]+impsfac2000sort$CWpop[10])/2,
											(impsfac2000sort$CWpop[10]+impsfac2000sort$CWpop[11])/2,(impsfac2000sort$CWpop[11]+impsfac2000sort$CWpop[12])/2,
											(impsfac2000sort$CWpop[12]+impsfac2000sort$CWpop[13])/2,(impsfac2000sort$CWpop[13]+impsfac2000sort$CWpop[14])/2,
											(impsfac2000sort$CWpop[14]+impsfac2000sort$CWpop[15])/2,(impsfac2000sort$CWpop[15]+impsfac2000sort$CWpop[16])/2,
											(impsfac2000sort$CWpop[16]+impsfac2000sort$CWpop[17])/2,(impsfac2000sort$CWpop[17]+impsfac2000sort$CWpop[18])/2,
											(impsfac2000sort$CWpop[18]+impsfac2000sort$CWpop[19])/2,(impsfac2000sort$CWpop[19]+impsfac2000sort$CWpop[20])/2)
											
impsfac2000sort$Whealth<-impsfac2000sort$num_tb_cases/totaltb
impsfac2000sort$CWhealth<-cumsum(impsfac2000sort$Whealth)
impsfac2000sort$logridit<-log10(impsfac2000sort$ridit)
impsfac2000sort$Wi<-sqrt(impsfac2000sort$population)
impsfac2000sort$XiWi<-impsfac2000sort$Wi*impsfac2000sort$logridit
impsfac2000sort$YiWi<-impsfac2000sort$Wi*impsfac2000sort$ir_tb
fit2000<-lm(impsfac2000sort$YiWi~impsfac2000sort$Wi + impsfac2000sort$XiWi + 0)
summary(fit2000)
impsfac2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*impsfac2000sort$logridit

quartz(width=10, height=6, pointsize=10)
plot(impsfac2010sort$ridit,impsfac2010sort$ir_tb, col="black", pch=0,
		 ylab="Tuberculosis incidence rates per 100,000 population", 
		 xlab="Country-level population gradient defined by access to improved sanitation facilities")
points(impsfac2005sort$ridit,impsfac2005sort$ir_tb, col="black",pch=1,
		 ylab="", 
		 xlab="")
points(impsfac2000sort$ridit,impsfac2000sort$ir_tb, col="black",pch=2,
		 ylab="", 
		 xlab="")
lines(impsfac2010sort$ridit,impsfac2010sort$predict2010, col="black",lty=1,
		 ylab="", 
		 xlab="")
lines(impsfac2005sort$ridit,impsfac2005sort$predict2005, col="black", lty=2,
		 ylab="", 
		 xlab="")
lines(impsfac2000sort$ridit,impsfac2000sort$predict2000, col="black", lty=3,
		 ylab="", 
		 xlab="")
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)

# Social gradient
slope_index_of_inequality_impsfac2000<-fit2000$coefficients[2]
slope_index_of_inequality_impsfac2005<-fit2005$coefficients[2]
slope_index_of_inequality_impsfac2010<-fit2010$coefficients[2]
round(slope_index_of_inequality_impsfac2000,2)
round(slope_index_of_inequality_impsfac2005,2)
round(slope_index_of_inequality_impsfac2010,2)


##### Health inequality for TB detection rate #####

pnotified<-df[, c("country","year","population","pnotified","num_tb_cases","ir_tb")]
pnotified2010<-as.data.frame(pnotified[pnotified$year=="2010",])
pnotified2010sort<-pnotified2010[order(pnotified2010$pnotified),]
totalp=sum(pnotified2010sort$population)
totaltb=sum(pnotified2010sort$num_tb_cases)
pnotified2010sort$Wpop<-pnotified2010sort$population/totalp
pnotified2010sort$CWpop<-cumsum(pnotified2010sort$Wpop)
pnotified2010sort$ridit<-c((0+pnotified2010sort$CWpop[1])/2,(pnotified2010sort$CWpop[1]+pnotified2010sort$CWpop[2])/2,
                      (pnotified2010sort$CWpop[2]+pnotified2010sort$CWpop[3])/2,(pnotified2010sort$CWpop[3]+pnotified2010sort$CWpop[4])/2,
                      (pnotified2010sort$CWpop[4]+pnotified2010sort$CWpop[5])/2,(pnotified2010sort$CWpop[5]+pnotified2010sort$CWpop[6])/2,
                      (pnotified2010sort$CWpop[6]+pnotified2010sort$CWpop[7])/2,(pnotified2010sort$CWpop[7]+pnotified2010sort$CWpop[8])/2,
                      (pnotified2010sort$CWpop[8]+pnotified2010sort$CWpop[9])/2,(pnotified2010sort$CWpop[9]+pnotified2010sort$CWpop[10])/2,
                      (pnotified2010sort$CWpop[10]+pnotified2010sort$CWpop[11])/2,(pnotified2010sort$CWpop[11]+pnotified2010sort$CWpop[12])/2,
                      (pnotified2010sort$CWpop[12]+pnotified2010sort$CWpop[13])/2,(pnotified2010sort$CWpop[13]+pnotified2010sort$CWpop[14])/2,
                      (pnotified2010sort$CWpop[14]+pnotified2010sort$CWpop[15])/2,(pnotified2010sort$CWpop[15]+pnotified2010sort$CWpop[16])/2,
                      (pnotified2010sort$CWpop[16]+pnotified2010sort$CWpop[17])/2,(pnotified2010sort$CWpop[17]+pnotified2010sort$CWpop[18])/2,
                      (pnotified2010sort$CWpop[18]+pnotified2010sort$CWpop[19])/2,(pnotified2010sort$CWpop[19]+pnotified2010sort$CWpop[20])/2)
pnotified2010sort$Whealth<-pnotified2010sort$num_tb_cases/totaltb
pnotified2010sort$CWhealth<-cumsum(pnotified2010sort$Whealth)
pnotified2010sort$logridit<-log10(pnotified2010sort$ridit)
pnotified2010sort$Wi<-sqrt(pnotified2010sort$population)
pnotified2010sort$XiWi<-pnotified2010sort$Wi*pnotified2010sort$logridit
pnotified2010sort$YiWi<-pnotified2010sort$Wi*pnotified2010sort$ir_tb
fit2010<-lm(pnotified2010sort$YiWi~pnotified2010sort$Wi + pnotified2010sort$XiWi + 0)
summary(fit2010)
pnotified2010sort$predict2010<-coef(summary(fit2010))[1,1] + coef(summary(fit2010))[2,1]*pnotified2010sort$logridit

pnotified2005<-as.data.frame(pnotified[pnotified$year=="2005",])
pnotified2005sort<-pnotified2005[order(pnotified2005$pnotified),]
totalp=sum(pnotified2005sort$population)
totaltb=sum(pnotified2005sort$num_tb_cases)
pnotified2005sort$Wpop<-pnotified2005sort$population/totalp
pnotified2005sort$CWpop<-cumsum(pnotified2005sort$Wpop)
pnotified2005sort$ridit<-c((0+pnotified2005sort$CWpop[1])/2,(pnotified2005sort$CWpop[1]+pnotified2005sort$CWpop[2])/2,
                      (pnotified2005sort$CWpop[2]+pnotified2005sort$CWpop[3])/2,(pnotified2005sort$CWpop[3]+pnotified2005sort$CWpop[4])/2,
                      (pnotified2005sort$CWpop[4]+pnotified2005sort$CWpop[5])/2,(pnotified2005sort$CWpop[5]+pnotified2005sort$CWpop[6])/2,
                      (pnotified2005sort$CWpop[6]+pnotified2005sort$CWpop[7])/2,(pnotified2005sort$CWpop[7]+pnotified2005sort$CWpop[8])/2,
                      (pnotified2005sort$CWpop[8]+pnotified2005sort$CWpop[9])/2,(pnotified2005sort$CWpop[9]+pnotified2005sort$CWpop[10])/2,
                      (pnotified2005sort$CWpop[10]+pnotified2005sort$CWpop[11])/2,(pnotified2005sort$CWpop[11]+pnotified2005sort$CWpop[12])/2,
                      (pnotified2005sort$CWpop[12]+pnotified2005sort$CWpop[13])/2,(pnotified2005sort$CWpop[13]+pnotified2005sort$CWpop[14])/2,
                      (pnotified2005sort$CWpop[14]+pnotified2005sort$CWpop[15])/2,(pnotified2005sort$CWpop[15]+pnotified2005sort$CWpop[16])/2,
                      (pnotified2005sort$CWpop[16]+pnotified2005sort$CWpop[17])/2,(pnotified2005sort$CWpop[17]+pnotified2005sort$CWpop[18])/2,
                      (pnotified2005sort$CWpop[18]+pnotified2005sort$CWpop[19])/2,(pnotified2005sort$CWpop[19]+pnotified2005sort$CWpop[20])/2)
pnotified2005sort$Whealth<-pnotified2005sort$num_tb_cases/totaltb
pnotified2005sort$CWhealth<-cumsum(pnotified2005sort$Whealth)
pnotified2005sort$logridit<-log10(pnotified2005sort$ridit)
pnotified2005sort$Wi<-sqrt(pnotified2005sort$population)
pnotified2005sort$XiWi<-pnotified2005sort$Wi*pnotified2005sort$logridit
pnotified2005sort$YiWi<-pnotified2005sort$Wi*pnotified2005sort$ir_tb
fit2005<-lm(pnotified2005sort$YiWi~pnotified2005sort$Wi + pnotified2005sort$XiWi + 0)
summary(fit2005)
pnotified2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*pnotified2005sort$logridit

pnotified2000<-as.data.frame(pnotified[pnotified$year=="2000",])
pnotified2000sort<-pnotified2000[order(pnotified2000$pnotified),]
totalp=sum(pnotified2000sort$population)
totaltb=sum(pnotified2000sort$num_tb_cases)
pnotified2000sort$Wpop<-pnotified2000sort$population/totalp
pnotified2000sort$CWpop<-cumsum(pnotified2000sort$Wpop)
pnotified2000sort$ridit<-c((0+pnotified2000sort$CWpop[1])/2,(pnotified2000sort$CWpop[1]+pnotified2000sort$CWpop[2])/2,
                      (pnotified2000sort$CWpop[2]+pnotified2000sort$CWpop[3])/2,(pnotified2000sort$CWpop[3]+pnotified2000sort$CWpop[4])/2,
                      (pnotified2000sort$CWpop[4]+pnotified2000sort$CWpop[5])/2,(pnotified2000sort$CWpop[5]+pnotified2000sort$CWpop[6])/2,
                      (pnotified2000sort$CWpop[6]+pnotified2000sort$CWpop[7])/2,(pnotified2000sort$CWpop[7]+pnotified2000sort$CWpop[8])/2,
                      (pnotified2000sort$CWpop[8]+pnotified2000sort$CWpop[9])/2,(pnotified2000sort$CWpop[9]+pnotified2000sort$CWpop[10])/2,
                      (pnotified2000sort$CWpop[10]+pnotified2000sort$CWpop[11])/2,(pnotified2000sort$CWpop[11]+pnotified2000sort$CWpop[12])/2,
                      (pnotified2000sort$CWpop[12]+pnotified2000sort$CWpop[13])/2,(pnotified2000sort$CWpop[13]+pnotified2000sort$CWpop[14])/2,
                      (pnotified2000sort$CWpop[14]+pnotified2000sort$CWpop[15])/2,(pnotified2000sort$CWpop[15]+pnotified2000sort$CWpop[16])/2,
                      (pnotified2000sort$CWpop[16]+pnotified2000sort$CWpop[17])/2,(pnotified2000sort$CWpop[17]+pnotified2000sort$CWpop[18])/2,
                      (pnotified2000sort$CWpop[18]+pnotified2000sort$CWpop[19])/2,(pnotified2000sort$CWpop[19]+pnotified2000sort$CWpop[20])/2)

pnotified2000sort$Whealth<-pnotified2000sort$num_tb_cases/totaltb
pnotified2000sort$CWhealth<-cumsum(pnotified2000sort$Whealth)
pnotified2000sort$logridit<-log10(pnotified2000sort$ridit)
pnotified2000sort$Wi<-sqrt(pnotified2000sort$population)
pnotified2000sort$XiWi<-pnotified2000sort$Wi*pnotified2000sort$logridit
pnotified2000sort$YiWi<-pnotified2000sort$Wi*pnotified2000sort$ir_tb
fit2000<-lm(pnotified2000sort$YiWi~pnotified2000sort$Wi + pnotified2000sort$XiWi + 0)
summary(fit2000)
pnotified2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*pnotified2000sort$logridit

quartz(width=10, height=6, pointsize=10)
plot(pnotified2010sort$ridit,pnotified2010sort$ir_tb, col="black", pch=0,
     ylab="Tuberculosis incidence rates per 100,000 population", 
     xlab="Country-level population gradient defined by TB detection rate ")
points(pnotified2005sort$ridit,pnotified2005sort$ir_tb, col="black",pch=1,
       ylab="", 
       xlab="")
points(pnotified2000sort$ridit,pnotified2000sort$ir_tb, col="black",pch=2,
       ylab="", 
       xlab="")
lines(pnotified2010sort$ridit,pnotified2010sort$predict2010, col="black",lty=1,
      ylab="", 
      xlab="")
lines(pnotified2005sort$ridit,pnotified2005sort$predict2005, col="black", lty=2,
      ylab="", 
      xlab="")
lines(pnotified2000sort$ridit,pnotified2000sort$predict2000, col="black", lty=3,
      ylab="", 
      xlab="")
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)

# Social gradient
slope_index_of_inequality_pnotified2000<-fit2000$coefficients[2]
slope_index_of_inequality_pnotified2005<-fit2005$coefficients[2]
slope_index_of_inequality_pnotified2010<-fit2010$coefficients[2]
round(slope_index_of_inequality_pnotified2000,2)
round(slope_index_of_inequality_pnotified2005,2)
round(slope_index_of_inequality_pnotified2010,2)


#########################################################################
#########################################################################
#### Exploratory between-countries health inequality gap analysis  ######
#########################################################################
#########################################################################


#########################################################################
###################    GDP ##############################################
#########################################################################

gdp2010sort$qgdp<-cut(gdp2010sort$gdp,quantile(gdp2010sort$gdp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
gdp2005sort$qgdp<-cut(gdp2005sort$gdp,quantile(gdp2005sort$gdp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
gdp2000sort$qgdp<-cut(gdp2000sort$gdp,quantile(gdp2000sort$gdp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
list(gdp2010sort$country,gdp2010sort$qgdp)
list(gdp2005sort$country,gdp2005sort$qgdp)
list(gdp2000sort$country,gdp2000sort$qgdp)

qpg2010<-sapply(split(gdp2010sort$population,gdp2010sort$qgdp),sum)
qpg2005<-sapply(split(gdp2005sort$population,gdp2005sort$qgdp),sum)
qpg2000<-sapply(split(gdp2000sort$population,gdp2000sort$qgdp),sum)

wpopg2000<-c(qpg2000[1]/sum(qpg2000),qpg2000[2]/sum(qpg2000),qpg2000[3]/sum(qpg2000),qpg2000[4]/sum(qpg2000))
wpopg2005<-c(qpg2005[1]/sum(qpg2005),qpg2005[2]/sum(qpg2005),qpg2005[3]/sum(qpg2005),qpg2005[4]/sum(qpg2005))
wpopg2010<-c(qpg2010[1]/sum(qpg2010),qpg2010[2]/sum(qpg2010),qpg2010[3]/sum(qpg2010),qpg2010[4]/sum(qpg2010))

gdp2010sort$wpop2010<-ifelse(gdp2010sort$qgdp=="Q1", gdp2010sort$population/qpg2010[1],0)
gdp2010sort$wpop2010<-ifelse(gdp2010sort$qgdp=="Q2", gdp2010sort$population/qpg2010[2],gdp2010sort$wpop2010)
gdp2010sort$wpop2010<-ifelse(gdp2010sort$qgdp=="Q3", gdp2010sort$population/qpg2010[3],gdp2010sort$wpop2010)
gdp2010sort$wpop2010<-ifelse(gdp2010sort$qgdp=="Q4", gdp2010sort$population/qpg2010[4],gdp2010sort$wpop2010)

gdp2005sort$wpop2005<-ifelse(gdp2005sort$qgdp=="Q1", gdp2005sort$population/qpg2005[1],0)
gdp2005sort$wpop2005<-ifelse(gdp2005sort$qgdp=="Q2", gdp2005sort$population/qpg2005[2],gdp2005sort$wpop2005)
gdp2005sort$wpop2005<-ifelse(gdp2005sort$qgdp=="Q3", gdp2005sort$population/qpg2005[3],gdp2005sort$wpop2005)
gdp2005sort$wpop2005<-ifelse(gdp2005sort$qgdp=="Q4", gdp2005sort$population/qpg2005[4],gdp2005sort$wpop2005)

gdp2000sort$wpop2000<-ifelse(gdp2000sort$qgdp=="Q1", gdp2000sort$population/qpg2000[1],0)
gdp2000sort$wpop2000<-ifelse(gdp2000sort$qgdp=="Q2", gdp2000sort$population/qpg2000[2],gdp2000sort$wpop2000)
gdp2000sort$wpop2000<-ifelse(gdp2000sort$qgdp=="Q3", gdp2000sort$population/qpg2000[3],gdp2000sort$wpop2000)
gdp2000sort$wpop2000<-ifelse(gdp2000sort$qgdp=="Q4", gdp2000sort$population/qpg2000[4],gdp2000sort$wpop2000)

gdp2010sort$wrate<-gdp2010sort$wpop2010*gdp2010sort$ir_tb
gdp2005sort$wrate<-gdp2005sort$wpop2005*gdp2005sort$ir_tb
gdp2000sort$wrate<-gdp2000sort$wpop2000*gdp2000sort$ir_tb

meang2010<-sapply(split(gdp2010sort$wrate,gdp2010sort$qgdp),sum)
meang2010

meang2005<-sapply(split(gdp2005sort$wrate,gdp2005sort$qgdp),sum)
meang2005

meang2000<-sapply(split(gdp2000sort$wrate,gdp2000sort$qgdp),sum)
meang2000



Q1<-c(meang2000[1],meang2005[1],meang2010[1]) 
Q1<-round(Q1,2)
Q2<-c(meang2000[2],meang2005[2],meang2010[2]) 
Q2<-round(Q2,2)
Q3<-c(meang2000[3],meang2005[3],meang2010[3]) 
Q3<-round(Q3,2)
Q4<-c(meang2000[4],meang2005[4],meang2010[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4) 
row.names(r)<-c('2000','2005','2010') 

quartz(width=10, height=6, pointsize=10)
b<-barplot(r,col=c("darkgray","gray","lightgray"),beside=T,ylim=c(0,200),
           xlab="Quantiles of GDP", ylab="Average estimated incidence rate of tuberculosis")
legend("topright",c("2000","2005","2010"),
       col= c("darkgray","gray","lightgray"),pch=15,bty="n") 
text(x=b,y=c(r[1:12]),labels=c(r[1:12]),cex=1.25,pos=3)
text(2.5,3,"Most disadvantaged",cex=1.25,font=1)
text(14.5,3,"Least disadvantaged",cex=1.25,font=1)

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_gdp2000<-sum(wpopg2000*meang2000)
regional_mean_rate_gdp2005<-sum(wpopg2005*meang2005)
regional_mean_rate_gdp2010<-sum(wpopg2010*meang2010)
round(regional_mean_rate_gdp2000,2)
round(regional_mean_rate_gdp2005,2)
round(regional_mean_rate_gdp2010,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_gdp2000<-meang2000[1] - meang2000[4]
absolute_Kuznets_index_gdp2005<-meang2005[1] - meang2005[4]
absolute_Kuznets_index_gdp2010<-meang2010[1] - meang2010[4]
round(absolute_Kuznets_index_gdp2000,2)
round(absolute_Kuznets_index_gdp2005,2)
round(absolute_Kuznets_index_gdp2010,2)

relative_Kuznets_index_gdp2000<-meang2000[1]/meang2000[4]
relative_Kuznets_index_gdp2005<-meang2005[1]/meang2005[4]
relative_Kuznets_index_gdp2010<-meang2010[1]/meang2010[4]
round(relative_Kuznets_index_gdp2000,2)
round(relative_Kuznets_index_gdp2005,2)
round(relative_Kuznets_index_gdp2010,2)


#########################################################################
###################   Health expenditure #######################
#########################################################################

hexp2010sort$qhexp<-cut(hexp2010sort$hexp,quantile(hexp2010sort$hexp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hexp2005sort$qhexp<-cut(hexp2005sort$hexp,quantile(hexp2005sort$hexp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hexp2000sort$qhexp<-cut(hexp2000sort$hexp,quantile(hexp2000sort$hexp),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
list(hexp2010sort$country,hexp2010sort$qhexp)
list(hexp2005sort$country,hexp2005sort$qhexp)
list(hexp2000sort$country,hexp2000sort$qhexp)

qph2010<-sapply(split(hexp2010sort$population,hexp2010sort$qhexp),sum)
qph2005<-sapply(split(hexp2005sort$population,hexp2005sort$qhexp),sum)
qph2000<-sapply(split(hexp2000sort$population,hexp2000sort$qhexp),sum)

wpoph2000<-c(qph2000[1]/sum(qph2000),qph2000[2]/sum(qph2000),qph2000[3]/sum(qph2000),qph2000[4]/sum(qph2000))
wpoph2005<-c(qph2005[1]/sum(qph2005),qph2005[2]/sum(qph2005),qph2005[3]/sum(qph2005),qph2005[4]/sum(qph2005))
wpoph2010<-c(qph2010[1]/sum(qph2010),qph2010[2]/sum(qph2010),qph2010[3]/sum(qph2010),qph2010[4]/sum(qph2010))

hexp2010sort$wpop2010<-ifelse(hexp2010sort$qhexp=="Q1", hexp2010sort$population/qph2010[1],0)
hexp2010sort$wpop2010<-ifelse(hexp2010sort$qhexp=="Q2", hexp2010sort$population/qph2010[2],hexp2010sort$wpop2010)
hexp2010sort$wpop2010<-ifelse(hexp2010sort$qhexp=="Q3", hexp2010sort$population/qph2010[3],hexp2010sort$wpop2010)
hexp2010sort$wpop2010<-ifelse(hexp2010sort$qhexp=="Q4", hexp2010sort$population/qph2010[4],hexp2010sort$wpop2010)

hexp2005sort$wpop2005<-ifelse(hexp2005sort$qhexp=="Q1", hexp2005sort$population/qph2005[1],0)
hexp2005sort$wpop2005<-ifelse(hexp2005sort$qhexp=="Q2", hexp2005sort$population/qph2005[2],hexp2005sort$wpop2005)
hexp2005sort$wpop2005<-ifelse(hexp2005sort$qhexp=="Q3", hexp2005sort$population/qph2005[3],hexp2005sort$wpop2005)
hexp2005sort$wpop2005<-ifelse(hexp2005sort$qhexp=="Q4", hexp2005sort$population/qph2005[4],hexp2005sort$wpop2005)

hexp2000sort$wpop2000<-ifelse(hexp2000sort$qhexp=="Q1", hexp2000sort$population/qph2000[1],0)
hexp2000sort$wpop2000<-ifelse(hexp2000sort$qhexp=="Q2", hexp2000sort$population/qph2000[2],hexp2000sort$wpop2000)
hexp2000sort$wpop2000<-ifelse(hexp2000sort$qhexp=="Q3", hexp2000sort$population/qph2000[3],hexp2000sort$wpop2000)
hexp2000sort$wpop2000<-ifelse(hexp2000sort$qhexp=="Q4", hexp2000sort$population/qph2000[4],hexp2000sort$wpop2000)

hexp2010sort$wrate<-hexp2010sort$wpop2010*hexp2010sort$ir_tb
hexp2005sort$wrate<-hexp2005sort$wpop2005*hexp2005sort$ir_tb
hexp2000sort$wrate<-hexp2000sort$wpop2000*hexp2000sort$ir_tb

meanh2010<-sapply(split(hexp2010sort$wrate,hexp2010sort$qhexp),sum)
meanh2010

meanh2005<-sapply(split(hexp2005sort$wrate,hexp2005sort$qhexp),sum)
meanh2005

meanh2000<-sapply(split(hexp2000sort$wrate,hexp2000sort$qhexp),sum)
meanh2000



Q1<-c(meanh2000[1],meanh2005[1],meanh2010[1]) 
Q1<-round(Q1,2)
Q2<-c(meanh2000[2],meanh2005[2],meanh2010[2]) 
Q2<-round(Q2,2)
Q3<-c(meanh2000[3],meanh2005[3],meanh2010[3]) 
Q3<-round(Q3,2)
Q4<-c(meanh2000[4],meanh2005[4],meanh2010[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4) 
row.names(r)<-c('2000','2005','2010') 

quartz(width=10, height=6, pointsize=10)
b<-barplot(r,col=c("darkgray","gray","lightgray"),beside=T,ylim=c(0,200),
					 xlab="Quantiles of total health expenditure per capita", ylab="Average estimated incidence rate of tuberculosis")
legend("topright",c("2000","2005","2010"),
col= c("darkgray","gray","lightgray"),pch=15,bty="n") 
text(x=b,y=c(r[1:12]),labels=c(r[1:12]),cex=1.25,pos=3)
text(2.5,3,"Most disadvantaged",cex=1.25,font=1)
text(14.5,3,"Least disadvantaged",cex=1.25,font=1)

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_hexp2000<-sum(wpoph2000*meanh2000)
regional_mean_rate_hexp2005<-sum(wpoph2005*meanh2005)
regional_mean_rate_hexp2010<-sum(wpoph2010*meanh2010)
round(regional_mean_rate_hexp2000,2)
round(regional_mean_rate_hexp2005,2)
round(regional_mean_rate_hexp2010,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_hexp2000<-meanh2000[1] - meanh2000[4]
absolute_Kuznets_index_hexp2005<-meanh2005[1] - meanh2005[4]
absolute_Kuznets_index_hexp2010<- meanh2010[1] - meanh2010[4]
round(absolute_Kuznets_index_hexp2000,2)
round(absolute_Kuznets_index_hexp2005,2)
round(absolute_Kuznets_index_hexp2010,2)

relative_Kuznets_index_hexp2000<-meanh2000[1]/meanh2000[4]
relative_Kuznets_index_hexp2005<-meanh2005[1]/meanh2005[4]
relative_Kuznets_index_hexp2010<-meanh2010[1]/meanh2010[4]
round(relative_Kuznets_index_hexp2000,2)
round(relative_Kuznets_index_hexp2005,2)
round(relative_Kuznets_index_hexp2010,2)



#######  Access to improved sanitation facilities  ##########
impsfac2000sort$qimpsfac<-cut(impsfac2000sort$impsfac,quantile(impsfac2000sort$impsfac),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
impsfac2005sort$qimpsfac<-cut(impsfac2005sort$impsfac,quantile(impsfac2005sort$impsfac),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
impsfac2010sort$qimpsfac<-cut(impsfac2010sort$impsfac,quantile(impsfac2010sort$impsfac),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
list(impsfac2010sort$country,impsfac2010sort$qimpsfac)
list(impsfac2005sort$country,impsfac2005sort$qimpsfac)
list(impsfac2000sort$country,impsfac2000sort$qimpsfac)

qpf2000<-sapply(split(impsfac2000sort$population,impsfac2000sort$qimpsfac),sum)
qpf2005<-sapply(split(impsfac2005sort$population,impsfac2005sort$qimpsfac),sum)
qpf2010<-sapply(split(impsfac2010sort$population,impsfac2010sort$qimpsfac),sum)


wpopf2000<-c(qpf2000[1]/sum(qpf2000),qpf2000[2]/sum(qpf2000),qpf2000[3]/sum(qpf2000),qpf2000[4]/sum(qpf2000))
wpopf2005<-c(qpf2005[1]/sum(qpf2005),qpf2005[2]/sum(qpf2005),qpf2005[3]/sum(qpf2005),qpf2005[4]/sum(qpf2005))
wpopf2010<-c(qpf2010[1]/sum(qpf2010),qpf2010[2]/sum(qpf2010),qpf2010[3]/sum(qpf2010),qpf2010[4]/sum(qpf2010))

impsfac2010sort$wpop2010<-ifelse(impsfac2010sort$qimpsfac=="Q1", impsfac2010sort$population/qpf2010[1],0)
impsfac2010sort$wpop2010<-ifelse(impsfac2010sort$qimpsfac=="Q2", impsfac2010sort$population/qpf2010[2],impsfac2010sort$wpop2010)
impsfac2010sort$wpop2010<-ifelse(impsfac2010sort$qimpsfac=="Q3", impsfac2010sort$population/qpf2010[3],impsfac2010sort$wpop2010)
impsfac2010sort$wpop2010<-ifelse(impsfac2010sort$qimpsfac=="Q4", impsfac2010sort$population/qpf2010[4],impsfac2010sort$wpop2010)

impsfac2005sort$wpop2005<-ifelse(impsfac2005sort$qimpsfac=="Q1", impsfac2005sort$population/qpf2005[1],0)
impsfac2005sort$wpop2005<-ifelse(impsfac2005sort$qimpsfac=="Q2", impsfac2005sort$population/qpf2005[2],impsfac2005sort$wpop2005)
impsfac2005sort$wpop2005<-ifelse(impsfac2005sort$qimpsfac=="Q3", impsfac2005sort$population/qpf2005[3],impsfac2005sort$wpop2005)
impsfac2005sort$wpop2005<-ifelse(impsfac2005sort$qimpsfac=="Q4", impsfac2005sort$population/qpf2005[4],impsfac2005sort$wpop2005)

impsfac2000sort$wpop2000<-ifelse(impsfac2000sort$qimpsfac=="Q1", impsfac2000sort$population/qpf2000[1],0)
impsfac2000sort$wpop2000<-ifelse(impsfac2000sort$qimpsfac=="Q2", impsfac2000sort$population/qpf2000[2],impsfac2000sort$wpop2000)
impsfac2000sort$wpop2000<-ifelse(impsfac2000sort$qimpsfac=="Q3", impsfac2000sort$population/qpf2000[3],impsfac2000sort$wpop2000)
impsfac2000sort$wpop2000<-ifelse(impsfac2000sort$qimpsfac=="Q4", impsfac2000sort$population/qpf2000[4],impsfac2000sort$wpop2000)

impsfac2010sort$wrate<-impsfac2010sort$wpop2010*impsfac2010sort$ir_tb
impsfac2005sort$wrate<-impsfac2005sort$wpop2005*impsfac2005sort$ir_tb
impsfac2000sort$wrate<-impsfac2000sort$wpop2000*impsfac2000sort$ir_tb

meanf2010<-sapply(split(impsfac2010sort$wrate,impsfac2010sort$qimpsfac),sum)
meanf2010

meanf2005<-sapply(split(impsfac2005sort$wrate,impsfac2005sort$qimpsfac),sum)
meanf2005

meanf2000<-sapply(split(impsfac2000sort$wrate,impsfac2000sort$qimpsfac),sum)
meanf2000


Q1<-c(meanf2000[1],meanf2005[1],meanf2010[1])
Q1<-round(Q1,2)
Q2<-c(meanf2000[2],meanf2005[2],meanf2010[2]) 
Q2<-round(Q2,2)
Q3<-c(meanf2000[3],meanf2005[3],meanf2010[3]) 
Q3<-round(Q3,2)
Q4<-c(meanf2000[4],meanf2005[4],meanf2010[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4) 
row.names(r)<-c('2000','2005','2010') 

quartz(width=10, height=6, pointsize=10)
b<-barplot(r,col=c("darkgray","gray","lightgray"),beside=T,ylim=c(0,200),
					 xlab="Quantiles of access to improved sanitation facilities", ylab="Average estimated incidence rate of tuberculosis")
legend("topright",c("2000","2005","2010"),
col= c("darkgray","gray","lightgray"),pch=15,bty="n") 
text(x=b,y=c(r[1:12]),labels=c(r[1:12]),cex=1.25,pos=3)
text(2.5,3,"Most disadvantaged",cex=1.25,font=1)
text(14.5,3,"Least disadvantaged",cex=1.25,font=1)

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_impsfac2000<-sum(wpopf2000*meanf2000)
regional_mean_rate_impsfac2005<-sum(wpopf2005*meanf2005)
regional_mean_rate_impsfac2010<-sum(wpopf2010*meanf2010)
round(regional_mean_rate_impsfac2000,2)
round(regional_mean_rate_impsfac2005,2)
round(regional_mean_rate_impsfac2010,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_impsfac2000<-meanf2000[1] - meanf2000[4]
absolute_Kuznets_index_impsfac2005<-meanf2005[1] - meanf2005[4]
absolute_Kuznets_index_impsfac2010<-meanf2010[1] - meanf2010[4]
  
round(absolute_Kuznets_index_impsfac2000,2)
round(absolute_Kuznets_index_impsfac2005,2)
round(absolute_Kuznets_index_impsfac2010,2)

relative_Kuznets_index_impsfac2000<-meanf2000[1]/meanf2000[4]
relative_Kuznets_index_impsfac2005<-meanf2005[1]/meanf2005[4]
relative_Kuznets_index_impsfac2010<-meanf2010[1]/meanf2010[4]
round(relative_Kuznets_index_impsfac2000,2)
round(relative_Kuznets_index_impsfac2005,2)
round(relative_Kuznets_index_impsfac2010,2)



#######  TB detection rate   ##########
pnotified2000sort$qpnotified<-cut(pnotified2000sort$pnotified,quantile(pnotified2000sort$pnotified),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
pnotified2005sort$qpnotified<-cut(pnotified2005sort$pnotified,quantile(pnotified2005sort$pnotified),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
pnotified2010sort$qpnotified<-cut(pnotified2010sort$pnotified,quantile(pnotified2010sort$pnotified),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
list(pnotified2010sort$country,impsfac2010sort$qpnotified)
list(pnotified2005sort$country,impsfac2005sort$qpnotified)
list(pnotified2000sort$country,impsfac2000sort$qpnotified)

qpt2000<-sapply(split(pnotified2000sort$population,pnotified2000sort$qpnotified),sum)
qpt2005<-sapply(split(pnotified2005sort$population,pnotified2005sort$qpnotified),sum)
qpt2010<-sapply(split(pnotified2010sort$population,pnotified2010sort$qpnotified),sum)


wpopt2000<-c(qpt2000[1]/sum(qpt2000),qpt2000[2]/sum(qpt2000),qpt2000[3]/sum(qpt2000),qpt2000[4]/sum(qpt2000))
wpopt2005<-c(qpt2005[1]/sum(qpt2005),qpt2005[2]/sum(qpt2005),qpt2005[3]/sum(qpt2005),qpt2005[4]/sum(qpt2005))
wpopt2010<-c(qpt2010[1]/sum(qpt2010),qpt2010[2]/sum(qpt2010),qpt2010[3]/sum(qpt2010),qpt2010[4]/sum(qpt2010))

pnotified2010sort$wpop2010<-ifelse(pnotified2010sort$qpnotified=="Q1", pnotified2010sort$population/qpt2010[1],0)
pnotified2010sort$wpop2010<-ifelse(pnotified2010sort$qpnotified=="Q2", pnotified2010sort$population/qpt2010[2],pnotified2010sort$wpop2010)
pnotified2010sort$wpop2010<-ifelse(pnotified2010sort$qpnotified=="Q3", pnotified2010sort$population/qpt2010[3],pnotified2010sort$wpop2010)
pnotified2010sort$wpop2010<-ifelse(pnotified2010sort$qpnotified=="Q4", pnotified2010sort$population/qpt2010[4],pnotified2010sort$wpop2010)

pnotified2005sort$wpop2005<-ifelse(pnotified2005sort$qpnotified=="Q1", pnotified2005sort$population/qpt2005[1],0)
pnotified2005sort$wpop2005<-ifelse(pnotified2005sort$qpnotified=="Q2", pnotified2005sort$population/qpt2005[2],pnotified2005sort$wpop2005)
pnotified2005sort$wpop2005<-ifelse(pnotified2005sort$qpnotified=="Q3", pnotified2005sort$population/qpt2005[3],pnotified2005sort$wpop2005)
pnotified2005sort$wpop2005<-ifelse(pnotified2005sort$qpnotified=="Q4", pnotified2005sort$population/qpt2005[4],pnotified2005sort$wpop2005)

pnotified2000sort$wpop2000<-ifelse(pnotified2000sort$qpnotified=="Q1", pnotified2000sort$population/qpt2000[1],0)
pnotified2000sort$wpop2000<-ifelse(pnotified2000sort$qpnotified=="Q2", pnotified2000sort$population/qpt2000[2],pnotified2000sort$wpop2000)
pnotified2000sort$wpop2000<-ifelse(pnotified2000sort$qpnotified=="Q3", pnotified2000sort$population/qpt2000[3],pnotified2000sort$wpop2000)
pnotified2000sort$wpop2000<-ifelse(pnotified2000sort$qpnotified=="Q4", pnotified2000sort$population/qpt2000[4],pnotified2000sort$wpop2000)

pnotified2010sort$wrate<-pnotified2010sort$wpop2010*pnotified2010sort$ir_tb
pnotified2005sort$wrate<-pnotified2005sort$wpop2005*pnotified2005sort$ir_tb
pnotified2000sort$wrate<-pnotified2000sort$wpop2000*pnotified2000sort$ir_tb

meant2010<-sapply(split(pnotified2010sort$wrate,pnotified2010sort$qpnotified),sum)
meant2010

meant2005<-sapply(split(pnotified2005sort$wrate,pnotified2005sort$qpnotified),sum)
meant2005

meant2000<-sapply(split(pnotified2000sort$wrate,pnotified2000sort$qpnotified),sum)
meant2000


Q1<-c(meant2000[1],meant2005[1],meant2010[1])
Q1<-round(Q1,2)
Q2<-c(meant2000[2],meant2005[2],meant2010[2]) 
Q2<-round(Q2,2)
Q3<-c(meant2000[3],meant2005[3],meant2010[3]) 
Q3<-round(Q3,2)
Q4<-c(meant2000[4],meant2005[4],meant2010[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4) 
row.names(r)<-c('2000','2005','2010') 

quartz(width=10, height=6, pointsize=10)
b<-barplot(r,col=c("darkgray","gray","lightgray"),beside=T,ylim=c(0,200),
           xlab="Quantiles of TB detection rate ", ylab="Average estimated incidence rate of tuberculosis")
legend("topright",c("2000","2005","2010"),
       col= c("darkgray","gray","lightgray"),pch=15,bty="n") 
text(x=b,y=c(r[1:12]),labels=c(r[1:12]),cex=1.25,pos=3)
text(2.5,3,"Most disadvantaged",cex=1.25,font=1)
text(14.5,3,"Least disadvantaged",cex=1.25,font=1)

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_pnotified2000<-sum(wpopt2000*meant2000)
regional_mean_rate_pnotified2005<-sum(wpopt2005*meant2005)
regional_mean_rate_pnotified2010<-sum(wpopt2010*meant2010)
round(regional_mean_rate_pnotified2000,2)
round(regional_mean_rate_pnotified2005,2)
round(regional_mean_rate_pnotified2010,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_pnotified2000<-meant2000[1] - meant2000[4]
absolute_Kuznets_index_pnotified2005<-meant2005[1] - meant2005[4]
absolute_Kuznets_index_pnotified2010<-meant2010[1] - meant2010[4]
round(absolute_Kuznets_index_pnotified2000,2)
round(absolute_Kuznets_index_pnotified2005,2)
round(absolute_Kuznets_index_pnotified2010,2)

relative_Kuznets_index_pnotified2000<-meant2000[1]/meant2000[4]
relative_Kuznets_index_pnotified2005<-meant2005[1]/meant2005[4]
relative_Kuznets_index_pnotified2010<-meant2010[1]/meant2010[4]
round(relative_Kuznets_index_pnotified2000,2)
round(relative_Kuznets_index_pnotified2005,2)
round(relative_Kuznets_index_pnotified2010,2)

###############################################################
################# Concentration Curve #########################
###############################################################

###############################################################
############################ GDP  ##############################

CWpopf2010<-c(0,gdp2010sort$CWpop)
CWhealthf2010<-c(0,gdp2010sort$CWhealth)
CWpopf2005<-c(0,gdp2005sort$CWpop)
CWhealthf2005<-c(0,gdp2005sort$CWhealth)
CWpopf2000<-c(0,gdp2000sort$CWpop)
CWhealthf2000<-c(0,gdp2000sort$CWhealth)


ccurve2010<-data.frame(y=CWhealthf2010, x=CWpopf2010)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2010 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2010, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2010)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

k<-ccurve.optx2010[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02010<-f(x,k)
delta_x_y<-x-lf02010
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_gdp2010<-2*sum(delta_x_y)*0.01
round(health_concentration_index_gdp2010,2)


k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_gdp2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_gdp2005,2)


k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_gdp2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_gdp2000,2)

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2010,CWhealthf2010, col="black",pch=0, xlab="Country-level population gradient defined by GDP", ylab="Tuberculosis incidence rates per 100,000 population")
points(CWpopf2005,CWhealthf2005, col="black",pch=1)
points(CWpopf2000,CWhealthf2000, col="black",pch=2)
lines(x,lf02010,col="black", lty=1)
lines(x,lf02005,col="black", lty=2)
lines(x,lf02000,col="black", lty=3)
lines(x,x)
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2), lty=c(1,2,3),cex = .8)





###############################################################
############# health expenditure per capita  ####################

CWpopf2010<-c(0,hexp2010sort$CWpop)
CWhealthf2010<-c(0,hexp2010sort$CWhealth)
CWpopf2005<-c(0,hexp2005sort$CWpop)
CWhealthf2005<-c(0,hexp2005sort$CWhealth)
CWpopf2000<-c(0,hexp2000sort$CWpop)
CWhealthf2000<-c(0,hexp2000sort$CWhealth)


ccurve2010<-data.frame(y=CWhealthf2010, x=CWpopf2010)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2010 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2010, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2010)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

#k=-1.26967452211873
k<-ccurve.optx2010[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02010<-f(x,k)
delta_x_y<-x-lf02010
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hexp2010<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hexp2010,2)


k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hexp2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hexp2005,2)


k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hexp2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hexp2000,2)

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2010,CWhealthf2010, col="black",pch=0, xlab="Country-level population gradient defined by health expenditure per capita", ylab="Tuberculosis incidence rates per 100,000 population")
points(CWpopf2005,CWhealthf2005, col="black",pch=1)
points(CWpopf2000,CWhealthf2000, col="black",pch=2)
lines(x,lf02010,col="black", lty=1)
lines(x,lf02005,col="black", lty=2)
lines(x,lf02000,col="black", lty=3)
lines(x,x)
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2), lty=c(1,2,3),cex = .8)



############# Access to improved sanitation facilities  ####################

CWpopf2010<-c(0,impsfac2010sort$CWpop)
CWhealthf2010<-c(0,impsfac2010sort$CWhealth)
CWpopf2005<-c(0,impsfac2005sort$CWpop)
CWhealthf2005<-c(0,impsfac2005sort$CWhealth)
CWpopf2000<-c(0,impsfac2000sort$CWpop)
CWhealthf2000<-c(0,impsfac2000sort$CWhealth)


ccurve2010<-data.frame(y=CWhealthf2010, x=CWpopf2010)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2010 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2010, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2010)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

#k=-1.26967452211873
k<-ccurve.optx2010[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02010<-f(x,k)
delta_x_y<-x-lf02010
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impsfac2010<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impsfac2010,2)

#k=-1.01062753789465
k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impsfac2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impsfac2005,2)

#k=-1.18651625193744
k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impsfac2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impsfac2000,2)

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2010,CWhealthf2010, col="black", pch=0, xlab="Country-level population gradient defined by access to improved sanitation facilities", ylab="Tuberculosis incidence rates per 100,000 population")
points(CWpopf2005,CWhealthf2005, col="black", pch=1)
points(CWpopf2000,CWhealthf2000, col="black", pch=2)
lines(x,lf02010,col="black", lty=1)
lines(x,lf02005,col="black", lty=2)
lines(x,lf02000,col="black", lty=3)
lines(x,x)
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)




############# TB detection rate   ####################

CWpopf2010<-c(0,pnotified2010sort$CWpop)
CWhealthf2010<-c(0,pnotified2010sort$CWhealth)
CWpopf2005<-c(0,pnotified2005sort$CWpop)
CWhealthf2005<-c(0,pnotified2005sort$CWhealth)
CWpopf2000<-c(0,pnotified2000sort$CWpop)
CWhealthf2000<-c(0,pnotified2000sort$CWhealth)


ccurve2010<-data.frame(y=CWhealthf2010, x=CWpopf2010)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2010 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2010, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2010)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

#k=-1.26967452211873
k<-ccurve.optx2010[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02010<-f(x,k)
delta_x_y<-x-lf02010
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_pnotified2010<-2*sum(delta_x_y)*0.01
round(health_concentration_index_pnotified2010,2)

#k=-1.01062753789465
k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_pnotified2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_pnotified2005,2)

#k=-1.18651625193744
k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_pnotified2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_pnotified2000,8)

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2010,CWhealthf2010, col="black", pch=0, xlab="Country-level population gradient defined by TB detection rate ", ylab="Tuberculosis incidence rates per 100,000 population")
points(CWpopf2005,CWhealthf2005, col="black", pch=1)
points(CWpopf2000,CWhealthf2000, col="black", pch=2)
lines(x,lf02010,col="black", lty=1)
lines(x,lf02005,col="black", lty=2)
lines(x,lf02000,col="black", lty=3)
lines(x,x)
legend(locator(1),c("2010","2005","2000"),col=c("black","black","black"),pch=c(0,1,2),lty=c(1,2,3),cex = .8)



