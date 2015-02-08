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
library(reshape2)
library(plyr)

#### reshaping the population dataset ############################
population<-read.csv("population.csv")
population_data<-subset(population,select=-c(country_code))
population_f<-melt(population_data, Country=c("country"))
population_f$year<-as.numeric(substr(population_f$variable,2,5))
population_f$population<-population_f$value
population_f<-subset(population_f,select=-c(variable,value))
population_f$country<-as.character(population_f$country)


population_f$country<-factor(population_f$country, label = c("Antigua and Barbuda","Argentina", "Bahamas", "Barbados","Belize", "Bolivia", "Brazil","Chile",
                                                   "Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador","Grenada",
                                                   "Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
                                                   "Peru","Puerto Rico","Saint Kitts and Nevis","Saint Vincent and the Grenadines","Suriname","Trinidad and Tobago",
                                                   "Uruguay","Venezuela"))

population_f<-subset(population_f,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                         "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                         "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                         "Uruguay","Venezuela"))

population_f <- population_f[order(population_f$country,population_f$year),] 

#### reshaping the HDI dataset ############################

hdi<-read.csv("HDI1980_2013.csv")
hdi_data<-subset(hdi,select=-c(HDI.Rank))
hdi_world<-melt(hdi_data, Country=c("Country"))
hdi_world$year<-as.numeric(substr(hdi_world$variable,2,5))
hdi_world$hdi<-hdi_world$value
hdi_world<-subset(hdi_world,select=-c(variable,value))
hdi_world$Country<-as.character(hdi_world$Country)


hdi_lac_f<-subset(hdi_world,Country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                         "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                         "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                         "Uruguay","Venezuela"))

hdi_lac_f <- rename(hdi_lac_f, c(Country="country"))

hdi_lac_f <- hdi_lac_f[order(hdi_lac_f$country,hdi_lac_f$year),] 

################# reshaping TB data ######################################

tb_world<-read.csv("world_tb_dataset.csv")
tb_americas<-subset(tb_world,tb_world$WHO.region=="Americas")

data1=subset(tb_americas,Indicator %in% c("Incidence of tuberculosis (per 100 000 population per year)"))
data1<-data1[,c(4,2,5,6,7)]
data1 <- rename(data1, c(Numeric="ir_tb"))
data1 <- rename(data1, c(Low="ir_tb_low"))
data1 <- rename(data1, c(High="ir_tb_high"))

data2=subset(tb_americas,Indicator %in% c("Incidence of tuberculosis (per 100 000 population) (HIV-positive cases)"))
data2<-data2[,c(4,2,5,6,7)]
data2 <- rename(data2, c(Numeric="ir_tb_hiv_pos"))
data2 <- rename(data2, c(Low="ir_tb_hiv_pos_low"))
data2 <- rename(data2, c(High="ir_tb_hiv_pos_high"))


data3=subset(tb_americas,Indicator %in% c("Number of incident tuberculosis cases"))
data3<-data3[,c(4,2,5,6,7)]
data3 <- rename(data3, c(Numeric="num_tb_cases"))
data3 <- rename(data3, c(Low="num_tb_cases_low"))
data3 <- rename(data3, c(High="num_tb_cases_high"))


data4=subset(tb_americas,Indicator %in% c("Number of incident tuberculosis cases,  (HIV-positive cases)"))
data4<-data4[,c(4,2,5,6,7)]
data4 <- rename(data4, c(Numeric="num_tb_cases_hiv_pos"))
data4 <- rename(data4, c(Low="num_tb_cases_hiv_pos_low"))
data4 <- rename(data4, c(High="num_tb_cases_hiv_pos_high"))

merge1 <- merge(data1,data2,by=c("country","year"),all=TRUE)
merge2 <- merge(merge1,data3,by=c("country","year"),all=TRUE)
merge3 <- merge(merge2,data4,by=c("country","year"),all=TRUE)

tb_lac<-merge3[,c(1,2,3,9)]

tb_lac$country<-factor(tb_lac$country, label = c("Antigua and Barbuda","Argentina", "Bahamas", "Barbados","Belize", "Bolivia", "Brazil","Canada",
                                                 "Chile","Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador","Grenada",
                                                   "Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
                                                   "Peru","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Suriname","Trinidad and Tobago",
                                                   "United States of America","Uruguay","Venezuela"))

tb_lac_f<-subset(tb_lac,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                         "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                         "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                         "Uruguay","Venezuela"))
tb_lac_f<-subset(tb_lac_f, tb_lac_f$year>=1995)

##### reshaping TB data for male and female

tb_world_gender<-read.csv("TB_notifications_2015-02-03.csv")
tb_americas_gender<-subset(tb_world_gender,tb_world_gender$g_whoregion=="AMR")

tb_americas_gender$country<-factor(tb_americas_gender$country, label = c("Anguilla","Antigua and Barbuda","Argentina", "Aruba","Bahamas", "Barbados","Belize", "Bermuda","Bolivia (Plurinational State of)", 
                                                 "Bonaire, Saint Eustatius and Saba","Brazil","British Virgin Islands","Canada","Cayman Islands","Chile","Colombia","Costa Rica","Cuba","Curaçao","Dominica",
                                                 "Dominican Republic","Ecuador","El Salvador","Grenada","Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Monserrat","Netherlands Antilles","Nicaragua",
                                                 "Panama","Paraguay","Peru","Puerto Rico","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Sint Maarten (Dutch part)","Suriname","Trinidad and Tobago",
                                                 "Turks and Caicos Islands","United States of America","Uruguay","US Virgin Islands","Venezuela (Bolivarian Republic of)"))

tb_lac_gender<-subset(tb_americas_gender,country %in% c("Argentina", "Bolivia (Plurinational State of)", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                       "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                       "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                       "Uruguay","Venezuela (Bolivarian Republic of)"))

tb_lac_gender$country <- revalue(tb_lac_gender$country, c("Bolivia (Plurinational State of)"="Bolivia", "Venezuela (Bolivarian Republic of)"="Venezuela"))


tb_lac_gender_f<-subset(tb_lac_gender, tb_lac_gender$year==2012)


tb_lac_gender_f$female_tb<-tb_lac_gender_f$new_sp_f014 + tb_lac_gender_f$new_sp_f1524 + tb_lac_gender_f$new_sp_f2534 +
  tb_lac_gender_f$new_sp_f3544 + tb_lac_gender_f$new_sp_f4554 + tb_lac_gender_f$new_sp_f514 + tb_lac_gender_f$new_sp_f5564 +
  tb_lac_gender_f$new_sp_f65

tb_lac_gender_f$male_tb<-tb_lac_gender_f$new_sp_m014 + tb_lac_gender_f$new_sp_m1524 + tb_lac_gender_f$new_sp_m2534 +
  tb_lac_gender_f$new_sp_m3544 + tb_lac_gender_f$new_sp_m4554 + tb_lac_gender_f$new_sp_m514 + tb_lac_gender_f$new_sp_m5564 +
  tb_lac_gender_f$new_sp_m65

tb_lac_gender_ff<-tb_lac_gender_f[, c("country","year","female_tb","male_tb")]

#View(tb_lac_gender_f[,c(1,6,160)])

#########################################################################
################# Building the final dataset ############################  
tb_lac_ff<-merge(tb_lac_f,population_f,by=c("country","year"),all=TRUE)

hdi_lac_fff<-subset(hdi_lac_f,year %in% c(2000,2005,2009,2013)) ## we used the same value of 2005 hdi for 2000 
tb_lac_fff<-subset(tb_lac_ff,year %in% c(2000,2005,2009,2013))

df<-merge(tb_lac_fff,hdi_lac_fff,by=c("country","year"),all=TRUE)

lengend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba","Dominican Republic",
            "Ecuador","El Salvador","Guatemala ","Guyana","Haiti","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Trinidad and Tobago","Uruguay","Venezuela")

#########################################################
##### Health inequality for Human Development Index #####
#########################################################


############################################################################
############################ regression weight analysis ####################

hdi2013<-as.data.frame(df[df$year=="2013",])

hdi2013sort<-hdi2013[order(hdi2013$hdi),]
totalp=sum(hdi2013sort$population)
totaltb=sum(hdi2013sort$num_tb_cases)
hdi2013sort$Wpop<-hdi2013sort$population/totalp
hdi2013sort$CWpop<-cumsum(hdi2013sort$Wpop)
hdi2013sort$ridit<-c((0+hdi2013sort$CWpop[1])/2,(hdi2013sort$CWpop[1]+hdi2013sort$CWpop[2])/2,
                     (hdi2013sort$CWpop[2]+hdi2013sort$CWpop[3])/2,(hdi2013sort$CWpop[3]+hdi2013sort$CWpop[4])/2,
                     (hdi2013sort$CWpop[4]+hdi2013sort$CWpop[5])/2,(hdi2013sort$CWpop[5]+hdi2013sort$CWpop[6])/2,
                     (hdi2013sort$CWpop[6]+hdi2013sort$CWpop[7])/2,(hdi2013sort$CWpop[7]+hdi2013sort$CWpop[8])/2,
                     (hdi2013sort$CWpop[8]+hdi2013sort$CWpop[9])/2,(hdi2013sort$CWpop[9]+hdi2013sort$CWpop[10])/2,
                     (hdi2013sort$CWpop[10]+hdi2013sort$CWpop[11])/2,(hdi2013sort$CWpop[11]+hdi2013sort$CWpop[12])/2,
                     (hdi2013sort$CWpop[12]+hdi2013sort$CWpop[13])/2,(hdi2013sort$CWpop[13]+hdi2013sort$CWpop[14])/2,
                     (hdi2013sort$CWpop[14]+hdi2013sort$CWpop[15])/2,(hdi2013sort$CWpop[15]+hdi2013sort$CWpop[16])/2,
                     (hdi2013sort$CWpop[16]+hdi2013sort$CWpop[17])/2,(hdi2013sort$CWpop[17]+hdi2013sort$CWpop[18])/2,
                     (hdi2013sort$CWpop[18]+hdi2013sort$CWpop[19])/2,(hdi2013sort$CWpop[19]+hdi2013sort$CWpop[20])/2,
                     (hdi2013sort$CWpop[20]+hdi2013sort$CWpop[21])/2,(hdi2013sort$CWpop[21]+hdi2013sort$CWpop[22])/2,
                     (hdi2013sort$CWpop[22]+hdi2013sort$CWpop[23])/2)

hdi2013sort$Whealth<-hdi2013sort$num_tb_cases/totaltb
hdi2013sort$CWhealth<-cumsum(hdi2013sort$Whealth)
hdi2013sort$logridit<-log10(hdi2013sort$ridit)
hdi2013sort$Wi<-sqrt(hdi2013sort$population)
hdi2013sort$XiWi<-hdi2013sort$Wi*hdi2013sort$logridit
hdi2013sort$YiWi<-hdi2013sort$Wi*hdi2013sort$ir_tb
fit2013<-lm(hdi2013sort$YiWi~hdi2013sort$Wi + hdi2013sort$XiWi + 0)
summary(fit2013)
hdi2013sort$predict2013<-coef(summary(fit2013))[1,1] + coef(summary(fit2013))[2,1]*hdi2013sort$logridit



hdi2009<-as.data.frame(df[df$year=="2009",])
hdi2009sort<-hdi2009[order(hdi2009$hdi),]
totalp=sum(hdi2009sort$population)
totaltb=sum(hdi2009sort$num_tb_cases)
hdi2009sort$Wpop<-hdi2009sort$population/totalp
hdi2009sort$CWpop<-cumsum(hdi2009sort$Wpop)
hdi2009sort$ridit<-c((0+hdi2009sort$CWpop[1])/2,(hdi2009sort$CWpop[1]+hdi2009sort$CWpop[2])/2,
                     (hdi2009sort$CWpop[2]+hdi2009sort$CWpop[3])/2,(hdi2009sort$CWpop[3]+hdi2009sort$CWpop[4])/2,
                     (hdi2009sort$CWpop[4]+hdi2009sort$CWpop[5])/2,(hdi2009sort$CWpop[5]+hdi2009sort$CWpop[6])/2,
                     (hdi2009sort$CWpop[6]+hdi2009sort$CWpop[7])/2,(hdi2009sort$CWpop[7]+hdi2009sort$CWpop[8])/2,
                     (hdi2009sort$CWpop[8]+hdi2009sort$CWpop[9])/2,(hdi2009sort$CWpop[9]+hdi2009sort$CWpop[10])/2,
                     (hdi2009sort$CWpop[10]+hdi2009sort$CWpop[11])/2,(hdi2009sort$CWpop[11]+hdi2009sort$CWpop[12])/2,
                     (hdi2009sort$CWpop[12]+hdi2009sort$CWpop[13])/2,(hdi2009sort$CWpop[13]+hdi2009sort$CWpop[14])/2,
                     (hdi2009sort$CWpop[14]+hdi2009sort$CWpop[15])/2,(hdi2009sort$CWpop[15]+hdi2009sort$CWpop[16])/2,
                     (hdi2009sort$CWpop[16]+hdi2009sort$CWpop[17])/2,(hdi2009sort$CWpop[17]+hdi2009sort$CWpop[18])/2,
                     (hdi2009sort$CWpop[18]+hdi2009sort$CWpop[19])/2,(hdi2009sort$CWpop[19]+hdi2009sort$CWpop[20])/2,
                     (hdi2009sort$CWpop[20]+hdi2009sort$CWpop[21])/2,(hdi2009sort$CWpop[21]+hdi2009sort$CWpop[22])/2,
                     (hdi2009sort$CWpop[22]+hdi2009sort$CWpop[23])/2)
hdi2009sort$Whealth<-hdi2009sort$num_tb_cases/totaltb
hdi2009sort$CWhealth<-cumsum(hdi2009sort$Whealth)
hdi2009sort$logridit<-log10(hdi2009sort$ridit)
hdi2009sort$Wi<-sqrt(hdi2009sort$population)
hdi2009sort$XiWi<-hdi2009sort$Wi*hdi2009sort$logridit
hdi2009sort$YiWi<-hdi2009sort$Wi*hdi2009sort$ir_tb
fit2009<-lm(hdi2009sort$YiWi~hdi2009sort$Wi + hdi2009sort$XiWi + 0)
summary(fit2009)
hdi2009sort$predict2009<-coef(summary(fit2009))[1,1] + coef(summary(fit2009))[2,1]*hdi2009sort$logridit

hdi2005<-as.data.frame(df[df$year=="2005",])
hdi2005sort<-hdi2005[order(hdi2005$hdi),]
totalp=sum(hdi2005sort$population)
totaltb=sum(hdi2005sort$num_tb_cases)
hdi2005sort$Wpop<-hdi2005sort$population/totalp
hdi2005sort$CWpop<-cumsum(hdi2005sort$Wpop)
hdi2005sort$ridit<-c((0+hdi2005sort$CWpop[1])/2,(hdi2005sort$CWpop[1]+hdi2005sort$CWpop[2])/2,
                     (hdi2005sort$CWpop[2]+hdi2005sort$CWpop[3])/2,(hdi2005sort$CWpop[3]+hdi2005sort$CWpop[4])/2,
                     (hdi2005sort$CWpop[4]+hdi2005sort$CWpop[5])/2,(hdi2005sort$CWpop[5]+hdi2005sort$CWpop[6])/2,
                     (hdi2005sort$CWpop[6]+hdi2005sort$CWpop[7])/2,(hdi2005sort$CWpop[7]+hdi2005sort$CWpop[8])/2,
                     (hdi2005sort$CWpop[8]+hdi2005sort$CWpop[9])/2,(hdi2005sort$CWpop[9]+hdi2005sort$CWpop[10])/2,
                     (hdi2005sort$CWpop[10]+hdi2005sort$CWpop[11])/2,(hdi2005sort$CWpop[11]+hdi2005sort$CWpop[12])/2,
                     (hdi2005sort$CWpop[12]+hdi2005sort$CWpop[13])/2,(hdi2005sort$CWpop[13]+hdi2005sort$CWpop[14])/2,
                     (hdi2005sort$CWpop[14]+hdi2005sort$CWpop[15])/2,(hdi2005sort$CWpop[15]+hdi2005sort$CWpop[16])/2,
                     (hdi2005sort$CWpop[16]+hdi2005sort$CWpop[17])/2,(hdi2005sort$CWpop[17]+hdi2005sort$CWpop[18])/2,
                     (hdi2005sort$CWpop[18]+hdi2005sort$CWpop[19])/2,(hdi2005sort$CWpop[19]+hdi2005sort$CWpop[20])/2,
                     (hdi2005sort$CWpop[20]+hdi2005sort$CWpop[21])/2,(hdi2005sort$CWpop[21]+hdi2005sort$CWpop[22])/2,
                     (hdi2005sort$CWpop[22]+hdi2005sort$CWpop[23])/2)

hdi2005sort$Whealth<-hdi2005sort$num_tb_cases/totaltb
hdi2005sort$CWhealth<-cumsum(hdi2005sort$Whealth)
hdi2005sort$logridit<-log10(hdi2005sort$ridit)
hdi2005sort$Wi<-sqrt(hdi2005sort$population)
hdi2005sort$XiWi<-hdi2005sort$Wi*hdi2005sort$logridit
hdi2005sort$YiWi<-hdi2005sort$Wi*hdi2005sort$ir_tb
fit2005<-lm(hdi2005sort$YiWi~hdi2005sort$Wi + hdi2005sort$XiWi + 0)
summary(fit2005)
hdi2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*hdi2005sort$logridit


hdi2000<-as.data.frame(df[df$year=="2000",])
hdi2000sort<-hdi2000[order(hdi2000$hdi),]
totalp=sum(hdi2000sort$population)
totaltb=sum(hdi2000sort$num_tb_cases)
hdi2000sort$Wpop<-hdi2000sort$population/totalp
hdi2000sort$CWpop<-cumsum(hdi2000sort$Wpop)
hdi2000sort$ridit<-c((0+hdi2000sort$CWpop[1])/2,(hdi2000sort$CWpop[1]+hdi2000sort$CWpop[2])/2,
                     (hdi2000sort$CWpop[2]+hdi2000sort$CWpop[3])/2,(hdi2000sort$CWpop[3]+hdi2000sort$CWpop[4])/2,
                     (hdi2000sort$CWpop[4]+hdi2000sort$CWpop[5])/2,(hdi2000sort$CWpop[5]+hdi2000sort$CWpop[6])/2,
                     (hdi2000sort$CWpop[6]+hdi2000sort$CWpop[7])/2,(hdi2000sort$CWpop[7]+hdi2000sort$CWpop[8])/2,
                     (hdi2000sort$CWpop[8]+hdi2000sort$CWpop[9])/2,(hdi2000sort$CWpop[9]+hdi2000sort$CWpop[10])/2,
                     (hdi2000sort$CWpop[10]+hdi2000sort$CWpop[11])/2,(hdi2000sort$CWpop[11]+hdi2000sort$CWpop[12])/2,
                     (hdi2000sort$CWpop[12]+hdi2000sort$CWpop[13])/2,(hdi2000sort$CWpop[13]+hdi2000sort$CWpop[14])/2,
                     (hdi2000sort$CWpop[14]+hdi2000sort$CWpop[15])/2,(hdi2000sort$CWpop[15]+hdi2000sort$CWpop[16])/2,
                     (hdi2000sort$CWpop[16]+hdi2000sort$CWpop[17])/2,(hdi2000sort$CWpop[17]+hdi2000sort$CWpop[18])/2,
                     (hdi2000sort$CWpop[18]+hdi2000sort$CWpop[19])/2,(hdi2000sort$CWpop[19]+hdi2000sort$CWpop[20])/2,
                     (hdi2000sort$CWpop[20]+hdi2000sort$CWpop[21])/2,(hdi2000sort$CWpop[21]+hdi2000sort$CWpop[22])/2,
                     (hdi2000sort$CWpop[22]+hdi2000sort$CWpop[23])/2)

hdi2000sort$Whealth<-hdi2000sort$num_tb_cases/totaltb
hdi2000sort$CWhealth<-cumsum(hdi2000sort$Whealth)
hdi2000sort$logridit<-log10(hdi2000sort$ridit)
hdi2000sort$Wi<-sqrt(hdi2000sort$population)
hdi2000sort$XiWi<-hdi2000sort$Wi*hdi2000sort$logridit
hdi2000sort$YiWi<-hdi2000sort$Wi*hdi2000sort$ir_tb
fit2000<-lm(hdi2000sort$YiWi~hdi2000sort$Wi + hdi2000sort$XiWi + 0)
summary(fit2000)
hdi2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*hdi2000sort$logridit

quartz(width=10, height=6, pointsize=10)
plot(hdi2013sort$ridit,hdi2013sort$ir_tb, col="deepskyblue",pch=0,
     ylab="Tasa de incidencia de tuberculosis (por 100,000 hab)", 
     xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)")
points(hdi2009sort$ridit,hdi2009sort$ir_tb, col="dodgerblue",pch=1,
       ylab="", 
       xlab="")
points(hdi2005sort$ridit,hdi2005sort$ir_tb, col="dodgerblue3",pch=2,
       ylab="", 
       xlab="")
points(hdi2000sort$ridit,hdi2000sort$ir_tb, col="deepskyblue4",pch=3,
       ylab="", 
       xlab="")
lines(hdi2013sort$ridit,hdi2013sort$predict2013, col="deepskyblue", lty=1,
      ylab="", 
      xlab="")
lines(hdi2009sort$ridit,hdi2009sort$predict2009, col="dodgerblue", lty=2,
      ylab="", 
      xlab="")
lines(hdi2005sort$ridit,hdi2005sort$predict2005, col="dodgerblue3", lty=3,
      ylab="", 
      xlab="")
lines(hdi2000sort$ridit,hdi2000sort$predict2000, col="deepskyblue4", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2013","2009","2005","2000"),col=c("deepskyblue","dodgerblue","dodgerblue3","deepskyblue4"),lty=c(1,2,3,4),pch=c(0,1,2,3),cex = .8)
text(0.8,150, "Índice de desigualdad de la pendiente (IDP)", col="red")
text(0.8,140, "2000=-55.1", col="red")
text(0.8,130, "2005=-71.2", col="red")
text(0.8,120, "2009=-61.5", col="red")
text(0.8,110, "2013=-53.5", col="red")

# Social gradient
slope_index_of_inequality_hdi2000<-fit2000$coefficients[2]
slope_index_of_inequality_hdi2005<-fit2005$coefficients[2]
slope_index_of_inequality_hdi2009<-fit2009$coefficients[2]
slope_index_of_inequality_hdi2013<-fit2013$coefficients[2]
round(slope_index_of_inequality_hdi2000,2)
round(slope_index_of_inequality_hdi2005,2)
round(slope_index_of_inequality_hdi2009,2)
round(slope_index_of_inequality_hdi2013,2)



######################################################################################
######################  Quantiles of Human Development Index  ########################


hdi2013sort$qhdi<-cut(hdi2013sort$hdi,quantile(hdi2013sort$hdi),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2009sort$qhdi<-cut(hdi2009sort$hdi,quantile(hdi2009sort$hdi),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2005sort$qhdi<-cut(hdi2005sort$hdi,quantile(hdi2005sort$hdi),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2000sort$qhdi<-cut(hdi2000sort$hdi,quantile(hdi2000sort$hdi),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))

list(hdi2013sort$country,hdi2013sort$qhdi)
list(hdi2009sort$country,hdi2009sort$qhdi)
list(hdi2005sort$country,hdi2005sort$qhdi)
list(hdi2000sort$country,hdi2000sort$qhdi)

qpg2013<-sapply(split(hdi2013sort$population,hdi2013sort$qhdi),sum)
qpg2009<-sapply(split(hdi2009sort$population,hdi2009sort$qhdi),sum)
qpg2005<-sapply(split(hdi2005sort$population,hdi2005sort$qhdi),sum)
qpg2000<-sapply(split(hdi2000sort$population,hdi2000sort$qhdi),sum)

wpopg2013<-c(qpg2013[1]/sum(qpg2013),qpg2013[2]/sum(qpg2013),qpg2013[3]/sum(qpg2013),qpg2013[4]/sum(qpg2013))
wpopg2009<-c(qpg2009[1]/sum(qpg2009),qpg2009[2]/sum(qpg2009),qpg2009[3]/sum(qpg2009),qpg2009[4]/sum(qpg2009))
wpopg2005<-c(qpg2005[1]/sum(qpg2005),qpg2005[2]/sum(qpg2005),qpg2005[3]/sum(qpg2005),qpg2005[4]/sum(qpg2005))
wpopg2000<-c(qpg2000[1]/sum(qpg2000),qpg2000[2]/sum(qpg2000),qpg2000[3]/sum(qpg2000),qpg2000[4]/sum(qpg2000))

hdi2013sort$wpop2013<-ifelse(hdi2013sort$qhdi=="Q1", hdi2013sort$population/qpg2013[1],0)
hdi2013sort$wpop2013<-ifelse(hdi2013sort$qhdi=="Q2", hdi2013sort$population/qpg2013[2],hdi2013sort$wpop2013)
hdi2013sort$wpop2013<-ifelse(hdi2013sort$qhdi=="Q3", hdi2013sort$population/qpg2013[3],hdi2013sort$wpop2013)
hdi2013sort$wpop2013<-ifelse(hdi2013sort$qhdi=="Q4", hdi2013sort$population/qpg2013[4],hdi2013sort$wpop2013)

hdi2009sort$wpop2009<-ifelse(hdi2009sort$qhdi=="Q1", hdi2009sort$population/qpg2009[1],0)
hdi2009sort$wpop2009<-ifelse(hdi2009sort$qhdi=="Q2", hdi2009sort$population/qpg2009[2],hdi2009sort$wpop2009)
hdi2009sort$wpop2009<-ifelse(hdi2009sort$qhdi=="Q3", hdi2009sort$population/qpg2009[3],hdi2009sort$wpop2009)
hdi2009sort$wpop2009<-ifelse(hdi2009sort$qhdi=="Q4", hdi2009sort$population/qpg2009[4],hdi2009sort$wpop2009)

hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q1", hdi2005sort$population/qpg2005[1],0)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q2", hdi2005sort$population/qpg2005[2],hdi2005sort$wpop2005)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q3", hdi2005sort$population/qpg2005[3],hdi2005sort$wpop2005)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q4", hdi2005sort$population/qpg2005[4],hdi2005sort$wpop2005)

hdi2000sort$wpop2000<-ifelse(hdi2000sort$qhdi=="Q1", hdi2000sort$population/qpg2000[1],0)
hdi2000sort$wpop2000<-ifelse(hdi2000sort$qhdi=="Q2", hdi2000sort$population/qpg2000[2],hdi2000sort$wpop2000)
hdi2000sort$wpop2000<-ifelse(hdi2000sort$qhdi=="Q3", hdi2000sort$population/qpg2000[3],hdi2000sort$wpop2000)
hdi2000sort$wpop2000<-ifelse(hdi2000sort$qhdi=="Q4", hdi2000sort$population/qpg2000[4],hdi2000sort$wpop2000)

hdi2013sort$wrate<-hdi2013sort$wpop2013*hdi2013sort$ir_tb
hdi2009sort$wrate<-hdi2009sort$wpop2009*hdi2009sort$ir_tb
hdi2005sort$wrate<-hdi2005sort$wpop2005*hdi2005sort$ir_tb
hdi2000sort$wrate<-hdi2000sort$wpop2000*hdi2000sort$ir_tb

meang2013<-sapply(split(hdi2013sort$wrate,hdi2013sort$qhdi),sum)
meang2013

meang2009<-sapply(split(hdi2009sort$wrate,hdi2009sort$qhdi),sum)
meang2009

meang2005<-sapply(split(hdi2005sort$wrate,hdi2005sort$qhdi),sum)
meang2005

meang2000<-sapply(split(hdi2000sort$wrate,hdi2000sort$qhdi),sum)
meang2000



Q1<-c(meang2000[1],meang2005[1],meang2009[1],meang2013[1]) 
Q1<-round(Q1,2)
Q2<-c(meang2000[2],meang2005[2],meang2009[2],meang2013[2]) 
Q2<-round(Q2,2)
Q3<-c(meang2000[3],meang2005[3],meang2009[3],meang2013[3]) 
Q3<-round(Q3,2)
Q4<-c(meang2000[4],meang2005[4],meang2009[4],meang2013[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4)
r<-t(r)
rownames(r)<-c('2000','2005','2009','2013')
colnames(r)<-c('Q1','Q2','Q3','Q4')
r

quartz(width=10, height=6, pointsize=10)
b<-barplot(t(r),col=c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),beside=T,ylim=c(0,220),
           xlab="Cuartíles del Indice de Desarrollo Humano (IDH)", ylab="Tasa promedio de incidencia de TB (por 100,000 hb)")
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),pch=15,bty="n") 
text(x=c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5,16.5,17.5,18.5,19.5),
     y=c(114.17, 127.28, 94.64, 88.31,76.76,51.52,57.40,50.66,58.53,47.67,45.47,42.77,31.36,26.30,25.27,21.50),
     labels=c(114.17, 127.28, 94.64, 88.31,76.76,51.52,57.40,50.66,58.53,47.67,45.47,42.77,31.36,26.30,25.27,21.50),cex=1.25,pos=3)
text(10,200, "Índice de Kuznets absoluto", col="red")
text(10,190, "2000=-82.8", col="red")
text(10,180, "2005=-100.98", col="red")
text(10,170, "2009=-69.37", col="red")
text(10,160, "2013=-66.81", col="red")
text(10,150, "Índice de Kuznets relativo", col="red")
text(10,140, "2000=3.64", col="red")
text(10,130, "2005=4.84", col="red")
text(10,120, "2009=3.75", col="red")
text(10,110, "2013=4.11", col="red")

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_hdi2000<-sum(wpopg2000*meang2000)
regional_mean_rate_hdi2005<-sum(wpopg2005*meang2005)
regional_mean_rate_hdi2009<-sum(wpopg2009*meang2009)
regional_mean_rate_hdi2013<-sum(wpopg2013*meang2013)
round(regional_mean_rate_hdi2000,2)
round(regional_mean_rate_hdi2005,2)
round(regional_mean_rate_hdi2009,2)
round(regional_mean_rate_hdi2013,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_hdi2000<-meang2000[4]-meang2000[1]
absolute_Kuznets_index_hdi2005<-meang2005[4]-meang2005[1]
absolute_Kuznets_index_hdi2009<-meang2009[4]-meang2009[1]
absolute_Kuznets_index_hdi2013<-meang2013[4]-meang2013[1]
round(absolute_Kuznets_index_hdi2000,2)
round(absolute_Kuznets_index_hdi2005,2)
round(absolute_Kuznets_index_hdi2009,2)
round(absolute_Kuznets_index_hdi2013,2)

relative_Kuznets_index_hdi2000<-meang2000[1]/meang2000[4]
relative_Kuznets_index_hdi2005<-meang2005[1]/meang2005[4]
relative_Kuznets_index_hdi2009<-meang2009[1]/meang2009[4]
relative_Kuznets_index_hdi2013<-meang2013[1]/meang2013[4]
round(relative_Kuznets_index_hdi2000,2)
round(relative_Kuznets_index_hdi2005,2)
round(relative_Kuznets_index_hdi2009,2)
round(relative_Kuznets_index_hdi2013,2)



##########################################################################
############################# Concentration curve  #######################


CWpopf2013<-c(0,hdi2013sort$CWpop)
CWhealthf2013<-c(0,hdi2013sort$CWhealth)
CWpopf2009<-c(0,hdi2009sort$CWpop)
CWhealthf2009<-c(0,hdi2009sort$CWhealth)
CWpopf2005<-c(0,hdi2005sort$CWpop)
CWhealthf2005<-c(0,hdi2005sort$CWhealth)
CWpopf2000<-c(0,hdi2000sort$CWpop)
CWhealthf2000<-c(0,hdi2000sort$CWhealth)


ccurve2013<-data.frame(y=CWhealthf2013, x=CWpopf2013)
ccurve2009<-data.frame(y=CWhealthf2009, x=CWpopf2009)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2013 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2013, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2013)

ccurve.optx2009 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2009, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2009)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

k<-ccurve.optx2013[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02013<-f(x,k)
delta_x_y<-x-lf02013
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2013<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2013,2)


k<-ccurve.optx2009[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02009<-f(x,k)

delta_x_y<-x-lf02009
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2009<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2009,2)


k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2005,2)


k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2000,2)

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2013,CWhealthf2013, col="deepskyblue",pch=0, xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)", ylab="Número de casos incidentes de TB (acumulado)")
points(CWpopf2009,CWhealthf2009, col="dodgerblue",pch=1)
points(CWpopf2005,CWhealthf2005, col="dodgerblue3",pch=2)
points(CWpopf2000,CWhealthf2000, col="deepskyblue4",pch=3)
lines(x,lf02013,col="deepskyblue", lty=1)
lines(x,lf02009,col="dodgerblue", lty=2)
lines(x,lf02005,col="dodgerblue3", lty=3)
lines(x,lf02000,col="deepskyblue4", lty=4)
lines(x,x)
legend(locator(1),c("2013","2009","2005","2000"),col=c("deepskyblue","dodgerblue","dodgerblue3","deepskyblue4"),pch=c(0,1,2,3), lty=c(1,2,3,4),cex = .8)
text(0.8,0.25, "Índices de concentración de salud (IC)", col="red")
text(0.8,0.22, "2000=-0.22", col="red")
text(0.8,0.18, "2005=-0.30", col="red")
text(0.8,0.15, "2009=-0.28", col="red")
text(0.8,0.12, "2013=-0.26", col="red")


write.csv(hdi2000,"hdi2000.csv")
write.csv(hdi2005,"hdi2005.csv")
write.csv(hdi2009,"hdi2009.csv")
write.csv(hdi2013,"hdi2013.csv")

