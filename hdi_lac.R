library(gdata)
library(epicalc)
library(rgl)
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


# Social gradient
slope_index_of_inequality_hdi2000<-fit2000$coefficients[2]
slope_index_of_inequality_hdi2005<-fit2005$coefficients[2]
slope_index_of_inequality_hdi2009<-fit2009$coefficients[2]
slope_index_of_inequality_hdi2013<-fit2013$coefficients[2]
round(slope_index_of_inequality_hdi2000,2)
round(slope_index_of_inequality_hdi2005,2)
round(slope_index_of_inequality_hdi2009,2)
round(slope_index_of_inequality_hdi2013,2)

mylabel1a= bquote(2000==.(format(slope_index_of_inequality_hdi2000,digits=4))) 
mylabel2a= bquote(2005==.(format(slope_index_of_inequality_hdi2005,digits=4))) 
mylabel3a= bquote(2009==.(format(slope_index_of_inequality_hdi2009,digits=4))) 
mylabel4a= bquote(2013==.(format(slope_index_of_inequality_hdi2013,digits=4))) 


quartz(width=10, height=6, pointsize=10)
plot(hdi2013sort$ridit,hdi2013sort$ir_tb, col="red",pch=0,
     ylab="Tasa de incidencia de tuberculosis (por 100,000 hab)", 
     xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)")
points(hdi2009sort$ridit,hdi2009sort$ir_tb, col="blue",pch=1,
       ylab="", 
       xlab="")
points(hdi2005sort$ridit,hdi2005sort$ir_tb, col="green",pch=2,
       ylab="", 
       xlab="")
points(hdi2000sort$ridit,hdi2000sort$ir_tb, col="purple",pch=3,
       ylab="", 
       xlab="")
lines(hdi2013sort$ridit,hdi2013sort$predict2013, col="red", lty=1,
      ylab="", 
      xlab="")
lines(hdi2009sort$ridit,hdi2009sort$predict2009, col="blue", lty=2,
      ylab="", 
      xlab="")
lines(hdi2005sort$ridit,hdi2005sort$predict2005, col="green", lty=3,
      ylab="", 
      xlab="")
lines(hdi2000sort$ridit,hdi2000sort$predict2000, col="purple", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2013","2009","2005","2000"),col=c("red","blue","green","purple"),lty=c(1,2,3,4),pch=c(0,1,2,3),cex = .8)
text(0.8,150, "Índice de desigualdad de la pendiente (IDP)", col="red")
text(0.8,140, labels=mylabel1a, col="red")
text(0.8,130, labels=mylabel2a, col="red")
text(0.8,120, labels=mylabel3a, col="red")
text(0.8,110, labels=mylabel4a, col="red")




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
#r<-t(r)
rownames(r)<-c('2000','2005','2009','2013')
colnames(r)<-c('Q1','Q2','Q3','Q4')
r

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
absolute_Kuznets_index_hdi2000<-meang2000[1]-meang2000[4]
absolute_Kuznets_index_hdi2005<-meang2005[1]-meang2005[4]
absolute_Kuznets_index_hdi2009<-meang2009[1]-meang2009[4]
absolute_Kuznets_index_hdi2013<-meang2013[1]-meang2013[4]
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

mylabel1a= bquote(2000==.(format(absolute_Kuznets_index_hdi2000,digits=4))) 
mylabel2a= bquote(2005==.(format(absolute_Kuznets_index_hdi2005,digits=6))) 
mylabel3a= bquote(2009==.(format(absolute_Kuznets_index_hdi2009,digits=4))) 
mylabel4a= bquote(2013==.(format(absolute_Kuznets_index_hdi2013,digits=4))) 
mylabel5a= bquote(2000==.(format(relative_Kuznets_index_hdi2000,digits=3))) 
mylabel6a= bquote(2005==.(format(relative_Kuznets_index_hdi2005,digits=3))) 
mylabel7a= bquote(2009==.(format(relative_Kuznets_index_hdi2009,digits=3))) 
mylabel8a= bquote(2013==.(format(relative_Kuznets_index_hdi2013,digits=3))) 


quartz(width=10, height=6, pointsize=10)
b<-barplot(t(r),col=c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),beside=T,ylim=c(0,220),
           xlab="Cuartíles del Indice de Desarrollo Humano (IDH)", ylab="Tasa promedio de incidencia de TB (por 100,000 hb)")
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),pch=15,bty="n") 
text(x=c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5,16.5,17.5,18.5,19.5),
     y=c(114.17,76.76,58.53,31.36,127.28,51.52,47.67,26.30,94.64,57.40,45.47,25.27,88.31,50.66,42.77,21.50),
     labels=c(114.17,76.76,58.53,31.36,127.28,51.52,47.67,26.30,94.64,57.40,45.47,25.27,88.31,50.66,42.77,21.50),cex=1.25,pos=3)
text(10,200, "Índice de Kuznets absoluto", col="red")
text(10,190, labels=mylabel1a, col="red")
text(10,180, labels=mylabel2a, col="red")
text(10,170, labels=mylabel3a, col="red")
text(10,160, labels=mylabel4a, col="red")
text(10,150, "Índice de Kuznets relativo", col="red")
text(10,140, labels=mylabel5a, col="red")
text(10,130, labels=mylabel6a, col="red")
text(10,120, labels=mylabel7a, col="red")
text(10,110, labels=mylabel8a, col="red")




## lines and dots graphics
quartz(width=10, height=6, pointsize=10)
plot(r[1,],c(2000,2000,2000,2000),bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
ticks = c(2000, 2005, 2009, 2013)
axis(side = 2, at = ticks)
from.x <- c(114.17, 76.76, 58.53)
to.x   <- c(76.76, 58.53-1, 31.36)
to.y   <- from.y <- c(2000, 2000, 2000,2000)
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[2,],c(2005,2005,2005,2005), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(127.28, 51.52, 47.67, 26.30)
to.x   <- c(51.52, 47.67, 26.30)
to.y   <- from.y <- c(2005, 2005, 2005,2005) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[3,],c(2009,2009,2009,2009), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(94.64, 57.40, 45.47)
to.x   <- c(57.40, 45.47, 25.27)
to.y   <- from.y <- c(2009, 2009, 2009,2009) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[4,],c(2013,2013,2013,2013), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="") 
from.x <- c(88.31, 50.66, 42.77)
to.x   <- c(50.66, 42.77, 21.50)
to.y   <- from.y <- c(2013, 2013, 2013,2013) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= rainbow(4),
       bg=rainbow(4), pch=19,bty="n", cex=1.5) 



## lines and dots graphics weighted
quartz(width=10, height=6, pointsize=10)
symbols(r[1,],c(2000,2000,2000,2000),circles=sqrt(qpg2000/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[1,],c(2000,2000,2000,2000),bg="black", pch=21, cex=0.5, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
ticks = c(2000, 2005, 2009, 2013)
axis(side = 2, at = ticks)
from.x <- c(114.17, 76.76, 58.53)
to.x   <- c(76.76, 58.53-1, 31.36)
to.y   <- from.y <- c(2000, 2000, 2000,2000)
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[2,],c(2005,2005,2005,2005),circles=sqrt(qpg2005/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[2,],c(2005,2005,2005,2005), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(127.28, 51.52, 47.67, 26.30)
to.x   <- c(51.52, 47.67, 26.30)
to.y   <- from.y <- c(2005, 2005, 2005,2005) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[3,],c(2009,2009,2009,2009),circles=sqrt(qpg2009/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[3,],c(2009,2009,2009,2009), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(94.64, 57.40, 45.47)
to.x   <- c(57.40, 45.47, 25.27)
to.y   <- from.y <- c(2009, 2009, 2009,2009) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[4,],c(2013,2013,2013,2013),circles=sqrt(qpg2013/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[4,],c(2013,2013,2013,2013), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="") 
from.x <- c(88.31, 50.66, 42.77)
to.x   <- c(50.66, 42.77, 21.50)
to.y   <- from.y <- c(2013, 2013, 2013,2013) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
legend("topright",title="Population size",c("Q1","Q2","Q3","Q4"),
       col= rainbow(4),
       bg=rainbow(4), pch=19,bty="n", cex=1.5,
       pt.cex=c(1,1.4,3,1.4)) 



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


mylabel1b= bquote(2000==.(format(health_concentration_index_hdi2000,digits=2))) 
mylabel2b= bquote(2005==.(format(health_concentration_index_hdi2005,digits=2))) 
mylabel3b= bquote(2009==.(format(health_concentration_index_hdi2009,digits=2))) 
mylabel4b= bquote(2013==.(format(health_concentration_index_hdi2013,digits=2))) 

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2013,CWhealthf2013, col="red",pch=0, xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)", ylab="Número de casos incidentes de TB (acumulado)")
points(CWpopf2009,CWhealthf2009, col="blue",pch=1)
points(CWpopf2005,CWhealthf2005, col="green",pch=2)
points(CWpopf2000,CWhealthf2000, col="purple",pch=3)
lines(x,lf02013,col="red", lty=1)
lines(x,lf02009,col="blue", lty=2)
lines(x,lf02005,col="green", lty=3)
lines(x,lf02000,col="purple", lty=4)
lines(x,x)
legend(locator(1),c("2013","2009","2005","2000"),col=c("red","blue","green","purple"),pch=c(0,1,2,3), lty=c(1,2,3,4),cex = .8)
text(0.8,0.25, "Índices de concentración de salud (IC)", col="red")
text(0.8,0.22, labels=mylabel1b, col="red")
text(0.8,0.18, labels=mylabel2b, col="red")
text(0.8,0.15, labels=mylabel3b, col="red")
text(0.8,0.12, labels=mylabel4b, col="red")


write.csv(hdi2000,"hdi2000.csv")
write.csv(hdi2005,"hdi2005.csv")
write.csv(hdi2009,"hdi2009.csv")
write.csv(hdi2013,"hdi2013.csv")



#####################################################################################
######################## Data mining World Bank Dataset ###########################
#### Cargando bases de datos 

dat1<-read.csv("se.adt.litr.zs_Indicator.csv", skip = 2)
dat2<-read.csv("sh.h2o.safe.zs_Indicator.csv", skip = 2)
dat3<-read.csv("sh.h2o.safe.ur.zs_Indicator.csv", skip = 2)
dat4<-read.csv("sh.sta.acsn_Indicator.csv", skip = 2)
dat5<-read.csv("sh.sta.acsn.ur_Indicator.csv", skip = 2)

##### literacy 
litrate_data<-subset(dat1,select=-c(Indicator.Name,Country.Code,Indicator.Code, X1960,
                                          X1961,X1962,X1963,X1964,X1965,X1966,
                                          X1967,X1968,X1969,X1970,X1971,X1972,
                                          X1973,X1974,X1975,X1976,X1977,X1978,
                                          X1979,X1980,X1981,X1982,X1983,X1984,
                                          X1985,X1986,X1987,X1988,X1989,X2013,X2014))
litrate_world<-melt(litrate_data, Country=c("Country.Name"))
litrate_world$year<-as.numeric(substr(litrate_world$variable,2,5))
litrate_world$litrate<-litrate_world$value
litrate_world<-subset(litrate_world,select=-c(variable,value))
litrate_world$Country.Name<-as.character(litrate_world$Country.Name)
litrate_world <- rename(litrate_world, c(Country.Name="country"))

litrate_world$country <- revalue(litrate_world$country, c("Venezuela, RB"="Venezuela"))

litrate_lac_f<-subset(litrate_world,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                           "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                           "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                           "Uruguay","Venezuela"))

litrate_lac_f <- litrate_lac_f[order(litrate_lac_f$country,litrate_lac_f$year),] 


#######################################################################################
####################### Health inequality for improved water  #########################
#######################################################################################

impwater_data<-subset(dat2,select=-c(Indicator.Name,Country.Code, Indicator.Code, X1960,
                                          X1961,X1962,X1963,X1964,X1965,X1966,
                                          X1967,X1968,X1969,X1970,X1971,X1972,
                                          X1973,X1974,X1975,X1976,X1977,X1978,
                                          X1979,X1980,X1981,X1982,X1983,X1984,
                                          X1985,X1986,X1987,X1988,X1989,X2013,X2014))

impwater_world<-melt(impwater_data, Country=c("Country.Name"))
impwater_world$year<-as.numeric(substr(impwater_world$variable,2,5))
impwater_world$impwater<-impwater_world$value
impwater_world<-subset(impwater_world,select=-c(variable,value))
impwater_world$Country.Name<-as.character(impwater_world$Country.Name)
impwater_world <- rename(impwater_world, c(Country.Name="country"))

impwater_world$country <- revalue(impwater_world$country, c("Venezuela, RB"="Venezuela"))

impwater_lac_f<-subset(impwater_world,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                                   "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                                   "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                                   "Uruguay","Venezuela"))

impwater_lac_f <- impwater_lac_f[order(impwater_lac_f$country,impwater_lac_f$year),] 


################# Building the final dataset ############################  
impwater_lac_fff<-subset(impwater_lac_f,year %in% c(2000,2005,2009,2012)) ## we used the same value of 2005 impwater for 2000 
tb_lac_fff<-subset(tb_lac_ff,year %in% c(2000,2005,2009,2012))

df2<-merge(tb_lac_fff,impwater_lac_fff,by=c("country","year"),all=TRUE)

lengend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba","Dominican Republic",
            "Ecuador","El Salvador","Guatemala ","Guyana","Haiti","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Trinidad and Tobago","Uruguay","Venezuela")


############################################################################
############################ regression weight analysis ####################

impwater2012<-as.data.frame(df2[df2$year=="2012",])

impwater2012sort<-impwater2012[order(impwater2012$impwater),]
totalp=sum(impwater2012sort$population)
totaltb=sum(impwater2012sort$num_tb_cases)
impwater2012sort$Wpop<-impwater2012sort$population/totalp
impwater2012sort$CWpop<-cumsum(impwater2012sort$Wpop)
impwater2012sort$ridit<-c((0+impwater2012sort$CWpop[1])/2,(impwater2012sort$CWpop[1]+impwater2012sort$CWpop[2])/2,
                     (impwater2012sort$CWpop[2]+impwater2012sort$CWpop[3])/2,(impwater2012sort$CWpop[3]+impwater2012sort$CWpop[4])/2,
                     (impwater2012sort$CWpop[4]+impwater2012sort$CWpop[5])/2,(impwater2012sort$CWpop[5]+impwater2012sort$CWpop[6])/2,
                     (impwater2012sort$CWpop[6]+impwater2012sort$CWpop[7])/2,(impwater2012sort$CWpop[7]+impwater2012sort$CWpop[8])/2,
                     (impwater2012sort$CWpop[8]+impwater2012sort$CWpop[9])/2,(impwater2012sort$CWpop[9]+impwater2012sort$CWpop[10])/2,
                     (impwater2012sort$CWpop[10]+impwater2012sort$CWpop[11])/2,(impwater2012sort$CWpop[11]+impwater2012sort$CWpop[12])/2,
                     (impwater2012sort$CWpop[12]+impwater2012sort$CWpop[13])/2,(impwater2012sort$CWpop[13]+impwater2012sort$CWpop[14])/2,
                     (impwater2012sort$CWpop[14]+impwater2012sort$CWpop[15])/2,(impwater2012sort$CWpop[15]+impwater2012sort$CWpop[16])/2,
                     (impwater2012sort$CWpop[16]+impwater2012sort$CWpop[17])/2,(impwater2012sort$CWpop[17]+impwater2012sort$CWpop[18])/2,
                     (impwater2012sort$CWpop[18]+impwater2012sort$CWpop[19])/2,(impwater2012sort$CWpop[19]+impwater2012sort$CWpop[20])/2,
                     (impwater2012sort$CWpop[20]+impwater2012sort$CWpop[21])/2,(impwater2012sort$CWpop[21]+impwater2012sort$CWpop[22])/2,
                     (impwater2012sort$CWpop[22]+impwater2012sort$CWpop[23])/2)


impwater2012sort$Whealth<-impwater2012sort$num_tb_cases/totaltb
impwater2012sort$CWhealth<-cumsum(impwater2012sort$Whealth)
impwater2012sort$logridit<-log10(impwater2012sort$ridit)
impwater2012sort$Wi<-sqrt(impwater2012sort$population)
impwater2012sort$XiWi<-impwater2012sort$Wi*impwater2012sort$logridit
impwater2012sort$YiWi<-impwater2012sort$Wi*impwater2012sort$ir_tb
fit2012<-lm(impwater2012sort$YiWi~impwater2012sort$Wi + impwater2012sort$XiWi + 0)
summary(fit2012)
impwater2012sort$predict2012<-coef(summary(fit2012))[1,1] + coef(summary(fit2012))[2,1]*impwater2012sort$logridit



impwater2009<-as.data.frame(df[df$year=="2009",])
impwater2009sort<-impwater2009[order(impwater2009$impwater),]
totalp=sum(impwater2009sort$population)
totaltb=sum(impwater2009sort$num_tb_cases)
impwater2009sort$Wpop<-impwater2009sort$population/totalp
impwater2009sort$CWpop<-cumsum(impwater2009sort$Wpop)
impwater2009sort$ridit<-c((0+impwater2009sort$CWpop[1])/2,(impwater2009sort$CWpop[1]+impwater2009sort$CWpop[2])/2,
                     (impwater2009sort$CWpop[2]+impwater2009sort$CWpop[3])/2,(impwater2009sort$CWpop[3]+impwater2009sort$CWpop[4])/2,
                     (impwater2009sort$CWpop[4]+impwater2009sort$CWpop[5])/2,(impwater2009sort$CWpop[5]+impwater2009sort$CWpop[6])/2,
                     (impwater2009sort$CWpop[6]+impwater2009sort$CWpop[7])/2,(impwater2009sort$CWpop[7]+impwater2009sort$CWpop[8])/2,
                     (impwater2009sort$CWpop[8]+impwater2009sort$CWpop[9])/2,(impwater2009sort$CWpop[9]+impwater2009sort$CWpop[10])/2,
                     (impwater2009sort$CWpop[10]+impwater2009sort$CWpop[11])/2,(impwater2009sort$CWpop[11]+impwater2009sort$CWpop[12])/2,
                     (impwater2009sort$CWpop[12]+impwater2009sort$CWpop[13])/2,(impwater2009sort$CWpop[13]+impwater2009sort$CWpop[14])/2,
                     (impwater2009sort$CWpop[14]+impwater2009sort$CWpop[15])/2,(impwater2009sort$CWpop[15]+impwater2009sort$CWpop[16])/2,
                     (impwater2009sort$CWpop[16]+impwater2009sort$CWpop[17])/2,(impwater2009sort$CWpop[17]+impwater2009sort$CWpop[18])/2,
                     (impwater2009sort$CWpop[18]+impwater2009sort$CWpop[19])/2,(impwater2009sort$CWpop[19]+impwater2009sort$CWpop[20])/2,
                     (impwater2009sort$CWpop[20]+impwater2009sort$CWpop[21])/2,(impwater2009sort$CWpop[21]+impwater2009sort$CWpop[22])/2,
                     (impwater2009sort$CWpop[22]+impwater2009sort$CWpop[23])/2)
impwater2009sort$Whealth<-impwater2009sort$num_tb_cases/totaltb
impwater2009sort$CWhealth<-cumsum(impwater2009sort$Whealth)
impwater2009sort$logridit<-log10(impwater2009sort$ridit)
impwater2009sort$Wi<-sqrt(impwater2009sort$population)
impwater2009sort$XiWi<-impwater2009sort$Wi*impwater2009sort$logridit
impwater2009sort$YiWi<-impwater2009sort$Wi*impwater2009sort$ir_tb
fit2009<-lm(impwater2009sort$YiWi~impwater2009sort$Wi + impwater2009sort$XiWi + 0)
summary(fit2009)
impwater2009sort$predict2009<-coef(summary(fit2009))[1,1] + coef(summary(fit2009))[2,1]*impwater2009sort$logridit

impwater2005<-as.data.frame(df[df$year=="2005",])
impwater2005sort<-impwater2005[order(impwater2005$impwater),]
totalp=sum(impwater2005sort$population)
totaltb=sum(impwater2005sort$num_tb_cases)
impwater2005sort$Wpop<-impwater2005sort$population/totalp
impwater2005sort$CWpop<-cumsum(impwater2005sort$Wpop)
impwater2005sort$ridit<-c((0+impwater2005sort$CWpop[1])/2,(impwater2005sort$CWpop[1]+impwater2005sort$CWpop[2])/2,
                     (impwater2005sort$CWpop[2]+impwater2005sort$CWpop[3])/2,(impwater2005sort$CWpop[3]+impwater2005sort$CWpop[4])/2,
                     (impwater2005sort$CWpop[4]+impwater2005sort$CWpop[5])/2,(impwater2005sort$CWpop[5]+impwater2005sort$CWpop[6])/2,
                     (impwater2005sort$CWpop[6]+impwater2005sort$CWpop[7])/2,(impwater2005sort$CWpop[7]+impwater2005sort$CWpop[8])/2,
                     (impwater2005sort$CWpop[8]+impwater2005sort$CWpop[9])/2,(impwater2005sort$CWpop[9]+impwater2005sort$CWpop[10])/2,
                     (impwater2005sort$CWpop[10]+impwater2005sort$CWpop[11])/2,(impwater2005sort$CWpop[11]+impwater2005sort$CWpop[12])/2,
                     (impwater2005sort$CWpop[12]+impwater2005sort$CWpop[13])/2,(impwater2005sort$CWpop[13]+impwater2005sort$CWpop[14])/2,
                     (impwater2005sort$CWpop[14]+impwater2005sort$CWpop[15])/2,(impwater2005sort$CWpop[15]+impwater2005sort$CWpop[16])/2,
                     (impwater2005sort$CWpop[16]+impwater2005sort$CWpop[17])/2,(impwater2005sort$CWpop[17]+impwater2005sort$CWpop[18])/2,
                     (impwater2005sort$CWpop[18]+impwater2005sort$CWpop[19])/2,(impwater2005sort$CWpop[19]+impwater2005sort$CWpop[20])/2,
                     (impwater2005sort$CWpop[20]+impwater2005sort$CWpop[21])/2,(impwater2005sort$CWpop[21]+impwater2005sort$CWpop[22])/2,
                     (impwater2005sort$CWpop[22]+impwater2005sort$CWpop[23])/2)

impwater2005sort$Whealth<-impwater2005sort$num_tb_cases/totaltb
impwater2005sort$CWhealth<-cumsum(impwater2005sort$Whealth)
impwater2005sort$logridit<-log10(impwater2005sort$ridit)
impwater2005sort$Wi<-sqrt(impwater2005sort$population)
impwater2005sort$XiWi<-impwater2005sort$Wi*impwater2005sort$logridit
impwater2005sort$YiWi<-impwater2005sort$Wi*impwater2005sort$ir_tb
fit2005<-lm(impwater2005sort$YiWi~impwater2005sort$Wi + impwater2005sort$XiWi + 0)
summary(fit2005)
impwater2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*impwater2005sort$logridit


impwater2000<-as.data.frame(df[df$year=="2000",])
impwater2000sort<-impwater2000[order(impwater2000$impwater),]
totalp=sum(impwater2000sort$population)
totaltb=sum(impwater2000sort$num_tb_cases)
impwater2000sort$Wpop<-impwater2000sort$population/totalp
impwater2000sort$CWpop<-cumsum(impwater2000sort$Wpop)
impwater2000sort$ridit<-c((0+impwater2000sort$CWpop[1])/2,(impwater2000sort$CWpop[1]+impwater2000sort$CWpop[2])/2,
                     (impwater2000sort$CWpop[2]+impwater2000sort$CWpop[3])/2,(impwater2000sort$CWpop[3]+impwater2000sort$CWpop[4])/2,
                     (impwater2000sort$CWpop[4]+impwater2000sort$CWpop[5])/2,(impwater2000sort$CWpop[5]+impwater2000sort$CWpop[6])/2,
                     (impwater2000sort$CWpop[6]+impwater2000sort$CWpop[7])/2,(impwater2000sort$CWpop[7]+impwater2000sort$CWpop[8])/2,
                     (impwater2000sort$CWpop[8]+impwater2000sort$CWpop[9])/2,(impwater2000sort$CWpop[9]+impwater2000sort$CWpop[10])/2,
                     (impwater2000sort$CWpop[10]+impwater2000sort$CWpop[11])/2,(impwater2000sort$CWpop[11]+impwater2000sort$CWpop[12])/2,
                     (impwater2000sort$CWpop[12]+impwater2000sort$CWpop[13])/2,(impwater2000sort$CWpop[13]+impwater2000sort$CWpop[14])/2,
                     (impwater2000sort$CWpop[14]+impwater2000sort$CWpop[15])/2,(impwater2000sort$CWpop[15]+impwater2000sort$CWpop[16])/2,
                     (impwater2000sort$CWpop[16]+impwater2000sort$CWpop[17])/2,(impwater2000sort$CWpop[17]+impwater2000sort$CWpop[18])/2,
                     (impwater2000sort$CWpop[18]+impwater2000sort$CWpop[19])/2,(impwater2000sort$CWpop[19]+impwater2000sort$CWpop[20])/2,
                     (impwater2000sort$CWpop[20]+impwater2000sort$CWpop[21])/2,(impwater2000sort$CWpop[21]+impwater2000sort$CWpop[22])/2,
                     (impwater2000sort$CWpop[22]+impwater2000sort$CWpop[23])/2)

impwater2000sort$Whealth<-impwater2000sort$num_tb_cases/totaltb
impwater2000sort$CWhealth<-cumsum(impwater2000sort$Whealth)
impwater2000sort$logridit<-log10(impwater2000sort$ridit)
impwater2000sort$Wi<-sqrt(impwater2000sort$population)
impwater2000sort$XiWi<-impwater2000sort$Wi*impwater2000sort$logridit
impwater2000sort$YiWi<-impwater2000sort$Wi*impwater2000sort$ir_tb
fit2000<-lm(impwater2000sort$YiWi~impwater2000sort$Wi + impwater2000sort$XiWi + 0)
summary(fit2000)
impwater2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*impwater2000sort$logridit


# Social gradient
slope_index_of_inequality_impwater2000<-fit2000$coefficients[2]
slope_index_of_inequality_impwater2005<-fit2005$coefficients[2]
slope_index_of_inequality_impwater2009<-fit2009$coefficients[2]
slope_index_of_inequality_impwater2012<-fit2012$coefficients[2]
round(slope_index_of_inequality_impwater2000,2)
round(slope_index_of_inequality_impwater2005,2)
round(slope_index_of_inequality_impwater2009,2)
round(slope_index_of_inequality_impwater2012,2)

mylabel1e= bquote(2000==.(format(slope_index_of_inequality_impwater2000,digits=4))) 
mylabel2e= bquote(2005==.(format(slope_index_of_inequality_impwater2005,digits=4))) 
mylabel3e= bquote(2009==.(format(slope_index_of_inequality_impwater2009,digits=4))) 
mylabel4e= bquote(2012==.(format(slope_index_of_inequality_impwater2012,digits=4))) 


quartz(width=10, height=6, pointsize=10)
plot(impwater2012sort$ridit,impwater2012sort$ir_tb, col="red",pch=0,
     ylab="Tasa de incidencia de tuberculosis (por 100,000 hab)", 
     xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)")
points(impwater2009sort$ridit,impwater2009sort$ir_tb, col="blue",pch=1,
       ylab="", 
       xlab="")
points(impwater2005sort$ridit,impwater2005sort$ir_tb, col="green",pch=2,
       ylab="", 
       xlab="")
points(impwater2000sort$ridit,impwater2000sort$ir_tb, col="purple",pch=3,
       ylab="", 
       xlab="")
lines(impwater2012sort$ridit,impwater2012sort$predict2012, col="red", lty=1,
      ylab="", 
      xlab="")
lines(impwater2009sort$ridit,impwater2009sort$predict2009, col="blue", lty=2,
      ylab="", 
      xlab="")
lines(impwater2005sort$ridit,impwater2005sort$predict2005, col="green", lty=3,
      ylab="", 
      xlab="")
lines(impwater2000sort$ridit,impwater2000sort$predict2000, col="purple", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2012","2009","2005","2000"),col=c("red","blue","green","purple"),lty=c(1,2,3,4),pch=c(0,1,2,3),cex = .8)
text(0.8,170, "Índice de desigualdad de la pendiente (IDP)", col="red")
text(0.8,160, labels=mylabel1e, col="red")
text(0.8,150, labels=mylabel2e, col="red")
text(0.8,140, labels=mylabel3e, col="red")
text(0.8,130, labels=mylabel4e, col="red")




######################################################################################
######################  Quantiles of Improved water  ########################
impwater2012sort$qimpwater<-cut(impwater2012sort$impwater,quantile(impwater2012sort$impwater),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
impwater2009sort$qimpwater<-cut(impwater2009sort$impwater,quantile(impwater2009sort$impwater),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
impwater2005sort$qimpwater<-cut(impwater2005sort$impwater,quantile(impwater2005sort$impwater),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
impwater2000sort$qimpwater<-cut(impwater2000sort$impwater,quantile(impwater2000sort$impwater),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))

list(impwater2012sort$country,impwater2012sort$qimpwater)
list(impwater2009sort$country,impwater2009sort$qimpwater)
list(impwater2005sort$country,impwater2005sort$qimpwater)
list(impwater2000sort$country,impwater2000sort$qimpwater)

qpg2012<-sapply(split(impwater2012sort$population,impwater2012sort$qimpwater),sum)
qpg2009<-sapply(split(impwater2009sort$population,impwater2009sort$qimpwater),sum)
qpg2005<-sapply(split(impwater2005sort$population,impwater2005sort$qimpwater),sum)
qpg2000<-sapply(split(impwater2000sort$population,impwater2000sort$qimpwater),sum)

wpopg2012<-c(qpg2012[1]/sum(qpg2012),qpg2012[2]/sum(qpg2012),qpg2012[3]/sum(qpg2012),qpg2012[4]/sum(qpg2012))
wpopg2009<-c(qpg2009[1]/sum(qpg2009),qpg2009[2]/sum(qpg2009),qpg2009[3]/sum(qpg2009),qpg2009[4]/sum(qpg2009))
wpopg2005<-c(qpg2005[1]/sum(qpg2005),qpg2005[2]/sum(qpg2005),qpg2005[3]/sum(qpg2005),qpg2005[4]/sum(qpg2005))
wpopg2000<-c(qpg2000[1]/sum(qpg2000),qpg2000[2]/sum(qpg2000),qpg2000[3]/sum(qpg2000),qpg2000[4]/sum(qpg2000))

impwater2012sort$wpop2012<-ifelse(impwater2012sort$qimpwater=="Q1", impwater2012sort$population/qpg2012[1],0)
impwater2012sort$wpop2012<-ifelse(impwater2012sort$qimpwater=="Q2", impwater2012sort$population/qpg2012[2],impwater2012sort$wpop2012)
impwater2012sort$wpop2012<-ifelse(impwater2012sort$qimpwater=="Q3", impwater2012sort$population/qpg2012[3],impwater2012sort$wpop2012)
impwater2012sort$wpop2012<-ifelse(impwater2012sort$qimpwater=="Q4", impwater2012sort$population/qpg2012[4],impwater2012sort$wpop2012)

impwater2009sort$wpop2009<-ifelse(impwater2009sort$qimpwater=="Q1", impwater2009sort$population/qpg2009[1],0)
impwater2009sort$wpop2009<-ifelse(impwater2009sort$qimpwater=="Q2", impwater2009sort$population/qpg2009[2],impwater2009sort$wpop2009)
impwater2009sort$wpop2009<-ifelse(impwater2009sort$qimpwater=="Q3", impwater2009sort$population/qpg2009[3],impwater2009sort$wpop2009)
impwater2009sort$wpop2009<-ifelse(impwater2009sort$qimpwater=="Q4", impwater2009sort$population/qpg2009[4],impwater2009sort$wpop2009)

impwater2005sort$wpop2005<-ifelse(impwater2005sort$qimpwater=="Q1", impwater2005sort$population/qpg2005[1],0)
impwater2005sort$wpop2005<-ifelse(impwater2005sort$qimpwater=="Q2", impwater2005sort$population/qpg2005[2],impwater2005sort$wpop2005)
impwater2005sort$wpop2005<-ifelse(impwater2005sort$qimpwater=="Q3", impwater2005sort$population/qpg2005[3],impwater2005sort$wpop2005)
impwater2005sort$wpop2005<-ifelse(impwater2005sort$qimpwater=="Q4", impwater2005sort$population/qpg2005[4],impwater2005sort$wpop2005)

impwater2000sort$wpop2000<-ifelse(impwater2000sort$qimpwater=="Q1", impwater2000sort$population/qpg2000[1],0)
impwater2000sort$wpop2000<-ifelse(impwater2000sort$qimpwater=="Q2", impwater2000sort$population/qpg2000[2],impwater2000sort$wpop2000)
impwater2000sort$wpop2000<-ifelse(impwater2000sort$qimpwater=="Q3", impwater2000sort$population/qpg2000[3],impwater2000sort$wpop2000)
impwater2000sort$wpop2000<-ifelse(impwater2000sort$qimpwater=="Q4", impwater2000sort$population/qpg2000[4],impwater2000sort$wpop2000)

impwater2012sort$wrate<-impwater2012sort$wpop2012*impwater2012sort$ir_tb
impwater2009sort$wrate<-impwater2009sort$wpop2009*impwater2009sort$ir_tb
impwater2005sort$wrate<-impwater2005sort$wpop2005*impwater2005sort$ir_tb
impwater2000sort$wrate<-impwater2000sort$wpop2000*impwater2000sort$ir_tb

meang2012<-sapply(split(impwater2012sort$wrate,impwater2012sort$qimpwater),sum)
meang2012

meang2009<-sapply(split(impwater2009sort$wrate,impwater2009sort$qimpwater),sum)
meang2009

meang2005<-sapply(split(impwater2005sort$wrate,impwater2005sort$qimpwater),sum)
meang2005

meang2000<-sapply(split(impwater2000sort$wrate,impwater2000sort$qimpwater),sum)
meang2000



Q1<-c(meang2000[1],meang2005[1],meang2009[1],meang2012[1]) 
Q1<-round(Q1,2)
Q2<-c(meang2000[2],meang2005[2],meang2009[2],meang2012[2]) 
Q2<-round(Q2,2)
Q3<-c(meang2000[3],meang2005[3],meang2009[3],meang2012[3]) 
Q3<-round(Q3,2)
Q4<-c(meang2000[4],meang2005[4],meang2009[4],meang2012[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4)
#r<-t(r)
rownames(r)<-c('2000','2005','2009','2012')
colnames(r)<-c('Q1','Q2','Q3','Q4')
r

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_impwater2000<-sum(wpopg2000*meang2000)
regional_mean_rate_impwater2005<-sum(wpopg2005*meang2005)
regional_mean_rate_impwater2009<-sum(wpopg2009*meang2009)
regional_mean_rate_impwater2012<-sum(wpopg2012*meang2012)
round(regional_mean_rate_impwater2000,2)
round(regional_mean_rate_impwater2005,2)
round(regional_mean_rate_impwater2009,2)
round(regional_mean_rate_impwater2012,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_impwater2000<-meang2000[1]-meang2000[4]
absolute_Kuznets_index_impwater2005<-meang2005[1]-meang2005[4]
absolute_Kuznets_index_impwater2009<-meang2009[1]-meang2009[4]
absolute_Kuznets_index_impwater2012<-meang2012[1]-meang2012[4]
round(absolute_Kuznets_index_impwater2000,2)
round(absolute_Kuznets_index_impwater2005,2)
round(absolute_Kuznets_index_impwater2009,2)
round(absolute_Kuznets_index_impwater2012,2)

relative_Kuznets_index_impwater2000<-meang2000[1]/meang2000[4]
relative_Kuznets_index_impwater2005<-meang2005[1]/meang2005[4]
relative_Kuznets_index_impwater2009<-meang2009[1]/meang2009[4]
relative_Kuznets_index_impwater2012<-meang2012[1]/meang2012[4]
round(relative_Kuznets_index_impwater2000,2)
round(relative_Kuznets_index_impwater2005,2)
round(relative_Kuznets_index_impwater2009,2)
round(relative_Kuznets_index_impwater2012,2)

mylabel1e= bquote(2000==.(format(absolute_Kuznets_index_impwater2000,digits=4))) 
mylabel2e= bquote(2005==.(format(absolute_Kuznets_index_impwater2005,digits=6))) 
mylabel3e= bquote(2009==.(format(absolute_Kuznets_index_impwater2009,digits=4))) 
mylabel4e= bquote(2012==.(format(absolute_Kuznets_index_impwater2012,digits=4))) 
mylabel5e= bquote(2000==.(format(relative_Kuznets_index_impwater2000,digits=3))) 
mylabel6e= bquote(2005==.(format(relative_Kuznets_index_impwater2005,digits=3))) 
mylabel7e= bquote(2009==.(format(relative_Kuznets_index_impwater2009,digits=3))) 
mylabel8e= bquote(2012==.(format(relative_Kuznets_index_impwater2012,digits=3))) 


quartz(width=10, height=6, pointsize=10)
b<-barplot(t(r),col=c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),beside=T,ylim=c(0,220),
           xlab="Cuartíles del Indice de Desarrollo Humano (IDH)", ylab="Tasa promedio de incidencia de TB (por 100,000 hb)")
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),pch=15,bty="n") 
text(x=c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5,16.5,17.5,18.5,19.5),
     y=c(114.17,76.76,58.53,31.36,127.28,51.52,47.67,26.30,94.64,57.40,45.47,25.27,88.31,50.66,42.77,21.50),
     labels=c(114.17,76.76,58.53,31.36,127.28,51.52,47.67,26.30,94.64,57.40,45.47,25.27,88.31,50.66,42.77,21.50),cex=1.25,pos=3)
text(10,200, "Índice de Kuznets absoluto", col="red")
text(10,190, labels=mylabel1e, col="red")
text(10,180, labels=mylabel2e, col="red")
text(10,170, labels=mylabel3e, col="red")
text(10,160, labels=mylabel4e, col="red")
text(10,150, "Índice de Kuznets relativo", col="red")
text(10,140, labels=mylabel5e, col="red")
text(10,130, labels=mylabel6e, col="red")
text(10,120, labels=mylabel7e, col="red")
text(10,110, labels=mylabel8e, col="red")




## lines and dots graphics
quartz(width=10, height=6, pointsize=10)
plot(r[1,],c(2000,2000,2000,2000),bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
ticks = c(2000, 2005, 2009, 2012)
axis(side = 2, at = ticks)
from.x <- c(114.17, 76.76, 58.53)
to.x   <- c(76.76, 58.53-1, 31.36)
to.y   <- from.y <- c(2000, 2000, 2000,2000)
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[2,],c(2005,2005,2005,2005), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(127.28, 51.52, 47.67, 26.30)
to.x   <- c(51.52, 47.67, 26.30)
to.y   <- from.y <- c(2005, 2005, 2005,2005) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[3,],c(2009,2009,2009,2009), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(94.64, 57.40, 45.47)
to.x   <- c(57.40, 45.47, 25.27)
to.y   <- from.y <- c(2009, 2009, 2009,2009) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[4,],c(2012,2012,2012,2012), bg=rainbow(4), pch=21, cex=2, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="") 
from.x <- c(88.31, 50.66, 42.77)
to.x   <- c(50.66, 42.77, 21.50)
to.y   <- from.y <- c(2012, 2012, 2012,2012) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= rainbow(4),
       bg=rainbow(4), pch=19,bty="n", cex=1.5) 



## lines and dots graphics weighted
quartz(width=10, height=6, pointsize=10)
symbols(r[1,],c(2000,2000,2000,2000),circles=sqrt(qpg2000/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[1,],c(2000,2000,2000,2000),bg="black", pch=21, cex=0.5, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
ticks = c(2000, 2005, 2009, 2012)
axis(side = 2, at = ticks)
from.x <- c(114.17, 76.76, 58.53)
to.x   <- c(76.76, 58.53-1, 31.36)
to.y   <- from.y <- c(2000, 2000, 2000,2000)
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[2,],c(2005,2005,2005,2005),circles=sqrt(qpg2005/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[2,],c(2005,2005,2005,2005), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(127.28, 51.52, 47.67, 26.30)
to.x   <- c(51.52, 47.67, 26.30)
to.y   <- from.y <- c(2005, 2005, 2005,2005) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[3,],c(2009,2009,2009,2009),circles=sqrt(qpg2009/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[3,],c(2009,2009,2009,2009), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="")
from.x <- c(94.64, 57.40, 45.47)
to.x   <- c(57.40, 45.47, 25.27)
to.y   <- from.y <- c(2009, 2009, 2009,2009) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
symbols(r[4,],c(2012,2012,2012,2012),circles=sqrt(qpg2012/pi),inches=1/4, bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,130), ylim=c(1999,2015), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
par(new=TRUE)
plot(r[4,],c(2012,2012,2012,2012), bg="black", pch=21, cex=0.5, lwd=3, xlim=c(10,130), ylim=c(1999,2015), axes=FALSE, xlab="", ylab="") 
from.x <- c(88.31, 50.66, 42.77)
to.x   <- c(50.66, 42.77, 21.50)
to.y   <- from.y <- c(2012, 2012, 2012,2012) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
legend("topright",title="Population size",c("Q1","Q2","Q3","Q4"),
       col= rainbow(4),
       bg=rainbow(4), pch=19,bty="n", cex=1.5,
       pt.cex=c(1,1.4,3,1.4)) 



##########################################################################
############################# Concentration curve  #######################


CWpopf2012<-c(0,impwater2012sort$CWpop)
CWhealthf2012<-c(0,impwater2012sort$CWhealth)
CWpopf2009<-c(0,impwater2009sort$CWpop)
CWhealthf2009<-c(0,impwater2009sort$CWhealth)
CWpopf2005<-c(0,impwater2005sort$CWpop)
CWhealthf2005<-c(0,impwater2005sort$CWhealth)
CWpopf2000<-c(0,impwater2000sort$CWpop)
CWhealthf2000<-c(0,impwater2000sort$CWhealth)


ccurve2012<-data.frame(y=CWhealthf2012, x=CWpopf2012)
ccurve2009<-data.frame(y=CWhealthf2009, x=CWpopf2009)
ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2000<-data.frame(y=CWhealthf2000, x=CWpopf2000)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2012 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2012, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2012)

ccurve.optx2009 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2009, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2009)

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2000 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2000, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2000)

x<-seq(0,1,0.01)

k<-ccurve.optx2012[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02012<-f(x,k)
delta_x_y<-x-lf02012
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impwater2012<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impwater2012,2)


k<-ccurve.optx2009[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02009<-f(x,k)

delta_x_y<-x-lf02009
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impwater2009<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impwater2009,2)


k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)

delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impwater2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impwater2005,2)


k<-ccurve.optx2000[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02000<-f(x,k)

delta_x_y<-x-lf02000
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_impwater2000<-2*sum(delta_x_y)*0.01
round(health_concentration_index_impwater2000,2)


mylabel1e= bquote(2000==.(format(health_concentration_index_impwater2000,digits=2))) 
mylabel2e= bquote(2005==.(format(health_concentration_index_impwater2005,digits=2))) 
mylabel3e= bquote(2009==.(format(health_concentration_index_impwater2009,digits=2))) 
mylabel4e= bquote(2012==.(format(health_concentration_index_impwater2012,digits=2))) 

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2012,CWhealthf2012, col="red",pch=0, xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)", ylab="Número de casos incidentes de TB (acumulado)")
points(CWpopf2009,CWhealthf2009, col="blue",pch=1)
points(CWpopf2005,CWhealthf2005, col="green",pch=2)
points(CWpopf2000,CWhealthf2000, col="purple",pch=3)
lines(x,lf02012,col="red", lty=1)
lines(x,lf02009,col="blue", lty=2)
lines(x,lf02005,col="green", lty=3)
lines(x,lf02000,col="purple", lty=4)
lines(x,x)
legend(locator(1),c("2012","2009","2005","2000"),col=c("red","blue","green","purple"),pch=c(0,1,2,3), lty=c(1,2,3,4),cex = .8)
text(0.8,0.25, "Índices de concentración de salud (IC)", col="red")
text(0.8,0.22, labels=mylabel1e, col="red")
text(0.8,0.18, labels=mylabel2e, col="red")
text(0.8,0.15, labels=mylabel3e, col="red")
text(0.8,0.12, labels=mylabel4e, col="red")


write.csv(impwater2000,"impwater2000.csv")
write.csv(impwater2005,"impwater2005.csv")
write.csv(impwater2009,"impwater2009.csv")
write.csv(impwater2012,"impwater2012.csv")



#######################################################################################
################################### Improved water urban
impwaterurban_data<-subset(dat3,select=-c(Indicator.Name,Country.Code, Indicator.Code, X1960,
                                     X1961,X1962,X1963,X1964,X1965,X1966,
                                     X1967,X1968,X1969,X1970,X1971,X1972,
                                     X1973,X1974,X1975,X1976,X1977,X1978,
                                     X1979,X1980,X1981,X1982,X1983,X1984,
                                     X1985,X1986,X1987,X1988,X1989,X2013,X2014))

impwaterurban_world<-melt(impwaterurban_data, Country=c("Country.Name"))
impwaterurban_world$year<-as.numeric(substr(impwaterurban_world$variable,2,5))
impwaterurban_world$impwaterurban<-impwaterurban_world$value
impwaterurban_world<-subset(impwaterurban_world,select=-c(variable,value))
impwaterurban_world$Country.Name<-as.character(impwaterurban_world$Country.Name)
impwaterurban_world <- rename(impwaterurban_world, c(Country.Name="country"))

impwaterurban_world$country <- revalue(impwaterurban_world$country, c("Venezuela, RB"="Venezuela"))

impwaterurban_lac_f<-subset(impwaterurban_world,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                                     "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                                     "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                                     "Uruguay","Venezuela"))

impwaterurban_lac_f <- impwaterurban_lac_f[order(impwaterurban_lac_f$country,impwaterurban_lac_f$year),] 


################################################################################################
################################## Improved sanitation
impsanifac_data<-subset(dat4,select=-c(Indicator.Name,Country.Code, Indicator.Code, X1960,
                                          X1961,X1962,X1963,X1964,X1965,X1966,
                                          X1967,X1968,X1969,X1970,X1971,X1972,
                                          X1973,X1974,X1975,X1976,X1977,X1978,
                                          X1979,X1980,X1981,X1982,X1983,X1984,
                                          X1985,X1986,X1987,X1988,X1989,X2013,X2014))

impsanifac_world<-melt(impsanifac_data, Country=c("Country.Name"))
impsanifac_world$year<-as.numeric(substr(impsanifac_world$variable,2,5))
impsanifac_world$impsanifac<-impsanifac_world$value
impsanifac_world<-subset(impsanifac_world,select=-c(variable,value))
impsanifac_world$Country.Name<-as.character(impsanifac_world$Country.Name)
impsanifac_world <- rename(impsanifac_world, c(Country.Name="country"))

impsanifac_world$country <- revalue(impsanifac_world$country, c("Venezuela, RB"="Venezuela"))

impsanifac_lac_f<-subset(impsanifac_world,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                                               "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                                               "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                                               "Uruguay","Venezuela"))

impsanifac_lac_f <- impsanifac_lac_f[order(impsanifac_lac_f$country,impsanifac_lac_f$year),] 


#########################################################################
################# Building the final dataset ############################  
impsanifac_lac_fff<-subset(impsanifac_lac_f,year %in% c(2000,2005,2009,2012)) 
tb_lac_ffff<-subset(tb_lac_ff,year %in% c(2000,2005,2009,2012))

df<-merge(tb_lac_ffff,impsanifac_lac_fff,by=c("country","year"),all=TRUE)

lengend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba","Dominican Republic",
            "Ecuador","El Salvador","Guatemala ","Guyana","Haiti","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Trinidad and Tobago","Uruguay","Venezuela")


impsanifac<-df[, c("country","year","population","impsanifac","num_tb_cases","ir_tb")]

impsanifac2012<-as.data.frame(impsanifac[impsanifac$year=="2012",])
impsanifac2012sort<-impsanifac2012[order(impsanifac2012$impsanifac),]
totalp=sum(impsanifac2012sort$population)
totaltb=sum(impsanifac2012sort$num_tb_cases)
impsanifac2012sort$Wpop<-impsanifac2012sort$population/totalp
impsanifac2012sort$CWpop<-cumsum(impsanifac2012sort$Wpop)
impsanifac2012sort$ridit<-c((0+impsanifac2012sort$CWpop[1])/2,(impsanifac2012sort$CWpop[1]+impsanifac2012sort$CWpop[2])/2,
                         (impsanifac2012sort$CWpop[2]+impsanifac2012sort$CWpop[3])/2,(impsanifac2012sort$CWpop[3]+impsanifac2012sort$CWpop[4])/2,
                         (impsanifac2012sort$CWpop[4]+impsanifac2012sort$CWpop[5])/2,(impsanifac2012sort$CWpop[5]+impsanifac2012sort$CWpop[6])/2,
                         (impsanifac2012sort$CWpop[6]+impsanifac2012sort$CWpop[7])/2,(impsanifac2012sort$CWpop[7]+impsanifac2012sort$CWpop[8])/2,
                         (impsanifac2012sort$CWpop[8]+impsanifac2012sort$CWpop[9])/2,(impsanifac2012sort$CWpop[9]+impsanifac2012sort$CWpop[10])/2,
                         (impsanifac2012sort$CWpop[10]+impsanifac2012sort$CWpop[11])/2,(impsanifac2012sort$CWpop[11]+impsanifac2012sort$CWpop[12])/2,
                         (impsanifac2012sort$CWpop[12]+impsanifac2012sort$CWpop[13])/2,(impsanifac2012sort$CWpop[13]+impsanifac2012sort$CWpop[14])/2,
                         (impsanifac2012sort$CWpop[14]+impsanifac2012sort$CWpop[15])/2,(impsanifac2012sort$CWpop[15]+impsanifac2012sort$CWpop[16])/2,
                         (impsanifac2012sort$CWpop[16]+impsanifac2012sort$CWpop[17])/2,(impsanifac2012sort$CWpop[17]+impsanifac2012sort$CWpop[18])/2,
                         (impsanifac2012sort$CWpop[18]+impsanifac2012sort$CWpop[19])/2,(impsanifac2012sort$CWpop[19]+impsanifac2012sort$CWpop[20])/2,
                         (impsanifac2012sort$CWpop[20]+impsanifac2012sort$CWpop[21])/2,(impsanifac2012sort$CWpop[21]+impsanifac2012sort$CWpop[22])/2,
                         (impsanifac2012sort$CWpop[22]+impsanifac2012sort$CWpop[23])/2)
impsanifac2012sort$Whealth<-impsanifac2012sort$num_tb_cases/totaltb
impsanifac2012sort$CWhealth<-cumsum(impsanifac2012sort$Whealth)
impsanifac2012sort$logridit<-log10(impsanifac2012sort$ridit)
impsanifac2012sort$Wi<-sqrt(impsanifac2012sort$population)
impsanifac2012sort$XiWi<-impsanifac2012sort$Wi*impsanifac2012sort$logridit
impsanifac2012sort$YiWi<-impsanifac2012sort$Wi*impsanifac2012sort$ir_tb
fit2012<-lm(impsanifac2012sort$YiWi~impsanifac2012sort$Wi + impsanifac2012sort$XiWi + 0)
summary(fit2012)
impsanifac2012sort$predict2012<-coef(summary(fit2012))[1,1] + coef(summary(fit2012))[2,1]*impsanifac2012sort$logridit


impsanifac2009<-as.data.frame(impsanifac[impsanifac$year=="2009",])
impsanifac2009sort<-impsanifac2009[order(impsanifac2009$impsanifac),]
totalp=sum(impsanifac2009sort$population)
totaltb=sum(impsanifac2009sort$num_tb_cases)
impsanifac2009sort$Wpop<-impsanifac2009sort$population/totalp
impsanifac2009sort$CWpop<-cumsum(impsanifac2009sort$Wpop)
impsanifac2009sort$ridit<-c((0+impsanifac2009sort$CWpop[1])/2,(impsanifac2009sort$CWpop[1]+impsanifac2009sort$CWpop[2])/2,
                         (impsanifac2009sort$CWpop[2]+impsanifac2009sort$CWpop[3])/2,(impsanifac2009sort$CWpop[3]+impsanifac2009sort$CWpop[4])/2,
                         (impsanifac2009sort$CWpop[4]+impsanifac2009sort$CWpop[5])/2,(impsanifac2009sort$CWpop[5]+impsanifac2009sort$CWpop[6])/2,
                         (impsanifac2009sort$CWpop[6]+impsanifac2009sort$CWpop[7])/2,(impsanifac2009sort$CWpop[7]+impsanifac2009sort$CWpop[8])/2,
                         (impsanifac2009sort$CWpop[8]+impsanifac2009sort$CWpop[9])/2,(impsanifac2009sort$CWpop[9]+impsanifac2009sort$CWpop[10])/2,
                         (impsanifac2009sort$CWpop[10]+impsanifac2009sort$CWpop[11])/2,(impsanifac2009sort$CWpop[11]+impsanifac2009sort$CWpop[12])/2,
                         (impsanifac2009sort$CWpop[12]+impsanifac2009sort$CWpop[13])/2,(impsanifac2009sort$CWpop[13]+impsanifac2009sort$CWpop[14])/2,
                         (impsanifac2009sort$CWpop[14]+impsanifac2009sort$CWpop[15])/2,(impsanifac2009sort$CWpop[15]+impsanifac2009sort$CWpop[16])/2,
                         (impsanifac2009sort$CWpop[16]+impsanifac2009sort$CWpop[17])/2,(impsanifac2009sort$CWpop[17]+impsanifac2009sort$CWpop[18])/2,
                         (impsanifac2009sort$CWpop[18]+impsanifac2009sort$CWpop[19])/2,(impsanifac2009sort$CWpop[19]+impsanifac2009sort$CWpop[20])/2,
                         (impsanifac2009sort$CWpop[20]+impsanifac2009sort$CWpop[21])/2,(impsanifac2009sort$CWpop[21]+impsanifac2009sort$CWpop[22])/2,
                         (impsanifac2009sort$CWpop[22]+impsanifac2009sort$CWpop[23])/2)
impsanifac2009sort$Whealth<-impsanifac2009sort$num_tb_cases/totaltb
impsanifac2009sort$CWhealth<-cumsum(impsanifac2009sort$Whealth)
impsanifac2009sort$logridit<-log10(impsanifac2009sort$ridit)
impsanifac2009sort$Wi<-sqrt(impsanifac2009sort$population)
impsanifac2009sort$XiWi<-impsanifac2009sort$Wi*impsanifac2009sort$logridit
impsanifac2009sort$YiWi<-impsanifac2009sort$Wi*impsanifac2009sort$ir_tb
fit2009<-lm(impsanifac2009sort$YiWi~impsanifac2009sort$Wi + impsanifac2009sort$XiWi + 0)
summary(fit2009)
impsanifac2009sort$predict2009<-coef(summary(fit2009))[1,1] + coef(summary(fit2009))[2,1]*impsanifac2009sort$logridit


impsanifac2005<-as.data.frame(impsanifac[impsanifac$year=="2005",])
impsanifac2005sort<-impsanifac2005[order(impsanifac2005$impsanifac),]
totalp=sum(impsanifac2005sort$population)
totaltb=sum(impsanifac2005sort$num_tb_cases)
impsanifac2005sort$Wpop<-impsanifac2005sort$population/totalp
impsanifac2005sort$CWpop<-cumsum(impsanifac2005sort$Wpop)
impsanifac2005sort$ridit<-c((0+impsanifac2005sort$CWpop[1])/2,(impsanifac2005sort$CWpop[1]+impsanifac2005sort$CWpop[2])/2,
                         (impsanifac2005sort$CWpop[2]+impsanifac2005sort$CWpop[3])/2,(impsanifac2005sort$CWpop[3]+impsanifac2005sort$CWpop[4])/2,
                         (impsanifac2005sort$CWpop[4]+impsanifac2005sort$CWpop[5])/2,(impsanifac2005sort$CWpop[5]+impsanifac2005sort$CWpop[6])/2,
                         (impsanifac2005sort$CWpop[6]+impsanifac2005sort$CWpop[7])/2,(impsanifac2005sort$CWpop[7]+impsanifac2005sort$CWpop[8])/2,
                         (impsanifac2005sort$CWpop[8]+impsanifac2005sort$CWpop[9])/2,(impsanifac2005sort$CWpop[9]+impsanifac2005sort$CWpop[10])/2,
                         (impsanifac2005sort$CWpop[10]+impsanifac2005sort$CWpop[11])/2,(impsanifac2005sort$CWpop[11]+impsanifac2005sort$CWpop[12])/2,
                         (impsanifac2005sort$CWpop[12]+impsanifac2005sort$CWpop[13])/2,(impsanifac2005sort$CWpop[13]+impsanifac2005sort$CWpop[14])/2,
                         (impsanifac2005sort$CWpop[14]+impsanifac2005sort$CWpop[15])/2,(impsanifac2005sort$CWpop[15]+impsanifac2005sort$CWpop[16])/2,
                         (impsanifac2005sort$CWpop[16]+impsanifac2005sort$CWpop[17])/2,(impsanifac2005sort$CWpop[17]+impsanifac2005sort$CWpop[18])/2,
                         (impsanifac2005sort$CWpop[18]+impsanifac2005sort$CWpop[19])/2,(impsanifac2005sort$CWpop[19]+impsanifac2005sort$CWpop[20])/2,
                         (impsanifac2005sort$CWpop[18]+impsanifac2005sort$CWpop[20])/2,(impsanifac2005sort$CWpop[21]+impsanifac2005sort$CWpop[22])/2,
                         (impsanifac2005sort$CWpop[22]+impsanifac2005sort$CWpop[23])/2)
impsanifac2005sort$Whealth<-impsanifac2005sort$num_tb_cases/totaltb
impsanifac2005sort$CWhealth<-cumsum(impsanifac2005sort$Whealth)
impsanifac2005sort$logridit<-log10(impsanifac2005sort$ridit)
impsanifac2005sort$Wi<-sqrt(impsanifac2005sort$population)
impsanifac2005sort$XiWi<-impsanifac2005sort$Wi*impsanifac2005sort$logridit
impsanifac2005sort$YiWi<-impsanifac2005sort$Wi*impsanifac2005sort$ir_tb
fit2005<-lm(impsanifac2005sort$YiWi~impsanifac2005sort$Wi + impsanifac2005sort$XiWi + 0)
summary(fit2005)
impsanifac2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*impsanifac2005sort$logridit

impsanifac2000<-as.data.frame(impsanifac[impsanifac$year=="2000",])
impsanifac2000sort<-impsanifac2000[order(impsanifac2000$impsanifac),]
totalp=sum(impsanifac2000sort$population)
totaltb=sum(impsanifac2000sort$num_tb_cases)
impsanifac2000sort$Wpop<-impsanifac2000sort$population/totalp
impsanifac2000sort$CWpop<-cumsum(impsanifac2000sort$Wpop)
impsanifac2000sort$ridit<-c((0+impsanifac2000sort$CWpop[1])/2,(impsanifac2000sort$CWpop[1]+impsanifac2000sort$CWpop[2])/2,
                         (impsanifac2000sort$CWpop[2]+impsanifac2000sort$CWpop[3])/2,(impsanifac2000sort$CWpop[3]+impsanifac2000sort$CWpop[4])/2,
                         (impsanifac2000sort$CWpop[4]+impsanifac2000sort$CWpop[5])/2,(impsanifac2000sort$CWpop[5]+impsanifac2000sort$CWpop[6])/2,
                         (impsanifac2000sort$CWpop[6]+impsanifac2000sort$CWpop[7])/2,(impsanifac2000sort$CWpop[7]+impsanifac2000sort$CWpop[8])/2,
                         (impsanifac2000sort$CWpop[8]+impsanifac2000sort$CWpop[9])/2,(impsanifac2000sort$CWpop[9]+impsanifac2000sort$CWpop[10])/2,
                         (impsanifac2000sort$CWpop[10]+impsanifac2000sort$CWpop[11])/2,(impsanifac2000sort$CWpop[11]+impsanifac2000sort$CWpop[12])/2,
                         (impsanifac2000sort$CWpop[12]+impsanifac2000sort$CWpop[13])/2,(impsanifac2000sort$CWpop[13]+impsanifac2000sort$CWpop[14])/2,
                         (impsanifac2000sort$CWpop[14]+impsanifac2000sort$CWpop[15])/2,(impsanifac2000sort$CWpop[15]+impsanifac2000sort$CWpop[16])/2,
                         (impsanifac2000sort$CWpop[16]+impsanifac2000sort$CWpop[17])/2,(impsanifac2000sort$CWpop[17]+impsanifac2000sort$CWpop[18])/2,
                         (impsanifac2000sort$CWpop[18]+impsanifac2000sort$CWpop[19])/2,(impsanifac2000sort$CWpop[19]+impsanifac2000sort$CWpop[20])/2,
                         (impsanifac2000sort$CWpop[20]+impsanifac2000sort$CWpop[21])/2,(impsanifac2000sort$CWpop[21]+impsanifac2000sort$CWpop[22])/2,
                         (impsanifac2000sort$CWpop[22]+impsanifac2000sort$CWpop[23])/2)

impsanifac2000sort$Whealth<-impsanifac2000sort$num_tb_cases/totaltb
impsanifac2000sort$CWhealth<-cumsum(impsanifac2000sort$Whealth)
impsanifac2000sort$logridit<-log10(impsanifac2000sort$ridit)
impsanifac2000sort$Wi<-sqrt(impsanifac2000sort$population)
impsanifac2000sort$XiWi<-impsanifac2000sort$Wi*impsanifac2000sort$logridit
impsanifac2000sort$YiWi<-impsanifac2000sort$Wi*impsanifac2000sort$ir_tb
fit2000<-lm(impsanifac2000sort$YiWi~impsanifac2000sort$Wi + impsanifac2000sort$XiWi + 0)
summary(fit2000)
impsanifac2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*impsanifac2000sort$logridit

# Social gradient
slope_index_of_inequality_impsanifac2000<-fit2000$coefficients[2]
slope_index_of_inequality_impsanifac2005<-fit2005$coefficients[2]
slope_index_of_inequality_impsanifac2009<-fit2009$coefficients[2]
slope_index_of_inequality_impsanifac2012<-fit2012$coefficients[2]
round(slope_index_of_inequality_impsanifac2000,2)
round(slope_index_of_inequality_impsanifac2005,2)
round(slope_index_of_inequality_impsanifac2009,2)
round(slope_index_of_inequality_impsanifac2012,2)

mylabel1c= bquote(2000==.(format(slope_index_of_inequality_impsanifac2000,digits=4))) 
mylabel2c= bquote(2005==.(format(slope_index_of_inequality_impsanifac2005,digits=4))) 
mylabel3c= bquote(2009==.(format(slope_index_of_inequality_impsanifac2009,digits=4))) 
mylabel4c= bquote(2013==.(format(slope_index_of_inequality_impsanifac2012,digits=4))) 

quartz(width=10, height=6, pointsize=10)
plot(impsanifac2012sort$ridit,impsanifac2012sort$ir_tb, col="red", pch=0,
     ylab="Tuberculosis incidence rates per 100,000 population", 
     xlab="Country-level population gradient defined by access to improved sanitation facilities")
points(impsanifac2009sort$ridit,impsanifac2009sort$ir_tb, col="blue",pch=1,
       ylab="", 
       xlab="")
points(impsanifac2005sort$ridit,impsanifac2005sort$ir_tb, col="green",pch=2,
       ylab="", 
       xlab="")
points(impsanifac2000sort$ridit,impsanifac2000sort$ir_tb, col="purple",pch=3,
       ylab="", 
       xlab="")
lines(impsanifac2012sort$ridit,impsanifac2012sort$predict2012, col="red",lty=1,
      ylab="", 
      xlab="")
lines(impsanifac2009sort$ridit,impsanifac2009sort$predict2009, col="blue", lty=2,
      ylab="", 
      xlab="")
lines(impsanifac2005sort$ridit,impsanifac2005sort$predict2005, col="green", lty=3,
      ylab="", 
      xlab="")
lines(impsanifac2000sort$ridit,impsanifac2000sort$predict2000, col="purple", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2012","2009","2005","2000"),col=c("red","blue","green","purple"),pch=c(0,1,2,3),lty=c(1,2,3,4),cex = .8)
text(0.8,170, "Índice de desigualdad de la pendiente (IDP)", col="red")
text(0.8,160, labels=mylabel1c, col="red")
text(0.8,150, labels=mylabel2c, col="red")
text(0.8,140, labels=mylabel3c, col="red")
text(0.8,130, labels=mylabel4c, col="red")




################################################################################################
######################################## Improve sanitation urban

impsanifacurban_data<-subset(dat5,select=-c(Indicator.Name,Country.Code, Indicator.Code, X1960,
                                       X1961,X1962,X1963,X1964,X1965,X1966,
                                       X1967,X1968,X1969,X1970,X1971,X1972,
                                       X1973,X1974,X1975,X1976,X1977,X1978,
                                       X1979,X1980,X1981,X1982,X1983,X1984,
                                       X1985,X1986,X1987,X1988,X1989,X2013,X2014))

impsanifacurban_world<-melt(impsanifacurban_data, Country=c("Country.Name"))
impsanifacurban_world$year<-as.numeric(substr(impsanifacurban_world$variable,2,5))
impsanifacurban_world$impsanifacurban<-impsanifacurban_world$value
impsanifacurban_world<-subset(impsanifacurban_world,select=-c(variable,value))
impsanifacurban_world$Country.Name<-as.character(impsanifacurban_world$Country.Name)
impsanifacurban_world <- rename(impsanifacurban_world, c(Country.Name="country"))

impsanifacurban_world$country <- revalue(impsanifacurban_world$country, c("Venezuela, RB"="Venezuela"))

impsanifacurban_lac_f<-subset(impsanifacurban_world,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                                         "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                                         "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                                         "Uruguay","Venezuela"))

impsanifacurban_lac_f <- impsanifacurban_lac_f[order(impsanifacurban_lac_f$country,impsanifacurban_lac_f$year),]


################# Building the final dataset ############################  
impsanifacurban_lac_fff<-subset(impsanifacurban_lac_f,year %in% c(2000,2005,2009,2012)) ## we used the same value of 2005 hdi for 2000 
tb_lac_ffff1<-subset(tb_lac_ff,year %in% c(2000,2005,2009,2012))

df1<-merge(tb_lac_ffff1,impsanifacurban_lac_fff,by=c("country","year"),all=TRUE)

lengend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba","Dominican Republic",
            "Ecuador","El Salvador","Guatemala ","Guyana","Haiti","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Trinidad and Tobago","Uruguay","Venezuela")


impsanifacurban<-df1[, c("country","year","population","impsanifacurban","num_tb_cases","ir_tb")]

impsanifacurban2012<-as.data.frame(impsanifacurban[impsanifacurban$year=="2012",])
impsanifacurban2012sort<-impsanifacurban2012[order(impsanifacurban2012$impsanifacurban),]
totalp=sum(impsanifacurban2012sort$population)
totaltb=sum(impsanifacurban2012sort$num_tb_cases)
impsanifacurban2012sort$Wpop<-impsanifacurban2012sort$population/totalp
impsanifacurban2012sort$CWpop<-cumsum(impsanifacurban2012sort$Wpop)
impsanifacurban2012sort$ridit<-c((0+impsanifacurban2012sort$CWpop[1])/2,(impsanifacurban2012sort$CWpop[1]+impsanifacurban2012sort$CWpop[2])/2,
                            (impsanifacurban2012sort$CWpop[2]+impsanifacurban2012sort$CWpop[3])/2,(impsanifacurban2012sort$CWpop[3]+impsanifacurban2012sort$CWpop[4])/2,
                            (impsanifacurban2012sort$CWpop[4]+impsanifacurban2012sort$CWpop[5])/2,(impsanifacurban2012sort$CWpop[5]+impsanifacurban2012sort$CWpop[6])/2,
                            (impsanifacurban2012sort$CWpop[6]+impsanifacurban2012sort$CWpop[7])/2,(impsanifacurban2012sort$CWpop[7]+impsanifacurban2012sort$CWpop[8])/2,
                            (impsanifacurban2012sort$CWpop[8]+impsanifacurban2012sort$CWpop[9])/2,(impsanifacurban2012sort$CWpop[9]+impsanifacurban2012sort$CWpop[10])/2,
                            (impsanifacurban2012sort$CWpop[10]+impsanifacurban2012sort$CWpop[11])/2,(impsanifacurban2012sort$CWpop[11]+impsanifacurban2012sort$CWpop[12])/2,
                            (impsanifacurban2012sort$CWpop[12]+impsanifacurban2012sort$CWpop[13])/2,(impsanifacurban2012sort$CWpop[13]+impsanifacurban2012sort$CWpop[14])/2,
                            (impsanifacurban2012sort$CWpop[14]+impsanifacurban2012sort$CWpop[15])/2,(impsanifacurban2012sort$CWpop[15]+impsanifacurban2012sort$CWpop[16])/2,
                            (impsanifacurban2012sort$CWpop[16]+impsanifacurban2012sort$CWpop[17])/2,(impsanifacurban2012sort$CWpop[17]+impsanifacurban2012sort$CWpop[18])/2,
                            (impsanifacurban2012sort$CWpop[18]+impsanifacurban2012sort$CWpop[19])/2,(impsanifacurban2012sort$CWpop[19]+impsanifacurban2012sort$CWpop[20])/2,
                            (impsanifacurban2012sort$CWpop[20]+impsanifacurban2012sort$CWpop[21])/2,(impsanifacurban2012sort$CWpop[21]+impsanifacurban2012sort$CWpop[22])/2,
                            (impsanifacurban2012sort$CWpop[22]+impsanifacurban2012sort$CWpop[23])/2)
impsanifacurban2012sort$Whealth<-impsanifacurban2012sort$num_tb_cases/totaltb
impsanifacurban2012sort$CWhealth<-cumsum(impsanifacurban2012sort$Whealth)
impsanifacurban2012sort$logridit<-log10(impsanifacurban2012sort$ridit)
impsanifacurban2012sort$Wi<-sqrt(impsanifacurban2012sort$population)
impsanifacurban2012sort$XiWi<-impsanifacurban2012sort$Wi*impsanifacurban2012sort$logridit
impsanifacurban2012sort$YiWi<-impsanifacurban2012sort$Wi*impsanifacurban2012sort$ir_tb
fit2012<-lm(impsanifacurban2012sort$YiWi~impsanifacurban2012sort$Wi + impsanifacurban2012sort$XiWi + 0)
summary(fit2012)
impsanifacurban2012sort$predict2012<-coef(summary(fit2012))[1,1] + coef(summary(fit2012))[2,1]*impsanifacurban2012sort$logridit


impsanifacurban2009<-as.data.frame(impsanifacurban[impsanifacurban$year=="2009",])
impsanifacurban2009sort<-impsanifacurban2009[order(impsanifacurban2009$impsanifacurban),]
totalp=sum(impsanifacurban2009sort$population)
totaltb=sum(impsanifacurban2009sort$num_tb_cases)
impsanifacurban2009sort$Wpop<-impsanifacurban2009sort$population/totalp
impsanifacurban2009sort$CWpop<-cumsum(impsanifacurban2009sort$Wpop)
impsanifacurban2009sort$ridit<-c((0+impsanifacurban2009sort$CWpop[1])/2,(impsanifacurban2009sort$CWpop[1]+impsanifacurban2009sort$CWpop[2])/2,
                            (impsanifacurban2009sort$CWpop[2]+impsanifacurban2009sort$CWpop[3])/2,(impsanifacurban2009sort$CWpop[3]+impsanifacurban2009sort$CWpop[4])/2,
                            (impsanifacurban2009sort$CWpop[4]+impsanifacurban2009sort$CWpop[5])/2,(impsanifacurban2009sort$CWpop[5]+impsanifacurban2009sort$CWpop[6])/2,
                            (impsanifacurban2009sort$CWpop[6]+impsanifacurban2009sort$CWpop[7])/2,(impsanifacurban2009sort$CWpop[7]+impsanifacurban2009sort$CWpop[8])/2,
                            (impsanifacurban2009sort$CWpop[8]+impsanifacurban2009sort$CWpop[9])/2,(impsanifacurban2009sort$CWpop[9]+impsanifacurban2009sort$CWpop[10])/2,
                            (impsanifacurban2009sort$CWpop[10]+impsanifacurban2009sort$CWpop[11])/2,(impsanifacurban2009sort$CWpop[11]+impsanifacurban2009sort$CWpop[12])/2,
                            (impsanifacurban2009sort$CWpop[12]+impsanifacurban2009sort$CWpop[13])/2,(impsanifacurban2009sort$CWpop[13]+impsanifacurban2009sort$CWpop[14])/2,
                            (impsanifacurban2009sort$CWpop[14]+impsanifacurban2009sort$CWpop[15])/2,(impsanifacurban2009sort$CWpop[15]+impsanifacurban2009sort$CWpop[16])/2,
                            (impsanifacurban2009sort$CWpop[16]+impsanifacurban2009sort$CWpop[17])/2,(impsanifacurban2009sort$CWpop[17]+impsanifacurban2009sort$CWpop[18])/2,
                            (impsanifacurban2009sort$CWpop[18]+impsanifacurban2009sort$CWpop[19])/2,(impsanifacurban2009sort$CWpop[19]+impsanifacurban2009sort$CWpop[20])/2,
                            (impsanifacurban2009sort$CWpop[20]+impsanifacurban2009sort$CWpop[21])/2,(impsanifacurban2009sort$CWpop[21]+impsanifacurban2009sort$CWpop[22])/2,
                            (impsanifacurban2009sort$CWpop[22]+impsanifacurban2009sort$CWpop[23])/2)
impsanifacurban2009sort$Whealth<-impsanifacurban2009sort$num_tb_cases/totaltb
impsanifacurban2009sort$CWhealth<-cumsum(impsanifacurban2009sort$Whealth)
impsanifacurban2009sort$logridit<-log10(impsanifacurban2009sort$ridit)
impsanifacurban2009sort$Wi<-sqrt(impsanifacurban2009sort$population)
impsanifacurban2009sort$XiWi<-impsanifacurban2009sort$Wi*impsanifacurban2009sort$logridit
impsanifacurban2009sort$YiWi<-impsanifacurban2009sort$Wi*impsanifacurban2009sort$ir_tb
fit2009<-lm(impsanifacurban2009sort$YiWi~impsanifacurban2009sort$Wi + impsanifacurban2009sort$XiWi + 0)
summary(fit2009)
impsanifacurban2009sort$predict2009<-coef(summary(fit2009))[1,1] + coef(summary(fit2009))[2,1]*impsanifacurban2009sort$logridit


impsanifacurban2005<-as.data.frame(impsanifacurban[impsanifacurban$year=="2005",])
impsanifacurban2005sort<-impsanifacurban2005[order(impsanifacurban2005$impsanifacurban),]
totalp=sum(impsanifacurban2005sort$population)
totaltb=sum(impsanifacurban2005sort$num_tb_cases)
impsanifacurban2005sort$Wpop<-impsanifacurban2005sort$population/totalp
impsanifacurban2005sort$CWpop<-cumsum(impsanifacurban2005sort$Wpop)
impsanifacurban2005sort$ridit<-c((0+impsanifacurban2005sort$CWpop[1])/2,(impsanifacurban2005sort$CWpop[1]+impsanifacurban2005sort$CWpop[2])/2,
                            (impsanifacurban2005sort$CWpop[2]+impsanifacurban2005sort$CWpop[3])/2,(impsanifacurban2005sort$CWpop[3]+impsanifacurban2005sort$CWpop[4])/2,
                            (impsanifacurban2005sort$CWpop[4]+impsanifacurban2005sort$CWpop[5])/2,(impsanifacurban2005sort$CWpop[5]+impsanifacurban2005sort$CWpop[6])/2,
                            (impsanifacurban2005sort$CWpop[6]+impsanifacurban2005sort$CWpop[7])/2,(impsanifacurban2005sort$CWpop[7]+impsanifacurban2005sort$CWpop[8])/2,
                            (impsanifacurban2005sort$CWpop[8]+impsanifacurban2005sort$CWpop[9])/2,(impsanifacurban2005sort$CWpop[9]+impsanifacurban2005sort$CWpop[10])/2,
                            (impsanifacurban2005sort$CWpop[10]+impsanifacurban2005sort$CWpop[11])/2,(impsanifacurban2005sort$CWpop[11]+impsanifacurban2005sort$CWpop[12])/2,
                            (impsanifacurban2005sort$CWpop[12]+impsanifacurban2005sort$CWpop[13])/2,(impsanifacurban2005sort$CWpop[13]+impsanifacurban2005sort$CWpop[14])/2,
                            (impsanifacurban2005sort$CWpop[14]+impsanifacurban2005sort$CWpop[15])/2,(impsanifacurban2005sort$CWpop[15]+impsanifacurban2005sort$CWpop[16])/2,
                            (impsanifacurban2005sort$CWpop[16]+impsanifacurban2005sort$CWpop[17])/2,(impsanifacurban2005sort$CWpop[17]+impsanifacurban2005sort$CWpop[18])/2,
                            (impsanifacurban2005sort$CWpop[18]+impsanifacurban2005sort$CWpop[19])/2,(impsanifacurban2005sort$CWpop[19]+impsanifacurban2005sort$CWpop[20])/2,
                            (impsanifacurban2005sort$CWpop[18]+impsanifacurban2005sort$CWpop[20])/2,(impsanifacurban2005sort$CWpop[21]+impsanifacurban2005sort$CWpop[22])/2,
                            (impsanifacurban2005sort$CWpop[22]+impsanifacurban2005sort$CWpop[23])/2)
impsanifacurban2005sort$Whealth<-impsanifacurban2005sort$num_tb_cases/totaltb
impsanifacurban2005sort$CWhealth<-cumsum(impsanifacurban2005sort$Whealth)
impsanifacurban2005sort$logridit<-log10(impsanifacurban2005sort$ridit)
impsanifacurban2005sort$Wi<-sqrt(impsanifacurban2005sort$population)
impsanifacurban2005sort$XiWi<-impsanifacurban2005sort$Wi*impsanifacurban2005sort$logridit
impsanifacurban2005sort$YiWi<-impsanifacurban2005sort$Wi*impsanifacurban2005sort$ir_tb
fit2005<-lm(impsanifacurban2005sort$YiWi~impsanifacurban2005sort$Wi + impsanifacurban2005sort$XiWi + 0)
summary(fit2005)
impsanifacurban2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*impsanifacurban2005sort$logridit

impsanifacurban2000<-as.data.frame(impsanifacurban[impsanifacurban$year=="2000",])
impsanifacurban2000sort<-impsanifacurban2000[order(impsanifacurban2000$impsanifacurban),]
totalp=sum(impsanifacurban2000sort$population)
totaltb=sum(impsanifacurban2000sort$num_tb_cases)
impsanifacurban2000sort$Wpop<-impsanifacurban2000sort$population/totalp
impsanifacurban2000sort$CWpop<-cumsum(impsanifacurban2000sort$Wpop)
impsanifacurban2000sort$ridit<-c((0+impsanifacurban2000sort$CWpop[1])/2,(impsanifacurban2000sort$CWpop[1]+impsanifacurban2000sort$CWpop[2])/2,
                            (impsanifacurban2000sort$CWpop[2]+impsanifacurban2000sort$CWpop[3])/2,(impsanifacurban2000sort$CWpop[3]+impsanifacurban2000sort$CWpop[4])/2,
                            (impsanifacurban2000sort$CWpop[4]+impsanifacurban2000sort$CWpop[5])/2,(impsanifacurban2000sort$CWpop[5]+impsanifacurban2000sort$CWpop[6])/2,
                            (impsanifacurban2000sort$CWpop[6]+impsanifacurban2000sort$CWpop[7])/2,(impsanifacurban2000sort$CWpop[7]+impsanifacurban2000sort$CWpop[8])/2,
                            (impsanifacurban2000sort$CWpop[8]+impsanifacurban2000sort$CWpop[9])/2,(impsanifacurban2000sort$CWpop[9]+impsanifacurban2000sort$CWpop[10])/2,
                            (impsanifacurban2000sort$CWpop[10]+impsanifacurban2000sort$CWpop[11])/2,(impsanifacurban2000sort$CWpop[11]+impsanifacurban2000sort$CWpop[12])/2,
                            (impsanifacurban2000sort$CWpop[12]+impsanifacurban2000sort$CWpop[13])/2,(impsanifacurban2000sort$CWpop[13]+impsanifacurban2000sort$CWpop[14])/2,
                            (impsanifacurban2000sort$CWpop[14]+impsanifacurban2000sort$CWpop[15])/2,(impsanifacurban2000sort$CWpop[15]+impsanifacurban2000sort$CWpop[16])/2,
                            (impsanifacurban2000sort$CWpop[16]+impsanifacurban2000sort$CWpop[17])/2,(impsanifacurban2000sort$CWpop[17]+impsanifacurban2000sort$CWpop[18])/2,
                            (impsanifacurban2000sort$CWpop[18]+impsanifacurban2000sort$CWpop[19])/2,(impsanifacurban2000sort$CWpop[19]+impsanifacurban2000sort$CWpop[20])/2,
                            (impsanifacurban2000sort$CWpop[20]+impsanifacurban2000sort$CWpop[21])/2,(impsanifacurban2000sort$CWpop[21]+impsanifacurban2000sort$CWpop[22])/2,
                            (impsanifacurban2000sort$CWpop[22]+impsanifacurban2000sort$CWpop[23])/2)

impsanifacurban2000sort$Whealth<-impsanifacurban2000sort$num_tb_cases/totaltb
impsanifacurban2000sort$CWhealth<-cumsum(impsanifacurban2000sort$Whealth)
impsanifacurban2000sort$logridit<-log10(impsanifacurban2000sort$ridit)
impsanifacurban2000sort$Wi<-sqrt(impsanifacurban2000sort$population)
impsanifacurban2000sort$XiWi<-impsanifacurban2000sort$Wi*impsanifacurban2000sort$logridit
impsanifacurban2000sort$YiWi<-impsanifacurban2000sort$Wi*impsanifacurban2000sort$ir_tb
fit2000<-lm(impsanifacurban2000sort$YiWi~impsanifacurban2000sort$Wi + impsanifacurban2000sort$XiWi + 0)
summary(fit2000)
impsanifacurban2000sort$predict2000<-coef(summary(fit2000))[1,1] + coef(summary(fit2000))[2,1]*impsanifacurban2000sort$logridit

# Social gradient
slope_index_of_inequality_impsanifacurban2000<-fit2000$coefficients[2]
slope_index_of_inequality_impsanifacurban2005<-fit2005$coefficients[2]
slope_index_of_inequality_impsanifacurban2009<-fit2009$coefficients[2]
slope_index_of_inequality_impsanifacurban2012<-fit2012$coefficients[2]
round(slope_index_of_inequality_impsanifacurban2000,2)
round(slope_index_of_inequality_impsanifacurban2005,2)
round(slope_index_of_inequality_impsanifacurban2009,2)
round(slope_index_of_inequality_impsanifacurban2012,2)


mylabel1d= bquote(2000==.(format(slope_index_of_inequality_impsanifacurban2000,digits=4))) 
mylabel2d= bquote(2005==.(format(slope_index_of_inequality_impsanifacurban2005,digits=4))) 
mylabel3d= bquote(2009==.(format(slope_index_of_inequality_impsanifacurban2009,digits=4))) 
mylabel4d= bquote(2013==.(format(slope_index_of_inequality_impsanifacurban2012,digits=4))) 


quartz(width=10, height=6, pointsize=10)
plot(impsanifacurban2012sort$ridit,impsanifacurban2012sort$ir_tb, col="red", pch=0,
     ylab="Tuberculosis incidence rates per 100,000 population", 
     xlab="Country-level population gradient defined by access to improved sanitation facilities")
points(impsanifacurban2009sort$ridit,impsanifacurban2009sort$ir_tb, col="blue",pch=1,
       ylab="", 
       xlab="")
points(impsanifacurban2005sort$ridit,impsanifacurban2005sort$ir_tb, col="green",pch=2,
       ylab="", 
       xlab="")
points(impsanifacurban2000sort$ridit,impsanifacurban2000sort$ir_tb, col="purple",pch=3,
       ylab="", 
       xlab="")
lines(impsanifacurban2012sort$ridit,impsanifacurban2012sort$predict2012, col="red",lty=1,
      ylab="", 
      xlab="")
lines(impsanifacurban2009sort$ridit,impsanifacurban2009sort$predict2009, col="blue", lty=2,
      ylab="", 
      xlab="")
lines(impsanifacurban2005sort$ridit,impsanifacurban2005sort$predict2005, col="green", lty=3,
      ylab="", 
      xlab="")
lines(impsanifacurban2000sort$ridit,impsanifacurban2000sort$predict2000, col="purple", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2012","2009","2005","2000"),col=c("red","blue","green","purple"),pch=c(0,1,2,3),lty=c(1,2,3,4),cex = .8)
text(0.8,170, "Índice de desigualdad de la pendiente (IDP)", col="red")
text(0.8,160, labels=mylabel1d, col="red")
text(0.8,150, labels=mylabel2d, col="red")
text(0.8,140, labels=mylabel3d, col="red")
text(0.8,130, labels=mylabel4d, col="red")



