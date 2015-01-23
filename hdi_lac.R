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

#### Shaping the population dataset ############################
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

#### Shaping the HDI dataset ############################

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

################# Shaping TB data ######################################

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

tb_lac$country<-factor(tb_lac$country, label = c("Antigua and Barbuda","Argentina", "Bahamas", "Barbados","Belize", "Bolivia", "Brazil","Canada","Chile",
                                                   "Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador","Grenada",
                                                   "Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
                                                   "Peru","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Suriname","Trinidad and Tobago",
                                                   "United States of America","Uruguay","Venezuela"))

tb_lac_f<-subset(tb_lac,country %in% c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba",
                                         "Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Haiti","Honduras",
                                         "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                                         "Uruguay","Venezuela"))
tb_lac_f<-subset(tb_lac_f, tb_lac_f$year>=1995)

#########################################################################
################# Building the final dataset ############################  
tb_lac_ff<-merge(tb_lac_f,population_f,by=c("country","year"),all=TRUE)


df1<-subset(inequality,year %in% c(2000,2005,2009,2012))

dff<-merge(df1,hdi_lac_ff,by=c("country","year"),all=TRUE)



lengend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Cuba","Dominican Republic",
            "Ecuador","El Salvador","Guatemala ","Guyana","Haiti","Honduras","Mexico","Nicaragua",
            "Panama","Paraguay","Peru","Suriname","Trinidad and Tobago","Uruguay","Venezuela")


##### Health inequality for GDP #####

hdi<-dff[, c("country","year",)]

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
gdp2010sort$logridit<-log(gdp2010sort$ridit)
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
gdp2005sort$logridit<-log(gdp2005sort$ridit)
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
gdp2000sort$logridit<-log(gdp2000sort$ridit)
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

