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
library(nlme)
library(sensitivity)
library(rgl)
library(Rglpk)
library(spatstat)
library(sp)
library(maps)
library(mapproj)
library(gpclib)
library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables
library(car)
library(plm)
library(lmtest)
library(gplots)
library(simecol)
library(latticeExtra)
library(VIM)
library(reshape)
library(psych)
library(gmodels)
library(GGally)
library(gvlma)
library(MVA)





#### Cargando bases de datos 
inequality<-read.csv("pahowbdfinal.csv")
##health_inequality<-read.csv("Healthindicators.csv")

inequality1<-subset(inequality,inequality$country!="Suriname" & inequality$country!="Cuba" & inequality$country!="Guyana" & inequality$year>1994)
inequality1<-subset(inequality1,inequality1$year>1994 & inequality1$year<2012)

inequality1$country <- factor(inequality1$country,labels = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Dominican Republic",
					 "Ecuador","El Salvador","Guatemala ","Haiti","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru","Trinidad and Tobago","Uruguay","Venezuela"))


df=data.frame(id=1:nrow(inequality1),year=inequality1$year,country=inequality1$country,country_code=inequality1$country_code,
							wb_income_group=inequality1$wb_income_group,wb_income_group_code=inequality1$wb_income_group_code,
							gdp=inequality1$GDP_per_capita_PPP_constant_2005_international, gni=inequality1$GNI_percapita_PPP_constant_2005_international,p.hex.gdp=(inequality1$Health_expenditure_percapita_PPP_constant_2005_international/inequality1$GDP_per_capita_PPP_constant_2005_international),
							hexp=inequality1$Health_expenditure_percapita_PPP_constant_2005_international,
							ir_tb=inequality1$tb_incidence,t_mort=inequality1$rate_deaths_hiv_neg_tb_cases,t_jail=inequality1$tasa_encarcelamiento_100k,
							urbgrow=inequality1$Urban_population_growth,phiv=inequality1$vih_prevalence,
							lexp=inequality1$esperanza_vida_nacer_anos,impswt=inequality1$Proporcion_poblacion_usa_fuentes_mejoradas_agua_potable,
							impsfac=inequality1$proporcion_poblacion_utiliza_instalaciones_mejoradas_saneamiento,
							bcg=inequality1$p_poblacion_menores_1ano_inmunizada_tuberculosis,unemploy=inequality1$prop_desem_fuerza_trabajo,
							AIDS=inequality1$tasa_incidencia_SIDA_por_100000_hab,p_hexp_gdp=inequality1$Health_expenditure_percapita_PPP_constant_2005_international/inequality1$GDP_per_capita_PPP_constant_2005_international,
							pnotified=inequality1$captacion, num_tb_cases=inequality1$num_incid_tb_case)

legend = c("Argentina", "Bolivia", "Brazil","Chile","Colombia","Costa Rica","Dominican Republic",
					 "Ecuador","El Salvador","Guatemala ","Haiti","Honduras","Mexico","Nicaragua",
					 "Panama","Paraguay","Peru","Trinidad and Tobago","Uruguay","Venezuela")


#################################################################
#######################  Panel data analysis ##################


##Regular OLS regression does not consider heterogeneity across groups or time
levels(df$country)
ols1<-lm(ir_tb ~ gni, data=df)
summary(ols1)

#### TB incidence rate  VS GNI
##Fixed effects using Least squares dummy variable model (departments)
fixed.dum1 <-lm(log(ir_tb) ~ log(gni) + factor(country) -1, data=df)
summary(fixed.dum1)

## Fixed effects: n entity-specific intercepts (using plm)
fixed1 <-plm(log(ir_tb) ~ log(gni), data=df, index=c("country", "year"), model="within")
summary(fixed1)
print(fixed1)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random1 <-plm(log(ir_tb) ~ log(gni), data=df, index=c("country", "year"), model="random")
summary(random1)

##fixed or random effects
phtest(fixed1, random1)

## Least squares dummy variable model
yhat1_ls<-fixed.dum1$fitted

yhat1_fx=fixed1$model[[1]] - fixed1$residuals
yhat1_rd=random1$model[[1]] - random1$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat1_ls~log(df$gni)|df$country, boxplots=FALSE,legend.coords="topright",legend.title="Countries",xlab="GNI per capita, PPP (current international $)", ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=677, pointsize=10)
scatterplot(yhat1_fx~log(df$gni)|df$country, boxplots=FALSE,legend.coords="topright",legend.title="Countries",xlab="GNI per capita, PPP (current international $)", ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat1_rd~log(df$gni)|df$country, boxplots=FALSE,legend.coords="topright",legend.title="Countries",xlab="log(GNI per capita, PPP (current international $))", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)


### IR TB VS health expenditure

## Fixed effects: n entity-specific intercepts (using plm)
fixed2 <-plm(log(ir_tb) ~ log(p.hex.gdp), data=df, index=c("country", "year"), model="within")
summary(fixed2)
print(fixed2)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random2 <-plm(log(ir_tb) ~ log(p.hex.gdp), data=df, index=c("country", "year"), model="random")
summary(random2)

##fixed or random effects
phtest(fixed2, random2)

yhat2_rd=random2$model[[1]] - random2$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat2_rd~df$hexp|df$country, boxplots=FALSE,legend.coords="topright",legend.title="Countries",
						xlab="Health expenditure per capita, PPP (current international $)", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat2_rd~df$hexp, boxplots=FALSE,legend.coords="topright",legend.title="Countries",
						xlab="Health expenditure per capita, PPP (current international $)", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

### IR TB VS Unemployment rate

## Fixed effects: n entity-specific intercepts (using plm)
fixed3 <-plm(log(ir_tb) ~ log(unemploy), data=df, index=c("country", "year"), model="within")
summary(fixed3)
print(fixed3)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random3 <-plm(log(ir_tb) ~ log(unemploy), data=df, index=c("country", "year"), model="random")
summary(random3)

##fixed or random effects
phtest(fixed3, random3)

yhat3_rd=random3$model[[1]] - random3$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat3_rd~log(df$unemploy)|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",xlab="log(Percentage of the workforce unemployed)", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

### IR TB VS Urban population growth

## Fixed effects: n entity-specific intercepts (using plm)
fixed4 <-plm(log(ir_tb) ~ log(urbgrow), data=df, index=c("country", "year"), model="within")
summary(fixed4)
print(fixed4)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random4 <-plm(ir_tb ~ urbgrow, data=df, index=c("country", "year"), model="random")
summary(random4)

##fixed or random effects
phtest(fixed4, random4)

yhat4_rd=random4$model[[1]] - random4$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat4_rd~df$urbgrow|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
xlab="Urban population growth", 
ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat4_rd~df$urbgrow, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
xlab="Urban population growth", 
ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)


### IR TB VS Incarceration rate per 100,000 population

## Fixed effects: n entity-specific intercepts (using plm)
fixed5 <-plm(log(ir_tb) ~ log(t_jail), data=df, index=c("country", "year"), model="within")
summary(fixed5)
print(fixed5)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random5 <-plm(log(ir_tb) ~ log(t_jail), data=df, index=c("country", "year"), model="random")
summary(random5)

##fixed or random effects
phtest(fixed5, random5)

yhat5_rd=random5$model[[1]] - random5$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat5_rd~log(df$t_jail)|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",xlab="log(Incarceration rate per 100,000 population)", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)


### IR TB VS Life expectancy

## Fixed effects: n entity-specific intercepts (using plm)
fixed6 <-plm(ir_tb ~ lexp, data=df, index=c("country", "year"), model="within")
summary(fixed6)
print(fixed6)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random6 <-plm(ir_tb ~ lexp, data=df, index=c("country", "year"), model="random")
summary(random6)

##fixed or random effects
phtest(fixed6, random6)

yhat6_rd=random6$model[[1]] - random6$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat6_rd~df$lexp|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
						xlab="Life expectancy", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat6_rd~df$lexp, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
						xlab="Life expectancy", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)


### IR TB VS Proportion of the population using improved drinking water

## Fixed effects: n entity-specific intercepts (using plm)
fixed7 <-plm(log(ir_tb) ~ log(impswt), data=df, index=c("country", "year"), model="within")
summary(fixed7)
print(fixed7)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random7 <-plm(ir_tb ~ impswt, data=df, index=c("country", "year"), model="random")
summary(random7)

##fixed or random effects
phtest(fixed7, random7)

yhat7_rd=random7$model[[1]] - random7$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat7_rd~df$impswt|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
						xlab="Proportion of the population using improved drinking water", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat7_rd~df$impswt, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",
						xlab="Proportion of the population using improved drinking water", 
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)


### IR TB VS Proportion of the population using improved sanitation facilities

## Fixed effects: n entity-specific intercepts (using plm)
fixed8 <-plm(log(ir_tb) ~ log(impsfac), data=df, index=c("country", "year"), model="within")
summary(fixed8)
print(fixed8)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random8 <-plm(ir_tb ~ impsfac, data=df, index=c("country", "year"), model="random")
summary(random8)

##fixed or random effects
phtest(fixed8, random8)

yhat8_rd=random8$model[[1]] - random8$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat8_rd~df$impsfac|df$country, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",
						xlab="Proportion of the population using improved sanitation facilities",
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat8_rd~df$impsfac, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",
						xlab="Proportion of the population using improved sanitation facilities",
						ylab="Predicted Tuberculosis incidence rates per 100,000 population",cex = 0.6, smooth=FALSE)

### IR TB VS HIV prevalence

## Fixed effects: n entity-specific intercepts (using plm)
fixed9 <-plm(log(ir_tb) ~ log(phiv), data=df, index=c("country", "year"), model="within")
summary(fixed9)
print(fixed9)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random9 <-plm(log(ir_tb) ~ log(phiv), data=df, index=c("country", "year"), model="random")
summary(random9)

##fixed or random effects
phtest(fixed9, random9)

yhat9_rd=random9$model[[1]] - random9$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat9_rd~log(df$phiv)|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",xlab="log(HIV prevalence)", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

### IR TB VS Incidence rates of AIDS per 100,000 population

## Fixed effects: n entity-specific intercepts (using plm)
fixed10 <-plm(log(ir_tb) ~ log(AIDS), data=df, index=c("country", "year"), model="within")
summary(fixed10)
print(fixed10)

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model)
random10 <-plm(log(ir_tb) ~ log(AIDS), data=df, index=c("country", "year"), model="random")
summary(random10)

##fixed or random effects
phtest(fixed10, random10)

yhat10_rd=random10$model[[1]] - random10$residuals

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat10_rd~log(df$AIDS)|df$country, boxplots=FALSE,legend.coords="topleft",legend.title="Countries",xlab="log(Incidence rates of AIDS per 100,000 population)", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
radius <- sqrt(df$t_jail/ pi )
symbols(df$gni, df$ir_tb, circles=radius, inches=0.35, xlab="GNI", ylab="Tuberculosis Incidence Rates per 100,000 population")
text(df$gni, df$ir_tb, df$country_code, cex=0.5)

###################################
#### Multivariate analysis ######## 
###################################

##RANDOM-EFFECTS MODEL(Random Intercept, Partial Pooling Model) 
### SETP 1

randomf2 <-plm(log(ir_tb) ~ log(gdp), data=df, index=c("country", "year"), model="random")
summary(randomf2)

randomf3 <-plm(log(ir_tb) ~ log(p.hex.gdp), data=df, index=c("country", "year"), model="random")
summary(randomf3)

randomf4 <-plm(log(ir_tb) ~ log(phiv), data=df, index=c("country", "year"), model="random")
summary(randomf4)

randomf5 <-plm(log(ir_tb) ~ log(AIDS), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf5)

randomf6 <-plm(log(ir_tb) ~ log(lexp), data=df, index=c("country", "year"), model="random")
summary(randomf6)

randomf7 <-plm(log(ir_tb) ~ log(pnotified), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf7)

randomf8 <-plm(log(ir_tb) ~ log(impswt), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf8)

randomf9 <-plm(log(ir_tb) ~ log(impsfac), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf9)

randomf10 <-plm(log(ir_tb) ~ log(unemploy), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf10)

randomf11 <-plm(log(ir_tb) ~ log(t_jail), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf11)

randomf12 <-plm(log(ir_tb) ~ log(urbgrow), data=df, na.action=na.omit,index=c("country", "year"), model="random")
summary(randomf12)

#### STEP 2

randomf1 <-plm(log(ir_tb) ~ log(gdp) + log(gni), data=df, index=c("country", "year"), model="random")
summary(randomf1)


randomf2 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp), data=df, index=c("country", "year"), model="random")
summary(randomf2)

randomf3 <-plm(log(ir_tb) ~ log(gdp) + log(impsfac), data=df, index=c("country", "year"), model="random")
summary(randomf3)

randomf4 <-plm(log(ir_tb) ~ log(gdp) + log(impswt), data=df, index=c("country", "year"), model="random")
summary(randomf4)

randomf5 <-plm(log(ir_tb) ~ log(gdp) + log(pnotified), data=df, index=c("country", "year"), model="random")
summary(randomf5)

randomf6 <-plm(log(ir_tb) ~ log(gdp) + log(t_jail), data=df, index=c("country", "year"), model="random")
summary(randomf6)


#### STEP 3
randomf1 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(impsfac), data=df, index=c("country", "year"), model="random")
summary(randomf1)

randomf2 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(impswt), data=df, index=c("country", "year"), model="random")
summary(randomf2)

randomf3 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(t_jail), data=df, index=c("country", "year"), model="random")
summary(randomf3)

randomf3 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(pnotified), data=df, index=c("country", "year"), model="random")
summary(randomf3)

#### STEP 4

randomf1 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(pnotified) + log(impsfac), data=df, index=c("country", "year"), model="random")
summary(randomf1)

randomf2 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(pnotified) + log(t_jail), data=df, index=c("country", "year"), model="random")
summary(randomf2)

randomf3 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(pnotified) + log(impswt), data=df, index=c("country", "year"), model="random")
summary(randomf3)

#### STEP 5
randomf1 <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp) + log(pnotified) + log(impsfac) + log(t_jail), data=df, index=c("country", "year"), model="random")
summary(randomf1)

#### Final model
randomf <-plm(log(ir_tb) ~ log(gdp) + log(p.hex.gdp)  + log(pnotified) + log(impsfac), data=df, index=c("country", "year"), model="random")
summary(randomf)
yhat=randomf$model[[1]] - randomf$residuals

vif(randomf)
quartz(width=10, height=6, pointsize=10)
scatterplot(yhat~log(df$hexp)|df$country, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",xlab="log(Health expenditure per capita, PPP (constant 2005 international $))", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat~log(df$lexp)|df$country, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",xlab="log(Life expectancy at birth, total (years))", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat~log(df$pnotified)|df$country, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",xlab="log(Notified cases (% of estimated tuberculosis cases))", ylab="log(Predicted Tuberculosis incidence rates per 100,000 population)",cex = 0.6, smooth=FALSE)

quartz(width=10, height=6, pointsize=10)
scatterplot(yhat~log(df$impsfac)|df$country, boxplots=FALSE,legend.coords="bottomleft",legend.title="Countries",xlab="log(Life expectancy at birth, total (years))", ylab="log(Improved sanitation facilities (% of population with access))",cex = 0.6, smooth=FALSE)

