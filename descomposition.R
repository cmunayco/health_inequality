library(IC2)
library(ineq)
data(hhbudgets)

## Decomposition by Groups for Extended Gini Coefficient
decompSGini(x=hhbudgets[,"ingreso"], z=hhbudgets[,"estructura"], param=4)
decompSGini(x=hhbudgets[,"transporte"], z=hhbudgets[,"estructura"], w=hhbudgets[,"factor"], decomp="YL", ELMO=FALSE)
summary(decompSGini(x=hhbudgets[,"transporte"], z=hhbudgets[,"tenencia"], w=hhbudgets[,"factor"], param=1.5))

## Decomposition by Groups for GEI
decompGEI(hhbudgets[,"ingreso"], hhbudgets[,"estructura"], alpha=4)
summary(hhbudgets[,"tenencia"]) #35 NA􏰀s
decompGEI(x=hhbudgets[,"transporte"], z=hhbudgets[,"tenencia"], w=hhbudgets[,"factor"], ELMO=FALSE) 
summary(decompGEI(x=hhbudgets[,"transporte"], z=hhbudgets[,"tenencia"], w=hhbudgets[,"factor"], alpha=1.5))


 Decomposition by Groups for Atkinson Index
decompAtkinson(hhbudgets[,"ingreso"], hhbudgets[,"estructura"], epsilon=3)
summary(hhbudgets[,"tenencia"]) # 35 NA's
summary(decompAtkinson(x=hhbudgets[,"transporte"], z=hhbudgets[,"tenencia"], w=hhbudgets[,"factor"], decomp="DP", ELMO=FALSE))
summary(decompAtkinson(x=hhbudgets[,"transporte"], z=hhbudgets[,"tenencia"], w=hhbudgets[,"factor"], decomp="DP", epsilon=0.25))


