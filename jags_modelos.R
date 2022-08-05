library(R2jags)
require(readxl)
require(magrittr)
require(tidyverse)
library(HDInterval)


direccion <- "C:/Users/valen/Documents/mortal_covid.xlsx"
mortal_covid <- read_excel(direccion)
dim(mortal_covid)


mortal_covid<- mortal_covid %>% filter(OsSats>60)
dim(mortal_covid)

#---eliminar variable ---#

colnames(mortal_covid)
mortal_covid <- mortal_covid[,-5]




#-----crear variable de severidad---#

severidad_grupos <-c()

for (i in 1:length(mortal_covid$Severity)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Severity[i]>=6 &  mortal_covid$Severity[i]<=11 ){severidad_grupos[i]<-3}
  else if(mortal_covid$Severity[i]>=3 &  mortal_covid$Severity[i]<=5){severidad_grupos[i]<-2}
  else if(mortal_covid$Severity[i]>=0 & mortal_covid$Severity[i]<=2 ){severidad_grupos[i]<-1}
  
}

mortal_covid<-cbind(mortal_covid,severidad_grupos)


#-----crear variable de Glucose_range ---#

Glucose_range <-c()

for (i in 1:length(mortal_covid$Glucose)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Glucose[i]<=60 |  mortal_covid$Glucose[i]>200 ){Glucose_range[i]<-1}
  else {Glucose_range[i]<-0}
  
}

mortal_covid<-cbind(mortal_covid,Glucose_range)

#--- convertirla en  factor--- #
mortal_covid$severidad_grupos %<>% as.factor

str(mortal_covid$severidad_grupos)



#---- verificar que este bien --#
str(mortal_covid$Glucose_range)




#-----crear variable de Sodium_range ---#

Sodium_range <-c()

for (i in 1:length(mortal_covid$Sodium)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Sodium[i]<125 |  mortal_covid$Sodium[i]>154 ){Sodium_range[i]<-1}
  else {Sodium_range[i]<-0}
  
}

mortal_covid<-cbind(mortal_covid,Sodium_range)

#--- convertirla en factor--- #
mortal_covid$Sodium_range %<>% as.factor


#---- verificar que este bien --#
str(mortal_covid$Sodium_range)




#-----crear variable de Ferritin_range ---#

Ferritin_range <-c()

for (i in 1:length(mortal_covid$Ferritin)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Ferritin[i]<300 ){Ferritin_range[i]<-1}
  else {Ferritin_range[i]<-0}
  
}
mortal_covid<-cbind(mortal_covid,Ferritin_range)

#--- convertirla en numerica--- #
mortal_covid$Ferritin_range %<>% as.factor








#---- tranformacion de variables ---#

#---convertir variables binarias a factor---#


mortal_covid$`Derivation cohort` %<>% as.factor
mortal_covid$LOS_Y %<>% as.factor
mortal_covid$Black %<>% as.factor
mortal_covid$White %<>% as.factor
mortal_covid$Asian %<>% as.factor
mortal_covid$Latino %<>% as.factor
mortal_covid$MI %<>% as.factor
mortal_covid$PVD %<>% as.factor
mortal_covid$CHF %<>% as.factor
mortal_covid$CVD %<>% as.factor
mortal_covid$DEMENT %<>% as.factor
mortal_covid$COPD %<>% as.factor
mortal_covid$`DM Complicated` %<>% as.factor
mortal_covid$`DM Simple` %<>% as.factor
mortal_covid$`Renal Disease` %<>% as.factor
mortal_covid$`All CNS` %<>% as.factor
mortal_covid$`Pure CNS` %<>% as.factor
mortal_covid$Stroke %<>% as.factor
mortal_covid$Seizure %<>% as.factor
mortal_covid$OldSyncope %<>% as.factor
mortal_covid$OldOtherNeuro %<>% as.factor
mortal_covid$OtherBrnLsn %<>% as.factor

mortal_covid$O2SatsYes %<>% as.factor
mortal_covid$TempYes %<>% as.factor
mortal_covid$MapYes %<>% as.factor
mortal_covid$DDimerYes %<>% as.factor
mortal_covid$PltsYes %<>% as.factor
mortal_covid$PltsScore %<>% as.factor
mortal_covid$CrtnScore %<>% as.factor
mortal_covid$INRYes %<>% as.factor
mortal_covid$TempYes %<>% as.factor
mortal_covid$BUNYes %<>% as.factor
mortal_covid$CrtnYes %<>% as.factor
mortal_covid$SodimuYes %<>% as.factor
mortal_covid$GlucoseYese %<>% as.factor
mortal_covid$ASTYes %<>% as.factor
mortal_covid$ALTYes %<>% as.factor
mortal_covid$WBCYes%<>% as.factor
mortal_covid$LymphoYes %<>% as.factor
mortal_covid$IL6Yes %<>% as.factor
mortal_covid$FerritinYes %<>% as.factor
mortal_covid$CrctProtYes %<>% as.factor
mortal_covid$ProCalCYes %<>% as.factor
mortal_covid$TropYes %<>% as.factor


mortal_covid$`INR > 1.2` %<>% as.factor
mortal_covid$`MAP < 70` %<>% as.factor
mortal_covid$`O2 Sat < 94` %<>% as.factor
mortal_covid$`Temp > 38` %<>% as.factor
mortal_covid$`D-Dimer > 3` %<>% as.factor
mortal_covid$`Troponin > 0.1` %<>% as.factor
mortal_covid$`BUN > 30` %<>% as.factor
mortal_covid$`Sodium < 139 or > 154` %<>% as.factor
mortal_covid$`Glucose <60 or > 500` %<>% as.factor
mortal_covid$`Ferritin > 300` %<>% as.factor
mortal_covid$`AST > 40` %<>% as.factor
mortal_covid$`ALT > 40` %<>% as.factor
mortal_covid$`WBC <1.8 or > 4.8` %<>% as.factor
mortal_covid$`IL6 > 150` %<>% as.factor
mortal_covid$`C-Reactive Prot > 10` %<>% as.factor
mortal_covid$`Procalciton > 0.1` %<>% as.factor
mortal_covid$`Lymphocytes < 1` %<>% as.factor
mortal_covid$`Troponin > 0.1` %<>% as.factor


summary(mortal_covid)







#--- variables  modelo---#

#coincideieron
attach(mortal_covid)

y = Death

x1 = (LOS-mean(LOS))/sd(LOS)
x2 = Stroke
x3 = (Age...27 - mean(Age...27))/sd(Age...27)
x4 = (OsSats-mean(OsSats))/sd(OsSats)
x5 = (MAP - mean(MAP))/sd(MAP)
x6 = (Plts - mean(Plts))/sd(Plts)
x7 = (CrctProtein - mean(CrctProtein))/sd(CrctProtein)
x8 = (Procalcitonin - mean(Procalcitonin))/sd(Procalcitonin)
x9 = Ferritin_range
x10 = Glucose_range
x11 = DDimerYes
x12 = `BUN > 30`
x13 = `IL6 > 150`
x14 = severidad_grupos




X = model.matrix(~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)

head(X)
dim(X)
#--- modelo ---#

modelo1 <- function(){
  for (i in 1:N) {
    y[i] ~ dbern(p[i])   #variable respuesta
    
    logit(p[i]) <- inprod(b[], x[i,])  #predictor lineal
  }
  for (j in 1:K) {
    b[j] ~ dnorm(0,1.0E-12)  #apriori
  }
}



#Tamano de la muestra
N = dim(mortal_covid)[1]

# Input o informacion de entrada
data.input.jags <- list(y=y, x = X, N = N, K = ncol(X))


#Parametros a monitorear
bayes.mod.params <- c("b")


#Puntos iniciales de la cadena MCMC
bayes.mod.inits <- function(){
  list("b" = rnorm(15))  #cantidad de variables 
}


set.seed(123)
bayes.mod.fit <- jags(data = data.input.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 10000,
                      n.burnin = 1000, model.file = modelo1)

print(bayes.mod.fit)

##### DIC: 3792.6








