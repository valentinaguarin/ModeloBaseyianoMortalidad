library(magrittr)
library(rstan)
require(readxl)
require(tidyverse)
library(HDInterval)

#--- data ---#

direccion <- "C:/Users/valen/Documents/mortal_covid.xlsx"
mortal_covid <- read_excel(direccion)

#View(mortal_covid)

#---convertir variables binarias a factor---#


mortal_covid$PVD %<>% as.factor
mortal_covid$`Derivation cohort` %<>% as.factor
mortal_covid$LOS_Y %<>% as.factor
mortal_covid$Black %<>% as.factor
mortal_covid$White %<>% as.factor
mortal_covid$Asian %<>% as.factor
mortal_covid$Latino %<>% as.factor
mortal_covid$MI %<>% as.factor

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

mortal_covid%>%str()

#-----crear variable de severidad---#

severidad_grupos <-c()

for (i in 1:length(mortal_covid$Severity)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Severity[i]>=6 &  mortal_covid$Severity[i]<=11 ){severidad_grupos[i]<-3}
  else if(mortal_covid$Severity[i]>=3 &  mortal_covid$Severity[i]<=5){severidad_grupos[i]<-2}
  else if(mortal_covid$Severity[i]>=0 & mortal_covid$Severity[i]<=2 ){severidad_grupos[i]<-1}
  
}

mortal_covid<-cbind(mortal_covid,severidad_grupos)

#--- convertirla en  factor--- #
mortal_covid$severidad_grupos %<>% as.factor

str(mortal_covid$severidad_grupos)


#----- corrección de variables binarias -----#

#-----crear variable de Glucose_range ---#

Glucose_range <-c()

for (i in 1:length(mortal_covid$Glucose)) {  #funcion para asignar valores a newvar
  if(mortal_covid$Glucose[i]<=60 |  mortal_covid$Glucose[i]>200 ){Glucose_range[i]<-1}
  else {Glucose_range[i]<-0}
  
}

mortal_covid<-cbind(mortal_covid,Glucose_range)

#--- convertirla en factor--- #
mortal_covid$Glucose_range %<>% as.factor


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


#---- verificar que este bien --#
str(mortal_covid$Ferritin_range)



#--- COCIENTE FALLECIDOS VS NO FALLECIDOSS POR NIVEL ----#

#----- severidad grupo 1 ----#
fallecidos1= sum(mortal_covid$severidad_grupos=="1" & mortal_covid$Death=="1")
no_fallecidos1= sum(mortal_covid$severidad_grupos=="1" & mortal_covid$Death=="0")


cociente_grupo1<- fallecidos1/no_fallecidos1
cociente_grupo1


#----- severidad grupo 2 ----#
fallecidos2= sum(mortal_covid$severidad_grupos=="2" & mortal_covid$Death=="1")
no_fallecidos2= sum(mortal_covid$severidad_grupos=="2" & mortal_covid$Death=="0")


cociente_grupo2<- fallecidos2/no_fallecidos2
cociente_grupo2

#----- severidad grupo 3 ----#
fallecidos3= sum(mortal_covid$severidad_grupos=="3" & mortal_covid$Death=="1")
no_fallecidos3= sum(mortal_covid$severidad_grupos=="3" & mortal_covid$Death=="0")


cociente_grupo3<- fallecidos3/no_fallecidos3
cociente_grupo3

summary(mortal_covid)


colnames(mortal_covid)


#---- quitar la variable age..5 ---- #

mortal_covid <-mortal_covid[,-5]
#view(mortal_covid)


#---- agregar Na en las variables que tienen problema en el rango ---#


#--- NA'S---#

#---Temp

mortal_covid$Temp[mortal_covid$Temp < 33.0] <- NA
mortal_covid$Temp[mortal_covid$Temp > 43.0] <- NA

class(mortal_covid$Temp)



#---OsSats

mortal_covid$OsSats[mortal_covid$OsSats < 60] <- NA

#view(mortal_covid)

#queda con 217 NAS

summary(mortal_covid)

view(mortal_covid)

#---- Selección de todas las variables ---#
library(MASS) 

full.model <- lm(Death~ . , data=mortal_covid)
summary(full.model)

modboth <- stepAIC(full.model, trace=FALSE, direction="both")
summary(modboth)



#-- aplicar filtro a OsSats---#
mortal_covid<- mortal_covid %>% filter(OsSats>60)

#---variables significativas de todas con los Na---#


modelo1 <-glm(Death ~ LOS_Y + Stroke + OldSyncope+Age...27+ OsSats+Temp+ MAP+ DDimerYes+
                Plts  +BUN+ CrtnYes + Creatinine + CrtnScore +AST + `IL6 > 150`+
                `Ferritin > 300`+ CrctProtein+ Procalcitonin + Troponin+  
                Ferritin_range + Glucose_range + severidad_grupos ,
                data=mortal_covid, family =  binomial(link = "logit"))

summary(modelo1)

#AIC: 3440.2


#--- modelo 3 sacando las que no son signficiativas ---#

modelo2 <-glm(Death ~ LOS_Y + Stroke +Age...27+ OsSats+ MAP+ DDimerYes+
                Plts  +BUN + Creatinine + CrtnScore +AST + `IL6 > 150`+
                `Ferritin > 300`+ CrctProtein+ Procalcitonin + Troponin+  
                Ferritin_range + Glucose_range + severidad_grupos ,
              data=mortal_covid, family =  binomial(link = "logit"))

summary(modelo2)

#AIC: 3466.6





