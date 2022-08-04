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
view(mortal_covid)



#------ selección variables numéricas -----#

modelo_numericas <- glm(Death ~ LOS + Age...27 + MAP + Ddimer + Plts + INR + Troponin + Glucose + BUN + Creatinine + Sodium + Ferritin + Plts +
                          Lympho + AST + ALT + WBC + Lympho + IL6 + CrctProtein + Procalcitonin + Troponin, data = mortal_covid, family = 
                          binomial(link = "logit"))

summary(modelo_numericas)


#----- Selección de variables binarias---#


modelo_binarias <- glm(Death ~ `Derivation cohort`+ LOS_Y + Black + White + Asian + Latino + MI + PVD + CHF + CVD +
                         DEMENT + COPD + `DM Complicated` + `DM Simple` + `Renal Disease` + `All CNS` + `Pure CNS` +
                         Stroke + Seizure + OldSyncope + OldOtherNeuro + OtherBrnLsn + O2SatsYes + TempYes + MapYes+
                         DDimerYes + PltsYes + PltsScore + CrtnScore + INRYes + TempYes + BUNYes + CrtnYes + SodimuYes +
                         GlucoseYese + ASTYes + ALTYes + WBCYes + LymphoYes + IL6Yes + FerritinYes + CrctProtYes + 
                         ProCalCYes + TropYes + `INR > 1.2` + `MAP < 70` + `O2 Sat < 94` + `Temp > 38` + `D-Dimer > 3` +
                         `Troponin > 0.1` + `BUN > 30` + `Sodium < 139 or > 154` + `Glucose <60 or > 500` + `Ferritin > 300`+
                         `AST > 40` + `ALT > 40` + `WBC <1.8 or > 4.8` + `IL6 > 150` + `C-Reactive Prot > 10` + `Procalciton > 0.1`+
                         `Lymphocytes < 1` + `Troponin > 0.1`+ severidad_grupos, data = mortal_covid, family = 
                         binomial(link = "logit"))

summary(modelo_binarias)


#---- Selección de todas las variables ---#
library(MASS) 

full.model <- lm(Death~ . , data=mortal_covid)
summary(full.model)

modboth <- stepAIC(full.model, trace=FALSE, direction="both")
summary(modboth)


#---- probar modelo con todas las variables significativas ----#

modelo1 <-glm(Death ~ LOS_Y + LOS + PVD + COPD+
                Stroke + Seizure + OldSyncope + Age...27+
                OsSats + `O2 Sat < 94`+ Temp + MAP + `MAP < 70`+
                Plts + BUN + CrtnScore + `AST > 40` + `IL6 > 150` +
                CrctProtein+Procalcitonin   + Troponin +
                Ferritin_range + Glucose_range + severidad_grupos, data=mortal_covid, family =  binomial(link = "logit"))

summary(modelo1)

#AIC: 3614

modelo2 <-glm(Death ~  LOS + PVD  + Age...27+
                `O2 Sat < 94`+ Temp + MAP + `MAP < 70`+
                Plts + BUN + CrtnScore + `AST > 40` + `IL6 > 150` +
                CrctProtein+Procalcitonin   + Troponin +
                Ferritin_range + Glucose_range + severidad_grupos, data=mortal_covid, family =  binomial(link = "logit"))

summary(modelo2)

#AIC: 3743.3

#---variables significativas ---#

modelo3 <-glm(Death ~ LOS_Y + Stroke + Age...27+ AgeScore+ MAP+ `MAP < 70`+ Plts + BUNYes + BUN + CrtnYes +
                `AST > 40` + `IL6 > 150` + `Ferritin > 300` + Procalcitonin + `Procalciton > 0.1` + Troponin
              ,data=mortal_covid1, family =  binomial(link = "logit"))

summary(modelo3)

#AIC: 3634





