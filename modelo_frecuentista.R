require(readxl)
require(magrittr)
require(tidyverse)


direccion <- "C:/Users/Valen/Downloads/mortal_covid.xlsx"
mortal_covid <- read_excel(direccion)

#---Filtros---#

mortal_covid1<- mortal_covid %>% filter(Temp>0, AST>0, BUN>0, Ddimer >0, Lympho >0,Lympho < 100, Ddimer < 5, Troponin<1,AST<580, Creatinine<7, MAP>0, INR>0,Procalcitonin>0,Ferritin<100000)

#-----crear variable de severidad---#

severidad_grupos <-c()

for (i in 1:length(mortal_covid1$Severity)) {  #funcion para asignar valores a newvar
  if(mortal_covid1$Severity[i]>=6 &  mortal_covid1$Severity[i]<=11 ){severidad_grupos[i]<-3}
  else if(mortal_covid1$Severity[i]>=3 &  mortal_covid1$Severity[i]<=5){severidad_grupos[i]<-2}
  else if(mortal_covid1$Severity[i]>=0 & mortal_covid1$Severity[i]<=2 ){severidad_grupos[i]<-1}
  
}

mortal_covid1<-cbind(mortal_covid1,severidad_grupos)


x12 = as.factor(mortal_covid1$severidad_grupos)
class(x12)
#---- tranformacion de variables ---#

mortal_covid1$severidad_grupos<- as.factor(mortal_covid1$severidad_grupos)
class(severidad_grupos)
mortal_covid1$PVD<- as.factor(mortal_covid1$PVD)
class(PVD)
mortal_covid1$COPD<- as.factor(mortal_covid1$COPD)
mortal_covid1$`DM Complicated`<- as.factor(mortal_covid1$`DM Complicated`)
mortal_covid1$Stroke<- as.factor(mortal_covid1$Stroke)
mortal_covid1$OldSyncope<- as.factor(mortal_covid1$OldSyncope)

class(severidad_grupos)
#---- BOX-PLOT ---#



modelo1 <-glm(Death ~ Glucose+Temp+CHF+Age...26+Severity, data=mortal_covid1, family =  binomial(link = "logit"))

summary(modelo1)


?predict.glm

max(predict(modelo1, type="response"))


#---- evaluando una predicción---#

predict(modelo1, newdata=data.frame(Glucose=250, Temp=40,CHF=1,Age...26=80,Severity=1), type="response")
  
library(MASS) 

full.model <- lm(Death~ . , data=mortal_covid1)
summary(full.model)


modback <- stepAIC(full.model, trace=FALSE, direction="backward")
summary(modback)

modforw <- stepAIC(full.model, trace=FALSE, direction="forward")
summary(modforw)

modboth <- stepAIC(full.model, trace=FALSE, direction="both")
summary(modboth)



#---- probando el modelo  con las posibles variables---- #


modelo2 <-glm(Death ~LOS+ PVD+COPD+`DM Complicated`+Stroke +OldSyncope+Glucose+Age...26+OsSats+
                Temp+MAP+BUN+Glucose+AST+CrctProtein+Procalcitonin   +        
              Troponin, data=mortal_covid1, family =  binomial(link = "logit"))

summary(modelo2)

#MODELO 2 -AIC: 2624.5

#-------- MODELO SIN ALGUNAS VARIABLES -----#

#sin Dm- complicated 
#sin oldSyncope



modelo3 <-glm(Death ~LOS+ PVD+COPD+Stroke +Glucose+Age...26+OsSats+
                Temp+MAP+BUN+Glucose+AST+CrctProtein+Procalcitonin   +        
                Troponin+severidad_grupos, data=mortal_covid1, family =  binomial(link = "logit"))

summary(modelo3)



#----- MODELO 4-- #
#sin COPD
modelo4 <-glm(Death ~LOS+ PVD+Stroke +Glucose+Age...26+OsSats+
                Temp+MAP+BUN+Glucose+AST+CrctProtein+Procalcitonin   +        
                Troponin, data=mortal_covid1, family =  binomial(link = "logit"))

summary(modelo4)

#----- MODELO 5--- #
#sin glucose,sin AST, sin troponin
modelo5 <-glm(Death ~LOS+ PVD+Stroke+Age...26+OsSats+
                Temp+MAP+BUN+CrctProtein+Procalcitonin+as.factor(severidad_grupos)    
                , data=mortal_covid1, family =  binomial(link = "logit"))

modelo5$
summary(modelo5)

#TODAS DAN SIGNIFICATIVAS 


#EL MEJOR MODELO ES EL 5 AIC MENOR 

y = mortal_covid1$Death

x1 = (mortal_covid1$LOS - mean(mortal_covid1$LOS))/sd(mortal_covid1$LOS)
x2 = (mortal_covid1$Age...26 - mean(mortal_covid1$Age...26))/sd(mortal_covid1$Age...26)
x3 = (mortal_covid1$OsSats - mean(mortal_covid1$OsSats))/sd(mortal_covid1$OsSats)
x4 = (mortal_covid1$Temp - mean(mortal_covid1$Temp))/sd(mortal_covid1$Temp)
x5 = (mortal_covid1$MAP - mean(mortal_covid1$MAP))/sd(mortal_covid1$MAP)
x6 = (mortal_covid1$CrctProtein - mean(mortal_covid1$CrctProtein))/sd(mortal_covid1$CrctProtein)
x7 = (mortal_covid1$Procalcitonin - mean(mortal_covid1$Procalcitonin))/sd(mortal_covid1$Procalcitonin)
x8 = as.factor(mortal_covid1$severidad_grupos)

modelo6<- glm(y~x1+x2+x3+x4+x5+x6+x7+x8,data = mortal_covid1, family =  binomial(link = "logit"))
summary(modelo6)
