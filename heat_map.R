#mapa de calor
library(magrittr)
library(rstan)
require(readxl)
require(tidyverse)
library(HDInterval)
#--mapa de calor variables numéricas---


#--- paso 1:lectura de la base ---#
#pearson está entre -1 y 1 siendo -1 una relacion inversa fuerte y 1 una relación directa fuerte#

direccion <- "C:/Users/maria/OneDrive/Escritorio/SEMESTRE_2022-1/BAYESIANA/numericas_finales.xlsx"
mortal_numericas <- read_excel(direccion)
class(mortal_numericas$LOS)
class(mortal_numericas$Age)
mortal_numericas$Temp[mortal_numericas$Temp < 33.0] <- NA
mortal_numericas$Temp[mortal_numericas$Temp > 43.0] <- NA
mortal_numericas
#----paso2:calculo de correlaciones con pearson----#
pearson <- round(cor(mortal_numericas,  use="complete.obs", method="pearson"),2)
#pearson%>%  kbl() %>% kable_material(c("striped", "hover"), font_size = 12)%>% scroll_box(width = "100%", height = "400px")

#paso3:obtencion de los triagulos inferior y superior de la matriz de correlacion

# Get lower triangle of the correlation matrix
get_lower_tri<-function(pearson){
  pearson[upper.tri(pearson)] <- NA
  return(pearson)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(pearson){
  pearson[lower.tri(pearson)]<- NA
  return(pearson)
}
#llamado al triangulo superior
upper_tri <- get_upper_tri(pearson)

#paso4:generacion del mapa de calor
library(reshape2)#libreria necesaria
library(ggplot2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
melted_cormat
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low ="lightsteelblue1" , high = "deepskyblue3", mid = "lightcyan1"  , 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlación\nde Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()

ggheatmap

#añadimos la correlacion de pearson a cada recuadro
ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
