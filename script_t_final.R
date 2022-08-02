#Trabajo final
rm(list=ls())
setwd("C:/Users/pcere/Dropbox/Machine Learning/Trabajo final")

library(pacman)
p_load(tidyverse,
       caret,
       huxreg,
       haven,
       rpart)



#### comezamos a arreglar base

df <- base_mar %>% select(PAISPRO, DEPTODES, VIATRANS, ACUERDO, PNK, CODA, VAFODO, FLETE, VACID, IMP1, LUIN, COPAEX)


# alistamiento de variables

#convertimos numerica la variable acuerdo para luego dejarla como dummy

df$ACUERDO <- as.numeric(df$ACUERDO)

#los que no tienen acuerdo NA los ponemos en 0

df$ACUERDO[is.na(df$ACUERDO)] <- 0

#Dejamos todos los acuerdo como una dummy, si tiene algun acuerdo se pondrá 1

df$ACUERDO[df$ACUERDO > 0] <- 1


#eliminamos la observacion que tiene en peso neto 0, para eso primero lo convertimos en un nulo y luego se elimina

length(df$PNK)

df$PNK[df$PNK <= 0] <- NA


df <- df[!is.na(df$PNK),]


#convertimos COPAEX, LUIN, PAISPRO en factor

df$COPAEX <- factor(df$COPAEX) 

df$LUIN <- factor(df$LUIN)

df$PAISPRO <- factor(df$PAISPRO)




table(df$PNK > 0)

sum(is.na(df$PNK))























#Cargamos base para 2021

#Completa_2021 <- read_dta('data/IMPO.dta')

#saveRDS(Completa_2021, file="base_2021.rds")


#Ponemos ctrl para el cross validation
FiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
##Función para regresiones
ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE,
                     summaryFunction = FiveStats)

base_2021 <- readRDS('data/base_2021.rds')

set.seed(1712)
arbol<-train(
  #aquí va la regresión
  data=base_2021,
  method= "rpart",
  trcontrol= ,
  tuneLength=200
)


base_mar <- subset(base_2021, VIATRANS==1)

table(base_mar$CLASE)


#Fin del script




