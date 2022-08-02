
#Trabajo final
rm(list=ls())
setwd("C:/Users/pcere/Dropbox/Machine Learning/Trabajo final")

library(pacman)
p_load(tidyverse,
       caret,
       huxreg,
       haven,
       rpart)

base_mar <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/Trabajo final/data/base_mar.rds")

#### comezamos a arreglar base

df <- base_mar %>% select(PAISPRO, DEPTODES, VIATRANS, ACUERDO, PNK, CODA, VAFODO, FLETE, VACID, IMP1, LUIN, COPAEX)


# alistamiento de variables

#convertimos numerica la variable acuerdo para luego dejarla como dummy

df$ACUERDO <- as.numeric(df$ACUERDO)

#los que no tienen acuerdo NA los ponemos en 0

df$ACUERDO[is.na(df$ACUERDO)] <- 0

#Dejamos todos los acuerdo como una dummy, si tiene algun acuerdo se pondr? 1

df$ACUERDO[df$ACUERDO > 0] <- 1


#eliminamos la observacion que tiene en peso neto 0, para eso primero lo convertimos en un nulo y luego se elimina

length(df$PNK)

df$PNK[df$PNK <= 0] <- NA


df <- df[!is.na(df$PNK),]


#convertimos COPAEX, LUIN, PAISPRO en factor

df$COPAEX <- factor(df$COPAEX) 

df$LUIN <- factor(df$LUIN)

df$PAISPRO <- factor(df$PAISPRO)

#generamos una interacci�n

df <- df %>% mutate(PNK2 = PNK^2,
                    PNK4 = PNK^4,
                    peso_acuerdo = PNK*ACUERDO)

# eliminar copaex porque est� generando ruido

df$COPAEX = NULL





### estad�sticas descriptivas

library(vtable)

#Getting complex
st(df, col.breaks = 12,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))

table(df$ACUERDO)

sum(is.na(df$PNK))
sum(df$CODA)














#################################################33
#Modelos#
#SE utilizará el paquete caret, por lo que se creará una función para ver 
#los estadísticos de comparación y una función de control con cross validation

FiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))


ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE,
                     summaryFunction = FiveStats)

#Regresión tradicional

set.seed(1712)
reg_tranqui<-train(
  VAFODO ~ PAISPRO + DEPTODES + ACUERDO + PNK + 
  #aqu? va la regresi?n
  data=base_2021,
  method= "rpart",
  trcontrol= ,
  tuneLength=200
)


##
base_mar <- subset(base_2021, VIATRANS==1)

table(base_mar$CLASE)



#Random Forest


#XGBoosting


#Fin del script




