
#Trabajo final
rm(list=ls())


library(pacman)
p_load(tidyverse,
       caret,
       huxreg,
       haven,
       rpart,
       randomForest,
       xgboost)


Semestre_2021 <- readRDS("C:/Users/pcere/Dropbox/Machine Learning/Trabajo final/Semestre_2021.rds")


### tomamos las variables solo para transporte maritimo




Semestre_2021<-subset(Semestre_2021, VIATRANS==1)
#### comezamos a arreglar base

df <- Semestre_2021 %>% select(PAISPRO, DEPTODES, VIATRANS, ACUERDO, PNK, CODA, VAFODO, FLETE, VACID, IMP1, LUIN, COPAEX)


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

df <- df[!is.na(df$PAISPRO),]
df <- df[!is.na(df$DEPTODES),]
df <- df[!is.na(df$VAFODO),]
#convertimos COPAEX, LUIN, PAISPRO en factor

df$DEPTODES <- factor(df$DEPTODES) 

df$COPAEX <- factor(df$COPAEX) 

df$LUIN <- factor(df$LUIN)

df$PAISPRO <- factor(df$PAISPRO)

df$VAFODO_fact <- factor(df$VAFODO)
#generamos una interacci?n

df <- df %>% mutate(PNK2 = PNK^2,
                    PNK4 = PNK^4,
                    peso_acuerdo = PNK*ACUERDO)

# eliminar copaex porque est? generando ruido

df$COPAEX = NULL


#creamos bases train y test

set.seed(1712)
df <- df %>% mutate(holdout= as.logical(1:nrow(df) %in%
                                                  sample(nrow(df), nrow(df)*.3)))
test <- df[df$holdout==T,]
train <- df[df$holdout==F,]






### estad?sticas descriptivas

library(vtable)

#Getting complex
st(df, col.breaks = 15,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))

table(df$PAISPRO)

sum(is.na(df$VAFODO_fact))


#################################################
#Modelos#
#SE utilizará el paquete caret, por lo que se creará una función para ver 
#los estadísticos de comparación y una función de control con cross validation

FiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))


ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE,
                     summaryFunction = FiveStats)

#Definimos grilla para xgboost
grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1,3,5),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))
#Regresión tradicional

set.seed(1712)
reg_tranqui<-train(
  VAFODO ~ PAISPRO + DEPTODES + ACUERDO + PNK + PNK2 + IMP1 + LUIN + peso_acuerdo,
  data=train,
  method= "lm",
  trcontrol= ctrl
  #tuneLength=200
)
reg_tranqui

##
#tree






#Random Forest
set.seed(1712)
selva <- train(
  VAFODO_fact ~ PAISPRO + PNK,
  data=train,
  method = "rf",
  trControl = ctrl
)

#XGBoosting


set.seed(1712)
xgboost <- train(
  VAFODO_fact ~ PAISPRO + DEPTODES,
  data=train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid_default
)


watchlist = list(train=train, test=test)
model = xgb.train(data = train,
                  max.depth = 3,
                  nrounds = 100)
#Fin del script




