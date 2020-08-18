# NAIVE BAYES #

#Librerías requeridas
library(e1071)
library(naivebayes)
library(forcats)
library(dplyr)
library(caret)   #Para la matriz de confusión

baseoriginal=read.csv("../Proyecto/StopandFrisk2011.csv")
names(baseoriginal)
base=baseoriginal[,c(7,8,15,17,26,6)]
base=na.exclude(base)
str(base)
names(base)
###Todas las variables tienen que estar en factor
cols<- c("frisked","sex","race","build","arstmade","borough") 
base[cols]<-lapply(base[cols], as.factor)

levels(base$sex)<-c("Mujer", "Hombre")

levels(base$race)<-c("Black", "Black Hispanic", "White Hispanic", "White", "Asian/Pacific Islander", "Am. Indian/ Native Alaska")

levels(base$build)<-c("Heavy", "Muscular", "Medium", "Thin")

base2=base %>% mutate(build = fct_recode(build,
                            "Heavy"="Heavy",
                            "Heavy"="Muscular",
                            "No Heavy"="Medium",
                            "No Heavy"="Thin"
                            )) 

base2=base2 %>% mutate(race = fct_recode(race,
                                        "Black"="Black", 
                                        "No Black"="Black Hispanic", 
                                        "No Black"="White Hispanic", 
                                        "No Black"="White", 
                                        "No Black"="Asian/Pacific Islander", 
                                        "No Black"="Am. Indian/ Native Alaska"
                                        ))

str(base2)

###se divide en el grupo de prueba y de entrenamiento
set.seed(1500)
index<-sample(1:nrow(base),0.3*nrow(base2),replace=FALSE) 
train<-base2[-index,cols] #Conjunto de datos de entrenamiento
test<-base2[index,cols]


#Modelo Naive Bayes

NB<-naiveBayes(arstmade~race+frisked+build+sex,data=train)
NB

#Matriz de confusion
predicciones<-predict(NB,newdata=test) #Realiza la prediccion
tab<-table(predicción=predicciones,Valores_reales=test$arstmade) #Para visualizar
plot(predicciones)
tab
confusionMatrix(tab)#Para visualizar



