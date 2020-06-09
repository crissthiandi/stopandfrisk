
library(e1071)
library(naivebayes)
library(forcats)
library(caret)   #Para la matriz de confusión

base=read.csv("StopandFrisk2011.csv")
base=na.exclude(base)

###Todas las variables tienen que estar en factor
cols<- c("frisked","sex","race","build","arstmade","borough") 
base2=base[cols]

base2[cols]<-lapply(base[cols], as.factor)

levels(base2$sex)<-c("Mujer", "Hombre")

levels(base2$race)<-c("Black", "Black Hispanic", "White Hispanic", "White", "Asian/Pacific Islander", "Am. Indian/ Native Alaska")

levels(base2$build)<-c("Heavy", "Muscular", "Medium", "Thin")



###se divide en el grupo de prueba y de entrenamiento
set.seed(1500)
index<-sample(1:nrow(base2),0.3*nrow(base2),replace=FALSE) 
train<-base2[-index,cols] #Conjunto de datos de entrenamiento
test<-base2[index,cols]


#Modelo Naive Bayes

NB<-naiveBayes(ars~race+frisked+build+sex,data=train)
NB

#Matriz de confusion
predicciones<-predict(NB,newdata=test) #Realiza la prediccion
tab<-table(predicción=predicciones,Valores_reales=test$frisked) #Para visualizar
plot(predicciones)
tab
confusionMatrix(tab)#Para visualizar

