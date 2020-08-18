library(cluster) #Hacer análisis de clusters
library(factoextra) #Visualizar el cluster
library(dplyr) ##se usar? para la funcion select
#####
setwd("~/Semestre 6.8/Mineria de Datos/proyecto/Parte 2")
base<-read.csv("StopandFrisk2011.csv")
#base<-na.omit(base)
#View(base)
##### 
######clasificamos a los detenidos por semana del año #uso de merge en la base
date<-unique(base$datestop)
date
nm<-seq.int(from = 1, to = 365)
nm
ab<-data.frame(date,nm)
ab$week<-floor(((ab$nm+6)/7))
ab
ab$week[365]=52
ab<-select(ab,-nm)
base<-merge(base,ab,by.x = "datestop",by.y = "date") 

#####
######creamos vectores vacios para rellenar con la informacion de las variables
semana<-seq.int(from = 1, to = 52)
d=nrow(base)
detn<-rep(0,52) #detenidos arstmade
detncris=rep(0,52)
v1<-rep(0,52)
v2<-rep(0,52)
v3<-rep(0,52)
v4<-rep(0,52)
v5<-rep(0,52)
v6<-rep(0,52)
v7<-rep(0,52)
v8<-rep(0,52)
v9<-rep(0,52)
v10<-rep(0,52)


#####
#creación de las base de datos
for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      detn[i]=detn[i]+1
    }
  }
}


detncris=base %>% count(week)

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v1[i]=v1[i]+base$cs_objcs[j]
    }
  }
}

#mejora cris

v1cris=NULL
v1cris= base %>% group_by(week) %>% count(cs_objcs) %>% filter(cs_objcs==1)
v1cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v2[i]=v2[i]+base$cs_descr[j]
    }
  }
}

v2cris=NULL
v2cris= base %>% group_by(week) %>% count(cs_descr) %>% filter(cs_descr==1)
v2cris



for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v3[i]=v3[i]+base$cs_casng[j]
    }
  }
}

v3cris=NULL
v3cris= base %>% group_by(week) %>% count(cs_casng) %>% filter(cs_casng==1)
v3cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v4[i]=v4[i]+base$cs_lkout[j]
    }
  }
}

v4cris=NULL
v4cris= base %>% group_by(week) %>% count(cs_lkout) %>% filter(cs_lkout==1)
v4cris



for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v5[i]=v5[i]+base$cs_cloth[j]
    }
  }
}

v5cris=NULL
v5cris= base %>% group_by(week) %>% count(cs_cloth) %>% filter(cs_cloth==1)
v5cris


for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v6[i]=v6[i]+base$cs_drgtr[j]
    }
  }
}

v6cris=NULL
v6cris= base %>% group_by(week) %>% count(cs_drgtr) %>% filter(cs_drgtr==1)
v6cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v7[i]=v7[i]+base$cs_furtv[j]
    }
  }
}

v7cris=NULL
v7cris= base %>% group_by(week) %>% count(cs_furtv) %>% filter(cs_furtv==1)
v7cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v8[i]=v8[i]+base$cs_vcrim[j]
    }
  }
}

v8cris=NULL
v8cris= base %>% group_by(week) %>% count(cs_vcrim) %>% filter(cs_vcrim==1)
v8cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v9[i]=v9[i]+base$cs_bulge[j]
    }
  }
}

v9cris=NULL
v9cris= base %>% group_by(week) %>% count(cs_bulge) %>% filter(cs_bulge==1)
v9cris

for (i in 1:52) {
  for (j in 1:d) {
    if(base$week[j]==i){
      v10[i]=v10[i]+base$cs_other[j]
    }
  }
}

v10cris=NULL
v10cris= base %>% group_by(week) %>% count(cs_other) %>% filter(cs_other==1)
v10cris

base1<-data.frame(semana,detn,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
colnames(base1)<-c("semana","num_det","cs_objcs","cs_descr","cs_casng","cs_lkout","cs_cloth","cs_drgtr","cs_furtv","cs_vcrim","cs_bulge","cs_other")
base1[,2:12]<-scale(base1[,2:12])#estandarizamos los datos

#modificación de cris

base1cris<-data.frame(semana,detncris$n,v1cris$n,v2cris$n,v3cris$n,v4cris$n,v5cris$n,v6cris$n,v7cris$n,v8cris$n,v9cris$n,v10cris$n)
colnames(base1cris)<-c("semana","num_det","cs_objcs","cs_descr","cs_casng","cs_lkout","cs_cloth","cs_drgtr","cs_furtv","cs_vcrim","cs_bulge","cs_other")
base1cris[,2:12]<-scale(base1cris[,2:12])#estandarizamos los datos

##### Se modifica todo lo que sigue con solo cambiar la base
base1=base1cris

fviz_nbclust(x = base1, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(base1, method = "euclidean"), nstart = 50)

clustermedia <- kmeans(base1[,2:12], centers=) #centers es el número de clusters
####no se si dividir en 6 ya que la variable Race toma 6 valores

fviz_cluster(clustermedia, base1[,2:12], geom = "point")
fviz_cluster(clustermedia, base1[,2:12], geom = "text")

d <- dist(base1, method = "euclidean")
# Clustering jerárquico usando enlace completo
cluster1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(cluster1, cex = 0.6, hang = -1,labels=base1$semana) #Hang sirve para indicar donde se colocan las etiquetas y labels para poner las etiquetas de las hojas


#####
##Método Agnes
cluster2 <- agnes(base1, method = "single")
pltree(cluster2, cex = 0.6, hang = -1, main = "Dendograma AGNES",labels=base1$semana)
rect.hclust(cluster2,k=6,border=2:20)
#####
#Método Diana
cluster3 <- diana(base1, metric = "average")
pltree(cluster3, cex = 0.6, hang = -1, main = "Dendograma DIANA",labels=base1$semana)
rect.hclust(cluster3,k=6,border=2:20)

######
#cortamos el arbol
clust<-cutree(cluster3,k=6)
fviz_cluster(list(data = base1[,2:12], cluster = clust,geom="text"))
####Visualizar el cluster recortado

##cluster basado en densidades
library(fpc)
library(dbscan)
cluster5 <- fpc::dbscan(base1[,2:12] eps = .15, MinPts = 5)
fviz_cluster(cluster5, base1[,2:12], stand = FALSE, ellipse=T)

plot.dbscan(cluster5, base1[,2:12], main = "DBSCAN", frame = FALSE)##No corre !!
