library(cluster) #Hacer analisis de clusters
library(factoextra) #Visualizar el cluster
#####
#Cluster Particionamiento
###Cuando cargamos la base de datos desde .csv podemos etiquetar los registros con los nombres
### y evitamos tener que poner etiqueta
base<-read.csv("StopandFrisk2011.csv")
base=baseoriginal
#base<-na.omit(base)
#View(base)
filas<-nrow(base)
muestra<-sample.int(n=filas,size=filas*.1,replace = F)
train<-base[muestra,]

base2<-train[,c(8,66,67,68,69,70,71,72,73,96,97)]
base2<-na.omit(base2)
base2[,2:11]<-scale(base2[,2:11])

datos=base2[,2:11]

fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(datos, method = "binary"), nstart = 50)


clustermedia <- kmeans(base2[,2:11], centers=6) #centers es el número de clusters

fviz_cluster(clustermedia, base2[,2:11], geom = "point")
fviz_cluster(clustermedia, base2[,2:11], geom = "text")


d <- dist(base2[,2:11], method = "binary")
# Clustering jerárquico usando enlace completo
cluster1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(cluster1, cex = 0.6, hang = -1,labels=base2$race) #Hang sirve para indicar donde se colocan las etiquetas y labels para poner las etiquetas de las hojas


#####
##Método Agnes
cluster2 <- agnes(base2, method = "single")
pltree(cluster2, cex = 0.6, hang = -1, main = "Dendograma AGNES",labels=datos$Name)
rect.hclust(cluster2,k=6,border=2:20)
#####
#Método Diana
cluster3 <- diana(base2, metric = "average")
pltree(cluster3, cex = 0.6, hang = -1, main = "Dendograma DIANA",labels=datos$Name)
rect.hclust(cluster3,k=6,border=2:20)