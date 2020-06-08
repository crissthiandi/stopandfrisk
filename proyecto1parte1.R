#Se está compartiendo la base de datos con información recopilada durante 2011 en la 
#ciudad de New York de un programa conocido 
#como "Stop and Risk" al igual que un único archivo pdf con el significado de las columnas.
#El proyecto consiste en entregar un documento pdf con la siguiente información después de 
#haber realizado una revisión: 

#1. Describir el programa stop and risk indicando su objetivo y la metodología para recolectar la información.
#2. Revisar la base de datos y hacer un análisis descriptivo de la información contenida en ella apoyándose de tablas y gráficas.
#3. A partir de lo realizado en el punto uno y dos, plantear estrategias para evaluar los objetivos del programa. 
#No se requiere hacer un análisis en esta parte sino solamente realizar las propuestas basado en lo realizado con anterioridad.

#Se considerará para la evaluación los siguientes aspectos:

#Redacción clara y ordenada
#faltas de ortografía
#Presentación de la información
#Contenido
#Referencias Bibliográficas

library(ggplot2)
library(dplyr)


setwd("c:/Users/Acer E15/Google Drive/semestre 8/Mineria de datos/Proyecto final/")
base <- read.csv("../Proyecto/StopandFrisk2011.csv")

#Análisis Descriptivo
str(base)
View(base)
summary(base)

a=as.data.frame(table(base$sex,useNA = "ifany"))

sexo<-ifelse(base$sex=="1","Hombre","Mujer")

# graficas ----------------------------------------------------------------


ggplot(base) + 
  geom_bar(mapping = aes(x = sexo, y = ..prop.., group = 1), fill="Pink", stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="Sexo",y="Porcentaje" )+
  ggtitle("Sexo de las personas detenidas")+
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text(size = 14))+
  coord_flip()



sum(base$sex, na.rm = T)/(685724-12110) #porcentaje de hombres detenidos

base$race=as.factor(base$race)
levels(base$race)= c("Negro","NegroHispano","BlancoHispano","Blanco","Asiatico","Nativo Am.")

ggplot(base) + 
  geom_bar(mapping = aes(x = race, y = ..prop.., group = 1),fill="red3", stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="Raza",y="Porcentaje")+ggtitle("Raza de las personas detenidas")+
  coord_flip()+theme(plot.title = element_text(size=16),
                     axis.title = element_text(size = 15),
                     axis.text = element_text(size = 13))

#Porcentajes según raza
nrow(subset(base,race=="Negro"))/(685724-22607)
nrow(subset(base,race=="NegroHispano"))/(685724-22607)
nrow(subset(base,race=="BlancoHispano"))/(685724-22607)
nrow(subset(base,race=="Blanco"))/(685724-22607)
nrow(subset(base,race=="Asiatico"))/(685724-22607)
nrow(subset(base,race=="Nativo Am."))/(685724-22607)

cachear<-ifelse(base$frisked=="1","Si","No")
ggplot(base) + 
  geom_bar(mapping = aes(x = cachear, y = ..prop.., group = 1),fill="pink", stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="¿Se cacheo al detenido?",y="Porcentaje")+ggtitle("Cacheo de las personas detenidas")

nrow(subset(base,frisked=="1"))/(685724-22607)
nrow(subset(base,frisked=="0"))/(685724-22607)


contrabando<-ifelse(base$contrabn=="1","Si","No")
ggplot(base) + 
  geom_bar(mapping = aes(x = contrabando, y = ..prop.., group = 1),fill="yellow", stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="¿Se encontró artículos de contrabando?",y="Porcentaje")+ggtitle("Contrabando de las personas detenidas")

nrow(subset(base,contrabn=="1"))/(length(!is.na(base$contrabn)))
nrow(subset(base,contrabn=="0"))/(length(!is.na(base$contrabn)))



pistola<-ifelse(base$pistol=="1","Si","No")
ggplot(base) + 
  geom_bar(mapping = aes(x = pistola, y = ..prop.., group = 1),fill="darkred", stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="¿Se le encontró una pístola a la persona detenida?",y="Porcentaje")+ggtitle("Presencia de pistola")

nrow(subset(base,pistol=="1"))/(length(!is.na(base$pistol)))
nrow(subset(base,pistol=="0"))/(length(!is.na(base$pistol)))



arresto<-ifelse(base$arstmade=="1","Si","No")
ggplot(base)+geom_bar(aes(x=arresto, y=..prop.., group=1), fill="orange",
                       stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="¿Se arrestó a la persona detenida?",y="Porcentaje")+ggtitle("Arresto")



# plots bivariadas --------------------------------------------------------

base2<-arrange(base,arstmade) 
base2$arstmade<- as.factor(base2$arstmade)
levels(base2$arstmade)<-c("No arresto", "Sí arresto")
base2=subset(base2,!is.na(race))

ggplot(base2, aes(fill=arstmade, y= frisked, x=race)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_viridis(discrete = T) +
  #scale_y_continuous(labels = scales::percent_format())+
  ggtitle("Personas cacheadas que fueron arrestadas por etnia") +
  xlab("Raza")+ylab("Cantidad de personas cacheadas")+
  labs(fill = "¿Hubo arresto?")+
  coord_flip()+theme(plot.title = element_text(size=16),
                     axis.title = element_text(size = 15),
                     axis.text = element_text(size = 13))

#comprobando lo anterior
nrow(subset(base,race=="Negro" & frisked=="1" ))
nrow(subset(base,race=="BlancoHispano" & frisked=="1" ))

nrow(subset(base,race=="Negro" & frisked=="1" & arstmade=="1" ))/nrow(subset(base,race=="Negro" & frisked=="1" ))
nrow(subset(base,race=="BlancoHispano" & frisked=="1" & arstmade=="1" ))/nrow(subset(base,race=="BlancoHispano" & frisked=="1" ))



#función cris
base2$forceuse=indicador(subset(x=base2,select = 34:42))

base2=subset(base2,select = c(arstmade,frisked,forceuse,race))
base2$forceuse=as.factor(base2$forceuse)
levels(base2$forceuse)=c("No hubo","Si hubo")

ggplot(base2, aes(fill=forceuse, y= frisked, x=race)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_viridis(discrete = T) +
  #scale_y_continuous(labels = scales::percent_format())+
  ggtitle("Personas cacheadas con uso de Fuerza por etnia") +
  xlab("Etnia")+ylab("Cantidad de personas cacheadas")+
  labs(fill = "¿Uso de fuerza?")+
  coord_flip()+theme(plot.title = element_text(size=16),
                     axis.title = element_text(size = 15),
                     axis.text = element_text(size = 13))

#comprobando lo anterior
nrow(subset(base,race=="Negro" & frisked=="1" ))
nrow(subset(base,race=="BlancoHispano" & frisked=="1" ))

nrow(subset(base2,race=="Negro" & frisked=="1" & forceuse=="Si hubo" ))/nrow(subset(base,race=="Negro" & frisked=="1"  ))
nrow(subset(base2,race=="BlancoHispano" & frisked=="1" & forceuse=="No hubo" ))/nrow(subset(base,race=="BlancoHispano" & frisked=="1" ))



#empezamos con el analisis
unique(base$year)
#correcto, todo corresponde a 2011
unique(base$pct)
#se checa que estos datos esten contenidos en el inervalo 1 y 123
min(unique(base$pct))
max(unique(base$pct))
#aqui no sirve unique, lo mejor seria graficar
#no se que pex con ser_num
a=unique(base$ser_num)
length(a)
#checar si es unico positivo y entero
str(base$ser_num) 
min(a)
max(a)
# se puede ver que es entero, positivo

# Se checa datestop
a=unique(base$datestop)
head(a)
min(a) 
#checar que sea de la forma 12312011
max(a) 
#checar que no sea menor a 01012011
length(a) #coincide con el numero de dias en el year, 365 si no es bisiesto
#por esto podemos notar que no hay errores de dedos en esta seccion
library('stringr')
length(base$datestop)
a=which(is.na(base$datestop))
a
a=base$datestop
a[1]
head(a)
fecha=NULL
Errorres=0
for (i in seq_along(base$datestop)) {
  tam=str_length(a[i])
  #realiza la convercion a fecha
  ifelse(as.numeric(str_sub(a[i],tam-5,tam-4))>31,(Errorres=Errorres+1),Errorres)
  ifelse(as.numeric(str_sub(a[i],1,tam-6))>12,(Errorres=Errorres+1),Errorres)
  ifelse(as.numeric(str_sub(a[i],tam-3,tam))!=2011,(Errorres=Errorres+1),Errorres)
  
  fecha[i]=paste(str_sub(a[i],tam-5,tam-4),
             str_sub(a[i],1,tam-6),
             str_sub(a[i],tam-3,tam),
             sep = '-')
  
}
Errorres #no hay ningun error
head(fecha)

base$datestop2=as.Date.character(fecha,origin='dd-mm-yyyy',format = '%d-%m-%Y')
head(base$datestop2)
str(base$datestop2)




#datestop
a=unique(base$timestop) #checar que no pase de 2359
str(base$timestop) 

min(a) #puede durar un stop 0 seg?

max(a) #no supera 2359
length(base$timestop[base$timestop<10]) 
#5829 stops duraron menos de 10 minutos, eso es raro



unique(base$borough) #supongo que se deberia de llamar 
#city y los na es cuando fue fuera de la ciudad
#o hay datos faltantes
names(base)[6]<-"city"
names(base)
unique(base$sex)
#1 hombre y 0 mujer, hay nas
a=which(is.na(base$sex))
length(a)
unique(base$race)

#efectivamente las 6 razas y nas


p<-unique(base$dob)
max(p) # se checa que no pase de 1231yyyy
min(p) #se checa con 101yyyy

library('stringr')
length(base$dob)
a=which(is.na(base$dob))
a
a=base$dob
a[1]
head(a)
fecha=NULL
Errorres=0
for (i in seq_along(base$dob)) {
  tam=str_length(a[i])
  #realiza la convercion a fecha
  ifelse(as.numeric(str_sub(a[i],tam-5,tam-4))>31,(Errorres=Errorres+1),Errorres)
  ifelse(as.numeric(str_sub(a[i],1,tam-6))>12,(Errorres=Errorres+1),Errorres)
  ifelse(as.numeric(str_sub(a[i],tam-3,tam))>1996 | as.numeric(str_sub(a[i],tam-3,tam))<1920 ,(Errorres=Errorres+1),Errorres)
  
  fecha[i]=paste(str_sub(a[i],tam-5,tam-4),
             str_sub(a[i],1,tam-6),
             str_sub(a[i],tam-3,tam),
             sep = '-')
}
Errorres #281,925 errores se tienen, diferente a datos enteros errados
#personas con mÃ¡s de 90 y menos de 15 
head(fecha)


base$dob2=as.Date.character(fecha,origin='dd-mm-yyyy',format = '%d-%m-%Y')
head(base$dob2)
str(base$dob2)
#se convierte a elementos fecha

#se checa los elementos fuera de valor
plot(unique(base$age))
lines(y=rep(90,times=300),seq_len(300),col='red')
lines(y=rep(14,times=300),seq_len(300),col='red')
#redundante con respecto a fecha de nacimiento ya que podemos calcular sus edades
#podemos ver que hay 

p=unique(base$age)
plot(sort(p))
p=base$age
length(p[(14>=p | p>=90)])
length(p[(14>=p)])
length(p[(p>=90)])

hist(p[(14>=p)])
hist(p[(p>=90)])



# plot edades -------------------------------------------------------------

p=sort(unique(base$age))
plotear=data.frame(equis=seq_along(p),ye=p)

ggplot(plotear)+geom_line(aes(x=equis,y=ye),size=2,lineend="round",alpha=0.8)+
  labs(x="Valores",y="Edad" )+
  ggtitle("Valores unicos de la Variable Edad")+
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))+
  geom_hline(yintercept = c(14,90),colour="red3")



#221 valores unicos dañados
#es decir solo 83 edades correctas

length(base$age[(base$age<100 & base$age< 15)])
#solo hay 12627 datos "consistentes"

#hay que checar que coincida en cuanto a la edad
#checar edad, heigth y weight con grafica
p=unique(base$height)
max(p) #aunque 95 pulgadas es mucho, no es mayor al recor mundial
#de hecho es algo comun en ciertas razas

min(p) #auque 36 es menos de 1 metro, una persona enana podria tener estas caracteristicas

plot(p)

# peso y altura plots -----------------------------------------------------

p=sort(p)
plotear=data.frame(equis=seq_along(p),ye=p)

ggplot(plotear)+geom_line(aes(x=equis,y=ye),size=2,lineend="round",alpha=0.8)+
  labs(x="Valores",y="Alturas" )+
  ggtitle("Valores unicos de la Variable Altura")+
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))+
  geom_hline(yintercept = c(40,80),colour="red3")
  
lines(y=rep(40,times=60),seq_len(60),col='red') #altura maxima promedio
lines(y=rep(80,times=60),seq_len(60),col='red') #altura minima promedio

p=unique(base$weight)
max(p) # el mayor al recor mundial son 600kg, o esta personas esta entre una de las mÃ s gordas del mundo
#apartir de 400 kg, podriamos pensar que es raro que alguien en esta condicion sea sospechoso,
#checaremos detalles de este dato

min(p) #esta persona no existe xd, el peso minimo es de una mujer en adultez es de 25 kg

length(base$weight[base$weight==999])
#por lo anterior podemos pensar que es un dato erroneo, 531 personas de ese peso es poco probable

length(base$weight[base$weight==1])
length(base$weight[base$weight==2])
length(base$weight[base$weight==3])

length(base$weight[base$weight<60]) #menor a 27 kg
#por lo anterior podemos pensar que es un dato erroneo, 1122 personas de ese peso es poco probable

plot(p)
lines(y=rep(196,times=400),seq_len(400),col='red') #peso maximo promedio
lines(y=rep(60,times=400),seq_len(400),col='red') #peso minimo promedio


unique(base$haircolr)

#hay nas y valores de todo menos 6 (gent de pelo blanc)
unique(base$eyecolor)
#hay nas
unique(base$build)
#hay nas
unique(base$othfeatr) #em esto no da mucha info
subset(base$othfeatr,base$othfeatr!="NA") #solo 19 elementos son diferente de NA
#por lo que, se puede llegar a sospechar que esta columna no da mucha informacion

unique(base$frisked)
unique(base$searched)
head(base$frisked) # se confirma que 1 es true y 0 es false
head(base$rf_furt)
#checar que si frisked es 0, no hubo razon alguna para ello
#supondremos que uno es verdadero y cero es falso
unique(c(base$frisked,base$searched,base$contrabn,base$pistol,base$riflshot,
         base$asltweap,base$knifcuti,base$machgun))
unique(base$othrweap)
unique(base$arstmade) 
#artsmade debe de ser verdadera para que artsoffen tenga valor quiza seria mejor
#tener un NA, cuando no sea verdadera
str(base$arstoffn) #podemos ver que este elemento tiene muchois niveles,
#buscamos en el vector y encontramos que
length(subset(base$arstoffn,base$arstoffn==''))
#644,844 datos son vacios, por lo que
1-(length(base$arstoffn)/length(subset(base$arstoffn,base$arstoffn=='')))^(-1)
#solo el 5.96% de la base tiene valores

#quiza checar incoherencias aqui
#was a summons issued?
#      sumoffen* offense suspect was summonsed
#estas vienen juntitas
unique(base$sumissue)

p=unique(base$perstop)
length(p)
p=unique(base$crimecode) # en vez de detailcm
length(p)
p=unique(base$perobs)
length(p)

unique(c(base$pf_hands, base$pf_wall,base$pf_grnd,base$pf_drwep,base$pf_ptwep,
         base$pf_baton,base$pf_hcuff,base$pf_pepsp,base$pf_other))

unique(c(base$cs_objcs,base$cs_descr,base$cs_casng,base$cs_lkout,base$cs_cloth,
         base$cs_drgtr,base$cs_furtv,base$cs_vcrim,base$cs_bulge,base$cs_other,
         base$rf_vcrim,base$rf_othsw,base$rf_attir,base$rf_vcact,base$rf_rfcmp,
         base$rf_verbl,base$rf_knowl,base$rf_furt,base$rf_bulg,base$sb_hdobj,
         base$sb_outln,base$sb_admis,base$sb_other))
#todo bien
unique(c(base$ac_proxm,base$ac_evasv,base$ac_assoc,base$ac_cgdir,base$ac_incid,
         base$ac_time,base$ac_stsnd,base$ac_other,base$ac_rept,base$ac_inves))
unique(base$forceuse) #falta columna de la razon de la fuerza

unique(base$inout) #bien
unique(base$trhsloc) #bien

#parece bien
unique(base$premtype)

#todo bien
unique(base$premname)
unique(base$addrnum)
unique(base$stname)
unique(base$stinter)
unique(base$crossst)
unique(base$addrpct)
unique(base$sector)
unique(base$beat)
unique(base$post)
unique(base$xcoord)
unique(base$ycoord)

#todo bien
unique(base$typeofid)
unique(base$othpers)
unique(base$explnstp)
unique(base$repcmd)
unique(base$revcmd)
unique(base$offunif)
unique(base$offverb)
unique(base$officrid)
unique(base$offshld)
unique(base$radio)
unique(base$recstat)
unique(base$linecm)

#propuesta de valores de age
head(base$datestop2)
head(base$dob2)
newage<-floor((base$datestop2-base$dob2)/365.25)
head(newage)
base$age2<-as.integer(newage)
head(base$age2)
str(base$age2)
max(base$age2) #errores por la edad en algun dato

plot(unique(base$age2)) #podemos ver daÃ±o en los datos de arriba y abajo, error de dedo al capturar
lines(y=rep(90,times=200),x=seq_len(200),col='red')
lines(y=rep(15,times=200),x=seq_len(200),col='red')



write.csv(base,file = 'salida.csv')

base<-na.omit(base) #eliminar filas que tengan alguna entrada vacía