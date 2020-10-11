library(dplyr)
library(tidyr) 
library(readxl)
library("cluster")
library("NbClust")
library("factoextra")
library("data.table")
library("stats")
library("dendextend")
library("tidyverse")
library(ggplot2)
library(tidyr)
library(dbscan)
library(fpc)
library(rgdal)
library(RColorBrewer)
library(classInt)
#Acidente de transito

#importacion de los historicos de los años 2017, 2018, 2019
datos2017<-read.csv2("Datos/BBD_EDG_2017_csv.csv")
datospreuva2018<-read.csv2("Datos/prueva2018.csv")
datos2019<-read.csv2("Datos/BDD_EDG_2019.csv")
Codigo2018<-read.csv2("Datos/Parroquias2017_2019.csv")

#Columnas elejidas necesarias 
coluSI<-c("prov_fall","cant_fall","lug_viol","area_fall","nac_fall","sexo","edad","mor_viol","parr_fall","anio_fall")

#Eliguiendo las columnas ya elegisdas
datos2019<-datos2019[,names(datos2019)%in%coluSI]
datos2017<-datos2017[,names(datos2017)%in%coluSI]
datospreuva2018<-datospreuva2018[,names(datospreuva2018)%in%coluSI]

#Filtrar las muertes que han sido por accidentes de transito
datos2019 <- filter(datos2019, mor_viol==1)
datos2017 <- filter(datos2017, mor_viol==1)
datospreuva2018 <- filter(datospreuva2018, mor_viol==" Accidente de transporte terrestre")

#Se procede a normalizar los datos del año 2018

#Funcion para colocar  el respectivo codigo a las parroquias
codi2018<-function(){
  ParroquiasF<-NULL
  nn=1
  conti=0
  contj=0
  bandera=FALSE
  for (i in 1:nrow(datospreuva2018)) {
    for (j in 1:nrow(Codigo2018)) {
      if(datospreuva2018[i,5]==Codigo2018[j,1]&& datospreuva2018[i,6]==Codigo2018[j,2] && datospreuva2018[i,7]==Codigo2018[j,3]){
        ParroquiasF<-c(ParroquiasF,Codigo2018[j,4])
        contj=contj+1
        bandera=TRUE
        break
      }
    }
    if(bandera==TRUE){
      bandera=FALSE
    }else{
      ParroquiasF<-c(ParroquiasF,nn)
    }
  }
  datospreuva2018<<-data.frame(ParroquiasF,datospreuva2018)
  
}
codi2018()

#Proceso para eliminar las muertes que tenga como Exterior 
for (i in 1:nrow(datospreuva2018)) {
  if(datospreuva2018[i,1]==0){
    datospreuva2018<-datospreuva2018[-i,]
    break
  }
}

rm(i)
#para remplazar los datos de parroquia por los de los codigos
datospreuva2018$parr_fall<-datospreuva2018$ParroquiasF
#eliminar la columna ParroquiasF
datospreuva2018<-datospreuva2018[, !(names(datospreuva2018) %in% "ParroquiasF")]
#Normalizar la columna area del accidente 1 para Urbano y 2 rural
for (i in 1:nrow(datospreuva2018)) {
  if((datospreuva2018[i,7]%%10^2)>50){
    datospreuva2018[i,8]=2
  }else if((datospreuva2018[i,7]%%10^2)<=50){
    datospreuva2018[i,8]=1
  }
}
rm(i)
#Normalizar la columna Sexo donde 1 es Hombre y 2 Mujer
for (i in 1:nrow(datospreuva2018)){
  if(datospreuva2018[i,2]=="Hombre"){
    datospreuva2018[i,2]=1
  }else{
    datospreuva2018[i,2]=2
  }
}
rm(i)
datospreuva2018[,2]<-as.numeric(datospreuva2018[,2])

#Unir los 3 Dataset de accidentes de transito
Tabla<-rbind(datos2017,datospreuva2018,datos2019)
#sacar la frecuencia
PARROQUIA= Tabla %>%
  group_by(parr_fall) %>%
  summarise(frequency = n())
#Suplantar edades desconocids por el promedio del dataset
Tabla$edad[Tabla$edad==999]<-0
Tabla$edad[Tabla$edad==0]<-mean(Tabla$edad)
#Funcio para calcular el promedio de edad de muerte en accidente de trnasito en cada parroquia


#odenar los codigos de las parroquias
Tabla<-Tabla[order(Tabla$parr_fall,decreasing = FALSE),]
#Sacar el promedio de una parroquia
promedioEdad2<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7]){
        sum=sum+Tabla[i,4]
        aux=aux+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7]){
        sum=sum+Tabla[j,4]
        aux=aux+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7]){
        sum=sum+Tabla[l,4]
        aux=aux+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7]){
        sum=sum+Tabla[k,4]
        aux=aux+1
      }
    }
  }
  
  promedio=sum/aux
  return(round(promedio,2))
}

#Funcion para colorar las edades en las una el dataset General Nota Dura media hora en dar respuesta
columnaEdad<-function(){
  PromEdad=NULL
  for (i in 1:nrow(PARROQUIA)) {
    PromEdad<-c(PromEdad,promedioEdad2(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  Acidentes2017_2019<<-data.frame(PARROQUIA,PromEdad)
}
columnaEdad()
Acidentes2017_2019[,3]<-round(Acidentes2017_2019[,3])

#Sacar el porcentaje de los hombre muertos en una parroquia

promedioH<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7] && Tabla[i,2]==1){
        sum=sum+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7] && Tabla[j,2]==1){
        sum=sum+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7] && Tabla[l,2]==1){
        sum=sum+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7] && Tabla[k,2]==1){
        sum=sum+1
      }
    }
  }
  
  promedio=sum/y
  return(round(promedio,2))
}

#Porcentaje de mujeres que han muerto en accidente de trnansito
promedioM<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7] && Tabla[i,2]==2){
        sum=sum+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7] && Tabla[j,2]==2){
        sum=sum+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7] && Tabla[l,2]==2){
        sum=sum+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7] && Tabla[k,2]==2){
        sum=sum+1
      }
    }
  }
  
  promedio=sum/y
  return(round(promedio,2))
}
#Funcion para colocar las columnas de hombre y mujer
columnaSexos<-function(){ #Dura en ejecutarse alrededor de 1 munito con 15 segundos
  PromHombre=NULL
  PromMujer=NULL
  for (i in 1:nrow(PARROQUIA)) {
    PromHombre<-c(PromHombre,promedioH(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  for (i in 1:nrow(PARROQUIA)) {
    PromMujer<-c(PromMujer,promedioM(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  
  Acidentes2017_2019<<-data.frame(Acidentes2017_2019,PromHombre,PromMujer)
}

columnaSexos()

#Funcionn para sacar el porcentaje de muertes en el 2017
porce2017<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7] && Tabla[i,3]==2017){
        sum=sum+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7] && Tabla[j,3]==2017){
        sum=sum+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7] && Tabla[l,3]==2017){
        sum=sum+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7] && Tabla[k,3]==2017){
        sum=sum+1
      }
    }
  }
  
  porcentaje=sum/y
  return(round(porcentaje,2))
}
#Funcion para sacar el porcentje de los muertos en el año 2018
porce2018<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7] && Tabla[i,3]==2018){
        sum=sum+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7] && Tabla[j,3]==2018){
        sum=sum+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7] && Tabla[l,3]==2018){
        sum=sum+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7] && Tabla[k,3]==2018){
        sum=sum+1
      }
    }
  }
  
  porcentaje=sum/y
  return(round(porcentaje,2))
}
#Funcion para sacar el porcentaje de los muertos en el año 2019
porce2019<-function(x,y){
  aux=0
  sum=0
  refe=round(nrow(Tabla)/4)
  x1=refe
  x2=refe*2
  x3=refe*3
  pibote1<-Tabla[x1,7]
  pibote2<-Tabla[x2,7]
  pibote3<-Tabla[x3,7]
  if(pibote1>x){
    for (i in 1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[i,7] && Tabla[i,3]==2019){
        sum=sum+1
      }
    }
  }else if(pibote1<=x  && x <pibote2){
    for (j in x1:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[j,7] && Tabla[j,3]==2019){
        sum=sum+1
      }
    }
  }else if(pibote2<=x && x<pibote3){
    for (l in x2:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[l,7] && Tabla[l,3]==2019){
        sum=sum+1
      }
    }
  }else if(x>=pibote3){
    for (k in x3:nrow(Tabla)) {
      if(aux==y){
        break
      }else if(x==Tabla[k,7] && Tabla[k,3]==2019){
        sum=sum+1
      }
    }
  }
  
  porcentaje=sum/y
  return(round(porcentaje,2))
}

#Columnas de porcentaje de los 3 años
columnaAnios<-function(){ #Dura en ejecutarse alrededor de 2 minutos
  Porcent2017=NULL
  Porcent2018=NULL
  Porcent2019=NULL
  for (i in 1:nrow(PARROQUIA)) {
    Porcent2017<-c(Porcent2017,porce2017(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  for (i in 1:nrow(PARROQUIA)) {
    Porcent2018<-c(Porcent2018,porce2018(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  for (i in 1:nrow(PARROQUIA)) {
    Porcent2019<-c(Porcent2019,porce2019(PARROQUIA[i,1],PARROQUIA[i,2]))
  }
  
  Acidentes2017_2019<<-data.frame(Acidentes2017_2019,Porcent2017,Porcent2018,Porcent2019)
}
columnaAnios()

#Renombrar Atributos
Acidentes2017_2019<-rename(Acidentes2017_2019,Lugar_del_Accidente=parr_fall,N_de_Muertos=frequency,PorcentH=PromHombre,PorcenM=PromMujer)

#funcion para colorlar las provincias cantones y parroquias con la referenciia del codigo
PCP<-function(){
  Provincia=NULL
  Canton=NULL
  Parroquia=NULL
  for (i in 1:nrow(Acidentes2017_2019)) {
    for (j in 1:nrow(Codigo2018)) {
      if(Acidentes2017_2019[i,1]==Codigo2018[j,4]){
        Provincia<-c(Provincia,Codigo2018[j,1])
        Canton<-c(Canton,Codigo2018[j,2])
        Parroquia<-c(Parroquia,Codigo2018[j,3])
        break
      }
    }
  }
  Acidentes2017_2019<<-data.frame(Acidentes2017_2019,Provincia,Canton,Parroquia)
}
PCP()

#Unir las columnas provincia, canton, parroquia y remplazarla por lugar del accidente
Acidentes2017_2019<-unite(Acidentes2017_2019,"Lugar del Accidente",c(9:11),sep = " - ",remove = TRUE)

Acidentes2017_2019$Lugar_del_Accidente<-Acidentes2017_2019$`Lugar del Accidente`

Acidentes2017_2019<-Acidentes2017_2019[, !(names(Acidentes2017_2019) %in% "Lugar del Accidente")]

#---------------------------------Exportacion de los datos--------------------
write.csv2(Acidentes2017_2019, file = "Acidentes de transito 2017-2019.csv",row.names = FALSE)

#--------------------------------Importacion de los datos---------------------
Experimentos<-read.csv2("Datos/Acidentes de transito 2017-2019.csv")
#-----------------------------agrupamiento DBscam ---------------
Acii<-Experimentos[,-9]
Aciii<-Acii[,-1]
kNNdistplot(Aciii, k=3)
abline(h=6, lty=2)

set.seed(123)
f<-fpc::dbscan(Aciii, eps = 6,MinPts = 3)
f
Acidente_DB<- dbscan::dbscan(Aciii,6,3)
Acidente_DB
fviz_cluster(f,Aciii,geom = "point")

i<-order(Acidente_DB$cluster)
matriz_particion<-as_tibble(cbind(objeto=Acii$Lugar_del_Accidente[i],
                                  grupo=Acidente_DB$cluster[i]))
print(matriz_particion)
#-----------------grafico de barras------------------
barplot(height = tabla$Numeros_de_Muertes, 
        names=tabla$años, 
        col=c('red','green','blue'),
        xlab="Años de las muertes",
        ylab="N° de Muertes", 
        main="Muertes en el Ecuador 2017-2019")

tablita<-table(matriz_particion[,2])
tablita<-data_frame(palabra=names(tablita), recuento=as.numeric(tablita))
tablita
barplot(height = tablita$recuento, 
        names=tablita$palabra, 
        col=c('green','red','blue'),
        xlab="Grupos",
        ylab="Cantidad de Parroquias", 
        main="Clustering")

#----------------------Metricas de validacion------------------------------



fviz_nbclust(x = Aciii, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")


km_indices<-cluster.stats(d=dist(Aciii,method = "euclidean"),
                          clustering = Acidente_DB$cluster)

km_indices<-cluster.stats(d=get_dist(Aciii,method = "pearson"),
                          clustering = Acidente_DB$cluster)

#Medidas de Homogeniedad
km_indices$average.within
#medida de separacion
km_indices$average.between
#indice Dunn
km_indices$dunn
#----------------------------------------------------------------------------


#------------------Experimento 1----------------------------
#Aumentar el tamaño del radio y comparar con las mestricas de calidad si mejora o no 

set.seed(123)
f1<-fpc::dbscan(Aciii, eps = 7,MinPts = 3)
f1
Acidente_DB1<- dbscan::dbscan(Aciii,7,3)
Acidente_DB1
fviz_cluster(f1,Aciii,geom = "point")


km_indices1<-cluster.stats(d=get_dist(Aciii,method = "pearson"),
                          clustering = Acidente_DB1$cluster)

#Medidas de Homogeniedad
km_indices1$average.within
#medida de separacion
km_indices1$average.between
#indice Dunn
km_indices$dunn


#------------------Experimento 2----------------------------
#disminuiremos el tamaño del radio y comparar con las mestricas de calidad si mejora o no 

set.seed(123)
f2<-fpc::dbscan(Aciii, eps = 5.83,MinPts = 3)
f2
Acidente_DB2<- dbscan::dbscan(Aciii,5.83,3)
Acidente_DB2
fviz_cluster(f2,Aciii,geom = "point")


km_indices2<-cluster.stats(d=get_dist(Aciii,method = "pearson"),
                           clustering = Acidente_DB2$cluster)

#Medidas de Homogeniedad
km_indices2$average.within
#medida de separacion
km_indices2$average.between
#indice Dunn
km_indices$dunn


#-------------------Nuevo closificador---------------
clasificador3001<-function(){
  Etiqueta=NULL
  for (i in 1:nrow(Acii)) {
    for (j in 1:nrow(matriz_particion)) {
      if(Acii[i,1]==matriz_particion[j,1]){
        if(matriz_particion[j,2]==0){
          Etiqueta<-c(Etiqueta,"Operativo de Transito Constante")
          break
        }else if(matriz_particion[j,2]==1){
          Etiqueta<-c(Etiqueta,"Operativos Regulares frecuentes")
          break
        }else if(matriz_particion[j,2]==2){
          Etiqueta<-c(Etiqueta,"Operativos de Transito SemiConstantes")
          break
        }
      }
    }
  }
  Acii<<-data.frame(Acii,Etiqueta)
}
clasificador3001()



#---------------------Mapa del Ecuador---------------------

dirmapas<-"Datos/SHP"
setwd(dirmapas)
poligonos<-readOGR("nxparroquias.shp",layer = "nxparroquias")
dir.resul<-"~/Tutorual/Proyecto_FFF/Datos/Resultados"
setwd(dir.resul)
plot(poligonos)
poligonos=poligonos[poligonos$DPA_PROVIN !=90,]#eliminar las areas no dellimiatadas
plot(poligonos)
poligonos@data[,1]<-as.numeric(poligonos@data[,1])
N_FxP<-function(){
  N_F=NULL
  bandera=FALSE
  for (i in 1:nrow(poligonos)) {
    for (j in 1:nrow(PARROQUIA)) {
      bandera=FALSE
      if(poligonos@data[i,1]==PARROQUIA[j,1]){
        N_F<-c(N_F,PARROQUIA[j,2])
        bandera=TRUE
        break
      }
    }
    if(bandera==FALSE){
      N_F<-c(N_F,0)
    }
  }
  N_F<-matrix(data = N_F,nrow = nrow(poligonos),ncol = 1)
  N_F<-as.data.frame(N_F)
  row.names(N_F)<-row.names(poligonos)
  N_F<<-N_F
  #poligonos.data<<-SpatialPolygonsDataFrame(poligonos,N_F)
}
N_FxP()
poligonos@data[,9]<-as.numeric(poligonos@data$Numero_F)
#poligonos.data<-SpatialPolygonsDataFrame(poligonos,N_F)
#poligonos.data<-SpatialPolygonsDataFrame(poligonos@data,poligonos.data@data)
poligonos@data<-cbind(poligonos@data,N_F)
plovar<-poligonos$Numero_F
nclr<-3 #Numero de colores
plotclr<-brewer.pal(nclr,"Blues")
class <- classIntervals(plovar,nclr,style="quantile") # Aqui fijo el numero de decimales
colcode<-findColours(class,plotclr)
jpeg("PropIndMap.jpeg",quality=100,height = 600,width = 800)
plot(poligonos,col=colcode,borders="grey",axe=TRUE)
title(main = "Proporcion de Accidentes de Transito 2017-2019 Ecuador",cex=3)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=1.25)
dev.off()

#---------------------------Exportar la dataset etiquetado--------------------
write.csv2(Acii,file = "Resultado.csv",row.names = FALSE)
