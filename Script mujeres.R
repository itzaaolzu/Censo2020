#####CENSO 2020
####Ejercicio de muestras de mujeres


#Instalar paquete
install.packages("foreign")

##Librerias básicas
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr) #leer csv
library(tibble)
library(data.table) #Para hacer dataframes datatable, menos pesados
library(foreign) #Para transformar formato stata




#El tamaño de las bases requiere expandir memoria en R
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

#memory.size(max = TRUE)
#memory.size()
#memory.limit(size=56000)

#Directorio
setwd("~/Censo 2020/Cuestionario Ampliado")

#Abrir csv personas
Personas00 <- read_csv("Personas00.CSV")

#Funcion setDT para crear un data table (mas ligero)
Personas<-setDT(Personas00)
remove(Personas00)

#Revision de NA
names(Personas)
anyNA(Personas$CLAVIVP)
anyNA(Personas$SEXO)
anyNA(Personas$TAMLOC)
#No hay NAs

#Corte de base discapacidad
Personas<-Personas[,c(-(25:38))]
names(Personas)

#corte detalles laboral (prestaciones)
Personas<-Personas[,c(-(51:66))]

#Crear variable hogar persona
VIV_MEPER<-paste(Personas$ID_VIV,Personas$NUMPER, sep="")
Personas<-cbind(Personas, VIV_MEPER)
remove(VIV_MEPER)

#Checar NAs en variable creada
anyNA(Personas$VIV_MEPER)

######Base total SOLO mujeres####
Mujeres<- Personas %>%
  dplyr::filter(SEXO ==3)

####Bases para corroborar totales con Tabulados de PoblaciónHyM del 
##cuestionario ampliado


#Base h y m 10-14 años
Per_1014<- Personas %>%
  dplyr::filter(EDAD>=10 & EDAD<=14)

Per_1014 %>%
  dplyr::count(TAMLOC, SEXO, wt=FACTOR)

#Base h y m 10-14 años
Per_1519<- Personas %>%
  dplyr::filter(EDAD>=15 & EDAD<=19)

Per_1519 %>%
  dplyr::count(TAMLOC, SEXO, wt=FACTOR)

#Base h y m 20-24 años
Per_2024<- Personas %>%
  dplyr::filter(EDAD>=20 & EDAD<=24)

Per_2024 %>%
  dplyr::count(TAMLOC, SEXO, wt=FACTOR)


remove(Per_1014)
remove(Per_1519)
remove(Per_2024)

#####################################
#######Base h y m 12 a 20 años######
###################################

Per_1220<- Personas %>%
  dplyr::filter(EDAD>=12 & EDAD<=20)

#Quitamos Personas completa porque pesa mucho
remove(Personas)


#####BASE MIGRANTES###########################################################################

#Abrir csv migrantes
Migrantes00 <- read_csv("Migrantes00.CSV")

#Funcion setDT para crear un data table más ligero
Migrantes<-setDT(Migrantes00)
remove(Migrantes00)

#Revision
names(Migrantes)
head(Migrantes)
tail(Migrantes)
Migrantes[(120070:120099),]

#Migrantes solos
Migrantes %>%
  dplyr::count(MSEXO, wt=FACTOR)


#Crear variable hogar persona para union con personas
VIV_MEPER<-paste(Migrantes$ID_VIV,Migrantes$MPERLS, sep = "")

#Se une a la base
Migrantes<-cbind(Migrantes,VIV_MEPER)
remove(VIV_MEPER)

#Revisión
names(Migrantes)
names(Per_1220)
head(Personas)
tail(Personas)
dimnames(Personas)

#######Union de bases Per 12-20  y Migrantes

#Union muchos a muchos, toda la informacion se considera de migrantes y personas
UnionMig<- merge(Per_1220, Migrantes, by = "VIV_MEPER",
                 all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)


#vista de base nueva
head(UnionMig)
tail(UnionMig)


#Se tienen variables duplicasdas uno de la base Personas y otro de la base Migrantes.
#Necesito crear una variable que junte ambos 

#Extracción de variables repetidas
nombres<-names(UnionMig)
nom_dupX<-nombres[c(nombres %>%
                      str_detect(".x"))]
nom_dupY<-nombres[c(nombres %>%
                      str_detect(".y"))]


#Nueva variable Entidades
ENT_N<-c()
for(i in 1:nrow(UnionMig)){
  ENT_N[i]<-ifelse(!is.na(UnionMig$ENT.x[i]),UnionMig$ENT.x[i],
                      ifelse(is.na(UnionMig$ENT.x[i]),UnionMig$ENT.y[i], NA))
}

#Nueva variable Municipios
MUN_N<-c()
for(i in 1:nrow(UnionMig)){
  MUN_N[i]<-ifelse(!is.na(UnionMig$MUN.x[i]),UnionMig$MUN.x[i],
                   ifelse(is.na(UnionMig$MUN.x[i]),UnionMig$MUN.y[i], NA))
}


#Nueva variable tamaño localidad
LOC50K_N<-c()
for(i in 1:nrow(UnionMig)){
  LOC50K_N[i]<-ifelse(!is.na(UnionMig$LOC50K.x[i]),UnionMig$LOC50K.x[i],
                   ifelse(is.na(UnionMig$LOC50K.x[i]),UnionMig$LOC50K.y[i], NA))
}


#Nueva variable ID vivienda
ID_VIV_N<-c()
for(i in 1:nrow(UnionMig)){
  ID_VIV_N[i]<-ifelse(!is.na(UnionMig$ID_VIV.x[i]),UnionMig$ID_VIV.x[i],
                      ifelse(is.na(UnionMig$ID_VIV.x[i]),UnionMig$ID_VIV.y[i], NA))
}


#Nueva variable cobertura
COBERTURA_N<-c()
for(i in 1:nrow(UnionMig)){
  COBERTURA_N[i]<-ifelse(!is.na(UnionMig$COBERTURA.x[i]),UnionMig$COBERTURA.x[i],
                      ifelse(is.na(UnionMig$COBERTURA.x[i]),UnionMig$COBERTURA.y[i], NA))
}


#Nueva variable estrato
ESTRATO_N<-c()
for(i in 1:nrow(UnionMig)){
  ESTRATO_N[i]<-ifelse(!is.na(UnionMig$ESTRATO.x[i]),UnionMig$ESTRATO.x[i],
                         ifelse(is.na(UnionMig$ESTRATO.x[i]),UnionMig$ESTRATO.y[i], NA))
}


#Nueva variable upm
UPM_N<-c()
for(i in 1:nrow(UnionMig)){
  UPM_N[i]<-ifelse(!is.na(UnionMig$UPM.x[i]),UnionMig$UPM.x[i],
                       ifelse(is.na(UnionMig$UPM.x[i]),UnionMig$UPM.y[i], NA))
}


#Nueva variable Factor
FACTOR_N<-c()
for(i in 1:nrow(UnionMig)){
  FACTOR_N[i]<-ifelse(!is.na(UnionMig$FACTOR.x[i]),UnionMig$FACTOR.x[i],
                      ifelse(is.na(UnionMig$FACTOR.x[i]),UnionMig$FACTOR.y[i], NA))
}


#Nueva variable Clave vivienda
CLAVIVP_N<-c()
for(i in 1:nrow(UnionMig)){
  CLAVIVP_N[i]<-ifelse(!is.na(UnionMig$CLAVIVP.x[i]),UnionMig$CLAVIVP.x[i],
                      ifelse(is.na(UnionMig$CLAVIVP.x[i]),UnionMig$CLAVIVP.y[i], NA))
}

#Nueva variable Factor
TAMLOC_N<-c()
for(i in 1:nrow(UnionMig)){
  TAMLOC_N[i]<-ifelse(!is.na(UnionMig$TAMLOC.x[i]),UnionMig$TAMLOC.x[i],
                      ifelse(is.na(UnionMig$TAMLOC.x[i]),UnionMig$TAMLOC.y[i], NA))
}

#Variable union de sexo personas y sexo migrantes
SEXO_N<-c()
for(i in 1:nrow(UnionMig)){
  SEXO_N[i]<-ifelse(!is.na(UnionMig$SEXO[i]),UnionMig$SEXO[i],
                      ifelse(is.na(UnionMig$SEXO[i]),UnionMig$MSEXO[i], NA))
}

#Variable union de ID_Personas y ID_MIIgrante
ID_PER_N<-c()
for(i in 1:nrow(UnionMig)){
  ID_PER_N[i]<-ifelse(!is.na(UnionMig$ID_PERSONA[i]),UnionMig$ID_PERSONA[i],
                    ifelse(is.na(UnionMig$ID_PERSONA[i]),UnionMig$ID_MII[i], NA))
}

names(UnionMig)


#Arreglo de nuevas varibles
Nuevas_Var<-cbind(ID_PER_N,ENT_N,MUN_N,LOC50K_N,ID_VIV_N,COBERTURA_N,ESTRATO_N,UPM_N,FACTOR_N,
                  CLAVIVP_N,SEXO_N,TAMLOC_N)

#Transformación a datatable
Nuevas_Var<-as.data.table(Nuevas_Var)
class(Nuevas_Var$FACTOR_N)

#Convertir a númerico el Factor
Nuevas_Var$FACTOR_N<-as.numeric(as.character((Nuevas_Var$FACTOR_N)))

#Borrar para liberar espacio
remove(ID_PER_N,ENT_N,MUN_N,LOC50K_N,ID_VIV_N,COBERTURA_N,ESTRATO_N,UPM_N,FACTOR_N,
CLAVIVP_N,SEXO_N,TAMLOC_N,nom_dupX,nom_dupY)
remove(i)


#########Union todas las variables#####
UnionMig<-cbind(UnionMig,Nuevas_Var)
class(UnionMig$FACTOR_N)

###La union de la base Personas y la base Migrantes nos da el total de personas en
###los hogares + las que migraron en los ultimos 5 años 

###No se obtiene información sociodemografica de los emigrantes que no han retornado
###por lo que la base UnionMig unicamente sirve para ver a fondo las caracteristicas
###de los hogares totales y de los emigrantes, así como caracteristicas de su migración


###########################################################################
###########################################################################
##########Construimos las variables sobre la base de Per_1220##############


#######VARIABLE EDAD
edad<-Per_1220$EDAD
EDAD_G<-rep(0,length(edad))
EDAD_G[edad>=12 & edad<=14]<-1
EDAD_G[edad>=15 & edad<=17]<-2
EDAD_G[edad>=18 & edad<=19]<-3
EDAD_G[edad==20]<-4

#Revision
head(EDAD_G)
head(edad)

#Clasificaciones usadas:
#   1- 12 a 14
#   2- 15 a 17
#   3- 18 a 19
#   4- 20


#######VARIABLE UNIDAS
#Con pregunta situación conyugal

#Revision de la varible situacion conyugal
sitconyu<-Per_1220$SITUA_CONYUGAL
anyNA(Per_1220$SITUA_CONYUGAL)
class(sitconyu)

SITUA_CONYU_G<-rep(0,length(sitconyu))
#1-union libre, 5-casada civil, 6-casado religion, 7- casada religion y civil
SITUA_CONYU_G[sitconyu==1 | sitconyu==5 | sitconyu ==6 | sitconyu ==7]<-1
#2-separada, 3-divorciada, 4-viuda
SITUA_CONYU_G[sitconyu==2 | sitconyu==3 | sitconyu ==4]<-2
#8-soltera
SITUA_CONYU_G[sitconyu==8]<-3
#9-No especificado
SITUA_CONYU_G[sitconyu==9]<-9

#Revision de clasificación
head(SITUA_CONYU_G, n=30)
head(sitconyu, n=30)

#Clasificaciones usadas
#   1- unidas
#   2- exunidas
#   3- solteras
#   9- No especificado


####VARIABLES DE ESCOLARIDAD

#Cuadro para ver el % de pobalción de 12-20 años que asisten a la escuela
names(Per_1220)
tb1<-Per_1220 %>% #llamamos a la base
        dplyr::count(ASISTEN, SEXO) %>% #Contamos por cada variable enunciada
        dplyr::group_by(ASISTEN) %>% #agrupa
        dplyr::mutate(percent =n /sum(n)) %>% #podemos agregar columnas con mutate
        dplyr::select(-percent) %>% #podemos quitar columnas para ver de mejor forma el cuadro
        spread(SEXO,n, fill = 0) 

knitr::kable(tb1) #Presentación en formato tabla

#Cuadro que muestra nivel de escolaridad
tb2<-Per_1220 %>% #llamamos a la base
  dplyr::count(NIVACAD, SEXO) %>% #Contamos por cada variable enunciada
  dplyr::group_by(SEXO) %>% #agrupa segun renglon o columna
  dplyr::mutate(percent =n /sum(n)*100) %>% #podemos agregar columnas con mutate
  dplyr::select(-n) %>% #podemos quitar columnas para ver de mejor forma el cuadro
  spread(SEXO,percent, fill = 0) 


#####VARIABLE ESCOLARIDAD ACUMULADA
#Revision de la varible Escolaridad Acumulada
escoacu<-Per_1220$ESCOACUM
anyNA(Per_1220$ESCOACUM)
class(escoacu)

ESCOACUM_G<-rep(0,length(escoacu))

#0-Sin escolaridad o preescolar aprobado
ESCOACUM_G[escoacu==0]<-0
#1-Primaria incompleta
ESCOACUM_G[escoacu>=1 & escoacu<=5]<-1
#2-Primaria completa
ESCOACUM_G[escoacu==6]<-2
#3-Secundaria incompleta
ESCOACUM_G[escoacu==7 | escoacu==8]<-3
#4-Secundaria completa
ESCOACUM_G[escoacu==9]<-4
#5-Prepa y más
ESCOACUM_G[escoacu>=10 & escoacu<=98]<-5
#6-No especificado
ESCOACUM_G[escoacu==99]<-6

#Revision
head(ESCOACUM_G, n=30)
head(escoacu, n=30)



####Arreglo con variables agrupadas para hombres y mujeres
Vars_GHyM<-cbind(EDAD_G, SITUA_CONYU_G, ESCOACUM_G)

#Transformación a datatable
Vars_GHyM<-as.data.table(Vars_GHyM)

#Union de datos
Per_1220<-cbind(Per_1220,Vars_GHyM) 
names(Per_1220)



#Hasta aqui en la base PEr12-20 HyM se tienen las variables que se pueden calcular
#tambien para los hombres

##################################################33

###############################
######MUJERES-12A20############

##Se hace corte de mujeres 12 a 20 años
Muj_1220<- Per_1220 %>%
              dplyr::filter(SEXO==3)


##Maternidad y Paridad
##Con pregunta hijos nacidos vivas

#Revision de distribucion sin agrupar
Hijos_Nac_V1<-Muj_1220 %>%
                dplyr::count(HIJOS_NAC_VIVOS)

#Revision variable hijos_nc_vivos
hijos_ncv<-Muj_1220$HIJOS_NAC_VIVOS
anyNA(Muj_1220$HIJOS_NAC_VIVOS)
class(hijos_ncv)

####Variable Paridad
HIJOS_NCV_G<-rep(0,length(hijos_ncv))
#1-sin hijos
HIJOS_NCV_G[hijos_ncv==0]<-1
#2-un hijo
HIJOS_NCV_G[hijos_ncv==1]<-2
#3-mas de un hijo
HIJOS_NCV_G[hijos_ncv>1 & hijos_ncv<98]<-3
#98-No especificado por la omisión en todas las preguntas del tema
HIJOS_NCV_G[hijos_ncv==98]<-98
#99-No especificado
HIJOS_NCV_G[hijos_ncv==99]<-99

#Revision
head(Muj_1220$HIJOS_NAC_VIVOS)
head(HIJOS_NCV_G)

#####Variable Maternidad
MATERNIDAD_G<-rep(0,length(hijos_ncv))
#1-No mamás
MATERNIDAD_G[hijos_ncv==0]<-1
#2-Mamás
MATERNIDAD_G[hijos_ncv>0 & hijos_ncv<98]<-2
#98-No especificado por la omisión en todas las preguntas del tema
MATERNIDAD_G[hijos_ncv==98]<-98
#99-No especificado
MATERNIDAD_G[hijos_ncv==99]<-99

#Revision
head(Muj_1220$HIJOS_NAC_VIVOS)
head(MATERNIDAD_G)


#####FECUNDIDAD RECIENTE####

#Revision variable año de nacimiento
fechanani<-Muj_1220$FECHA_NAC_A
anyNA(Muj_1220$FECHA_NAC_A)
class(fechanani)
sum(is.na(fechanani))

#Revision variable mes de nacimiento
fechanmes<-Muj_1220$FECHA_NAC_M
anyNA(Muj_1220$FECHA_NAC_M)
class(fechanmes)
sum(is.na(fechanmes))

#Variable de mes y año de nacimiento total (TODO EN CHARACTER CON DIA PARA TRANSFORMAR A FECHA)
FECHAMA_G<-paste(fechanmes,"-", fechanani, sep="")
FECHAMA_G[FECHAMA_G=="NA-NA"]<-NA
class(FECHAMA_G)
anyNA(FECHAMA_G)
sum(is.na(FECHAMA_G))
head(FECHAMA_G)

#Variable fecha (formato FECHA R)
FECHAMA_G_R=as.Date(paste("01-",fechanmes,"-", fechanani, sep=""), format = "%d-%m-%Y")
head(FECHAMA_G_R, n=300)
class(FECHAMA_G_R)
sum(is.na(FECHAMA_G_R))

#Las que tuvieron fecundidad reciente ( desde marzo 2019 )
FEC_RECI_G<-rep(0, length(FECHAMA_G_R))
#1-Fecundidad un año antes del censo
FEC_RECI_G[FECHAMA_G_R >= "2019-03-01"]<-1
#2-Fecundidad hace más de año antes del censo
FEC_RECI_G[FECHAMA_G_R < "2019-02-28"]<-2
#
FEC_RECI_G[is.na(FECHAMA_G_R)]<-NA

sum(is.na(FEC_RECI_G))
head(FEC_RECI_G, n=500)



####Arreglo con variables agrupadas para hombres y mujeres
Vars_G_M<-cbind(MATERNIDAD_G, HIJOS_NCV_G, FECHAMA_G, FECHAMA_G_R, FEC_RECI_G)

#Transformación a datatable
Vars_G_M<-as.data.table(Vars_G_M)

#Union de datos
Muj_1220<-cbind(Muj_1220,Vars_G_M) 
names(Muj_1220)

#####TABLAS DE LAS VARIABLES

Muj_1220 %>%
  dplyr::count(FEC_RECI_G)

########GUARDAR BASES
write.dta(Muj_1220, "~/Censo 2020/Cuestionario Ampliado/Muj_1220.dta")
write.dta(Per_1220, "~/Censo 2020/Cuestionario Ampliado/Per_1220.dta")




#################################################################################################









###Variable edad
edad<-c(12,12,16,15,20,19)

v2<-rep(0,length(edad))

v2[edad>=12 & edad<=14]<-1
v2[edad>=15 & edad<=17]<-2
v2[edad>=18 & edad<=19]<-3
v2[edad=20]<-3

###Unidas
#Con pregunta situación conyugal

#Revision de la varible situacion conyugal








#Base h y m 12 a 20 años
names(Per_1220)
class(Per_1220$CLAVIVP)
Entidades<- Per_1220 %>%
              dplyr::count(SEXO) 
sum(Entidades$n)


u<-eval((parse(text=paste("UnionMig$",j,sep=""))))[1]
remove(Entidades)

ENT_N[(2601680:2601689),]



UnionMig %>%
  dplyr::count(FACTOR.x)


head(MUN_N,n=30)
remove(FACTOR_N)
remove(i)

UnionMig<-cbind(UnionMig,cond)
names(UnionMig)
nrow(UnionMig)
class(UnionMig$FACTOR_N)



###Variable edad
edad<-c(12,12,16,15,20,19)

v2<-rep(0,length(edad))

v2[edad>=12 & edad<=14]<-1
v2[edad>=15 & edad<=17]<-2
v2[edad>=18 & edad<=19]<-3
v2[edad=20]<-3


###Unidas
#Con pregunta situación conyugal


##Maternidad
##Con pregunta hijas nacidas vivas

#Paridad
#Con pregunta hijas nacidas vivas


#fecundidad reciente
#Las que lo tuvieron en el ultimo año











Union<- merge(Mujeres, Migrantes, by.x = "ID_PERSONA", by.y="ID_MII",
              all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)

UnionMig<- merge(Mujeres, Migrantes, by.x = "ID_PERSONA", by.y="",
                 all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)




UnionMig %>% 
  dplyr::count(MSEXO, wt=cond)

max(Migrantes$MPER)


UnionMig[1:50,]

#Base unicamente mujeres
Mujeres <- Personas %>%
              dplyr::filter(SEXO==3) 

names(Per_1220)

#Verificacion########
Edades_f<- Per_1220 %>%
              dplyr::count(SEXO, wt=FACTOR)

#Guardar csv
write.csv(Edades_f,"D:/Documents/Censo 2020/Cuestionario Ampliado/Edades_f.csv",row.names = TRUE)


#Base mujeres 12 a 20 años
Muj_1220 <- Mujeres %>%
              dplyr::filter(EDAD>=12 & EDAD<=20)

#Tabla de mujeres por edad con factor de expansion
Edades<- Mujeres %>%
            dplyr::filter(SEXO==3) %>%
            dplyr::count(EDAD, SEXO)

Edades_f<- Union %>%
              dplyr::filter(SEXO==3) %>%
              dplyr::count(EDAD, SEXO, wt=FACTOR)

#Total de mujeres sin factor de expansión 
sum(Edades_f$n)

#

#Quienes son mamás





Union<- data.table(Personas, key="ID_PERSONA")[
  data.table(Migrantes, key="ID_MII"),
  allow.cartesian=TRUE
]

remove(Union)

pob_mit_proyecciones %>% 
  dplyr::filter(cve_geo==0) %>% #filtro quedate con Republica Mexicana(0)
  dplyr::filter(ano==2020) %>%  #quedate con el 2020
  dplyr::count(edad, sexo, wt=poblacion) #contabiliza la variable población







#####################################################################################



for(i in UnionMig){
  x<-i
  
}
names(UnionMig)
head(UnionMig)
length(x)
remove(x)  

dat<-vector(mode = "list") 
for(i in 1:nrow(UnionMig)){
  for(j in nom_dupX){
    for(k in nom_dupY){
      dat[[j]]<-ifelse(!is.na(eval((parse(text=paste("UnionMig$",j,sep=""))))[i]),
                       eval((parse(text=paste("UnionMig$",j,sep=""))))[i],
                       ifelse(is.na(eval((parse(text=paste("UnionMig$",j,sep=""))))[i]),
                              eval((parse(text=paste("UnionMig$",k,sep=""))))[i], NA))
      
      
    }
  }
}

######################################################################################