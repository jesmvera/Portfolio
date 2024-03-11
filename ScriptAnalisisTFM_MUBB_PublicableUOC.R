########### Librerias y directorios ####################

wd.analysis <- "D:/jsmv/OneDrive/Documents/COURSES/MASTER BIOESTADISTICA Y BIOINFORMATICA/11. TESIS/analysis"

# bds directory
wd.bds <- "D:/jsmv/OneDrive/Documents/COURSES/MASTER BIOESTADISTICA Y BIOINFORMATICA/11. TESIS/BDs"

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggraph)
library(ggpubr)
library(tidync)
library(ggpubr)
library(GGally)
library(ncdf4) # paquete para manipulacion de archivos netcdf
library(raster) # paquete para manipulacion de objetos raster
library(rgdal) # paquete para analisis geoespacial
library(geosphere) # paquete para distancias geoespacial
library(lubridate) # paquete para manipulacion de fechas y horas
library(stringr) # paquete para manipulacion de caracteres
library(dismo) # paquete para analisis geoespacial
library(rgeos) # paquete para analisis geoespacial
library(spdep) # paquete para analisis geoespacial
library(spatialreg) # paquete para analisis geoespacial
library(gstat) # paquete para analisis geoespacial
library(spacetime) # paquete para analisis espaciotemporal
library(animation) # paquete para visualizacion
library(maps) # paquete para visualizacion
library(mapproj) # paquete para visualizacion
library(FactoMineR) # paquete para analisis multivariante
library(factoextra) # paquete para analisis multivariante
library(univOutl) # paquete para deteccion de valores atipicos
library(forecast) # paquete para pronosticos
library(meteo)
library(caret)
library(STRbook)
library(CCA)
library(Hmisc)
library(RColorBrewer)
library(colorspace)
library(gplots)
library(pheatmap)
library(plyr)
library(devtools)
library(tensorflow)
library(keras)
library(Rcpp)
library(reticulate)
library(processx)
library(kerasR)
library(BAMMtools) # Para usar la clasificacion de cortes naturales segun Jenks
library(clipr)
library(moments)
library(pROC)

semilla <- 12345
setwd(wd.analysis) # setting work directory

############### Importacion de datos de las cuatro fuentes ###################

######## Datos de pesca ###########

# auxiliar para colocar los niveles de año y mes correctamente como factores
yymes.factores <- expand.grid(seq(2017,2022), seq(1,12)) %>% 
  arrange(Var1, Var2) %>% 
  mutate(Var3 = paste(Var1, Var2, sep = "-")) %>% 
  dplyr::select(Var3) %>% 
  pull

## Importar Data de registros pesqueros
dfzpesca <- read.csv(paste(wd.bds,"base_pesca.csv", sep = "/"), header = TRUE)
dim(dfzpesca)
str(dfzpesca)

## Importar codigo de areas (division segun la entidad pesquera)
cod.area <- read.csv(paste(wd.bds,"codigo_areas.csv", sep = "/"), header = TRUE)
dim(cod.area)
str(cod.area)

######### Importar los archivos .nc de clorofila ##########

our_nc_data1 <- nc_open(paste(wd.bds,"BDchlor/A20170012017031.L3m_MO_CHL_chlor_a_9km.nc", 
                              sep = "/"))
print(our_nc_data1)
nc_close(our_nc_data1)

# funcion para obtener las variables de interes de cada archivo (mes)
get.datosChl <- function(archivo){
  # abrir la conexion
  our_nc_data1 <- nc_open(archivo)
  # print(our_nc_data1)
  # attributes(our_nc_data1$var) # ver atributos de las variables
  # attributes(our_nc_data1$dim) # ver atributos de las dimensiones: espacial y temporal
  
  # Obtener y guardar las coordenadas de longitud y latitud
  lat <- ncvar_get(our_nc_data1, "lat")
  lon <- ncvar_get(our_nc_data1, "lon")
  # Obtener la dismension del tiempo
  time <- as.POSIXct(ncatt_get(our_nc_data1, 0, "time_coverage_start")$value, tz="GMT")
  
  # obtener la variable de medicion de clorofila en porciones de matrices
  # mg m^-3 - unidad de medida de la clorofila mg por metro cúbico
  chlor_array <- ncvar_get(our_nc_data1, "chlor_a") # Variable de la concentracion de clorofila
  fillvalue <- ncatt_get(our_nc_data1, "chlor_a", "_FillValue") # valores en blanco
  
  nc_close(our_nc_data1) # cerrar la conexion
  
  # crear una variable del tiempo en formato año y mes del archivo importado
  mes <- paste(year(time), month(time), sep = "-")
  
  # dim(chlor_array) # verificar las dimensiones
  
  # reeemplazar los valores en blanco con NA
  chlor_array[chlor_array==fillvalue$value] <- NA
  chlor_array
  
  # Crear una matriz con la dimension espacial
  lonlat <- as.matrix(expand.grid(lon,lat))
  # guardarlo como objeto vector
  chlor_vec_long <- as.vector(chlor_array)
  # length(chlor_vec_long)
  
  # crear objeto data.frame
  chlor_obs <- data.frame(cbind(lonlat, chlor_vec_long))
  # head(chlor_obs)
  # renombrando las etiquetas de las columnas del data.frame
  colnames(chlor_obs) <- c("Long", "Lat", "Monthly.Chlor")
  
  # filtrando la zona de estudio de la institucion pesquera y eliminando los NA
  chlor.ob <- chlor_obs %>% 
    dplyr::filter(Long >= -82 & Long <= -79 & Lat >= -4 & Lat <= 1) %>% 
    na.omit
  
  # adicionando el mes que se esta importando
  chlor.ob$yymm.clor <- rep(mes, times = nrow(chlor.ob))
  
  return(chlor.ob)
}

# Declarar data frame
df <- NULL

# Abrir todos los archivos importados
files <-  list.files(paste(wd.bds,"BDchlor", sep = "/"),
                     pattern='*.nc', full.names=TRUE)

# Recorriendo todos los archivos
for(i in seq_along(files)) {
  lw <- get.datosChl(files[i])
  # Uniendo la informacion de todos los archivos en un solo data.frame
  rbind(df, lw) -> df
}

# Guardando la informacion en un unico archivo
# write.csv(df, paste(wd.bds, "data_clor_mes_con_coord.csv", sep = "/"), row.names = FALSE)

######### Importar archivos .nc de la temperatura del agua ##########

# funcion para obtener las variables de interes en un unico archivo
get.datosSST <- function(archivo){
  # abrir conexion
  our_nc_data1 <- nc_open(archivo)
  # print(our_nc_data1)
  # attributes(our_nc_data1$var) # ver variables en el archivo
  # attributes(our_nc_data1$dim) # ver dimensiones del archivo: espacial, temporal
  
  # Obtener la longitud y latitud y almacenarlos en un objeto
  lat <- ncvar_get(our_nc_data1, "lat")
  lon <- ncvar_get(our_nc_data1, "lon")
  # obtener la dimension del tiempo
  time <- as.POSIXct(ncatt_get(our_nc_data1, 0, "time_coverage_start")$value, tz="GMT")
  
  # obtener la variable de medicion de temperatura del agua en porciones de matrices
  # grados celsius - unidad de medida de la temperatura superficial del agua
  sst_array <- ncvar_get(our_nc_data1, "sst") # variable de la temperatura del agua
  qualsst_array <- ncvar_get(our_nc_data1, "qual_sst") # niveles de calidad
  fillvalue <- ncatt_get(our_nc_data1, "sst", "_FillValue") # valores en blanco
  
  nc_close(our_nc_data1) # cerrar conexion
  
  # crear una variable del tiempo en formato año y mes del archivo importado
  mes <- paste(year(time), month(time), sep = "-")
  
  # dim(sst_array) # verificar las dimensiones del archivo
  # reemplazar valores en blanco con NA
  sst_array[sst_array==fillvalue$value] <- NA
  sst_array
  
  # Crear una matriz con la dimension espacial 
  lonlat <- as.matrix(expand.grid(lon,lat))
  # guardarlo como objeto vector
  sst_vec_long <- as.vector(sst_array)
  # length(sst_vec_long)
  
  # crear un data.frame
  sst_obs <- data.frame(cbind(lonlat, sst_vec_long))
  # head(sst_obs)
  # renombrando las etiquetas de las columnas del data.frame
  colnames(sst_obs) <- c("Long", "Lat", "Monthly.SST")
  
  # filtrando la zona de estudio de la institucion pesquera y eliminando los NA
  sst.ob <- sst_obs %>% 
    dplyr::filter(Long >= -82 & Long <= -79 & Lat >= -4 & Lat <= 1) %>% 
    na.omit
  
  # adicionando el mes que se esta importando
  sst.ob$yymm.sst <- rep(mes, times = nrow(sst.ob))
  
  return(sst.ob)
}

# Declarar data frame
df <- NULL

# abrir todos los archivos importados de temperatura del agua
files2 <-  list.files(paste(wd.bds,"BDSST", sep = "/"),
                      pattern='*.nc', full.names=TRUE)

# Recorriendo cada archivo
for(i in seq_along(files2)) {
  lw <- get.datosSST(files2[i])
  # Uniendo la informacion de todos los archivos en un solo data.frame
  rbind(df, lw) -> df
}

# Guardando la informacion en un unico archivo
# write.csv(df, paste(wd.bds, "data_SST_mes_con_coord.csv", sep = "/"), row.names = FALSE)

######### Importar los archivos .csv de magnitud del viento ##########

# Declarar data.frame
df <- NULL

# abrir todos los archivos
files3 <-  list.files(paste(wd.bds,"BDWindSpeed", sep = "/"),
                      pattern='*.csv', full.names=TRUE)

# Recorrer cada archivo
for(i in seq_along(files3)) {
  # saltarse las dos primeras filas de informacion (no son titulos)
  archivo <- read.csv(files3[i], skip = 2, header = FALSE)
  archivo$V1 <- as_datetime(archivo$V1) # variable de fecha y hora de la medicion
  # crear variable que almcena la fecha en formato año y mes
  archivo$mes <- paste(unique(year(archivo$V1)), unique(month(archivo$V1)), sep = "-")
  # Uniendo la informacion de todos los archivos en un solo data.frame
  rbind(df, archivo) -> df
}

datosWindMag <- df[,-1]
# renombrando las columnas
colnames(datosWindMag) <- c("Lat", "Long", "WindMag.mxs", "Mes")
# Guardando la informacion en un unico archivo
# write.csv(datosWindMag, paste(wd.bds, "data_WindMag_mes_con_coord.csv", sep = "/"), row.names = FALSE)

################ union de todos los datasets ##################

# concentracion de clorofila en las zonas de pesca
# data clorofila completa por codigo de area

clor.area <- read.csv(paste(wd.bds, "clorofila_codigo_area_completa.csv", sep = "/"),
                      header = TRUE)
str(clor.area)

# obtenemos el valor promedio de concentracion de clorofila por año-mes y area
clor.area.distinct <- clor.area %>% 
  dplyr::select(-c(Long, Lat, id)) %>% 
  dplyr::filter(cod.area != "XX") %>% 
  dplyr::group_by(cod.area, yymm.clor) %>% 
  dplyr::summarise(clor.mass = mean(Monthly.Chlor)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yy = as.numeric(substr(yymm.clor, 1, 4)),
                mm = as.numeric(ifelse(nchar(yymm.clor)==6, 
                                       substr(yymm.clor, nchar(yymm.clor)-1+1, nchar(yymm.clor)),
                                       substr(yymm.clor, nchar(yymm.clor)-2+1, nchar(yymm.clor)))))

str(clor.area.distinct)

clor.area.distinct <- clor.area.distinct %>% 
  mutate(area.mes = paste(cod.area, mm, sep = "_"))

# SST en las zonas de pesca
# data SST completa por codigo de area

SST.area <- read.csv(paste(wd.bds, "SST_codigo_area_completa.csv", sep = "/"),
                     header = TRUE)
str(SST.area)

# obtenemos el valor promedio de SST por año-mes y area
SST.area.distinct <- SST.area %>% 
  dplyr::select(-c(Long, Lat, id)) %>% 
  dplyr::filter(cod.area != "XX") %>% 
  dplyr::group_by(cod.area, yymm.sst) %>% 
  dplyr::summarise(SST = mean(Monthly.SST)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yy = as.numeric(substr(yymm.sst, 1, 4)),
                mm = as.numeric(ifelse(nchar(yymm.sst)==6, 
                                       substr(yymm.sst, nchar(yymm.sst)-1+1, nchar(yymm.sst)),
                                       substr(yymm.sst, nchar(yymm.sst)-2+1, nchar(yymm.sst)))))

str(SST.area.distinct)

# dataset de info de pesca con clorofila promedio por codigo de area
df.aux3 <- dplyr::left_join(dfzpesca, clor.area.distinct, by = c("cod.area", "yymm.zarpe" = "yymm.clor")) %>% 
  dplyr::select(-c(yy, mm))
dim(df.aux3)

# adicionando dataset de temperatura del agua
df.aux10 <- left_join(df.aux3, SST.area.distinct, by = c("cod.area", "yymm.zarpe" = "yymm.sst")) %>% 
  dplyr::select(-c(yy, mm))

df.aux3 <- df.aux10
dim(df.aux3)

summary(df.aux3$clor.mass) # observamos que hay NA's porque no ha habido medicion
# detectada para dicha zona en dichos años-meses

summary(df.aux3$SST)

# Ahora corregiremos la temperatura del agua, considerando las otras mediciones observadas.
# Donde haya NA, ocuparemos la temperatura del agua promediada de las mediciones del satelite
df.aux3$SSTcorrect <- ifelse(is.na(df.aux3$water.temperature), df.aux3$SST, df.aux3$water.temperature)

summary(df.aux3$SSTcorrect) # tiene un NA

df.aux3 <- df.aux3 %>% 
  mutate(area.mes = paste(cod.area, mm.zarpe, sep = "_")) # variable del area y mes concatenadas

# Guardando el dataset
# write.csv(df.aux3, paste(wd.bds, "data_pesca_clor_SST_completa.csv", sep = "/"), row.names = FALSE)

## importar el archivo de mangnitud del viento por codigo de area trabajado manualmente
# este archivo contiene las mediciones puntuales de cada mes (filas), en cada area 
# codificada por la institucion (columnas)

windmag.areas <- read.csv(paste(wd.bds,"data_WindMag_x_cod_area_mes.csv", sep = "/"),
                          header = TRUE)

# mantener solo la variable mes y todas las de areas
windmag.areas <- windmag.areas[,-c(1:3,5)]
str(windmag.areas)

# transformar el dataset a lo largo, colocando las areas de la J19 a la S4 (Columnas) como filas 
windmag.areas.long <- tidyr::gather(windmag.areas, cod.area, Monthly.WindMag, J19:S4)
# Mantener las mediciones con temperaturas validas (mayor que cero)
windmag.areas.long <- windmag.areas.long[which(windmag.areas.long$Monthly.WindMag>0),]
str(windmag.areas.long)

# crear una variable auxiliar que concatena el mes y el area para que sirva de campo de union
# con el dataset de los registros pesqueros
windmag.areas.long$id.zonames <- paste(windmag.areas.long$cod.area, windmag.areas.long$yymm.sst,
                                       sep = "_")

# juntando los datasets df.aux3 (registros pesqueros, clorofila y temperatura del agua) y
# el dataset de magnitud del viento windmag.areas.long
df.aux5 <- left_join(df.aux3, windmag.areas.long[,c(3,4)], by = "id.zonames")

# actualizando df.aux3
df.aux3 <- df.aux5

# creacion de variable CPUE = PescaXlance
df.aux3 <- df.aux3 %>% 
  mutate(clase.barco = str_trim(clase.barco, side = "both"),
         PescaXlance = captura.ton/no.lance)

# remover los meses de veda para pelágicos pequeños: Marzo y Septiembre de cada año
df.aux3 <- df.aux3 %>% 
  filter(!(mm.zarpe %in% c(3,9))) #%>% 
# mutate(yymm.zarpe = factor(df.aux3$yymm.zarpe, levels = yymes.factores))
str(df.aux3)

# Guardar el dataset
# write.csv(df.aux3, paste(wd.bds, "DataTrabajar_dfaux3.csv", sep = "/"), 
          # row.names = FALSE)

# Seleccion de las variables de interes en el dataset de trabajo
dffish.ocean <- df.aux3[,c("id", "id.zonames", "yymm.zarpe", "mm.zarpe", 
                           "clor.mass", "SSTcorrect", 
                           "Monthly.WindMag", "latitude.dd", "longitude.dd",
                           "latitude.dms", "longitude.dms", "cod.area", 
                           "captura.ton", "lat.meddegree", "lon.meddegree", 
                           "lat.mindegree", "lon.mindegree",
                           "lat.maxdegree", "lon.maxdegree", "no.lance",
                           "PescaXlance", "datetime.lance", "datetime.recogida",
                           "EsfuerzoHoras", "area.mes")]

# renombrar las columnas del dataset de trabajo
colnames(dffish.ocean) <- c("id", "id.zonames", "yymm.zarpe", "mes","Chlorophylla", "SST", 
                            "Wind.Magnitude", "lat.dd", "long.dd",
                            "lat.dms", "long.dms", "area", 
                            "captura.ton", "lat.meddegree", "long.meddegree", 
                            "lat.mindegree", "long.mindegree",
                            "lat.maxdegree", "long.maxdegree", "no.lance",
                            "PescaXlance", "datetime.lance", "datetime.recogida",
                            "EsfuerzoHoras", "area.mes")

# guardar dataset de trabajo generado
# write.csv(dffish.ocean, paste(wd.bds, "DataTrabajar_dffishocean.csv", sep = "/"), 
#           row.names = FALSE)

# eliminar las mediciones de junio, julio y agosto de 2022, ya que no se cuenta informacion
# oceanografica para dichos meses
dffish.ocean2 <- dffish.ocean %>% 
  filter(!(yymm.zarpe %in% c("2022-6", "2022-7", "2022-8")))
dim(dffish.ocean2)

# creando variable auxiliar del mes de medicion como objeto date
dffish.ocean2$yymm.zarpe2 <- as.Date(paste(dffish.ocean2$yymm.zarpe, "1", sep = "-"))


################ Exploracion ####################
hist(dffish.ocean2$PescaXlance) # CPUE sera PescaXlance 

###### Imputacion de faltantes de clorofila ##########

### analisis espaciotemporal de clorofila #####
# dataset de concentracion de clorofila en formato largo
clor.area.long <- clor.area %>% 
  mutate(yymm.clor2 = ym(yymm.clor)) %>% # defining yymm.clor as date format
  dplyr::select(Long, Lat, cod.area, yymm.clor2, Monthly.Chlor)#%>% 
# tidyr::spread(key = yymm.clor2, value = Monthly.Chlor)

str(clor.area.long)
head(clor.area.long)

##### analisis como areas ######
# juntamos todos los puntos de los limites en un solo data.frame - formato largo
# i.e. las areas aparecen dos veces repetidas, una fila que indica la longitud y latitud inferiores
# y la segunda vez para identificar la fila de longitud y latitud superiores

# areas segun su limite inferior tanto en longitud como latitud
groupa <- cod.area[,c("cod.area", "lon.mindegree", "lat.mindegree")]
colnames(groupa) <- c("area", "long", "lat")
rownames(groupa) <- paste(groupa$area, rep(1, times = dim(groupa)[1]), sep = ".")
# areas segun su limite superior tanto en longitud como latitud
groupb <- cod.area[,c("cod.area", "lon.maxdegree", "lat.maxdegree")]
colnames(groupb) <- c("area", "long", "lat")
rownames(groupb) <- paste(groupb$area, rep(2, times = dim(groupb)[1]), sep = ".")

# esta es la capa de los limites para las areas
areas.coord <- rbind(groupa, groupb) # juntar en una sola matriz
colnames(areas.coord) <- c("area", "x", "y")
head(areas.coord)

# limites - estructura
v1 <- dismo::voronoi(areas.coord[,-1])
proj4string(v1) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
class(v1) # "SpatialPolygonsDataFrame" package "sp"

## merging the data.frames
# XX son las areas fuera del estudio de la condificacion de la institucion pesquera
clorxarea.long <- left_join(clor.area.long[clor.area.long$cod.area != "XX",],
                            cod.area[,c("cod.area", "lon.mindegree", "lon.maxdegree",
                                         "lat.mindegree", "lat.maxdegree", "lon.middegree",
                                         "lat.middegree")], 
                            by = c("cod.area" = "cod.area"))
str(clorxarea.long)

####### identificacion de datos faltantes en clorofila #######
nasclor.data <- dffish.ocean2 %>% 
  filter(is.na(dffish.ocean2$Chlorophylla)) %>% 
  mutate(yymm.zarpe2 = ym(yymm.zarpe),
         yy = year(yymm.zarpe2),
         mm = month(yymm.zarpe2)) %>% 
  dplyr::select(id, id.zonames, yymm.zarpe2, area, long.meddegree, lat.meddegree, yy, mm)

dim(nasclor.data)

table(dffish.ocean2$yymm.zarpe2[-which(dffish.ocean2$id %in% nasclor.data$id)])

###### graficos espaciales ######
# Grafico espacial por area seleccionando un mes o año - agrupado
clor.area.temporal <- clorxarea.long %>% 
  mutate(yy = year(yymm.clor2),
         mm = month(yymm.clor2)) %>% 
  filter(str_detect(yymm.clor2, "2021")) %>%  # cambiar el año
  # filter(!(yymm.clor2 %in% c("2022-6", "2022-7", "2022-8"))) %>% 
  # filter(str_detect(yymm.clor2, "-12-")) %>% 
  dplyr::group_by(cod.area, yymm.clor2) %>% 
  summarise(lon.mindegree = mean(lon.mindegree),
            lon.maxdegree = mean(lon.maxdegree),
            lat.mindegree = mean(lat.mindegree),
            lat.maxdegree = mean(lat.maxdegree),
            Monthly.Chlor = mean(Monthly.Chlor),
            lon.middegree = mean(lon.middegree),
            lat.middegree = mean(lat.middegree),
            yy = mean(yy), mm = mean(mm))

table(clor.area.temporal$mm)

# etiquetas de los meses
names.months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago",
                  "Sep", "Oct", "Nov", "Dic")
names(names.months) <- c(1:12)

# Grafico espacial por meses en cada año
jpeg("Chlor_Distribucion_EspacialXMes_2021.jpeg", # cambiar nombre segun el año
     width = 15, height = 10, units = "in", res = 300)
ggplot(data = clor.area.temporal) +
  facet_wrap(~mm, labeller = labeller(mm = names.months)) +
  geom_rect(aes(xmin = lon.mindegree, ymin = lat.mindegree,
                xmax = lon.maxdegree, ymax = lat.maxdegree, 
                fill = Monthly.Chlor)) +
  # coord_quickmap() +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Chlor (mg/m3)", limits = c(0,13), breaks = c(0,4,9,13)) +
  geom_text(aes(label = cod.area, x = lon.middegree, y = lat.middegree), 
            color = "black", size = 2) +
  geom_text(data = nasclor.data[nasclor.data$yy == 2021,], # cambiar filtro segun el año
            aes(label = area, x = long.meddegree, y = lat.meddegree),
            color = "red", size = 2) +
  # geom_point(data = nasclor.data[nasclor.data$yy == 2017,], 
  #            aes(x = long.meddegree, y = lat.meddegree),
  #            color = "red", alpha = 0.5) +
  ggtitle("Distribución espacial de la concentración de clorofila por mes",
          subtitle = "Año: 2021") + # cambiar nombre segun el año
  xlab("Longitud") + ylab("Latitud") + labs(fill = "Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom")

dev.off()

# Grafico espacial por año de cada mes
jpeg("Chlor_Distribucion_EspacialXAño_12-Diciembre.jpeg", # cambiar nombre segun el mes
     width = 15, height = 10, units = "in", res = 300)
ggplot(data = clor.area.temporal) +
  facet_wrap(~yy) +
  geom_rect(aes(xmin = lon.mindegree, ymin = lat.mindegree,
                xmax = lon.maxdegree, ymax = lat.maxdegree, 
                fill = Monthly.Chlor)) +
  # coord_quickmap() +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Chlor (mg/m3)", limits = c(0,13)) +
  geom_text(aes(label = cod.area, x = lon.middegree, y = lat.middegree), 
            color = "black", size = 2) +
  geom_text(data = nasclor.data[nasclor.data$mm == 12,], 
            aes(label = area, x = long.meddegree, y = lat.meddegree),
            color = "red", size = 2) +
  ggtitle("Distribución espacial de la concentración de clorofila por año",
          subtitle = "Mes: Diciembre") + # cambiar nombre segun el mes
  xlab("Longitud") + ylab("Latitud") + labs(fill = "Chlor.Mass") +
  theme_bw()

dev.off()


###### graficos temporales ######

clor.area.espacial <- clorxarea.long %>% 
  # mutate(yy = year(yymm.clor2),
  #        mm = month(yymm.clor2)) %>%
  dplyr::filter(yymm.clor2 %in% seq.Date(from = as.Date("2017-01-01"), 
                                  to = as.Date("2022-05-01"), 
                                  by = "1 month")) %>% 
  dplyr::filter(cod.area %in% # desactivar filtros segun el grupo de areas
           # c("S4", "P8", "Q8", "P9", "Q9", "N11", "N12", # Grupo 1
           #   "O14", "L15", "M16", "M17")) %>%
           # c("J19", "J20", "K19", "K20", "K21", "L18", "L19", # Grupo 2
           #   "L20", "L21", "M18", "M19", "M20", "M21", "N20", "N21")) %>% 
           # c("J24", "K22", "K23", "K24", "K25", "L22", "L23", # Grupo 3
           #   "L24", "L25", "M22", "M24", "M25", "N25", "O25")) %>%
           # c("J26", "K26", "K27", "K30", "K31", "L26", "L27", # Grupo 4
           #   "L28", "L29", "L30", "L31", "M26", "M27", "M28", "M29", "M30")) %>%
           # c("N26", "N27", "N28", "N29", "N30", "O26", "O27", # Grupo 5
           #   "O28", "O29", "O30", "O31", "P27", "P28", "P29", "P30", "Q29")) %>%
            c("J24","K20","K21","K22","K23","K24","L18","L21","L22","L23","L24",
              "L25","M18","M20","M21","Q8")) %>% # Grupo de áreas con valores faltantes
  dplyr::group_by(cod.area, yymm.clor2) %>% 
  dplyr::summarise(lon.mindegree = mean(lon.mindegree),
            lon.maxdegree = mean(lon.maxdegree),
            lat.mindegree = mean(lat.mindegree),
            lat.maxdegree = mean(lat.maxdegree),
            Monthly.Chlor = mean(Monthly.Chlor),
            lon.middegree = mean(lon.middegree),
            lat.middegree = mean(lat.middegree) #,
            #yy = mean(yy), mm = mean(mm)
  )

# Grafico espacial por año de cada mes

# cambio temporal del nombre de la variable para grafico temporal
nasclor.data2 <- nasclor.data %>% 
  mutate(
    # renombrar columnas
    yymm.clor2 = yymm.zarpe2,
    cod.area = area) %>% 
  dplyr::select(yymm.clor2, cod.area) 

# data.frame que guardara todos los años consecutivos al del NA
yy.relacionnas <- data.frame(yymm.clor2 = c(),
                             cod.area = c())
for(i in 1:dim(nasclor.data2)[1]){
  # para cubrir todos los años con que contienen faltante
  mm <- month(nasclor.data2$yymm.clor2[i])
  # secuencia de años
  yymm.clor2 <- seq.Date(from = ymd(paste(paste("2017",mm,sep = "-"), "01", sep = "-")), 
                         to = as.Date("2022-05-01"), 
                         by = "1 year")
  # repitiendo el área del faltante para todos los años
  cod.area.vector <- rep(nasclor.data2$cod.area[i], times = length(yymm.clor2))
  # uniendo la data
  df.auxiliar <- cbind(yymm.clor2, cod.area.vector)
  yy.relacionnas <- rbind(yy.relacionnas, df.auxiliar)
  yy.relacionnas$yymm.clor2 <- as.numeric(yy.relacionnas$yymm.clor2)
}
str(yy.relacionnas)
yy.relacionnas$yymm.clor2 <- as_date(yy.relacionnas$yymm.clor2)
View(yy.relacionnas)


jpeg("Chlor_Distribucion_TemporalGrupoFaltantes.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(data = clor.area.espacial) +
  geom_line(aes(x = yymm.clor2, y = Monthly.Chlor)) +
  scale_x_date(
    # breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
    #                                           to = as.Date("2022-05-01"), 
    #                                           by = "1 year"),
    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                        to = as.Date("2022-05-01"), 
                                        by = "1 month")) +
  geom_vline(aes(xintercept = yymm.clor2), data = yy.relacionnas,
             color = "gray45") +
  geom_vline(aes(xintercept = yymm.clor2), data = nasclor.data2,
             color = "red") +
  facet_wrap(~cod.area) +
  ggtitle("Distribución temporal de la concentración de clorofila por código de área",
          subtitle = "Áreas con valores faltantes") +
  xlab("Mes") + ylab("Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15))
# theme(panel.spacing = unit(1, "lines"))

dev.off()

###### graficos hovmoller ######

# data agrupada del mes y longitud
hovmoller.long <- clorxarea.long %>% 
  filter(!(yymm.clor2 %in% c("2022-6", "2022-7", "2022-8"))) %>% 
  group_by(lon.mindegree, lon.maxdegree, yymm.clor2) %>% 
  summarise(Monthly.Chlor = mean(Monthly.Chlor)) %>% 
  ungroup() %>% 
  # adicionar un mes a la grilla
  mutate(yymm.clor3 = add_with_rollback(yymm.clor2, months(1)))
head(hovmoller.long)
# data agrupada del mes y latitud
hovmoller.lat <- clorxarea.long %>% 
  filter(!(yymm.clor2 %in% c("2022-6", "2022-7", "2022-8"))) %>% 
  group_by(lat.mindegree, lat.maxdegree, yymm.clor2) %>% 
  summarise(Monthly.Chlor = mean(Monthly.Chlor)) %>% 
  ungroup() %>% 
  # adicionar un mes a la grilla
  mutate(yymm.clor3 = add_with_rollback(yymm.clor2, months(1)))
head(hovmoller.lat)

# plot hovmoller lat
hovlong.plot <- ggplot(hovmoller.long) +
  geom_rect(aes(xmin = lon.mindegree, ymin = yymm.clor2,
                xmax = lon.maxdegree, ymax = yymm.clor3, 
                fill = Monthly.Chlor)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      # modificar los limites segun el rango de la variable de interes
                      name = "Chlor (mg/m3)", limits = c(0,13), breaks = c(0, 4, 9, 13)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "6 months")) +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # cambiar limites segun la longitud del area de interes
  scale_x_continuous(limits = c(-81.5, -79.8), breaks = seq(-81.5, -79.8, by = 0.5)) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot <- ggplot(hovmoller.lat) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.clor2,
                xmax = lat.maxdegree, ymax = yymm.clor3, 
                fill = Monthly.Chlor)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Chlor (mg/m3)", limits = c(0,13), breaks = c(0, 4, 9, 13)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "6 months"), position = "right") +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # cambiar limites segun la latitud del area de interes
  scale_x_continuous(limits = c(-3.7, 1), breaks = seq(-3.7, 1, by = 1)) +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15))

# juntando ambos graficos hovmoller para mostrarlos en una unica pantalla
multi.page <- ggarrange(hovlong.plot, hovlat.plot,
                        nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                       "Mes vs Latitud"),
                        common.legend = TRUE, legend = "bottom",
                        font.label = list(size = 15))

jpeg("Chlor_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page, top = text_grob("Hovmoller plot", 
                                            face = "bold", size = 16))
dev.off()


###### analisis estadistico previo a la prediccion de valores faltantes de clorofila ######

# media espacial empirica
chlor.emp.areamean <- dffish.ocean2 %>%
  # filter(area %in% c("J24","K20","K21","K22","K23","K24","L18","L21","L22","L23","L24",
  # "L25","M18","M20","M21","Q8")) %>% # Grupo de áreas con valores faltantes)
  dplyr::group_by(area) %>% 
  dplyr::summarise(mu.location = mean(Chlorophylla, na.rm = TRUE)) # media espacial empirica por locacion

View(chlor.emp.areamean) 

chlor.emp.areamean <- left_join(chlor.emp.areamean,
                                cod.area[ ,c("cod.area", "lon.mindegree", "lat.mindegree",
                                             "lon.maxdegree", "lat.maxdegree",
                                             "lon.middegree", "lat.middegree")],
                                by = c("area" = "cod.area"))
str(chlor.emp.areamean) 

# media temporal empirica
chlor.emp.mesmean <- dffish.ocean2 %>%
  dplyr::mutate(yymm.zarpe = as_date(paste(dffish.ocean2$yymm.zarpe, "1", sep = "-"))) %>% 
  # dplyr::filter(yymm.zarpe %in% unique(nasclor.data2$yymm.clor2)) %>% # Grupo de meses con valores faltantes)
  dplyr::group_by(yymm.zarpe) %>% 
  dplyr::summarise(mu.time = mean(Chlorophylla, na.rm = TRUE)) # media temporal empirica

View(chlor.emp.mesmean) 

# Grafico espacial de la concentración de clorofila promedio
clorareamean.plot <- ggplot(data = chlor.emp.areamean) +
  geom_rect(aes(xmin = lon.mindegree, ymin = lat.mindegree,
                xmax = lon.maxdegree, ymax = lat.maxdegree, 
                fill = mu.location)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3",
                      # cambiar limites en funcion del rango de la variable de interes
                      name = "Chlor (mg/m3)", limits = c(0,8), breaks = c(0,4,8)) +
  geom_text(aes(label = area, x = lon.middegree, y = lat.middegree), 
            color = "black", size = 4) +
  # resaltar area con valor faltante
  geom_text(data = nasclor.data,
            aes(label = area, x = long.meddegree, y = lat.meddegree),
            color = "red", size = 4) +
  ggtitle("") +
  xlab("Longitud") + ylab("Latitud") + labs(fill = "Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15))

# Grafico temporal de la concentración de clorofila promedio
clormesmean.plot <- ggplot(data = chlor.emp.mesmean) +
  geom_line(aes(x = yymm.zarpe, y = mu.time)) +
  scale_x_date(
    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                        to = as.Date("2022-05-01"), 
                                        by = "1 month")) +
  # geom_vline(aes(xintercept = yymm.clor2), data = yy.relacionnas,
  #            color = "gray45") +
  geom_vline(aes(xintercept = yymm.clor2), data = nasclor.data2,
             color = "red") +
  ggtitle("") +
  xlab("Mes") + ylab("Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15))

# juntar graficos de medias temporales y espaciales para mostrarlos en una pantalla
multi.page.clormean <- ggarrange(clorareamean.plot, clormesmean.plot,
                                 nrow = 1, ncol = 2,
                                 labels = c("Por área", "Por mes"),
                                 common.legend = TRUE, legend = "bottom",
                                 font.label = list(size = 15))

jpeg("Chlor_Distribucion_Promedio.jpeg",
     width = 15, height = 10, units = "in", res = 300)

annotate_figure(multi.page.clormean,
                top = text_grob("Distribución promedio de la concentración de clorofila", 
                                face = "bold", size = 16))

dev.off()

#### Prediccion de datos faltantes en clorofila #####

# Usaremos las dos dimensiones: espacial (longitud, latitud) y temporal (meses)
# Asi, agruparemos los datos observados por longtud, latitud, mes, y sumarizamos el promedio

## declararemos la variable como una secuencia mensual, para ello
# crearemos una secuencia que representara cada mes
meses.consecutivos <- data.frame(
  mes = seq.Date(from = as.Date("2017-01-01"),
                 to = as.Date("2022-05-01"),
                 by = "1 month"),
  seq.mes = c(1:length(seq.Date(from = as.Date("2017-01-01"),
                                to = as.Date("2022-05-01"),
                                by = "1 month"))))
# añadimos la secuencia de los meses a la data original 
dffish.ocean2 <- left_join(dffish.ocean2,meses.consecutivos, by = c("yymm.zarpe2" = "mes"))

# agruparemos los datos por locacion y mes
data.imput.clor <- dffish.ocean2 %>% 
  dplyr::group_by(long.meddegree, lat.meddegree, seq.mes) %>% 
  # sumarizamos por valor promedio
  dplyr::summarise(Chlorophylla = mean(Chlorophylla, na.rm = TRUE)) %>% 
  dplyr::ungroup()

# data con los valores faltantes - son 25 combinaciones de locacion y mes para clorofila = 68 registros en la base de pesca
data.faltante.clor <- data.imput.clor[which(is.na(data.imput.clor$Chlorophylla)),-4]

# removemos los valores con NA de la data que se usara para la prediccion 
data.imput.clor <- na.omit(data.imput.clor) 

# determinemos el mejor valor de alpha - para la formula de ponderacion por distancia inversa (IDW)
# Para ello, tomaremos como data de prueba a ciertas filas del grupoy evaluaremos la prediccion
# tomando en consideracion su valor real.
clor.idw.residuals <- data.frame(i = c(),
                                 error.est = c())
for(i in 1:15) {
  clor.faltantes.idw <- idw(formula = Chlorophylla ~ 1, # dep. variable
                            locations = ~ long.meddegree + lat.meddegree + seq.mes, # inputs
                            data = data.imput.clor[1:350,], # data set
                            newdata = data.imput.clor[351:471,-4], # prediction grid
                            idp = i) # inv. dist. power (alpha)
  res <- data.frame(alpha = i,
                    error.est = mean(clor.faltantes.idw$var1.pred - data.imput.clor$Chlorophylla[351:471])^2)
  clor.idw.residuals <- rbind(clor.idw.residuals, res)
}
clor.idw.residuals
# graficaremos el rendimiento predictivo de los diferentes valores de alpha
plot(clor.idw.residuals$alpha, clor.idw.residuals$error.est)

# usaremos el metodo de ponderacion de distancia inversa para imputar los faltantes
clor.faltantes.idw <- idw(formula = Chlorophylla ~ 1, # dep. variable
                          locations = ~ long.meddegree + lat.meddegree + seq.mes, # inputs
                          data = data.imput.clor, # data set
                          newdata = data.faltante.clor, # prediction grid
                          idp = 2) # inv. dist. power (alpha)

# para visualizacion en grafico posterior - los predichos seran identificados con color rojo
clor.faltantes.idw$color <- rep("red", times = dim(clor.faltantes.idw)[1])

# juntando los datos a la data completa de pesca
df.clor.imputada <- left_join(x = dffish.ocean2,
                              y = clor.faltantes.idw[,-5],
                              by = c("long.meddegree", "lat.meddegree","seq.mes"))

# actualizacion de datos faltantes en dataset, con los valores predichos
df.clor.imputada$Chlorophylla[which(is.na(df.clor.imputada$Chlorophylla))] <- df.clor.imputada$var1.pred[which(is.na(df.clor.imputada$Chlorophylla))]
df.clor.imputada$color <- replace(x = df.clor.imputada$color,
                                  list = which(is.na(df.clor.imputada$color)),
                                  values = "black")

# verificamos que no ha habido cambio significativo en el comportamiento de la variable
summary(df.clor.imputada$Chlorophylla)
summary(dffish.ocean2$Chlorophylla)

# Grafico espacial por area seleccionando un mes o año - agrupado
# agrupacion de area y fechas de los datos totales de clorofila
meses.faltantes.clor <- unique(nasclor.data$yymm.zarpe2)

clor.area.espacial.imputados <- clorxarea.long %>% 
  dplyr::filter(yymm.clor2 %in% meses.faltantes.clor) %>% 
  dplyr::group_by(cod.area, yymm.clor2) %>% 
  dplyr::summarise(lon.mindegree = mean(lon.mindegree),
            lon.maxdegree = mean(lon.maxdegree),
            lat.mindegree = mean(lat.mindegree),
            lat.maxdegree = mean(lat.maxdegree),
            Monthly.Chlor = mean(Monthly.Chlor),
            lon.middegree = mean(lon.middegree),
            lat.middegree = mean(lat.middegree)) %>% 
  dplyr::ungroup()

# datos imputados clorofila
mi.clor.imput2 <- df.clor.imputada %>% 
  dplyr::filter(!is.na(var1.pred)) %>% 
  dplyr::mutate(cod.area = area,
         yymm.clor2 = yymm.zarpe2) %>% 
  dplyr::group_by(cod.area, yymm.clor2) %>% 
  dplyr::summarise(lon.mindegree = mean(long.mindegree),
            lon.maxdegree = mean(long.maxdegree),
            lat.mindegree = mean(lat.mindegree),
            lat.maxdegree = mean(lat.maxdegree),
            Monthly.Chlor = mean(Chlorophylla),
            lon.middegree = mean(long.meddegree),
            lat.middegree = mean(lat.meddegree)) %>% 
  dplyr::ungroup()

# uniendo los datos observados con los imputados en la data total de clorofila
clor.area.espacial.imputados2 <- rbind(clor.area.espacial.imputados, mi.clor.imput2)
clor.area.espacial.imputados2 <- clor.area.espacial.imputados2 %>% 
  dplyr::group_by(cod.area, yymm.clor2) %>% 
  dplyr::summarise(lon.mindegree = mean(lon.mindegree),
            lon.maxdegree = mean(lon.maxdegree),
            lat.mindegree = mean(lat.mindegree),
            lat.maxdegree = mean(lat.maxdegree),
            Monthly.Chlor = mean(Monthly.Chlor),
            lon.middegree = mean(lon.middegree),
            lat.middegree = mean(lat.middegree)) %>% 
  dplyr::ungroup()

View(clor.area.espacial.imputados2[which(clor.area.espacial.imputados2$yymm.clor2 == "2017-08-01"),])

# Grafico espacial de valores imputados
plot.imp.sp <- ggplot(data = clor.area.espacial.imputados2) +
  facet_wrap(~yymm.clor2) +
  geom_rect(aes(xmin = lon.mindegree, ymin = lat.mindegree,
                xmax = lon.maxdegree, ymax = lat.maxdegree, 
                fill = Monthly.Chlor)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Chlor (mg/m3)", limits = c(0,5), breaks = c(0,2.5,5)) +
  # geom_text(aes(label = area, x = lon.middegree, y = lat.middegree), 
  #           color = "black", size = 3.5) +
  geom_rect(data = mi.clor.imput2, aes(xmin = lon.mindegree, ymin = lat.mindegree,
                                       xmax = lon.maxdegree, ymax = lat.maxdegree), fill = NA, color = "red") +
  # geom_text(data = nasclor.data, 
  #           aes(label = area, x = long.meddegree, y = lat.meddegree),
  #           color = "red", size = 2.5) +
  ggtitle("Distribución espacial") +
  xlab("Longitud") + ylab("Latitud") + labs(fill = "Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "none")

# grafico temporal de clorofila imputada
# añadir los datos de los faltante a la data de clor.area.long - esto es para lel grafico temporal
mi.clor.imput <- df.clor.imputada %>% 
  dplyr::filter(!is.na(var1.pred)) %>% 
  dplyr::mutate(Long = long.meddegree,
         Lat = lat.meddegree,
         cod.area = area,
         yymm.clor2 = yymm.zarpe2) %>% 
  dplyr::group_by(Long, Lat, cod.area, yymm.clor2) %>% 
  dplyr::summarise(Monthly.Chlor = mean(Chlorophylla)) %>% 
  dplyr::ungroup()

clor.area.temporal.imputados <- rbind(clor.area.long[which(clor.area.long$cod.area %in% unique(mi.clor.imput$cod.area)),], 
                                      mi.clor.imput)

plot.imp.temp <- ggplot(data = clor.area.temporal.imputados) +
  geom_line(aes(x = yymm.clor2, y = Monthly.Chlor)) +
  scale_x_date(
    # breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
    #                                           to = as.Date("2022-05-01"), 
    #                                           by = "1 year"),
    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                        to = as.Date("2022-05-01"), 
                                        by = "1 month")) +
  geom_point(data = mi.clor.imput,
             aes(x = yymm.clor2, y = Monthly.Chlor), color = "red", alpha = .7, size = 2) +
  # geom_vline(aes(xintercept = yymm.clor2), data = yy.relacionnas,
  #            color = "gray45") +
  # geom_vline(aes(xintercept = yymm.clor2), data = nasclor.data2,
  #            color = "red") +
  facet_wrap(~cod.area) +
  ggtitle("Distribución temporal") +
  xlab("Mes") + ylab("Chlor.Mass") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 12))
# theme(panel.spacing = unit(1, "lines"))

plot.imputados.clor <- ggarrange(plot.imp.sp, plot.imp.temp,
                                 nrow = 1, ncol = 2)

jpeg("Chlor_Distribucion_EspacioTemporal_Imputados.jpeg",
     width = 15, height = 10, units = "in", res = 300)

annotate_figure(plot.imputados.clor,
                top = text_grob("Concentración de clorofila de datos espacio-temporales imputados", 
                                face = "bold", size = 15))

dev.off()


# corrigiendo los datos faltantes de magnitud del viento, ya que se trataba de una mala codificacion en el mes
df.clor.imputada[which(is.na(df.clor.imputada$Wind.Magnitude)),"Wind.Magnitude"] <- c(4.957745, 4.957745, 4.957745)
df.clor.imputada[df.clor.imputada$id %in% c(2144,2145,2146),"id.zonames"] <- c("N26_2022-5", "O27_2022-5", "N27_2022-5")
df.clor.imputada[df.clor.imputada$id %in% c(2144,2145,2146),]

# guardaremos toda la informacion en un nuevo dataset
datosTodos <- dffish.ocean2
# write.csv(data.trabajar0, paste(wd.bds,"/DataFinal.csv", sep = "/"))
# write.csv(df.clor.imputada, paste(wd.bds,"/DatosconClorMagCompletos.csv", sep = "/"))


############## Preparacion de data final ###################

######### Datos para mapas predictivos #########

### Datos para prediccion - mapas
# Estos son datasets creados con la informacion oceanografica de todas las areas del mapa de
# la institucion pesquera (inclusive aquellas que no cuentan con registro pesquero de la especie)
# para cada mes

# Data para predicción de Enero
datapred1 <- read.csv(paste(wd.bds,"Data_prediccion_01Enero.csv", sep = "/"), header = TRUE)
# Data para predicción de Febrero
datapred2 <- read.csv(paste(wd.bds,"Data_prediccion_02Febrero.csv", sep = "/"), header = TRUE)
# Data para predicción de Abril
datapred4 <- read.csv(paste(wd.bds,"Data_prediccion_04Abril.csv", sep = "/"), header = TRUE)
# Data para predicción de Mayo
datapred5 <- read.csv(paste(wd.bds,"Data_prediccion_05Mayo.csv", sep = "/"), header = TRUE)
# Data para predicción de Junio
datapred6 <- read.csv(paste(wd.bds,"Data_prediccion_06Junio.csv", sep = "/"), header = TRUE)
# Data para predicción de Julio
datapred7 <- read.csv(paste(wd.bds,"Data_prediccion_07Julio.csv", sep = "/"), header = TRUE)
# Data para predicción de Agosto
datapred8 <- read.csv(paste(wd.bds,"Data_prediccion_08Agosto.csv", sep = "/"), header = TRUE)
# Data para predicción de Octubre
datapred10 <- read.csv(paste(wd.bds,"Data_prediccion_10Octubre.csv", sep = "/"), header = TRUE)
# Data para predicción de Noviembre
datapred11 <- read.csv(paste(wd.bds,"Data_prediccion_11Noviembre.csv", sep = "/"), header = TRUE)
# Data para predicción de Diciembre
datapred12 <- read.csv(paste(wd.bds,"Data_prediccion_12Diciembre.csv", sep = "/"), header = TRUE)


########### Preparacion de variables ####################

# Chequeando outliers de PescaXlance mediante el metodo de boxplot ajustado para distribuciones asimetricas
aberrantes <- boxB(datosTodos$PescaXlance, k=3, method = "asymmetric")
# No. of outliers in left tail: 0
# No. of outliers in right tail: 36
aberrantes$outliers
sort(datosTodos$PescaXlance[aberrantes$outliers])
sort(datosTodos$captura.ton[aberrantes$outliers])

# Removeremos los outliers
datosTodos2 <- datosTodos[-aberrantes$outliers,]

# calculando el CPUE maximo por mes - y el IAR
datosTodos2 <- datosTodos2 %>%
  dplyr::group_by(as.factor(mes)) %>%
  dplyr::mutate(PescaXlanceMAX = max(PescaXlance)) %>%
  dplyr::ungroup() %>% 
  # indice de abundancia relativa
  dplyr::mutate(IAR = PescaXlance/PescaXlanceMAX)

# Logaritmo de IAR, corregir para indeterminaciones
datosTodos2$logIAR <- log(ifelse(datosTodos2$IAR==0, 0.0001, datosTodos2$IAR))

# SQRT de IAR - es mejor porque mantiene la escala y reduce un poco el sesgo
datosTodos2$sqrtIAR <- sqrt(datosTodos2$IAR)
datosTodos2$yymm.zarpe2 <- mdy(datosTodos2$yymm.zarpe2)


# Agrupaciones
datosTodos2 <- datosTodos2 %>%
  dplyr::group_by(as.factor(mes)) %>%
  dplyr::mutate(medianaMES = median(sqrtIAR),
                tercil1MES = quantile(sqrtIAR, c(0.33)),
                tercil2MES = quantile(sqrtIAR, c(0.67)),
                grupoJenks.Corte2 = getJenksBreaks(var = sqrtIAR, k = 3)[2]) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(bondad.captura2 = ifelse(sqrtIAR <= medianaMES, 0, 1),
                bondad.captura3 = ifelse(sqrtIAR <= tercil1MES, 0, 
                                         ifelse(sqrtIAR <= tercil2MES, 1, 2)),
                bondad.captura4 = ifelse(sqrtIAR <= grupoJenks.Corte2, 0, 1))

# Mediana es 5
datosTodos2$bondad.capturaPesca2 = ifelse(datosTodos2$PescaXlance <= 5, 0, 1)

getJenksBreaks(var = datosTodos2$sqrtIAR, k = 3, subset = NULL)
# 0.0000000 0.4008919 1.0000000
getJenksBreaks(var = datosTodos2$sqrtIAR, k = 4, subset = NULL)
# 0.0000000 0.2897043 0.5477226 1.0000000

getJenksBreaks(var = datosTodos2$PescaXlance, k = 6, subset = NULL)
datosTodos2 %>% 
  dplyr::group_by(area) %>% 
  dplyr::summarize(PescaMin = min(PescaXlance),
                   PescaMax = max(PescaXlance),
                   PescaMedia = mean(PescaXlance),
                   PescaMediana = median(PescaXlance)) %>% 
  View()

datosTodos2$bondad.captura3.GRAL <- ifelse(datosTodos2$sqrtIAR <= 0.2897043, 0,
                                           ifelse(datosTodos2$sqrtIAR <= 0.5477226, 1, 2))

# Ordenando los datos por fecha
datosTodos2 <- datosTodos2 %>% 
  dplyr::arrange(clase.barco, yymm.zarpe2)


table(datosTodos2$bondad.captura2)
#   0   1 
# 967 888 
table(datosTodos2$bondad.captura3)
# 0   1   2 
# 643 610 602
table(datosTodos2$bondad.captura4)
#   0   1 
# 1262 593 
table(datosTodos2$bondad.captura5.GRAL)
# 0   1   2   3   4 
# 424 585 469 277 100
table(datosTodos2$bondad.captura3.GRAL)
# 0   1   2 
# 925 694 236 

# Agrupacion Jenks natural breaks classification - Todos
getJenksBreaks(var = datosTodos2$sqrtIAR, k = 6, subset = NULL)
# [1] 0.000000 0.348466 1.000000
getJenksBreaks(var = datosTodos2$PescaXlance, k = 6, subset = NULL)
# [1]  0.00000  5.60000 12.50000 21.66667 32.00000 54.00000

datosTodos2 %>%
  dplyr::summarise(grupoJenks.Corte1 = getJenksBreaks(var = sqrtIAR, k = 6)[1],
                   grupoJenks.Corte2 = getJenksBreaks(var = sqrtIAR, k = 6)[2],
                   grupoJenks.Corte3 = getJenksBreaks(var = sqrtIAR, k = 6)[3]) 

################ Analisis univariado ###################
str(datosTodos)

# Mes
summary.uni.mes <- datosTodos %>% 
  dplyr::group_by(yymm.zarpe2) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(proportion = paste(round(100*count/sum(count), 1), "%", sep = "")) #%>% 
#arrange(desc(proportion))

# Año
summary.uni.yy <- datosTodos %>% 
  dplyr::mutate(yy = year(yymm.zarpe2)) %>% 
  dplyr::group_by(yy) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(proportion = paste(round(100*count/sum(count), 1), "%", sep = "")) #%>% 
#arrange(desc(proportion))

# Area
summary.uni.area <- datosTodos %>%
  dplyr::group_by(area) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(proportion = count/sum(count)) %>% 
  #dplyr::mutate(proportion = paste(round(100*count/sum(count), 1), "%", sep = "")) %>% 
  dplyr::arrange(desc(proportion))

# write.table(summary.uni.area, paste(wd.analysis,"/outout_auxiliar.txt", sep = "/"),
#             row.names = FALSE)

# Numerical variables
summary(datosTodos[,c("Chlorophylla", "SST", "Wind.Magnitude", "PescaXlance")])
apply(datosTodos[,c("Chlorophylla", "SST", "Wind.Magnitude", "PescaXlance")], 2, 
      FUN = function(x) sd(x))
apply(datosTodos[,c("Chlorophylla", "SST", "Wind.Magnitude", "PescaXlance")], 2, 
      FUN = function(x) skewness(x))

summary(datosTodos2$IAR)
sd(datosTodos2$IAR)
skewness(datosTodos2$IAR)

summary(datosTodos2$sqrtIAR)
sd(datosTodos2$sqrtIAR)
skewness(datosTodos2$sqrtIAR)

################ Analisis multivariado ###################

##### Matriz de correlaciones #####

cor(datosTodos2[,c("Chlorophylla", "SST", "Wind.Magnitude", "sqrtIAR")],
    method = "spearman")
cor.test(datosTodos2$Wind.Magnitude, datosTodos2$sqrtIAR,
         method = "spearman")
plot(datosTodos2[,c("Chlorophylla", "SST")])
plot(datosTodos2[,c("Wind.Magnitude", "SST")])
plot(datosTodos2[,c("Wind.Magnitude", "sqrtIAR")])
plot(datosTodos2[,c("Chlorophylla", "SST")])

##### PCA #####

pca.fish <- prcomp(datosTodos2[,c("Chlorophylla", "SST", "Wind.Magnitude", "sqrtIAR")],
                   scale. = TRUE, center = TRUE)

jpeg("biplot_numericas_general.jpeg",
     width = 15, height = 10, units = "in", res = 300)

fviz_pca_biplot(pca.fish, geom.ind = "point", #col.ind = "black", fill.ind = "white",
                # col.var = "navyblue", #repel = TRUE, 
                labelsize = 7, pointsize = 3,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
) + theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

dev.off()

####### Medias espaciotemporales ########

# se usa la media empirica y se estandariza para medios de visualizacion

###### media espaciotemporal empirica ######
emp.mean.areatemp <- datosTodos2 %>%
  dplyr::group_by(area, yymm.zarpe2) %>% 
  # media espacial empirica por locacion
  dplyr::summarise(Clorofila = mean(Chlorophylla),
            Temperatura = mean(SST),
            MagViento = mean(Wind.Magnitude),
            IAR = mean(IAR),
            # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
            Registros = n()) %>% 
  dplyr::ungroup()

emp.mean.areatemp2 <- datosTodos2 %>%
  dplyr::group_by(long.meddegree, lat.meddegree, yymm.zarpe2) %>% 
  # media espacial empirica por locacion
  dplyr::summarise(Clorofila = mean(Chlorophylla),
            Temperatura = mean(SST),
            MagViento = mean(Wind.Magnitude),
            IAR = mean(IAR),
            # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
            Registros = n()) %>% 
  dplyr::ungroup()

###### media espacial empirica ######

# obtendremos los residuos de eliminar el efecto del tiempo
mean.area.res.temp.scaled <- emp.mean.areatemp %>% 
  dplyr::mutate(ClorofilaRes = residuals(lm(Clorofila ~ yymm.zarpe2,
                                     data = emp.mean.areatemp)),
         TemperaturaRes = residuals(lm(Temperatura ~ yymm.zarpe2,
                                       data = emp.mean.areatemp)),
         MagVientoRes = residuals(lm(MagViento ~ yymm.zarpe2,
                                     data = emp.mean.areatemp)),
         IARRes = residuals(lm(IAR ~ yymm.zarpe2,
                                data = emp.mean.areatemp)),
         RegistrosRes = residuals(lm(Registros ~ yymm.zarpe2,
                                     data = emp.mean.areatemp))) %>% 
  dplyr::group_by(area) %>% 
  dplyr::summarise(Clorofila2 = mean(ClorofilaRes),
            Temperatura2 = mean(TemperaturaRes),
            MagViento2 = mean(MagVientoRes),
            IAR2 = mean(IARRes),
            Registros2 = mean(RegistrosRes)) %>% 
  dplyr::ungroup() %>% 
  # escalando y centrando los datos
  dplyr::mutate(Clorofila = as.numeric(scale(Clorofila2, center = TRUE)),
         Temperatura = as.numeric(scale(Temperatura2, center = TRUE)),
         MagViento = as.numeric(scale(MagViento2, center = TRUE)),
         IAR = as.numeric(scale(IAR2, center = TRUE)),
         Registros = as.numeric(scale(Registros2, center = TRUE))) %>% 
  dplyr::select(area, Clorofila, Temperatura, MagViento, IAR, Registros)

# para los datos reales
emp.mean.area <- datosTodos2 %>%
  dplyr::group_by(area) %>% 
  # media espacial empirica por locacion
  dplyr::summarise(Clorofila = mean(Chlorophylla),
            Temperatura = mean(SST),
            MagViento = mean(Wind.Magnitude),
            IAR = mean(IAR),
            # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
            Registros = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(IAR))
# write.table(emp.mean.area, paste(wd.analysis,"/output_auxiliar.txt", sep = "/"),
#             row.names = FALSE)

# cambiar formato a largo
emp.mean.area.long <- emp.mean.area %>% 
  dplyr::mutate(Clorofila = as.numeric(scale(emp.mean.area$Clorofila, center = TRUE)),
         Temperatura = as.numeric(scale(emp.mean.area$Temperatura, center = TRUE)),
         MagViento = as.numeric(scale(emp.mean.area$MagViento, center = TRUE)),
         IAR = as.numeric(scale(emp.mean.area$IAR, center = TRUE)),
         Registros = as.numeric(scale(emp.mean.area$Registros, center = TRUE))) %>%
  # dplyr::select(area, Clorofila2, Temperatura2, MagViento2, CPUE2) %>%
  gather(key = "Variable", value = "Valor", -1)

head(emp.mean.area.long)

mat.emp.mean.area <- as.matrix(emp.mean.area[,-1])
rownames(mat.emp.mean.area) <- emp.mean.area$area
mat.emp.mean.area

# para los residuos
emp.mean.area.long.res <- mean.area.res.temp.scaled %>% 
  gather(key = "Variable", value = "Valor", -1)

# ordenar las areas por IAR descendentemente
areas.cpue.desc <- emp.mean.area %>% 
  arrange(desc(IAR)) %>% 
  dplyr::select(area) %>% 
  distinct() %>% 
  pull()

# usando las variables escalados
areas.cpue.desc.res <- mean.area.res.temp.scaled %>% 
  arrange(desc(IAR)) %>% 
  dplyr::select(area) %>% 
  distinct() %>% 
  pull()


## grafico de los residuos
jpeg("heatmap_promedio_x_area_res_sin_tiempo.jpeg",
     width = 15, height = 12, units = "in", res = 300)

emp.mean.area.long.res %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("IAR", "Clorofila", "MagViento",
                                      "Temperatura", "Registros")),
         area = factor(area, levels = rev(areas.cpue.desc.res))) %>% 
  ggplot(aes(Variable, area)) +
  geom_tile(aes(fill = Valor)) + 
  # geom_text(aes(label = round(Valor, 1)), size = 3) +
  scale_fill_gradient2(low = "royalblue", high = "tomato3", mid = "khaki1",
                       midpoint = 1.5) +
  xlab("Variable (estandarizada)") +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.off()

## plot data para los datos reales
jpeg("heatmap_promedio_x_area.jpeg",
     width = 15, height = 12, units = "in", res = 300)

emp.mean.area.long %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("IAR", "Clorofila", "MagViento",
                                      "Temperatura", "Registros")),
         area = factor(area, levels = rev(areas.cpue.desc))) %>% 
  ggplot(aes(Variable, area)) +
  geom_tile(aes(fill = Valor)) + 
  # geom_text(aes(label = round(Valor, 1)), size = 3) +
  scale_fill_gradient2(low = "royalblue", high = "tomato3", mid = "khaki1",
                       midpoint = 1.5) +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.off()


###### media temporal empirica ######

# obtendremos los residuos de eliminar el efecto del área
mean.temp.res.area.scaled <- emp.mean.areatemp2 %>% 
  dplyr::mutate(ClorofilaRes = residuals(lm(Clorofila ~ long.meddegree + lat.meddegree,
                                     data = emp.mean.areatemp2)),
         TemperaturaRes = residuals(lm(Temperatura ~ long.meddegree + lat.meddegree,
                                       data = emp.mean.areatemp2)),
         MagVientoRes = residuals(lm(MagViento ~ long.meddegree + lat.meddegree,
                                     data = emp.mean.areatemp2)),
         IARRes = residuals(lm(IAR ~ long.meddegree + lat.meddegree,
                                data = emp.mean.areatemp2)),
         RegistrosRes = residuals(lm(Registros ~ long.meddegree + lat.meddegree,
                                     data = emp.mean.areatemp2))) %>% 
  dplyr::group_by(yymm.zarpe2) %>% 
  dplyr::summarise(Clorofila2 = mean(ClorofilaRes),
            Temperatura2 = mean(TemperaturaRes),
            MagViento2 = mean(MagVientoRes),
            IAR2 = mean(IARRes),
            Registros2 = mean(RegistrosRes)) %>% 
  dplyr::ungroup() %>% 
  # escalando y centrando los datos
  dplyr::mutate(Clorofila = as.numeric(scale(Clorofila2, center = TRUE)),
         Temperatura = as.numeric(scale(Temperatura2, center = TRUE)),
         MagViento = as.numeric(scale(MagViento2, center = TRUE)),
         IAR = as.numeric(scale(IAR2, center = TRUE)),
         Registros = as.numeric(scale(Registros2, center = TRUE))) %>% 
  dplyr::select(yymm.zarpe2, Clorofila, Temperatura, MagViento, IAR, Registros)

# Medias reales por mes
emp.mean.temp <- datosTodos2 %>%
  dplyr::group_by(yymm.zarpe2) %>% 
  # media espacial empirica por mes
  dplyr::summarise(Clorofila = mean(Chlorophylla),
            Temperatura = mean(SST),
            MagViento = mean(Wind.Magnitude),
            IAR = mean(IAR),
            # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
            Registros = n()) %>% 
  dplyr::ungroup() #%>% 
# arrange(desc(CPUE))

# write.table(emp.mean.temp, paste(wd.analysis,"/output_auxiliar_mes.txt", sep = "/"),
#             row.names = FALSE)

# re-estructurando el dataset a formato largo
emp.mean.temp.long.res <- mean.temp.res.area.scaled %>% 
  gather(key = "Variable", value = "Valor", -1)


jpeg("lineplot_promedio_x_mes_residuos_sin_area.jpeg",
     width = 15, height = 10, units = "in", res = 300)

ggplot(data = emp.mean.temp.long.res) +
  geom_line(aes(x = yymm.zarpe2, y = Valor, color = Variable), size = 1.5) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 month")) +
  ggtitle("") +
  xlab("Mes") + ylab("Variable (estandarizada)") +
  scale_color_manual(labels = c("Clorofila", "IAR", "MagViento", "Registros", "Temperatura"), 
                     values = c("seagreen", "black", "royalblue", "palevioletred3", "orange3")) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title = element_text(size = 13.5),
        strip.text = element_text(size = 13.5),
        legend.text = element_text(size = 13.5), 
        title = element_text(size = 15), 
        legend.position = "bottom")

dev.off()

# Grafico de valores originales
# formato largo
emp.mean.temp.long <- emp.mean.temp %>% 
  dplyr::mutate(Clorofila = as.numeric(scale(emp.mean.temp$Clorofila, center = TRUE)),
         Temperatura = as.numeric(scale(emp.mean.temp$Temperatura, center = TRUE)),
         MagViento = as.numeric(scale(emp.mean.temp$MagViento, center = TRUE)),
         IAR = as.numeric(scale(emp.mean.temp$IAR, center = TRUE)),
         Registros = as.numeric(scale(emp.mean.temp$Registros, center = TRUE))) %>%
  # dplyr::select(area, Clorofila2, Temperatura2, MagViento2, CPUE2) %>%
  gather(key = "Variable", value = "Valor", -1)

# ordenar el mes por CPUE descendentemente
temp.cpue.desc <- emp.mean.temp %>% 
  arrange(desc(IAR)) %>% 
  dplyr::select(yymm.zarpe2) %>% 
  distinct() %>% 
  pull()

## grafico
emp.mean.temp.long %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("IAR", "Clorofila", "MagViento",
                                      "Temperatura", "Registros")),
         yymm.zarpe = factor(yymm.zarpe2, levels = rev(temp.cpue.desc))) %>% 
  ggplot(aes(Variable, yymm.zarpe2)) +
  geom_tile(aes(fill = Valor)) + 
  # geom_text(aes(label = round(Valor, 1)), size = 3) +
  scale_fill_gradient2(low = "royalblue", high = "tomato3", mid = "khaki1",
                       midpoint = 1) +
  ylab("Mes") +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))


#### grafico hovmoller de la magnitud del viento ####

# data agrupada del mes y longitud
hovmoller.long.windmag <- windmag.areas.long %>% 
  left_join(cod.area[,c("cod.area", "lon.middegree", "lat.middegree",
                        "lon.mindegree", "lat.mindegree",
                        "lon.maxdegree", "lat.maxdegree")], 
            by = c("cod.area")) %>%  
  dplyr::mutate(yymm.windmag2 = as_date(paste(yymm.sst, "1", sep = "-"))) %>% 
  dplyr::group_by(lon.mindegree, lon.maxdegree, yymm.windmag2) %>% 
  dplyr::summarise(Monthly.WindMag = mean(Monthly.WindMag)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yymm.windmag3 = add_with_rollback(yymm.windmag2, months(1))) %>% 
  na.omit
head(hovmoller.long.windmag)

# data agrupada del mes y latitud para las variables numericas
hovmoller.lat.windmag <- windmag.areas.long %>% 
  left_join(cod.area[,c("cod.area", "lon.middegree", "lat.middegree",
                        "lon.mindegree", "lat.mindegree",
                        "lon.maxdegree", "lat.maxdegree")], 
            by = c("cod.area")) %>%  
  dplyr::mutate(yymm.windmag2 = as_date(paste(yymm.sst, "1", sep = "-"))) %>% 
  dplyr::group_by(lat.mindegree, lat.maxdegree, yymm.windmag2) %>% 
  dplyr::summarise(Monthly.WindMag = mean(Monthly.WindMag)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yymm.windmag3 = add_with_rollback(yymm.windmag2, months(1))) %>% 
  na.omit
head(hovmoller.lat.windmag)

# graficar hovmoller 
hovlong.plot.wind <- ggplot(hovmoller.long.windmag) +
  geom_rect(aes(xmin = lon.mindegree, ymin = yymm.windmag2,
                xmax = lon.maxdegree, ymax = yymm.windmag3, 
                fill = Monthly.WindMag)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Viento (m/s)", limits = c(1,8), breaks = c(1, 4.5, 8)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # scale_x_continuous(limits = c(-81.5, -79.8), breaks = seq(-81.5, -79.8, by = 0.5)) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot.wind <- ggplot(hovmoller.lat.windmag) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.windmag2,
                xmax = lat.maxdegree, ymax = yymm.windmag3, 
                fill = Monthly.WindMag)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Viento (m/s)", limits = c(1,8), breaks = c(1, 4.5, 8)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # scale_x_continuous(limits = c(-81.5, -79.8), breaks = seq(-81.5, -79.8, by = 0.5)) +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

multi.page.wind <- ggarrange(hovlong.plot.wind, hovlat.plot.wind,
                        nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                       "Mes vs Latitud"),
                        common.legend = TRUE, legend = "bottom",
                        font.label = list(size = 15))

jpeg("WindMag_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page.wind, top = text_grob("Hovmoller plot", 
                                            face = "bold", size = 16))
dev.off()


#### grafico hovmoller de la temperatura ####

# data agrupada del mes y longitud
hovmoller.long.sst <- SST.area %>% 
  left_join(cod.area[,c("cod.area", "lon.middegree", "lat.middegree",
                        "lon.mindegree", "lat.mindegree",
                        "lon.maxdegree", "lat.maxdegree")], 
            by = c("cod.area")) %>%  
  dplyr::mutate(yymm.sst2 = as_date(paste(yymm.sst, "1", sep = "-"))) %>% 
  dplyr::group_by(lon.mindegree, lon.maxdegree, yymm.sst2) %>% 
  dplyr::summarise(Monthly.SST = mean(Monthly.SST)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yymm.sst3 = add_with_rollback(yymm.sst2, months(1))) %>% 
  na.omit
head(hovmoller.long.sst)

# data agrupada del mes y latitud para las variables numericas
hovmoller.lat.sst <- SST.area %>% 
  left_join(cod.area[,c("cod.area", "lon.middegree", "lat.middegree",
                        "lon.mindegree", "lat.mindegree",
                        "lon.maxdegree", "lat.maxdegree")], 
            by = c("cod.area")) %>%  
  dplyr::mutate(yymm.sst2 = as_date(paste(yymm.sst, "1", sep = "-"))) %>% 
  dplyr::group_by(lat.mindegree, lat.maxdegree, yymm.sst2) %>% 
  dplyr::summarise(Monthly.SST = mean(Monthly.SST)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(yymm.sst3 = add_with_rollback(yymm.sst2, months(1))) %>% 
  na.omit
head(hovmoller.lat.sst)


# plot hovmoller long
hovlong.plot.sst <- ggplot(hovmoller.long.sst) +
  geom_rect(aes(xmin = lon.mindegree, ymin = yymm.sst2,
                xmax = lon.maxdegree, ymax = yymm.sst3, 
                fill = Monthly.SST)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Temp (ºC)", limits = c(18,31), breaks = c(18, 24.5, 31)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot.sst <- ggplot(hovmoller.lat.sst) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.sst2,
                xmax = lat.maxdegree, ymax = yymm.sst3, 
                fill = Monthly.SST)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Temp (ºC)", limits = c(18,31), breaks = c(18, 24.5, 31)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year"), position = "right") +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

multi.page.sst <- ggarrange(hovlong.plot.sst, hovlat.plot.sst,
                            nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                           "Mes vs Latitud"),
                            common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 15))

jpeg("Temperature_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page.sst, top = text_grob("Hovmoller plot", 
                                                face = "bold", size = 16))
dev.off()


#### grafico hovmoller de la IAR ####

# data agrupada del mes y longitud - rehacer el grafico con windmag.areas.long
hovmoller.long.IAR <- datosTodos2 %>%
  dplyr::group_by(long.meddegree, long.mindegree, long.maxdegree, yymm.zarpe2) %>% 
  dplyr::summarise(Clorofila = mean(Chlorophylla),
                   Temperatura = mean(SST),
                   MagViento = mean(Wind.Magnitude),
                   IAR = mean(IAR),
                   # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
                   Registros = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(long.mindegree, long.maxdegree, yymm.zarpe2, 
                IAR) %>% 
  # gather(key = "Variable", value = "Valor", -c(1:4)) %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.long.IAR)

# data agrupada del mes y latitud para las variables numericas
hovmoller.lat.IAR <- datosTodos2 %>%
  dplyr::group_by(lat.meddegree, lat.mindegree, lat.maxdegree, yymm.zarpe2) %>% 
  # media espacial empirica por locacion
  dplyr::summarise(Clorofila = mean(Chlorophylla),
                   Temperatura = mean(SST),
                   MagViento = mean(Wind.Magnitude),
                   IAR = mean(IAR),
                   # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
                   Registros = n()) %>% 
  dplyr::ungroup() %>%
  dplyr::select(lat.mindegree, lat.maxdegree, yymm.zarpe2, 
                IAR) %>%
  # gather(key = "Variable", value = "Valor", -c(1:4)) %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.lat.IAR)


# plot hovmoller long
hovlong.plot.IAR <- ggplot(hovmoller.long.IAR) +
  geom_rect(aes(xmin = long.mindegree, ymin = yymm.zarpe2,
                xmax = long.maxdegree, ymax = yymm.zarpe3, 
                fill = IAR)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "IAR", limits = c(0,1), breaks = c(0, 0.5, 1)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # scale_x_continuous(limits = c(-81.5, -79.8), breaks = seq(-81.5, -79.8, by = 0.5)) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot.IAR <- ggplot(hovmoller.lat.IAR) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.zarpe2,
                xmax = lat.maxdegree, ymax = yymm.zarpe3, 
                fill = IAR)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "IAR", limits = c(0,1), breaks = c(0, .5, 1)) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  # scale_y_date(
  #    minor_breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
  #                                        to = as.Date("2022-05-01"), 
  #                                        by = "1 month")) +
  # scale_x_continuous(limits = c(-81.5, -79.8), breaks = seq(-81.5, -79.8, by = 0.5)) +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

multi.page.IAR <- ggarrange(hovlong.plot.IAR, hovlat.plot.IAR,
                        nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                       "Mes vs Latitud"),
                        common.legend = TRUE, legend = "bottom",
                        font.label = list(size = 15))

jpeg("IAR_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page.IAR, top = text_grob("Hovmoller plot", 
                                            face = "bold", size = 16))
dev.off()

#### grafico hovmoller de la CPUE ####

# data agrupada del mes y longitud - rehacer el grafico con windmag.areas.long
hovmoller.long.cpue <- datosTodos2 %>%
  dplyr::group_by(long.meddegree, long.mindegree, long.maxdegree, yymm.zarpe2) %>% 
  dplyr::summarise(
    Clorofila = mean(Chlorophylla),
    Temperatura = mean(SST),
    MagViento = mean(Wind.Magnitude),
    CPUE = mean(CPUE),
    # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
    Registros = n()
  ) %>% 
  dplyr::ungroup() %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.long.cpue)

# data agrupada del mes y latitud para las variables numericas
hovmoller.lat.cpue <- datosTodos2 %>%
  dplyr::group_by(lat.meddegree, lat.mindegree, lat.maxdegree, yymm.zarpe2) %>% 
  dplyr::summarise(
    Clorofila = mean(Chlorophylla),
    Temperatura = mean(SST),
    MagViento = mean(Wind.Magnitude),
    CPUE = mean(CPUE),
    # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
    Registros = n()
  ) %>% 
  dplyr::ungroup() %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.lat.cpue)


# plot hovmoller long
hovlong.plot.cpue <- ggplot(hovmoller.long.cpue) +
  geom_rect(aes(xmin = long.mindegree, ymin = yymm.zarpe2,
                xmax = long.maxdegree, ymax = yymm.zarpe3, 
                fill = CPUE)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "CPUE (ton/h)", limits = c(0,785)
                      #, breaks = c(18, 24.5, 31)
  ) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot.cpue <- ggplot(hovmoller.lat.cpue) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.zarpe2,
                xmax = lat.maxdegree, ymax = yymm.zarpe3, 
                fill = CPUE)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "CPUE (ton/h)", limits = c(0,785)
                      #, breaks = c(18, 24.5, 31)
  ) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

multi.page.cpue <- ggarrange(hovlong.plot.cpue, hovlat.plot.cpue,
                             nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                            "Mes vs Latitud"),
                             common.legend = TRUE, legend = "bottom",
                             font.label = list(size = 15))

jpeg("CPUE_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page.cpue, top = text_grob("Hovmoller plot", 
                                                 face = "bold", size = 16))
dev.off()


#### grafico hovmoller de la Registros ####

# data agrupada del mes y longitud - rehacer el grafico con windmag.areas.long
hovmoller.long.registros <- datosTodos2 %>%
  dplyr::group_by(long.meddegree, long.mindegree, long.maxdegree, yymm.zarpe2) %>% 
  dplyr::summarise(
    Clorofila = mean(Chlorophylla),
    Temperatura = mean(SST),
    MagViento = mean(Wind.Magnitude),
    CPUE = mean(CPUE),
    # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
    Registros = n()
  ) %>% 
  dplyr::ungroup() %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.long.registros)

# data agrupada del mes y latitud para las variables numericas
hovmoller.lat.registros <- datosTodos2 %>%
  dplyr::group_by(lat.meddegree, lat.mindegree, lat.maxdegree, yymm.zarpe2) %>% 
  dplyr::summarise(
    Clorofila = mean(Chlorophylla),
    Temperatura = mean(SST),
    MagViento = mean(Wind.Magnitude),
    CPUE = mean(CPUE),
    # registros corresponde a la cantidad de veces que se ha registrado captura en dicho área
    Registros = n()
  ) %>% 
  dplyr::ungroup() %>% 
  # adicionar un mes a la grilla para mejor visualizacion
  dplyr::mutate(yymm.zarpe3 = add_with_rollback(yymm.zarpe2, months(1)))
head(hovmoller.lat.registros)


# plot hovmoller long
hovlong.plot.registros <- ggplot(hovmoller.long.registros) +
  geom_rect(aes(xmin = long.mindegree, ymin = yymm.zarpe2,
                xmax = long.maxdegree, ymax = yymm.zarpe3, 
                fill = Registros)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Registros", limits = c(0,50)
                      #, breaks = c(18, 24.5, 31)
  ) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  ggtitle("") +
  xlab("Longitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

hovlat.plot.registros <- ggplot(hovmoller.lat.registros) +
  geom_rect(aes(xmin = lat.mindegree, ymin = yymm.zarpe2,
                xmax = lat.maxdegree, ymax = yymm.zarpe3, 
                fill = Registros)) +
  scale_fill_gradient(low = "lightskyblue", high = "springgreen3", 
                      name = "Registros", limits = c(0,50)
                      #, breaks = c(18, 24.5, 31)
  ) +
  scale_y_date(breaks = function(x) seq.Date(from = as.Date("2017-01-01"), 
                                             to = as.Date("2022-05-01"), 
                                             by = "1 year")) +
  ggtitle("") +
  xlab("Latitud") + ylab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12))

multi.page.registros <- ggarrange(hovlong.plot.registros, hovlat.plot.registros,
                                  nrow = 1, ncol = 2, labels = c("Mes vs. Longitud",
                                                                 "Mes vs Latitud"),
                                  common.legend = TRUE, legend = "bottom",
                                  font.label = list(size = 15))

jpeg("Registros_HovmollerPlot.jpeg",
     width = 15, height = 10, units = "in", res = 300)
annotate_figure(multi.page.registros, top = text_grob("Hovmoller plot", 
                                                      face = "bold", size = 16))
dev.off()


######### Division de la muestra #########
# separando la variable de respuesta

# para clasificacion
outputsY.Class2 <- datosTodos2$bondad.capturaPesca2 # Categoria 2
# outputsY.Class3 <- datosTodos2$bondad.captura3.GRAL # Categoria 3

table(outputsY.Class2)
# table(outputsY.Class3)

# equilibrar las categorias
equilibrar.class2 <- sample(x = which(outputsY.Class2==0), 
                            size = max(table(outputsY.Class2))-min(table(outputsY.Class2)))

outputsY.Class2EQ <- outputsY.Class2[-equilibrar.class2]
table(outputsY.Class2EQ)

# Para regresión
# outputsY <- datosTodos2$sqrtIAR
outputsY <- datosTodos2$sqrtIAR#sqrt(datosTodos2$PescaXlance) # datosTodos2$sqrtIAR#
#log(ifelse(datosTodos2$PescaXlance==0, 0.0001, datosTodos2$PescaXlance))

# muestras de entrenamiento, validacion y prueba - Tomaremos los últimos dado que son temporales

# En todas las ejecuciones, usaremos el primer 50% de datos para la data entrenamiento
# el siguiente 25% se usara en la muestra de validacion y
# el ultimo 25% se usara en la muestra de prueba
# de modo que, las muestras de validacion y prueba sean las mas recientes

#### ejemplos para Data de validacion
set.seed(semilla)
val.cases2 <- sample(index(datosTodos2), size = floor(0.2*nrow(datosTodos2)))

val.cases2 <- which(datosTodos2$yymm.zarpe %in% 
                      c("2021-6", "2021-7", "2021-8", "2021-9","2021-10", "2021-11"))
#val.cases2[!(val.cases2 %in% equilibrar.class2)]
length(val.cases2) # 374 ejemplos
length(val.cases2)/dim(datosTodos2)[1] # porcentaje 20.16%

#### ejemplos para Data de prueba
set.seed(semilla)
test.cases2 <- sample(which(!(index(datosTodos2) %in% val.cases2)),
                      size = floor(0.2*nrow(datosTodos2)))
test.cases2 <- which(datosTodos2$yymm.zarpe %in% 
                       c("2021-12", "2022-1", "2022-2", "2022-3", "2022-4", "2022-5"))
length(test.cases2) # 380 ejemplos
length(test.cases2)/dim(datosTodos2)[1] # porcentaje 20.48%


##### solo para clasificacion equilibrada
datosTodos3 <- datosTodos2[-equilibrar.class2,]
set.seed(semilla)
val.cases2.Class <- sample(index(datosTodos3), size = floor(0.2*nrow(datosTodos3)))
val.cases2.Class <- which(datosTodos3$yymm.zarpe %in% 
                            c("2021-6", "2021-7", "2021-8", "2021-9","2021-10", "2021-11"))
set.seed(semilla)
test.cases2.Class <- sample(which(!(index(datosTodos3) %in% val.cases2)),
                            size = floor(0.2*nrow(datosTodos3)))
test.cases2.Class <- which(datosTodos3$yymm.zarpe %in% 
                             c("2021-12", "2022-1", "2022-2", "2022-3", "2022-4", "2022-5"))

##### ejemplos para Data de entrenamiento

######### seleccion de los datos de entrada #########
inputs <- datosTodos2[, c("mes", "SST","Wind.Magnitude", "Chlorophylla",
                          "lat.dd", "long.dd")] #"lat.meddegree", "long.meddegree"

## solo para clasificacion que requiere equilibrar las clases
inputsClass <- datosTodos3[, c("mes", "SST","Wind.Magnitude", "Chlorophylla",
                               "lat.dd", "long.dd")] #"lat.meddegree", "long.meddegree"

### en la matriz de entradas
trainX.orig2 <- inputs[-c(test.cases2, val.cases2),] # 59.35% - 1088 ejemplos 
testX.orig2 <- inputs[test.cases2,] # 20.48% - 375 ejemplos 
valX.orig2 <- inputs[val.cases2,] # 20.16% - 373 ejemplos  

# removiendo del inputs para Clasificacion - No ejecutar si es regresion
trainX.orig2Class <- inputsClass[-c(test.cases2.Class, val.cases2.Class),] # 59.49% - 984 ejemplos 
testX.orig2Class <- inputsClass[test.cases2.Class,] # 20.07% - 332 ejemplos 
valX.orig2Class <- inputsClass[val.cases2.Class,] # 20.43% - 338 ejemplos  

### en los vectores de respuesta
## Para clasificación

# Categorias 2
# trainYClass2.orig2 <- outputsY.Class2[-c(test.cases2, val.cases2)]
# testYClass2.orig2 <- outputsY.Class2[test.cases2]
# valYClass2.orig2 <- outputsY.Class2[val.cases2]

# clasificacion que requiere ser equilibrada
trainYClass2.orig2 <- outputsY.Class2EQ[-c(test.cases2.Class, val.cases2.Class)]
testYClass2.orig2 <- outputsY.Class2EQ[test.cases2.Class]
valYClass2.orig2 <- outputsY.Class2EQ[val.cases2.Class]


table(trainYClass2.orig2)
#    0   1 
#  493 491  

table(testYClass2.orig2)
# 0   1 
# 144 187 

table(valYClass2.orig2)
# 0   1 
# 190 149

## Para Regresión
trainY.orig2 <- outputsY[-c(test.cases2, val.cases2)]
testY.orig2 <- outputsY[test.cases2]
valY.orig2 <- outputsY[val.cases2]

############### Escalar con min-max inputs en funcion de info de train sample ################

# medidas de train sample
minimos.trainX <- c(apply(trainX.orig2[,-c(5,6)], 2, min), 
                    min(-3.583333333, min(trainX.orig2$lat.dd)),
                    min(-81.41666667, min(trainX.orig2$long.dd)))
maximos.trainX <- c(apply(trainX.orig2[,-c(5,6)], 2, max),
                    max(1.416666667, max(trainX.orig2$lat.dd)),
                    max(-79.91666667, max(trainX.orig2$long.dd)))
rangos.trainX <- maximos.trainX - minimos.trainX

# muestras escaladas - inputs
trainX <- as.matrix((trainX.orig2 - matrix(minimos.trainX, nrow = nrow(trainX.orig2), byrow = TRUE,
                                           ncol = ncol(trainX.orig2)))/matrix(rangos.trainX, nrow = nrow(trainX.orig2), byrow = TRUE,
                                                                              ncol = ncol(trainX.orig2)))
testX <- as.matrix((testX.orig2 - matrix(minimos.trainX, nrow = nrow(testX.orig2), byrow = TRUE,
                                         ncol = ncol(testX.orig2)))/matrix(rangos.trainX, 
                                                                           nrow = nrow(testX.orig2), 
                                                                           byrow = TRUE,
                                                                           ncol = ncol(testX.orig2)))
valX <- as.matrix((valX.orig2 - matrix(minimos.trainX, nrow = nrow(valX.orig2), byrow = TRUE,
                                       ncol = ncol(valX.orig2)))/matrix(rangos.trainX, 
                                                                        nrow = nrow(valX.orig2), 
                                                                        byrow = TRUE,
                                                                        ncol = ncol(valX.orig2)))

summary(trainX)
summary(testX)
summary(valX)

### para clasificacion que requiere equilibrar
# medidas de train sample
minimos.trainXClass <- c(apply(trainX.orig2Class[,-c(5,6)], 2, min),
                         min(-3.583333333, min(trainX.orig2Class$lat.dd)),
                         min(-81.41666667, min(trainX.orig2Class$long.dd)))
maximos.trainXClass <- c(apply(trainX.orig2Class[,-c(5,6)], 2, max),
                         max(1.416666667, max(trainX.orig2Class$lat.dd)),
                         max(-79.91666667, max(trainX.orig2Class$long.dd)))
rangos.trainXClass <- maximos.trainXClass - minimos.trainXClass

# muestras escaladas - inputs
trainXClass <- as.matrix((trainX.orig2Class - matrix(minimos.trainXClass, nrow = nrow(trainX.orig2Class), byrow = TRUE,
                                                     ncol = ncol(trainX.orig2Class)))/matrix(rangos.trainXClass, nrow = nrow(trainX.orig2Class), byrow = TRUE,
                                                                                             ncol = ncol(trainX.orig2Class)))
testXClass <- as.matrix((testX.orig2Class - matrix(minimos.trainXClass, nrow = nrow(testX.orig2Class), byrow = TRUE,
                                                   ncol = ncol(testX.orig2Class)))/matrix(rangos.trainXClass, 
                                                                                          nrow = nrow(testX.orig2Class), 
                                                                                          byrow = TRUE,
                                                                                          ncol = ncol(testX.orig2Class)))
valXClass <- as.matrix((valX.orig2Class - matrix(minimos.trainXClass, nrow = nrow(valX.orig2Class), byrow = TRUE,
                                                 ncol = ncol(valX.orig2Class)))/matrix(rangos.trainXClass, 
                                                                                       nrow = nrow(valX.orig2Class), 
                                                                                       byrow = TRUE,
                                                                                       ncol = ncol(valX.orig2Class)))


summary(trainXClass)
summary(testXClass)
summary(valXClass)

## Para Regresión
trainY <- trainY.orig2
testY <- testY.orig2
valY <- valY.orig2

# ## Para Clasificación
# # Categorias 2
trainY2.Class <- trainYClass2.orig2
testY2.Class <- testYClass2.orig2
valY2.Class <- valYClass2.orig2

# estandarizar toda la muestra basada en los parametro de la de entrenamiento
inputs.scaled <- as.matrix((inputs - matrix(minimos.trainX, nrow = nrow(inputs), byrow = TRUE,
                                            ncol = ncol(inputs)))/matrix(rangos.trainX, nrow = nrow(inputs), byrow = TRUE,
                                                                         ncol = ncol(inputs)))

############### Escalar data prediccion para mapas ##############

## Enero
inputs1.predClass <- (as.matrix(datapred1[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred1[,-1]),
                                                         ncol = ncol(datapred1[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred1[,-1]),
                                                                               ncol = ncol(datapred1[,-1]),
                                                                               byrow = TRUE)

inputs1.pred <- (as.matrix(datapred1[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred1[,-1]),
                                                    ncol = ncol(datapred1[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred1[,-1]),
                                                                          ncol = ncol(datapred1[,-1]),
                                                                          byrow = TRUE)
## Febrero
inputs2.predClass <- (as.matrix(datapred2[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred2[,-1]),
                                                         ncol = ncol(datapred2[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred2[,-1]),
                                                                               ncol = ncol(datapred2[,-1]),
                                                                               byrow = TRUE)

inputs2.pred <- (as.matrix(datapred2[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred2[,-1]),
                                                    ncol = ncol(datapred2[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred2[,-1]),
                                                                          ncol = ncol(datapred2[,-1]),
                                                                          byrow = TRUE)

## Abril
inputs4.predClass <- (as.matrix(datapred4[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred4[,-1]),
                                                         ncol = ncol(datapred4[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred4[,-1]),
                                                                               ncol = ncol(datapred4[,-1]),
                                                                               byrow = TRUE)

inputs4.pred <- (as.matrix(datapred4[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred4[,-1]),
                                                    ncol = ncol(datapred4[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred4[,-1]),
                                                                          ncol = ncol(datapred4[,-1]),
                                                                          byrow = TRUE)


## Mayo
inputs5.predClass <- (as.matrix(datapred5[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred5[,-1]),
                                                         ncol = ncol(datapred5[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred5[,-1]),
                                                                               ncol = ncol(datapred5[,-1]),
                                                                               byrow = TRUE)

inputs5.pred <- (as.matrix(datapred5[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred5[,-1]),
                                                    ncol = ncol(datapred5[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred5[,-1]),
                                                                          ncol = ncol(datapred5[,-1]),
                                                                          byrow = TRUE)

## Junio
inputs6.predClass <- (as.matrix(datapred6[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred6[,-1]),
                                                         ncol = ncol(datapred6[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred6[,-1]),
                                                                               ncol = ncol(datapred6[,-1]),
                                                                               byrow = TRUE)

inputs6.pred <- (as.matrix(datapred6[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred6[,-1]),
                                                    ncol = ncol(datapred6[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred6[,-1]),
                                                                          ncol = ncol(datapred6[,-1]),
                                                                          byrow = TRUE)

## Julio
inputs7.predClass <- (as.matrix(datapred7[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred7[,-1]),
                                                         ncol = ncol(datapred7[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred7[,-1]),
                                                                               ncol = ncol(datapred7[,-1]),
                                                                               byrow = TRUE)

inputs7.pred <- (as.matrix(datapred7[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred7[,-1]),
                                                    ncol = ncol(datapred7[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred7[,-1]),
                                                                          ncol = ncol(datapred7[,-1]),
                                                                          byrow = TRUE)

## Agosto
inputs8.predClass <- (as.matrix(datapred8[,-1]) - matrix(minimos.trainXClass, 
                                                         nrow = nrow(datapred8[,-1]),
                                                         ncol = ncol(datapred8[,-1]),
                                                         byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred8[,-1]),
                                                                               ncol = ncol(datapred8[,-1]),
                                                                               byrow = TRUE)

inputs8.pred <- (as.matrix(datapred8[,-1]) - matrix(minimos.trainX, 
                                                    nrow = nrow(datapred8[,-1]),
                                                    ncol = ncol(datapred8[,-1]),
                                                    byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred8[,-1]),
                                                                          ncol = ncol(datapred8[,-1]),
                                                                          byrow = TRUE)

## Octubre
inputs10.predClass <- (as.matrix(datapred10[,-1]) - matrix(minimos.trainXClass, 
                                                           nrow = nrow(datapred10[,-1]),
                                                           ncol = ncol(datapred10[,-1]),
                                                           byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred10[,-1]),
                                                                                 ncol = ncol(datapred10[,-1]),
                                                                                 byrow = TRUE)

inputs10.pred <- (as.matrix(datapred10[,-1]) - matrix(minimos.trainX, 
                                                      nrow = nrow(datapred10[,-1]),
                                                      ncol = ncol(datapred10[,-1]),
                                                      byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred10[,-1]),
                                                                            ncol = ncol(datapred10[,-1]),
                                                                            byrow = TRUE)

## Noviembre
inputs11.predClass <- (as.matrix(datapred11[,-1]) - matrix(minimos.trainXClass, 
                                                           nrow = nrow(datapred11[,-1]),
                                                           ncol = ncol(datapred11[,-1]),
                                                           byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred11[,-1]),
                                                                                 ncol = ncol(datapred11[,-1]),
                                                                                 byrow = TRUE)

inputs11.pred <- (as.matrix(datapred11[,-1]) - matrix(minimos.trainX, 
                                                      nrow = nrow(datapred11[,-1]),
                                                      ncol = ncol(datapred11[,-1]),
                                                      byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred11[,-1]),
                                                                            ncol = ncol(datapred11[,-1]),
                                                                            byrow = TRUE)

## Diciembre
inputs12.predClass <- (as.matrix(datapred12[,-1]) - matrix(minimos.trainXClass, 
                                                           nrow = nrow(datapred12[,-1]),
                                                           ncol = ncol(datapred12[,-1]),
                                                           byrow = TRUE))/matrix(rangos.trainXClass, nrow = nrow(datapred12[,-1]),
                                                                                 ncol = ncol(datapred12[,-1]),
                                                                                 byrow = TRUE)

inputs12.pred <- (as.matrix(datapred12[,-1]) - matrix(minimos.trainX, 
                                                      nrow = nrow(datapred12[,-1]),
                                                      ncol = ncol(datapred12[,-1]),
                                                      byrow = TRUE))/matrix(rangos.trainX, nrow = nrow(datapred12[,-1]),
                                                                            ncol = ncol(datapred12[,-1]),
                                                                            byrow = TRUE)

######### Analisis de Redes Neuronales #########

## Analizaremos dos opciones: Un modelo de regresion y uno de clasificacion (binaria)

######### ANN: Problema de regresion #########

sink(paste(wd.analysis, "ANN_Salida_ModeloRegresion_Iteracion10.txt", sep = "/"))

##### 2 capas ocultas con 32 y 7 neuronas cada uno - ReLu, ReLu, Lineal #####

## construir el modelo
# modelo secuencial y adicion de capas ocultas
set.seed(semilla)
modelMLP18 <- keras_model_sequential()
modelMLP18 %>% layer_dense(units = 32, activation = 'relu', input_shape = c(6)) %>% 
  # layer_dropout(0.5) %>% 
  layer_dense(units = 7, activation = 'relu') %>%
  # layer_dropout(0.5) %>%
  # layer_dense(units = 7, activation = 'relu') %>%
  # layer_dropout(0.5) %>%
  layer_dense(1)
  # El modelo finaliza con una unidad y sin funcion de activacion porque el resultado es lineal. 

summary(modelMLP18)

## Compilar el modelo
modelMLP18 %>% compile(
  optimizer = 'rmsprop', # optimizador RMSprop
  # El optimizador RMSprop es similar al algoritmo del gradiente descendiente con momemtum 
  # El optimizador restringe las oscilaciones qe ocurren verticalmente
  # Asi, se puede incrementar la tasa de aprendizaje para que el algoritmo tome pasos mas largos
  # al hacerlo en direccion horizontal, para que a su vez converja mas rapido
  loss = 'mse', # mean squared error (MSE), que es el cuadrado de la diferencia
  # entre los valores predichos y los observados
  metrics = 'mae' #  mean absolute error (MAE). Es el valor absoluto de la diferencia
  # entre los valores predichos y los observados
)

## Ajustar el modelo
set.seed(semilla)
fitMLP18 <- modelMLP18 %>%
  keras::fit(trainX, trainY,
             epochs = 50,
             batch_size = 32, #128
             #validation_split = 0.2
             validation_data = list(valX, valY)
  )

#save_model_tf(modelMLP18, wd.analysis)

## Resultado de la ultima epoca
# Loss- MSE para muestra de entrenamiento
fitMLP18$metrics$loss[100] # 0.03617381
# Loss - MSE para muestra de validacion
fitMLP18$metrics$val_loss[100] # 1.703977
# MAE para muestra de entrenamiento
fitMLP18$metrics$mae[100] # 1.027161
# MAE para muestra de validacion
fitMLP18$metrics$val_mae[100] # 1.037302

ggpMLP18 <- plot(fitMLP18)

jpeg("ANN_MLP18_2hidden327_ValidationPerformance_Iter1.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggpMLP18 + theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom")
dev.off()

## Evaluar el modelo
set.seed(semilla)
modelMLP18 %>% evaluate(testX, testY)
#      loss     MAE
# 0.03294519 0.14187633   # RMSE<- sqrt(MSE) = 0.1815081

predichosMLP18 <- data.frame(Predichos = modelMLP18 %>% #predict(testX, batch_size = 32),
                               predict(testX), 
                             Reales = testY # testY
)

predichosMLP18$Error <- predichosMLP18$Predichos - predichosMLP18$Reales
predichosMLP18$IARReales <- predichosMLP18$Reales^2
predichosMLP18$IARPredichos <- predichosMLP18$Predichos^2
predichosMLP18$IARError <- predichosMLP18$IARPredichos - predichosMLP18$IARReales
summary(predichosMLP18$IARError)

# predichosMLP18test.history <- predichosMLP18$Predichos
predichosMLP18test.history <- cbind(predichosMLP18test.history, predichosMLP18$Predichos)

# errorMLP18test.history <- predichosMLP18$Error
errorMLP18test.history <- cbind(errorMLP18test.history, predichosMLP18$Error)

# plot(predichosMLP18$Error~index(predichosMLP18))
# abline(h = 0)
summary(predichosMLP18$Predichos)
summary(predichosMLP18$Reales)
summary(predichosMLP18$Error)

jpeg("ANN_MLP18_2hidden327_Residuals_Iter10.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(predichosMLP18) + 
  geom_point(aes(x = Predichos, y = Error), alpha = 0.4, size = 3) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  xlab("Predichos") + ylab("Residuos")
dev.off()

jpeg("ANN_MLP18_2hidden327_ResidualsIAR_Iter10.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(predichosMLP18) + 
  geom_point(aes(x = IARPredichos, y = IARError), alpha = 0.4, size = 3) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  xlab("Predichos IAR") + ylab("Residuos IAR")
dev.off()

jpeg("ANN_MLP18_2hidden327_PredictedvsObserved_Iter10.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(predichosMLP18) + 
  geom_line(aes(x = index(predichosMLP18), y = Predichos, color = 'Predichos')) + #, color = "red") +
  geom_line(aes(x = index(predichosMLP18), y = Reales, color = 'Reales')) +#, color = "blue") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  scale_color_manual(name='',
                     breaks=c('Predichos', 'Reales'),
                     values=c('Predichos'='red', 'Reales'='blue')) +
  xlab("index") + ylab("sqrt(IAR)")
dev.off()

jpeg("ANN_MLP18_2hidden327_PredictedvsObservedIAR_Iter10.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(predichosMLP18) + 
  geom_line(aes(x = index(predichosMLP18), y = IARPredichos, color = 'Predichos IAR')) + #, color = "red") +
  geom_line(aes(x = index(predichosMLP18), y = IARReales, color = 'Reales IAR')) +#, color = "blue") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  scale_color_manual(name='',
                     breaks=c('Predichos IAR', 'Reales IAR'),
                     values=c('Predichos IAR'='red', 'Reales IAR'='blue')) +
  xlab("index") + ylab("IAR")
dev.off()

ggplot(datosTodos) + 
  geom_line(aes(x = index(datosTodos), y = PescaXlance)) 

# Clasificando sqrtIAR- predicciones y reales
predichosMLP18$ClassPredichos <- ifelse(predichosMLP18$Predichos <= 0.4008919, 0, 1)
predichosMLP18$ClassReales <- ifelse(predichosMLP18$Reales <= 0.4008919, 0, 1)

# Matriz de confusiones solo con la data de test
caret::confusionMatrix(data = as.factor(predichosMLP18$ClassPredichos[test.cases2]), 
                       reference = as.factor(predichosMLP18$ClassReales[test.cases2]))

sink(paste(wd.analysis, "ANN_Salida_ModeloClass_Iteracion10.txt", sep = "/"))

######### ANN: problema de clasificacion #########
########## Modelo Dense Layers ##############
##### 2 capas ocultas de 32 y 7 nodos cada una - ReLu, ReLu, Sigmoide #####

## Construir el modelo
# modelo secuencial y adicionar las capas ocultas
# 2 capas ocultas de 32 y 7 nodos ocultos, respectivamente
set.seed(semilla)
modelMLP1 <- keras_model_sequential()
modelMLP1 %>% layer_dense(units = 32, activation = 'relu', input_shape = c(6)) %>% # 32
  # layer_dropout(0.5) %>% 
  # layer_dense(units = 10, activation = 'relu') %>% # 7
  layer_dense(units = 7, activation = 'relu') %>% 
  # layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid')
summary(modelMLP1)

## Compilar el modelo
modelMLP1 %>% compile(loss ='binary_crossentropy',
                      optimizer = 'adam',
                      metrics = 'accuracy')

## Ajustar el modelo
set.seed(semilla)
fitMLP1 <- modelMLP1 %>%
  keras::fit(trainXClass, trainY2.Class,# outputY.TrainClass,
             epochs = 50,
             batch_size = 32, #128
             #validation_split = 0.2
             validation_data = list(valXClass, valY2.Class#outputY.ValClass
             ))

## Resultados de la ultima epoca
# Loss para muestra de entrenamiento
fitMLP1$metrics$loss[100] # 0.6567077
# Loss para muestra de validacion
fitMLP1$metrics$val_loss[100] # 0.6812371
# Accuracy para muestra de entrenamiento
fitMLP1$metrics$accuracy[100] # 0.6188341
# Accuracy para muestra de validacion
fitMLP1$metrics$val_accuracy[100] # 0.5855615

ggpMLP1 <- plot(fitMLP1)

jpeg("ANN_MLP1_2hidden327_Iter10_ValidationPerformance.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggpMLP1 + theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom")
dev.off()

## Evaluar el modelo
set.seed(semilla)
modelMLP1 %>% evaluate(testXClass, testY2.Class)
#      loss  accuracy 
# 0.6807991  0.5587468  

## Prediccion & matriz de confusion - test data
probMLP1 <- modelMLP1 %>%
  predict(testXClass)

predichosMLP1 <- data.frame(Probabilidad = as.numeric(probMLP1),
                            Predicho = ifelse(probMLP1 <= 0.5, 0, 1),
                            Observado = testY2.Class)

caret::confusionMatrix(data = as.factor(predichosMLP1$Predicho), 
                       reference = as.factor(predichosMLP1$Observado))


jpeg("ANN_MLP1_2hidden327_Iter10_ROC_AUC.jpeg",
     width = 15, height = 10, units = "in", res = 300)
test_roc = roc(predichosMLP1$Observado ~ predichosMLP1$Predicho, 
               plot = TRUE, print.auc = TRUE)
dev.off()
as.numeric(test_roc$auc)

predictTest.history <- cbind(predictTest.history, predichosMLP1$Predicho)
predictProbTest.history <- cbind(predictProbTest.history, predichosMLP1$Probabilidad)

probMLP1 <- modelMLP1 %>%
  predict(trainXClass)

predichosMLP1 <- data.frame(Probabilidad = as.numeric(probMLP1),
                            Predicho = ifelse(probMLP1 <= 0.5, 0, 1),
                            Observado = trainY2.Class)

probMLP1_1 <- modelMLP1 %>%
  predict(inputs1.predClass)

table(ifelse(probMLP1_1 <= 0.5, 0, 1))/nrow(inputs1.predClass)

############ Predicciones para mapas ################

# Enero
probMLP1_Mes01Iter10 <- modelMLP1 %>%
  predict(inputs1.predClass)
summary(probMLP1_Mes01Iter10)
clipr::write_clip(probMLP1_Mes01Iter10)

predMLP18_Mes01Iter10 <- modelMLP18 %>%
  predict(inputs1.pred)
clipr::write_clip(predMLP18_Mes01Iter10)

# Febrero
probMLP1_Mes02Iter10 <- modelMLP1 %>%
  predict(inputs2.predClass)
summary(probMLP1_Mes02Iter10)
clipr::write_clip(probMLP1_Mes02Iter10)

predMLP18_Mes02Iter10 <- modelMLP18 %>%
  predict(inputs2.pred)
clipr::write_clip(predMLP18_Mes02Iter10)

# Abril
probMLP1_Mes04Iter10 <- modelMLP1 %>%
  predict(inputs4.predClass)
summary(probMLP1_Mes04Iter10)
clipr::write_clip(probMLP1_Mes04Iter10)

predMLP18_Mes04Iter10 <- modelMLP18 %>%
  predict(inputs4.pred)
clipr::write_clip(predMLP18_Mes04Iter10)

# Mayo
probMLP1_Mes05Iter10 <- modelMLP1 %>%
  predict(inputs5.predClass)
summary(probMLP1_Mes05Iter10)
clipr::write_clip(probMLP1_Mes05Iter10)

predMLP18_Mes05Iter10 <- modelMLP18 %>%
  predict(inputs5.pred)
clipr::write_clip(predMLP18_Mes05Iter10)

# Junio
probMLP1_Mes06Iter10 <- modelMLP1 %>%
  predict(inputs6.predClass)
summary(probMLP1_Mes06Iter10)
clipr::write_clip(probMLP1_Mes06Iter10)

predMLP18_Mes06Iter10 <- modelMLP18 %>%
  predict(inputs6.pred)
clipr::write_clip(predMLP18_Mes06Iter10)

# Julio
probMLP1_Mes07Iter10 <- modelMLP1 %>%
  predict(inputs7.predClass)
summary(probMLP1_Mes07Iter10)
clipr::write_clip(probMLP1_Mes07Iter10)

predMLP18_Mes07Iter10 <- modelMLP18 %>%
  predict(inputs7.pred)
clipr::write_clip(predMLP18_Mes07Iter10)

# Agosto
probMLP1_Mes08Iter10 <- modelMLP1 %>%
  predict(inputs8.predClass)
summary(probMLP1_Mes08Iter10)
clipr::write_clip(probMLP1_Mes08Iter10)

predMLP18_Mes08Iter10 <- modelMLP18 %>%
  predict(inputs8.pred)
clipr::write_clip(predMLP18_Mes08Iter10)

# Octubre
probMLP1_Mes10Iter10 <- modelMLP1 %>%
  predict(inputs10.predClass)
summary(probMLP1_Mes10Iter10)
clipr::write_clip(probMLP1_Mes10Iter10)

predMLP18_Mes10Iter10 <- modelMLP18 %>%
  predict(inputs10.pred)
clipr::write_clip(predMLP18_Mes10Iter10)

# Noviembre
probMLP1_Mes11Iter10 <- modelMLP1 %>%
  predict(inputs11.predClass)
summary(probMLP1_Mes11Iter10)
clipr::write_clip(probMLP1_Mes11Iter10)

predMLP18_Mes11Iter10 <- modelMLP18 %>%
  predict(inputs11.pred)
clipr::write_clip(predMLP18_Mes11Iter10)

# Diciembre
probMLP1_Mes12Iter10 <- modelMLP1 %>%
  predict(inputs12.predClass)
summary(probMLP1_Mes12Iter10)
clipr::write_clip(probMLP1_Mes12Iter10)

predMLP18_Mes12Iter10 <- modelMLP18 %>%
  predict(inputs12.pred)
clipr::write_clip(predMLP18_Mes12Iter10)


################## Mapas ###################

# Datos con las predicciones promedio de sqrtIAR e IAR
datosPredichos <- read.csv(paste(wd.bds,"PrediccionPromedioMes.csv", sep = "/"), header = TRUE)
str(datosPredichos)

datosPredichos <- datosPredichos[,-2]
  
summary(datosPredichos[,c(12:21)])

# boxplots por mes de los predichos de sqrtIAR
sqrtIAR.PredMes <- datosPredichos %>% 
  dplyr::select(21:30) %>% 
  gather(key = "Mes", value = "sqrtIAR")

sqrtIAR.PredMes$MesRecod <- factor(sqrtIAR.PredMes$Mes,
                              levels = c("Ene_sqrtIAR", "Feb_sqrtIAR", "Abr_sqrtIAR",
                                         "May_sqrtIAR", "Jun_sqrtIAR", "Jul_sqrtIAR",
                                         "Ago_sqrtIAR", "Oct_sqrtIAR", "Nov_sqrtIAR",
                                         "Dic_sqrtIAR"),
                              labels = c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12))


jpeg("ANN_MLP18_2hidden327_Boxplot_sqrtIAR_PredichoMes.jpeg",
     width = 15, height = 10, units = "in", res = 300)
ggplot(sqrtIAR.PredMes, aes(x = MesRecod, y = sqrtIAR)) +
  geom_boxplot() + theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  xlab("Mes") +
  scale_y_continuous(limits = c(0,1))
dev.off()

datamap <- datosPredichos %>% 
  dplyr::select(area, lat.mindegree, lat.maxdegree, lat.middegree,
                lon.mindegree, lon.maxdegree, long.middegree, Ene_IAR,
                Feb_IAR, Abr_IAR, May_IAR, Jun_IAR, Jul_IAR,
                Ago_IAR, Oct_IAR, Nov_IAR, Dic_IAR) %>% 
  gather(key = "Mes", value = "IAR", -c(area, lat.mindegree, lat.maxdegree, lat.middegree,
                                        lon.mindegree, lon.maxdegree, long.middegree)) %>% 
  dplyr::mutate(MesNum = rep(c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12), each = 219),
                MesName = factor(MesNum, levels = c(c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12)),
                                 labels = c("Ene", "Feb", "Abr", "May", "Jun",
                                            "Jul", "Ago", "Oct", "Nov", "Dic")))

datamap$color <- ifelse(datamap$area %in% names(table(datosTodos2$area)), "En uso", "Sin uso")


areas.usadas <- datamap[which(datamap$area %in% names(table(datosTodos2$area))),]

View(datamap)

jpeg("MapaIAR_Predicho_12Diciembre.jpeg",
     width = 15, height = 10, units = "in", res = 300)

datamap %>% 
  dplyr::filter(MesNum == 12) %>% 
ggplot() +
  # facet_wrap(~MesName) + # , labeller = labeller(mm = names.months)
  geom_rect(aes(xmin = lon.mindegree, ymin = lat.mindegree,
                xmax = lon.maxdegree, ymax = lat.maxdegree, 
                fill = IAR, color = color), lwd = 1) +
  coord_quickmap() +
  scale_fill_gradient(low = "lightgoldenrod1", high = "firebrick1", 
                      name = "IAR", limits = c(0,0.3) #, breaks = c(0, 0., .5)
                      ) +
  scale_color_manual(name='',
                     breaks=c('En uso', 'Sin uso'),
                     values=c('En uso'='black', 'Sin uso'='blue')) +
  geom_text(aes(label = area, x = long.middegree, y = lat.middegree), 
            color = "black", size = 2) +
  ggtitle("Predicción del Índice de Abundancia Relativa (IAR)",
          subtitle = "Diciembre") +
  xlab("Longitud") + ylab("Latitud") + labs(fill = "IAR") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "none") 
dev.off()


#################### Otros graficos #################


jpeg("Boxplot_sqrtIAR_X_mes.jpeg",
     width = 15, height = 8, units = "in", res = 300)
ggplot(datosTodos2, aes(x = as.factor(mes), y = sqrtIAR)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12)) +
  xlab("Mes")
dev.off()

jpeg("Boxplot_IAR_X_mes.jpeg",
     width = 15, height = 8, units = "in", res = 300)
ggplot(datosTodos2, aes(x = as.factor(mes), y = IAR)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        title = element_text(size = 15),
        legend.text = element_text(size = 12)) +
  xlab("Mes")
dev.off()
