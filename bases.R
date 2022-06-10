library(dplyr)
library(readxl)

#Sección Dividendos

Archivo2022<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRE4a_Z6cvVLrc46w4L2GFhF_zEp3crU8NCEnoKx_dqIH5fqPthQd4ZXyhhEElRcmrnBR_tPMEZV2iQ/pub?gid=0&single=true&output=csv", encoding = "UTF-8")
names(Archivo2022)[7] <- "FECHA.FINAL"
names(Archivo2022)[8] <- "TOTAL"
names(Archivo2022)[9] <- "CUOTA"
names(Archivo2022)[11] <- "TOTAL.ENTREGADO.EN.DIVIDENDOS"

Archivo2022$FECHA.ASAMBLEA <- as.Date(Archivo2022$FECHA.ASAMBLEA) #Convertir tipo de dato a fecha. 
Archivo2022$FECHA.INICIAL <- as.Date(Archivo2022$FECHA.INICIAL) #Convertir tipo de dato a fecha.
Archivo2022$FECHA.FINAL <- as.Date(Archivo2022$FECHA.FINAL) #Convertir tipo de dato a fecha.
Archivo2022$CUOTA <- as.numeric(Archivo2022$CUOTA) #Convertir tipo de dato a numÃ©rico.

#Sección Precios
#Se están descargando solo las acciones que se muestran aqui, buscar la manera de desacargar el resto

ACCIONES <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT-liEK7HQCbWF6wmzVtpy5nQReZB4eEY4TiSe6vZrwQzOm3CBhW0E-AyKBEJGYMRaiHuhC7Lkcb-_q/pub?output=csv")
ACCIONES$fecha <- as.Date(ACCIONES$fecha, format = "%d/%m/%y") #Convertir tipo de dato a fecha.

df <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
                    PFBCOLOM, NUTRESA, PFGRUPSURA,ECOPETROL) #Ordenar los precios de las columnas seleccionadas en una sola columna. 

opciones <- c("PFBCOLOM", "NUTRESA", "PFGRUPSURA","ECOPETROL")

