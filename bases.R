library(dplyr)
library(readxl)

#Sección Dividendos

Archivo2020<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRG-hlb0R-wchJu9rQwx4S-nS2ioCY8RsyPigRLLH1D1scB_6_o-6VPEKD1IiRrPG_c06O3G8AZQhD/pub?output=csv", encoding = "UTF-8")
names(Archivo2020)[7] <- "FECHA.FINAL"
names(Archivo2020)[8] <- "TOTAL"
names(Archivo2020)[9] <- "CUOTA"
names(Archivo2020)[11] <- "TOTAL.ENTREGADO.EN.DIVIDENDOS"

Archivo2020$FECHA.ASAMBLEA <- as.Date(Archivo2020$FECHA.ASAMBLEA) #Convertir tipo de dato a fecha. 
Archivo2020$FECHA.INICIAL <- as.Date(Archivo2020$FECHA.INICIAL) #Convertir tipo de dato a fecha.
Archivo2020$FECHA.FINAL <- as.Date(Archivo2020$FECHA.FINAL) #Convertir tipo de dato a fecha.
Archivo2020$CUOTA <- as.numeric(Archivo2020$CUOTA) #Convertir tipo de dato a numÃ©rico.

#Sección Precios
#Se están descargando solo las acciones que se muestran aqui, buscar la manera de desacargar el resto

ACCIONES <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT-liEK7HQCbWF6wmzVtpy5nQReZB4eEY4TiSe6vZrwQzOm3CBhW0E-AyKBEJGYMRaiHuhC7Lkcb-_q/pub?output=csv")
ACCIONES$fecha <- as.Date(ACCIONES$fecha, format = "%d/%m/%y") #Convertir tipo de dato a fecha.

df <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
                    PFBCOLOM, NUTRESA, PFGRUPSURA,ECOPETROL) #Ordenar los precios de las columnas seleccionadas en una sola columna. 

opciones <- c("PFBCOLOM", "NUTRESA", "PFGRUPSURA","ECOPETROL")

