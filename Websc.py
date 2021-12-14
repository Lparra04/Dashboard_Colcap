import numpy as np 
import pandas as pd
import requests
from datetime import date
import os
import openpyxl


url = "https://www.bvc.com.co/pps/tibco/portalbvc/Home/Mercados/informesbursatiles?com.tibco.ps.pagesvc.action=updateRenderState&rp.currentDocumentID=-566aa413_16cde9c48ee_2930c0a84ca9&rp.attachmentPropertyName=Attachment&com.tibco.ps.pagesvc.targetPage=1f9a1c33_132040fa022_-78750a0a600b&com.tibco.ps.pagesvc.mode=resource&rp.redirectPage=1f9a1c33_132040fa022_-787e0a0a600b"

archivo = requests.get(url)	#requests.get permite extraer informacion sobre la pagina, ya sea texto, imagenes o archivos.
ubicacion = "C:/Users/JosePinzon/Documents/FINTRADE/Python" #Especifica carpeta o ruta para guardar

fecha = str(date.today()) #Se guarda un objeto con la fecha de hoy
nombre = fecha+".xlsx" #Se guarda un objeto con la fecha y la extecion xlsx

ruta = os.path.join(ubicacion,nombre) #Permite unir las rutas, agrega el nombre del archivo a la ruta de ubicacion

with open (ruta, "wb") as output: #permite guardar el archivo en la ubicacion especificada, "write bytes"
  output.write(archivo.content) #Lo que se va a guardar,para este caso el contenido de la url
  


############################################## 2021 ##############################################



df = pd.read_excel("C:/Users/JosePinzon/Documents/FINTRADE/Python/2021-11-23.xlsx", header =None)
df.head(8)

tmp="FECHA ASAMBLEA"

Posicion = df.index[(df[0] == tmp)].tolist() #Se guarda un objeto con la posicion de los nombres de las columnas

columnas = df.loc[Posicion] # Se guarda un objeto con las columnas.
columnas = columnas.replace({'\n':' '}, regex=True) #Se elimina el salto.
columnas = columnas.squeeze(axis=0) #El objeto estaba en DataFrame, se debe cambiar a un objeto de una dimension.
df.columns = columnas #Se establecen los nombres de las columnas en el DF.



df = df[(df[tmp] == "-").idxmax():] 
#https://stackoverflow.com/questions/38707165/drop-all-rows-before-first-occurrence-of-a-value

df.fillna(method='ffill', inplace = True) #Reemplaza todos los NaN por el valor anterior.

variables = ["VALOR TOTAL DEL DIVIDENDO","MONTO TOTAL ENTREGADO EN DIVIDENDOS"]

df[variables] = df[variables].replace({'\$':''}, regex=True) #Se elimina el signo pesos.

df.replace("-",np.NaN, inplace = True) #se reemplaza - por NaN.

ruta = "C:/Users/JosePinzon/Documents/FINTRADE/Python/Archivo2021.xlsx" #Ruta para guardar el archivo.
df.to_excel(ruta, index = False) #Guardar el archivo en excel.





############################################## 2020 ##############################################




df1 = pd.read_excel("C:/Users/JosePinzon/Documents/FINTRADE/Python/2021-11-23.xlsx", header =None,sheet_name = 1)
df1.head(8)

tmp="FECHA ASAMBLEA"

Posicion = df1.index[(df1[0] == tmp)].tolist() #Se guarda un objeto con la posicion de los nombres de las columnas

columnas = df1.loc[Posicion] # Se guarda un objeto con las columnas.
columnas = columnas.replace({'\n':' '}, regex=True) #Se elimina el salto.
columnas = columnas.squeeze(axis=0) #El objeto estaba en DataFrame, se debe cambiar a un objeto de una dimension.
df1.columns = columnas #Se establecen los nombres de las columnas en el DF.



df1 = df1[(df1[tmp] == "-").idxmax():] 
#https://stackoverflow.com/questions/38707165/drop-all-rows-before-first-occurrence-of-a-value

df1.fillna(method='ffill', inplace = True) #Reemplaza todos los NaN por el valor anterior.

variables = ["VALOR TOTAL DEL DIVIDENDO","MONTO TOTAL ENTREGADO EN DIVIDENDOS"]

df1[variables] = df1[variables].replace({'\$':''}, regex=True) #Se elimina el signo pesos.

df1.replace("-",np.NaN, inplace = True) #se reemplaza - por NaN.

ruta = "C:/Users/JosePinzon/Documents/FINTRADE/Python/Archivo2020.xlsx" #Ruta para guardar el archivo.
df1.to_excel(ruta, index = False) #Guardar el archivo en excel.







