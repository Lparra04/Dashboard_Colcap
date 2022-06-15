library(dplyr)
library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(ggiraph)

server<- function(input, output, session) {
  source("C:/Users/Laura/Desktop/Dashboard_Colcap/R-Rstudio/bases.R") 
  
  ################################## PRIMER MENU ################################
  
  observe({
    dat0<-filter(Archivo2022,FECHA.ASAMBLEA >= input$IN_Fechas[1] & #Crea dat0 con filtro de la seleccion en el input IN_Fechas
                   FECHA.ASAMBLEA <= input$IN_Fechas[2])
    updatePickerInput(session, "IN_Sector", label = "2) Sector economico",  #Se actualiza el input del sector IN_Sector 
                      choices = sort(unique(dat0$SECTOR)),selected = unique(dat0$SECTOR)) #con los valores Ãºnicos de la columna SECTOR de df dat0 como opciones. Se seleccionan todas por defecto
  })
  
  
  observe({
    dat00<-Archivo2022$MONEDA[Archivo2022$SECTOR%in%input$IN_Sector] #Crea dat00, un objeto con los datos de la variable MONEDA aplicando el filtro de la seleccion en el input IN_Sector.
    updatePickerInput(session, "IN_Moneda", label = "3) Moneda",  #Se actualiza el input de la moneda "IN_Moneda" y su etiqueta.
                      choices = sort(unique(dat00)),selected = unique(dat00)) #con los valores Ãºnicos del objeto dat00 como opciones. Se seleccionan todas por defecto.
  })
  
  
  observe({
    dat1<-Archivo2022$EMISOR[Archivo2022$MONEDA%in%input$IN_Moneda] #Crea dat1, un objeto con los datos de la variable EMISOR aplicando el filtro de la seleccion en el input IN_Moneda.
    updatePickerInput(session, "IN_Emisor", label = "4) Emisor de la accion", 
                      choices = sort(unique(dat1)),selected = unique(dat1))
  })
  
  observe({
    dat2<-Archivo2022$NEMOTECNICO[Archivo2022$EMISOR%in%input$IN_Emisor] #Crea dat2, un objeto con los datos de la variable NEMOTECNICO aplicando el filtro de la seleccion en el input IN_Emisor
    updatePickerInput(session, "IN_Nemo", label = "5)NEMOTECNICO", 
                      choices = sort(unique(dat2)),selected = unique(dat2))
  })
  
  
  datn<-reactive({ #Funcion reactiva para la creacion de un df con todos los filtros aplicados sobre el df original. 
    Archivo2022 %>% filter(SECTOR %in% input$IN_Sector &
                             MONEDA %in% input$IN_Moneda &
                             EMISOR %in% input$IN_Emisor &
                             NEMOTECNICO %in% input$IN_Nemo)%>%
      select(-FECHA.INGRESO, -TOTAL.ENTREGADO.EN.DIVIDENDOS, #Eliminación de columnas.
             -DESCRIPCION.PAGO.PDU,-MODO.DE.PAGO)
    
  })
  
  output$Base <- renderDataTable({ #SE CREA EL OUTPUT BASE, QUE SEGUN EL RENDER IMPRIME UNA DATATABLE O DF. 
    #Contenido de Base.
    Base_M1 <- datn() #Se guarda el resultado de datn en el objeto Base_M1    
    Base_M1 #Imprime Base_M1.
  })
  
  
  
  Base_AD<-reactive({ #Función reactiva con el nombre Base_AD.
    Archivo2022%>%
      na.omit(Archivo2022$CUOTA)%>% #Omite NA de la columna CUOTA del df original.
      filter(FECHA.ASAMBLEA >= input$IN_Fechas[1] &
               FECHA.ASAMBLEA <= input$IN_Fechas[2] &
               SECTOR %in% input$IN_Sector &
               MONEDA %in% input$IN_Moneda &
               EMISOR %in% input$IN_Emisor &
               NEMOTECNICO %in% input$IN_Nemo)%>% #Filtros de los Inputs.
      group_by(MONEDA,FECHA.INICIAL)%>% 
      summarize(Total = sum(CUOTA))%>% #Creacion variable Total, que es la suma de la cuota para cada fecha.
      mutate(T_div=cumsum(Total)) #Creacion variable T_div, que es la suma acumulada de los dividendos para cada fecha.
  })
  
  output$Base_d <- renderDataTable({ #CREACION DEL OUTPUT Base_D.
    Base_AD() #Imprime Base_AD
  })
  
  
  output$Grafico_AD <- renderPlot({ #Se crea Grafico_AD, el cual contiene un grafico de ggplot2.
    Dividendos <- Base_AD() #Se establece el df Dividendos con la información de la función reactiva Base_AD()
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=T_div, color = MONEDA))+ #Seleccion de base de datos, columnas de los ejes x, y, color dependiendo el tipo de moneda.
      geom_step_interactive(size=1)+ #Tipo de grafico
      scale_y_continuous(labels = scales::label_comma(), #Ajuste numeros eje Y
                         breaks = scales::breaks_extended(n = 10))+ #Ajuste de saltos eje Y
      scale_x_date(date_breaks = "2 months", #Saltos fecha eje X
                   date_labels = "%b-%y")+ #Formato de fecha eje X
      labs(x = "Fecha de pago", y = "Dividendos")+ #Etiquetas eje x y eje Y
      theme(panel.background = element_rect(fill = "white"), text = element_text(size = 15), 
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.x = element_line(color = "black"), 
            axis.line.y = element_line(colour = "black")
      )#Tema o fondo del grafico.
    
  })
  
  output$Grafico_D <- renderPlot({ #Creacion del grafico llamado "Grafico_D".
    Dividendos <- Base_AD() #Base de datos
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=Total, color = MONEDA))+ #Seleccion de base de datos, columnas de los ejes x, y, color dependiendo el tipo de moneda.
      geom_step()+ #Tipo de grafico
      scale_y_continuous(labels = scales::label_comma(), 
                         breaks = scales::breaks_extended(n = 10))+
      scale_x_date(date_breaks = "2 months",
                   date_labels = "%b-%y")+
      labs(x = "Fecha de pago", y = "Dividendos")+
      theme_classic()
    
  })
  
  ################################# sEGUNDO MENU #################################
  
  
  Base_R<-reactive({ #Creacion de funcion reactiva llamada Base_R.
    Archivo2022 %>% filter(NEMOTECNICO %in% input$IN_Nemo1 &
                             FECHA.ASAMBLEA >= input$IN_Fechas1[1] & FECHA.ASAMBLEA <= input$IN_Fechas1[2])%>% #Filtros de el input fechas de la segunda pestaÃ±a.
      select(-FECHA.INGRESO, -TOTAL.ENTREGADO.EN.DIVIDENDOS, -FECHA.INICIAL, #Eliminacion de columnas.
             -DESCRIPCION.PAGO.PDU,-MONEDA,-TOTAL,-FECHA.FINAL)
  })
  
  
  
  Retorno_Precio <- reactive({#Funcion reactiva "Retorno_Precio".
    filter(df, Accion %in% input$IN_Nemo1 &
             fecha >= input$IN_Fechas1[1] & fecha <= input$IN_Fechas1[2])
  })
  
  output$base1 <- renderPrint({ #Creacion de la salida u output base1
    
    Reac_b <- Base_R() #Establece df Reac_b con la informaciÃ³n de la funcion reactiva Base_R().
    sum(Reac_b$CUOTA) #Imprime el resultado de la suma de los valores de la columna CUOTA.
    
  })
  
  
  output$base2 <- renderPrint({ #Creacion salida u output base2.
    Reac_b <- Retorno_Precio() #Establece df Reac_b con la informaciÃ³n de la funcion reactiva Retorno_Precio().
    tail(Reac_b$Precio,1) - head(Reac_b$Precio,1) #Resta el valor de la ultima fila de la columna Precio con el primer valor de la columna Precio para dar la diferencia de precios.
  })
  
}


