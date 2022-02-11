library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(readxl)
library(shinyWidgets)
library(ggplot2)

Archivo2020<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRG-hlb0R-wchJu9rQwx4S-nS2ioCY8RsyPigRLLH1D1scB_6_o-6VPEKD1IiRrPG_c06O3G8AZQhD/pub?output=csv")

names(Archivo2020)[7] <- "FECHA.FINAL"
names(Archivo2020)[8] <- "TOTAL"
names(Archivo2020)[9] <- "CUOTA"
names(Archivo2020)[11] <- "TOTAL.ENTREGADO.EN.DIVIDENDOS"

Archivo2020$FECHA.ASAMBLEA <- as.Date(Archivo2020$FECHA.ASAMBLEA) #Convertir tipo de dato a fecha. 
Archivo2020$FECHA.INICIAL <- as.Date(Archivo2020$FECHA.INICIAL) #Convertir tipo de dato a fecha.
Archivo2020$FECHA.FINAL <- as.Date(Archivo2020$FECHA.FINAL) #Convertir tipo de dato a fecha.
Archivo2020$CUOTA <- as.numeric(Archivo2020$CUOTA) #Convertir tipo de dato a numérico.



ACCIONES <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR-2gtUQ9kebLpXxVPz3MrAqsORd4CqewaRxP_QU8i4oAWOipvEDXj_aZ7rhFtI6VMErOlz8_V2iEWs/pub?gid=0&single=true&output=csv")
ACCIONES$fecha <- as.Date(ACCIONES$fecha, format = "%d/%m/%y") #Convertir tipo de dato a fecha.

df <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
                    PFBCOLOM, NUTRESA, PFGRUPSURA,ECOPETROL) #Ordenar los precios de las columnas seleccionadas en una sola columna. 

opciones <- c("PFBCOLOM", "NUTRESA", "PFGRUPSURA","ECOPETROL")

ui <- dashboardPage(
  dashboardHeader(title = span("Simulador Utilidades Colcap", #Titulo superior izquierdo.
                               style = "font-size: 16px"),  #Tamaño.
                  titleWidth = 250 ), #Ancho.
  
  
  dashboardSidebar(
    width = 250, #Ancho del menu.
    sidebarMenu(
      id = "Menu", #ID
      style = "position: relative; overflow: visible;",
      menuItem("Base de datos", tabName = "DataF",   #Creacion de una pestaña llamada base de datos, con ID DataF.
               icon = icon("database", lib = "font-awesome")), #Se agrega el icono de base de datos.
      
      menuItem("Retorno Acciones", tabName = 'ACC', #Creacion de una pestaña llamada retorno de acciones, con ID DataF.
               icon = icon("chart-line", lib = "font-awesome")) #Se agrega el incono de grafico.
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
    /* main sidebar */
        .skin-yellow .main-sidebar {
                              background-color: #042e4f;
                              }
    
    /* body */
    .content-wrapper, .right-side {
    background-color: #fafafa;
    }
    '))), #Se escribio codigo en CSS para personalizar el color del menu y fondo del cuerpo de la pagina.
    tabItems( #Trabajar con varias pestañas.
      tabItem( #Trabajar con una pestaña.
        tabName = "DataF",  # Se selecciona la pestaña DataF.
        fluidRow( #Organiza objetos en una fila.
          column(3, #Se crea una columna de ancho 3.
                 dateRangeInput("IN_Fechas", "1) Seleccione el rango de fechas", #Creacion de input para la fecha de asamblea.
                                start = "2020-01-01",end = "2022-12-31", #Se indican fechas seleccionadas por defecto.
                                min = "2020-01-01",max = "2024-12-31", #Limites de seleccion.
                                format = "yyyy-mm-dd", #Formato de impresion.
                                width = "200px"), #Ancho.
                 pickerInput("IN_Sector", label = "Sector Economico", #Creacion de input para seleccionar el sector economico.
                             choices = NULL, multiple = T,  #No se establecen opciones, se pueden seleccionar multiples opciones.
                             options = list(`actions-box` = TRUE)), #Permite seleccionar y anular la seleccion de TODO.
                 pickerInput("IN_Moneda", label = "Moneda", #Creacion de input para seleccionar la moneda.
                             choices = NULL, multiple = T, 
                             options = list(`actions-box` = TRUE))),
          column(3,
                 pickerInput("IN_Emisor",label = "Emisor", #Creacion de input para seleccionar el Emisor.
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE)),
                 pickerInput("IN_Nemo",label = "Nemotecnico", #Creacion de input para seleccionar el Nemotecnico.
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE))
          )),
        fluidRow( #Organiza objeto en una fila.
          column(12, #Se crea una columna de ancho 12.
                 h2(("GRAFICOS DIVIDENDOS"), align = "center", style = 'font-size:30px')) #Se agrega titulo h2, centrado y tamaño.
        ),
        br(), #Espacio entre el objeto anterior y lo que sigue.
        fluidRow(
          tabBox(width = 12,title = "Graficos", #Se crea una caja de tamaño 12 con titulo graficos.
                 
                 tabPanel("Dividendos", plotOutput("Grafico_D")), #Se crea un espacio o pestaña llamada "Dividendos", en donde se muestra el grafico llamado "Grafico_D".
                 tabPanel("Dividendos acumulados", plotOutput("Grafico_AD"))) #Se crea un espacio o pestaña llamada "Dividendos acumulados", en donde se muestra el grafico llamado "Grafico_AD".
        ),
        
        fluidRow(
          column(12,
                 h2(("BASES DE DATOS FECHAS EX-DIVIDENDO"), 
                    align = "center", style = 'font-size:30px'))
        ),
        br(),
        fluidRow(
          tabBox(width = 12,title = "Bases de datos",
                 
                 tabPanel("Base original", dataTableOutput("Base")), #Se crea un espacio o pestaña llamada "Base original", en donde se muestra el dataframe "Base". 
                 tabPanel("Resumen por dia", dataTableOutput("Base_d"))) #Se crea un espacio o pestaña llamada "Resumen por dia", en donde se muestra el dataframe "Base_d".
        ),
      ), #Se cierran los elementos a mostrar en la pestaña DataF.
      
      
      
      tabItem( #Se abre el codigo para indicar los elementos u objetos de otra pestaña.
        tabName = "ACC", #Seleccion de la pestaña ACC
        h2("En construccion"),
        fluidRow(
          column(3,
                 dateRangeInput("IN_Fechas1", "Seleccione el rango de fechas",
                                start = "2020-01-01",end = "2022-12-31",
                                min = "2020-01-01",max = "2024-12-31",
                                format = "yyyy-mm-dd",
                                width = "200px")),
          column(3,
                 selectInput("IN_Nemo1",label = "Nemotecnico accion.",
                             choices = opciones)
          )),
        h3("Retorno por dividendos"), #Creacion titulo h3.
        verbatimTextOutput("base1"), #Se muestra la salida u output del resultado de "base1".
        h3("Retorno por diferencia de precio"), #Creacion titulo h3.
        verbatimTextOutput("base2") #Se muestra la salida u output del resultado de "base2".
      )
    )
    
    
  ),
  skin = "yellow" #Tema de la pagina amarillo.
  
)
server <- function(input, output, session) {  
  
  
  ################################## PRIMER MENU #################################
  
  observe({
    dat0<-filter(Archivo2020,FECHA.ASAMBLEA >= input$IN_Fechas[1] & #Crea dat0 con filtro de la seleccion en el input IN_Fechas
                   FECHA.ASAMBLEA <= input$IN_Fechas[2])
    updatePickerInput(session, "IN_Sector", label = "2) Sector economico",  #Se actualiza el input del sector IN_Sector 
                      choices = sort(unique(dat0$SECTOR)),selected = unique(dat0$SECTOR)) #con los valores únicos de la columna SECTOR de df dat0 como opciones. Se seleccionan todas por defecto
  })
  
  
  observe({
    dat00<-Archivo2020$MONEDA[Archivo2020$SECTOR%in%input$IN_Sector] #Crea dat00, un objeto con los datos de la variable MONEDA aplicando el filtro de la seleccion en el input IN_Sector.
    updatePickerInput(session, "IN_Moneda", label = "3) Moneda",  #Se actualiza el input de la moneda "IN_Moneda" y su etiqueta.
                      choices = sort(unique(dat00)),selected = unique(dat00)) #con los valores únicos del objeto dat00 como opciones. Se seleccionan todas por defecto.
  })
  
  
  observe({
    dat1<-Archivo2020$EMISOR[Archivo2020$MONEDA%in%input$IN_Moneda] #Crea dat1, un objeto con los datos de la variable EMISOR aplicando el filtro de la seleccion en el input IN_Moneda.
    updatePickerInput(session, "IN_Emisor", label = "4) Emisor de la accion", 
                      choices = sort(unique(dat1)),selected = unique(dat1))
  })
  
  observe({
    dat2<-Archivo2020$NEMOTECNICO[Archivo2020$EMISOR%in%input$IN_Emisor] #Crea dat2, un objeto con los datos de la variable NEMOTECNICO aplicando el filtro de la seleccion en el input IN_Emisor
    updatePickerInput(session, "IN_Nemo", label = "5)Nemotecnico", 
                      choices = sort(unique(dat2)),selected = unique(dat2))
  })
  
  
  datn<-reactive({ #Funcion reactiva para la creacion de un df con todos los filtros aplicados sobre el df original. 
    Archivo2020 %>% filter(SECTOR %in% input$IN_Sector &
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
  
  
  
  Base_AD<-reactive({ #Funcion reactiva con el nombre Base_AD.
    Archivo2020%>%
      na.omit(Archivo2020$CUOTA)%>% #Omite NA dela columna CUOTA del df original.
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
    Base_AD()
  })
  
  
  output$Grafico_AD <- renderPlot({
    Dividendos <- Base_AD()
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=T_div, color = MONEDA))+
      geom_step()+
      scale_y_continuous(labels = scales::label_comma(), 
                         breaks = scales::breaks_extended(n = 10))+
      scale_x_date(date_breaks = "2 months",
                   date_labels = "%b-%y")+
      labs(x = "Fecha de pago", y = "Dividendos")+
      theme_classic()
    
  })
  
  output$Grafico_D <- renderPlot({
    Dividendos <- Base_AD()
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=Total, color = MONEDA))+
      geom_step()+
      scale_y_continuous(labels = scales::label_comma(), 
                         breaks = scales::breaks_extended(n = 10))+
      scale_x_date(date_breaks = "2 months",
                   date_labels = "%b-%y")+
      labs(x = "Fecha de pago", y = "Dividendos")+
      theme_classic()
    
  })
  
  
  ################################# sEGUNDO MENU #################################
  
  
  Base_R<-reactive({
    Archivo2020 %>% filter(NEMOTECNICO %in% input$IN_Nemo1 &
                             FECHA.ASAMBLEA >= input$IN_Fechas1[1] & FECHA.ASAMBLEA <= input$IN_Fechas1[2])%>%
      select(-FECHA.INGRESO, -TOTAL.ENTREGADO.EN.DIVIDENDOS, -FECHA.INICIAL,
             -DESCRIPCION.PAGO.PDU,-MONEDA,-TOTAL,-FECHA.FINAL)
  })
  
  
  
  Retorno_Precio <- reactive({
    filter(df, Accion %in% input$IN_Nemo1 &
             fecha >= input$IN_Fechas1[1] & fecha <= input$IN_Fechas1[2])
  })
  
  output$base1 <- renderPrint({
    
    Reac_b <- Base_R()
    sum(Reac_b$CUOTA)
    
  })
  
  
  output$base2 <- renderPrint({
    Reac_b <- Retorno_Precio()
    tail(Reac_b$Precio,1) - head(Reac_b$Precio,1)
  })
  
  
  
}

shinyApp(ui = ui, server = server)
