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
Archivo2020$CUOTA <- as.numeric(Archivo2020$CUOTA) #Convertir tipo de dato a numÃ©rico.



ACCIONES <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT-liEK7HQCbWF6wmzVtpy5nQReZB4eEY4TiSe6vZrwQzOm3CBhW0E-AyKBEJGYMRaiHuhC7Lkcb-_q/pub?output=csv")
ACCIONES$fecha <- as.Date(ACCIONES$fecha, format = "%d/%m/%y") #Convertir tipo de dato a fecha.

df <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
                    PFBCOLOM, NUTRESA, PFGRUPSURA,ECOPETROL) #Ordenar los precios de las columnas seleccionadas en una sola columna. 

opciones <- c("PFBCOLOM", "NUTRESA", "PFGRUPSURA","ECOPETROL")

ui <- dashboardPage(
  dashboardHeader(title = span("Visualizador Acciones Colombia", #Titulo superior izquierdo.
                               style = "font-size: 16px"),  #TamaÃ±o.
                  titleWidth = 250,
                  tags$li(a(tags$img(src = "data:image/jpeg;base64,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",
                                     title = "Logo", height = "50px")),
                          class = "dropdown")
                 ), 
  
  
  dashboardSidebar(
    width = 200, #Ancho del menu.
    sidebarMenu(
      id = "Menu", #ID
      style = "position: relative; overflow: visible;",
      menuItem("Base de datos", tabName = "DataF",   #Creacion de una pestaÃ±a llamada base de datos, con ID DataF.
               icon = icon("database", lib = "font-awesome")), #Se agrega el icono de base de datos.
      
      menuItem("Retorno Acciones", tabName = 'ACC', #Creacion de una pestaÃ±a llamada retorno de acciones, con ID DataF.
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
    tabItems( #Trabajar con varias pestaÃ±as.
      tabItem( #Trabajar con una pestaÃ±a.
        tabName = "DataF",  # Se selecciona la pestaÃ±a DataF.
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
                 h2(("GRAFICOS DIVIDENDOS"), align = "center", style = 'font-size:30px')) #Se agrega titulo h2, centrado y tamaÃ±o.
        ),
        br(), #Espacio entre el objeto anterior y lo que sigue.
        fluidRow(
          tabBox(width = 12,title = "Graficos", #Se crea una caja de tamaÃ±o 12 con titulo graficos.
                 
                 tabPanel("Dividendos", plotOutput("Grafico_D")), #Se crea un espacio o pestaÃ±a llamada "Dividendos", en donde se muestra el grafico llamado "Grafico_D".
                 tabPanel("Dividendos acumulados", plotOutput("Grafico_AD"))) #Se crea un espacio o pestaÃ±a llamada "Dividendos acumulados", en donde se muestra el grafico llamado "Grafico_AD".
        ),
        
        fluidRow(
          column(12,
                 h2(("BASES DE DATOS FECHAS EX-DIVIDENDO"), 
                    align = "center", style = 'font-size:30px'))
        ),
        br(),
        fluidRow(
          tabBox(width = 12,title = "Bases de datos",
                 
                 tabPanel("Base original", dataTableOutput("Base")), #Se crea un espacio o pestaÃ±a llamada "Base original", en donde se muestra el dataframe "Base". 
                 tabPanel("Resumen por dia", dataTableOutput("Base_d"))) #Se crea un espacio o pestaÃ±a llamada "Resumen por dia", en donde se muestra el dataframe "Base_d".
        ),
      ), #Se cierran los elementos a mostrar en la pestaÃ±a DataF.
      
      
      
      tabItem( #Se abre el codigo para indicar los elementos u objetos de otra pestaÃ±a.
        tabName = "ACC", #Seleccion de la pestaÃ±a ACC
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
                      choices = sort(unique(dat0$SECTOR)),selected = unique(dat0$SECTOR)) #con los valores Ãºnicos de la columna SECTOR de df dat0 como opciones. Se seleccionan todas por defecto
  })
  
  
  observe({
    dat00<-Archivo2020$MONEDA[Archivo2020$SECTOR%in%input$IN_Sector] #Crea dat00, un objeto con los datos de la variable MONEDA aplicando el filtro de la seleccion en el input IN_Sector.
    updatePickerInput(session, "IN_Moneda", label = "3) Moneda",  #Se actualiza el input de la moneda "IN_Moneda" y su etiqueta.
                      choices = sort(unique(dat00)),selected = unique(dat00)) #con los valores Ãºnicos del objeto dat00 como opciones. Se seleccionan todas por defecto.
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
      select(-FECHA.INGRESO, -TOTAL.ENTREGADO.EN.DIVIDENDOS, #EliminaciÃ³n de columnas.
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
    Base_AD() #Imprime Base_AD
  })
  
  
  output$Grafico_AD <- renderPlot({ #Se crea Grafico_AD, el cual contiene un grafico de ggplot2.
    Dividendos <- Base_AD() #Se establece el df Dividendos con la informaciÃ³n de la funciÃ³n reactiva Base_AD()
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=T_div, color = MONEDA))+ #Seleccion de base de datos, columnas de los ejes x, y, color dependiendo el tipo de moneda.
      geom_step()+ #Tipo de grafico
      scale_y_continuous(labels = scales::label_comma(), #Ajuste numeros eje Y
                         breaks = scales::breaks_extended(n = 10))+ #Ajuste de saltos eje Y
      scale_x_date(date_breaks = "2 months", #Saltos fecha eje X
                   date_labels = "%b-%y")+ #Formato de fecha eje X
      labs(x = "Fecha de pago", y = "Dividendos")+ #Etiquetas eje x y eje Y.
      theme_classic() #Tema o fondo del grafico.
    
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
    Archivo2020 %>% filter(NEMOTECNICO %in% input$IN_Nemo1 &
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

shinyApp(ui = ui, server = server)
