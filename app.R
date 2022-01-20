library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(shinyWidgets)

Archivo2020 <- read_excel("C:\\Users\\josep\\Documents\\Universidad\\FINTRADE\\Python\\Archivo2020.xlsx")
Archivo2020$`FECHA ASAMBLEA` <- as.Date(Archivo2020$`FECHA ASAMBLEA`)
ACCIONES <- read_excel("C:\\Users\\josep\\Documents\\Universidad\\FINTRADE\\Python\\ACCIONES.xlsx")
ACCIONES$fecha <- as.Date(ACCIONES$fecha)
algo <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
                      PFBCOLOM, NUTRESA, PFGRUPSURA,ECOPETROL)

opciones <- c("PFBCOLOM", "NUTRESA", "PFGRUPSURA","ECOPETROL")

ui <- dashboardPage(
  dashboardHeader(title = span("Simulador Utilidades Colcap",
                               style = "font-size: 16px"), 
                  titleWidth = 250 ),
  
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "Menu",
      style = "position: relative; overflow: visible;",
      menuItem("Base de datos", tabName = "DataF", icon = icon("database", lib = "font-awesome")),
      
      menuItem("Retorno Acciones", tabName = 'ACC', icon = icon("chart-line", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "DataF",
        fluidRow(
          column(3,
                 dateRangeInput("IN_Fechas", "Seleccione el rango de fechas",
                                start = "2020-01-01",end = "2020-12-31",
                                min = "2020-01-01",max = "2020-12-31",
                                format = "yyyy-mm-dd",
                                width = "200px")),
          column(3,
                 pickerInput("IN_Emisor",label = "Emisor",
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE)),
                 pickerInput("IN_Nemo",label = "Nemo",
                             choices = NULL,multiple = T,options = list(`actions-box` = TRUE)),
          )),
        
        h2("Base de datos fechas Ex-dividendo"),
        br(),
        fluidRow(
          column(11,
                 dataTableOutput("base")))
      ),
      
      
      
      tabItem(
        tabName = "ACC",
        h2("En construccion"),
        fluidRow(
          column(3,
                 dateRangeInput("IN_Fechas1", "Seleccione el rango de fechas",
                                start = "2020-01-01",end = "2021-12-31",
                                min = "2020-01-01",max = "2021-12-31",
                                format = "yyyy-mm-dd",
                                width = "200px")),
          column(3,
                 selectInput("IN_Nemo1",label = "Nemotecnico accion.",
                             choices = opciones)
          )),
        h3("Retorno por dividendos"),
        verbatimTextOutput("base1"),
        h3("Retorno por diferencia de precio"),
        verbatimTextOutput("base2")
      )
    )
    
    
  ),
  skin = "yellow"
  
)
server <- function(input, output, session) {  
  
  
  ################################## PRIMER MENU #################################
  
  observe({
    dat0<-filter(Archivo2020,`FECHA ASAMBLEA` >= input$IN_Fechas[1] & `FECHA ASAMBLEA` <= input$IN_Fechas[2])
    updatePickerInput(session, "IN_Emisor", label = "Emisor", choices = sort(unique(dat0$EMISOR)),selected = unique(dat0$EMISOR))
  })
  
  observe({
    dat1<-Archivo2020$NEMOTÉCNICO[Archivo2020$EMISOR%in%input$IN_Emisor]
    updatePickerInput(session, "IN_Nemo", label = "Nemotecnico", choices = sort(unique(dat1)),selected = unique(dat1))
  })
  
  
  datn<-reactive({
    Archivo2020 %>% filter(EMISOR %in% input$IN_Emisor &
                             NEMOTÉCNICO %in% input$IN_Nemo)%>%
      select(-`FECHA INGRESO`, -`MONTO TOTAL ENTREGADO EN DIVIDENDOS`, -`FECHA INICIAL`)
  })
  
  output$base <- renderDataTable({
    
    datn()
    
  })
  
  
  ################################# sEGUNDO MENU #################################
  
  
  Base_R<-reactive({
    Archivo2020 %>% filter(NEMOTÉCNICO %in% input$IN_Nemo1 &
                             `FECHA ASAMBLEA` >= input$IN_Fechas1[1] & `FECHA ASAMBLEA` <= input$IN_Fechas1[2])%>%
      select(-`FECHA INGRESO`, -`MONTO TOTAL ENTREGADO EN DIVIDENDOS`, -`FECHA INICIAL`,
             -`DESCRIPCIÓN PAGO PDU`,-MONEDA,-`VALOR TOTAL DEL DIVIDENDO`,-`FECHA FINAL Y DE PAGO`)
  })
  
  Retorno_Precio <- reactive({
    filter(algo, Accion %in% input$IN_Nemo1 & 
             fecha >= input$IN_Fechas1[1] & fecha <= input$IN_Fechas1[2]) 
    
  })
  
  output$base1 <- renderPrint({
    
    Buena <- Base_R()
    sum(Buena$`VALOR CUOTA`)
    
  })
  
  
  output$base2 <- renderPrint({
    
    Buena <- Retorno_Precio()
    tail(Buena$Precio,1) - head(Buena$Precio,1)
    
  })
  

  
}

shinyApp(ui = ui, server = server)
