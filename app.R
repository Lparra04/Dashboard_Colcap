library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(readxl)
library(shinyWidgets)
library(ggplot2)

library(readxl)
Archivo2020 <- read_excel("Archivo2020.xlsx", 
                          col_types = c("date", "text", "text", 
                                        "text", "text", "date", "numeric", 
                                        "text", "numeric", "text", "text", 
                                        "numeric", "text"))

Archivo2020$`FECHA ASAMBLEA` <- as.Date(Archivo2020$`FECHA ASAMBLEA`)
Archivo2020$`FECHA INICIAL` <- as.Date(Archivo2020$`FECHA INICIAL`)

ACCIONES <- read_excel("ACCIONES.xlsx")
ACCIONES$fecha <- as.Date(ACCIONES$fecha)

df <- tidyr::gather(ACCIONES, key = "Accion", value = "Precio",
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
      menuItem("Base de datos", tabName = "DataF", 
               icon = icon("database", lib = "font-awesome")),
      
      menuItem("Retorno Acciones", tabName = 'ACC',
               icon = icon("chart-line", lib = "font-awesome"))
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
                                width = "200px"),
                 pickerInput("IN_Sector", label = "Sector Economico",
                             choices = NULL, multiple = T, 
                             options = list(`actions-box` = TRUE))),
          column(3,
                 pickerInput("IN_Emisor",label = "Emisor",
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE)),
                 pickerInput("IN_Nemo",label = "Nemo",
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE))
          )),
        h2(),
        br(),
        plotOutput("Grafico_AD"),
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
    dat0<-filter(Archivo2020,`FECHA ASAMBLEA` >= input$IN_Fechas[1] &
                   `FECHA ASAMBLEA` <= input$IN_Fechas[2])
    updatePickerInput(session, "IN_Sector", label = "Sector economico", 
                      choices = sort(unique(dat0$SECTOR)),selected = unique(dat0$SECTOR))
  })
  
  observe({
    dat1<-Archivo2020$EMISOR[Archivo2020$SECTOR%in%input$IN_Sector]
    updatePickerInput(session, "IN_Emisor", label = "Emisor de la accion", 
                      choices = sort(unique(dat1)),selected = unique(dat1))
  })
  
  observe({
    dat2<-Archivo2020$NEMOTÉCNICO[Archivo2020$EMISOR%in%input$IN_Emisor]
    updatePickerInput(session, "IN_Nemo", label = "Nemotecnico", 
                      choices = sort(unique(dat2)),selected = unique(dat2))
  })
  
  
  datn<-reactive({
    Archivo2020 %>% filter(SECTOR %in% input$IN_Sector&
                             EMISOR %in% input$IN_Emisor &
                             NEMOTÉCNICO %in% input$IN_Nemo)%>%
      select(-`FECHA INGRESO`, -`MONTO TOTAL ENTREGADO EN DIVIDENDOS`, -`FECHA INICIAL`)
  })
  
  output$base <- renderDataTable({
    
    datn()
    
  })
  
  
  
  Base_AD<-reactive({
    Archivo2020%>%
      group_by(`FECHA INICIAL`)%>%
      filter(`FECHA ASAMBLEA` >= input$IN_Fechas[1] &
               `FECHA ASAMBLEA` <= input$IN_Fechas[2] &
               SECTOR %in% input$IN_Sector & 
               EMISOR %in% input$IN_Emisor &
               NEMOTÉCNICO %in% input$IN_Nemo)%>%
      summarize(Total = sum(`VALOR CUOTA`))%>%
      mutate(T_div=cumsum(Total))
  })
  
  
  
  output$Grafico_AD <- renderPlot({
    Dividendos <- Base_AD()
    Dividendos <- na.omit(Dividendos)
    ggplot(Dividendos, aes(x=`FECHA INICIAL`,y=T_div))+
      geom_step(colour = "Blue")+
      scale_y_continuous(labels = scales::label_comma(), 
                         breaks = scales::breaks_extended(n = 10))+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y")+
      labs(title = "Grafico dividendos acumulados Colcap")
    
  })
  
  
  ################################# sEGUNDO MENU #################################
  
  
  Base_R<-reactive({
    Archivo2020 %>% filter(NEMOTÉCNICO %in% input$IN_Nemo1 &
                             `FECHA ASAMBLEA` >= input$IN_Fechas1[1] & `FECHA ASAMBLEA` <= input$IN_Fechas1[2])%>%
      select(-`FECHA INGRESO`, -`MONTO TOTAL ENTREGADO EN DIVIDENDOS`, -`FECHA INICIAL`,
             -`DESCRIPCIÓN PAGO PDU`,-MONEDA,-`VALOR TOTAL DEL DIVIDENDO`,-`FECHA FINAL Y DE PAGO`)
  })
  
  
  
  Retorno_Precio <- reactive({
    filter(df, Accion %in% input$IN_Nemo1 &
             fecha >= input$IN_Fechas1[1] & fecha <= input$IN_Fechas1[2])
  })
  
  output$base1 <- renderPrint({
    
    Reac_b <- Base_R()
    sum(Reac_b$`VALOR CUOTA`)
    
  })
  
  
  output$base2 <- renderPrint({
    Reac_b <- Retorno_Precio()
    tail(Reac_b$Precio,1) - head(Reac_b$Precio,1)
  })
  
  
  
}

shinyApp(ui = ui, server = server)
