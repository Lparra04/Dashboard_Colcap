library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(readxl)
library(shinyWidgets)
library(ggplot2)

Archivo2020<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRG-hlb0R-wchJu9rQwx4S-nS2ioCY8RsyPigRLLH1D1scB_6_o-6VPEKD1IiRrPG_c06O3G8AZQhD/pub?output=csv")

Archivo2020$FECHA.ASAMBLEA <- as.Date(Archivo2020$FECHA.ASAMBLEA)
Archivo2020$FECHA.INICIAL <- as.Date(Archivo2020$FECHA.INICIAL)
Archivo2020$FECHA.FINAL.Y.DE.PAGO <- as.Date(Archivo2020$FECHA.FINAL.Y.DE.PAGO)
Archivo2020$VALOR.CUOTA <- as.numeric(Archivo2020$VALOR.CUOTA)
names(Archivo2020)[8] <- "TOTAL"


ACCIONES <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR-2gtUQ9kebLpXxVPz3MrAqsORd4CqewaRxP_QU8i4oAWOipvEDXj_aZ7rhFtI6VMErOlz8_V2iEWs/pub?gid=0&single=true&output=csv")
ACCIONES$fecha <- as.Date(ACCIONES$fecha, format = "%d/%m/%y")

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
                 dateRangeInput("IN_Fechas", "1) Seleccione el rango de fechas",
                                start = "2020-01-01",end = "2022-12-31",
                                min = "2020-01-01",max = "2024-12-31",
                                format = "yyyy-mm-dd",
                                width = "200px"),
                 pickerInput("IN_Sector", label = "Sector Economico",
                             choices = NULL, multiple = T, 
                             options = list(`actions-box` = TRUE)),
                 pickerInput("IN_Moneda", label = "Moneda",
                             choices = NULL, multiple = T, 
                             options = list(`actions-box` = TRUE))),
          column(3,
                 pickerInput("IN_Emisor",label = "Emisor",
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE)),
                 pickerInput("IN_Nemo",label = "Nemotecnico",
                             choices = NULL,multiple = T,
                             options = list(`actions-box` = TRUE))
          )),
        h2("Grafico dividendos acumulados"),
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
                                start = "2020-01-01",end = "2022-12-31",
                                min = "2020-01-01",max = "2024-12-31",
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
    dat0<-filter(Archivo2020,FECHA.ASAMBLEA >= input$IN_Fechas[1] &
                   FECHA.ASAMBLEA <= input$IN_Fechas[2])
    updatePickerInput(session, "IN_Sector", label = "2) Sector economico", 
                      choices = sort(unique(dat0$SECTOR)),selected = unique(dat0$SECTOR))
  })
  
  
  observe({
    dat00<-Archivo2020$MONEDA[Archivo2020$SECTOR%in%input$IN_Sector]
    updatePickerInput(session, "IN_Moneda", label = "3) Moneda", 
                      choices = sort(unique(dat00)),selected = unique(dat00))
  })
  
  
  observe({
    dat1<-Archivo2020$EMISOR[Archivo2020$MONEDA%in%input$IN_Moneda]
    updatePickerInput(session, "IN_Emisor", label = "4) Emisor de la accion", 
                      choices = sort(unique(dat1)),selected = unique(dat1))
  })
  
  observe({
    dat2<-Archivo2020$NEMOTECNICO[Archivo2020$EMISOR%in%input$IN_Emisor]
    updatePickerInput(session, "IN_Nemo", label = "5)Nemotecnico", 
                      choices = sort(unique(dat2)),selected = unique(dat2))
  })
  
  
  datn<-reactive({
    Archivo2020 %>% filter(SECTOR %in% input$IN_Sector &
                             MONEDA %in% input$IN_Moneda &
                             EMISOR %in% input$IN_Emisor &
                             NEMOTECNICO %in% input$IN_Nemo)%>%
      select(-FECHA.INGRESO, -MONTO.TOTAL.ENTREGADO.EN.DIVIDENDOS,
             -DESCRIPCION.PAGO.PDU)
    
  })
  
  output$base <- renderDataTable({
    
    Base_M1 <- datn()
    names(Base_M1)[7]="FECHA.FINAL"
    names(Base_M1)[9]="CUOTA"
    
    Base_M1
  })
  
  
  
  Base_AD<-reactive({
    Archivo2020%>%
      na.omit(Archivo2020$VALOR.CUOTA)%>%
      group_by(FECHA.INICIAL)%>%
      filter(FECHA.ASAMBLEA >= input$IN_Fechas[1] &
               FECHA.ASAMBLEA <= input$IN_Fechas[2] &
               SECTOR %in% input$IN_Sector & 
               EMISOR %in% input$IN_Emisor &
               NEMOTECNICO %in% input$IN_Nemo)%>%
      summarize(Total = sum(VALOR.CUOTA))%>%
      mutate(T_div=cumsum(Total))
  })
  
  
  
  output$Grafico_AD <- renderPlot({
    Dividendos <- Base_AD()
    ggplot(Dividendos, aes(x=FECHA.INICIAL,y=T_div))+
      geom_step(colour = "Blue")+
      scale_y_continuous(labels = scales::label_comma(), 
                         breaks = scales::breaks_extended(n = 10))+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y")+
      labs(x = "Fecha de pago", y = "Dividendos")+
      theme_bw()
    
  })
  
  
  ################################# sEGUNDO MENU #################################
  
  
  Base_R<-reactive({
    Archivo2020 %>% filter(NEMOTECNICO %in% input$IN_Nemo1 &
                             FECHA.ASAMBLEA >= input$IN_Fechas1[1] & FECHA.ASAMBLEA <= input$IN_Fechas1[2])%>%
      select(-FECHA.INGRESO, -MONTO.TOTAL.ENTREGADO.EN.DIVIDENDOS, -FECHA.INICIAL,
             -DESCRIPCION.PAGO.PDU,-MONEDA,-TOTAL,-FECHA.FINAL.Y.DE.PAGO)
  })
  
  
  
  Retorno_Precio <- reactive({
    filter(df, Accion %in% input$IN_Nemo1 &
             fecha >= input$IN_Fechas1[1] & fecha <= input$IN_Fechas1[2])
  })
  
  output$base1 <- renderPrint({
    
    Reac_b <- Base_R()
    sum(Reac_b$VALOR.CUOTA)
    
  })
  
  
  output$base2 <- renderPrint({
    Reac_b <- Retorno_Precio()
    tail(Reac_b$Precio,1) - head(Reac_b$Precio,1)
  })
  
  
  
}

shinyApp(ui = ui, server = server)
