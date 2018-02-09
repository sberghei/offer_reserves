# Shiny app (Reserve and offer market)

library(tidyverse)
library(RCurl)
library(shiny)
library(DT)
library(stringr)
library(lubridate)
library(plotly)

# load helper functions
source("helper.R")

# user interface
ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel(title = "Intro"),
    
    tabPanel(title = "Plots",
             
      sidebarPanel(width = 2,
        
        wellPanel(
          # Date Input + Action button
          dateInput(inputId = "date",
                  label = "Date",
                  value = "2017-12-01",
                  min = "2013-01-01",
                  max = "2017-12-31"),
          actionButton(inputId = "update_date", "Load data")
        ),
        
        br(),
        
        wellPanel(
        # Trading Period
          numericInput(inputId = "tp",
                       label = "Select trading period",
                       min = 1,
                       max = 48,
                       step = 1,
                       value = 1),
          
          # Trader
          selectInput(inputId = "trader",
                      label = "Select trader", 
                      choices = c("Contact Energy" = "CTCT",
                                  "Trust Energy" = "TRUS",
                                  "Mighty River Power" = "MRPL",
                                  "Genesis" = "GENE",
                                  "Meridian" = "MERI")),
          
          # Predispatch
          checkboxGroupInput(inputId = "predispatch",
                      label = "Select predispatch",
                      choices = c("First predispatch", "Last predispatch"),
                      selected = c("First predispatch", "Last predispatch"))
        )
      ),
    
      mainPanel(width = 10,
        splitLayout(
          plotlyOutput(outputId = "energy_offer_NI"),
          plotlyOutput(outputId = "energy_offer_SI")
        ),
        splitLayout(
          plotlyOutput(outputId = "reserve_offer_NI_FIR"),
          plotlyOutput(outputId = "reserve_offer_NI_SIR"),
          plotlyOutput(outputId = "reserve_offer_SI_FIR"),
          plotlyOutput(outputId = "reserve_offer_SI_SIR")
        )
      )
    )
  )
)

server <- function(input, output){
  
  # load data
  selected_data <- eventReactive(input$update_date, {
    data_string <-   str_replace_all(input$date, "-", "")
    year_string <-   str_sub(input$date, 1, 4)
    filename <-      paste0("ftp://emiftp.ea.govt.nz/Datasets/Wholesale/BidsAndOffers/Offers/", year_string, "/", data_string, "_Offers.csv" )
    read_csv(filename)
  }, ignoreNULL = TRUE)
  
  # create plot for energy offers
  offer_data <- reactive({
    filter(selected_data(), Trader == input$trader, TradingPeriod == input$tp) %>%
            left_join(mapping_island, by = c("PointOfConnection" = "POC code")) %>%
            mutate(SubmissionDate = ymd_hms(paste0(UTCSubmissionDate, UTCSubmissionTime)),
                   TradingDateTime = ymd_hm(paste0(TradingDate, tp_to_hour(TradingPeriod)))) %>%
            mutate(TimeDiff = difftime(SubmissionDate, TradingDateTime,  unit = "hours") ) %>%
            mutate(Predispatch = timediff_to_predispatch(TimeDiff, TradingPeriod)) 
  })
  
  output$energy_offer_NI <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Energy", Island == "NI")
    
    data_first_predispatch <- get_first_predispatch(temp)
      
    data_last_predispatch <- get_last_predispatch(temp)
      
    p <- ggplot() +
        geom_step(data = data_first_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
        geom_step(data = data_last_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
        expand_limits(x = 0, y = 0) 
    
    ggplotly(p)
  })
  
  output$energy_offer_SI <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Energy", Island == "SI")
    
    data_first_predispatch <- get_first_predispatch(temp)
    
    data_last_predispatch <- get_last_predispatch(temp)
    
    p <- ggplot() +
      geom_step(data = data_first_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0) 
    
    ggplotly(p)
  })

  # create plot for reserves
  output$reserve_offer_NI_FIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "FIR", Island == "NI") 
      
    data_first_predispatch_reserve <- get_first_predispatch(temp)
      
    data_last_predispatch_reserve <- get_last_predispatch(temp)
      
    q <- ggplot() +
        geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
        geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
        expand_limits(x = 0, y = 0)
    
    ggplotly(q)
  })
  
  output$reserve_offer_NI_SIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "SIR", Island == "NI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0)
    
    ggplotly(q)
  })
  
  # create plot for reserves
  output$reserve_offer_SI_FIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "FIR", Island == "SI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0)
    
    ggplotly(q)
  })
  
  output$reserve_offer_SI_SIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "SIR", Island == "SI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0)
    
    ggplotly(q)
  })
}

shinyApp(ui = ui, server = server)
