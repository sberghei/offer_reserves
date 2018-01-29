# Shiny app (Reserve and offer market)

library(tidyverse)
library(RCurl)
library(shiny)
library(DT)
library(stringr)


# user interface
ui <- fluidPage(
  sidebarPanel(
    
    # Date Input + Action button
    dateInput(inputId = "date",
              label = "Date",
              value = "2017-12-01",
              min = "2013-01-01",
              max = "2017-12-31"),
    actionButton(inputId = "update_date", "Load data"),
    
    # Trading Period
    numericInput(inputId = "tp",
                 label = "Trading Period",
                 min = 1,
                 max = 48,
                 step = 1,
                 value = 1),
    
    # Trader
    selectInput(inputId = "trader",
                label = "Trader", 
                choices = c("Contact Energy" = "CTCT",
                            "Trust Energy" = "TRUS"))
  ),
  
  mainPanel(
    #DT::dataTableOutput(outputId = "head_DT")
    plotOutput(outputId = "step_offers_energy"),
    plotOutput(outputId = "step_offers_reserves")
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
  
  # header of data table
  #output$head_DT <- DT::renderDataTable({
  #  DT::datatable(data = selected_data(), options = list(pageLength = 10))
  #})
  
  # create plot for energy offers
  output$step_offers_energy <- renderPlot({
    filter(selected_data(), Trader == input$trader, ProductType == "Energy",
           TradingPeriod == input$tp, IsLatest == "Y") %>%
      arrange(DollarsPerMegawattHour) %>%
      mutate(cumMegawatt = cumsum(Megawatt)) %>%
      ggplot(aes(x = cumMegawatt, y = DollarsPerMegawattHour)) +
      geom_step(direction = "vh") +
      expand_limits(x = 0, y = 0)
  })

  # create plot for reserves
  output$step_offers_reserves <- renderPlot({
    filter(selected_data(), Trader == input$trader, ProductType == "Reserve",
           TradingPeriod == input$tp, IsLatest == "Y") %>%
      arrange(DollarsPerMegawattHour) %>%
      mutate(cumMegawatt = cumsum(Megawatt)) %>%
      ggplot(aes(x = cumMegawatt, y = DollarsPerMegawattHour)) +
      geom_step(direction = "vh") +
      expand_limits(x = 0, y = 0)
  })
}

shinyApp(ui = ui, server = server)
