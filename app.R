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

# load data with mapping info
mapping_island <- read_csv("https://raw.githubusercontent.com/sberghei/offer_reserves/master/Data/20180210_NetworkSupplyPointsTable.csv")

mapping_island %<>% 
  select("POC code", "Island") %>%
  distinct()

##############################################################################################################################

# user interface
ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel(title = "Intro",
             
      titlePanel("Energy and reserve offers", windowTitle = "NZ reserves"),

      p("This app allows to graphically represent supply curves for energy and reserve offers by generators in the New Zealand electricity market."),
      
      h4("How to use it?"),
      p("Click on the 'Plots' tab at the top left of this site. On the new site choose a date and press the 'Load data' button. 
        It takes some moments (~30 seconds) until the offer data is loaded. You can choose the trading period (1-48) and a single trader or all traders.
        Each plot shows the supply curve submitted to the first (dotted line) and to the last predispatch (continuous line)."),
      
      h4("More"),
      
      p("The app only uses data that is publicly available on", a("https://www.emi.ea.govt.nz/", href = "https://www.emi.ea.govt.nz/"),
        "(provided by the Electricity Authority New Zealand)."),
      p("The R code can be found on this", a("Github account", href = "https://github.com/sberghei/offer_reserves"), "." )
    ),
    
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
                      choices = c("All" = "ALL",
                                  "Contact Energy" = "CTCT",
                                  "Genesis" = "GENE",
                                  "Meridian" = "MERI",
                                  "Mighty River Power" = "MRPL",
                                  "Trust Energy" = "TRUS"))
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
    if(input$trader == "ALL") { temp <- filter(selected_data(), TradingPeriod == input$tp) } else
    { temp <- filter(selected_data(), Trader == input$trader, TradingPeriod == input$tp) }
    
    temp %>%
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
        expand_limits(x = 0, y = 0) +
        labs(x = "MW", y = "Price in NZ$", title = "Energy offers, Northern Island") +
        theme(legend.position = "left")


    
    ggplotly(p)
  })
  
  output$energy_offer_SI <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Energy", Island == "SI")
    
    data_first_predispatch <- get_first_predispatch(temp)
    
    data_last_predispatch <- get_last_predispatch(temp)
    
    p <- ggplot() +
      geom_step(data = data_first_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0) +
      labs(x = "MW", y = "Price in NZ$", title = "Energy offers, Southern Island")
    
    ggplotly(p)
  })

  # create plot for reserves
  output$reserve_offer_NI_FIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "FIR", ProductCategory != "IL",
                   Island == "NI") 
      
    data_first_predispatch_reserve <- get_first_predispatch(temp)
      
    data_last_predispatch_reserve <- get_last_predispatch(temp)
      
    q <- ggplot() +
        geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
        geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
        expand_limits(x = 0, y = 0) +
        labs(x = "MW", y = "Price in NZ$", title = "Reserve offers (FIR), Northern Island") +
        theme(plot.title = element_text(size=10))
    
    ggplotly(q)
  })
  
  output$reserve_offer_NI_SIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "SIR", ProductCategory != "IL",
                   Island == "NI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0) +
      labs(x = "MW", y = "Price in NZ$", title = "Reserve offers (SIR), Northern Island") +
      theme(plot.title = element_text(size=10))
    
    ggplotly(q)
  })
  
  # create plot for reserves
  output$reserve_offer_SI_FIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "FIR", ProductCategory != "IL",
                   Island == "SI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0) +
      labs(x = "MW", y = "Price in NZ$", title = "Reserve offers (FIR), Southern Island") +
      theme(plot.title = element_text(size=10))
    
    ggplotly(q)
  })
  
  output$reserve_offer_SI_SIR <- renderPlotly({
    temp <- filter(offer_data(), ProductType == "Reserve", ProductSubClass == "SIR", ProductCategory != "IL",
                   Island == "SI") 
    
    data_first_predispatch_reserve <- get_first_predispatch(temp)
    
    data_last_predispatch_reserve <- get_last_predispatch(temp)
    
    q <- ggplot() +
      geom_step(data = data_first_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), linetype = "dashed", direction = "vh") +
      geom_step(data = data_last_predispatch_reserve, aes(x = cumMegawatt, y = DollarsPerMegawattHour), direction = "vh") +
      expand_limits(x = 0, y = 0) +
      labs(x = "MW", y = "Price in NZ$", title = "Reserve offers (SIR), Southern Island") +
      theme(plot.title = element_text(size=10))
    
    ggplotly(q)
  })
}

shinyApp(ui = ui, server = server)
