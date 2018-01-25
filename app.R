# Shiny app (Reserve and offer market)

library(tidyverse)
library(RCurl)
library(shiny)

# user interface
ui <- fluidPage(
  sidebarPanel(
    
  )
  
  mainPanel(
    plotOutput(ouputId = "step_offers")
  )
)

server <- function(input, output){
  test <- read_csv("ftp://emiftp.ea.govt.nz/Datasets/Wholesale/BidsAndOffers/Offers/2017/20170109_Offers.csv")
}

shinyApp(ui = ui, server = server)# Shiny app (Reserve and offer market)

