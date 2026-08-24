suppressPackageStartupMessages({
  library(oame)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(leaflet)
  library(dplyr)
})


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test"),
    p(oame::root_data_path())
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
