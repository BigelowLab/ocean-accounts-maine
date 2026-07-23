suppressPackageStartupMessages({
  library(oame)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(leaflet)
  library(dplyr)
})

# bigelowshinytheme::copy_www("package/inst/shiny")
DMR = oame::read_dmr_landings("modern") |>
  oame::prep_dmr_landings_county()
AMO = oame::read_amo()
NAO = oame::read_nao()
TIDE = oame::read_tide()
COUNTIES = oame::read_me_counties(crs = 3857)


ui <- shiny::fluidPage(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  bigelow_header(h2("Ocean Accounts - Maine")),
  bigelow_main_body(
  
    bslib::navset_tab(
      nav_panel("Climatology", 
                selectInput("Index",
                            "Choose an index to plot",
                            choices= c("AMO", "NAO", "Tides"),
                            selected= "AMO"),
                selectInput("indexType",
                            "Choose a plot style",
                            choices = c("timeseries", "monthly", "climatology"),
                            selected = "timeseries"),
                div(style = "height: 70vh; overflow-x: auto; display: flex;",
                    div(style = "width: 68vh; flex-shrink: 0; margin: 1vh;", 
                        bigelowshinytheme::bigelow_card(headerContent = "Climatology",
                                                        plotOutput("indexPlot", width = "100%", height = "100%")))
                  )),
      nav_panel("DMR Landings Map", 
                selectInput("dmrMapSpecies",
                            "Choose species",
                            choices = DMR$species |> unique() |> sort(),
                            selected = "Clam Soft"),
                selectInput("dmrMapVariable",
                            "Choose a variable",
                            choices=c("weight", "value", "trip_n", "harv_n"),
                            selected="trip_n"),
                selectInput("dmrMapYear",
                            "Choose years",
                            choices = c("recent", "all", 
                                        as.character(seq(from = min(DMR$year),max(DMR$year)))),
                            selected = "recent"),
                selectInput("dmrMapStyle",
                            "Choose style",
                            choices = c("plain", "cartogram"),
                            selected = "plain"),           
                div(style = "height: 70vh; overflow-x: auto; display: flex;",
                    div(style = "width: 68vh; flex-shrink: 0; margin: 1vh;", 
                        bigelowshinytheme::bigelow_card(headerContent = "DMR Landings Map by County",
                                                        plotOutput("dmrMapOutput", 
                                                                      width = "100%", 
                                                                      height = "100%")))
                    )
                )
    ), #navset_bar
  ), #main body
  # Footer with bigelow logo
  bigelowshinytheme::bigelow_footer("Tandy Center for Ocean Forecasting and National Ocean Economics Program")
) # fluidPage

server <- function(input, output, session) {
  ###
  #  index plot
  ###
  index_name = reactive({
    input$Index
  })
  
  index_type = reactive({
    input$indexType
  })
  
  output$indexPlot <- renderPlot({
    index = index_name()
    type = index_type()
    
    switch(tolower(index[1]),
           "nao" = suppressWarnings(oame::plot_nao(NAO, type = type)),
           "amo" = suppressWarnings(oame::plot_amo(AMO, type = type)),
           "tides" = oame::plot_tide(TIDE, type = type))
  })
  
  ###
  #  dmrMap
  ###
  dmrMap_years = reactive({
    input$dmrMapYear
  }) 
  dmrMap_species = reactive({
    input$dmrMapSpecies
  }) 
  dmrMap_varname = reactive({
    input$dmrMapVariable
  }) 
  dmrMap_style = reactive({
    input$dmrMapStyle
  }) 
  
  output$dmrMapOutput <- renderPlot({
    years = dmrMap_years()
    if (!any(c("recent", "all") %in% years)) years = as.numeric(years)
    spp = dmrMap_species()
    varname = dmrMap_varname()
    style = dmrMap_style()
    
    suppressWarnings(oame::map_species_by_county(x = DMR,
                          spp = spp,
                          years = years,
                          varname = varname,
                          counties = COUNTIES,
                          style = style))
  })
  
}

shinyApp(ui, server)
