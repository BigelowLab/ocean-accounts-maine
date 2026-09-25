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
HURDAT = oame::read_hurdat()
COAST = oame::read_coast()
OCEAN_ECON = oame::read_ocean_economy() |>
  dplyr::mutate(gdp = gdp/1000000000,
                rgdp = rgdp/1000000000,
                wages = wages/1000000000)


ui <- shiny::fluidPage(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  bigelow_header(h2("Ocean Accounts - Maine")),
  bigelow_main_body(
  
    bslib::navset_tab(
      nav_panel("Climatology", 
                fluidRow(
                selectInput("Index",
                            "Choose an index to plot",
                            choices= c("AMO", "NAO", "Tides"),
                            selected= "AMO"),
                selectInput("indexType",
                            "Choose a plot style",
                            choices = c("timeseries", "monthly", "climatology"),
                            selected = "timeseries")
                ),
                div(style = "height: 70vh; overflow-x: auto; display: flex;",
                    div(style = "width: 68vh; flex-shrink: 0; margin: 1vh;", 
                        bigelowshinytheme::bigelow_card(headerContent = "Climatology",
                                                        plotOutput("indexPlot", width = "100%", height = "100%")))
                  )),
      nav_panel("DMR Landings Map", 
                fluidRow(
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
                ),
                div(style = "height: 70vh; overflow-x: auto; display: flex;",
                    div(style = "width: 68vh; flex-shrink: 0; margin: 1vh;", 
                        bigelowshinytheme::bigelow_card(headerContent = "DMR Landings Map by County",
                                                        plotOutput("dmrMapOutput", 
                                                                      width = "100%", 
                                                                      height = "100%")))
                    )
                ),
      nav_panel("Hurricanes", 
                fluidRow(
                  selectInput("hurdatEpoch",
                              "Choose years of epoch",
                              choices = c(10, 25, 50, 100),
                              selected = 25),
                  selectInput("hurdatVariable",
                              "Choose a variable",
                              choices=c("wind_max_sus", "duration", "min_pres"),
                              selected="wind_max_sus")
                ),
                div(style = "height: 70vh; overflow-x: auto; display: flex;",
                    div(style = "width: 68vh; flex-shrink: 0; margin: 1vh;", 
                        bigelowshinytheme::bigelow_card(headerContent = "NOAA Hurricane Data",
                                                        plotOutput("hurdatOutput", 
                                                                   width = "100%", 
                                                                   height = "100%")))
                )
      ), #hurricanes
      nav_panel("Ocean Economy",
                layout_sidebar(
                  sidebar = sidebar(
                    selectInput("oe_area",
                                "Choose an area",
                                choices = unique(OCEAN_ECON$county),
                                selected = "Maine State"),
                    selectInput("oe_var",
                                "Choose a variable",
                                choices = c("gdp", "rgdp", "establishments", "employment", "wages"))
                    #selectInput("oe_sector",
                    #            "Choose a sector",
                    #            choices=unique(OCEAN_ECON$sector))
                  ),
                  bigelowshinytheme::bigelow_card(headerContent = "Maine Ocean Economy",
                                                  plotOutput("ocean_econ_plot"))
                )
      ) #ocean economy sb
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
  
  ### 
  #  hurdat
  ###
  hurdat_epoch =  reactive({
    input$hurdatEpoch
  }) 
  
  hurdat_variable =  reactive({
    input$hurdatVariable
  }) 
  
  output$hurdatOutput <- renderPlot({
    epoch = hurdat_epoch()
    color_by = hurdat_variable()
    oame::map_hurdat(x = HURDAT, 
               epoch = epoch,
               color_by = color_by,
               counties = COUNTIES,
               coast = COAST)
  })
  
  
  ocean_econ_data = reactive({
    
    if (input$oe_area == "Maine State") {
      dplyr::filter(OCEAN_ECON, 
                    county %in% input$oe_area,
                    industry == "Total",
                    #sector %in% input$oe_sector, 
                    !sector == "Ocean Economy")
    } else {
      dplyr::filter(OCEAN_ECON, 
                    county %in% input$oe_area, 
                    #sector %in% input$oe_sector, 
                    !sector == "Ocean Economy")
    }
  })
  
  output$ocean_econ_plot = renderPlot({
      oame::plot_ocean_economy(ocean_econ_data(), y_var = input$oe_var)
  })
}

shinyApp(ui, server)
