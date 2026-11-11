

suppressPackageStartupMessages({
  library(oame)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(leaflet)
  library(dplyr)
  library(plotly)
})


OCEAN_ECON = oame::read_ocean_economy() 
DMR = oame::read_dmr_landings("modern") |>
  oame::prep_dmr_landings_county()
LABS = oame::read_labs()

ui <- page_fillable(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  bigelow_header(h2("Ocean Accounts Maine")),
  padding = "1rem",
  gap = "1rem",
  layout_columns(
    col_widths = c(5,7),   # two full-width rows stacked
    #row_heights = c(1, 2),    # bottom card is twice the height of the top row
    gap = "1rem",

    # Top: four equal cards in a single row
    layout_columns(
      col_widths = c(12,12,12),
      row_heights = c(1,1,1),
      gap = "1rem",
      
      # Giving a card an id makes bslib report input$<id>_full_screen
      card(id = "blue_economy_card", 
           full_screen = TRUE,
           card_header("Blue Economy",
                       toolbar(
                         align = "right",
                         toolbar_input_select(
                           id = "oe_var",
                           label = "Variable",
                           choices = c("GDP" = "gdp", "Real GDP" = "rgdp", "Establishments" = "establishments", "Emplyment" = "employment", "Wages" = "wages"),
                           selected="gdp",
                           icon = icon("filter")
                         ),
                         toolbar_input_select(
                           id = "oe_area",
                           label = "Area",
                           choices = unique(OCEAN_ECON$county),
                           selected="Maine State",
                           icon = icon("filter")
                         )
                       )),
           card_body(plotlyOutput("ocean_econ_plot"))),
      
      card(id = "activity_lease_card", 
           full_screen = TRUE,
           card_header("Activity & Leases",
                       toolbar(
                         align = "right",
                         toolbar_input_select(
                           id = "dmrMapSpecies",
                           label = "Species",
                           choices = unique(DMR$species),
                           selected="Lobster American",
                           icon = icon("fish")
                         ),
                         toolbar_input_select(
                           id = "dmrMapVariable",
                           label = "Variable",
                           choices = c("Weight" = "weight", "Value" = "value", "Trips" = "trip_n", "Harvesters" = "harv_n"),
                           selected="value",
                           icon = icon("filter")
                         )
                         )),
           card_body(plotOutput("dmr_timeseries"))),
      
      card(full_screen = TRUE,
           card_header("Ecosystem Funding"))),
    
      card(full_screen = TRUE,
         card_header("Key Infrastructure"),
         card_body(leafletOutput("research_map")))
  ),
  bigelowshinytheme::bigelow_footer("Maine Technology Institute")
)


server <- function(input, output, session) {
  
  # Blue Economy Card
  ocean_econ_data = reactive({
    if (input$oe_area == "Maine State") {
      dplyr::filter(OCEAN_ECON, 
                    county %in% input$oe_area,
                    industry == "Total")
      #sector %in% input$oe_sector, 
      #!sector == "Ocean Economy")
    } else {
      dplyr::filter(OCEAN_ECON, 
                    county %in% input$oe_area)
    }
  })
  

  output$ocean_econ_plot = renderPlotly({
    suppressWarnings(oame::plot_ocean_economy_i(ocean_econ_data(), y_var = input$oe_var))
  })
  
  ## DMR Card
  
  dmrMap_species = reactive({
    input$dmrMapSpecies
  }) 
  dmrMap_varname = reactive({
    input$dmrMapVariable
  }) 
  
  output$dmr_timeseries = renderPlot({
    spp = dmrMap_species()
    varname = dmrMap_varname()
    suppressWarnings(oame::plot_landings(x = DMR, spp=spp, varname = varname))
    
  })
  
  output$research_map = renderLeaflet({
    oame::map_labs(LABS)
  })
}

shinyApp(ui, server)