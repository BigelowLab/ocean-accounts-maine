

suppressPackageStartupMessages({
  library(oame)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(leaflet)
  library(dplyr)
})


OCEAN_ECON = oame::read_ocean_economy() |>
  dplyr::mutate(gdp = gdp/1000000000,
                rgdp = rgdp/1000000000,
                wages = wages/1000000000)
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
    col_widths = c(12, 12),   # two full-width rows stacked
    row_heights = c(1, 2),    # bottom card is twice the height of the top row
    gap = "1rem",
    
    # Top: four equal cards in a single row
    layout_columns(
      col_widths = c(4,4,4),
      gap = "1rem",
      
      # Giving a card an id makes bslib report input$<id>_full_screen
      card(id = "blue_economy_card", 
           full_screen = TRUE,
           card_header("Blue Economy"),
           card_body(uiOutput("blue_economy_body", fill = TRUE))),
      
      card(id = "activity_lease_card", 
           full_screen = TRUE,
           card_header("Activity & Leases"),
           card_body(uiOutput("activity_lease_body", fill=TRUE))),
      
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
  
  output$ocean_econ_plot = renderPlot({
    suppressWarnings(oame::plot_ocean_economy(ocean_econ_data(), y_var = input$oe_var))
  })
  
  output$blue_economy_body <- renderUI({
    if (isTRUE(input$blue_economy_card_full_screen)) {
      tagList(selectInput("oe_area",
                          "Choose an area",
                          choices = unique(OCEAN_ECON$county),
                          selected = "Maine State"),
              selectInput("oe_var",
                          "Choose a variable",
                          choices = c("gdp", "rgdp", "establishments", "employment", "wages"),
                          selected = "gdp"),
              plotOutput("ocean_econ_plot")
              )
    } else {
      plotOutput("ocean_econ_plot")
    }
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
  
  output$activity_lease_body <- renderUI({
    if (isTRUE(input$activity_lease_card_full_screen)) {
      tagList(selectInput("dmrMapSpecies",
                          "Choose species",
                          choices = DMR$species |> unique() |> sort(),
                          selected = "Lobster American"),
              selectInput("dmrMapVariable",
                          "Choose a variable",
                          choices=c("weight", "value", "trip_n", "harv_n"),
                          selected="value"),
              plotOutput("dmr_timeseries")
      )
    } else {
      plotOutput("dmr_timeseries")
    }
  })
  
  output$research_map = renderLeaflet({
    oame::map_labs(LABS)
  })
}

shinyApp(ui, server)