

suppressPackageStartupMessages({
  library(oame)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(leaflet)
  library(dplyr)
  library(plotly)
  library(shinyWidgets)
  library(bsicons)
  library(scales)
})

OCEAN_ECON = oame::read_ocean_economy() 
DMR = oame::read_dmr_landings("modern") |>
  oame::prep_dmr_landings_county()
LABS = oame::read_labs()

filter_choices <- sort(unique(LABS$category))
pal <- colorFactor(c("#2C6E8F", "#E0A030", "#5B8C3A"), domain = filter_choices)


ui <- page_fillable(
  theme = bigelowshinytheme::bigelow_theme(),
  #includeCSS("www/additionalStyles.css"),
  bigelow_header(h2("Ocean Accounts Maine"),
                 #div(
                 #  class = "btn-group",
                 #  role = "group",
                 #  actionButton("left", "Map"),
                 #  actionButton("middle", "Split"),
                 #  actionButton("right", "Dashboard")
                 #),
                 radioGroupButtons(inputId="ui_select", label=NULL, choices=c("Map", "Split", "Dashboard"))
  ),
  padding = "1rem",
  gap = "1rem",
  uiOutput("ui_layout"),
  bigelowshinytheme::bigelow_footer("Maine Technology Institute")
)

server <- function(input, output, session) {

  output$ui_layout = renderUI({
    switch(input$ui_select,
           "Map" = {
             layout_sidebar(
               sidebar = sidebar(
                 virtualSelectInput(inputId="map_filter",
                                    label = "Map Filter",
                                    choices=list(
                                      "Research"= c("Non-profit", "Academic", "State Agency", "Investment")
                                    ),
                                    multiple=TRUE,
                                    selected=c("Non-profit", "Academic", "State Agency", "Investment"),
                                    keepAlwaysOpen=TRUE)
               ),
               leafletOutput("research_map", height=800)
             )
           },
           "Split" = {
             page_fillable(
               layout_columns(
                 fill = FALSE, gap = "1rem",
                 value_box(
                   title = "Ocean economy GDP",
                   value = textOutput("gdp_val"),
                   showcase = icon("location-dot"),
                   theme = "primary"
                 ),
                 value_box(
                   title = "Total establishments",
                   value = textOutput("estab_val"),
                   showcase = icon("location-dot"),
                   theme = "primary"
                 ),
                 value_box(
                   title = "Total jobs",
                   value = textOutput("job_val"),
                   showcase = icon("location-dot"),
                   theme = "primary"
                 ),
                 value_box(
                   title = "Total wages",
                   value = textOutput("wage_val"),
                   showcase = icon("location-dot"),
                   theme = "primary"
                 )
               ),
               layout_columns(
                 col_widths = c(6,6), 
                 #row_heights = c(1, 2),
                 gap = "1rem",
                 layout_columns(
                   col_widths = c(12,12,12),
                   row_heights = c(1,1,1),
                   gap = "1rem",
                   
                   card(id = "blue_economy_card", 
                        full_screen = TRUE,
                        card_header("Blue Economy",
                                    tooltip(bs_icon("info-circle"),"Blue Economy timeseries information"),
                                    toolbar(align = "right",
                                            toolbar_input_select(
                                              id = "oe_var",
                                              label = "Variable",
                                              choices = c("GDP" = "gdp", 
                                                          "Real GDP" = "rgdp", 
                                                          "Establishments" = "establishments", 
                                                          "Emplyment" = "employment", 
                                                          "Wages" = "wages"),
                                              selected="gdp",
                                              icon = icon("filter")),
                                            toolbar_input_select(
                                              id = "oe_area",
                                              label = "Area",
                                              choices = unique(OCEAN_ECON$county),
                                              selected="Maine State",
                                              icon = icon("filter")))),
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
                                        icon = icon("fish")),
                                      toolbar_input_select(
                                        id = "dmrMapVariable",
                                        label = "Variable",
                                        choices = c("Weight" = "weight", "Value" = "value", "Trips" = "trip_n", "Harvesters" = "harv_n"),
                                        selected="value",
                                        icon = icon("filter")))),
                        card_body(plotOutput("dmr_timeseries"))),
                   
                   card(full_screen = TRUE,
                        card_header("Ecosystem Funding"))),
                 
                 card(full_screen = TRUE,
                      card_header("Key Infrastructure"),
                      layout_sidebar(
                        sidebar = sidebar(
                          virtualSelectInput(inputId="map_filter",
                                             label = "Map Filter",
                                             choices=list(
                                               "Research"= c("Non-profit", "Academic", "State Agency", "Investment")
                                             ),
                                             multiple=TRUE,
                                             selected=c("Non-profit", "Academic", "State Agency", "Investment"),
                                             keepAlwaysOpen=TRUE)
                        ),
                        leafletOutput("research_map")))
               )
             )
           },
           "Dashboard" = {
             layout_column_wrap(
               width=1,
               gap = "1rem",
               card(id = "blue_economy_card", 
                    full_screen = TRUE,
                    card_header("Blue Economy",
                                toolbar(align = "right",
                                        toolbar_input_select(
                                          id = "oe_var",
                                          label = "Variable",
                                          choices = c("GDP" = "gdp", 
                                                      "Real GDP" = "rgdp", 
                                                      "Establishments" = "establishments", 
                                                      "Emplyment" = "employment", 
                                                      "Wages" = "wages"),
                                          selected="gdp",
                                          icon = icon("filter")),
                                        toolbar_input_select(
                                          id = "oe_area",
                                          label = "Area",
                                          choices = unique(OCEAN_ECON$county),
                                          selected="Maine State",
                                          icon = icon("filter")))),
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
                                    icon = icon("fish")),
                                  toolbar_input_select(
                                    id = "dmrMapVariable",
                                    label = "Variable",
                                    choices = c("Weight" = "weight", "Value" = "value", "Trips" = "trip_n", "Harvesters" = "harv_n"),
                                    selected="value",
                                    icon = icon("filter")))),
                    card_body(plotOutput("dmr_timeseries"))),
               
               card(full_screen = TRUE,
                    card_header("Ecosystem Funding")))
           })
  })
  
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
  
  gdp = reactive({
    d = dplyr::filter(ocean_econ_data(), sector == "Ocean Economy", year == max(.data$year))
    max(d$gdp)
  })
  
  output$gdp_val = renderText(label_currency()(gdp()))
  
  total_establishments = reactive({
    d = dplyr::filter(ocean_econ_data(), sector == "Ocean Economy", year == max(year))
    max(d$establishments)
  })
  
  output$estab_val = renderText(total_establishments())
  
  total_jobs = reactive({
    d = dplyr::filter(ocean_econ_data(), sector == "Ocean Economy", year == max(year))
    max(d$employment)
  })
  
  output$job_val = renderText(total_jobs())
  
  total_wages = reactive({
    d = dplyr::filter(ocean_econ_data(), sector == "Ocean Economy", year == max(year))
    max(d$wages)
  })
  
  output$wage_val = renderText(label_currency()(total_wages()))

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
  
  ## Map
  
  mapdata = reactive({
    dplyr::filter(LABS, category %in% input$map_filter)
  })
  
  observe({
    d <- mapdata()
    proxy <- leafletProxy("research_map") |> clearMarkers()
    if (nrow(d) == 0) return()
    
    entry <- sprintf(
      '<b><a href="%s" target="_blank">%s</a></b><br>%s',
      d$website, htmltools::htmlEscape(d$name),
      htmltools::htmlEscape(d$address)
    )
    groups <- split(seq_len(nrow(d)), paste(d$lat, d$long))
    m <- data.frame(
      lat   = sapply(groups, function(i) d$lat[i[1]]),
      long  = sapply(groups, function(i) d$long[i[1]]),
      type  = sapply(groups, function(i) d$category[i[1]]),
      popup = sapply(groups, function(i) paste(entry[i], collapse = "<hr>"))
    )
    
    proxy |>
      addCircleMarkers(
        lng = m$long, lat = m$lat, popup = m$popup,
        radius = 7, weight = 1.5, color = "white",
        fillColor = pal(m$type), fillOpacity = 0.9
      )
  })
  
  output$research_map = renderLeaflet({
    oame::map_labs(mapdata())
  })
}

shinyApp(ui, server)