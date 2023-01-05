#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#===== Library Stuff =====#


library(shiny)
library(shinydashboard)
library(tidycensus)
library(leaflet)
library(sf)
library(htmlwidgets)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(stringr)


#childcare_census <- st_read("data/childcare_census.gpkg")
#ma_tracts <- st_read("data/ma_tracts.gpkg")
#====================================================================================================

#===== Extra Stuff =====#
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/> children per slot", childcare_census$children_per_slot) %>% 
#   lapply(htmltools::HTML)
# 
# pal <- colorBin(palette = "OrRd", 5, domain = childcare_census$childcare_slots_per_tract)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# ma_tracks <- ma_tracts %>% 
#   rename("tract_geom" = "geom")
# 
# facilities <- childcare_census[c("name", "city", "tract")]
# facilities_df <- st_join(facilities, ma_tracts)


#====================================================================================================


####-------------- map stuff #### 

# map_interactive <- childcare_census %>% 
#   leaflet() %>% 
#   addProviderTiles(provider = "CartoDB.Positron") %>%
#   setView(lat = 42.40, lng = -72.59, zoom = 10) %>% 
#   
#   addPolygons(data = childcare_census$geometry.x,
#               label = labels,
#               weight = 1,
#               color = "#660000",
#               fillColor = pal(childcare_census$childcare_slots_per_tract)) %>% 
#   addLegend("bottomright",
#             pal = pal,
#             values = ~ childcare_census$childcare_slots_per_tract,
#             title = "Childare slots per tract",
#             opacity = 0.7) #%>% 
# #addMarkers()
# 
# map_interactive
# 
# saveWidget(map_interactive, "childcare_map.html")



#===== Test Map =====#

# map_interactive <- childcare_census %>% 
#   leaflet() %>% 
#   addProviderTiles(provider = "CartoDB.Positron") %>%
#   setView(lat = 42.40, lng = -72.59, zoom = 10) %>% 
#   
#   addPolygons(data = childcare_census$geometry.x,
#               label = labels,
#               weight = 1,
#               color = "#660000",
#               fillColor = pal(childcare_census$childcare_slots_per_tract)) %>% 
#   addLegend("bottomright",
#             pal = pal,
#             values = ~ childcare_census$childcare_slots_per_tract,
#             title = "Childare slots per tract",
#             opacity = 0.7)
# 
# map_interactive
# 
# saveWidget(map_interactive, "childcare_map.html")

#====================================================================================================

#===== UI =====#

ui <- fluidPage(
  
  navbarPage("Childcare Map", theme = shinytheme("lumen"),
             
             tabPanel("Facility Locator", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          #===== Sidebar Panel - options, etc. =====#
                          titlePanel("Facility Options"),
                          fluidRow(column(3,
                                          
                                          # Select which Gender(s) to plot
                                          checkboxGroupInput(inputId = "Option1",
                                                             label = "Select county(s):",
                                                             choices = c("Franklin", "Hampden", "Hampshire"),
                                                             selected = "Franklin"),
                                          
                                          # Select which Division(s) to plot
                                          checkboxGroupInput(inputId = "Option2",
                                                             label = "More ting(s):",
                                                             choices = c("A", "B", "C"),
                                                             selected = "A")
                          ),
                          ),
                        ),
                        #===== Main Panel - Map, Radio Buttons... =====#
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,
                                   
                                   radioButtons(inputId = "displayOptions",
                                                label = "Display:",
                                                choices = c("Track", "Facilities", "Neither"),
                                                selected = "Track")
                            )),
                          withSpinner(leafletOutput("facilitiesMap")),
                          hr(),
                          fluidRow(column(7, helpText("Tip: ...brrrruh")),
                          ),#fluidRow,
                          br(),
                          fluidRow(
                            box(
                              width = 12,
                              title = "Childcare Facilities",
                              solidHeader = T, status = 'primary',
                              withSpinner(dataTableOutput(outputId = "facilitiesFinder"))
                            )
                          )
                        ) #mainPanel
                      )
             ),
             #===== Navbar Menu - About... =====#
             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("This project is intended to....")),
                                          br(),
                                          h5(p("words")),
                                          br(),
                                          h5(p("words"),
                                             p("...."))
                                          
                                          #hr(),
                                          
                                   ),
                                   column(6,
                                          h4(p("About the Team")),
                                          h5(p("words"),
                                             p("words")
                                          ),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Sources:"),
                                 h6(
                                   p("Swimming Information from ",
                                     a("Source 1",
                                       href = "https://www.usaswimming.org/Home/times/ncaa-information"))),
                                 h6(
                                   p("Source 2 ",
                                     a("US News",
                                       href = "https://www.usnews.com/best-colleges/rankings"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    "by",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                    ".")
                        )
             )
  )
  
)

#====================================================================================================

server <- function(input, output) { 
  
  output$facilitiesMap <- renderLeaflet({
    
    #===== Design Stuff =====#
    
    # Prepare the text for tooltips:
    labels <- paste(
      childcare_census$tract,"<br/>", 
      "Children per licensed childcare slot: <strong>", 
      round(childcare_census$children_per_slot, digits = 3), "</strong><br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    pal <- colorBin(palette = "Blues", 5, domain = childcare_census$childcare_slots_per_tract)
    
    #===== Map =====# 
    leaflet(childcare_census) %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      setView(lat = 42.40, lng = -72.59, zoom = 9)%>% 
      addPolygons(data = ma_tracts,
                  label = labels,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  ),
                  weight = 0.5,
                  #fillColor = pal,
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(color="red", weight=3, bringToFront = TRUE),
                  color = "#660000") %>%
      addMarkers(label=childcare_census$name, 
                 labelOptions = labelOptions( 
                   style = list("font-weight" = "normal", padding = "3px 8px"), 
                   textsize = "13px", 
                   direction = "auto"),
                 options = markerOptions()
                 group="Individual Facilities") %>% 
      # addCircleMarkers(data = childcare_census,
      #                  radius = 4,
      #                  weight = 1,
      #                  color = "black",
      #                  label = ~name,
      #                  labelOptions = labelOptions( 
      #                    style = list("font-weight" = "normal", padding = "3px 8px"), 
      #                    textsize = "13px", 
      #                    direction = "auto"),
      #                  group = "Individual Facilities") %>%
      # addPolygons(data = desert_map, 
      #             weight = 0.5, 
      #             fillColor = ~pal(desert_map$desert), 
      #             fillOpacity = 0.6, 
      #             label = labels,
      #             group = "Identify Deserts") %>% 
      # addLegend("bottomright", 
      #           pal = pal,
      #           values = desert_map$desert,
      #           title = "Designation",
      #           opacity = 1,
      #           group = "Identify Deserts") %>% 
      # addPolygons (data = children_per_slot_ma_map, 
      #              weight = 1, 
      #              color = "black",
      #              fillColor = ~qpal(children_per_slot),
      #              fillOpacity = 0.5, 
      #              group = "Childcare Per Childcare Slot", 
      #              options = pathOptions(),
      #              label = labels,
      #              highlightOptions = highlightOptions(
      #                weight = 5,
      #                color = "#ffffff",
      #                fillOpacity = 0.8,
      #                bringToFront = TRUE,
      #                group = "Choropleth Map")) %>%
      addLayersControl(
        overlayGroups = c("Individual Facilities"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  }) #map
  
  #===== Table Stuff =====#
  
  click_county <- reactiveVal()
  
  observeEvent(input$facilitiesMap_shape_click,
               {
                 if (!is.null(click_county()) && click_county() == input$facilitiesMap_shape_click$id)
                   click_county(NULL)
                 else
                   click_county(input$facilitiesMap_shape_click$id)
               })
  #  data table
  
  print(click_county)
  
  output$facilitiesFinder <- DT::renderDataTable({
    DT::datatable(
      if(is.null(click_county()))
        childcare_census    # Not filtered
      else
        parcels
      #childcare_census %>% filter( county==click_county())
    )
  })
}
shinyApp(ui, server)