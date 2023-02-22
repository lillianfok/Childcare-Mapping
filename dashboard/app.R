
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

#===== Data =====#

childcare_census <- st_read("data/childcare_census.gpkg")
ma_tracts <- st_read("data/ma_tracts.gpkg")

children <- st_read("data/children_tracts.gpkg") %>%
  select(GEOID, pop)

ma_tracts <- ma_tracts %>% 
   rename("tract_geom" = "geom")


#===== Desert Map =====# 

#base map uses crs4326
st_transform(childcare_census, crs = 4326)

desert_binary <- childcare_census %>% 
  select(GEOID, desert, childcare_slots_per_tract, children_per_slot) %>% 
  st_drop_geometry() %>% 
  distinct()

#join ma tracts to binary indicator
desert_map <- ma_tracts %>% 
  left_join(desert_binary, by = "GEOID") %>% 
  left_join(children, by = "GEOID") %>%
  st_transform(crs = 4326) %>%
  mutate(children_per_slot = 
           case_when(is.na(children_per_slot) ~ pop,
                     TRUE ~ children_per_slot)) %>%
  replace_na(list(desert = "desert"))

#===== Choropleth Map =====# 

children_per_slot_map <- childcare_census %>% 
  select(GEOID, children_per_slot, childcare_slots_per_tract) %>%
  st_drop_geometry() %>%
  distinct()

children_per_slot_ma_map <- ma_tracts %>% 
  left_join(children_per_slot_map, by = "GEOID") %>% 
  left_join(children, by = "GEOID") %>%
  st_transform(crs = 4326) %>%
  mutate(children_per_slot = 
           case_when(is.na(children_per_slot) ~ pop,
                     TRUE ~ children_per_slot)) %>%
  replace_na(list(childcare_slots_per_tract = 0))

childcare_census_filtered <- childcare_census %>%
  select(name, address, city, county, minimum_age_range, maximum_age_range, total_licensed_capacity, tract) %>%
  distinct()


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

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem('Childcare Facilities', tabName = 'facility_maps', icon = icon('city'))
  )),
  dashboardBody(tabItems(
    #===== Dallas Map Tab =====#
    tabItem(tabName = 'facility_maps',
            fluidRow(
              box(
                width = 12, collapsible = T,
                title = 'Childcare Facilities Map',
                solidHeader = T, status = 'primary',
                leafletOutput('facilitiesMap')
              )
            ),
            fluidRow(
              box(
                width = 12, collapsible = T,
                title = 'Facilities',
                solidHeader = T, status = 'primary',
                dataTableOutput('facilitiesFinder')
              )
            )
    )
  ))
)


#===== UI =====#

# ui <- fluidPage(
#   
#   navbarPage("Childcare Map", theme = shinytheme("lumen"),
#              
#              tabPanel("Facility Locator", fluid = TRUE, icon = icon("globe-americas"),
#                       tags$style(button_color_css),
#                       # Sidebar layout with a input and output definitions
#                       sidebarLayout(
#                         #===== Main Panel - Map, Radio Buttons... =====#
#                         mainPanel(
#                           fluidRow(
#                             column(3, offset = 9,
#                                    
#                                    radioButtons(inputId = "displayOptions",
#                                                 label = "Display:",
#                                                 choices = c("Track", "Facilities", "Neither"),
#                                                 selected = "Track")
#                             )),
#                           withSpinner(leafletOutput("facilitiesMap")),
#                           hr(),
#                           fluidRow(column(7, helpText("Tip: ...brrrruh")),
#                           ),#fluidRow,
#                           br(),
#                           fluidRow(
#                             box(
#                               width = 12,
#                               title = "Childcare Facilities",
#                               solidHeader = T, status = 'primary',
#                               withSpinner(dataTableOutput(outputId = "facilitiesFinder"))
#                             )
#                           )
#                         ) #mainPanel
#                       )
#              ),
#              #===== Navbar Menu - About... =====#
#              navbarMenu("More", icon = icon("info-circle"),
#                         tabPanel("About", fluid = TRUE,
#                                  fluidRow(
#                                    column(6,
#                                           #br(),
#                                           h4(p("About the Project")),
#                                           h5(p("This project is intended to....")),
#                                           br(),
#                                           h5(p("words")),
#                                           br(),
#                                           h5(p("words"),
#                                              p("...."))
#                                           
#                                           #hr(),
#                                           
#                                    ),
#                                    column(6,
#                                           h4(p("About the Team")),
#                                           h5(p("words"),
#                                              p("words")
#                                           ),
#                                           br()
#                                    )
#                                  ),
#                                  br(),
#                                  hr(),
#                                  h5("Sources:"),
#                                  h6(
#                                    p("Swimming Information from ",
#                                      a("Source 1",
#                                        href = "https://www.usaswimming.org/Home/times/ncaa-information"))),
#                                  h6(
#                                    p("Source 2 ",
#                                      a("US News",
#                                        href = "https://www.usnews.com/best-colleges/rankings"))),
#                                  h5("Built with",
#                                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
#                                     "by",
#                                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
#                                     ".")
#                         )
#              )
#   )
#   
# )

#====================================================================================================

server <- function(input, output) { 
  
  output$facilitiesMap <- renderLeaflet({
    
    pioneer_valley_map <- leaflet(width = "100%") %>%
      setView(lat = 42.4000, lng = -72.5967, zoom = 9) %>%
      addProviderTiles(providers$Stamen.Toner)
  
    
    #set binary color palette
    pal <- colorFactor(palette = c("#800020", "#FCD299"), domain = desert_map$desert)
    
    #color palette
    chloro_bins <- c(0, 3, 10, 50, 100, 200)
    qpal <- colorBin("YlOrRd", children_per_slot_ma_map$children_per_slot, chloro_bins)

    #===== Labels =====# 
    
    #Desert Map
    desert_labels <- sprintf(
                        "<strong>Tract: </strong>%s<br/>
                        <strong>Children per licensed childcare slot: </strong>%s<br/>
                        <strong>Children under 5: </strong>%s<br/>
                        <strong>Childcare slots: </strong>%s<br/>", 
                        desert_map$NAME,
                        round(desert_map$children_per_slot, digits = 3),
                        desert_map$pop,
                        desert_map$childcare_slots_per_tract) %>%
                      lapply(htmltools::HTML)

    #Choropleth Map
    choro_labels <- sprintf(
                        "<strong>Tract: </strong>%s<br/>
                        <strong>Children per licensed childcare slot: </strong>%s<br/>
                        <strong>Children under 5: </strong>%s<br/>
                        <strong>Childcare slots: </strong>%s<br/>",
                        str_to_title(children_per_slot_ma_map$NAME),
                        round(children_per_slot_ma_map$children_per_slot, digits = 3),
                        children_per_slot_ma_map$pop,
                        children_per_slot_ma_map$childcare_slots_per_tract) %>%
                    lapply(htmltools::HTML)
    

    #Facilities
    facility_labels <- sprintf("<strong>Name: </strong>%s<br/>
                               <strong>Address: </strong>%s<br/>
                               <strong>County: </strong>%s<br/>
                               <strong>Childcare slots: </strong>%s<br/>
                               <strong>Age Range: </strong>%s - %s",
                          childcare_census_filtered$name,
                          str_to_title(childcare_census_filtered$address),
                          str_to_title(childcare_census_filtered$county),
                          childcare_census_filtered$total_licensed_capacity,
                          childcare_census_filtered$minimum_age_range,
                          childcare_census_filtered$maximum_age_range) %>%
                       lapply(htmltools::HTML)
    
    
    #===== Output Map =====# 
    pioneer_valley_map %>% 
      addMapPane("circles", zIndex=430) %>% 
      addMapPane("maps", zIndex=420) %>% 
      addCircleMarkers(data = childcare_census_filtered,
               radius = 2,
               weight = 1,
               color = "black",
               label = facility_labels,
               options = pathOptions(pane = "circles"),
               group = "Show Facilities") %>% 
      addPolygons(data = desert_map, 
               weight = 0.5, 
               fillColor = ~pal(desert_map$desert), 
               fillOpacity = 0.6, 
               label = desert_labels,
               options = pathOptions(pane = "maps"),
               group = "Desert Map") %>% 
      addLegend("bottomright", 
               pal = pal,
               values = desert_map$desert,
               title = "Designation",
               opacity = 1,
               group = "Desert Map") %>% 
      addPolygons (data = children_per_slot_ma_map, 
               weight = 1, 
               color = "black",
               fillColor = ~qpal(children_per_slot),
               fillOpacity = 0.4, 
               label = choro_labels,
               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#ffffff",
                                   fillOpacity = 0.8,
                                   bringToFront = TRUE),
               options = pathOptions(pane = "maps"),
               group = "Choropleth Map"
      ) %>% 
      addLegend ("bottomright", 
                 pal = qpal, 
                 values = as.double(children_per_slot_ma_map$children_per_slot),
                 title = "Children Per Slot",
                 opacity = 1,
                 group = "Choropleth Map") %>% 
      addLayersControl(
        overlayGroups = c("Show Facilities"),
        baseGroups = c("Choropleth Map", "Desert Map"),
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
        childcare_census_filtered %>% 
        st_drop_geometry() %>% 
        rename(slots = total_licensed_capacity) %>%
        mutate(age_range = paste(minimum_age_range, "-", maximum_age_range)) %>%
        select(name, address, slots, age_range)    # Not filtered
      else
        parcels
      #childcare_census %>% filter( county==click_county())
    )
  })
}
shinyApp(ui, server)