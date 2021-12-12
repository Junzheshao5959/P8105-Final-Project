library(tidyverse)
library(data.table)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(RJSONIO)
library(rgdal)
library(shinydashboard)
library(DT)
library(rlist)
library(shinyalert)
library(dtplyr)
setDTthreads(threads = 4)

#get manhattan geojson
manhattan <- rgdal::readOGR("data/nyc_taxi_zone.geojson")

manhattan_sub <- manhattan[manhattan$borough == "Manhattan",]
manhattan_sub$id <- 1:69

#temporary manhattan map data
manhattan_map_data <- manhattan[manhattan$borough == "Manhattan",]
manhattan_map_data$id <- 1:69

zone_list <- manhattan_map_data$zone
test_dt <- fread("data/test_dt_year.csv")
test_mta_dt <- fread("data/test_dt_mta.csv")
test_dt <- bind_rows(test_dt,test_mta_dt)
test_dt <- test_dt[,hour := unclass(as.POSIXlt(tpep_pickup_datetime))$hour]

#calculate mean speed of each zone
calculate_mean_speed <- function(PU,Type,month_choose,day_choose,time_range) {
  mean_speed_list <- c()
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculating Velocity", value = 0)

  for (zone in zone_list) {
    if (Type == "MTA") {
      mean_speed <- test_dt[PUZone == PU][DOZone == zone][type == Type][week %in% day_choose][hour %in% time_range]
    }
    else {
      mean_speed <- test_dt[PUZone == PU][DOZone == zone][type == Type][month %in% month_choose][week %in% day_choose][hour %in% time_range]
    }
    print(mean_speed)
    progress$inc(1/69, detail = zone)
    mean_speed_list <- c(mean_speed_list, mean(mean_speed[,velocity],na.rm = TRUE)*3.6)
  }
  return(replace(mean_speed_list,is.nan(mean_speed_list),NA))
}

month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
time <- c("Rush Hour AM","Midday","Rush Hour PM","Evening","Early Morning")
time_start_dictionary <- c("Rush Hour AM" = 7, "Midday" = 10, "Rush Hour PM" = 16, "Evening" = 19, "Early Morning" = 0)
time_end_dictionary <- c("Rush Hour AM" = 10, "Midday" = 16, "Rush Hour PM" = 19, "Evening" = 24, "Early Morning" = 7)

#design
ui <- dashboardPage(
  dashboardHeader(title = "Velocity Heatmap",
                  tags$li(a(href = 'http://www.google.com',icon("times-circle","fa-3x"),
                            title = "Back to Website"),class = "dropdown")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      useShinyalert(),
      column(width = 8,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("manhattan_map", height = "85vh")
             )
      ),
      column(width = 4,
             radioButtons("type", "Vehicle Type",c("Bike" = "bike","Taxi" = "taxi","Subway" = "MTA"), inline = TRUE),
             sliderTextInput("month", "Month",choices = month, selected = month[c(1:12)],grid = TRUE),
             sliderTextInput("day","Day",choices = day, selected = day[c(1:7)],grid = TRUE),
             checkboxGroupInput("time","Time",choices = time, selected = time, inline = TRUE),
             actionButton("button", "Search")
      )
    )
  )
)

#server
server <- function(input, output, session) {

  #color pal
  color_pal <- reactiveValues()
  color_pal <- NULL

  #map data
  map_data <- reactiveValues()
  map_data$choice <- manhattan_map_data
  map_data$speed <- NULL

  #PU zone
  pu_zone <- reactiveValues()
  pu_zone$zone <- NULL
  pu_zone$speed <- NULL

  #vehicle type
  type <- reactiveValues()
  type <- NULL

  #record the chose zone id
  choice <- reactiveValues()
  choice$id <- NULL

  #get month
  month_choose <- reactiveValues()
  month_choose <- NULL

  #get day
  day_choose <- reactiveValues()
  day_choose <- NULL

  #get time
  time_choose <- reactiveValues()
  time_choose <- NULL

  time_range <- reactiveValues()
  time_range <- NULL

  observeEvent(input$button, {
    type <- input$type
    month_choose <- input$month
    day_choose <- input$day
    time_choose <- input$time

    print(type)
    print(month_choose)
    print(day_choose)
    print(time_choose)

    month_choose <- c(which(month == month_choose[1]):which(month == month_choose[2]))
    day_choose <- c(which(day == day_choose[1]):which(day == day_choose[2]))
    day_choose <- replace(day_choose,day_choose == 7,0)

    for (i in time_choose) {
      time_range <- c(time_start_dictionary[[i]]:(time_end_dictionary[[i]] - 1),time_range)
    }

    print(type)
    print(month_choose)
    print(day_choose)
    print(time_range)


    if (is.null(pu_zone$zone)) {
      shinyalert(title = "Warning", text = "Please choose one zone on the map!", type = "error", html = TRUE)
      return(0)
    }
    #calculate mean speed
    pu_zone$zone <- manhattan_map_data[manhattan_map_data$id == choice$id,]$zone
    map_data$speed <- calculate_mean_speed(pu_zone$zone,type,month_choose,day_choose,time_range)
    pu_zone$speed <- map_data$speed[choice$id]

    print(map_data$speed)

    if (sum(is.na(map_data$speed)) == 69) {
      color_pal <- colorNumeric(colorRamp(c('blue', 'white','red')),
                                domain = c(0:10),
                                na.color = "#000000")
    }
    else {
      color_pal <- colorNumeric(colorRamp(c('blue', 'white','red')),
                                domain = map_data$speed,
                                na.color = "#000000",)
    }

    #add layer
    leafletProxy("manhattan_map") %>%
      addPolygons(data = manhattan_map_data, color = "white", fillOpacity = 2,
                  fillColor = ~color_pal(map_data$speed), group = "manhattan_heat",
                  label = paste(manhattan_map_data$zone, round(map_data$speed,2), "km/h", sep = "\n"),layerId = manhattan_map_data$id) %>%
      addLegend(position = "bottomright", pal = color_pal, values = map_data$speed, title = "Speed (km/h)", opacity = 2, layerId = "speed") %>%
      addPolygons(data = map_data$choice, color = "#00FF00", layerId = "choice", label = paste(pu_zone$zone, round(pu_zone$speed,2), "km/h", sep = "\n"))
  }
  )

  #respond to click
  observeEvent(input$manhattan_map_shape_click, {
    click <- input$manhattan_map_shape_click
    print(click)

    if (is.numeric(click$id)) { #check whether click "from" or "to" layer
      choice$id <- click$id
      map_data$choice <- manhattan_map_data[manhattan_map_data$id == click$id,]
    }
    else {
      map_data$choice <- manhattan_map_data[manhattan_map_data$id == choice$id,]
    }

    pu_zone$zone <- manhattan_map_data[manhattan_map_data$id == choice$id,]$zone

    #add layer
    leafletProxy("manhattan_map") %>%
      addPolygons(data = map_data$choice, color = "#00FF00", layerId = "choice", label = pu_zone$zone)
  })


  #initially draw map
  output$manhattan_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = manhattan_map_data,
                  color = "white",
                  smoothFactor = 0.5,
                  weight = 1,
                  group = "manhattan",
                  fillColor = topo.colors(20),
                  highlightOptions = highlightOptions(color = "#444444", weight = 2, bringToFront = TRUE),
                  dashArray = 3,
                  label = manhattan_map_data$zone,
                  layerId = manhattan_map_data$id)
  })
}

shinyApp(ui, server)
