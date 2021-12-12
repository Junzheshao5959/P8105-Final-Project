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

#read data
test_dt <- fread("data/test_dt_year.csv")
test_mta_dt <- fread("data/test_dt_mta.csv")
test_dt <- bind_rows(test_dt,test_mta_dt)

test_dt <- test_dt[,hour := unclass(as.POSIXlt(tpep_pickup_datetime))$hour]

#get manhattan geojson
manhattan <- rgdal::readOGR("data/nyc_taxi_zone.geojson")

manhattan_sub <- manhattan[manhattan$borough == "Manhattan",]
manhattan_sub$id <- 1:69

#temporary manhattan map data
manhattan_map_data <- manhattan[manhattan$borough == "Manhattan",]
manhattan_map_data$id <- 1:69

#dictionary for map
month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
time <- c("Rush Hour AM","Midday","Rush Hour PM","Evening","Early Morning")
time_start_dictionary <- c("Rush Hour AM" = 7, "Midday" = 10, "Rush Hour PM" = 16, "Evening" = 19, "Early Morning" = 0)
time_end_dictionary <- c("Rush Hour AM" = 10, "Midday" = 16, "Rush Hour PM" = 19, "Evening" = 24, "Early Morning" = 7)

#test function
test_function <- function(month_in, week_in, data = test_dt, PU, DO, min_distance, max_distance){
  x = test_dt[month %in% month_in][week %in% week_in][type == 'taxi'][PUZone == PU][DOZone == DO][trip_distance >= 1000 * min_distance][trip_distance <= 1000 * max_distance][,.(velocity,type)]
  print(nrow(x))
  y = test_dt[month %in% month_in][week %in% week_in][type == 'bike'][PUZone == PU][DOZone == DO][trip_distance >= 1000 * min_distance][trip_distance <= 1000 * max_distance][,.(velocity,type)]
  print(nrow(y))
  z = test_dt[month %in% month_in][week %in% week_in][type == 'MTA'][PUZone == PU][DOZone == DO][trip_distance >= 1000 * min_distance][trip_distance <= 1000 * max_distance][,.(velocity,type)]
  print(nrow(z))

  data <- data.frame()

  if (nrow(x) > 1) {
    data <- bind_rows(data,x)
  }

  if (nrow(y) > 1) {
    data <- bind_rows(data,y)
  }

  if (nrow(z) > 1) {
    data <- bind_rows(data,z)
  }

  print(nrow(data))
  print(data)

  print(length(unique(data$type)))

  if (length(unique(data$type)) <= 1) {
    result <- data.frame(
      Taxi = round(mean(x$velocity) * 3.6,3),
      Bike = round(mean(y$velocity) * 3.6,3),
      Subway = round(mean(z$velocity) * 3.6,3),
      P.anova = NA,
      P.Subway_Bike = NA,
      P.Taxi_Bike = NA,
      P.Taxi_Subway = NA
    )
  }
  else if (length(unique(data$type)) == 2) {
    if (nrow(x) <= 1) {
      aov_result <- aov(velocity ~ type,data = data) %>%
        summary() %>%
        unlist()
      result <- data.frame(
        Taxi = round(mean(x$velocity) * 3.6,3),
        Bike = round(mean(y$velocity) * 3.6,3),
        Subway = round(mean(z$velocity) * 3.6,3),
        P.anova = NA,
        P.Subway_Bike = aov_result[["Pr(>F)1"]],
        P.Taxi_Bike = NA,
        P.Taxi_Subway = NA
      )
    }
    else if (nrow(y) <= 1) {
      aov_result <- aov(velocity ~ type,data = data) %>%
        summary() %>%
        unlist()
      result <- data.frame(
        Taxi = round(mean(x$velocity) * 3.6,3),
        Bike = round(mean(y$velocity) * 3.6,3),
        Subway = round(mean(z$velocity) * 3.6,3),
        P.anova = NA,
        P.Subway_Bike = NA,
        P.Taxi_Bike = NA,
        P.Taxi_Subway = aov_result[["Pr(>F)1"]]
      )
    }
    else if (nrow(z) <= 1) {
      aov_result <- aov(velocity ~ type,data = data) %>%
        summary() %>%
        unlist()
      result <- data.frame(
        Taxi = round(mean(x$velocity) * 3.6,3),
        Bike = round(mean(y$velocity) * 3.6,3),
        Subway = round(mean(z$velocity) * 3.6,3),
        P.anova = NA,
        P.Subway_Bike = NA,
        P.Taxi_Bike = aov_result[["Pr(>F)1"]],
        P.Taxi_Subway = NA
      )
    }

  }
  else if (length(unique(data$type)) == 3) {
    aov_result <- aov(velocity ~ type,data = data) %>%
      summary() %>%
      unlist()

    data1 <- bind_rows(x,y)
    print(tibble(data1))

    aov_result1 <- aov(velocity ~ type,data = data1) %>%
      summary() %>%
      unlist()

    print(2)
    data1 <- bind_rows(x,z)

    aov_result2 <- aov(velocity ~ type,data = data1) %>%
      summary() %>%
      unlist()

    data1 <- bind_rows(y,z)

    aov_result3 <- aov(velocity ~ type,data = data1) %>%
      summary() %>%
      unlist()

    result <- data.frame(
      Taxi = round(mean(x$velocity) * 3.6,3),
      Bike = round(mean(y$velocity) * 3.6,3),
      Subway = round(mean(z$velocity) * 3.6,3),
      P.anova = aov_result[["Pr(>F)1"]],
      P.Subway_Bike = aov_result3[["Pr(>F)1"]],
      P.Taxi_Bike = aov_result1[["Pr(>F)1"]],
      P.Taxi_Subway = aov_result2[["Pr(>F)1"]]
    )
  }
  return(result)
}

#design the page
ui <- dashboardPage(
  dashboardHeader(title = "Choose Best Vehicle",
                  tags$li(a(href = 'http://www.google.com',icon("times-circle","fa-3x"),
                            title = "Back to Website"),class = "dropdown")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      useShinyalert(),
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("manhattan_map", height = "70vh")
             ),
             dataTableOutput("time_table")
      ),
      column(width = 3,
             sliderTextInput("month", "Month",choices = month, selected = month[c(1:12)],grid = TRUE),
             sliderTextInput("day","Day",choices = day, selected = day[c(1:7)],grid = TRUE),
             sliderInput("distance","Distance", min = 0, max = 25,value = c(0,25)),
             checkboxGroupInput("time","Time",choices = time, selected = time, inline = TRUE),
             actionButton("button", "Search")
      )
    )
  )
)

#server
server <- function(input, output, session) {

  #map data
  map_data <- reactiveValues()
  map_data$choice <- manhattan_sub

  #record the "from" zone id
  from <- reactiveValues()
  from$id <- NULL

  #record the "to" zone id
  to <- reactiveValues()
  to$id <- NULL

  #record whether "from" or "to" zone is appointed
  point_bool <- reactiveValues()
  point_bool$from <- FALSE
  point_bool$to <- FALSE

  #record click point coordinate
  coordinate_from <- reactiveValues()
  coordinate_from$lng <- NULL
  coordinate_from$lat <- NULL

  coordinate_to <- reactiveValues()
  coordinate_to$lng <- NULL
  coordinate_to$lat <- NULL

  #time table (example of table)
  time_table_dt <- reactiveValues()
  time_table_dt <- NULL

  #get month
  month_choose <- reactiveValues()
  month_choose <- NULL

  #get day
  day_choose <- reactiveValues()
  day_choose <- NULL

  #get time
  time_choose <- reactiveValues()
  time_choose <- NULL

  #time range
  time_range <- reactiveValues()
  time_range <- NULL

  #distance
  distance_choose <- reactiveValues()
  distance_choose <- NULL

  observeEvent(input$button, {
    month_choose <- input$month
    day_choose <- input$day
    time_choose <- input$time
    distance_choose <- input$distance

    print(day_choose)
    print(time_choose)

    month_choose <- c(which(month == month_choose[1]):which(month == month_choose[2]))

    day_choose <- c(which(day == day_choose[1]):which(day == day_choose[2]))
    day_choose <- replace(day_choose,day_choose == 7,0)

    #time period -> time in detail
    time_range <- c()
    for (i in time_choose) {
      time_range <- c(time_start_dictionary[i]:(time_end_dictionary[i] - 1),time_range)
    }


    print(month_choose)
    print(day_choose)
    print(time_range)

    if (is.null(from$id)) {
      shinyalert(title = "Warning", text = "Please choose departure zone on the map!", type = "error", html = TRUE)
      return(0)
    }
    if (is.null(to$id)) {
      shinyalert(title = "Warning", text = "Please choose destination zone on the map!", type = "error", html = TRUE)
      return(0)
    }

    #pick up zone and drop off zone
    PU_zone <- manhattan_map_data[manhattan_map_data$id == from$id,]$zone
    DO_zone <- manhattan_map_data[manhattan_map_data$id == to$id,]$zone

    print(PU_zone)
    print(DO_zone)

    aov_result <- test_function(month_in = c(month_choose), week_in = c(day_choose), PU = PU_zone, DO = DO_zone,
                                min_distance = distance_choose[1], max_distance = distance_choose[2])

    colnames(aov_result) <- c("Taxi (km/h)","Bike (km/h)", "Subway (km/h)", "P.value (anova)",
                              "P.value (Subway-Bike)","P.value (Taxi-Bike)","P.value (Taxi-Subway)")

    print(aov_result)

    #example of table
    output$time_table <- renderDataTable(
      dt <- DT::datatable(aov_result, options = list(dom = 't'))
    )
  })

  #respond to click
  observeEvent(input$manhattan_map_shape_click, {
    click <- input$manhattan_map_shape_click

    #appoint "from"
    if ((!point_bool$from && !point_bool$to) | (point_bool$from && point_bool$to)) {

      coordinate_from$lat <- click$lat
      coordinate_from$lng <- click$lng

      point_bool$from <- TRUE
      point_bool$to <- FALSE

      if (is.numeric(click$id)) {
        map_data$choice <- manhattan_map_data[manhattan_map_data$id == click$id,]
        from$id <- click$id
      }
      else {
        if (click$id == "from") {
          map_data$choice <- manhattan_map_data[manhattan_map_data$id == from$id,]
          from$id <- from$id
        }
        else if (click$id == "to") {
          map_data$choice <- manhattan_map_data[manhattan_map_data$id == to$id,]
          from$id <- to$id
        }
      }

      leafletProxy("manhattan_map") %>%
        removeShape(layerId = "to") %>%
        removeShape(layerId = "line") %>%
        addPolygons(data = map_data$choice,
                    color = "#556B2F",
                    fillColor = "556B2F",
                    fillOpacity = 0.5,
                    weight = 1,
                    layerId = "from")
    }
    #appoint "to"
    else {
      coordinate_to$lat <- click$lat
      coordinate_to$lng <- click$lng

      point_bool$to <- TRUE

      if (is.numeric(click$id)) { #check whether clikc "from" layer
        to$id <- click$id
        map_data$choice <- manhattan_map_data[manhattan_map_data$id == click$id,]
        leafletProxy("manhattan_map") %>%
          addPolygons(data = map_data$choice,
                      color = "#8B0000",
                      fillColor = "#8B0000",
                      opacity = 1,
                      fillOpacity = 0.5,
                      weight = 1,
                      layerId = "to") %>%
          addPolylines(lat = c(coordinate_from$lat,coordinate_to$lat), lng = c(coordinate_from$lng,coordinate_to$lng),
                       layerId = "line", color = "navy", weight = 2, opacity = 0.5)
      }
      else {
        if (click$id == "from") {
          to$id <- from$id
          map_data$choice <- manhattan_map_data[manhattan_map_data$id == from$id,]
        }
        #add layer
        leafletProxy("manhattan_map") %>%
          removeShape(layerId =  'from') %>%
          addPolygons(data = map_data$choice,
                      color = "#8B0000",
                      fillColor = "#8B0000",
                      opacity = 1,
                      fillOpacity = 0.5,
                      weight = 1,
                      layerId = "to") %>%
          addPolylines(lat = c(coordinate_from$lat,coordinate_to$lat), lng = c(coordinate_from$lng,coordinate_to$lng),
                       layerId = "line", color = "navy", weight = 2, opacity = 0.5)
      }
    }
  })

  #draw map
  output$manhattan_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = manhattan_sub,
                  color = "white",
                  smoothFactor = 0.5,
                  weight = 1,
                  fillColor = topo.colors(20),
                  highlightOptions = highlightOptions(color = "#444444", weight = 2, bringToFront = TRUE),
                  dashArray = 3,
                  group = "manhattan",
                  label = manhattan_sub$zone,
                  layerId = manhattan_sub$id)
  })
}

shinyApp(ui, server)
