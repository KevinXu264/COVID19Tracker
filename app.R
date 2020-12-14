library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
source("helpers.R")
library(leaflet)
library(DT)
library(stringr)

ui <- fluidPage(
  titlePanel(title = "COVID-19 Tracker"),
  
  
  sidebarLayout(
    sidebarPanel(
      h5(paste(sep = " ", "Welcome to Kevin's COVID-19 R Shiny Project! This is a simple app that allows you to see the COVID-19 cases
    by each state, and look at the statistics from each state. This data is from Johns Hopkins University, and 
    the data is COVID cases in the US by today's date:", format(Sys.Date(), "%m-%d-%Y"))),

      selectInput(inputId = "state",
                  label = "What state(s) would you like to see the statistics for?",
                  choices = covid_raw[,1],
                  selected = "California",
                  multiple = FALSE),
      textInput("linkInput", 
                label = "Want to check the COVID-19 data for another day? Insert link to github raw data below!
                UPDATE: This feature has been replaced by the pick date option below, which will automatically draw the git
                hub data for you!",
                value = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/12-05-2020.csv"),
      actionButton(inputId = "linkClicked",label = "Load Link"),
      br(),
      br(),
      dateInput(inputId = "date",label = "Pick a date below",
                format = "mm-dd-yyyy", 
                min = "2020-11-16", max = Sys.Date() - 1,
                value = Sys.Date() - 2),
      h5("If the current load date option does not work, please choose an earlier date. JHU might have changed/
         modified their data to be formatted differently for it to not load correctly, or there could not
         be sufficient data for the date requested in mind. Thank you!"),
      actionButton(inputId = "dateClicked",label = "Load Date"),

    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Cases", 
                h3("Cumulative Cases"),
                h6("Cumulative Cases is calculated by taking the confirmed number of cases and 
                   dividing it by the total number of test results."),
                leafletOutput(outputId = "cumulativeCases"),
                fluidRow(
                    column(3)
                  )
                ),
        tabPanel("Incidence Rate",
                 h3("Incidence Rate"),
                 h6("Incidence rate is the number of people that are suspected to incur COVID, calculated
                    in per 100,000 people."),
                 leafletOutput(outputId = "incidenceRate")),
        tabPanel("Case-Fatality Ratio",
                 h3("Case-Fatality Ratio"),
                 h6("Case Fatality ratio is the ratio of the number of deaths divided by the number of confirmed cases."),
                 leafletOutput(outputId = "caseFatality")),
        tabPanel("Overlay",
                 h3("Overlaid Graphs"),
                 leafletOutput(outputId = "overlay"))
      ),
      h2("The Data Table"),
      textOutput(outputId = "text"),
      DT::dataTableOutput(outputId = "dataStates", width = "100%"),
      tabsetPanel(
        tabPanel("US Cases",
          DT::dataTableOutput(outputId = "dataTable", width = "100%")
        ),
        tabPanel("World Cases",
                 p("Here is an option for you to find more data about a specific city, state, or country
                    if it so exists in the John Hopkins data set. Maybe world data will be a feature looked
                    at in the future, but this is what you have for the time being."),
          DT::dataTableOutput(outputId = "dataTableWorld", width = "100%")
        )
        
      )
    )
  )
)

server <- function(input,output) ({

  output$cumulativeCases <- renderLeaflet({
    mapStates <- map("state",fill=TRUE,plot=FALSE)
    leaflet(data = mapStates, options=leafletOptions(minZoom = 1, maxZoom = 6)) %>% 
      addTiles() %>% 
      addCircleMarkers(data = readCSVData(), 
                   lat = readCSVData()$Lat, 
                   lng = readCSVData()$Long_, 
                   group = "Cumulative Cases",
                   radius = readCSVData()$Confirmed / 20000,
                  fillOpacity = 0.5, popup = paste(sep = "<br/>",
                  (covid_raw$Province_State),
                  paste(sep = " ", "Confirmed:",readCSVData()$Confirmed),
                  paste(sep = " ", "Deaths:",readCSVData()$Deaths),
                  paste(sep = " ", "Recovered:",readCSVData()$Recovered))
                 )%>%
      setView(lat = 37.0902, lng = -95.7129, zoom= 3.5)
  })
  
  output$incidenceRate <- renderLeaflet({
    mapStates <- map("state",fill=TRUE,plot=FALSE)
    leaflet(data = mapStates, options=leafletOptions(minZoom = 1, maxZoom = 6)) %>% 
      addTiles() %>% 
      addCircleMarkers(data = readCSVData(), lat = readCSVData()$Lat, 
                 lng = readCSVData()$Long_, 
                 group = "Incidence Rate",
                 radius = readCSVData()$Incident_Rate / 300,
                 fillOpacity = 0.5,
                 color = "red",
                 popup = paste(sep = "<br/>",
                 (readCSVData()$Province_State),
                 paste(sep = "", "Incidence Rate: ", 
                readCSVData()$Incident_Rate,
                 " per 100,000 people"))
      )%>%
      setView(lat = 37.0902, lng = -95.7129, zoom= 3.5)
  })
  
  output$caseFatality <- renderLeaflet({
    mapStates <- map("state",fill=TRUE,plot=FALSE)
    leaflet(data = mapStates, options=leafletOptions(minZoom = 1, maxZoom = 6)) %>% 
      addTiles() %>% 
      addCircleMarkers(data = readCSVData(), lat = readCSVData()$Lat, 
                 lng = readCSVData()$Long_, 
                 group = "Case-Fatality Ratio",
                 radius = readCSVData()$Case_Fatality_Ratio * 5,
                 fillOpacity = 0.5,
                 color = "purple",
                 popup = paste(sep = "<br/>",
                 (readCSVData()$Province_State),
                 paste(sep = "", "Case Fatality Ratio: ", 
                 readCSVData()$Case_Fatality_Ratio,
                 " %"))
      )%>%
      setView(lat = 37.0902, lng = -95.7129, zoom= 3.5)
  })
  
  output$overlay <- renderLeaflet({
    mapStates <- map("state",fill=TRUE,plot=FALSE)
    leaflet(data = mapStates, options=leafletOptions(minZoom = 1, maxZoom = 8)) %>%
      addTiles() %>%
      addCircleMarkers(data = readCSVData(), lat = readCSVData()$Lat,
                       lng = readCSVData()$Long_,
                       group = "Case-Fatality Ratio",
                       radius = readCSVData()$Case_Fatality_Ratio * 5,
                       opacity = 0.5,
                       color = "purple",
                       popup = paste(sep = "<br/>",
                                     readCSVData()$Province_State,
                                     paste(sep = "", "Case Fatality Ratio: ",
                                           readCSVData()$Case_Fatality_Ratio,
                                           " %"))
      )%>%
    
      addCircleMarkers(data = readCSVData(), lat = readCSVData()$Lat, 
                       lng = readCSVData()$Long_, 
                       group = "Incidence Rate",
                       radius = readCSVData()$Incident_Rate / 300,
                       opacity = 0.5,
                       color = "red",
                       popup = paste(sep = "<br/>",
                       (readCSVData()$Province_State),
                       paste(sep = "", "Incidence Rate: ", 
                       readCSVData()$Incident_Rate,
                       " per 100,000 people"))
      )%>%
      addCircleMarkers(data = readCSVData(), 
                       lat = readCSVData()$Lat, 
                       lng = readCSVData()$Long_, 
                       group = "Cumulative Cases",
                       radius = readCSVData()$Confirmed / 20000,
                       opacity = 0.5, popup = paste(sep = "<br/>",
                       (readCSVData()$Province_State),
                       paste(sep = " ", "Confirmed:",readCSVData()$Confirmed),
                       paste(sep = " ", "Deaths:",readCSVData()$Deaths),
                       paste(sep = " ", "Recovered:",readCSVData()$Recovered))
      )%>%
      setView(lat = 37.0902, lng = -95.7129, zoom= 3.5) %>%
      addLayersControl(
        overlayGroups = c("Cumulative Cases","Incidence Rate","Case-Fatality Ratio"),
        options = layersControlOptions(collapsed=FALSE)
      )

  })
  
  output$text <- renderText(
    paste(sep = " ", "Date:",HTML(str_sub(newLink(), start = -14, end = -5)))
  )
  
  output$dataTable <- DT::renderDataTable({readData()
    })
  
  output$dataTableWorld <- DT::renderDataTable({updateDataWorld()})
  
  output$dataStates <- DT::renderDataTable({
    select(readDataStates(), City, Confirmed, Deaths, Recovered)
    })
  
  newLink2 <- eventReactive(input$linkClicked,{
    input$linkInput
  })
  
  newLink <- eventReactive(input$dateClicked,{
    paste(sep = "",
          "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",
          format(input$date, "%m-%d-%Y"),
          ".csv")
  })
  
  newLinkWorld <- eventReactive(input$dateClicked,{
    paste(sep = "",
          "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
          format(input$date, "%m-%d-%Y"),
          ".csv")
  })
  
  
  readDataStates <- reactive({
    readDataWorldParsed() %>%
    filter(Province_State %in% input$state) %>%
    arrange(desc(Confirmed))
  })

  readCSVData <- reactive(read.csv(newLink()))
  readCSVDataWorld <- reactive(read.csv(newLinkWorld(), na.strings = c("","NA")))
  
  readData <- reactive({
    covid_raw <- readCSVData()
    covid_raw <- filter(covid_raw, !is.na(Lat) & !is.na(Long_))
    covid_raw$Case_Fatality_Ratio <- trunc(covid_raw$Case_Fatality_Ratio * 100)/100
    covid_raw$Incident_Rate <- trunc(covid_raw$Incident_Rate * 10) / 10

    data.frame(
      Province_State = covid_raw$Province_State,
      Confirmed = covid_raw$Confirmed,
      Deaths = covid_raw$Deaths,
      Recovered = covid_raw$Recovered,
      Active = covid_raw$Active,
      Incident_Rate = covid_raw$Incident_Rate,
      Total_Test_Results = covid_raw$Total_Test_Results,
      Testing_Rate = covid_raw$Testing_Rate) %>%
      arrange(desc(Confirmed))
  })
  
  updateDataWorld <- reactive({
    covid_world <- readCSVDataWorld() 
    covid_world <- filter(covid_world, !is.na(Lat) & !is.na(Long_))
    countries <- covid_world %>%
      group_by(Country_Region) %>%
      summarise(sum(Confirmed), sum(Deaths), sum(Recovered), mean(Case_Fatality_Ratio), .groups= "keep")
      colnames(countries) <- c("Country","Confirmed","Deaths","Recovered","Case_Fatality_Ratio")
      arrange(countries, desc(Confirmed))

    
    # numOfCountries <- as.numeric(tally(countryNames)[1,1])
    # countries <- c(1:numOfCountries

# 
#     lapply(countries)
#     {
#       function(i)
#       {
#           country <- countryNames[i, 1]
#       }
#       sum_Confirmed <- as.numeric(
#         covid_world$Confirmed[
#           (covid_world$Country_Region %in% country)])
# 
#       dataWorldFrame() %>%
#         add_row(
#           Country_Region = country,
#           Confirmed = sum_Confirmed,
#           Deaths = sum_Confirmed,
#           Recovered = sum_Confirmed,
#           Active = sum_Confirmed)
#     }
#       
    
  })
  
  dataWorldFrame <- reactive({
   data.frame(
     Country_Region = character(),
     Confirmed = integer(),
     Deaths = integer(),
     Recovered = integer(),
     Active = integer()
   ) 

  })
  
  readDataWorldParsed<- reactive({
    covid_world <- readCSVDataWorld()
    data.frame(
      Country_Region = covid_world$Country_Region,
      Province_State = covid_world$Province_State,
      City = covid_world$Admin2,
      Confirmed = covid_world$Confirmed,
      Deaths = covid_world$Deaths,
      Recovered = covid_world$Recovered,
      Active = covid_world$Active)
  })
  
})

shinyApp(ui = ui, server = server)
