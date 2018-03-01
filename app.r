library(shiny)
library(tidyverse)

load("data_for_app.RData")

min_year <- min(data_filtered_indicators_country_partitions$Year)
max_year <- max(data_filtered_indicators_country_partitions$Year)
indexes <- unique(data_filtered_indicators_country_partitions$IndicatorName)
countries <- unique(data_filtered_indicators_country_partitions$CountryName)
continents <- unique(data_filtered_indicators_country_partitions$Continent)
regions <- unique(data_filtered_indicators_country_partitions$Region)

ui <- fluidPage(mainPanel(
                  tabsetPanel(
                    tabPanel("TimeSeries", 
                      inputPanel(
                        sliderInput("year_range", "Select the time range to plot",
                                    min = min_year,
                                    max = max_year, , value = c(min_year, max_year), step = 1, sep=""),
                        selectInput("index", "Index to plot", choices = indexes),
                        selectInput("country", "Country to plot", 
                                    choices = countries, selectize = TRUE, multiple = TRUE, selected = "Italy")),
                      plotOutput("timeseries")
                    )
                  )
  )
)

                      
  
server <- function(input,output){
  
  output$timeseries <- renderPlot({df <- data_filtered_indicators_country_partitions %>%
    filter(Year > input$year_range[1],  
           Year < input$year_range[2],
           IndicatorName == input$index,
           CountryName %in% input$country)
        ggplot(data = df, aes(x = Year, y = Value, group = CountryName, color = CountryName)) + 
                                                                            geom_line()})}
  


shinyApp(ui = ui, server = server)


