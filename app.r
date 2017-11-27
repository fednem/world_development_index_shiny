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
                        selectInput("grouping", "Aggregation level", choices = c("Country",
                                                                                 "Continent",
                                                                                 "Region"))),
                        
                      plotOutput("timeseries")
                    )
                  )
  )
)

                      
  
server <- function(input,output){
  output$timeseries <- renderPlot({ if(input$grouping == "Country") {
                                                                    df <- data_filtered_indicators_country_partitions %>%
                                                                          filter(Year > input$year_range[1],  
                                                                                 Year < input$year_range[2],
                                                                                 IndicatorName == input$index)
                                                                    ggplot(data = df, aes(x = Year, y = Value, group = CountryCode,
                                                                    color = CountryCode)) + 
                                                                    geom_line()} else if (input$grouping == "Continent") {
                                                                      df <- data_filtered_indicators_country_partitions %>%
                                                                        filter(Year > input$year_range[1],  
                                                                               Year < input$year_range[2],
                                                                               IndicatorName == input$index) %>%
                                                                        group_by(Continent, Year) %>%
                                                                        summarise(average = mean(Value))
                                                                      ggplot(data = df, aes(x = Year, y = average, 
                                                                                            group = Continent, color = Continent)) + 
                                                                        geom_line()} else if (input$grouping == "Region") {
                                                                          df <- data_filtered_indicators_country_partitions %>%
                                                                            filter(Year > input$year_range[1],  
                                                                                   Year < input$year_range[2],
                                                                                   IndicatorName == input$index) %>%
                                                                            group_by(Region, Year) %>%
                                                                            summarise(average = mean(Value))
                                                                          ggplot(data = df, aes(x = Year, y = average, 
                                                                                                group = Region, color = Region)) + 
                                                                            geom_line()}
                                        
                                      })}
  


shinyApp(ui = ui, server = server)


