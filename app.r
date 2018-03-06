library(shiny)
library(tidyverse)
library(stringr)

load("data_for_app.RData")

min_year <- min(data_filtered_indicators_country_partitions$Year)
max_year <- max(data_filtered_indicators_country_partitions$Year)
indexes <- unique(data_filtered_indicators_country_partitions$IndicatorName)
countries <- unique(data_filtered_indicators_country_partitions$CountryName)
continents <- unique(data_filtered_indicators_country_partitions$Continent)
regions <- unique(data_filtered_indicators_country_partitions$Region)

data_by_continent <- data_filtered_indicators_country_partitions %>%
  group_by(Continent, Year, IndicatorName) %>%
  summarise(Value = mean(Value)) %>%
  ungroup(.)

data_by_region <- data_filtered_indicators_country_partitions %>%
  group_by(Region, Year, IndicatorName) %>%
  summarise(Value = mean(Value)) %>%
  ungroup(.)


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
                    ),
                    tabPanel("Relationship between indexes",
                      inputPanel(
                        selectInput("x_axis", "Select the variable on the x axis",
                                    choices = indexes),
                        selectInput("y_axis", "Select the variable on the y axis",
                                    choices = indexes), 
                        selectInput("year_relation", "Select the year you want the data for",
                                    choices = c(min_year:max_year),
                                    selected = 1984),
                        radioButtons("color_by", "Select colour code",
                                     choices = c("Continent" = "Continent", "Macro Region" = "Region"))),
                        plotOutput("relation")
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
            geom_line()})
  output$relation <- renderPlot({df <- data_filtered_indicators_country_partitions %>%
    filter(Year == input$year_relation,
           IndicatorName %in% c(input$x_axis, input$y_axis)) %>%
    split(., .$IndicatorName) %>%
    map(~spread(., IndicatorName, Value)) %>%
    reduce(left_join, by = "CountryCode") %>%
    select(-ends_with("y"))
    colnames(df)[1:6] <- colnames(df)[1:6] %>% 
      str_split(., pattern = ".x") %>%
      map_chr(~`[`(.,1))
  
  ggplot(data = df, aes_string(x = as.name(input$x_axis), y = as.name(input$y_axis), color = input$color_by)) + 
          geom_point()})
}
  


shinyApp(ui = ui, server = server)


