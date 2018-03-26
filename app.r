library(shiny)
library(tidyverse)
library(stringr)
library(plotly)
load("data_for_app.RData")
test <- FALSE
min_year <- min(data_filtered_indicators_country_partitions$Year)
max_year <- max(data_filtered_indicators_country_partitions$Year)
indexes <- unique(data_filtered_indicators_country_partitions$IndicatorName)
countries <- unique(data_filtered_indicators_country_partitions$Country)
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
                           max = max_year, value = c(min_year, max_year), step = 1, sep=""),
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
                            choices = c("Continent" = "Continent", "Macro Region" = "Region")),
               checkboxInput("size_pop", "scale points \n for population size ?", value = FALSE),
               actionButton("update_relation","Update plot"),
               checkboxGroupInput("customize_annotation", 
                                  label = h4("Customize Annotation"), choices = list("Customize" = "cust"),selected = NULL),
               conditionalPanel(
                 condition = "input.customize_annotation == 'cust'", 
                 numericInput("font_size_ax", "Select font size for axes labels", value = 15),
                 numericInput("font_size_tick", "Select font size for axes values", value = 15))),
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
           Country %in% input$country)
  validate(
    need(length(df$Value) != 0, "There are no data in the selected years for the index chosen: please change index or
         expand the time range"))
  ggplot(data = df, aes(x = Year, y = Value, group = Country, color = Country)) + 
    geom_line(size = 1.5) + 
    ylab(input$index) + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())})
  
  df_relation <- eventReactive(input$update_relation, {validate(need(data_filtered_indicators_country_partitions %>%
                                                                  filter(Year == input$year_relation,
                                                                         IndicatorName %in% c(input$x_axis, input$y_axis, "Population, total")) %>%
                                                                  nrow(.) %>% `>`(.,0), "Neither of the selected indexes have been measured for the current year,
                                                                  please select a different year or different indexes"))
  df <- data_filtered_indicators_country_partitions %>%
    filter(Year == input$year_relation,
           IndicatorName %in% c(input$x_axis, input$y_axis, "Population, total")) %>%
    split(., .$IndicatorName) %>%
    map(~spread(., IndicatorName, Value)) %>%
    reduce(left_join, by = "CountryCode") %>%
    select(-ends_with("y"))
  colnames(df)[1:6] <- colnames(df)[1:6] %>% 
    str_split(., pattern = ".x") %>%
    map_chr(~`[`(.,1))
  df})
  
  by_size = eventReactive(input$size_pop, {if (input$size_pop == TRUE){TRUE} else {FALSE}})
  
  output$relation <- renderPlot({ validate(need(isolate(input$x_axis) %in% colnames(df_relation()) & isolate(input$y_axis) %in% colnames(df_relation()), 
                                                "The combinaton of indexes chosen is not present in the dataset for the selected year"))
    if (by_size()) {
    gplot <- ggplot(data = df_relation(), aes_string(x = isolate(as.name(input$x_axis)), 
                                            y = isolate(as.name(input$y_axis)), 
                                            color = isolate(input$color_by))) + 
      geom_point(aes(size = `Population, total`)) + 
      theme_bw() + 
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    if(length(input$customize_annotation) != 0) {print(gplot + 
                                                     theme(axis.title = element_text(size = input$font_size_ax)))}
    else {(print(gplot))}
    } else {
              gplot <- ggplot(data = df_relation(), aes_string(x = isolate(as.name(input$x_axis)),
                                                      y = isolate(as.name(input$y_axis)), 
                                                      color = isolate(input$color_by))) + 
                geom_point(size = 2) + 
                theme_bw() + 
                theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
              if(length(input$customize_annotation) != 0) {print(gplot + 
                                                               theme(axis.title = element_text(size = input$font_size_ax)))}
              else {(print(gplot))}}
  }
  
  )
  
}


shinyApp(ui = ui, server = server)
