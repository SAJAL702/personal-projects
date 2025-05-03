library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(forecast)

# Load Data
crime_data <- read_csv("D:/HACKATHON/cleaned_statewise_crime_data.csv")

# Reshape Data (Wide to Long Format)
crime_data_long <- crime_data %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Crime_Count") %>%
  mutate(Year = as.numeric(Year))

# Define UI
ui <- fluidPage(
  titlePanel("Time Series Analysis of Statewise Crime Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(crime_data_long$State)),
      numericInput("forecast_years", "Years to Forecast:", 5, min = 1, max = 10)
    ),
    
    mainPanel(
      plotOutput("timeSeriesPlot"),
      plotOutput("forecastPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    crime_data_long %>%
      filter(State == input$state) %>%
      arrange(Year)
  })
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Crime_Count)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(title = paste("Crime Trends in", input$state),
           x = "Year",
           y = "Crime Count") +
      theme_minimal()
  })
  
  # Forecasting
  output$forecastPlot <- renderPlot({
    ts_data <- ts(filtered_data()$Crime_Count, start = min(filtered_data()$Year), frequency = 1)
    fit <- auto.arima(ts_data)
    forecasted_values <- forecast(fit, h = input$forecast_years)
    
    autoplot(forecasted_values) +
      labs(title = paste("Crime Forecast for", input$state),
           x = "Year",
           y = "Crime Count") +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)
