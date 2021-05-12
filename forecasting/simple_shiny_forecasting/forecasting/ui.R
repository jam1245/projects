library(shiny)

# Define UI 
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Timeseries Forecasting"),
    
    # Sidebar with controls to select the dataset and forecast ahead duration
    sidebarPanel(
        selectInput("variable", "Variable:",
                    list("Air Passengers" = "AirPassengers", 
                         "Australian total wine sales" = "wineind",
                         "Australian monthly gas production" = "gas")),
        
        fileInput("i_file", "Upload your CSV file",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
        ),
        numericInput("ahead", "Months to Forecast Ahead:", 12),
        
        submitButton("Update View")
    ),
    
    
    
    # Show the caption and forecast plots
    mainPanel(
        h3(textOutput("caption")),
        
        tabsetPanel(
            tabPanel("Exponetial Smoothing (ETS) Forecast", plotOutput("etsForecastPlot")), 
            tabPanel("Arima Forecast", plotOutput("arimaForecastPlot")),
            tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
        )
    )
))