#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(forecast)
library(dplyr)
library(ggthemes)

file <- read.csv("Rwanda_hdi_gdp.csv")
file$X <- NULL
colnames(file) <- c("gdp", "hdi")
myts <- ts(file, start = 2003, frequency = 1)

#hdi_ts$gdp <- NULL
hdi_ts <- ts(file$hdi, start = 2003, frequency = 1)

# fit a dynamic regression model 
#fit <- auto.arima(rwanda_join[, "myts_sub"], xreg = rwanda_join[, "gdp_sub"])
# Forecast fit as fc
#fc <- forecast(fit, xreg = cbind(1500), h = 10)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    headerPanel("Rwanda Human Development Forecasting"),
    
    # Sidebar with controls to select the dataset and forecast ahead duration
    sidebarPanel(
        #selectInput("variable", "Variable:",
        #            list("Air Passengers" = "AirPassengers", 
        #                 "Australian total wine sales" = "wineind",
        #                 "Australian monthly gas production" = "gas")),
        numericInput("firstyear", "GDP Per Capita First Year Ahead:", 700),
        numericInput("secondyear", "GDP Per Capita Second Year Ahead:", 750),
        sliderInput("slider", "Forecast Years:", min = 1, max = 10, value = 3),
        submitButton("Update Forecasts"), 
        textOutput("selected_var")
    ),
    
    
    
    # Show the caption and forecast plots
    mainPanel(
        #h3(textOutput("caption")),
        
        tabsetPanel(
            tabPanel("Dynamic Regression", plotOutput("arimaForecastPlot"), plotOutput("arimaerrors")),
            tabPanel("Evaluating Multiple Forecast Methods", plotOutput("EvaluationForecastPlot")),
            tabPanel("Best Forecast", plotOutput("tbats")
        )
    )
))


# Define server logic 
server <- function(input, output) {
    
    train <- window(hdi_ts, end=c(2016,140))
    h <- length(myts) - length(train)
    ETS <- forecast(ets(train), h=h)
    ARIMA <- forecast(auto.arima(train, d=1, stepwise = FALSE), h = h)
    
   # getDataset <- reactive({
    #    if (input$variable=="AirPassengers")
    #    {
    #        return(AirPassengers)
    #    }
    #    else if (input$variable=="gas")
    #    {
    #        return(gas)
    #    }
    #    else
    #    {
    #        return(wineind)
    #    }
    #})
    
   # output$caption <- renderText({
    #    paste("Dataset: ", input$variable)
    #})
    
   # output$dcompPlot <- renderPlot({
    #    #ds_ts <- ts(getDataset(), frequency=12)
    #    f <- decompose(myts)
    #    plot(f)
    #})
    
    output$arimaForecastPlot <- renderPlot({
        #fit <- auto.arima(getDataset())
        r <- c(input$firstyear, input$secondyear)
        #r <- r * input$GDP
        fit <- auto.arima(myts[, "hdi"], xreg = myts[, "gdp"])
        #plot(forecast(fit, xreg = cbind(input$GDP), h=input$ahead))
        autoplot(forecast(fit, xreg = r)) + xlab("Years") + ylab("Human Development Index") + 
            theme_fivethirtyeight() +
            theme(axis.title = element_text()) + ylab('Human Development Index') + xlab("Time")
    })
    
    output$arimaerrors <- renderPlot({
        r <- c(input$firstyear, input$secondyear)
        fit <- auto.arima(myts[, "hdi"], xreg = myts[, "gdp"])
        checkresiduals(fit) + theme_fivethirtyeight()
    })
    
   # output$etsForecastPlot <- renderPlot({
    #    fit <- ets(hdi_ts)
    #    autoplot(forecast(fit, h=2)) + xlab("Years") + ylab("Human Development Index") + theme_fivethirtyeight()
    #})
    
    
    output$EvaluationForecastPlot <- renderPlot({
        
    train <- window(hdi_ts, end=c(1999,12))
    h <- length(hdi_ts) - length(train)
    ETS <- forecast(ets(train), h=h)
    ARIMA <- forecast(auto.arima(train, d=1, stepwise = FALSE),
                      h=h)
    NNAR <- forecast(nnetar(train), h=h)
    TBATS <- forecast(tbats(train, use.trend = TRUE), h=h)
    Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                        #STL[["mean"]] 
                        + NNAR[["mean"]] + 
                        TBATS[["mean"]])/4
    
    
    autoplot(hdi_ts) +
        autolayer(ETS, series="ETS", PI=FALSE) +
        autolayer(ARIMA, series="ARIMA", PI=FALSE) +
        autolayer(NNAR, series="NNAR", PI=FALSE) +
        autolayer(TBATS, series="TBATS", PI=FALSE) +
        autolayer(Combination, series="Combination") +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + ylab('Human Development Index') + xlab("Time") +
        ggtitle("Rwanda Human Development Forecasts")
    
    })
    
    output$tbats <- renderPlot({
        
        x <- input$slider
        TBATS <- forecast(tbats(hdi_ts, use.trend = TRUE), h=x)
        autoplot(TBATS) +         
            theme_fivethirtyeight() +
            theme(axis.title = element_text()) + ylab('Human Development Index') + xlab("Time") +
            ggtitle("TBATS: Rwanda Human Development Forecast")
    })    
        
    output$selected_var <- renderText({ 
        "Here is some text"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
