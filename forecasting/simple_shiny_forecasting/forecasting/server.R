library(shiny)
library(datasets)
library(forecast)
library(zoo)

shinyServer(function(input, output) {
    
    getDataset <- reactive({
        if (input$variable=="AirPassengers")
        {
            return(AirPassengers)
        }
        else if (input$variable=="gas")
        {
            return(gas)
        }
        else
        {
            return(wineind)
        }
    })
    
    ####### READ IN CSV FILE BASED ON SELECTION ####### 
    mySeries_raw <- reactive({
        
        inFile <- input$i_file
        
        if (is.null(inFile))
            return(NULL)
        
        mySeries <- read.csv(inFile$datapath, 
                             header = T,
                             strip.white=T,
                             stringsAsFactors=F,
                             fill=T) 
        
        #myts %>% auto.arima() %>% forecast() %>% autoplot()
    })
    

    
    #   mySeries$Date <- zoo::as.Date(mySeries$Date, format = "%d/%m/%Y")
    
    #   mySeries <- zoo(df$Close, order.by=as.Date(as.character(df$Date), format='%m/%d/%Y'))
    
    year_tbl <- mySeries_raw() %>% 
        filter(ds == min(Date)) %>% 
        mutate(year = lubridate::year(ds)) %>%
        mutate(month = lubridate::month(ds))
    
    temp <- mySeries_raw()
    
  #  myts <- ts(temp[,2:2], start = c(year_tbl$year, year_tbl$month), frequency = 12)
    
    
    
    output$caption <- renderText({
        paste("Dataset: ", input$variable)
    })
    
#    output$dcompPlot <- renderPlot({
#        ds_ts <- ts(getDataset(), frequency=12)
#        f <- decompose(ds_ts)
#        plot(f)
#    })
    
    output$arimaForecastPlot <- renderPlot({
        fit <- auto.arima(myts)
        plot(forecast(fit, h=input$ahead))
    })
    
    output$etsForecastPlot <- renderPlot({
        fit <- ets(myts)
        plot(forecast(fit, h=input$ahead))
    })
    
})