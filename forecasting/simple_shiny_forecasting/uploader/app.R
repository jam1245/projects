#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(vroom)
library(shiny)
library(timetk)
library(modeltime)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ui <- fluidPage(
    fileInput("file", NULL, accept = c(".csv", ".tsv")),
    numericInput("n", "Rows", value = 5, min = 1, step = 1),
    actionButton("submit_button", "Submit", class = "btn-success"),
    tableOutput("head"),
    
    # Show the caption and forecast plots
    mainPanel(
        h3(textOutput("caption")),
        
        tabsetPanel(
          #  tabPanel("Exponetial Smoothing (ETS) Forecast", plotOutput("etsForecastPlot")), 
            tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"))
      #      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
        )
    )
)

server <- function(input, output, session) {
    
    #rv <- reactiveValues()
    
    data <- reactive({
        req(input$file)
        
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               csv = vroom::vroom(input$file$datapath, delim = ","),
               tsv = vroom::vroom(input$file$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    

    
    output$head <- renderTable({
       # glimpse(data())
        head(data(), input$n)
    })
    
    observeEvent(input$submit_button, {
    
        df <- data() %>% as_tibble()
    })
    
    output$arimaForecastPlot <- renderPlotly({
     
     
     df %>%
     plot_time_series(Date, y)
        })
    
    
    
    output$plot_page_views <- renderPlotly({
        
        req(rv$forecast_list)
        
        g <- rv$forecast_list$pageViews %>%
            ggplot(aes(date, .mean, color = .model)) +
            geom_ribbon(aes(ymin = lo_95, ymax = hi_95), alpha = 0.5, color = "white", fill = "dodgerblue") +
            geom_line() +
            # facet_wrap(~ pagePath + search_type, ncol = 1, scales = "free_y") +
            theme_tq() +
            scale_color_tq() +
            labs(x = "", y = "")
        
        ggplotly(g) %>%
            layout(
                # xaxis = list(rangeslider = list(visible = T)),
                legend = list(orientation = 'h')
            )
    })
    
#    output$arimaForecastPlot <- renderPlot({
#        fit <- auto.arima(myts)
#        plot(forecast(fit, h=input$ahead))
#    })
    
#    output$etsForecastPlot <- renderPlot({
#        fit <- ets(myts)
#        plot(forecast(fit, h=input$ahead))
#    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
