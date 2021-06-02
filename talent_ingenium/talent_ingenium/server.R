# 1. PACKAGES -------------------------------------------------------------
source("library/library.R")

# 2. Data -------------------------------------------------------------
source("data/tidy_data.R")

# 3. Load Model -------------------------------------------------------------
my_model <- readRDS("model/my_rf_model.RDS")




rf_model_v2 <- ranger(cycletime ~ 
                          city_simple + 
                          JOB_CATEGORY + 
                          JOB_CLASS +
                          sourcing +
                          hiring_mngr_review+
                          interview+
                          offer_pending+
                          offer_to_accept+ 
                          Clearance +
                          LEVEL, # formula 
                      sd_df, # data
                      num.trees =500, 
                      respect.unordered.factors = "order", 
                      mtry = 2,
                      min.node.size = 3,
                      sample.fraction = .8,
                      importance = "impurity",
                      seed = 2135)







# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    ## 
    
    
    
    ### Cycle Time application Code 
    
    # 1. Data & Manipulation
    #source(file.path("Server_Code", "tidy_data.R"),  local = TRUE, encoding = "UTF-8")$value
    
    cycletimedata = reactive(data.frame(city_simple = input$city_simple, 
                                        Clearance = input$Clearance_ct,
                                        JOB_CATEGORY = input$JOB_CATEGORY,
                                        LEVEL = input$LEVEL,
                                        JOB_CLASS = input$JOB_CLASS,
                                        open_to_apply_time=input$slider, 
                                        screen_to_rfi_time = input$slider2, 
                                        apply_to_review_time=input$slider3, 
                                        extend_to_accept_time=input$slider4, 
                                        review_to_screen_time=input$slider5))
    
    
    output$vbox <- renderValueBox({
        
        ct_cycle_time_pred <- predict(my_model, cycletimedata())$predictions
        
        valueBox(paste0(round(ct_cycle_time_pred, digits = 1), ""), "Cycle Time Prediction", icon = icon("stopwatch"),  color = "orange")
        # "Cycle Time Prediction", value = data, icon="icon", color = "blue")
        
    })
    
    output$meanBox <- renderValueBox({
        
        cyclemean = df_cat %>% filter(city_simple == input$city_simple |
                                          Clearance == input$Clearance_ct |
                                          JOB_CATEGORY == input$JOB_CATEGORY |
                                          JOB_CLASS == input$JOB_CLASS | 
                                          LEVEL == input$LEVEL) %>% 
            summarize(mean = mean(cycletime, na.rm = TRUE))
        
        #cyclemean = mean(df_cat$cycletime)
        
        valueBox(paste0(round(cyclemean, digits = 1), ""), "Cycle Time Avg.     ", icon = icon("chart-bar"),  color = "blue")
        # "Cycle Time Prediction", value = data, icon="icon", color = "blue")
        
    })
    
    output$medianBox <- renderValueBox({
        
        cyclemedian = df_cat %>% filter(city_simple == input$city_simple |
                                            Clearance == input$Clearance_ct |
                                            JOB_CATEGORY == input$JOB_CATEGORY |
                                            JOB_CLASS == input$JOB_CLASS |
                                            LEVEL == input$LEVEL) %>% 
            summarize(median = median(cycletime, na.rm = TRUE))
        
        #cyclemean = mean(df_cat$cycletime)
        
        valueBox(paste0(round(cyclemedian, digits = 1), ""), "Cycle Time Median     ", icon = icon("chart-line"),  color = "blue")
        # "Cycle Time Prediction", value = data, icon="icon", color = "blue")
        
    })
    
    
    ### plot 
    
    
    output$freq_plot <- renderPlot({ 
        
        p <- df_cat %>% 
            group_by(JOB_CLASS, JOB_CATEGORY) %>%
            summarize(count=n()) %>%
            #count(JOB_CLASS, sort = TRUE) %>%  
            mutate(all_job_cat = sum(count)) %>% 
            filter(count >=150) %>%
            ggplot(aes(x=reorder(JOB_CLASS, all_job_cat),y= count)) +
            geom_bar(aes(fill=JOB_CATEGORY),stat='identity',color='white',size=.1) + 
            coord_flip() + 
            theme_minimal() +
            scale_fill_manual(values=c('#8c96c6',"#969696", "#2171b5", "#6baed6")) +
            # scale_fill_manual(name="",values = viridis::viridis(4)) +
            theme(legend.position='top',plot.title = element_text(size =10), 
                  axis.text = element_text(size =8), axis.title = element_text(size =8), 
                  axis.title.x = element_text(size =8), axis.title.y = element_text(size =8)) + 
            #scale_fill_manual(values=c("#6baed6","#9ecae1", "#2171b5", "#4292c6")) +
            labs(title = "",
                 subtitle = "",
                 caption = " ")  + 
            xlab('') + ylab('Jobs Filled') +
            guides(fill=guide_legend(title=""))
        p
    })
    
    output$plot1 <- renderPlotly({
        
        
        f1 <- list(
            family = "Arial, sans-serif",
            size = 10,
            color = "lightgrey"
        )
        f2 <- list(
            family = "Arial, sans-serif",
            size = 10.5,
            color = "black"
        )
        a <- list(
            title = "",
            titlefont = f1,
            showticklabels = TRUE,
            tickangle = 45,
            tickfont = f2,
            exponentformat = "E"
        )
        
        
        
        fig <- plot_ly(important_features_df, x = ~reorder(variables,coefficients), y = ~coefficients, type = 'bar')
        
        fig <- fig %>% layout(title = "Cycle Time Process Influencers", showlegend = F, 
                              xaxis = list(title = 'TA Variables', tickangle = 60), 
                              yaxis = list(title = 'Weight'), 
                              font = list(size = 8.5))
        
        fig <- fig %>% layout(autosize = F, width = 280, height = 300)
        
        fig
        
        
    })
    
    ### plot 2
    output$plot2 <- renderPlotly({
        
        
        fv <- file %>% filter(cycletime < 200) %>% lm(cycletime ~ open_to_apply_time,.) %>% fitted.values()
        
        
        
        fig <- file %>% filter(cycletime < 200)  %>%
            plot_ly(x = ~open_to_apply_time, y = ~cycletime, mode = "markers") %>% 
            add_markers(y = ~cycletime) %>% 
            add_trace(x = ~open_to_apply_time, y = fv, mode = "lines") %>%
            #  layout(showlegend = F)
            layout(title = "Primary Cycle Time Process Influencer", showlegend = F, 
                   xaxis = list(title = 'Open to Apply Time', tickangle = 60), 
                   yaxis = list(title = 'Cycle Time'), 
                   font = list(size = 8.5))
        
        fig <- fig %>% layout(autosize = F, width = 280, height = 300)
        
        
    })
    
    
    ### job category plot
    output$job_cat_plot <- renderPlotly({
        
        
        
        
        p <- ggplot(df_cat, aes(JOB_CATEGORY, cycletime, fill = JOB_CATEGORY)) +
            geom_boxplot()+
            theme_minimal() +
            coord_flip()+
            scale_fill_manual(values=c('#8c96c6',"#969696", "#2171b5", "#6baed6")) +
            
            #scale_fill_manual(values=c("#6baed6","#9ecae1", "#2171b5", "#4292c6")) +
            theme(legend.position='none',plot.title = element_text(size =8), 
                  axis.text = element_text(size =7.5), axis.title = element_text(size =7.5), 
                  axis.title.x = element_text(size =7.5), axis.title.y = element_text()) + 
            #scale_color_manual(name="",values = c("#08519c", "#006d2c", "#E1AF00", "#bd0026")) +
            ylab("Cycle Time (days)") +
            ylim(0, 200) +
            xlab('')+
            labs(title = "",
                 subtitle = "",
                 caption = "Source: RMS 2020") 
        
        p
        
        
    })
    
    ### job class plot
    output$job_class_plot <- renderPlot({
        
        
        jobs_class <- df_cat %>% 
            select(JOB_CLASS, cycletime) 
        
        d <- df_cat %>% 
            count(JOB_CLASS, sort = TRUE) %>%            
            mutate(counts = n) %>% 
            drop_na() %>% head(10)
        
        df3 = jobs_class[jobs_class$JOB_CLASS %in% d$JOB_CLASS,]
        
        
        #df3 = inner_join(jobs_class, d, by = "JOB_CLASS")
        
        
        
        p <- df3 %>% filter(cycletime <= 200) %>% 
            ggplot(aes(x=cycletime, y= JOB_CLASS, fill = JOB_CLASS)) + 
            geom_joy(scale = 1.5) + 
            theme_minimal() +
            #coord_flip()+
            #scale_fill_manual(values=c('#8c96c6',"#969696", "#2171b5", "#6baed6")) +
            scale_fill_manual(values=c('#3690c0',"#2b8cbe", "#c6dbef", "#9ecae1", 
                                       '#9ecae1',"#6baed6", "#4292c6", "#2171b5", 
                                       '#4eb3d3',"#67a9cf")) +
            theme(legend.position='none',plot.title = element_text(size =8), 
                  axis.text = element_text(size =8), axis.title = element_text(size =8), 
                  axis.title.x = element_text(size =8), axis.title.y = element_text()) + 
            #scale_color_manual(name="",values = c("#08519c", "#006d2c", "#E1AF00", "#bd0026")) +
            ylab("") +
            # ylim(0, 200) +
            xlab('Cycle Time (days)')+
            labs(title = "",
                 subtitle = "",
                 caption = "") 
        
        p
        
        
        
        
    })
    
    
    ## heatmap 
    output$heatmap <- renderPlotly({
        
        heatmaply(levels_grp[,2:10], show_dendrogram = FALSE,
                  fontsize_row = 8,
                  fontsize_col = 8,
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "#6392bf", high = "#1c5287", 
                                                                          limits = c(0, 35)))
        
        
        
    })
    
    
    ### Staffing Demand Code starts here 
    
    
    
    
    
    
    
    #### LEAFLET CODE HERE 
    
    
    #df %>% filter(city == input$city_name) # & median_cycletime.x >= input$range[1] & median_cycletime.x <= input$range[2])
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        df[df$median_cycletime.x >= input$range[1] & df$median_cycletime.x <= input$range[2],]
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric(input$colors, df$median_cycletime.x)
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        
        data <- filteredData()
        
        binpal <- colorBin("Blues", data$median_cycletime.x, 6, pretty = FALSE)
        
        
        
        leaflet(data) %>% 
            
            #    addTiles() %>%
            #    addProviderTiles(providers$Stamen.Toner) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            #     setView(39.02671271609735,-77.13495975135463, zoom = 10) %>% 
            fitBounds(-117, 24, -68.70975, 50.67552)  %>% 
            # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
            addCircles(radius = ~median_cycletime.x*800, weight = 1, color = "#fd903c",
                       stroke = TRUE, 
                       # fillColor = ~median_cycletime.x, 
                       fillColor = ~binpal(median_cycletime.x), #"#4a4d70",
                       fillOpacity = 0.5, popup = paste("City:", data$city, "<br>",
                                                        "State:", data$state, "<br>",
                                                        "Median Days of Cycle Time:", 
                                                        round(data$median_cycletime.x, digits = 1), "<br>" )
            )
        
        
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorpal()
        
        data <- filteredData()
        
        leafletProxy("map", data = data) %>%
            clearShapes() %>%
            addCircles(radius = ~median_cycletime.x*800, weight = 1, color = "#fd903c",
                       fillColor = ~pal(median_cycletime.x), fillOpacity = 0.6, popup = paste("City:", data$city, "<br>",
                                                                                              "State:", data$state, "<br>",
                                                                                              "Median Days of Cycle Time:", round(data$median_cycletime.x, digits = 1), "<br>" )
            )
    })
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = df)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% 
                #    fitBounds(-117, 24, -68.70975, 50.67552) %>% 
                addLegend(position = "bottomright",
                          pal = pal, values = ~median_cycletime.x, title = "Cycle Time Range"
                )
        }
    })
    
    observeEvent(input$city_name, {
        
        pal <- colorpal()
        
        
        data <- filteredData() %>% filter(city_simple == input$city_name)
        
        leafletProxy("map") %>%
            clearShapes() %>%
            setView(lng=data$long, lat=data$lat, 5) %>%
            #        addMarkers(data = df[df$city_simple == input$city_name, ],
            #                   ~Longitude,
            #                   ~Latitude,
            #                   group = "myMarkers")
            
            #addCircles(data = data[data$city_simple == input$city_name, ],
            addCircles(data = data,
                       radius = ~median_cycletime.x*800, weight = 1, color = "#fd903c",
                       fillColor = ~pal(median_cycletime.x), fillOpacity = 0.6, popup = paste("City:", data$city, "<br>",
                                                                                              "State:", data$state, "<br>",
                                                                                              "Median Days of Cycle Time:", round(data$median_cycletime.x, digits = 1), "<br>" )
            )
    })
    
    
    observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
        p <- input$map_marker_click
        if(!is.null(p$id)){
            if(is.null(input$city_name) || input$city_name!=p$id) updateSelectInput(session, "city_simple", selected=p$id)
        }
    })
    
    
    
    
    # leaflet Proxy - need to display the map in the navigation bar - second tab called staffing demand
    #     observe({
    #        req(input$tab_being_displayed == "Staffing Demand") # Only display if tab is 'Staffing Demand'
    #        leafletProxy("map", data = filteredData()) #%>%
    #          clearShapes() %>%
    #          addCircles(radius = ~median_cycletime.x, weight = 1, color = "#777777",
    #                     fillColor = ~median_cycletime.x, fillOpacity = 0.7, popup = ~paste(median_cycletime.x)
    #          )
    
    #      })
    
    
    
    
    
    ###### staffing data 
    staffing_data = reactive(data.frame(JOB_CATEGORY = input$JOB_category_staffing,
                                        LEVEL = input$level_staffing,
                                        JOB_CLASS = input$JOB_class_staffing,
                                        city_simple=input$city_name, 
                                        sourcing = input$sourcing, 
                                        hiring_mngr_review = input$hiring_mngr_review, 
                                        interview = input$interview,
                                        offer_pending = input$offer_pending, 
                                        offer_to_accept = input$offer_to_accept,
                                        Clearance = input$Clearance_staffing))
    
    
    
    #### Map page numeric outputs 
    
    
    output$total_staffing_hours <- renderText({
        cycle_time_pred <- predict(rf_model_v2, staffing_data())$predictions
        
        staffing_var_low <- 0.125 # low 
        staff_hours_low <- round(input$Requistions*cycle_time_pred*staffing_var_low, digits = 1)
        
        staffing_var_high <- 0.16 # high 
        staff_hours_high <- round(input$Requistions*cycle_time_pred*staffing_var_high, digits = 1)
        
        paste(staff_hours_low, "-", staff_hours_high, " Total Recruiter Hours")
        #  paste0(prettyNum(round(staff_hours, digits = 1), big.mark=","), " Total Recruiter Hours")
    })
    
    output$recruiters_hours <- renderText({
        
        cycle_time_pred <- predict(rf_model_v2, staffing_data())$predictions
        
        staffing_var_low <- 0.125 # low 
        staff_hours_low <- input$Requistions*cycle_time_pred*staffing_var_low
        recruiters_low <- round(staff_hours_low/input$Recruiters, digits = 1)
        
        staffing_var_high <- 0.16 # high 
        staff_hours_high <- input$Requistions*cycle_time_pred*staffing_var_high
        recruiters_high <- round(staff_hours_high/input$Recruiters, digits = 1)
        
        paste(recruiters_low, "-", recruiters_high, " Hours per Recruiter")
    })
    
    
    output$cycletime_pred <- renderText({
        
        cycle_time_pred <- predict(rf_model_v2, staffing_data())$predictions
        
        paste0(prettyNum(round(cycle_time_pred, digits = 1), big.mark=","), " Cycle Time Prediction")
        
    })
    
    ### Forecast Section 
    
    output$forecast_plot <- renderPlotly({
        
        # fcast_data = reactive(forecast_grps %>% filter(forecast_group == input$forecast_grp))
        
        forecast_grps %>% 
            filter(forecast_group == input$forecast_grp) %>% 
            plot_modeltime_forecast(.legend_show = FALSE, .title = paste((input$forecast_grp), "Forecast"))
        
    })
    
    output$forecast_table = DT::renderDataTable({
        
        DT::datatable(forecast_grps %>% filter(forecast_group == input$forecast_grp) %>% tail(14), options = list(lengthMenu = c(20, 30, 50), pageLength = 20))
        
        #     forecast_grps %>% #select(-X)
        #      filter(forecast_group == input$forecast_grp) %>% tail(14)
    })
    
    output$forecast_summary_year = DT::renderDataTable({
        
        
        forecast_grps %>% mutate( year = as.numeric(lubridate::year(.index) )) %>% 
            dplyr::mutate(.model_id = replace_na(.model_id, 0)) %>%
            filter(forecast_group == input$forecast_grp) %>%
            group_by(year) %>% 
            summarise(LRP_Hires = sum(.value)) %>% # , LRP_Lower_Bound = sum(.conf_lo), LRP_Upper_Bound = sum(.conf_hi)) %>%
            filter(year <= 2021) %>% tail(6)
        
    })
    
    
    
    
}


)