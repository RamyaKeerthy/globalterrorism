# df_all_trend<- read_csv('./p1_trend.csv')
# df_maps <- read_csv('./p1_maps.csv')

output$slider <- renderUI({
  sliderInput("year",min = min(df_maps$year), 
              max = max(df_maps$year), 
              value = min(df_maps$year), 
              animate = T, label = NULL,
              sep = "")
})

map1 = createLeafletMap(session, 'map1')
output$map1 <- renderLeaflet({df_maps %>% 
    filter(year == min(df_maps$year)) %>% 
    leaflet(options = leafletOptions(minZoom = 1.45)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(~longitude, ~latitude,  
                     radius = ~sqrt(Freq), fill = TRUE, 
                     fillColor = 'red', stroke = FALSE, opacity = 1,
                     fillOpacity = 0.4, 
                     layerId = ~country_name, label =  ~country_name)%>% 
    addCircles(~longitude, ~latitude, color = 'darkred', radius = 0.001, fill = TRUE
               ,layerId = ~country_name, label =  ~country_name)  })

data_for_line <- reactive({
  df_all_trend %>% 
    rename(Year = year) %>% 
    select(Year, Incidents)
})

output$linePlot0 <- renderPlotly({
  p <- ggplot(data = data_for_line(), aes(x=Year, y=Incidents)) + 
    coord_cartesian(xlim = c(1970,2017), ylim = c(0, 17000)) +
    geom_line(stat='identity',size = 0.5, color = "#b10026") +
    geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +theme_minimal()
  ggplotly(p, source = "sub_plot") %>% 
    event_register("plotly_click")
})


s <- reactive({
  event_data("plotly_click",source = "sub_plot")
})

# output$hoverDataOut <- renderText({
#   paste("Hover data:", paste(as.character(unlist(s())[3])))
# })

observeEvent(s(), {
  updateSliderInput(session, inputId = "year", label = "Year",
                    min = min(df_maps$year),
                    max = max(df_maps$year),
                    value = as.character(unlist(s())[3]))
})

observeEvent(input$year,{
  leafletProxy("map1", data= df_maps %>%
                 filter(year <= input$year) %>%
                 filter(year == max(year)) ) %>%
    clearMarkers() %>%
    clearShapes() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(~longitude, ~latitude, 
                     radius = ~sqrt(Freq), fill = TRUE, 
                     fillColor = 'red', stroke = FALSE, opacity = 1, 
                     layerId = ~country_name, label =  ~country_name
    )%>% 
    addCircles(~longitude, ~latitude, color = 'darkred', radius = 0.001, 
               fill = TRUE,layerId = ~country_name, label =  ~country_name)
  
})