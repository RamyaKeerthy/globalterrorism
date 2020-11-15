# df_stacked <- read_csv('stacked_data.csv')

map2 = createLeafletMap(session, 'map2')
output$map2 <- renderLeaflet({df_maps %>% 
    filter(year == min(df_maps$year)) %>% 
    leaflet(options = leafletOptions(maxZoom = 1.6, zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(~longitude, ~latitude, 
                     # popup = ~as.character(Sensor_Name),~longitude, ~latitude, 
                     radius = ~sqrt(Freq), fill = TRUE, 
                     fillColor = 'red', stroke = FALSE, opacity = 0.5,
                     fillOpacity = 0.4, 
                     layerId = ~country_name, label =  ~country_name)%>% 
    addCircles(~longitude, ~latitude, color = 'red', radius = 0.001, fill = TRUE
               ,layerId = ~country_name, label =  ~country_name) })

observeEvent(input$dropdown1,{
  leafletProxy("map2", data= df_maps %>%
                 filter(year <= input$dropdown1) %>%
                 filter(year == max(year)) ) %>%
    clearMarkers() %>%
    clearShapes() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(~longitude, ~latitude,
                     radius = ~sqrt(Freq), fill = TRUE,
                     fillColor = 'red', stroke = FALSE, opacity = 0.5,
                     layerId = ~country_name, label =  ~country_name
    )%>%
    addCircles(~longitude, ~latitude, color = 'red', radius = 0.001,
               fill = TRUE,layerId = ~country_name, label =  ~country_name)

})

data_stack <- reactive({
  df_stacked %>% 
    filter(year == input$dropdown1) %>% 
    group_by(weaptype1_txt, targtype1_txt) %>% 
    summarise(Casualities = sum(nkill)) %>% 
    rename(Target = targtype1_txt, Weapon = weaptype1_txt) %>% 
    select(Target, Weapon, Casualities)
})

output$stackedGraph <- renderPlotly({
  p <- data_stack() %>% 
    ggplot(aes(fill=Weapon, x=Target, y = Casualities))+
    geom_bar(position="stack", stat="identity", color = "white")+
    scale_fill_manual(values = c('#ee5e1e', '#ec424e', '#d73972', 
                                 '#b4418b','#874c98', '#565295', '#275185' , '#004c6d'))+
    theme_minimal()
  fig <-  ggplotly(p)
  fig %>% layout(xaxis = list(showgrid = TRUE),
                 yaxis = list(showgrid = TRUE,  showticklabels = TRUE),
                 legend=list(title=list(text='<b> Weapons </b> ')))

})