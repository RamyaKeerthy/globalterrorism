#Map server side
map = createLeafletMap(session, 'map')
output$map <- renderLeaflet({    
  myBorders <- world.borders[world.borders$NAME == input$dropdownCountry, ]
  leaflet(options = leafletOptions(maxZoom = 18, zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = myBorders, color = "darkred",  weight = 3) })

# Get data based on country dropdown
filtered_data <- reactive({
  df_trends_year[df_trends_year$country_name==input$dropdownCountry,]
})

filtered_years <- reactive({
  df_spider[df_spider$country_name==input$dropdownCountry,]
})

data_line <- reactive({
  df_trends_year[df_trends_year$country_name==input$dropdownCountry,] %>% 
    rename(Year = year, Incidents = Freq)
})
  
# Trend line
output$linePlot1 <- renderPlotly({
  p <- ggplot(data = data_line(), aes(x=Year, y=Incidents)) + 
    geom_line(stat='identity',size = 0.5, color = "#b10026")+
    geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +theme_minimal()
  ggplotly(p)})

# hover text
displayed_text1 <- reactive({
  req(input$plot_hover1)
  hover1 <- input$plot_hover1
  dist <- sqrt((hover1$x - filtered_data()$year)^2 + (hover1$y - filtered_data()$Freq)^2)
  
  if(min(dist) < 0.3) {
    filtered_data()$year[which.min(dist)]
  } else {
    NULL
  }
})

output$hover_info1 <- renderPrint({
  req(displayed_text1())
  
  cat("Year: ")
  displayed_text1()
})
data_pie_target <- reactive({
  df_donut_target[df_donut_target$country_name==input$dropdownCountry,]
})

output$piePlotTarget <- renderPlotly({
  
  colors <- c('rgb(55, 74, 139)', 'rgb(175, 47, 59)', 'rgb(236, 99, 61)',
              'rgb(102, 149, 203)','rgb(68, 103, 173)','rgb(252, 202, 151)','rgb(208, 237, 246)')
  fig <- plot_ly(data_pie_target(), labels = ~targtype1_txt, values = ~Freq, type = 'pie',
                 textposition = 'inside', marker = list(colors = colors,
                                                        line = list(color = '#FFFFFF', width = 2)),
                 insidetextfont = list(color = '#FFFFFF'), width = 500, height = 280)
  fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        legend=list(title=list(text='<b> Targets </b>')))
  
  fig
  })


data_pie_weapon <- reactive({
  df_donut_weapon[df_donut_weapon$country_name==input$dropdownCountry,]
})

output$piePlotWeapon <- renderPlotly({
  colors <- c('rgb(55, 74, 139)', 'rgb(175, 47, 59)', 'rgb(236, 99, 61)',
              'rgb(102, 149, 203)','rgb(68, 103, 173)','rgb(252, 202, 151)','rgb(208, 237, 246)')
  fig <- plot_ly(data_pie_weapon(), labels = ~weaptype1_txt, values = ~Freq, type = 'pie',
                 textposition = 'inside', marker = list(colors = colors,
                                                        line = list(color = '#FFFFFF', width = 2)),
                 insidetextfont = list(color = '#FFFFFF'), width = 500, height = 280)
  fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        legend=list(title=list(text='<b> Weapons </b> ')))
  
  fig 
  # bp1 <- ggplot(data = df_donut_weapon[df_donut_weapon$country_name==input$dropdownCountry,],
  #               aes(y=Freq, x="",fill=weaptype1_txt)) + 
  #   geom_bar(width = 1, stat="identity", color = "white") 
  # pie1 <- bp1 + coord_polar("y", start=0) + theme_void()
  # pie1 + scale_fill_brewer(palette="Set2")
  })