# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("shinythemes")
# install.packages("sf")
# install.packages("shinyWidgets")

library(shiny)
library(shinydashboard)
library(tidyverse)  
library(dplyr) 
library(leaflet)
library(ggplot2)
library(plotly)
library(shinythemes)
library(sf)
library(shinyWidgets)

# Read the relevant data
df_maps <- read_csv('data_for_map.csv')
df_all_trend <- read_csv('data_for_trend.csv')
df_stacked <- read_csv('data_for_stack.csv')
df_donut_weapon <- read_csv('data_for_weapon.csv')
df_donut_target <- read_csv('data_for_target.csv')
df_select <- read_csv('data_for_country.csv')
df_trends_year <- read_csv('data_for_trend_country.csv')
world.borders <- read_sf( dsn = getwd(), layer = "TM_WORLD_BORDERS-0.3" )

# Initialize UI using shinyUI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Global Terrorism"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("biohazard")),
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Trends", tabName = "Mainmenu", icon = icon("chart-line"),startExpanded = TRUE,
               menuSubItem("Trends by country", tabName = "trends"),
               menuSubItem("Trends by year", tabName = "radar"))
    )
  ),
  dashboardBody(  
    tags$head( 
    tags$style(HTML('.main-sidebar { font-size: 18px; } .main-header .logo {
        font-weight: bold;
        font-size: 25px;
    }
        .slider-animate-button { font-size: 20pt !important; }
      '))
  ),
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                       box(width = 12, height = 750,
                           column(6,titlePanel(h1(strong("TRENDS OF GLOBAL TERRORISM"))),
                                  div(
                                    br(),
                                    p("Terrorism is the violent act of criminal activity committed by a group 
                                    or individual associated to or inspired by an organization. It has been
                                    a part of world events since decades, but the peak of terrorism has reached 
                                      a new level in recent times."),
                                    br(),
                                    p("The study aims to visualise the trends of 
                                      global terrorism over the last five decades.
                                    With an annual analysis across the globe and regional analysis by country, 
                                      the focus is to understand the trends in the nature of incidents and the targets."),
                                    style= "font-size: 14pt; text-align: justify;"
                                  ),
                                  br(), br(),br(),
                                  p("Click to visualise", align="left", style = "color: grey"),
                actionBttn('switchtab', 'Overview',
                           color = "danger",  style = "jelly", size = "lg",icon = icon("globe")),
                actionBttn('switchtab1', 'Trends by Country',
                           color = "danger",  style = "jelly", size = "lg",icon = icon("chart-pie")),
                actionBttn('switchtab2', 'Trends by Year',
                           color = "danger",  style = "jelly", 
                           size = "lg",icon = icon("chart-bar"))
                ), column(6, img(src = "study.jpg",height=700)),
                div(class = "footer",
                    includeHTML("html/footer.html")
                ))
                  )
              ),
      
      # Overview content
      tabItem(tabName = "overview",
                fluidRow(
                  column(10, titlePanel(h2(strong("THE COMPLETE PICTURE")))),
                  column(2, actionBttn('switchtab8',
                                       color = "danger",  style = "jelly", 
                                       size = "md",icon = icon("chart-pie")),
                         actionBttn('switchtab9',
                                    color = "danger",  style = "jelly", 
                                    size = "md",icon = icon("chart-bar")),
                actionBttn('switchtab3',
                           color = "danger",  style = "jelly", 
                           size = "md",icon = icon("home")))),
              br(),
              fluidRow(
                # shinythemes::themeSelector(),
                box(width = 3, height = 715,
                    br(),
                    br(),
                  p("The spread of incidents across the globe over the years
                    shows the increasing number of countries under threat.
                     ", style = "text-align: justify;font-size: 14pt;"),
                  p("
                    Notice an incremental trend in terrorism, 
                    however, the increasing events are concentrated in few parts of the world.
                     ", style = "text-align: justify;font-size: 14pt;"),
                  br(), br(),br(),br(),br(), br(),br(),br(),br(),br(),
                  br(),br(), br(),br(),
                  p("Slide through the timeline or click on the 
                     trend line to view the spread of events in a certain year." , 
                    style = "text-align: justify;font-size: 10pt;color: grey"),
                  p("Want to visualise the trend on the map? Click on the play button under the slider" , 
                    style = "text-align: justify;font-size: 10pt; color: grey")
                ),
                box(width = 9,
                uiOutput(outputId = "slider", label='Year'),
                leafletOutput("map1", width = "100%", height = "420"),
                plotlyOutput("linePlot0", width = "100%", height = 165
                           )
              ))),
      # Second tab content
      tabItem(tabName = "trends",
              fluidRow( column(10,titlePanel(h2(strong("STUDY TRENDS BY COUNTRY")))),
                              column(2,actionBttn('switchtab6', 
                                                  color = "danger",  style = "jelly", 
                                                  size = "md",icon = icon("chart-bar")),
                                     actionBttn('switchtab4',
                                                  color = "danger",  style = "jelly", 
                                                  size = "md",icon = icon("home")))),
                        fluidRow(
                          box(width = 4, height = 330,
                              pickerInput('dropdownCountry', 'COUNTRY', 
                                          as.vector(sort(df_select$country_name)), 
                                          options=pickerOptions(liveSearch=T, size = 6)),
                            leafletOutput("map", width = "100%", height = 200)
                          ),
                          box(width = 8,  height = 330,
                              p("Incidents trend of a country", style="font-size: 14pt;", align="center"),
                            plotlyOutput("linePlot1", width =  "100%", height = 275)
                          )
                          
                        ),
                        fluidRow(
                          box(height = 360,width = 12, 
                              h3(strong("Targets and Weapons of the Country"), align="center"),
                            column(width=6,plotlyOutput("piePlotTarget", width = "100%", height = 250)),
                            column(width=6, plotlyOutput("piePlotWeapon", width = "100%", height = 250))
                          )
                        )
                        
              ),
      tabItem(tabName = "radar",
              fluidRow(column(10,titlePanel(h2(strong("STUDY TRENDS BY YEAR")))),
                              column(2,actionBttn('switchtab7', 
                                                  color = "danger",  style = "jelly", 
                                                  size = "md",icon = icon("chart-pie")),
                                     actionBttn('switchtab5',
                                                  color = "danger",  style = "jelly", 
                                                  size = "md",icon = icon("home")))),
              fluidRow(box(width = 12,
                
                p("The study of trend by year is important because it 
                correlates the global data on the weapons used, targetted groups,
                  and their respective casualities in a certain year.
                  This interactive screen shows how the targets change over the years.", style="font-size: 14pt;"),
                br(),
                p("Change the year to visualise the trend on map and 
                  hover on the stacked bar chart to see the exact figures ", style="font-size: 11pt;color: grey")
              )),
                fluidRow(
                  box(pickerInput('dropdown1', 'YEAR', 
                                  as.vector(sort(unique(df_maps$year))), 
                                  options=pickerOptions(liveSearch=T, size = 6)),
                    # selectInput("dropdown1",label="Year",
                    #               choices=as.vector(sort(unique(df_maps$year)))),
                      leafletOutput("map2", width = "100%", height = "400")),
                  box(height = "500",plotlyOutput("stackedGraph", height = "450"))
                ))
      )
    )
  )


server <- function(input, output,session) {
  source("P0_interact.R",local=TRUE)
  source("P1_interact.R",local=TRUE)
  source("P2_interact.R",local=TRUE)
  source("P3_interact.R",local=TRUE)
}

shinyApp(ui, server)