library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(janitor)
library(readr)
library(sf)
library(corrplot)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

pd <- read_csv("police_deaths_in_america_v2.csv")
loc <- read_csv("statelatlong.csv")

pd <- distinct(pd)
pd <- drop_na(pd)
summary(pd)

pd <- full_join(pd, loc, by=c('State'='City'))

us_total = pd %>% 
  filter(K9_Unit == 0) %>%
  group_by(Year) %>% 
  summarise(N=n())
us_total$csum <- cumsum(us_total$N)

us_total_noCovid <- pd %>% 
  filter(K9_Unit == 0) %>%
  filter(Cause_of_Death != 'COVID19') %>% 
  group_by(Year) %>% 
  summarise(N=n())
us_total_noCovid$csum <- cumsum(us_total_noCovid$N)

#plotdog
K9 = pd %>% 
  filter(K9_Unit == 1) %>%
  group_by(Year) %>% 
  summarise(N=n())
K9$csum <- cumsum(K9$N)

#map
pd_freq = pd %>%
  filter(!is.na(Latitude)) %>%
  group_by(State,Latitude,Longitude) %>%
  summarise(N=n())


state <- pd %>% 
  filter(State!='United States') %>%
  distinct(State) %>% 
  pull(State)


ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "Policy Deaths Index"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info")),
      menuItem(
        "Trend",
        icon = icon("chart-area"),
        menuSubItem("Yearly Cumulative Trend", tabName = "pg1"),
        menuSubItem("Map by State", tabName = "pg2")
      ),
      menuItem("Cause", tabName = "cause", icon = icon("list-alt")),
      menuItem("Conclustion", tabName = "conclusion", icon = icon("book")),
      menuItem("Database", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Project Summary"),
        fluidRow(
          box(
            title = "About the Dataset",
            solidHeader = TRUE,
            status = "success",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(
                     tags$span(
                       "This is the shiny dashboard application designed to explore the Police Death Index",
                  
                     ),
                     br(),
                     br(),
                     fluidRow(
                       column(
                         6,
                         tags$li(" "),
                         tags$li(" "),
                         tags$li(" ")
                       ),
                       column(
                         6,
                         tags$li(" "),
                         tags$li(" ")
                       )
                     ),
                     br(),
                     fluidRow(tags$i(tags$i(" ")))
                   ))
          )
        ),
        fluidRow (
          box(
            title = "Initiatives",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(
                     tags$span(
                       " Chris Cosgriff, the founder of ODMP, has said “when  a police officer is killed, it’s not an agent that loses an officer, it’s an entire nation.” "
                     ),
                     br(),
                     br(),
                     tags$span("In the United States, a large number of law enforcement personnel and police canines have been killed. Some deaths are results of diseases or other natural causes, while a great number of polices have died as a result of accidents. In this project, we would like to study the fatal police deaths happened in the United States from 1791 to the present day, figuring out the trend and causes behind the deaths. "
                     ),
                     br(),
                     br(),
                     tags$span("The data includes the victim's rank, name, department, date of death, and cause of death. The data spans from 1791 to the present day. This dataset will be updated on monthly basis.
"),
                     br(),
                     br(),
                     br(),
                     tags$li(
                       tags$strong(
                         "Data resource link: https://www.kaggle.com/datasets/mayureshkoli/police-deaths-in-usa-from-1791-to-2022 "
                       ),
                       tags$a(href = "https://www.kaggle.com/mathurinache/world-happiness-report-20152021")
                     ),

                   ))
          )
        ),
        h2("Group Members"),
        fluidRow (
          box(
            title = "About Us",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(
                     fluidRow(
                       column(
                         6,
                         tags$li("Chang Su"),
                         tags$li("Fei Gu"),
                         tags$li("Lydia Zheng")
                       ),
                       column(
                         6,
                         tags$li("Weichen Zhao"),
                         tags$li("Meredith Gu")
                       )
                     ),
                     br(),
                     fluidRow(tags$i(tags$i(" ")))
                   ))
          )
        )
      ),
      
      
      tabItem(tabName = "pg1",
              fluidRow(
                box(
                  title = 'human police death trend',
                  width = 500,
                  height = 500,
                  plotlyOutput("plot1", height = 500))
              ),
              fluidRow(
                box(
                  title = 'k9 dog death trend',
                  width = 500,
                  height = 500,
                  plotlyOutput("plotdog", height = 500))
                ),
              ),
      
      
      tabItem(tabName = "pg2",
              fluidRow(
              box(
                title = 'Map',
                width = 500,
                height = 500,
                leafletOutput("myMap", width = "100%")
              )
              ),
              
              fluidRow(
                box( width = 12, height = "100%",
                     box(
                       width = 4,
                       selectInput(
                         "State",
                         "Please Select a Region",
                         choices = state,
                         selected = "All Regions",
                         width = "100%"
                       )),
                       box(
                         width = 8,
                         plotlyOutput("plot2")
                       )
                     
                  )
                )  
              ),
      
              
      
      tabItem(tabName = 'cause',
              fluidRow(
                box(
                  width = 500,
                  height = 500,
                  plotOutput("plot3")
                )
              ),
              fluidRow(
                box(
                    width = 500,
                    height = 400,
                    plotOutput("wordcloud"))
              )),
      
      tabItem(tabName = 'conclusion',
              fluidRow(
                box(title = "Trend",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    height = 250
                )
              ),
              fluidRow(
                box(title = "Cause",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    height = 250
                )
              ),
              fluidRow(
                box(title = "Covid Influence",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    height = 250
                )
              )),
      
      tabItem(tabName = 'data',
              fluidRow(
                box(
                  title = 'source',
                  solidHeader = TRUE,
                  status = "primary",
                  width = 500,
                  collapsible = TRUE,
                  height = 1500,
                  dataTableOutput('myTable')
                )
              ) )
    )
  )
)









server <- function(input, output, session) {
  
  ###ggplot by year###
  output$plot1 = renderPlotly({
    h <- ggplot() + 
      geom_line(data = us_total, aes(x = Year, y = csum), color = 'red') + 
      geom_line(data = us_total_noCovid, aes(x = Year, y = csum), color = 'blue')
    ggplotly(h)
  })
  
  # ggplot by state (cumulative, line chart, all dates)
  output$plot2 = renderPlotly({
    data_state <- pd %>% 
      filter(State!='United States') %>%
      filter(State==input$State) %>%
      group_by(State, Year) %>% 
      summarise(N=n())

   
      
    
    data_state$csum <- ave(data_state$N, data_state$State, FUN=cumsum)
    
    f <- ggplot(data_state, aes(Year, csum, colour = State)) +
      geom_line()
    
    ggplotly(f)
    
    
  })
  
  output$myMap = renderLeaflet({
    leaflet(data = pd_freq) %>% # Create empty space
      addTiles() %>% # Show the world map
      setView(lat = 37.0902, lng = -95.7129, zoom = 4) %>% # Choose the location of the map
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addCircleMarkers(lng = ~Longitude, 
                       lat = ~Latitude, radius = ~N/50,
                       fillColor = "red", 
                       fillOpacity = 0.8,
                       weight = 0,
                       label = ~N)
  })
  
  output$plot3 = renderPlot({
    pd %>%
      filter(State != 'United States') %>%
      group_by(Cause_of_Death)  %>%
      summarise(n = n())  %>%
      arrange(desc(n)) %>%
      head(10) %>%
      ggplot(aes(
        x = reorder(Cause_of_Death,-n),
        y = n,
        fill = Cause_of_Death
      )) +
      geom_bar(stat = 'identity', position = 'dodge') +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Cause of Deaths', y = 'Deaths', title = 'Top 10 Police deaths by causes')
  })
  
  output$wordcloud = renderPlot({
    pd_cod <- pd %>%
      group_by(Cause_of_Death) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      filter(n >= 100)
    
    names(pd_cod) <- c("cod", "count")
    
    color = brewer.pal(8, "Dark2")
    
    wordcloud(words = pd_cod$cod,
              freq=pd_cod$count,
              min.freq = 100,
              random.order=F,
              rot.per=0.1,
              scale=c(4,1),
              colors=color)
    
    output$plotdog = renderPlotly({
      
      g <- ggplot() + 
        geom_line(data = K9, aes(x = Year, y = csum), color = 'red')
      ggplotly(g)
      
    })
    
    output$myTable <- renderDataTable({
      return(datatable(pd))
    })
    
    

    
  })
  
  
}

shinyApp(ui = ui, server = server)
