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
  dashboardHeader(title = "Police Deaths Index"),
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
      menuItem("Conclusion", tabName = "conclusion", icon = icon("book")),
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
                         tags$li("Cause of Death "),
                         tags$li("Department of Police "),
                         tags$li("Date of Death ")
                       ),
                       column(
                         6,
                         tags$li("State of Death "),
                         tags$li("K9 information ")
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
                         tags$li("Fei Gu - fgu6@jhu.edu"),
                         tags$li("Meredith Gu - xgu19@jhu.edu"),
                         tags$li("Chang Su - csu21@jhu.edu")
                       ),
                       column(
                         6,
                         tags$li("Weichen Zhao - wzhao36@jhu.edu"),
                         tags$li("Lydia Zheng - xzheng50@jhu.edu")
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
                checkboxInput("with_COVID19", label = "with COVID19", value = FALSE),
                box(
                  title = 'Trend for Human Police Deaths',
                  width = 500,
                  height = 600,
                  plotlyOutput("plot1", height = 500))
              ),
              fluidRow(
                box(
                  title = 'Trend for k9 Dog Deaths',
                  width = 500,
                  height = 600,
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
                         title = "Trend for the Selected State",
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
                    height = 480,
                    column(12,
                           tags$div(
                             tags$strong(
                               "Yearly Cumulative Trend",
                               ),
                             br(),
                           br(),
                             tags$li("Human Police"),
                             
                             tags$span("- From 1791 to date, there are over 25,803 human police officer deaths have been registered. On average, there are about 112 officered died each year.
"),
                            br(),
                             tags$span("- From the graph, we can see the human police deaths number increase at a steady rate. The growing rate has been slightly slowed when the U.S. government passed the first piece of national gun control legislation on June 26, 1934 — The National Firearms Act.
"),
                             br(),
                       
                             tags$span("- Covid influence: Due to COVID-19, the human police officers death rate increased significantly in the recent three year. There are 771 officers passed away due to COVID-19.

"),
                             br(),
                             br(),
                             tags$li("K9 Dog
"),
                        
                             
                             tags$span("- K9 dog is the nickname for the police dog, that is specifically trained to assist police and other law enforcement personnel.
"),
                            
                             br(),
                             tags$span("- From 1791 to date, there are about 487 K9 dog deaths have been registered.
"),
                             br(),
                             br(),
                             tags$strong("Map
"),
                             br(),
                           br(),
                          
                             tags$span("- Texas recorded the highest police death count with 2204, followed by New York with 1868 and California with 1732.
")
                            
                             
                             
                             ))
                )
              ),
              fluidRow(
                box(title = "Cause",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    height = 250,
                    column(12,
                           tags$div(
                             tags$span(
                               "- As shown on the graph, the top ten major cause of deaths are: gunfire, automobile crash, motorcycle crash, heart attack, vehicular assault, struck by vehicle, COVID-19, vehicle pursuit, assault, and gunfire(inadvertent). 
"),
                             br(),
                             br(),
                             tags$span("- Among the causes, gunfire caused the most number of deaths. From 1791 to 2022, there are 13,118 officers died because of gunfire, accounting for more than half of the total deaths. 
"),
                             br(),
                             br(),
                             tags$span("- From 2020, the COVID19 started to become the most common deaths causes among the police officers. In 2021, there are 443 officers died because of COVID19 and only 67 officers died under gun.")
                               
                             ))
                )
              ),
),
      
      tabItem(tabName = 'data',
              fluidRow(
                  dataTableOutput('myTable')
              ) )
    )
  )
)









server <- function(input, output, session) {
  
  ###ggplot by year###
  output$plot1 = renderPlotly({
    h <- ggplot() + 
      geom_line(data = us_total_noCovid, aes(x = Year, y = csum), color = 'blue') +
      xlab("Year") + ylab("Total Number of Deaths (Cumulative)")
    
    if(input$with_COVID19==TRUE){
      h <- ggplot() + 
        geom_line(data = us_total, aes(x = Year, y = csum), color = 'red') + 
        geom_line(data = us_total_noCovid, aes(x = Year, y = csum), color = 'blue') +
        xlab("Year") + ylab("Total Number of Deaths (Cumulative)")
    }
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
                       fillOpacity = 0.6,
                       weight = 0,
                       label = ~N)
  })
  
  output$plot3 = renderPlot({
    
    pdd <- pd %>%
      filter(State != 'United States') %>%
      group_by(Cause_of_Death)  %>%
      summarise(n = n())  %>%
      arrange(desc(n)) %>%
      head(10) 
    pdd$group_gun <- c("Gunfire",rep("Others",9))
    
    pdd %>%
      ggplot(aes(
        x = reorder(Cause_of_Death,-n),
        y = n,
        fill = group_gun
      )) + guides(fill=guide_legend(title = "Cause of Deaths"))  +
      scale_fill_manual(values = c("Gunfire" = "red", "Others" = "grey")) +
      geom_bar(stat = 'identity', position = 'dodge') +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Cause of Deaths', y = 'Deaths', title = 'Top 10 Police Deaths by Causes')
  })
  
  output$wordcloud = renderPlot({
    pd_cod <- pd %>%
      group_by(Cause_of_Death) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      filter(n >= 100)
    
    names(pd_cod) <- c("cod", "count")
    
    color = brewer.pal(8, "Dark2")
    set.seed(5)
    wordcloud(words = pd_cod$cod,
              freq=pd_cod$count,
              min.freq = 100,
              random.order=F,
              rot.per=0.1,
              scale=c(8,1),
              colors=color)
    
    output$plotdog = renderPlotly({
      
      g <- ggplot() + 
        geom_line(data = K9, aes(x = Year, y = csum), color = 'Purple') +
        xlab("Year") + ylab("Total Number of Deaths of K9 Dogs (Cumulative)")
      ggplotly(g)
      
    })
    
    output$myTable <- renderDataTable({
      return(datatable(pd))
    })
    
    

    
  })
  
  
}

shinyApp(ui = ui, server = server)
