########################## Packages in use ########################## 

packages <- c('shiny', 'shinydashboard', 'shinythemes', 
              'tidyverse', 'ggplot2', 'plotly')

for (p in packages){
  library(p, character.only=T)
}

########################## Reading the files ########################## 

#all_wday <- readRDS('data/all_wday.rds')
all <- readRDS('data/all.rds')
restaurants <- readRDS('data/restaurants.rds')
pubs <- readRDS('data/pubs.rds')

lvl <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

########################## Shiny UI ####################################

ui <- navbarPage(
  title = "Financial Health of the city",
  fluid = TRUE,
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel("User Guide"),
  navbarMenu("Businesses in Town",
             tabPanel("Restaurants",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "locationId1",
                                                 label = "Select Restaurant Id:",
                                                 choices = sort(unique(restaurants$locationId)),
                                                 selected = 445)
                        ),
                        mainPanel(width = 9,
                                  plotlyOutput("lineplot1"))
                      )
             ),
             tabPanel("Pubs",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "locationId2",
                                                 label = "Select Pub Id:",
                                                 choices = sort(unique(pubs$locationId)),
                                                 selected = 442)
                        ),
                        mainPanel(width = 9,
                                  plotlyOutput("lineplot2"))
                      )),
             tabPanel("Overall")),
  navbarMenu("Q2"),
  navbarMenu("Employment & Turnover")         
)

############################## Shiny Server #################################

server <- function(input, output){
  
  ############## Shiny Server: restaurant weekday ####################
  output$lineplot1 <- renderPlotly({
    
    restaurants1 <- restaurants %>%
      group_by(locationId, date) %>%
      summarise(visits= n(),
                weekday = first(weekday)) %>%
      ungroup() 
    
    restaurants_mean <- restaurants1 %>%
      group_by(locationId, weekday) %>%
      summarise(meanv = mean(visits)) %>%
      ungroup()
    
    ggplotly(restaurants1 %>%
      filter(locationId == input$locationId1) %>%
      ggplot(aes(x= date,
                 y = visits)) +
      geom_line() +
      geom_hline(data = restaurants_mean %>%
                   filter(locationId==input$locationId1),
                 aes(yintercept = meanv),
                 color= '#20b2aa',
                 linetype= 'dashed',
                 size = .4) +
      labs(x= 'Weekday', y= 'Number of Customer Visits',
           title= paste0('Restaurant ', input$locationId1,
           "'s Customer Visits Trend by Weekdays")) +
      facet_grid(~ factor(weekday, levels = lvl))+
        theme_bw())
  })
  
  ############## Shiny Server: restaurant weekday ####################
  output$lineplot2 <- renderPlotly({
    
    pubs1 <- pubs %>%
      group_by(locationId, date) %>%
      summarise(visits= n(),
                weekday = first(weekday)) %>%
      ungroup() 
    
    pubs_mean <- pubs1 %>%
      group_by(locationId, weekday) %>%
      summarise(meanv = mean(visits)) %>%
      ungroup()
    
    ggplotly(pubs1 %>%
      filter(locationId == input$locationId2) %>%
      ggplot(aes(x= date,
                 y = visits)) +
      geom_line() +
      geom_hline(data = pubs_mean %>%
                   filter(locationId==input$locationId2),
                 aes(yintercept = meanv),
                 color= '#ff7f50',
                 linetype= 'dashed',
                 size = .4) +
      labs(x= 'Weekday', y= 'Number of Customer Visits',
           title= paste0('Pub ', input$locationId2,
                         "'s Customer Visits Trend by Weekdays")) +
      facet_grid(~ factor(weekday, levels = lvl))+
        theme_bw())
      
  })
  
  
}
  
shinyApp(ui = ui, server = server)
  

