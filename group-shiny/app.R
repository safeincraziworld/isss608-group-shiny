########################## Packages in use ########################## 

# packages <- c('shiny', 'shinydashboard', 'shinythemes', 
#               'tidyverse', 'ggplot2', 'plotly')


pacman::p_load(ggiraph, plotly, rmarkdown, psych, sf, tmap,
               DT, patchwork, gglorenz, hrbrthemes,shinydashboard,
               gganimate, tidyverse, ggthemes, reactable,
               readxl, gifski, gapminder, quantmod, shinythemes,
               treemap, treemapify, ggridges,dataui,zoo, reactablefmtr, crosstalk,
               rPackedBar, lubridate, remotes, ggplot2, dplyr, ggstatsplot,fs,
               lubridate, shiny, tools, writexl, ggHoriPlot,heatmaply ,rsconnect,shinycssloaders)


########################## Reading the files ########################## 

UserGuide <- file_temp("UserGuide.pdf", tmp_dir = "www", ext = ".pdf")



all <- readRDS('data/Q1/all.rds')
all_monthly <- readRDS('data/Q1/all_monthly.rds')

lvl <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

#### Q2 ####
Participants<-read_csv("data/Q2/Participants.csv",show_col_types = FALSE)
ParticipantsApartmentLocation<-read_csv("data/Q2/ParticipantsApartmentLocation.csv",show_col_types = FALSE)
buildings<-read_sf("data/Q2/Buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

ParticipantSavings<-readRDS("data/Q2/ParticipantSavings.rds")
FinHealth<-readRDS("data/Q2/FinHealth.rds")
ParticipantMonthlySavings<-readRDS("data/Q2/ParticipantMonthlySavings.rds")
ParticipantMonthlySpark<-readRDS("data/Q2/ParticipantMonthlySpark.rds")
buildings<-read_sf("data/Q2/Buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

InterestGroupGraph<-readRDS("data/Q2/InterestGroupGraph.rds")
StatusLogDetails<-readRDS("data/Q2/StatusLogDetails.rds")
EducationExpenseCategory<-readRDS("data/Q2/EducationExpenseCategory.rds")
ExpenseProportionMonthly<-readRDS("data/Q2/ExpenseProportionMonthly.rds")




#### Q3 ####

jobs <- readRDS("data/Q3/jobs.rds")
logs_path_PrevJob <- readRDS("data/Q3/logs_path_PrevJob.rds")
logs_path_RecJob <- readRDS("data/Q3/logs_path_RecJob.rds")
no.ofjobs<- readRDS("data/Q3/no.ofjobs.rds")
no.ofjobs_table <- readRDS("data/Q3/no.ofjobs_table.rds")
participants <- readRDS("data/Q3/participants.rds")
prevEmp_sf <- readRDS("data/Q3/prevEmp_sf.rds")
recntEmp_sf <- readRDS("data/Q3/recntEmp_sf.rds")
switchEmployeesAllDetails <- readRDS("data/Q3/switchEmployeesAllDetails.rds")
workinmoreplaces <- readRDS("data/Q3/workinmoreplaces.rds")




########################## Q3 ########################## 

partid = c(logs_path_PrevJob$participantId)

###################### drilldown bar graph #############3

alias <- switchEmployeesAllDetails
alias$participantId=as.numeric(alias$participantId)
morePlacesPayChange <- left_join(x = alias,
                                 y= workinmoreplaces,
                                 by = "participantId")

########################## Shiny UI ####################################

ui <- navbarPage(
  title = "Financial Health of the city",
  fluid = TRUE,
  theme=shinytheme("cosmo"),
  id = "navbarID",
  tabPanel("Introduction",
           h1("Welcome to our App!"),
           
           
           mainPanel(
             tags$a(href="https://github.com/safeincraziworld/isss608-group-shiny/blob/master/group-shiny/www/UserGuide.pdf", "Click Here for user guide!"),
             img(src = "Graph1.png",height="auto",width="auto"))),
      
  
  
  navbarMenu("Businesses in Town",
             tabPanel("Customer Visits",
                      titlePanel("Is the business more prosperous on weekdays or weekends?"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = 'locationType1',
                                                 label= 'Select Business Type:',
                                                 choices= c('Restaurant', 'Pub'),
                                                 selected = 'Restaurant'),
                                     uiOutput('secondSelection')
                        ),
                        mainPanel(width = 9,
                                  shinycssloaders::withSpinner(plotlyOutput("lineplot")))
                      )
             ),
             tabPanel("Revenue by Month",
                      titlePanel("Is business revenue growing or diminishing?"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "locationType2",
                                                 label = 'Select Business Type:',
                                                 choices= c('Restaurant', 'Pub'),
                                                 selected = 'Restaurant'),
                                     uiOutput('secondSelection2')
                        ),
                        mainPanel(width = 9,
                                  shinycssloaders::withSpinner(plotlyOutput("lineplot2")))          
                      )),
             tabPanel("Revenue by Rank",
                      titlePanel("Which businesses have the highest monthly revenue?"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId='month',
                                                 label = 'Select Month:',
                                                 choices = sort(unique(all_monthly$`Month Year`)),
                                                 selected = 'Mar 2022')
                        ),
                        mainPanel(width= 9,
                                  shinycssloaders::withSpinner(packedBarOutput("packedbar")))
                      ))
             #tabPanel('Business Location',
             #sidebarLayout(
             #sidebarPanel(width = 3,
             #selectInput(inputId = ,
             #label = ,
             #choices = ,
             #selected = )
             #))
             #)
  ),
  
  #######
  navbarMenu("Participants in Town",
             
             tabPanel("Participants Health",
                      titlePanel("Are the Participants financially healthy?"),
                      fluidRow(
                        column(3,div(valueBoxOutput("value1"),style="color:white"),style="background-color:navy;width=100px"),
                        
                        column(6,div(valueBoxOutput("value2"),style="color:white"),style="background-color:purple;"),
                        
                        column(3,div(valueBoxOutput("value3"),style="color:white"),style="background-color:green;")
                      ),
                      
                      fluidRow(
                        column(3,selectInput(inputId = "HaveKidsDashboard", 
                                             label =   "Have Kids",
                                             choices =  c("All" = "All",
                                                          "Yes" = "TRUE",
                                                          "No" = "FALSE"),
                                             selected = "All"
                        )),
                        
                        column(3,selectInput(inputId = "HouseHoldSizeDashboard", 
                                             label =   "HouseHold Size",
                                             choices =  c("1" = "1",
                                                          "2" = "2",
                                                          "3"="3"),
                                             multiple = TRUE,
                                             selected = c("1",
                                                          "2",
                                                          "3")
                        )),
                        column(4,selectInput(inputId = "EducationDashboard", 
                                             label =   "Education Qualification",
                                             choices =  c("Low"="Low",
                                                          "High School or College"="HighSchoolOrCollege",
                                                          "Bachelors"="Bachelors",
                                                          "Graduate"="Graduate"),
                                             multiple = TRUE,
                                             selected = c("Low",
                                                          "HighSchoolOrCollege",
                                                          "Bachelors",
                                                          "Graduate")
                        ))
                        
                        
                        
                        
                        
                      ),
                      
                      fluidRow(
                        column(3,sliderInput("age", "Age:",
                                             min = min(participants$age), max = max(participants$age),
                                             value = c(min(participants$age),max(participants$age))))),
                      
                      fluidRow(
                        column(12,shinycssloaders::withSpinner(reactableOutput("EarningReactableDashboard", 
                                                                               width = "auto", 
                                                                               height = "auto", 
                                                                               inline = FALSE)))
                      )),
             tabPanel("Wages vs Cost of living",
                      titlePanel("Are the participants spending more than they earn?"),
                      tabsetPanel(
                        tabPanel("Overall Picture",
                                 
                                 
                                 
                                 
                                 
                                 
                                 fluidRow(
                                   column(3,checkboxGroupInput("Lorenz", "Variables to show:",
                                                               c("Earning" = "TotalEarning",
                                                                 "Expense" = "TotalExpense"),
                                                               selected = "TotalEarning")),
                                   column(9,shinycssloaders::withSpinner(plotlyOutput("LorenzCurve")))
                                   
                                 )),
                        tabPanel("Participant Details",
                                 fluidRow(
                                   
                                   
                                   
                                   
                                   column(3,selectInput(inputId = "Months", 
                                                        label =   "Select the Month",
                                                        c("Mar 2022"="Mar 2022",
                                                          "Apr 2022"="Apr 2022",
                                                          "May 2022"="May 2022",
                                                          "Jun 2022"="Jun 2022",
                                                          "Jul 2022"="Jul 2022",
                                                          "Aug 2022"="Aug 2022",
                                                          "Sep 2022"="Sep 2022",
                                                          "Oct 2022"="Oct 2022",
                                                          
                                                          "Nov 2022" = "Nov 2022",
                                                          "Dec 2022" = "Dec 2022",
                                                          "Jan 2023" = "Jan 2023",
                                                          "Feb 2023" = "Feb 2023",
                                                          "Mar 2023"="Mar 2023",
                                                          "Apr 2023"="Apr 2023",
                                                          "May 2023"="May 2023"
                                                        ),
                                                        multiple=TRUE,
                                                        selected = "Nov 2022"))
                                 ),
                                 
                                 
                                 # fluidRow(
                                 #   column(3,checkboxGroupInput("category", "Categories:",
                                 #                               c("Education" = "Education",
                                 #                                 "Food" = "Food",
                                 #                                 "Recreation" = "Recreation",
                                 #                                 "Shelter" = "Shelter"),
                                 #                               selected = "Education")),
                                 #   column(9,plotlyOutput("ExpensesTrellis"))
                                 #   
                                 # ),
                                 fluidRow(
                                   column(12,shinycssloaders::withSpinner(reactableOutput("WagesExpenseDashboard", 
                                                                                          width = "auto", 
                                                                                          height = "auto", 
                                                                                          inline = FALSE)))
                                 )),
                        tabPanel("Expenses",
                                 fluidRow(
                                   column(3,selectInput(inputId = "categorySelected", 
                                                        label =   "Select the Category",
                                                        c("Education" = "Education",
                                                          "Food" = "Food",
                                                          "Recreation" = "Recreation",
                                                          "Shelter" = "Shelter"),
                                                        multiple=TRUE,
                                                        selected = c("Food","Education","Shelter","Recreation"))),
                                   column(3,selectInput(inputId = "Week", 
                                                        label =   "Select the Week",
                                                        c("Monday"="Monday","Tuesday"="Tuesday",
                                                          "Wednesday"="Wednesday","Thursday"="Thursday",
                                                          "Friday"="Friday","Saturday"="Saturday","Sunday"="Sunday"),
                                                        multiple=TRUE,
                                                        selected = c("Monday","Tuesday",
                                                                     "Wednesday","Thursday",
                                                                     "Friday","Saturday","Sunday"))),
                                   
                                   
                                   column(12,shinycssloaders::withSpinner(plotOutput("HeatMap")))
                                 ),
                        )
                        
                        # fluidRow(
                        #   column(12,plotOutput("FinLocation")),
                        
                        
                        
                        
                        
                        
                        #mainPanel(
                        #  uiOutput("CoordinatedPlot"),
                        #  width = "100%", height = "400px"
                        #),
                        
                        
                        
                        
                      )),
             
             tabPanel("Similarities between groups",
                      titlePanel("Can we find some similarity?"),
                      tabsetPanel(
                        tabPanel("Interest Groups",
                                 fluidRow(column(12,shinycssloaders::withSpinner(reactableOutput("GroupsDashboard", 
                                                                                                 width = "auto", 
                                                                                                 height = "auto", 
                                                                                                 inline = FALSE)))),
                                 fluidRow(
                                   column(3,
                                          selectInput("InterestGroup", "Interest Group",
                                                      c("A" = "A",
                                                        "B" = "B",
                                                        "C" = "C",
                                                        "D" = "D",
                                                        "E"="E",
                                                        "F"="F",
                                                        "G"="G",
                                                        "H"="H",
                                                        "I"="I"),
                                                      multiple = TRUE,
                                                      selected = c("A" = "A",
                                                                   "B" = "B",
                                                                   "C" = "C",
                                                                   "D" = "D",
                                                                   "E"="E",
                                                                   "F"="F",
                                                                   "G"="G",
                                                                   "H"="H",
                                                                   "I"="I"))),
                                   
                                   
                                   column(9,shinycssloaders::withSpinner(plotOutput("InterestGroups"))))),
                        tabPanel("Cluster Analysis",
                                 fluidRow(
                                   column(3,selectInput(inputId = "category", 
                                                        label =   "Select the Category",
                                                        c("Education" = "Education",
                                                          "Food" = "Food",
                                                          "Recreation" = "Recreation",
                                                          "Shelter" = "Shelter"),
                                                        multiple=TRUE,
                                                        selected = c("Food","Education"))),
                                   column(9,shinycssloaders::withSpinner(plotlyOutput("HeatMapGroup")))
                                   
                                 )))
                      
                      
             )
             
  ),
  
  navbarMenu("Employment & Turnover",
             tabPanel("Turnover Analysis",
                      fluidPage(
                        titlePanel("What is the impact of job switch among participants ?"),
                        fluidRow(
                          column(
                            width = 12,
                            height = 100,
                            tabsetPanel(
                              
                              tabPanel("Job Route",
                                       box(
                                         width = 20,
                                         height = 100,
                                         selectInput(inputId = "participants",
                                                     label = "Select Participant Id",
                                                     choices = partid,
                                                     selected = c(partid[5]))
                                         
                                       ),
                                       fluidRow(
                                         box("Commute route from home to work before job change (Marker- Ex Employer Location)",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "befRoute",
                                                                                     width = 500,
                                                                                     height = 500),
                                             )),
                                         box("Commute route from home to work after job change  (Marker- New Employer Location)",
                                             shinycssloaders::withSpinner(plotOutput(outputId  = "aftRoute",
                                                                                     width = 500,
                                                                                     height = 500))
                                         )
                                       )
                                       
                                       
                              ),
                              tabPanel("Change of Wage",
                                       sidebarPanel(
                                         width = 2,
                                         #height = 50,
                                         radioButtons(inputId = "groupbyCategory", 
                                                      label =   "Choose Category :",
                                                      choices =  c("Education Level" = "educationLevel",
                                                                   "Household Size" = "householdSize",
                                                                   "Kids" = "haveKids",
                                                                   "Interest Group" = "interestGroup",
                                                                   "Age Group" = "ageGroup"),
                                                      selected = "interestGroup"
                                         )),
                                       mainPanel(width = 25,
                                                 box(
                                                   shinycssloaders::withSpinner(plotOutput("barPayPlot",
                                                                                           width = "900px")))
                                                 #height = "500px"))
                                       ),
                                       
                                       DT::dataTableOutput(outputId = "barPayPlotTable")
                                       
                                       
                              ),
                              tabPanel("Change of Workplaces",
                                       titlePanel("Please click on the bar to see the pay comparison"),
                                       mainPanel(
                                         fluidRow(
                                           column(6,
                                                  shinycssloaders::withSpinner(plotlyOutput(
                                                    outputId="placesworked", 
                                                    width="500px",
                                                    height="400px"))),  
                                           column(6,
                                                  shinycssloaders::withSpinner(plotlyOutput(
                                                    outputId="paychange", 
                                                    width="500px",
                                                    height="400px")))
                                         ),
                                         
                                         #verbatimTextOutput("info")
                                       ),
                                       
                              ),
                              
                              
                            )
                          )
                        )
                      )
             ),
             tabPanel("Employment Pattern",
                      fluidPage(
                        titlePanel("What is the pattern found in the employment ?"),
                        fluidRow(
                          column(
                            width = 12,
                            height = 100,
                            tabsetPanel(
                              tabPanel("Education vs Pay",
                                       sidebarPanel(
                                         width = 3,
                                         #height = 320,
                                         checkboxGroupInput(inputId = "edu", 
                                                            label =   "Education Requirement:",
                                                            choices =  c("Low" = "Low",
                                                                         "High School or College" = "HighSchoolOrCollege",
                                                                         "Bachelors" = "Bachelors",
                                                                         "Graduate" = "Graduate"),
                                                            selected = c("Low","HighSchoolOrCollege")
                                         ),
                                         textInput(
                                           inputId = "plot_title",
                                           label = "Plot title",
                                           placeholder = "Enter text to be used as plot title"),
                                         actionButton("goButton", "Go!"),
                                         
                                         checkboxInput(inputId = "showData",
                                                       label = "Show data table",
                                                       value = TRUE)
                                       ),
                                       
                                       mainPanel(width = 15,
                                                 box(
                                                   shinycssloaders::withSpinner(plotlyOutput("rainPlot",
                                                                                             height = "400px")))
                                       ),
                                       
                                       DT::dataTableOutput(outputId = "rainPlotTable")
                                       
                              ),
                              
                              
                            )
                          )
                        )
                        
                      )),
             tabPanel("Employer Health",
                      fluidPage(
                        titlePanel("Which employers are financially healthy ?"),
                        fluidRow(
                          column(
                            width = 12,
                            height = 100,
                            tabsetPanel(
                              tabPanel("Wage By Employers",
                                       sidebarPanel(
                                         width = 4,
                                         height = 80,
                                         selectInput(inputId = "color",
                                                     label = "Choose Color type:",
                                                     choices = c("Light - Dark Blue" = "Blues",
                                                                 "Light - Dark Green" = "Greens",
                                                                 "Light - Dark Orange" = "Oranges",
                                                                 "Light - Dark Purple" = "Purples",
                                                                 "Light - Dark Red" = "Reds"),
                                                     selected = "Blues"),
                                         
                                         sliderInput(inputId = "no.ofemp",
                                                     label = "Choose min. and max. no. of jobs",
                                                     min = 2,
                                                     max = 9,
                                                     value= c(3,7),
                                         ),
                                         box(
                                           width = 15,
                                           height=200,
                                           h4('How to read the chart ?'),
                                           textOutput("readChart"),
                                         )
                                       ),
                                       box(shinycssloaders::withSpinner(plotOutput("treemapPlot"))),
                                       DT::dataTableOutput(outputId = "treemapTable")
                                       
                                       
                                       
                              ),
                              tabPanel("Employers location",
                                       sidebarPanel(
                                         width = 4,
                                         height = 80,
                                         selectInput(inputId = "emp",
                                                     label = "Choose an option ",
                                                     choices = c("Employees left the firm" = "left",
                                                                 "Employees joined the firm" = "joined"),
                                                     selected = "Left"),
                                         box(
                                           width = 15,
                                           height=200,
                                           h4('How to read the Map ?'),
                                           textOutput("readMap")
                                         ),
                                         checkboxInput(inputId = "showData",
                                                       label = "Show data table",
                                                       value = TRUE)
                                       ),
                                       box(shinycssloaders::withSpinner(tmapOutput("mapPlot"))),
                                       DT::dataTableOutput(outputId = "aTable")
                                       
                              ),
                              
                            )
                          )
                        )
                        
                      )))     
)

############################## Shiny Server #################################

server <- function(input, output){
  
  output$Introduction<-renderPlot({
    
    s1<-ggplot(data = diamonds, mapping = aes(x = clarity, y=x)) + geom_boxplot()+
      theme_void()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")+
      scale_y_continuous(NULL,breaks = NULL)+
      scale_fill_brewer(palette="Set3")
    
    
    s2<-ggplot(diamonds,aes(x=depth,color=cut))+
      geom_density()+
      theme_void()+
      theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")+
      scale_y_continuous(NULL,breaks = NULL)
    
    s3<-ggplot(data = diamonds, mapping = aes(x = clarity)) + geom_bar(aes(fill = cut))+
      theme_void()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")+
      scale_y_continuous(NULL,breaks = NULL)+
      scale_fill_brewer(palette="Set3")
    
    fair_diamonds <- diamonds %>%
      filter(cut == "Fair")
    
    s5<-ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point() + 
      geom_line()+
      theme_void()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")+
      scale_y_continuous(NULL,breaks = NULL)
    
    s6<-ggplot(data = diamonds) +
      geom_bar(mapping = aes(x = cut, fill = cut), width = 1, show.legend = FALSE) +
      coord_polar() +
      theme_void()+
      labs(title = "", x = NULL, y = NULL)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")+
      scale_y_continuous(NULL,breaks = NULL)
    
    s7<-ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
      geom_violin() +
      scale_y_log10()+
      scale_fill_brewer()+
      theme_void()+
      labs(title = "", x = NULL, y = NULL)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")
    s8<-ggplot(diamonds, aes(cut, color)) +
      geom_jitter(aes(color = cut), size = 0.5)+
      theme_void()+
      labs(title = "", x = NULL, y = NULL)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")
    
    s9<-ggplot(diamonds[1:100, c("color", "depth")], aes(depth, y = color,
                                                         fill = 0.5 - abs(0.5 - stat(ecdf)))) +
      stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
      scale_fill_gradient(low = "white", high = "#87CEFF",
                          name = "Tail prob.")+
      theme_void()+
      labs(title = "", x = NULL, y = NULL)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")
    
    s10<-ggplot(iris, aes(Species, Sepal.Width)) + 
      ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="pink") + 
      geom_boxplot(width = .1, outlier.shape = NA) +
      gghalves::geom_half_point(side = "l", range_scale = 0, shape = 95, size = 15, alpha = .3)+
      theme_void()+
      labs(title = "", x = NULL, y = NULL)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")
    p<-(s3 + s2/s7) / (s1 + s5+s6)/(s9+s10)
    p
    
  })
  
  
  
  output$frame <- renderUI({
    link<<-paste0("https://github.com/safeincraziworld/isss608-group-shiny/blob/master/group-shiny/www/UserGuide.pdf")
    my_test <- tags$iframe(src=link, height=600, width=535)
    print(my_test)
    
  })
  
  ############## Shiny Server: weekday visits ####################
  output$secondSelection <- renderUI({
    selectInput(inputId= 'locationId1',
                label= 'Select Business ID:',
                choices= sort(unique(all$locationId[all$locationType==input$locationType1])),
                selected = 445)
  })
  output$lineplot <- renderPlotly({
    
    all1 <- all %>%
      group_by(locationType, locationId, date) %>%
      summarise(visits= n(),
                weekday = first(weekday)) %>%
      ungroup() 
    
    all_mean <- all1 %>%
      group_by(locationId, weekday) %>%
      summarise(meanv = mean(visits)) %>%
      ungroup()
    
    ggplotly(all1 %>%
               filter(locationId == input$locationId1) %>%
               ggplot(aes(x= date,
                          y = visits)) +
               geom_line() +
               geom_hline(data = all_mean %>%
                            filter(locationId==input$locationId1),
                          aes(yintercept = meanv),
                          color= '#20b2aa',
                          linetype= 'dashed',
                          size = .8) +
               labs(x= 'Weekday', y= 'Number of Customer Visits',
                    title= paste0("Do Customers like to visit ", input$locationId1,
                                  " on weekday or weekends?")) +
               facet_grid(~ factor(weekday, levels = lvl))+
               theme_bw() +
               theme(axis.text.x = element_text(size=3.5))
    )
  })
  
  ############## Shiny Server: monthly revenue ####################
  
  output$secondSelection2 <- renderUI({
    selectInput(inputId= 'locationId2',
                label= 'Select Business ID:',
                choices= sort(unique(all_monthly$locationId[all_monthly$locationType==input$locationType2])),
                selected = 445)
  })
  
  output$lineplot2 <- renderPlotly({
    ggplotly(all_monthly %>%
               filter(locationId==input$locationId2) %>%
               ggplot(aes(x= `Month Year`, y= Revenue)) +
               labs(x= 'Month Year', y = 'Revenue $',
                    title= paste0("Is Business Looking Good for ", input$locationId2, "?")) +
               geom_line() +
               theme_bw()
    )
    
    
  })
  
  ######################### packed bar ################################################
  
  output$packedbar <- renderPackedBar({
    
    all_mon <- all_monthly %>% 
      filter(`Month Year` == input$month)
    
    p2 <- plotly_packed_bar(input_data = all_mon, 
                            label_column = 'locationId',
                            value_column = 'Revenue',
                            number_rows = 20,
                            plot_title = paste0("Business' Monthly Revenue in ",
                                                input$month),  
                            xaxis_label = 'Monthly Revenue',
                            hover_label = 'Monthly Revenue',
                            min_label_width = 0.02,
                            color_bar_color = '#20b2aa',
                            label_color = 'white') 
    
    p2 <- p2 %>%
      layout(
        xaxis= list(
          ticktext = list('0', '$10K', '$20K', '$30K', '$40K', '$50K')
        )
      )
    p2
  })
  
  ########################## Q2 ########################## 
  NumberOfParicipants<-Participants%>%
    tally()
  
  output$HeatMapGroup<-renderPlotly({
    
    ParticipantsExpenseCategory<-EducationExpenseCategory
    
    row.names(ParticipantsExpenseCategory) <- ParticipantsExpenseCategory$participantId
    ParticipantsExpenseCategory <-
      select(ParticipantsExpenseCategory,input$category)
    ParticipantsExpenseCategory_matrix <- data.matrix(ParticipantsExpenseCategory)
    heatmaply(normalize(ParticipantsExpenseCategory_matrix),
              Colv=NA,
              seriate = "none",
              colors = Blues,
              k_row = 3,
              margins = c(NA,200,60,NA),
              fontsize_row = 4,
              fontsize_col = 5,
              main="Participants and their financial status \nDataTransformation using Normalise Method",
              xlab = "Categories",
              ylab = "ParticipantId"
    )
    
    
    
  })
  
  #### tmap #### 
  
  
  
  output$FinLocation<-renderPlot({
    z<-ParticipantSavings%>%
      st_as_sf(wkt="currentLocation")
    
    
    tm_shape(buildings)+
      tm_polygons(col = "white",
                  border.col = "grey",
                  border.lwd = 1) +
      
      tm_shape(z)+
      
      tm_dots(col="blue",
              alpha=0.5,
              border.col="black",
              border.lwd=1,
              border.alpha=1,
              size="TotalEarning",
              palette="Set1")+
      tm_layout(main.title = "Where is the work?*",
                
                main.title.size = 2,
                legend.height = 0.3,
                legend.width = 0.3,
                legend.outside = FALSE,
                legend.position = c("right", "top"),
                frame = FALSE)+
      tm_compass()+
      tm_credits("*Observed from Mon-Fri at 8 am-8 pm",
                 position=c("left", "bottom"))
  })
  
  
  #### valueBox #### 
  output$value1 <- renderValueBox({
    valueBox(
      value=div(NumberOfParicipants$n,style="font-size:16px;")
      #paste(NumberOfParicipants$n, format="d")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Participants</b>')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      value=div(paste("Mar 22-May 23"),style="font-size:16px;")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Period</b>')
      ,icon = icon("calendar")
    )  
  })
  output$value3 <- renderValueBox({
    valueBox(
      value=div(paste("9"),style="font-size:16px;")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Groups</b>')
      ,icon = icon("user",lib='glyphicon')
      ,color = "yellow")   
  })
  
  
  
  
  
  ### Trellis Display ###
  output$ExpensesTrellis <- renderPlotly({
    
    
    ggplotly((ggplot(FinHealth%>%
                       filter(category==input$category)%>%
                       group_by(Year,Month)%>%
                       mutate(percent=round(TotalAmount*100/sum(TotalAmount),2))%>%
                       ungroup(),
                     aes(x=factor(Month,
                                  levels=c("Mar 22","Apr 22","May 22","Jun 22","Jul 22","Aug 22","Sep 22",
                                           "Oct 22","Nov 22","Dec 22","Jan 23","Feb 23","Mar 23","Apr 23",
                                           "May 23")),
                         y=TotalAmount*-1,
                         fill=category,
                         shape=category,
                         text=paste("Total Amount: ", round(TotalAmount*-1,2),"<br>Category: ",category)))+
                geom_point()+
                scale_fill_brewer(palette="Set2")+
                xlab("Month")+
                ylab("Expenditure")+
                ggtitle("How have been monthly expenses been?")+
                coord_flip()+
                theme_bw()),
             tooltip = c("text"))
    
  })
  
  
  ### Time series ###
  output$ExpensesEachMonth <- renderPlot({
    
    if(input$yaxis=="haveKids"){
      ggplot(StatusLogDetails%>%
               filter(Month %in% input$Months)%>%
               filter(Weekday %in% input$Week)%>%
               filter(category %in% input$categorySelected)) +
        geom_density_ridges_gradient(aes(y = haveKids, 
                                         x = TotalAmount,
                                         fill=stat(x),
                                         height=..density..),
                                     scale = 1,
                                     stat="density",
                                     rel_min_height = 0.01,
                                     bandwidth=80)+
        scale_fill_viridis_c(name = "Amount", option = "C")+
        xlab("Amount")+
        ylab("Kids")+
        facet_grid(~Month)+
        ggtitle("Expenses during the On vs Off season")+
        labs(caption="Source: https://r-graph-gallery.com/ridgeline-plot.html")+
        theme(axis.title.y=element_text(angle=0))
      
    }
    
    else if(input$yaxis=="householdSize"){
      ggplot(StatusLogDetails%>%
               filter(Month %in% input$Months)%>%
               filter(Weekday %in% input$Week)%>%
               filter(category %in% input$categorySelected)) +
        geom_density_ridges_gradient(aes(y = householdSize, 
                                         x = TotalAmount,
                                         fill=stat(x),
                                         height=..density..),
                                     scale = 1,
                                     stat="density",
                                     rel_min_height = 0.01,
                                     bandwidth=80)+
        scale_fill_viridis_c(name = "Amount", option = "C")+
        xlab("Amount")+
        ylab("Household Size")+
        facet_grid(~Month)+
        ggtitle("Expenses during the On vs Off season")+
        labs(caption="Source: https://r-graph-gallery.com/ridgeline-plot.html")+
        theme(axis.title.y=element_text(angle=0))
      
    }
    
    else if(input$yaxis=="educationLevel"){
      ggplot(StatusLogDetails%>%
               filter(Month %in% input$Months)%>%
               filter(Weekday %in% input$Week)%>%
               filter(category %in% input$categorySelected)) +
        geom_density_ridges_gradient(aes(y = educationLevel, 
                                         x = TotalAmount,
                                         fill=stat(x),
                                         height=..density..),
                                     scale = 1,
                                     stat="density",
                                     rel_min_height = 0.01,
                                     bandwidth=80)+
        scale_fill_viridis_c(name = "Amount", option = "C")+
        xlab("Amount")+
        ylab("Education Level")+
        facet_grid(~Month)+
        ggtitle("Expenses during the On vs Off season")+
        labs(caption="Source: https://r-graph-gallery.com/ridgeline-plot.html")+
        theme(axis.title.y=element_text(angle=0))
      
    }
    
  })
  
  
  output$GroupsDashboard <- renderReactable({
    
    
    
    ParticipantSavings%>%
      mutate(interestGroup_colours=case_when(
        interestGroup=="A" ~"#F5A24B",
        interestGroup=="B" ~"#AF52D5",
        interestGroup=="C" ~"#4C9B9B",
        interestGroup=="D" ~"#C0DFA1",
        interestGroup=="E" ~"#9FC490",
        interestGroup=="F" ~"#82A3A1",
        interestGroup=="G" ~"#465362",
        interestGroup=="H" ~"#011936",
        interestGroup=="I" ~"#012957",
      ))%>%
      select(participantId,interestGroup,interestGroup_colours,TotalEarning,TotalExpense,joviality)%>%
      reactable(
        
        columns = list(
          participantId = colDef(maxWidth = 120),
          interestGroup=colDef(
            cell = color_tiles(
              data = .,
              color_ref = 'interestGroup_colours'
            )
            
          ),
          `TotalEarning` = colDef(
            cell = color_tiles(ParticipantSavings,
                               colors=viridis::mako(5),
                               number_fmt = scales::comma_format(accuracy = 0.1))
          ),
          `TotalExpense` = colDef(
            cell = color_tiles(ParticipantSavings,
                               colors=viridis::mako(5),
                               number_fmt = scales::comma_format(accuracy = 0.1))
          ),
          `joviality` =  colDef(
            cell = data_bars(
              data =ParticipantSavings,
              fill_color = viridis::mako(5),
              background = '#F1F1F1',
              fill_opacity = 0.8,
              round_edges = TRUE,
              text_position = 'outside-end',
              number_fmt = scales::comma_format(accuracy = 0.001)
            )
          )
        )
      )%>% 
      add_title(
        title = 'Are we financially fit?', 
        
        align = 'center',
        font_color = '#000000'
      )
    
  })
  
  
  
  output$EarningReactableDashboard <- renderReactable({
    
    if(input$HaveKidsDashboard!="All"){
      ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
        filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
        filter(haveKids==input$HaveKidsDashboard)%>%
        filter(age>= input$age[1] & age<=input$age[2])%>%
        filter(educationLevel %in% input$EducationDashboard)%>%
        select(participantId,Earning,Expense,joviality)
    }
    else{
      ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
        filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
        filter(age>= input$age[1] & age<=input$age[2])%>%
        filter(educationLevel %in% input$EducationDashboard)%>%
        select(participantId,Earning,Expense,joviality)
    }
    
    
    
    
    
    
    
    
    reactable(
      ParticipantMonthlySparkData,
      columns = list(
        participantId = colDef(maxWidth = 120),
        `Earning` = colDef(
          cell = react_sparkline(ParticipantMonthlySparkData,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        ),
        `Expense` = colDef(
          cell = react_sparkline(ParticipantMonthlySparkData,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        ),
        `joviality` =  colDef(
          cell = data_bars(
            data =ParticipantMonthlySparkData,
            fill_color = viridis::mako(5),
            background = '#F1F1F1',
            fill_opacity = 0.8,
            round_edges = TRUE,
            text_position = 'outside-end',
            number_fmt = scales::comma_format(accuracy = 0.001)
          )
        )
      )
    )%>% 
      add_title(
        title = 'Are we financially fit?', 
        
        align = 'center',
        font_color = '#000000'
      )
    
  })
  
  output$WagesExpenseDashboard <- renderReactable({
    
    
    
    
    
    
    
    ExpenseProportionMonthlyData<-ExpenseProportionMonthly%>%
      filter(Month %in% input$Months)%>%
      select(participantId,PropEducation,PropFood,PropShelter,PropRecreation)
    
    
    reactable(
      ExpenseProportionMonthlyData,
      columns = list(
        participantId = colDef(maxWidth = 120),
        `PropEducation` = colDef(
          name = 'Education (%)',
          cell = bubble_grid(
            data = ExpenseProportionMonthlyData,
            colors = '#edf8e9',
            min_value=0,
            max_value=10,
            number_fmt = scales::number_format(accuracy = 0.01)
          )
        ),
        `PropFood` = colDef(
          name = 'Food (%)',
          cell = bubble_grid(
            data = ExpenseProportionMonthlyData,
            colors = '#56A3A6',
            min_value=0,
            max_value=50,
            number_fmt = scales::number_format(accuracy = 0.01)
          )
          
        ),
        `PropShelter` = colDef(
          name = 'Shelter (%)',
          cell = bubble_grid(
            data = ExpenseProportionMonthlyData,
            colors = '#E3B505',
            min_value=0,
            max_value=200,
            number_fmt = scales::number_format(accuracy = 0.01)
          )
          
        ),
        `PropRecreation` = colDef(
          name = 'Recreation (%)',
          cell = bubble_grid(
            data = ExpenseProportionMonthlyData,
            colors = '#f2f0f7',
            min_value=0,
            max_value=100,
            number_fmt = scales::number_format(accuracy = 0.01)
          )
          
        )
      )
    )%>% 
      add_title(
        title = 'Are we financially fit?', 
        
        align = 'center',
        font_color = '#000000'
      )
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  output$InterestGroups<- renderPlot({
    
    
    InterestGroupGraph%>%
      filter(interestGroup %in% input$InterestGroup) %>% 
      ggplot() +
      geom_horizon(aes(x = date, y=log(Expense*-1)), origin = "midpoint", horizonscale = 6)+
      facet_grid(interestGroup~.)+
      theme_few() +
      scale_fill_hcl(palette = 'RdBu') +
      theme(panel.spacing.y=unit(0, "lines"), strip.text.y = element_text(
        size = 5, angle = 0, hjust = 0),
        legend.position = 'none',
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()
      ) +
      scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b%y") +
      ggtitle('Expenses among Interest Groups')
    
    
    
    
    
    
    
    
  })
  
  
  
  output$HeatMap<- renderPlot({
    
    ggplot(StatusLogDetails%>%
             filter(category %in% input$categorySelected)%>%
             filter(Weekday %in% input$Week)%>%
             group_by(participantId,Weekday,category)%>%
             summarise(Expense=sum(TotalAmount)),
           aes(x=Weekday,
               category,
               fill = Expense)) +
      geom_tile(aes(text=paste("Total Time: ",Expense)),color = "white",
                size = 0.1,lwd = 1.5,linetype = 1) +
      coord_equal() +
      scale_fill_gradient2(low = "#075AFF",
                           mid = "#FFFFCC",
                           high = "#FF0000")+
      labs(x = NULL,
           y = NULL,
           title = "How are weekly trends?")+
      theme_ipsum()+
      guides(fill = guide_colourbar(barwidth = 0.5,
                                    barheight = 5))+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 10,angle=90),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6))
    
    
  })
  
  output$LorenzCurve<-renderPlotly({
    
    lorenz<-ggplot(ParticipantSavings%>%
                     select(participantId,
                            input$Lorenz)%>%
                     pivot_longer(-1)) +
      stat_lorenz(aes(value,color=name),
                  show.legend = FALSE)+
      coord_fixed()+
      theme_minimal()+
      theme(legend.title= element_blank())+
      ggtitle("Inequality amongst participants")+
      geom_abline(linetype = "dashed")+
      xlab("Cummulative Percentage of Participants")+
      ylab("Cummulative Percentage of Amount")+
      scale_color_manual(values=c('darkgreen','blue'))+
      labs(caption="Source: https://www.investopedia.com/terms/l/lorenz-curve.asp")
    
    
    
    ggplotly(lorenz)
  })
  
  
  
  ########################## Q3 ########################## 
  
  
  output$placesworked <- renderPlotly({
    p <- ggplot(data= morePlacesPayChange, 
                aes(x=numberofplacesworked)) +
      geom_bar(fill = "#669933") +
      labs(y= 'No. of participants',title="Multiple Workplaces", x='No. of workplaces') +
      # scale_x_discrete(breaks=c("2","3"),
      #                  labels=c("2","3"))+
      theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
            axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
            axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
            
      )
    ggplotly(p)
  })
  
  output$paychange <- renderPlotly({
    d <- event_data("plotly_click")
    if (is.null(d)) return(NULL)
    p <- morePlacesPayChange %>% 
      filter(numberofplacesworked %in% d$x) %>%
      ggplot(aes(x=payStatus)) +
      geom_bar(fill="#FFCC66")  +
      labs(y= 'No. of participants',title="Did Wage increase or decrease ?", x='') +
      theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
            axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
            axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
            
            
      )
    ggplotly(p) %>%
      layout(xaxis = list(title = d$x))
  })
  # output$info <- renderPrint({
  #   event_data("plotly_click")
  #})   
  
  
  
  output$rainPlot <- renderPlotly({
    input$goButton
    
    p <- ggplot(jobs %>% filter(educationRequirement == input$edu) ,
                aes(x = educationRequirement, y = hourlyRate, fill=educationRequirement)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      #ggtitle(label = "Wage Distribution for Different Education Level",
      #subtitle = "High Wages For Higher Educated")+
      theme_minimal()+
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),
            plot.subtitle = element_text(size=12,hjust = 0.5,color='mediumvioletred'))+
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey')) +
      labs(title = isolate({
        toTitleCase(input$plot_title)
      }))
    
    ggplotly(p)
  })
  
  output$rainPlotTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(jobs %>% 
                      filter(educationRequirement == input$edu) %>%
                      mutate(hourlyRate = round(hourlyRate,0)) %>%
                      dplyr::select(jobId, employerId, hourlyRate, educationRequirement),
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
    
  })  
  
  
  output$mapPlot <- renderTmap({
    
    
    if(input$emp == "left") {
      #tmap_mode("plot")
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "black",
                    border.lwd = 1)+
        tm_shape(prevEmp_sf) +
        tm_compass() +
        tm_bubbles(col = "red",
                   n=3,
                   size = "no.ofempLeft")+
        tm_view(set.zoom.limits = c(13,18))
    } 
    else {
      #tmap_mode("plot")
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "black",
                    border.lwd = 1)+
        tm_shape(recntEmp_sf) +
        tm_compass() +
        tm_bubbles(col = "green",
                   size = "no.ofempShifted")+
        tm_view(set.zoom.limits = c(13,18))
    }
    
  })
  
  output$readMap <- renderText(
    "Size of the bubble is proportional to no. of employees left / joined ")
  
  output$aTable <- DT::renderDataTable({
    if(input$emp == "left"){
      if(input$showData){
        
        DT::datatable(prevEmp_sf %>%
                        dplyr::select(employerId,no.ofempLeft),
                      options= list(pageLength = 10),
                      rownames = FALSE)
      }
    }
    else if(input$emp == "joined"){
      if(input$showData){
        DT::datatable(recntEmp_sf %>%
                        dplyr::select(employerId,no.ofempShifted),
                      options= list(pageLength = 10),
                      rownames = FALSE)
      }
    }
  })  
  
  ############# Turnover analysis before and after route map ###################
  
  
  output$befRoute <- renderPlot({
    
    
    logs_path_PrevJob <- logs_path_PrevJob %>%
      filter(participantId == input$participants)
    
    
    tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "grey",
                  border.lwd = 1) +
      tm_shape(logs_path_PrevJob) +
      tm_lines(col = "red") +
      tm_bubbles(col = "blue",
                 shape= marker_icon(),
                 border.col = "black",
                 border.lwd = 2) +
      tm_compass(position = c("right", "top"),
                 type = "4star",
                 show.labels = 2) +
      tm_layout(main.title = "Previous Job Route",
                main.title.position = "center",
                main.title.size = 1,
                legend.show = FALSE)
  })
  
  output$aftRoute <- renderPlot({
    
    logs_path_RecJob <- logs_path_RecJob %>%
      filter(participantId == input$participants)
    
    
    tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "grey",
                  border.lwd = 1) +
      tm_shape(logs_path_RecJob) +
      tm_lines(col = "red") +
      tm_bubbles(col = "blue",
                 shape= marker_icon(),
                 border.col = "black",
                 border.lwd = 2) +
      tm_compass(position = c("right", "top"),
                 type = "4star",
                 show.labels = 2) +
      tm_layout(main.title = "Latest Job Route",
                main.title.position = "center",
                main.title.size = 1,
                legend.show = TRUE)
    
    
  })
  
  
  output$treemapPlot <- renderPlot ({
    treemap(no.ofjobs %>% 
              filter(no.ofjobs >= input$no.ofemp[1] &
                       no.ofjobs <= input$no.ofemp[2]),
            index = c('label', 'employerId'),
            vSize = 'totalWage',
            vColor = 'Average Wage',
            palette = input$color,
            type = 'value',
            title = 'Which employers provide many jobs ?')
    
    rootname = 'Employee Hourly Wage by Workplace'
  })
  
  output$readChart <- renderText("Size of the block - Total Wage\n Colour of the block - Average Wage\n
                                  Read chart as - Employer whose Id is 4734 has 4 jobs and they provide highest wage among other employers who also have 4 jobs")
  
  
  output$treemapTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(no.ofjobs_table %>% 
                      filter(no.ofjobs >= input$no.ofemp[1] &
                               no.ofjobs <= input$no.ofemp[2]),
                    caption = "Is the high wage due to educational qualification ?",
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
    
  })  
  
  
  
  output$barPayPlot <- renderPlot({
    
    if(input$groupbyCategory == "educationLevel"){
      
      #switchEmployeesAllDetails$participantId <- as.character(switchEmployeesAllDetails$participantId)
      ggplot(switchEmployeesAllDetails,
             aes(x=educationLevel, y=payDiff))+
        geom_bar(stat ="identity",aes(fill = payStatus))+
        scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
        labs(y= 'Pay\n Difference',title="Change in Wage by Education Level", x='Education Level') +
        theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
              axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
              axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
        )
    }
    
    else if(input$groupbyCategory == "householdSize"){
      ggplot(switchEmployeesAllDetails,
             aes(x=householdSize, y=payDiff))+
        geom_bar(stat="identity", aes(fill = payStatus))+
        scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
        labs(y= 'Pay\n Difference',title="Wage Difference by Household size", x='Household Size') +
        theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
              axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
              axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
        )
      
      
    }
    else if(input$groupbyCategory == "haveKids"){
      ggplot(switchEmployeesAllDetails,
             aes(x=haveKids, y=payDiff))+
        geom_bar(stat="identity", aes(fill = payStatus))+
        scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
        labs(y= 'Pay\n Difference',title="Wage Difference by Kids", x='') +
        theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
              axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
              axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
        )
      
    }
    else if(input$groupbyCategory == "interestGroup"){
      ggplot(switchEmployeesAllDetails,
             aes(x=interestGroup, y=payDiff))+
        geom_bar(stat="identity", aes(fill = payStatus))+
        scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
        labs(y= 'Pay\n Difference',title="Employee Wage Difference by Interest Group", x='Interest Group') +
        theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
              axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
              axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
        )
      
      
    }
    else if(input$groupbyCategory == "ageGroup"){
      ggplot(switchEmployeesAllDetails,
             aes(x=ageGroup, y=payDiff))+
        geom_bar(stat="identity", aes(fill = payStatus))+
        scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
        labs(y= 'Pay\n Difference',title="Wage Difference among Age Groups", x='Age Group') +
        theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
              axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
              axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
        )
      
      
    }
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)


