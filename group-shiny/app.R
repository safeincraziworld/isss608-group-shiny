########################## Packages in use ########################## 

# packages <- c('shiny', 'shinydashboard', 'shinythemes', 
#               'tidyverse', 'ggplot2', 'plotly')

packages=c('ggiraph', 'plotly', 'rmarkdown','psych','sf','tmap',
           'DT', 'patchwork','gglorenz','hrbrthemes','shinydashboard',
           'gganimate', 'tidyverse','ggthemes','reactable',
           'readxl', 'gifski', 'gapminder','quantmod','shinythemes',
           'treemap', 'treemapify','ggridges','zoo','reactablefmtr','crosstalk',
           'rPackedBar','lubridate','remotes','ggplot2','dplyr','ggstatsplot',
           'lubridate','shiny','tools','writexl')

for (p in packages){
  library(p, character.only=T)
}

########################## Reading the files ########################## 

#### Q1 ####
#all_wday <- readRDS('data/all_wday.rds')
all <- readRDS('data/all.rds')
restaurants <- readRDS('data/restaurants.rds')
pubs <- readRDS('data/pubs.rds')

lvl <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')


#### Q2 ####
Participants<-read_csv("data/Participants.csv",show_col_types = FALSE)
ParticipantsApartmentLocation<-read_csv("data/ParticipantsApartmentLocation.csv",show_col_types = FALSE)
buildings<-read_sf("data/buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

ParticipantSavings<-readRDS("data/Q2/ParticipantSavings.rds")
FinHealth<-readRDS("data/Q2/FinHealth.rds")
ParticipantMonthlySavings<-readRDS("data/Q2/ParticipantMonthlySavings.rds")
ParticipantMonthlySpark<-readRDS("data/Q2/ParticipantMonthlySpark.rds")

InterestGroupGraph<-readRDS("data/Q2/InterestGroupGraph.rds")
StatusLogDetails<-readRDS("data/Q2/StatusLogDetails.rds")


#### Q3 ####
employers <- readRDS("data/Q3/employers.rds")
empWorkinMultiplePlaces <- readRDS("data/Q3/empWorkinMultiplePlaces.rds")
empWorkinMultiplePlaces_latest <- readRDS("data/Q3/empWorkinMultiplePlaces_latest.rds")
empWorkinMultiplePlaces_latest_groupby <- readRDS("data/Q3/empWorkinMultiplePlaces_latest_groupby.rds")
empWorkinMultiplePlaces_previous <- readRDS("data/Q3/empWorkinMultiplePlaces_previous.rds")
empWorkinMultiplePlaces_previous_groupby <- readRDS("data/Q3/empWorkinMultiplePlaces_previous_groupby.rds")
jobs <- readRDS("data/Q3/jobs.rds")
logs_path_PrevJob <- readRDS("data/Q3/logs_path_PrevJob.rds")
logs_path_RecJob <- readRDS("data/Q3/logs_path_RecJob.rds")
no.ofjobs<- readRDS("data/Q3/no.ofjobs.rds")
no.ofjobs_table <- readRDS("data/Q3/no.ofjobs_table.rds")
participants <- readRDS("data/Q3/participants.rds")
pay_hires <- readRDS("data/Q3/pay_hires.rds")
prevEmp_sf <- readRDS("data/Q3/prevEmp_sf.rds")
recntEmp_sf <- readRDS("data/Q3/recntEmp_sf.rds")
switchEmployeesAllDetails <- readRDS("data/Q3/switchEmployeesAllDetails.rds")
transitionEmpDetails <- readRDS("data/Q3/transitionEmpDetails.rds")
transitionTable <- readRDS('data/Q3/transitionTable.rds')
work <- readRDS("data/Q3/work.rds")
work_home <- readRDS("data/Q3/work_home.rds")
work_home_filt <- readRDS("data/Q3/work_home_filt.rds")
workinmoreplaces <- readRDS("data/Q3/workinmoreplaces.rds")

########################## Q2 ########################## 

# ######## Data Cleaning
# 
# PartMonthYear=FinancialJournal%>%
#   mutate(Year=as.numeric(year(timestamp)),
#          Month=as.character(timestamp,"%b %y"),
#          MonthNumeric=as.numeric(month(timestamp)))%>%
#   group_by(participantId,Year,Month,MonthNumeric,category)%>%
#   summarise(TotalAmount=sum(amount))
# 
# ######## Getting the location of all participants
# 
# #ParticipantLog<-readRDS('data/logs_fread.rds')
# #ParticipantsApartmentLocation<-ParticipantLog%>%
# #  filter(currentMode=="AtHome")%>%
# #  distinct(participantId,currentLocation)
# #write_csv(ParticipantsApartmentLocation,"data/ParticipantsApartmentLocation.csv")
# 
# 
# ######## Data for Time Series
# PartDailyExpense<-FinancialJournal%>%
#   mutate(date=date(timestamp))%>%
#   group_by(participantId,date,category)%>%
#   summarise(TotalAmount=sum(amount))
# 
# PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
#                                    y=Participants,
#                                    by=c("participantId"="participantId")
# )
# 
# 
# 
# 
# ParticipantsFinancialJournal <- inner_join(x= PartMonthYear,
#                                            y= Participants, 
#                                            by= 'participantId')
# 
# 
# ParticipantsFinancialJournalExpense=ParticipantsFinancialJournal%>%
#   filter(category!='Wage')%>%
#   group_by(participantId,Year,Month)%>%
#   summarise(Expense=sum(TotalAmount)*-1)
# 
# ParticipantsFinancialJournalEarnings=ParticipantsFinancialJournal%>%
#   filter(category=='Wage')%>%
#   group_by(participantId,Year,Month)%>%
#   summarise(Earn=sum(TotalAmount))
# 
# ParticipantsEarningsVsExpense <- left_join(
#   x= ParticipantsFinancialJournalExpense, 
#   y= ParticipantsFinancialJournalEarnings, 
#   by= c('participantId'='participantId',
#         'Year'='Year',
#         'Month'='Month'))
# 
# 
# FinHealth=ParticipantsFinancialJournal%>%
#   group_by(Year,Month,category)%>%
#   summarise(TotalAmount=sum(TotalAmount))
# 
# 
# 
# Expenditure=FinHealth%>%
#   filter(category!='Wage' & category!='RentAdjustment')%>%
#   group_by(Year,Month)%>%
#   summarise(Expense=sum(TotalAmount)*-1)
# 
# Earnings=FinHealth%>%
#   filter(category=='Wage')%>%
#   group_by(Year,Month)%>%
#   summarise(Earn=sum(TotalAmount))
# 
# 
# EarningsVsExpense <- inner_join(
#   x= Expenditure, 
#   y= Earnings, 
#   by= c('Year'='Year','Month'='Month'))
# 
# 
# ParticipantMonthlySavings<-left_join(
#   x=ParticipantsEarningsVsExpense,
#   y=Participants,
#   by='participantId')%>%
#   mutate(Savings=Earn-Expense)
# 
# 
# ParticipantSavings<-
#   left_join(x=ParticipantMonthlySavings%>%
#               group_by(participantId)%>%
#               summarise(TotalSavings=sum(Savings),
#                         TotalEarning=sum(Earn),
#                         TotalExpense=sum(Expense)),
#             y=Participants,
#             by='participantId')%>%
#   left_join(.,ParticipantsApartmentLocation,
#             by='participantId')
# 
# # Data for Heat Map
# 
# #StatusLogDetails<-PartDetailsDailyExpense%>%
# #  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))%>%
# #  filter(category=='Food' | category=='Recreation')
# 
# #Data for candlestick
# #DailyCurrentModeTime<-PartDetailsDailyExpense%>%
# #  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))
# 
# 
# # Open=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   filter(day(date)==max(day(date)))%>%
# #   group_by(Month,category)%>%
# #   summarise(OpenTimeSpent=mean(TotalAmount))
# # 
# # Close=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   filter(day(date)==min(day(date)))%>%
# #   group_by(Month,category)%>%
# #   summarise(CloseTimeSpent=mean(TotalAmount))
# # 
# # High=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   summarise(HighTimespent=max(TotalAmount))
# # 
# # Low=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   summarise(LowTimespent=min(TotalAmount))
# # 
# # 
# # CandlestickData=left_join(High, Low, by= c('Month'='Month',
# #                                            'category'='category')) %>%
# #   left_join(., Open, by=c(
# #     'Month'='Month',
# #     'category'='category'))%>% 
# #   left_join(., Close, by=c(
# #     'Month'='Month',
# #     'category'='category'))
# 
# ### Data for Sparklines
# 
# ParticipantMonthlySpark<-ParticipantMonthlySavings%>%
#   group_by(participantId)%>%
#   summarise(Expense=list(Expense),
#             Earning=list(Earn))%>%
#   left_join(.,Participants,
#             by=c("participantId"="participantId"))
# 
# #ParticipantMonthlyEarningSpark<-ParticipantMonthlySavings%>%
# #  group_by(participantId)%>%
# #  summarise(Earning=list(Earn))
# 
# 
# 
# ######## Plots 
# 
# 
# ### Time series ###
# 
# PartDailyExpense<-FinancialJournal%>%
#   mutate(date=date(timestamp))%>%
#   group_by(participantId,date,category)%>%
#   summarise(TotalAmount=sum(amount))%>%
#   filter(category!="Wage")%>%
#   group_by(participantId,date)%>%
#   summarise(Expense=sum(TotalAmount))
# 
# PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
#                                    y=Participants,
#                                    by=c("participantId"="participantId"))
# 
# 
# S<-PartDetailsDailyExpense%>%
#   group_by(date,interestGroup)%>%
#   summarise(Expense=sum(Expense))
# 
# 
# InterestGroupGraph<-PartDetailsDailyExpense%>%
#   group_by(date,interestGroup)%>%
#   summarise(Expense=sum(Expense))


### Coordinated Plot ###

# PShighlighted <- highlight_key(ParticipantSavings%>%select(-TotalSavings))
# Er <- ggplot(data=PShighlighted, 
#              aes(x = TotalEarning,
#                  y = joviality,
#                  color=as.character(householdSize),
#                  text=paste("Earning: ",round(TotalEarning,2),
#                             "<br>Joviality: ",round(joviality,2),
#                             "<br>Household Size: ",householdSize))) +
#   geom_point(size=1)+
#   xlab("Earning")+
#   ylab("Joviality")
# 
# Ex <- ggplot(data=PShighlighted, 
#              aes(x = TotalExpense,
#                  y = joviality,
#                  color=as.character(householdSize),
#                  text=paste("Expense: ",round(TotalExpense,2),
#                             "<br>Joviality: ",round(joviality,2),
#                             "<br>Household Size: ",householdSize))) +
#   geom_point(size=1)+
#   ggtitle("Can money buy happiness?")+
#   theme(legend.position="none")

#FB<-highlight(subplot(ggplotly(Er,tooltip = c("text")),ggplotly(Ex,tooltip = c("text"))),"plotly_selected")
#crosstalk::bscols(FB,DT::datatable(z,options = list(
#  columnDefs = list(list(className = 'dt-center', targets = 5)),
#  pageLength = 10,
#  autoWidth = TRUE,
#  scrollX = T,
#  lengthMenu = c(5, 10, 15, 20))),
#  widths = c(12,12))




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
  navbarMenu("Q2",
             
             tabPanel("Q2.1",
                      
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
                        column(12,reactableOutput("EarningReactableDashboard", 
                                                  width = "auto", 
                                                  height = "auto", 
                                                  inline = FALSE))
                      )),
             tabPanel("Q2.2",
                      
                      
                      
                      
                      
                      
                      
                      
                      fluidRow(
                        column(3,checkboxGroupInput("Lorenz", "Variables to show:",
                                                    c("Earning" = "TotalEarning",
                                                      "Expense" = "TotalExpense"),
                                                    selected = "TotalEarning")),
                        column(9,plotlyOutput("LorenzCurve"))
                        
                      ),
                      fluidRow(
                        column(3,selectInput(inputId = "yaxis", 
                                             label =   "Select the comparison",
                                             choices =  c("Kids" = "haveKids",
                                                          "HouseholdSize" = "householdSize",
                                                          "Education Level" = "educationLevel"),
                                             selected = "haveKids"
                        )),
                        column(3,selectInput(inputId = "categorySelected", 
                                             label =   "Select the Category",
                                             c("Education" = "Education",
                                               "Food" = "Food",
                                               "Recreation" = "Recreation",
                                               "Shelter" = "Shelter"),
                                             multiple=TRUE,
                                             selected = "Food")),
                        column(3,selectInput(inputId = "Week", 
                                             label =   "Select the Week",
                                             c("Monday"="Monday","Tuesday"="Tuesday",
                                               "Wednesday"="Wednesday","Thursday"="Thursday",
                                               "Friday"="Friday","Saturday"="Saturday","Sunday"="Sunday"),
                                             multiple=TRUE,
                                             selected = c("Monday","Tuesday",
                                                          "Wednesday","Thursday",
                                                          "Friday","Saturday","Sunday"))),
                        
                        column(3,selectInput(inputId = "Months", 
                                             label =   "Select the Month",
                                             c("Nov 2022" = "Nov 2022",
                                               "Dec 2022" = "Dec 2022",
                                               "Jan 2023" = "Jan 2023",
                                               "Feb 2023" = "Feb 2023"),
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
                        column(12,reactableOutput("WagesExpenseDashboard", 
                                                  width = "auto", 
                                                  height = "auto", 
                                                  inline = FALSE))
                      ),
                      fluidRow(
                        column(6,plotOutput("ExpensesEachMonth")),
                        column(6,plotOutput("HeatMap"))
                      ),
                      
                      
                      # fluidRow(
                      #   column(12,plotOutput("FinLocation")),
                      
                      
                      
                      
                      
                      
                      #mainPanel(
                      #  uiOutput("CoordinatedPlot"),
                      #  width = "100%", height = "400px"
                      #),
                      
                      
                      
                      
             ),
             
             tabPanel("Q2.3",
                      fluidRow(column(12,reactableOutput("GroupsDashboard", 
                                                         width = "auto", 
                                                         height = "auto", 
                                                         inline = FALSE))),
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
                                             "H"="H"),
                                           multiple = TRUE,
                                           selected = c("A" = "A",
                                                        "B" = "B",
                                                        "C" = "C",
                                                        "D" = "D",
                                                        "E"="E",
                                                        "F"="F",
                                                        "G"="G",
                                                        "H"="H"))),
                        
                        
                        column(9,plotOutput("InterestGroups")))
                      
                      
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
                                             plotOutput(outputId = "befRoute",
                                                        width = 500,
                                                        height = 500),
                                         ),
                                         box("Commute route from home to work after job change  (Marker- New Employer Location)",
                                             plotOutput(outputId  = "aftRoute",
                                                        width = 500,
                                                        height = 500)
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
                                                   plotOutput("barPayPlot",
                                                                width = "900px"))
                                                                #height = "500px"))
                                       ),
                                       
                                       DT::dataTableOutput(outputId = "barPayPlotTable")
                                       
                                       
                              ),
                              tabPanel("Change of Workplaces",
                                       titlePanel("Please click on the bar to see the pay comparison"),
                                       mainPanel(
                                         fluidRow(
                                           column(6,
                                                  plotlyOutput(
                                                    outputId="placesworked", 
                                                    width="500px",
                                                    height="400px")),  
                                           column(6,
                                                  plotlyOutput(
                                                    outputId="paychange", 
                                                    width="500px",
                                                    height="400px"))
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
                                                   plotlyOutput("rainPlot",
                                                              height = "400px"))
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
                                       box(plotOutput("treemapPlot")),
                                       DT::dataTableOutput(outputId = "treemapTable")
                                       
                                       
                                       
                              ),
                              tabPanel("Emp location",
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
                                       box(tmapOutput("mapPlot")),
                                       DT::dataTableOutput(outputId = "aTable")
                                       
                              ),
                              
                            )
                          )
                        )
                        
                      )))     
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
  
  
  ########################## Q2 ########################## 
  NumberOfParicipants<-Participants%>%
    tally()
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
      value=div(paste("8"),style="font-size:16px;")
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
    
    # if(input$HaveKidsDashboard!="All"){
    #   ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
    #     filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
    #     filter(haveKids==input$HaveKidsDashboard)%>%
    #     filter(age>= input$age[1] & age<=input$age[2])%>%
    #     filter(educationLevel %in% input$EducationDashboard)%>%
    #     select(participantId,Earning,Expense,joviality)
    # }
    # else{
    #   ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
    #     filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
    #     filter(age>= input$age[1] & age<=input$age[2])%>%
    #     filter(educationLevel %in% input$EducationDashboard)%>%
    #     select(participantId,Earning,Expense,joviality)
    # }
    
    
    
    
    #ParticipantMonthlySparkShared<-SharedData$new(ParticipantMonthlySparkData)
    
    
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
    
    
    
    
    #ParticipantMonthlySparkShared<-SharedData$new(ParticipantMonthlySparkData)
    
    
    
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
    
    
    
    
    
    #ParticipantMonthlySparkShared<-SharedData$new(ParticipantMonthlySparkData)
    
    StatusLogDetailsExpenseData<-StatusLogDetails%>%
      filter(Month %in% input$Months)%>%
      filter(Weekday %in% input$Week)%>%
      filter(category %in% input$categorySelected)%>%
      group_by(participantId)%>%
      summarise(Expense=sum(TotalAmount)*-1)
    
    StatusLogDetailsEarningData<-StatusLogDetails%>%
      filter(Month %in% input$Months)%>%
      filter(Weekday %in% input$Week)%>%
      filter(category =="Wage")%>%
      group_by(participantId)%>%
      summarise(Earn=sum(TotalAmount)*-1)
    StatusLogDetailseData<-left_join(y=StatusLogDetailsExpenseData,
                                     x=StatusLogDetailsEarningData,
                                     by=c("participantId"="participantId"))
    reactable(
      StatusLogDetailseData,
      columns = list(
        participantId = colDef(maxWidth = 120),
        `Earn` = colDef(
          name = 'Wage',
          minWidth = 150,
          align = 'right',
          
          cell = data_bars(
            data = StatusLogDetailseData,
            text_position = 'outside-end',
            fill_color = viridis::mako(5),
            number_fmt = scales::number_format(accuracy = 0.01)
          )
        ),
        `Expense` = colDef(
          name = 'Expense',
          minWidth = 150,
          align = 'left',
          cell = data_bars(
            data = StatusLogDetailseData,
            text_position = 'outside-end',
            fill_color = viridis::mako(5),
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
  
  
  
  
  
  
  
  #output$CoordinatedPlot <- renderUI({
  
  #crosstalk::bscols(FB,DT::datatable(z,options = list(
  #columnDefs = list(list(className = 'dt-center', targets = 5)),
  #pageLength = 10,
  #autoWidth = TRUE,
  #scrollX = T,
  #lengthMenu = c(5, 10, 15, 20))),
  #widths = c(12,12))
  #})
  
  
  
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
    
    
    
    # ggplot(InterestGroupGraph%>%
    #          filter(interestGroup %in% input$InterestGroup))+
    #   geom_line(aes(x=date,y=log(Expense*-1),color=interestGroup))
    
    
    
    
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
           title = "Is it all work and no play?")+
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
    
    #scale_color_manual(labels = c("Earnings", "Savings","Expense"))+
    
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
  

