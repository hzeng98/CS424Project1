#include necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(stringr)
library(readr)

#read csv 
halsted <- read.csv('halsted.csv')
roosevelt <- read.csv('roosevelt.csv')
ohare <- read.csv('airport.csv')
#print(halsted)


date_h <-halsted$date
date_r <-roosevelt$date
date_o <-ohare$date
#print(year_h)


#set the weekday for each dataset
halsted$weekday <- wday(mdy(halsted$date),label = TRUE, abbr = TRUE)
roosevelt$weekday <- wday(mdy(roosevelt$date),label = TRUE, abbr = TRUE)
ohare$weekday <- wday(mdy(ohare$date),label = TRUE, abbr = TRUE)
#print(halsted$weekday)


#set the year for each dataset
halsted$year <- year(mdy(halsted$date))
roosevelt$year <- year(mdy(roosevelt$date))
ohare$year <- year(mdy(ohare$date))
#print(halsted$year)


#set the month for each dataset
halsted$month <- month(mdy(halsted$date),label = TRUE, abbr = TRUE)
roosevelt$month <- month(mdy(roosevelt$date),label = TRUE, abbr = TRUE)
ohare$month <- month(mdy(ohare$date),label = TRUE, abbr = TRUE)
#print(halsted$month)


years <-c(2001:2021)
station <- c("UIC-Halsted" = "UIC-Halsted",
             "O'Hare Airport" = "O'Hare Airport",
             "Roosevelt"="Roosevelt"
             )
month <-c("Jan"="Jan",
          "Feb"="Feb",
          "Mar"="Mar",
          "Apr"="Apr",
          "May"="May",
          "Jun"="Jun",
          "Jul"="Jul",
          "Aug"="Aug",
          "Sep"="Sep",
          "Oct"="Oct",
          "Nov"="Nov",
          "Dec"="Dec"
          )

weekday <-c("Mon"="Mon",
            "Tue"="Tue",
            "Wed"="Wed",
            "Thu"="Thu",
            "Fri"="Fri",
            "Sat"="Sat",
            "Sun"="Sun"
            )



ui <- dashboardPage(
  dashboardHeader(title = "CS424 Project1"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   #menu bar with 1 panel and about page
                   sidebarMenu(
                     id="tabs",
                     menuItem("Subway Data", tabName = "Subway_Data", icon = NULL),
                     menuItem("Select years",  icon = icon("file-text-o"),
                              menuSubItem("2001", tabName = "y1", icon = icon("angle-right")),
                              menuSubItem("2002", tabName = "y2", icon = icon("angle-right")),
                              menuSubItem("2003", tabName = "y3", icon = icon("angle-right")),
                              menuSubItem("2004", tabName = "y4", icon = icon("angle-right")),
                              menuSubItem("2005", tabName = "y5", icon = icon("angle-right")),
                              menuSubItem("2006", tabName = "y6", icon = icon("angle-right")),
                              menuSubItem("2007", tabName = "y7", icon = icon("angle-right")),
                              menuSubItem("2008", tabName = "y8", icon = icon("angle-right")),
                              menuSubItem("2009", tabName = "y9", icon = icon("angle-right")),
                              menuSubItem("2010", tabName = "y10", icon = icon("angle-right")),
                              menuSubItem("2011", tabName = "y11", icon = icon("angle-right")),
                              menuSubItem("2012", tabName = "y12", icon = icon("angle-right")),
                              menuSubItem("2013", tabName = "y13", icon = icon("angle-right")),
                              menuSubItem("2014", tabName = "y14", icon = icon("angle-right")),
                              menuSubItem("2015", tabName = "y15", icon = icon("angle-right")),
                              menuSubItem("2016", tabName = "y16", icon = icon("angle-right")),
                              menuSubItem("2017", tabName = "y17", icon = icon("angle-right")),
                              menuSubItem("2018", tabName = "y18", icon = icon("angle-right")),
                              menuSubItem("2019", tabName = "y19", icon = icon("angle-right")),
                              menuSubItem("2020", tabName = "y20", icon = icon("angle-right")),
                              menuSubItem("2021", tabName = "y21", icon = icon("angle-right"))
                     ),
                     menuItem("About Page", tabName = "About", icon = NULL)
                   )
  ),
  
#==========    ==========    ==========    dashboard    ==========    ==========    ==========
  dashboardBody(
    tabItems(
      tabItem(tabName="Subway Data",
            fluidRow(
                # first three bar charts
                column(6,
                    # select years
                     fluidRow(
                       column(4,
                              selectInput("year1", "Year1", years, selected = 2019),
                              selectInput("station1", "Station1", station, selected = "UIC-Halsted")
                       )
                     ),
                     
                    fluidRow(
                      box(
                        title = "Stacked bar chart", solidHeader = TRUE, status = "primary", width = 12, 
                        collapsible = TRUE,
                    
                        h5("The amount of each day of the year"),
                        plotOutput("dy", height = 200),
                        h5("The amount of each month of the year"),
                        plotOutput("my", height = 200),
                        h5("The amount of each weekday of the year"),
                        plotOutput("wdy", height = 200)
                      )  
                    )
                ),
                # second three bar charts
                column(6,
                       fluidRow(
                         column(4,
                                selectInput("year2", "Year2", years, selected = 2019),
                                selectInput("station2", "Station2", station, selected = "O'Hare Airport")
                          )
                       ),
                       fluidRow(
                         box(
                           title = "Stacked bar chart", solidHeader = TRUE, status = "primary", width = 12, 
                           collapsible = TRUE,
                           
                           h5("The amount of each day of the year"),
                           plotOutput("dy2", height = 200),
                           h5("The amount of each month of the year"),
                           plotOutput("my2", height = 200),
                           h5("The amount of each weekday of the year"),
                           plotOutput("wdy2", height = 200)
                         )
                       )
              )
            )
      ),
      tabItem(tabName = "about",
              box( width = NULL, status = "primary", solidHeader = TRUE, title= "About page",
                   mainPanel(
                     h1("Data reference"),
                     h3("https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f "),
                     h1("App developer"),
                     h3("Haoxuan Zeng"),
                     h3("This application is part of my CS424 project 1 at the University of Illinois at Chicago, Spring 2022."),
                     
                   )
              )
              
      )
    )
  ))
    
server <- function(input, output, session) {
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  

  #    Stacked bar chart (dependent to station and year) *need implement
  output$dy <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      h <- subset(halsted, halsted$year=input$year)
      ggplot(h$rides, h$date, xlab="Count", ylab="Date")+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      o <- subset(ohare, ohare$year=input$year)
      ggplot(o$rides, o$date, xlab="Count", ylab="Date")+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      r <- subset(roosevelt, roosevelt$year=input$year)
      ggplot(r$rides, r$date, xlab="Count", ylab="Date")+
        geom_bar()
    }
  })
  
  output$my <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
  })
  
  output$wdy <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
  })
  
  
  
  #    Second Stacked bar chart (dependent to station and year) *need implement
  output$dy2 <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
  })
  
  output$my2 <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
  })
  
  output$wdy2 <- renderPlot({
    if(input$station1 == "UIC-Halsted"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "O'Hare Airport"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
    if(input$station1 == "Roosevelt"){
      print(input$station1)
      ggplot(data=halsted$rides, aes(x=Order.Priority))+
        geom_bar()
    }
  })
  
  
  # 21 years 
  observe({
    if (input$tabs=="y1"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2001)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2001)
    }
    if (input$tabs=="y2"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2002)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2002)
    }
    if (input$tabs=="y3"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2003)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2003)
      
    }
    if (input$tabs=="y4"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2004)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2004)
    }
    if (input$tabs=="y5"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2005)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2005)
    }
    if (input$tabs=="y6"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2006)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2006)
      
    }
    if (input$tabs=="y1"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2001)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2001)
    }
    if (input$tabs=="y2"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2002)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2002)
    }
    if (input$tabs=="y3"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2003)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2003)
      
    }
    if (input$tabs=="y4"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2004)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2004)
    }
    if (input$tabs=="y5"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2005)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2005)
    }
    if (input$tabs=="y6"){
      updateSelectInput(session, "year1", label = "Year2", choices = years,
                        selected = 2006)
      updateSelectInput(session, "year2", label = "Year1", choices = years,
                        selected = 2006)
      
    }
    if (input$tabs=="y7"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2007)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2007)
    }
    if (input$tabs=="y8"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2008)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2008)
    }
    if (input$tabs=="y9"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2009)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2009)
      
    }
    if (input$tabs=="y10"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2010)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2010)
    }
    if (input$tabs=="y11"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2011)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2011)
    }
    if (input$tabs=="y12"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2012)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2012)
      
    }
    if (input$tabs=="y13"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2013)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2013)
    }
    if (input$tabs=="y14"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2014)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2014)
    }
    if (input$tabs=="y15"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2015)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2015)
      
    }
    if (input$tabs=="y16"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2016)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2016)
    }
    if (input$tabs=="y17"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2017)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2017)
    }
    if (input$tabs=="y18"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2018)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2018)
      
    }
    if (input$tabs=="y19"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2019)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2019)
    }
    if (input$tabs=="y20"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2020)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2020)
    }
    if (input$tabs=="y21"){
      updateSelectInput(session, "year1", label = "Year1", choices = years,
                        selected = 2021)
      updateSelectInput(session, "year2", label = "Year2", choices = years,
                        selected = 2021)
      
    }
  }) 
}

shinyApp(ui=ui,server=server)

#ui <-fluidPage("Hello World")
#server <- function(input,output){}
#shinyApp(ui=ui,server=server)
