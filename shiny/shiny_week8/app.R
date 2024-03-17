#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(tidyverse)
library(rsconnect)


# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("PSY 8712 Week 8 Project"),
  
  
  sidebarLayout(
    sidebarPanel(
      #creating ui option for gender by using select input so the users cnan select their gender option 
      selectInput("gender",
                  "Select Gender",
                  choices= c("Male","Female","All"),
                  selected= "All"),
      #creating ui option for error band by using select input so the users can select to display or suppress the error band
      selectInput("errorband",
                  "Display or Suppress Error Band",
                  choices = c("Display Error Band", "Suppress Error Band"),
                  selected= "Display Error Band"),
      #creating ui option for date by using select input so the users can select to include or exclude participants from before July 1, 2017
      selectInput("date",
                  "Include or Exclude Participants before July 1, 2017",
                  choices= c("Include Participants before July 1, 2017", "Exclude Participants before July 1, 2017"),
                  selected= "Include Participants before July 1, 2017")
    ),
    
    # Show a plot of the of correlation
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

#Define server logic required to draw a scatterplot based on the input from the ui
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    #import data for scatterplot using rds as recommended in class
    w8 <- readRDS("w8.rds")
    
    
    
    #changing plot to display chosen gender option by using if else statements so that if the first option is not true, the code runs for the seocnd option 
    if(input$gender!="All"){
      data_shiny <- w8 %>%
        filter(gender==input$gender)
    } else if (input$gender== "All") {
      data_shiny <- w8
    }
    #changing plot to include or exclude participants from 7/1/2017 where it only filters out the participants if that is selected 
    if(input$date== "Exclude Participants before July 1, 2017"){
      data_shiny <- w8 %>%
        filter(timeEnd >= ymd("2017-07-01"))
    }
    
    #changing plot to display or suppress error band by using if else so if they choose to supress the error band the second part of the statement runs 
    if(input$errorband == "Display Error Band"){ 
      ggplot(data_shiny, aes(avg_q1q6, avg_q8q10)) + 
        geom_point() +
        geom_smooth(method=lm, color="purple", se=TRUE) +
        labs(x="Mean Scores on Q1-Q6",
             y= "Mean Scores on Q8-Q10",
             title="Figure 1")
    } else if (input$errorband == "Suppress Error Band"){
      ggplot(data_shiny, aes(avg_q1q6, avg_q8q10)) +
        geom_point() +
        geom_smooth(method=lm, color="purple", se=FALSE) +
        labs(x= "Mean Scores on Q1-Q6",
             y= "Mean Scors on Q8-Q10",
             title="Figure 1")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



