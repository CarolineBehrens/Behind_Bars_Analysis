#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# library(tidycensus)
# library(shinyWidgets)
library(gtsummary)
library(shinythemes)
library(gt)
library(broom.mixed)
#library(wesanderson)
# library(ggplot2)
library(viridis)
#library(hrbrthemes)
# library(dplyr)

library(readr)
source("make_plots.R")

ui <- navbarPage(
    "Behind Bars",
    tabPanel("Overview",
             fluidPage(theme = shinytheme("darkly"),
                 titlePanel("Crime and Mortality Trends from 2001-2016"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c(state_crime$name)
                         )),
                     mainPanel(plotOutput("state_crime"),
                               plotOutput("mortality_plot"))
             ))),
    tabPanel("Model",
             titlePanel("Model Discussion"),
             h3("Preliminary Thoughts"),
             p("From my initial data exploration I was curoious about the 
               relationship between northern and southern states and how 
               the amount of prisoners each state has varies. I figured that 
               a good indicator of this would be the population of the state in
               general because more citizens creats more opportunity for crime.
               I also know that the relationship between violent crime 
               and prisoner count has to be linear because those who 
               commit violent crimes will most likely be put in prison." ),
             
             h3("Mathematical Model"),
             withMathJax(
             '$$ PrisonerCountInThousands_i = \\beta_0 + 
                      \\beta_1 PopulationBins_i + \\beta_2 South_i + 
                      \\beta_3 ViolentCrimeInThousands_i + 
                      \\beta_4 South*ViolentCrimeInThousands_i + 
                     \\epsilon_i $$'),
             h3("Table"),
             gt_output("model_prisoner_count"),
    
    h3("Analysis"),
    p(""),
    plotOutput("model_plot")),
    
    
    tabPanel("Deeper Dive into Crime Type",
             titlePanel("Crime Trends Per State"), 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "state",
                   "State",
                   c(crime_w_bins$jurisdiction)
                 ),
                 selectInput(
                   "crime",
                   "Crime",
                   c("Robbery","Vehicle Theft", "Murder Manslaughter",
                     "Rape", "Aggrevated Assault","Burglary", "Larceny"
                     ))
                 ),
               mainPanel(plotOutput("crime_per_state"))),
             p("")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background Motivations"),
             p("I have always been intrigued about the criminal justice 
               system, but have never before gotten to look at statistics 
               pertaining to crimes and prisoners across each state. I have 
               done research before related to programs in place at prisons, 
               but have never explored the types of people and crimes they are
               convicted of."),
      
             h3("About Me"),
             p("My name is Caroline Behrens and I study Economics. 
             You can reach me at Cbehrens@college.harvard.edu."),
             p(tags$a(href ="https://github.com/CarolineBehrens/Prison_Final_Project")))) 
             
                         

#Define server logic required to draw a histogram

server <- function(input, output){
 output$state_crime <- renderPlot({ 
  state_crime %>%
   filter(name == input$plot_type) %>%
    ggplot(aes(x = year, y = total/1000, color = type)) +
   geom_point(size = 3)
 
})
 
 
 output$mortality_plot <- renderPlot(mortality_plot)
 output$model_prisoner_count <- render_gt(model_prisoner_count)
 output$model_plot <- renderPlot(model_plot)
 
 #case when function is allowing for multiple dropdowns for 
 #second interactive plot
 output$crime_per_state <- renderPlot({
   case_when(
     input$crime == "Robbery" ~ list(crime_w_bins %>%
         filter(jurisdiction == input$state) %>%
          ggplot(aes(y = robbery, x = year)) + 
          geom_point(color = 'purple') + 
            geom_smooth()),
     input$crime == "Vehicle Theft" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = vehicle_theft, x = year)) + 
                                       geom_point(color = 'blue') + 
                                          geom_smooth()),
     input$crime == "Murder Manslaughter" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = murder_manslaughter, x = year)) + 
                                       geom_point(color = 'red') +
                                          geom_smooth()),
     input$crime == "Rape" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = rape_legacy, x = year)) + 
                                       geom_point(color = 'green') +
                                     geom_smooth()),
     input$crime == "Aggrevated Assault" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = agg_assault, x = year)) + 
                                       geom_point(color = 'blue') +
                                          geom_smooth()),
     input$crime == "Burglary" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = burglary, x = year)) + 
                                       geom_point(color = 'red') +
                                         geom_smooth()),
     input$crime == "Larceny" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = larceny, x = year)) + 
                                       geom_point(color = 'green') +
                                        geom_smooth())
  
   )
   
   
   
 
 })
} 

  

# Run the application 
shinyApp(ui = ui, server = server)



