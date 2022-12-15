#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
shinyUI(fluidPage(
  
  titlePanel(tags$h1("Baby Names (US): 1880 - 2017")),
  
  navbarPage(" ",
             
             tabPanel("Top 10 names each year", icon = icon("chart-column"), 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year", "Select Year", 
                                      choices = unique(most_pop$year)
                          ), 
                          selectInput("sex", "Select Sex", 
                                      choices = unique(most_pop$sex)
                          )
                        ), 
                        mainPanel(
                          plotOutput("top_10_names")
                        )
                      )), 
             
             tabPanel("Most Popular Name by Decade", icon = icon("chart-gantt"), 
                      fluidRow(column = 3,
                               selectInput("decade", "Select decade", 
                                           choices = unique(top$decade)
                               )
                      ), 
                      fluidRow(column = 12,
                               plotOutput("names_decade_m")
                      ),
                      fluidRow(column = 12,
                               plotOutput("names_decade_f")
                      )
                      
             ), 
             tabPanel("Top 10 names over time (Female)", icon = icon("chart-gantt"), 
                      fluidRow(column = 6,
                               selectInput("time_names_f",
                                           "Select Names:",
                                           choices = unique(as.character(time_f$name)), 
                                           multiple = TRUE
                               )
                      ),
                      
                      fluidRow(column = 6, 
                               plotOutput("plot_time_f")
                      )
             ),
             
             tabPanel("Top 10 names over time (Male)", icon = icon("chart-gantt"), 
                      fluidRow(column = 6,
                               selectInput("time_names_m",
                                           "Select Names:",
                                           choices = unique(as.character(time_m$name)), 
                                           multiple = TRUE
                               )
                      ),
                      fluidRow(column = 6, 
                               plotOutput("plot_time_m"))
             ),
             
             tabPanel("Number of Individual Names by Year", icon = icon("chart-line"), 
                      fluidRow(column = 6,
                               selectInput("sex_unique",
                                           "Select Sex:",
                                           choices = unique(as.character(unique$sex)), 
                                           multiple = TRUE
                               )
                      ),
                      fluidRow(column = 6, 
                               plotOutput("unique_plot"))
             ), 
             
             tabPanel("Top names in each state: 1910 - 2017", icon = icon("cloud"), 
                      fluidRow(column = 6,
                               selectInput("state", "Select State", 
                                           choices = unique(top_state$state_name)
                               )
                      ), 
                      fluidRow(column = 12,
                               plotOutput("word_cloud")
                      )
             ),
             
             tabPanel("Top names in each state (map): 1910 - 2017", 
                      icon = icon("earth-americas"), 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("map_year", "Select Year", 
                                      choices = unique(state$year)
                          ), 
                          selectInput("map_sex", "Select Sex", 
                                      choices = unique(state$sex)
                          )
                        ), 
                        mainPanel(
                          plotOutput("map_plot"),
                          tableOutput("map_data")
                        )
                      )           
             ) 
  )
)
)
