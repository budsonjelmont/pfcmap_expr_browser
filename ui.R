#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  tags$script(src = 'init_lookups.js'),
  titlePanel(
    tags$div(class = "container text-center",
      tags$h1("PFC Map expression browser"),
    ),
  windowTitle = 'browsertitle'),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("genequery", h3("Gene symbol:"), 
                value = "") 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("vPlot")
    )
  )
))
