#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   # Application title
#   tags$script(src = 'init_lookups.js'),
#   
#   
#   titlePanel(
#     tags$div(class = 'container text-center',
#       tags$h1('PFC Map expression browser'),
#     ),
#   windowTitle = 'browsertitle'),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       textInput('genequery', h3('Gene symbol:'), 
#                 value = '') 
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput('exprPlot'),
#        plotOutput('ratioPlot')
#     )
#   )
# ))

shinyUI(
  fluidPage(
# Application title
#   tags$script(src = 'init_lookups.js'),
#   titlePanel(
#     tags$div(class = 'container text-center',
#       tags$h1('PFC Map expression browser'),
#     ),
    fluidRow(
      tags$head(
        # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
      ),
      column(12,
        flowLayout(textInput('genequery', h5('Gene symbol:'),value = ''),
                     # radioButtons('capbtn', '', choices = c('No capture','Capture'), selected = 'Capture',
                     #    inline = T, width = NULL, choiceNames = NULL,
                     #    choiceValues = NULL),
                     # actionGroupButtons(c('captoggle','nocaptoggle'), c('Capture','No capture'), status = "default",
                     #    size = "normal", direction = "horizontal", fullwidth = FALSE)
                   awesomeRadio('capbtn', '', c('Capture','No capture'), selected = 'Capture', inline = FALSE,
                                status = "primary", checkbox = FALSE, width = NULL),
                   awesomeRadio('logbtn', '', c('Log2','Linear'), selected = 'Log2', inline = FALSE,
                                status = "primary", checkbox = FALSE, width = NULL)
                     )
      )
    ),
    fluidRow(
      column(6,
             plotOutput('exprPlot')
      ),
      column(6,
             plotOutput('ratioPlot')
      )
    )
  )
)