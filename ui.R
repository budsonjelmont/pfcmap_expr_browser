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

shinyUI(
  fluidPage(
# Application title
    tags$script(src = 'init_lookups.js'),
#   titlePanel(
#     tags$div(class = 'container text-center',
#       tags$h1('PFC Map expression browser'),
#     ),
    fluidRow(
      tags$head(
        # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
        tags$style(type="text/css", " div<div.awesome-radio-class{ width: 10%;}")
      ),
      # column(12,
      #   flowLayout(
      #     textInput('genequery', h5('Gene symbol'),value = ''),
      #      awesomeRadio('capbtn', h5('Capture probes'), c('Capture','No capture'), selected = 'Capture', inline = FALSE,
      #                   status = "primary", checkbox = FALSE, width = NULL),
      #      awesomeRadio('scalebtn', h5('Scale'), c('Log2','Linear'), selected = 'Log2', inline = FALSE,
      #                   status = "primary", checkbox = FALSE, width = NULL)
      #   )
      # )
      column(2,
         selectizeInput('genequery', h5('Gene symbol'), choices = NULL)
         #textInput('genequery', h5('Gene symbol'),value = '')
      ),
      column(2,
         awesomeRadio('capbtn', h5('Capture probes'), c('Capture','No capture'), selected = 'Capture', inline = FALSE,
            status = "primary", checkbox = FALSE, width = '80%')
      ),
      column(2,
         awesomeRadio('scalebtn', h5('Scale'), c('Log2','Linear'), selected = 'Log2', inline = FALSE,
            status = "primary", checkbox = FALSE, width = NULL)
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