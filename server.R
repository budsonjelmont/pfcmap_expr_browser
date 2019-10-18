#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)
library(reshape2)
source('./vplot.R')

datcols=c('Pbid','associated_gene','PreCap_MSSM_060','PreCap_MSSM_072',
  'PreCap_MSSM_073','PreCap_MSSM_104',
  'PreCap_MSSM_182','PreCap_MSSM_401',
  'PreCap_Pitt_034','PreCap_Pitt_046',
  'PreCap_Pitt_122','PreCap_Pitt_140')

exprdf = exprdf[,datcols]

pbids = c("PBfusion.988.2","PBfusion.989.2")
df = melt(exprdf[pbids,datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot

# Define server logic required to build the violin plot
shinyServer(function(input, output) {
   
  output$vPlot <- renderPlot({
    query=input$genequery
    df=melt(exprdf[which(exprdf$associated_gene==query),datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    doViolin(df,query)
  })
  
})
