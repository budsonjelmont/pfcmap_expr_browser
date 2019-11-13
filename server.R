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
source('./plots.R')

basecols = c('MSSM_060','MSSM_072',
             'MSSM_073','MSSM_104',
             'MSSM_182','MSSM_401',
             'Pitt_034','Pitt_046',
             'Pitt_122','Pitt_140')

precapcols = c('PreCap_MSSM_060','PreCap_MSSM_072',
               'PreCap_MSSM_073','PreCap_MSSM_104',
               'PreCap_MSSM_182','PreCap_MSSM_401',
               'PreCap_Pitt_034','PreCap_Pitt_046',
               'PreCap_Pitt_122','PreCap_Pitt_140')

postcapcols = c('PostCap_MSSM_060','PostCap_MSSM_072',
                'PostCap_MSSM_073','PostCap_MSSM_104',
                'PostCap_MSSM_182','PostCap_MSSM_401',
                'PostCap_Pitt_034','PostCap_Pitt_046',
                'PostCap_Pitt_122','PostCap_Pitt_140')

# Calculate total expression (sum) across all isoforms
exprdf$PreCap_exprsum = apply(exprdf[precapcols],1,sum)
exprdf$PostCap_exprsum = apply(exprdf[precapcols],1,sum)

# Calculate usage ratio of each isoform
precapratios = lapply(
  precapcols,
  function(coly){
    exprdf[coly]/exprdf$PreCap_exprsum
  }     
)

# Calculate usage ratio of each isoform
postcapratios = lapply(
  postcapcols,
  function(coly){
    exprdf[coly]/exprdf$PostCap_exprsum
  }     
)

# Convert to data frame and assign names
precapratios = as.data.frame(precapratios)
postcapratios = as.data.frame(postcapratios)
colnames(precapratios) = paste(precapcols,'ratio',sep='_')
colnames(postcapratios) = paste(postcapcols,'ratio',sep='_')

# Add ratio columns to data frame & then reassign row names
exprdf = merge(exprdf, precapratios, by='row.names', sort = TRUE) # This step drops the rownames from the resulting dataframe & adds a column 'Row.names'
exprdf = merge(exprdf, postcapratios, by.x='Row.names', by.y='row.names', sort = TRUE)
rownames(exprdf) = exprdf$Row.names

#exprdf = exprdf[,datcols]

#pbids = c('PBfusion.988.2','PBfusion.989.2')
#df = melt(exprdf[pbids,datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot

# Define server logic required to build the violin plot
shinyServer(function(input, output) {
    output$exprPlot <- renderPlot({
      query=input$genequery
      if(query==''){return()}
      cap=input$capbtn
      scale=input$logbtn
      if(cap=='Capture'){
        datcols = precapcols
      } else {
        datcols = postcapcols
      }
      datcols = c('Pbid','gene_name',datcols)
      # datcols = c('Pbid','gene_name',
      #   'PreCap_MSSM_060','PreCap_MSSM_072',
      #   'PreCap_MSSM_073','PreCap_MSSM_104',
      #   'PreCap_MSSM_182','PreCap_MSSM_401',
      #   'PreCap_Pitt_034','PreCap_Pitt_046',
      #   'PreCap_Pitt_122','PreCap_Pitt_140')
      df=melt(exprdf[which(exprdf$gene_name==query),datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      if(scale=='Log2'){
        doAbundancePlot_log(df,query,range(df$expression))
      } else {
        doAbundancePlot_linear(df,query,range(df$expression))
      }
    })
    output$ratioPlot <- renderPlot({
      query = input$genequery
     if(query==''){return()}
      cap = input$capbtn
      scale = input$logbtn
      if(cap=='Capture'){
        datcols = precapcols
      } else {
        datcols = postcapcols
      }
      datcols = c('Pbid','gene_name',datcols)
      # datcols = c('Pbid','gene_name',
      #  'PreCap_MSSM_060_ratio','PreCap_MSSM_072_ratio',
      #  'PreCap_MSSM_073_ratio','PreCap_MSSM_104_ratio',
      #  'PreCap_MSSM_182_ratio','PreCap_MSSM_401_ratio',
      #  'PreCap_Pitt_034_ratio','PreCap_Pitt_046_ratio',
      #  'PreCap_Pitt_122_ratio','PreCap_Pitt_140_ratio')
      
      df=melt(exprdf[which(exprdf$gene_name==query),datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      if(scale=='Log2'){
        doUsagePlot_log(df,query,range(df$expression))
      } else {
        doUsagePlot_linear(df,query,range(df$expression))
      }
    })
})
