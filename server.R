library(shiny)

library(ggplot2)
library(reshape2)
source('./plots.R')

idcols = c('Pbid','gene_name')

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

# Define server logic to make the plots
shinyServer(function(input, output) {
    # Construct list of data columns to get
    # datcols = reactive({
    #   if(cap=='Capture'){
    #     datcols = postcapcols
    #   } else {
    #     datcols = precapcols
    #   }
    # })
    # Reshape data for input to ggplot
    df = reactive({
      query=input$genequery
      if(query==''){return()}
      cap=input$capbtn
      if(cap=='Capture'){
        datcols = postcapcols
      } else {
        datcols = precapcols
      }
      # Get ratio columns as well
      #datcols=datcols + unlist(lapply(datcols, function(x){paste(x,'ratio',sep='_')}))
      datcols=c(idcols,datcols)
      df=melt(exprdf[which(exprdf$gene_name==query),datcols], value.name = 'expression', variable.name='sampleName') #gives you the input to ggplot
      df=df[df$expression!=0,]
    })
    # Make expression plot
    output$exprPlot = renderPlot({
      if(input$genequery==''){return()}
      df=df()
      scale=input$scalebtn
      if(scale=='Log2'){
        doAbundancePlot_log(df,range(df$expression))
      } else {
        doAbundancePlot_linear(df,range(df$expression))
      }
    })
    # Make isoform usage plot
    output$ratioPlot = renderPlot({
      if(input$genequery==''){return()}
      df=df()
      scale=input$scalebtn
      if(scale=='Log2'){
        doUsagePlot_log(df,range(df$expression))
      } else {
        doUsagePlot_linear(df,range(df$expression))
      }
    })
})
