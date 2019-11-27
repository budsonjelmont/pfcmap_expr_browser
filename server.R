library(shiny)

library(ggplot2)
library(data.table)
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

# Read dataset
infile = 'FL-RH_combined_joined-isoform-TPM-counts_median_average_YH_GeneIDsAdded+ratios.csv'
exprdf = fread(infile)

# Preprocessing the expression data frame to add usage ratios
rownames(exprdf) = exprdf$Pbid

# Define server logic to make the plots
shinyServer(function(input, output, session) {
    updateSelectizeInput(session, 'genequery', choices=c('',exprdf$gene_name), selected=character(0), server=TRUE)
    #updateSelectizeInput(session, 'genequery', choices=cbind(name=c('',exprdf$gene_name),c('',exprdf$id)), options=list(render= I(
    #  'function(item, escape){return "<div>" + escape(item.name) + "</div>";}')
    #  ),
    #  selected=character(0),
    #  server=TRUE
    #)
    # On gene query change, get the corresponding transcript rows
    genedf = reactive({
      query=input$genequery
      print('query:')
      print(query)
      if(query==''){return()}
      genedf=exprdf[which(exprdf$gene_name==query),]
    })
    # On pre/post-capture change, select the relevant columns & return 2 dataframes (one for abundance, another for ratios)
    df=reactive({
      query=input$genequery
      if(query==''){return()}
      genedf=genedf()
      cap=input$capbtn
      if(cap=='Capture'){
        datcols = postcapcols
      } else {
        datcols = precapcols
      }
      # Make abundance df
      abund_df=melt(genedf[,c(idcols,datcols), with=FALSE], id.vars='Pbid', measure.vars=datcols, value.name='expression') #Melt data for input to ggplot
      # Make ratio df
      ratiocols=unlist(lapply(datcols, function(x){paste(x,'ratio',sep='_')}))
      ratio_df=melt(genedf[,c(idcols,ratiocols), with=FALSE], id.vars='Pbid', measure.vars=ratiocols, value.name='expression') #Melt data for input to ggplot
      list(abundance=abund_df, isouse=ratio_df)
    })
    # Make expression plot
    output$exprPlot = renderPlot({
      query=input$genequery
      if(query==''){return()}
      df=df()
      abund_df = df[['abundance']]
      abund_df = abund_df[abund_df$expression!=0,]
      scale=input$scalebtn
      if(scale=='Log2'){
        doAbundancePlot_log(abund_df,range(abund_df$expression))
      } else {
        doAbundancePlot_linear(abund_df,range(abund_df$expression))
      }
    })
    # Make isoform usage plot
    output$ratioPlot = renderPlot({
      query=input$genequery
      if(query==''){return()}
      df=df()
      isouse_df = df[['isouse']]
      isouse_df = isouse_df[isouse_df$expression!=0,]
      isouse_df = isouse_df[!is.na(isouse_df$expression),]
      scale=input$scalebtn
      if(scale=='Log2'){
        doUsagePlot_log(isouse_df,range(isouse_df$expression))
      } else {
        doUsagePlot_linear(isouse_df,range(isouse_df$expression))
      }
    })
})
