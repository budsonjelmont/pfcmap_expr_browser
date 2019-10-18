library(ggplot2)
library(gdata)

# Plot params #
plotPath = 'ncRNAexprPlots/'
ggalpha = 0.5
ggshape = 1
Yaxlow = -8
Yaxhigh = 8
Yaxstep = 2

# Read dataset
infile = '~/Documents/lncRNA map/FL-RH_combined_joined-isoform-TPM-counts_median_average_YH_GeneIDsAdded.csv'
exprdf = read.csv(infile)

rownames(exprdf) = exprdf$Pbid

###############
# Func to draw the violin plot & save as PNG
doViolin = function(df,geneName){
  df$expression = as.numeric(as.character(df$expression))
  p=ggplot(aes(y = log2(expression), x = Pbid, fill = Pbid, group = Pbid, color = Pbid), data = df) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(fill='white', color='black', width=0.1, outlier.alpha = ggalpha, outlier.shape = ggshape) +
    geom_point(fill=NA, color = 'black', alpha = ggalpha, shape = ggshape, show.legend = FALSE) +
    #    scale_fill_manual(values=c('darkgreen', 'blue', 'purple', 'orange')) +
    ggtitle(geneName) +
    xlab('Cell line') +
    ylab('log2 expression') +
    scale_y_continuous(limits=c(Yaxlow,Yaxhigh),breaks=seq(Yaxlow,Yaxhigh,by=Yaxstep),labels=seq(Yaxlow,Yaxhigh,by=Yaxstep)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color='black', size=1.25, fill=NA, linetype='solid'),
      axis.text.x = element_text(colour='black', size=12, angle=10, hjust=1),
      axis.text.y = element_text(colour='black', size=20),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin=margin(t=0, r=10.5, b=0, l=0), size=22),
      axis.ticks.length = unit(0.33, "cm"),
      legend.position = 'none'
    ) +
    labs(x = "Dilution")
    return(p)
}
################