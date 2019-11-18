library(ggplot2)
library(gdata)

# Plot params #
plotPath = 'ncRNAexprPlots/'
ggalpha = 0.5
ggshape = 1
#Yaxlow = -8
#Yaxhigh = 8
#Yaxstep = 2

# Read dataset
infile = 'FL-RH_combined_joined-isoform-TPM-counts_median_average_YH_GeneIDsAdded+ratios.csv'
exprdf = read.csv(infile)

# Preprocessing the expression data frame to add usage ratios
rownames(exprdf) = exprdf$Pbid

###############
# 1a. Func to draw the abundance violin plot (log scale)
doAbundancePlot_log = function(df,yrange){
  df$expression = as.numeric(as.character(df$expression))
  Ylow=log2(yrange[1])
  Yhigh=log2(yrange[2])
  Ybuffer=(Yhigh-Ylow)/5
  Yaxlow=round(Ylow - Ybuffer)
  Yaxhigh=round(Yhigh + Ybuffer)
  Yaxstep = round((Yaxhigh-Yaxlow)/8, digits=4)
  p=ggplot(aes(y = log2(expression), x = Pbid, fill = Pbid, group = Pbid, color = Pbid), data = df) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(fill='white', color='black', width=0.1, outlier.alpha = ggalpha, outlier.shape = ggshape) +
    geom_point(fill=NA, color = 'black', alpha = ggalpha, shape = ggshape, show.legend = FALSE) +
    #    scale_fill_manual(values=c('darkgreen', 'blue', 'purple', 'orange')) +
    ggtitle('Abundance') +
    ylab('log2 expression') +
    scale_y_continuous(limits=c(Yaxlow,Yaxhigh),breaks=seq(Yaxlow,Yaxhigh,by=Yaxstep),labels=seq(Yaxlow,Yaxhigh,by=Yaxstep)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color='black', size=1.25, fill=NA, linetype='solid'),
      axis.text.x = element_text(colour='black', size=12, angle=30, hjust=1),
      axis.text.y = element_text(colour='black', size=14),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin=margin(t=0, r=10.5, b=0, l=0), size=16),
      axis.ticks.length = unit(0.25, 'cm'),
      legend.position = 'none'
    ) +
    labs(x = 'Dilution')
    return(p)
}
# 1b. Func to draw the abundance violin plot (linear scale)
doAbundancePlot_linear = function(df,yrange){
  df$expression = as.numeric(as.character(df$expression))
  Ylow=yrange[1]
  Yhigh=yrange[2]
  Ybuffer=(Yhigh-Ylow)/5
  Yaxlow=max(round(Ylow - Ybuffer),0)
  Yaxhigh=round(Yhigh + Ybuffer) 
  Yaxstep = round((Yaxhigh-Yaxlow)/8, digits=4)
  p=ggplot(aes(y = expression, x = Pbid, fill = Pbid, group = Pbid, color = Pbid), data = df) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(fill='white', color='black', width=0.1, outlier.alpha = ggalpha, outlier.shape = ggshape) +
    geom_point(fill=NA, color = 'black', alpha = ggalpha, shape = ggshape, show.legend = FALSE) +
    #    scale_fill_manual(values=c('darkgreen', 'blue', 'purple', 'orange')) +
    ggtitle('Abundance') +
    ylab('Expression') +
    scale_y_continuous(limits=c(Yaxlow,Yaxhigh),breaks=seq(Yaxlow,Yaxhigh,by=Yaxstep),labels=seq(Yaxlow,Yaxhigh,by=Yaxstep)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color='black', size=1.25, fill=NA, linetype='solid'),
      axis.text.x = element_text(colour='black', size=12, angle=30, hjust=1),
      axis.text.y = element_text(colour='black', size=14),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin=margin(t=0, r=10.5, b=0, l=0), size=16),
      axis.ticks.length = unit(0.25, 'cm'),
      legend.position = 'none'
    ) +
    labs(x = 'Dilution')
  return(p)
}
################
###############
# 2a. Func to draw the isoform usage ratio violin plot (log scale)
doUsagePlot_log = function(df,yrange){
  df$expression = as.numeric(as.character(df$expression))
  Ylow=log2(yrange[1])
  Yhigh=log2(yrange[2])
  Ybuffer=(Yhigh-Ylow)/5
  Yaxlow=round(Ylow - Ybuffer)
  Yaxhigh=round(Yhigh + Ybuffer)
  Yaxstep = round((Yaxhigh-Yaxlow)/8, digits=4)
  p=ggplot(aes(y = log2(expression), x = Pbid, fill = Pbid, group = Pbid, color = Pbid), data = df) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(fill='white', color='black', width=0.1, outlier.alpha = ggalpha, outlier.shape = ggshape) +
    geom_point(fill=NA, color = 'black', alpha = ggalpha, shape = ggshape, show.legend = FALSE) +
    #    scale_fill_manual(values=c('darkgreen', 'blue', 'purple', 'orange')) +
    ggtitle('Usage') +
    ylab('') +
    scale_y_continuous(limits=c(Yaxlow,Yaxhigh),breaks=seq(Yaxlow,Yaxhigh,by=Yaxstep),labels=seq(Yaxlow,Yaxhigh,by=Yaxstep)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color='black', size=1.25, fill=NA, linetype='solid'),
      axis.text.x = element_text(colour='black', size=12, angle=30, hjust=1),
      axis.text.y = element_text(colour='black', size=16),
      #axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
      # axis.title.y = element_text(margin=margin(t=0, r=10.5, b=0, l=0), size=22),
      axis.ticks.length = unit(0.25, 'cm'),
      legend.position = 'none'
    ) +
    labs(x = 'Dilution')
  return(p)
}
# 2a. Func to draw the isoform usage ratio violin plot (linear scale)
doUsagePlot_linear = function(df,yrange){
  df$expression = as.numeric(as.character(df$expression))
  Ylow=0
  Yhigh=1
  Ybuffer=(Yhigh-Ylow)/5
  Yaxlow=0
  Yaxhigh=1
  Yaxstep = round((Yaxhigh-Yaxlow)/8, digits=4)
  p=ggplot(aes(y = expression, x = Pbid, fill = Pbid, group = Pbid, color = Pbid), data = df) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(fill='white', color='black', width=0.1, outlier.alpha = ggalpha, outlier.shape = ggshape) +
    geom_point(fill=NA, color = 'black', alpha = ggalpha, shape = ggshape, show.legend = FALSE) +
    #    scale_fill_manual(values=c('darkgreen', 'blue', 'purple', 'orange')) +
    ggtitle('Usage') +
    ylab('') +
    scale_y_continuous(limits=c(Yaxlow,Yaxhigh),breaks=seq(Yaxlow,Yaxhigh,by=Yaxstep),labels=seq(Yaxlow,Yaxhigh,by=Yaxstep)) + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color='black', size=1.25, fill=NA, linetype='solid'),
      axis.text.x = element_text(colour='black', size=12, angle=30, hjust=1),
      axis.text.y = element_text(colour='black', size=16),
      #axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
      # axis.title.y = element_text(margin=margin(t=0, r=10.5, b=0, l=0), size=22),
      axis.ticks.length = unit(0.25, 'cm'),
      legend.position = 'none'
    ) +
    labs(x = 'Dilution')
  return(p)
}
################
