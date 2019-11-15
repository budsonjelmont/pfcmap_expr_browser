# Preprocess the expression data frame to add usage ratios

infile = 'FL-RH_combined_joined-isoform-TPM-counts_median_average_YH_GeneIDsAdded.csv'
outfile = 'FL-RH_combined_joined-isoform-TPM-counts_median_average_YH_GeneIDsAdded+ratios.csv'

# Read dataset
exprdf = read.csv(infile)
rownames(exprdf) = exprdf$Pbid

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

# Make list of data frames (one per gene) where each contains column (individual) sum of all transcripts' expression 
isosums = sapply(as.character(unique(exprdf$associated_gene)),function(gene){
  apply(exprdf[which(exprdf$associated_gene==gene),c(precapcols,postcapcols)],2,sum)
}, simplify=FALSE, USE.NAMES = TRUE)
# Convert to data frame
sumdf = do.call(rbind, isosums)

# Calculate usage ratios
l = apply(exprdf,1,function(row){
  sapply(c(precapcols,postcapcols),function(colname,r){
    return(as.numeric(r[[colname]])/sumdf[r['associated_gene'],colname])
  },row, simplify=FALSE, USE.NAMES=TRUE)  
}
)

ratiodf = do.call(rbind,l)

# Assign unique column names to ratio data frame
colnames(ratiodf) = paste(colnames(ratiodf),'ratio',sep='_')

# Add ratio columns to data frame & then reassign row names & drop redundant columns
exprdf = merge(exprdf, ratiodf, by='row.names', sort = TRUE) # This step drops the rownames from the resulting dataframe & adds a column 'Row.names'
rownames(exprdf) = exprdf$Row.names
exprdf = exprdf[,!(names(exprdf) %in% c('Row.names','X.isoform'))]

# Clean data & write out
exprdf = data.frame(lapply(exprdf, as.character), stringsAsFactors=FALSE)
write.table(exprdf, outfile, sep=',', quote=FALSE, row.names = FALSE, col.names = TRUE)
