###############################################################
# GPL and DAVID: Getting a list of differentially expressed
# genes (rather than probes) for DAVID
###############################################################

################################################################
# The majority of this script is taken from GEO-and-limma.R,
# to identify differentially expressed genes between 
# males and females
################################################################

library(dplyr)
library(affy)
library(GEOquery)


###########################################################
# Get the processed data for GSE1297. The object
# returned by getGEO is a LIST of AffyBatch objects
# getGEO returns a list because each GEO Series
# may contain multiple platforms
###########################################################

# Normally use GEOquery, but in class we can use workspace
# library(GEOquery)
GSE1297 <- getGEO("GSE1297")

# extract clinical information and gender
GSE1297.p <- pData(GSE1297[[1]])
gender <- GSE1297.p$characteristics_ch1.6

# extract gene expression data; for this datset, we must take log2
GSE1297.expr <- exprs(GSE1297[[1]])
GSE1297.expr <- log2(GSE1297.expr)
boxplot(GSE1297.expr, main = "log2 processed data")


################################################################
# Find differentially expressed (DE) probes between males
# and females. 
###############################################################

library(limma)

# construct design matrix
design <- model.matrix(~0+gender)

# let's change the column names
colnames(design) <- c("Female", "Male")

## limma package fits a linear model to each row of the expression matrix ##
fit <- lmFit(GSE1297.expr, design)

## Specify the contrasts, which must match column names of design matrix ##
contrast.matrix <- makeContrasts(Male - Female,levels=design)

## fit model based on contrasts (e.g., Female - Male)
fit2 <- contrasts.fit(fit, contrast.matrix)

# calculate moderate t-statistics by moderating standard errors
# toward a common value, which makes answers more robust
fit2 <- eBayes(fit2)

## find all probes differentially expressed at FDR of 5%
tt <- topTable(fit2,p.value = 0.05, sort.by = "p", number = nrow(GSE1297.expr))
tt


###############################################################
# Create a heatmap using the top probes (FDR < 0.05)
###############################################################
m <- match(rownames(tt), rownames(GSE1297.expr))
X <- GSE1297.expr[m,]
col.heat <- colorRampPalette(c("yellow", "blue"))(200)

# set colors for gender #
col.gender <- as.integer(as.factor(gender))
col.gender <- c("pink", "blue")[col.gender]

# generate heatmap
heatmap(X, ColSideColors = col.gender, col = col.heat)



###############################################################
# Let's find the genes associated with all probes
# This requires using microarray annotations available from
# GEO (see below) or bioconductor (not discussed) 
###############################################################
platform <- annotation(GSE1297[[1]])   
platform

# download the platform data 
pl <- getGEO(platform)
pl <- Table(pl)

##########################################################
# Next, find the gene for the desired probe 
# The column names may vary by platform, but for GPL96,
# the probes are in the ID column and the genes are in 
# the "Gene Symbol" column
##########################################################
probes <- rownames(tt)
m <- match(probes, pl$ID)
genes <- pl$`Gene Symbol`[m]
genes

## only keep probes that correspond to genes ##
keep <- genes!=""
genes <- genes[keep]


# on this platform, probes corresponding to multiple gene names are separated
# by " /// ". We can extract all genes using strsplit, which returns a 
# list
genes <- strsplit(genes, " /// ")

# using unlist, we can get a vector of all genes
genes <- unlist(genes)

# get a unique set of genes 
genes <- unique(genes) 


## view/save genes for input into DAVID (https://david.ncifcrf.gov/)
## you may save the genes to a file by setting the file argument,
## if a path is not specified, the file will be saved in your
## current working directory - see getwd() - by default
write.table(genes, row.names = FALSE, quote = FALSE)

########################################################
# We will use DAVID (https://david.ncifcrf.gov/) for
# functional analysis of gene lists.

# Select Functional Annotation -> Upload gene list, 
# set identifier to OFFICIAL_GENE_SYMBOL, and select Gene List.
# If necessary, set the List species and Background species 
# appropiately (in this case to Homo sapiens)

# Select the following to view the biological processes
#   and pathways

# 1) Gene_Ontology --> GOTERM_BP_DIRECT (click on Chart)
# 2) Pathways --> KEGG_PATHWAY (click on Chart)


###########################################################################
# Additional example: the genes below are part of a Cell Cycle 
# Progression (CCP) signature. What Biological terms and pathways are 
# associated with these genes, based on DAVID?
###########################################################################
genes <- c("ASF1B", "ASPM", "AURKA", "BIRC5", "BUB1B", "C18orf24", 
           "CDC2", "CDC20", "CDCA3", "CDCA8", "CDKN3", "CENPF", "CENPM",
           "CEP55", "DLGAP5", "DTL", "FOXM1", "KIAA0101", "KIF20A", 
           "MCM10", "NUSAP1", "ORC6L", "PBK", "PRC1", "PTTG1", "RAD51",
           "RAD54L", "RRM2", "TK1", "TOP2A")


write.table(genes, row.names = FALSE, quote = FALSE)