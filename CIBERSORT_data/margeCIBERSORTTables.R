sigMat <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/LM22.csv")
mixData <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/ExampleMixtures-GEPs.csv")

head(sigMat$GeneSymbol) #no idea why it needs a dot between Gene and symbol, Rstudio found it out
head(mixData$GeneSymbol)