# extract the signature matrix probes and their values from mixture Matrix (example: LM22, merge ExampleMixtures-GEPs ), to be used for modelling
library(som)

sigMat <- read.csv("allcellSortedData.csv")
mixDataMain <- read.csv("mixedDataSorted.csv")

#check if the column name is the same in both tables. Here "GeneSymbol". It was originally different in LM22 and #Mixture table and has been corrected.
mixDataShort <- merge(mixDataMain, sigMat,   by = "GeneSymbol")
write.csv(mixDataShort, "mixDataAlignedSigMat.csv")
#mixData <- mixDataShort[1:6]
mixData <- mixDataShort[1:25]
write.csv(mixData, "mixDataAligned.csv")
sigMat <- mixDataShort[26:32]
write.csv(sigMat, "cellSortedAligned.csv")