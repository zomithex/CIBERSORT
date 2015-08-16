# extract the LM22 genes from mixtureMatrix(merge ExampleMixtures-GEPs and LM22) and then normalize the LM22(sigMat) and the extracted mixData
library(som)

sigMat <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/Final/102_normalized/LM22-mostVar102.csv")
mixDataMain <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/CIBERSORT-ExampleDATA/ExampleMixtures-GEPs.csv")

mixDataShort <- merge(mixDataMain, sigMat,   by = "GeneSymbol")
write.csv(mixDataShort, "mixDaraShort.csv")
mixData <- mixDataShort[1:6]
write.csv(mixData, "mixDaraShortValues.csv")
#write.csv(sigMat, "sigMatFinal.csv")


sigMatNorm <- as.data.frame(normalize(data.matrix(sigMat[,2:23]), byrow = F))
write.csv(sigMatNorm, "sigMatNormalized.csv")

meanSigMatNorm <-mean(data.matrix(sigMatNorm))
sdSigMatNorm <- sd(data.matrix(sigMatNorm))
dataSigMat <- cbind(meanSigMatNorm, sdSigMatNorm)
write.csv(dataSigMat, "sigMat_dataNormalized.csv")

mixDataNorm <- as.data.frame(normalize(data.matrix(mixData[, 2:6]), byrow = F))
write.csv(mixDataNorm, "mixDataNormalized.csv")

meanmixDataNorm <-mean(data.matrix(mixDataNorm))
sdmixDataNorm <- sd(data.matrix(mixDataNorm))
datamixDataNorm <- cbind(meanmixDataNorm, sdmixDataNorm)
write.csv(datamixDataNorm, "mixData_dataNormalized.csv")