#read the signature matrix file and put each cell type values in a separate file. 
#Then, it calculaes the acerage value (between samples) for each cell type and return each in a separate file.

typesAvgFiles <- function(sourceFile = "/Users/mitra/Desktop/work/cell_sorted_data_sortedByName.csv") {
#produce B, ... , avgB, ... files

  
LM7<- read.csv(sourceFile)


i <- 0

B <- LM7[, c(grep("_B", colnames(LM7)))]
print(head(B))

CD4T <- LM7[, c(grep("_CD4T", colnames(LM7)))]
CD8T <- LM7[, c(grep("_CD8T", colnames(LM7)))]
G <- LM7[, c(grep("_G", colnames(LM7)))]
Mo <- LM7[, c(grep("_Mo", colnames(LM7)))]
NK <- LM7[, c(grep("_NK", colnames(LM7)))]
RBC <- LM7[, c(grep("_RBC", colnames(LM7)))]

write.csv(Mo, file = "Mo.csv")
write.csv(G, file = "G.csv")
write.csv(RBC, file = "RBC.csv")
write.csv(NK, file = "NK.csv")
write.csv(B, file = "B.csv")
write.csv(CD4T, file = "CD4T.csv")
write.csv(CD8T, file = "CD8T.csv")


avgMo = avgG = avgRBC = avgNK = avgB = avgCD4T = avgCD8T = rep(0, length = nrow(LM7))
i <- 0

for (i in 1:nrow(LM7)){
  avgMo[i] <- mean(as.numeric(Mo[i,]))
  avgG[i] <- mean(as.numeric(G[i,]))
  avgRBC[i] <- mean(as.numeric(RBC[i,]))
  avgNK[i] <- mean(as.numeric(NK[i,]))
  avgB[i] <- mean(as.numeric(B[i,]))
  avgCD4T[i] <- mean(as.numeric(CD4T[i,]))
  avgCD8T[i] <- mean(as.numeric(CD8T[i,]))
  
  print(head(avgB))
}

write.csv(avgMo, file = "avgMo.csv")
write.csv(avgG, file = "avgG.csv")
write.csv(avgRBC, file = "avgRBC.csv")
write.csv(avgNK, file = "avgNK.csv")
write.csv(avgB, file = "avgB.csv")
write.csv(avgCD4T, file = "avgCD4T.csv")
write.csv(avgCD8T, file = "avgCD8T.csv")

}