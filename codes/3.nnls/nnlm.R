# To calcualte non-negative linear regression coefficinets for the 24 samples- predicting cell-types. The format of the files and name of the samples are important.
# need "nnls" package.
# IMPORTANT, the orders of columns in sigMat file and in the predicted coefficients output file has to be the same.

library(nnls)
nnlm <- function( directory = "C:/Users/mitra/Desktop/cellType-R/nnlm", significants = "sigMatFormatted.csv", mixData = "Mix.csv", sampleNo = 24, typeNo = 7, outputFile = "nnlmTest.csv"){
  sigMat <- read.csv(significants)
  mixData <- read.csv(mixData)
  #set.seed(20+i)
  #B = sigMat[,2]
  #CD4T = sigMat[,3]
  #CD8T = sigMat[,4]
  #G = sigMat[,5]
  #Mo =sigMat[,6]
  #NK = sigMat[,7]
  #RBC = sigMat[,8]
  
  #print(head(sigMat))
  #print(head(mixData))
  
  for (i in 1:sampleNo){
    
    
    no = sprintf("%01d", i)
    no2 = sprintf("%01d", i+221)
    # the following two lines are for jumping TS229 and TS239 which are absent in our data.
    if (i+221 > 228){ no2 = sprintf("%01d", i+222)}
    if (i+221 >238){no2 = sprintf("%01d", i+223)}
    
    Mix = mixData[,i+1]
    print(head(Mix))
    nnlmModel = nnls(as.matrix(sigMat[,2:8]), Mix)
    
    
    #print(model)
    coefs <- coef(model)
    print(coefs)
    
    coefsNorm <- coefs*100/ sum(coefs)
    #filename = paste("lmCo", no, ".csv")
    sampleName = paste("TS", no2, sep = "")
    
    if (i ==1){  
      write.table( t(as.data.frame(coefsNorm)), file = outputFile, append=T, sep="," , row.names = sampleName, col.names=c("\",\"Mo","G","RBC", "NK", "B" , "CD4T" , "CD8T"))
    }else{
      write.table( t(as.data.frame(coefsNorm)), file = outputFile, append=T, sep="," , row.names = sampleName, col.names= F)
    }
    
    
  }
}
      