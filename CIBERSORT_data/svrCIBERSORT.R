#svr <- function() {
  library(e1071)
  library(som)
  
  sigMat <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/LM22_102.csv")
  mixDataMain <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/mixDataMerged.csv")
  
  mixDataShort <- merge(mixDataMain, sigMat,   by = "GeneSymbol")
  write.csv(mixDataShort, "mixDaraShort.csv")
  mixData <- mixDataShort[1:6]
  
  # mean, sd, normalize kust accept numeric variables. To convert a data frame to numeric matrix , data.matrix() function is needed
  #sigMat <- as.data.frame(normalize(data.matrix(sigMat), byrow = F))
  #mixData <- as.data.frame(normalize(data.matrix(mixData), byrow = F))
  
  #write.csv(sigMat,"sigMatNormalized.csv")
  #write.csv(mixData,"mixDataNormalized.csv")
  
  #print(head(sigMat))
  #print(head(mixData))
  
  Ref <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/ExampleMixtures-GroundTruth.csv")
  
  
  
  for (j in 1:5) {
    data = cbind(mixData[,j+1], sigMat[2:23])
    
    
    sampleName = sprintf( "Mix%01d", j)
    fileName = paste("result_", sampleName, ".csv", sep = "")
    #print(class(data))
    #print(head(data))
    
    #svr_model3 <- svm( data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
    # +data[,11]+data[,12]+data[,13]+data[,14]+data[,15]+data[,16]+data[,17]+data[,18]+data[,19]+data[,20] +data[,21] 
    # +data[,22] +data[,23]  ,data  ,scale = TRUE, type = "nu-regression")
    #print(svr_model3)
    nu = 0.5
    cost = 1
    tuneResult <- tune(svm, data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
                       +data[,11]+data[,12]+data[,13]+data[,14]+data[,15]+data[,16]+data[,17]+data[,18]+data[,19]+data[,20] +data[,21] 
                       +data[,22] +data[,23],  data = data, type = "nu-regression",
                       ranges = list(nu = seq(0.25, 0.75, 0.25), cost = 2^(2:9)))
    print(tuneResult)
    res <- cbind(tuneResult$best.model$nu, tuneResult$best.model$cost)
    write.table( as.data.frame(res), file = "tuned.csv", append=T, sep="," ,  row.names = sampleName, col.names=c("\",\"nu","cost"))
    
    tuneResult <- tune(svm, data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
                       +data[,11]+data[,12]+data[,13]+data[,14]+data[,15]+data[,16]+data[,17]+data[,18]+data[,19]+data[,20] +data[,21] 
                       +data[,22] +data[,23],  data = data, type = "nu-regression",
                       ranges = list(nu = tuneResult$best.model$nu, cost = 2^(2:9)))
    
    print(tuneResult)
    
    res <- cbind(tuneResult$best.model$nu, tuneResult$best.model$cost)
    write.table( as.data.frame(res), file = "tuned.csv", append=T, sep="," ,  row.names = sampleName, col.names=c("\",\"nu","cost"))
    
    
    tuneResult <- tune(svm, data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
                       +data[,11]+data[,12]+data[,13]+data[,14]+data[,15]+data[,16]+data[,17]+data[,18]+data[,19]+data[,20] +data[,21] 
                       +data[,22] +data[,23],  data = data, type = "nu-regression",
                       ranges = list(nu = seq(0.25, 0.75, 0.25), cost = tuneResult$best.model$cost))
    
    print(tuneResult)
    res <- cbind(tuneResult$best.model$nu, tuneResult$best.model$cost)
    write.table( as.data.frame(res), file = "tuned.csv", append=T, sep="," ,  row.names = sampleName, col.names=c("\",\"nu","cost"))
    
    
    tunedModel <- tuneResult$best.model
    #print(tunedModel)
    
    predicted_tunedModel <- predict(tunedModel, data)
    #print(head(predicted_tunedModel))
    
    errorT <- data[,1] - predicted_tunedModel 
    #print(errorT)
    
    tunedModelRMSE <- sqrt(mean(errorT^2))
    #print(tunedModelRMSE)
    
    tst <- cor.test(data[,1], predicted_tunedModel)
    corr1 <- round(tst$estimate, 3)
    pval1 <- round(tst$p.value,3)   
    
    resultT <- cbind(tunedModel$cost, tunedModel$nu, tuneResult$best.performance, tunedModelRMSE, corr1 , pval1 )
    write.table( as.data.frame(resultT), file = "resultSamples.csv", append=T, sep="," ,  row.names = sampleName, col.names=c("\",\"cost", "nu", "best.performance", "RMSE", "Pearson Correlation", "P-Value"))
    
    values <- cbind(data[,1], predicted_tunedModel, tst$estimate, corr1)
    write.table(as.data.frame(values), file = fileName, append=T, sep="," , col.names=c("\",\"Y","predictedY", "Pearson Correlation", "rounded Correlation"))
    
    coefTuned = matrix(0, ncol= 22, nrow = 1)
    coefTuned <- t(tunedModel$coefs) %*% tunedModel$SV
    # Set negative svr regression coefficients to zero
    coefTuned[coefTuned <0 ] <- 0
    #normalize the coefs
    coefTunedNormed <- round(coefTuned /sum(coefTuned), 2)
   
    

    
    tst_coef <- cor.test(as.numeric(Ref[j,2:23]), as.numeric(coefTunedNormed[1,]))
    corrCoef <- round(tst_coef$estimate, 3)
    pvalCoef <- round(tst_coef$p.value,3)
    
    errorCoef <- as.numeric(Ref[j,2:23])- as.numeric(coefTunedNormed[1,])
    RMSEcoef <- sqrt(mean(errorCoef^2))
    
 
    resultCoef <- cbind(coefTunedNormed, corrCoef, pvalCoef, RMSEcoef )
    
    if (j ==1){  
      write.table( as.data.frame(resultCoef), file = "resultCoefs.csv", append=T, sep="," , row.names = sampleName
                   , col.names=c("\",\"B cells naive","B cells memory","Plasma cells", "T cells CD8", "T cells CD4 naive" , "T cells CD4 memory resting" , "T cells CD4 memory activated", 
                   "T cells follicular helper", "T cells regulatory (Tregs)", "T cells gamma delta", "NK cells resting", "NK cells activated", "Monocytes", "Macrophages M0",
                    "Macrophages M1", "Macrophages M2", "Dendritic cells resting", "Dendritic cells activated", "Mast cells resting", "Mast cells activated", "Eosinophils", "Neutrophils"
                   ,"Pearson Correlation", "P-value", "RMSE"))
    }else{
      write.table( as.data.frame(resultCoef), file = "resultCoefs.csv", append=T, sep="," , row.names = sampleName)
      
    }     
    
    
  }
  
  
  
#}