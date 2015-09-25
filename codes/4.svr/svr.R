# Read the normalized data from sigMatNormalized(LM22) and mixDataNormalized (mixture Data), and calculate the model 

#svr <- function() {
library(e1071)
#library(som)

sigMat <- read.csv("sigMatAligned.csv")
mixData <- read.csv("mixDataAligned.csv")

Ref <- read.csv("Ref-229hazf.csv")


for (j in 1:5) {
          data = cbind(mixData[,j+1], sigMat[2:23])
          
          i = 0
          k = 0
          t = 0
          
          sampleName = sprintf( "Mix%01d", j)
          fileName = paste("result_", sampleName, ".csv", sep = "")
          #print(class(data))
          #print(head(data))
          
          #svr_model3 <- svm( data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
          # +data[,11]+data[,12]+data[,13]+data[,14]+data[,15]+data[,16]+data[,17]+data[,18]+data[,19]+data[,20] +data[,21] 
          # +data[,22] +data[,23]  ,data  ,scale = TRUE, type = "nu-regression")
          #print(svr_model3)
          
          #tuneResult <- tune(svm, data[,1] ~  data[,2]+ data[,3] +data[,4] + data[,5] +data[,6] + data[,7] + data[,8] +data[,9]+data[,10]
          #                   +data[,22] +data[,23],  data = data, type = "nu-regression",
          #                   ranges = list(nu = seq(0.25, 0.75, 0.25)), scale = F)
          
          #svr_Model <- svm(data[,1]~., data=data, type = "nu-regression", scale = F)
          #print(tuneResult)
         # res <- cbind(tuneResult$best.model$nu, tuneResult$best.model$cost)
          svrPredictionRMSE = array(0, length(nrow(data)))
          for (k in seq(0.25, 0.75, 0.25)){
                  svr_model <- svm(data[,1] ~ . , data, scale = F, type = "nu-regression", nu = k)
                  predictedm <- predict(svr_model, data)
                  #print(head(predictedm))
                  error <- data[,1] - predictedm
                  #print(error)
                  i = i+1
                  svrPredictionRMSE[i] <-  sqrt(mean(error^2))
                  print(svrPredictionRMSE[i])
          }
          
  
          if (which.min(svrPredictionRMSE) ==1 )
          {k=0.25
          t = 1}
          if (which.min(svrPredictionRMSE) ==2 )
          {k=0.5
          t = 2}
          if (which.min(svrPredictionRMSE) ==3 )
          {k=0.75 
          t=3} 
  
          res <- cbind(k,t )
          write.table( as.data.frame(res), file = "tuned.csv", append=T, sep="," ,  row.names = sampleName, col.names=c("\",\"nu","svrPredictionRMSE"))
          
          svr_model <- svm(data[,1] ~ ., data, scale = F, type = "nu-regression", nu = k)
          #tunedModel <- tuneResult$best.model
          #print(svr_model)
          
          #changed -1
          predicted_svr_model <- predict(svr_model, data)
          #print(head(predicted_tunedModel))
          
          error <- data[,1] - predicted_svr_model
          #print(errorT)
          
          predicted_svr_model_RMSE <- sqrt(mean(error^2))
          #print(tunedModelRMSE)
          
          tst_Sample <- cor.test(data[,1], predicted_svr_model)
          corrSample <- round(tst_Sample$estimate, 3)
          pvalSample <- round(tst_Sample$p.value,3)   
          
          #new
          #svmt = table(pred = predicted_tunedModel, true = data[ ,1] )
          #write.csv(svmt, ("svmtComparison.csv"))
          
          resultT <- cbind(data[,1], predicted_svr_model, tunedModelRMSE, corrSample , pvalSample )
          write.table( as.data.frame(resultT), file = fileName, append=T, sep=",", col.names=c("\",\"Mix", "predicted_svr_model", "RMSE", "Pearson Correlation", "P-Value"))
          
          #values <- cbind(data[,1], predicted_tunedModel, tst_Sample$estimate, corrSample)
          #write.table(as.data.frame(values), file = fileName, append=T, sep="," , col.names=c("\",\"Y","predictedY", "Pearson Correlation", "rounded Correlation"))
          
          coefTuned = matrix(0, ncol= 22, nrow = 1)
          coefTuned <- t(tunedModel$coefs) %*% tunedModel$SV
          # Set negative svr regression coefficients to zero
          coefTuned[coefTuned <0 ] <- 0
          #normalize the coefs
          coefTunedNormed <- round(coefTuned /sum(coefTuned), 2)
          print(coefTunedNormed)
          print(j)
          print(data[1,])
          
          tst_coef <- cor.test(as.numeric(Ref[j,2:23]), as.numeric(coefTunedNormed[1,2:23]))
          corrCoef <- round(tst_coef$estimate, 3)
          pvalCoef <- round(tst_coef$p.value,3)
          
          errorCoef <- as.numeric(Ref[j,2:23])- as.numeric(coefTunedNormed[1,2:23])
          RMSEcoef <- sqrt(mean(errorCoef^2))
          
          
          resultCoef <- cbind(t(coefTunedNormed[2:23]), corrCoef, pvalCoef, RMSEcoef )
          
          if (j ==1){  
            write.table( as.data.frame(resultCoef), file = "resultCoefs.csv", append=T, sep="," , row.names = sampleName,
                         col.names=c("\",\"B cells naive","B cells memory","Plasma cells", "T cells CD8", "T cells CD4 naive" , "T cells CD4 memory resting" , "T cells CD4 memory activated", 
                                       "T cells follicular helper", "T cells regulatory (Tregs)", "T cells gamma delta", "NK cells resting", "NK cells activated", "Monocytes", "Macrophages M0",
                                       "Macrophages M1", "Macrophages M2", "Dendritic cells resting", "Dendritic cells activated", "Mast cells resting", "Mast cells activated", "Eosinophils", "Neutrophils"
                                       ,"Pearson Correlation", "P-value", "RMSE"))
          }else{
            write.table( as.data.frame(resultCoef), file = "resultCoefs.csv", append=T, sep="," , row.names = sampleName)
            
          }     
          

}


