
#read the Pvalue, effectSize and average value files.
#check the threshold for Pvalues and effect size apirwise between cell types for each probe
#every probe which passes both threshold will be save in signature matrix with its average (between samples) values (between samples)

makeSigMat <- function(sourceFile = "/Users/mitra/Desktop/work/cell_sorted_data_sortedByName.csv", directory = "/Users/mitra/Desktop/cellType-R/new-24July", cellTypeNo = 7) {


# probe has to be significant in all pairwise comparisons.
# P-value threshold < 10-4
# Effect size threshold: average difference >0.15

#LM7<- read.csv(sourceFile)

PvalMo <- read.csv(paste(directory,"/PvalMo.csv", sep = ""))
PvalG <- read.csv(paste(directory,"/PvalG.csv", sep = ""))
PvalRBC <- read.csv(paste(directory,"/PvalRBC.csv", sep = ""))
PvalNK <- read.csv(paste(directory,"/PvalNK.csv", sep = ""))
PvalB <- read.csv(paste(directory,"/PvalB.csv", sep = ""))
PvalCD4T <- read.csv(paste(directory,"/PvalCD4T.csv", sep = ""))
PvalCD8T <- read.csv(paste(directory,"/PvalCD8T.csv", sep = ""))

#print(head(PvalMo))

effectSizeMo <- read.csv(paste(directory,"/effectSizeMo.csv", sep = ""))
effectSizeG <- read.csv(paste(directory,"/effectSizeG.csv", sep = ""))
effectSizeRBC <- read.csv(paste(directory,"/effectSizeRBC.csv", sep = ""))
effectSizeNK <- read.csv(paste(directory,"/effectSizeNK.csv", sep = ""))
effectSizeB <- read.csv(paste(directory,"/effectSizeB.csv", sep = ""))
effectSizeCD4T <- read.csv(paste(directory,"/effectSizeCD4T.csv", sep = ""))
effectSizeCD8T <- read.csv(paste(directory,"/effectSizeCD8T.csv", sep = ""))

#print(head(effectSizeCD8T))

avgMo <- read.csv(paste(directory,"/avgMo.csv", sep = "")) 
avgG <- read.csv(paste(directory,"/avgG.csv", sep = "")) 
avgRBC <- read.csv(paste(directory,"/avgRBC.csv", sep = "")) 
avgNK <- read.csv(paste(directory,"/avgNK.csv", sep = "")) 
avgB <- read.csv(paste(directory,"/avgB.csv", sep = "")) 
avgCD4T <- read.csv(paste(directory,"/avgCD4T.csv", sep = "")) 
avgCD8T <- read.csv(paste(directory,"/avgCD8T.csv", sep = "")) 

#print(head(avgMo))
#print(head(avgG))
m=0

sigMat = matrix(0,nrow = nrow(LM7), ncol = (cellTypeNo +1 ))
print(head(sigMat))

  for (i in 1: nrow(LM7)){
   
             
             sigMat[i,1] = as.character(LM7[i,1]) 
   # print(sigMat[i,1] )
    
    
    
              if (all(PvalMo[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeMo[i, 2: (cellTypeNo - 1) ] > 0.15)) {
    print(i)  
                print("Mo")
                
                m= rbind(i)
                sigMat[i, 2] = avgMo[i, 2]
                sigMat[i, 3] = avgG[i, 2]
                sigMat[i, 4] = avgRBC[i, 2]
                sigMat[i, 5] = avgNK[i, 2]
                sigMat[i, 6] = avgB[i, 2]
                sigMat[i, 7] = avgCD4T[i, 2]
                sigMat[i, 8] = avgCD8T[i, 2]
           #test     
                print(avgMo[i, 2]) 
                print(sigMat[i, 2])
                print(avgG[i, 2])
                print( sigMat[i, 3])
                print(as.character(LM7[i,1]) )
                print( sigMat[i,1])
           #test     
              } 
  

   
            if (all(PvalG[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeG[i, 2: (cellTypeNo - 1) ] > 0.15)) {
    print(i) 
              print("G")
              m= rbind(i)
              #sigMat[i,1] = as.character(LM7[i,1])
              sigMat[i, 2] = avgMo[i, 2]
              sigMat[i, 3] = avgG[i, 2]
              sigMat[i, 4] = avgRBC[i, 2]
              sigMat[i, 5] = avgNK[i, 2]
              sigMat[i, 6] = avgB[i, 2]
              sigMat[i, 7] = avgCD4T[i, 2]
              sigMat[i, 8] = avgCD8T[i, 2]
              
              print(avgMo[i, 2]) 
              print(sigMat[i, 2])
              
              
            } 
    
    
         if (all(PvalRBC[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeRBC[i, 2: (cellTypeNo - 1) ] > 0.15)) {
    print(i)  
           print("RBC")
           m= rbind(i)
            #sigMat[i,1] = as.character(LM7[i,1])
            sigMat[i, 2] = avgMo[i, 2]
            sigMat[i, 3] = avgG[i, 2]
            sigMat[i, 4] = avgRBC[i, 2]
            sigMat[i, 5] = avgNK[i, 2]
            sigMat[i, 6] = avgB[i, 2]
            sigMat[i, 7] = avgCD4T[i, 2]
            sigMat[i, 8] = avgCD8T[i, 2]
            
            
            print(avgMo[i, 2]) 
            print(sigMat[i, 2])
            
            
          } 
    
    
        if (all(PvalNK[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeNK[i, 2: (cellTypeNo - 1) ] > 0.15)) {
 print(i) 
          print("NK")
          m= rbind(i)
          #sigMat[i,1] = as.character(LM7[i,1])
          sigMat[i, 2] = avgMo[i, 2]
          sigMat[i, 3] = avgG[i, 2]
          sigMat[i, 4] = avgRBC[i, 2]
          sigMat[i, 5] = avgNK[i, 2]
          sigMat[i, 6] = avgB[i, 2]
          sigMat[i, 7] = avgCD4T[i, 2]
          sigMat[i, 8] = avgCD8T[i, 2]
          
          print(avgMo[i, 2]) 
          print(sigMat[i, 2])
          
          
        } 
    
    
      if (all(PvalB[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeB[i, 2: (cellTypeNo - 1) ] > 0.15)) {
print(i) 
        print("B")
        m= rbind(i)
        #sigMat[i,1] = as.character(LM7[i,1])
        sigMat[i, 2] = avgMo[i, 2]
        sigMat[i, 3] = avgG[i, 2]
        sigMat[i, 4] = avgRBC[i, 2]
        sigMat[i, 5] = avgNK[i, 2]
        sigMat[i, 6] = avgB[i, 2]
        sigMat[i, 7] = avgCD4T[i, 2]
        sigMat[i, 8] = avgCD8T[i, 2]
        
        print(avgMo[i, 2]) 
        print(sigMat[i, 2])
        
        
      } 
    
      if (all(PvalCD4T[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeCD4T[i, 2: (cellTypeNo - 1) ] > 0.15)) {
 print(i) 
        print("CD4T")
        m= rbind(i)
        #sigMat[i,1] = as.character(LM7[i,1])
        sigMat[i, 2] = avgMo[i, 2]
        sigMat[i, 3] = avgG[i, 2]
        sigMat[i, 4] = avgRBC[i, 2]
        sigMat[i, 5] = avgNK[i, 2]
        sigMat[i, 6] = avgB[i, 2]
        sigMat[i, 7] = avgCD4T[i, 2]
        sigMat[i, 8] = avgCD8T[i, 2]
        
        print(avgMo[i, 2]) 
        print(sigMat[i, 2])
        
        
      }
   
  
  
      if (all(PvalCD8T[i, 2: (cellTypeNo - 1) ] < 0.0001) & all(effectSizeCD8T[i, 2: (cellTypeNo - 1) ] > 0.15)) {
print(i) 
        print("CD8T")
        m= rbind(i)
        #sigMat[i,1] = as.character(LM7[i,1])
        sigMat[i, 2] = avgMo[i, 2]
        sigMat[i, 3] = avgG[i, 2]
        sigMat[i, 4] = avgRBC[i, 2]
        sigMat[i, 5] = avgNK[i, 2]
        sigMat[i, 6] = avgB[i, 2]
        sigMat[i, 7] = avgCD4T[i, 2]
        sigMat[i, 8] = avgCD8T[i, 2]
        
        print(avgMo[i, 2]) 
        print(sigMat[i, 2])
        
      
      }
  
      
  #print(sigMat[i,])
  
  }
write.csv(m, "chosen.csv")
write.csv(sigMat, paste(directory,"/sigMat.csv", sep = ""))
}