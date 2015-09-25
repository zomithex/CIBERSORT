#read the files with the average values for each cell types, and return the effect size matrixes for them

effectSize <- function( sourceFile = "/Users/mitra/Desktop/work/cell_sorted_data_sortedByName.csv",directory = "/Users/mitra/Desktop/cellType-R/new-24July", cellTypeNo = 7) {
  #LM7<- read.csv(sourceFile)
  
  effectSizeB <-  matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeCD4T <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeCD8T <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeG <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeMo <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeNK <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  effectSizeRBC <- matrix(0, nrow= nrow(LM7), ncol = (cellTypeNo - 1))
  
  #print(head(effectSizeB))
  
  i <- 0
  
  
  
  #print(head(B))
  
  
  avgMo <- read.csv(paste(directory,"/avgMo.csv", sep = "")) 
  avgG <- read.csv(paste(directory,"/avgG.csv", sep = "")) 
  avgRBC <- read.csv(paste(directory,"/avgRBC.csv", sep = "")) 
  avgNK <- read.csv(paste(directory,"/avgNK.csv", sep = "")) 
  avgB <- read.csv(paste(directory,"/avgB.csv", sep = "")) 
  avgCD4T <- read.csv(paste(directory,"/avgCD4T.csv", sep = "")) 
  avgCD8T <- read.csv(paste(directory,"/avgCD8T.csv", sep = "")) 
  
  print(head(avgB))
  
  
  
  for (i in 1:nrow(LM7)){ 
  
    print(i)
    
    b = 0
    
    b5 <- avgB[i, 2 ]- avgCD4T[i, 2 ]
    b6 <- avgB[i, 2 ]- avgCD8T[i, 2 ]
    b2 <- avgB[i, 2 ]- avgG[i, 2 ]
    b1 <- avgB[i, 2 ]- avgMo[i, 2 ] 
    b4 <- avgB[i, 2 ]- avgNK[i, 2 ]
    b3 <- avgB[i, 2 ]- avgRBC[i, 2 ]
    
    #print("b1")
    print(head(b1))
    
    b <- cbind(b1,b2,b3,b4,b5,b6)
    
    #print(i)
    #print("hereB")
    #print(head(effectSizeB))
    
    if (i==1) {effectSizeB<-b} else{
      effectSizeB <- rbind(effectSizeB,b)
    }
    
    #print(class(effectSizeB))
    #print(dim(effectSizeB))
    print(head(effectSizeB))
    
    cd4t = 0
    
    cd4t5 <- avgCD4T[i, 2 ]- avgB[i, 2 ]
    cd4t6 <- avgCD4T[i, 2 ]- avgCD8T[i, 2 ]
    cd4t2 <- avgCD4T[i, 2 ]- avgG[i, 2 ]
    cd4t1 <- avgCD4T[i, 2 ]- avgMo[i, 2 ] 
    cd4t4 <- avgCD4T[i, 2 ]- avgNK[i, 2 ]
    cd4t3 <- avgCD4T[i, 2 ]- avgRBC[i, 2 ]
    
    cd4t <- cbind(cd4t1,cd4t2,cd4t3,cd4t4,cd4t5,cd4t6)
    
    if (i==1) {effectSizeCD4T<-cd4t} else{
      effectSizeCD4T <- rbind(effectSizeCD4T,cd4t)
    }
    
    #print(i)
    #print("hereCD4T")
    #print(head(effectSizeCD4T))
    
    
    cd8t = 0
    
    cd8t5 <- avgCD8T[i, 2 ]- avgCD4T[i, 2 ]
    cd8t6 <- avgCD8T[i, 2 ]- avgB[i, 2 ]
    cd8t2 <- avgCD8T[i, 2 ]- avgG[i, 2 ]
    cd8t1 <- avgCD8T[i, 2 ]- avgMo[i, 2 ] 
    cd8t4 <- avgCD8T[i, 2 ]- avgNK[i, 2 ]
    cd8t3 <- avgCD8T[i, 2 ]- avgRBC[i, 2 ]
    
    cd8t <- cbind(cd8t1,cd8t2,cd8t3,cd8t4,cd8t5,cd8t6)
    
    if (i==1) {effectSizeCD8T<-cd8t} else{
      effectSizeCD8T <- rbind(effectSizeCD8T,cd8t)
    }
    
    print(i)
    #print("hereCD8T")
    print(head(effectSizeCD8T))
    
    
    g = 0
    
    g5 <- avgG[i, 2 ]- avgCD4T[i, 2 ]
    g6 <- avgG[i, 2 ]- avgCD8T[i, 2 ]
    g2 <- avgG[i, 2 ]- avgB[i, 2 ]
    g1 <- avgG[i, 2 ]- avgMo[i, 2 ] 
    g4 <- avgG[i, 2 ]- avgNK[i, 2 ]
    g3 <- avgG[i, 2 ]- avgRBC[i, 2 ]
    
    g <- cbind(g1,g2,g3,g4,g5,g6)
    
    if (i==1) {effectSizeG<-g} else{
      effectSizeG <- rbind(effectSizeG,g)
    }
    
    print(i)
    #print("hereG")
    print(head(effectSizeG))
    
    
    nk = 0
    
    nk5 <- avgNK[i, 2 ]- avgCD4T[i, 2 ]
    nk6 <- avgNK[i, 2 ]- avgCD8T[i, 2 ]
    nk2 <- avgNK[i, 2 ]- avgG[i, 2 ]
    nk1 <- avgNK[i, 2 ]- avgMo[i, 2 ] 
    nk4 <- avgNK[i, 2 ]- avgB[i, 2 ]
    nk3 <- avgNK[i, 2 ]- avgRBC[i, 2 ]
    
    nk <- cbind(nk1,nk2,nk3,nk4,nk5,nk6)
    
    if (i==1) {effectSizeNK<-nk} else{
      effectSizeNK <- rbind(effectSizeNK,nk)
    }
    print(i)
    #print("hereNK")
    print(head(effectSizeNK))
    
    
    
    rbc = 0
    
    rbc5 <- avgRBC[i, 2 ]- avgCD4T[i, 2 ]
    rbc6 <- avgRBC[i, 2 ]- avgCD8T[i, 2 ]
    rbc2 <- avgRBC[i, 2 ]- avgG[i, 2 ]
    rbc1 <- avgRBC[i, 2 ]- avgMo[i, 2 ] 
    rbc4 <- avgRBC[i, 2 ]- avgNK[i, 2 ]
    rbc3 <- avgRBC[i, 2 ]- avgB[i, 2 ]
    
    rbc <- cbind(rbc1,rbc2,rbc3,rbc4,rbc5,rbc6)
    
    if (i==1) {effectSizeRBC<-rbc} else{
      effectSizeRBC <- rbind(effectSizeRBC,rbc)
    }
    
    #print(i)
    #print("hereRBC")
    #print(head(effectSizeRBC))
    
    
    mo = 0
    
    mo5 <- avgMo[i, 2 ]- avgCD4T[i, 2 ]
    mo6 <- avgMo[i, 2 ]- avgCD8T[i, 2 ]
    mo2 <- avgMo[i, 2 ]- avgG[i, 2 ]
    mo1 <- avgMo[i, 2 ]- avgB[i, 2 ] 
    mo4 <- avgMo[i, 2 ]- avgNK[i, 2 ]
    mo3 <- avgMo[i, 2 ]- avgRBC[i, 2 ]
    
    mo <- cbind(mo1,mo2,mo3,mo4,mo5,mo6)
    
    if (i==1) {effectSizeMo<-mo} else{
      effectSizeMo <- rbind(effectSizeMo,mo)
    }
    
    print(i)
    #print("hereMo")
    print(head(effectSizeMo))
    
    
  }  
  
  
  
  
  
  write.csv(effectSizeMo, file = "effectSizeMo.csv")
  write.csv(effectSizeG, file = "effectSizeG.csv")
  write.csv(effectSizeRBC, file = "effectSizeRBC.csv")
  write.csv(effectSizeNK, file = "effectSizeNK.csv")
  write.csv(effectSizeB, file = "effectSizeB.csv")
  write.csv(effectSizeCD4T, file = "effectSizeCD4T.csv")
  write.csv(effectSizeCD8T, file = "effectSizeCD8T.csv")
  
  
}







