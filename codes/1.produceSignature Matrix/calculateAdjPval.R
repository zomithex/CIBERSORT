
#read the separated file for each cell type, number of probes x number of samples, and calculate the pairwise adjusted p-value between cell types and return the ones for each cell type in a separate file (PvalB, PvalG, ...)

pvalCalculation <- function(directory = "/Users/mitra/Desktop/cellType-R/new-24July", sampleNo = 7 ) {
  #sourceFile = "/Users/mitra/Desktop/work/cell_sorted_data_sortedByName.csv")
  #LM7<- read.csv("cell_sorted_data_sortedByName.csv")

PvalB <- 0
PvalCD4T <- 0
PvalCD8T <- 0
PvalG <- 0
PvalMo <- 0
PvalNK <- 0
PvalRBC <- 0

i <- 0

B <- read.csv(paste(directory,"/B.csv", sep = ""))
CD4T <- read.csv(paste(directory,"/CD4T.csv", sep = ""))
CD8T <- read.csv(paste(directory,"/CD8T.csv", sep = ""))
G <- read.csv(paste(directory,"/G.csv", sep = ""))
Mo <- read.csv(paste(directory,"/Mo.csv", sep = ""))
NK <- read.csv(paste(directory,"/NK.csv", sep = ""))
RBC <- read.csv(paste(directory,"/RBC.csv", sep = ""))

#print(head(B))


for (i in 1:nrow(B)){
     # for (i in 1:nrow(B)){ 
        
                b = 0
                
                b5 <- t.test(B[i, 2:8], CD4T[i, 2:8])$p.value
                b6 <- t.test(B[i, 2:8], CD8T[i,2:7])$p.value
                b2 <- t.test(B[i, 2:8], G[i, 2:8])$p.value
                b1 <- t.test(B[i, 2:8], Mo[i, 2:8])$p.value
                b4 <- t.test(B[i, 2:8], NK[i, 2:8])$p.value
                b3 <- t.test(B[i, 2:8], RBC[i, 2:8])$p.value
                
                b <- cbind(b1,b2,b3,b4,b5,b6)
                b <- p.adjust(b, method = "bonferroni")
                
                print(b)
                if (i==1) {PvalB<-b} else{
                  PvalB <- rbind(PvalB,b)
                }
                
                
                cd4t = 0
                
                cd4t5 <- t.test(CD4T[i, 2:8],B[i, 2:8])$p.value
                
                cd4t6 <- t.test(CD4T[i, 2:8], CD8T[i, 2:7])$p.value
                cd4t2 <- t.test(CD4T[i, 2:8], G[i, 2:8])$p.value
                cd4t1 <- t.test(CD4T[i, 2:8], Mo[i, 2:8])$p.value
                cd4t4 <- t.test(CD4T[i, 2:8], NK[i, 2:8])$p.value
                cd4t3 <- t.test(CD4T[i, 2:8], RBC[i, 2:8])$p.value
                
                cd4t <- cbind(cd4t1,cd4t2,cd4t3,cd4t4,cd4t5,cd4t6)
                cd4t <- p.adjust(cd4t, method = "bonferroni")
                print(cd4t)
                if (i==1) {PvalCD4T<-cd4t} else{
                  PvalCD4T <- rbind(PvalCD4T,cd4t)    
                }
                
                
                cd8t <- 0
                cd8t5 <- t.test(CD8T[i, 2:7],B[i, 2:8])$p.value
                cd8t6 <- t.test(CD8T[i, 2:7], CD4T[i, 2:8])$p.value
                cd8t2 <- t.test(CD8T[i, 2:7], G[i, 2:8])$p.value
                cd8t1 <- t.test(CD8T[i, 2:7], Mo[i, 2:8])$p.value
                cd8t4 <- t.test(CD8T[i, 2:7], NK[i, 2:8])$p.value
                cd8t3 <- t.test(CD8T[i, 2:7], RBC[i, 2:8])$p.value
                
                cd8t <- cbind(cd8t1,cd8t2,cd8t3,cd8t4,cd8t5,cd8t6)
                cd8t <- p.adjust(cd8t, method = "bonferroni")
                print(cd8t)
                if (i==1) {PvalCD8T<-cd8t} else{
                  PvalCD8T <- rbind(PvalCD8T,cd8t)  
                }
                
                
                g <- 0
                g4 <- t.test(G[i, 2:8], B[i, 2:8])$p.value
                
                g5 <- t.test(G[i, 2:8], CD4T[i, 2:8])$p.value
                g6 <- t.test(G[i, 2:8], CD8T[i, 2:7])$p.value
                g1 <- t.test(G[i, 2:8], Mo[i, 2:8])$p.value
                g3 <- t.test(G[i, 2:8], NK[i, 2:8])$p.value
                g2 <- t.test(G[i, 2:8], RBC[i, 2:8])$p.value
                
                g <- cbind(g1,g2,g3,g4,g5,g6)
                g <- p.adjust(g, method = "bonferroni")
                print(g)
                if (i==1) {PvalG<-g} else{
                  PvalG <- rbind(PvalG,g)
                }
                
                
                mo <- 0
                mo4 <- t.test(Mo[i, 2:8], B[i, 2:8])$p.value
                mo5 <- t.test(Mo[i, 2:8], CD4T[i, 2:8])$p.value
                mo6 <- t.test(Mo[i, 2:8], CD8T[i, 2:7])$p.value
                mo1 <- t.test(Mo[i, 2:8], G[i, 2:8])$p.value
                mo3 <- t.test(Mo[i, 2:8], NK[i, 2:8])$p.value
                mo2 <- t.test(Mo[i, 2:8], RBC[i, 2:8])$p.value
                
                mo <- cbind(mo1,mo2,mo3,mo4,mo5,mo6)
                mo <- p.adjust(mo, method = "bonferroni")
                print(mo)
                if (i==1) {PvalMo<-mo} else{
                  PvalMo <- rbind(PvalMo,mo)
                }
                
                
                nk <- 0
                nk4 <- t.test(NK[i, 2:8], B[i, 2:8])$p.value
                nk5 <- t.test(NK[i, 2:8], CD4T[i, 2:8])$p.value
                nk6 <- t.test(NK[i, 2:8], CD8T[i, 2:7])$p.value
                nk2 <- t.test(NK[i, 2:8], G[i, 2:8])$p.value
                nk1 <- t.test(NK[i, 2:8], Mo[i, 2:8])$p.value
                nk3 <- t.test(NK[i, 2:8], RBC[i, 2:8])$p.value
                
                nk <- cbind(nk1,nk2,nk3,nk4,nk5,nk6)
                nk <- p.adjust(nk, method = "bonferroni")
                print(nk)
                if (i==1) {PvalNK<-nk} else{
                  PvalNK <- rbind(PvalNK,nk)
                }
                
                
                rbc4 <- t.test(RBC[i, 2:8], B[i, 2:8])$p.value
                rbc5 <- t.test(RBC[i, 2:8], CD4T[i, 2:8])$p.value
                rbc6 <- t.test(RBC[i, 2:8], CD8T[i, 2:7])$p.value
                rbc2 <- t.test(RBC[i, 2:8], G[i, 2:8])$p.value
                rbc1 <- t.test(RBC[i, 2:8], Mo[i, 2:8])$p.value
                rbc3 <- t.test(RBC[i, 2:8], NK[i, 2:8])$p.value
                
                rbc <- cbind(rbc1,rbc2,rbc3,rbc4,rbc5,rbc6)
                rbc <- p.adjust(rbc, method = "bonferroni")
                print(b)
                if (i==1) {PvalRBC<-rbc} else{
                  PvalRBC <- rbind(PvalRBC,rbc)
                }
                
        }  

                          
                         
       
        #print(PvalB)
        write.csv(PvalMo, file = "PvalMo.csv")
        write.csv(PvalG, file = "PvalG.csv")
        write.csv(PvalRBC, file = "PvalRBC.csv")
        write.csv(PvalNK, file = "PvalNK.csv")
        write.csv(PvalB, file = "PvalB.csv")
        write.csv(PvalCD4T, file = "PvalCD4T.csv")
        write.csv(PvalCD8T, file = "PvalCD8T.csv")
        #PvalMin <- cbind(min(PvalMo), min(PvalG), min(PvalRBC), min(PvalNK), min(PvalB), min(PvalCD4T), min(PvalCD8T))
       # write.csv(PvalMin, "PvalMin.csv")
        
        
}       
        

  



