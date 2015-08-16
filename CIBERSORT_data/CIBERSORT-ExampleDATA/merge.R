B <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/B cells naive.csv")
C <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/B cells memory_2.csv")
D <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Plasma cells_3.csv")
E <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells CD8_4.csv")
F1 <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells CD4 naive_5.csv")
G <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells CD4 memory resting_6.csv")
H <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells CD4 memory activated_7.csv")
I <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells follicular helper_8.csv")
J <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells regulatory (Tregs)_9.csv")
K <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/T cells gamma delta_10.csv")
L <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/NK cells resting_11.csv")
M <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/NK cells activated_12.csv")
N <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Monocytes_13.csv")
O <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Macrophages M0_14.csv")
P <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Macrophages M1_15.csv")
Q <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Macrophages M2_16.csv")
R <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Dendritic cells resting_17.csv")
S <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Dendritic cells activated_18.csv")
T1 <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Mast cells resting_19.csv")
U <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Mast cells activated_20.csv")
V <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Eosinophils_21.csv")
W <- read.csv("/Users/mitra/Desktop/cellType-R/CIBERSORT_data/forConditionNumber/Neutrophils_22.csv")



mydata1 <- merge(B, C,   by = "GeneSymbol", all=TRUE)
write.csv(mydata1, "mydata1.csv")

mydata2 <- merge(mydata1, D,   by = "GeneSymbol", all=TRUE)
write.csv(mydata2, "mydata2.csv")

mydata3 <- merge(mydata2, E,   by = "GeneSymbol", all=TRUE)
write.csv(mydata3, "mydata3.csv")

mydata4 <- merge(mydata3, F1,   by = "GeneSymbol", all=TRUE)
write.csv(mydata4, "mydata4.csv")

mydata5 <- merge(mydata4, G,   by = "GeneSymbol", all=TRUE)
write.csv(mydata5, "mydata5.csv")

mydata6 <- merge(mydata5, H,   by = "GeneSymbol", all=TRUE)
write.csv(mydata6, "mydata6.csv")

mydata7 <- merge(mydata6, I,   by = "GeneSymbol", all=TRUE)
write.csv(mydata7, "mydata7.csv")

mydata8 <- merge(mydata7, J,   by = "GeneSymbol", all=TRUE)
write.csv(mydata8, "mydata8.csv")

mydata9 <- merge(mydata8, K,   by = "GeneSymbol", all=TRUE)
write.csv(mydata9, "mydata9.csv")

mydata10 <- merge(mydata9, L,   by = "GeneSymbol", all=TRUE)
write.csv(mydata10, "mydata10.csv")

mydata11 <- merge(mydata10, M,   by = "GeneSymbol", all=TRUE)
write.csv(mydata11, "mydata11.csv")

mydata12 <- merge(mydata11, N,   by = "GeneSymbol", all=TRUE)
write.csv(mydata12, "mydata12.csv")

mydata13 <- merge(mydata12, O,   by = "GeneSymbol", all=TRUE)
write.csv(mydata13, "mydata13.csv")

mydata14 <- merge(mydata13, P,   by = "GeneSymbol", all=TRUE)
write.csv(mydata14, "mydata14.csv")

mydata15 <- merge(mydata14, Q,   by = "GeneSymbol", all=TRUE)
write.csv(mydata15, "mydata15.csv")

mydata16 <- merge(mydata15, R,   by = "GeneSymbol", all=TRUE)
write.csv(mydata16, "mydata16.csv")

mydata17 <- merge(mydata16, S,   by = "GeneSymbol", all=TRUE)
write.csv(mydata17, "mydata17.csv")

mydata18 <- merge(mydata17, T1,   by = "GeneSymbol", all=TRUE)
write.csv(mydata18, "mydata18.csv")

mydata19 <- merge(mydata18, U,   by = "GeneSymbol", all=TRUE)
write.csv(mydata19, "mydata19.csv")

mydata20 <- merge(mydata19, V,   by = "GeneSymbol", all=TRUE)
write.csv(mydata20, "mydata20.csv")

mydata21 <- merge(mydata20, W,   by = "GeneSymbol", all=TRUE)
write.csv(mydata21, "mydataFinal.csv")

LM22 <- read.csv("LM22.csv")
mydataFinal_filled <- merge(LM22, mydata21,   by = "GeneSymbol", all=TRUE)
write.csv(mydataFinal_filled, "mydataFinal_filled.csv")
