# This function, reads the observed data and the predicted data from a single file (format important) amd make the scatterplot of predicted versus observed for each cell type.

comPlotType <- function(directory = "C:/Users/user/Desktop/mit", fileName = "lmPlot.csv") {
  #code to clear all plots in Rstudio: 
  #dev.off(dev.list()["RStudioGD"])
  
  n.col = 3
  n.row = 3
  par(mfrow = c(n.row, n.col))
  par(mar = c(2,1,5,2))
  
  lm <- read.csv(paste(directory, "/", fileName, sep = ""))
  
  for (i in 1:7){
    x= as.numeric(lm[i, 2:25])
    y = as.numeric(lm[i+7,2:25])
    
    plot(x,y, col = i+3, frame = F, xlim = range(x), ylim = range(y), main = as.character(lm[i,1]), cex.main = 0.9, col.main = i+3
         )
    text(x,y, as.character(c(1:24)),cex= 0.7, pos=1)
    
    fit <- lm(y~x, data = lm) 
    abline(fit, col = "green", lty = 2)
    
    tst <- cor.test(x, y) 
    
    m <- paste("corr=", as.character(round(tst$estimate, 2)), ",p=" ,as.character(round(tst$p.value,3)))
    mtext(m, side = 3, cex = 0.5)
    
    # problem : "cex" is not working in changing the size of the font in mtext. It remanins too big for the plots.
    # mtext(paste("corr = ", as.character(round(tst$estimate, 2)), ",p = " ,as.character(round(tst$p.value,3)), side = 3, cex = 0.1))
    
    
  }
}

