# This function, reads the observed data and the predicted data from a single file (format important) amd make the scatterplot of predicted versus observed for each sample.

comPlotSample <- function(directory = "/Users/mitra/Desktop/cellType-R/new-24July/ConNo", fileName = "svrPlot797.csv") {
  #code to clear all plots in Rstudio: 
  #dev.off(dev.list()["RStudioGD"])
  
  n.col = 6
  n.row = 6
  par(mfrow = c(n.row, n.col))
  par(mar = c(0,1,3,2))
  
  lm <- read.csv(paste(directory, "/", fileName, sep = ""))
  
  for (i in 1:24){
    x= lm[1:7, i+1] # x is the reference ratios from cell-count file
    y = lm[8:14,i+1] #y is the ratios from the model
    
    plot(x,y, col = "blue", frame = F, xlim = range(x), ylim = range(y))
    text(x,y, as.character(lm[1:7, 1]),cex= 0.7, pos=1)
    
    fit <- lm(y~x, data = lm) 
    abline(fit, col = "red", lty = 2)
    
    tst <- cor.test(x, y) 
    
    m <- paste("corr=", as.character(round(tst$estimate, 2)), ",p=" ,as.character(round(tst$p.value,3)))
    mtext(m, side = 3, cex = 0.5)
    
    # problem : "cex" is not working in changing the size of the font in mtext. It remanins too big for the plots.
    # mtext(paste("corr = ", as.character(round(tst$estimate, 2)), ",p = " ,as.character(round(tst$p.value,3)), side = 3, cex = 0.1))
    
    
  }
}

