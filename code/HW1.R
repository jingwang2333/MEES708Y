library(dplyr)
library(ggplot2)
function(ggplot2)

    
    
edit(plot)    
    
    
    
    
    
    
# Dataset for the source figure
ad <-  read.csv("/Volumes/GoogleDrive/My\ Drive/school/2022Fall/MEES708X+Y/Y/MEES708Y/dataraw/AD_cluster_3.csv")
rownames(ad) <- ad[,1]
ad <- ad[,-1]

# Dataset in ts format
EuStockMarkets

# Dataset with missing values
ad_na <- ad
ad_na$CHOTF[ad$CHOTF == 0] <- NA

# Dataset of 1 time series (should give an error)
ts01 <- rnorm(100)
ts02 <- ts(ts01)
ts03 <- ad[, 1, drop = FALSE]

myplot <- function(x){
    x <- ggplot2()
}