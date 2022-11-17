#Jing Wang

library(dplyr)
library(ggplot2)
function(ggplot2)
install.packages("matrixStats")
library(matrixStats)
library(tidyr)
install.packages("xts")
library(xts)
    

 ############################## source and test ##########################

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


################## This function plot #########################3
    
   myplot <- function(df, title, plotmean = FALSE, color1, color2, color3) {
     if (is.ts(df)) {
       x <- time(df)
       x <- as.numeric(x)
       df <- as.data.frame(df)
     } else {
              df <- as.data.frame(df)
              x <- rownames(df)
              x <- as.numeric(x)
              }
             if (ncol(df) > 1) {
                 is.na(df)
                 mean <- rowMeans(df, na.rm = TRUE)
                 median <- apply(df, 1, median, na.rm = TRUE)
                 Q1Q3 <- t(apply(df, 1, quantile, c(0.25, 0.75), na.rm = TRUE))
                 df <- cbind(df, mean, median, Q1Q3)
              plot(x, df$mean, type = "n", 
                   las = 1, cex = 1, 
                   xlab = "Year", ylab = "Attainment deficit(%)",
                   main = title)
              abline( 
                      lty = 1, lwd = 1 , col = "lightgrey")
              if (plotmean) {
                  polygon(c(x, rev(x)), c(df$'25%',rev(df$'75%')),
                          col = color3, border = NA)
                  points(x, df$mean, type = "o",
                         col = color1, lwd = 2)
                  lines(x, df$median, col = color2, lwd = 2)
                  legend("bottomright", legend = c("Average", "Median", "Q1 & Q3"),
                         bty = "o", bg = "white", cex = 0.6,
                         lwd = c(2, 2, NA), col = c(color1, color2, color3),
                         pch = c(1, NA, 15))
              } else {
                  polygon(c(x, rev(x)), c(df$'25%',rev(df$'75%')),
                          col = color3, border = NA)
                  lines(x, df$median, col = color2, lwd = 2)
                  legend("bottomright", legend = c( NA, "Median", "Q1 & Q3"),
                         bty = "o", bg = "white", cex = 0.6,
                         lwd = c(NA, 2, NA), col = c(NA, color2, color3),
                         pch = c(NA, NA, 15))
            }
           } else {
                     return("It cannot calculate quantiles per time point")
       }
   }
    
   myplot(ad_na, "tsplot", plotmean = TRUE, "black", "yellow", "pink")
   
   

   

