library(dplyr)
library(ggplot2)
function(ggplot2)
install.packages("matrixStats")
library(matrixStats)
    
    
    
    
    
    
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


# calculate the value (mean, median and quantile)

ad1 <- as.numeric(ad)
mean <- rowMeans(ad)
mean <- rowMeans(ad, na.rm = TRUE)
median <- rowMedians(as.matrix(ad))
Q1Q3 <- t(apply(ad, 1, quantile, c(0.25, 0.75)))

is.data.frame(ad)

rowQuantiles(ad)


ad <- cbind(ad, mean, median, Q1Q3)

   
    
# ggplot code
    
    colors <- c("mean" = "black", "median" = "dark grey", "Q1&Q3" = "light blue")
    ggplot(ad_new, aes(x = Year, y = mean)) +
        geom_ribbon(ad_new, 
                    mapping = aes(ymax = ad_new[,8], ymin = ad_new[,9]), fill = "light blue", alpha = 0.5) +
        geom_line(linetype = "solid", size = 0.8, color = "black") +
        geom_point(shape = 21, size = 2, color = "black") +
        geom_line(size = 0.8, aes(y = median, color = "median")) +
        labs(y = "Attainment deficit (%)", 
             title = "Cluster 3") +
        scale_color_manual(name = "legend", 
                           labels = c("mean", "median", "Q1&Q3"),
                           values = c("mean" = "black", "median" = "dark grey", "Q1&Q3" = "light blue")) +
        scale_shape_manual(name = "legend",
                           labels = c("mean", "median", "Q1&Q3"),
                           values = c("mean" = 21, "median" = NA, "Q1&Q3" = NA)) +
        guides(colour = guide_legend(override.aes = list(
            linetype = c("solid", "solid", "solid"),
            color = c("black", "dark grey", "light blue"))),
        shape = guide_legend(override.aes = list(
            shape = c(21, NA, NA)))
        ) +
        theme_bw() +
        theme(axis.line = element_line(color = 'black'),
              plot.background = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(size = 0.3, color = "light grey"),
              panel.grid.minor.y = element_line(size = 0.3, color = "light grey"),
              panel.border = element_blank()) +
        scale_x_continuous(breaks = seq(1985, 2015, 5), limits = c(1985, 2015)) +
        scale_y_continuous(breaks = seq(-80, 0, 20), limits = c(-80, 0)) 
    

    rownames(ad)
    
    # use base R to plot
    plot(rownames(ad), ad$mean, type = "n",
         las = 1, cex = 1, ylim = c(-82, 0),
         xlab = "Year", ylab = "Attainment deficit(%)",
         main = "Cluster 3")
    abline(h = c(0, -20, -40, -60, -80),
           lty = 1, lwd = 1 , col = "lightgrey")
    polygon(c(rownames(ad), rev(rownames(ad))), c(ad$'25%',rev(ad$'75%')),
            col = 13, border = NA)
    points(rownames(ad), ad$mean, type = "o",
           col = "black", lwd = 2)
    lines(rownames(ad), ad$median, col = 16, lwd = 2)
    legend("bottomleft", legend = c("Average", "Median", "Q1 & Q3"),
           bty = "o", bg = "white",
           lwd = c(2, 2, NA), col = c("black", 16, 13),
           pch = c(1, NA, 15))
   
    # write the function
   myplot <- function(df, title, plotmean, color1, color2, color3){
           if(ncol(df) > 1) {
               df <- as.data.frame(df)
               mean <- rowMeans(df, na.rm = TRUE)
               median <- rowMedians(as.matrix(df), na.rm = TRUE)
               Q1Q3 <- t(apply(df, 1, quantile, c(0.25, 0.75)))
               df <- cbind(df, mean, median, Q1Q3)
            plot(rownames(df), df$mean, type = "n",
                 las = 1, cex = 1,
                 xlab = "Year", ylab = "Attainment deficit(%)",
                 main = title)
            abline(lty = 1, lwd = 1 , col = "lightgrey")
            polygon(c(rownames(df), rev(rownames(df))), c(df$'25%',rev(df$'75%')),
                    col = color3, border = NA)
            points(rownames(df), df$mean, type = "o",
                   col = color1, lwd = 2)
            lines(rownames(df), df$median, col = color2, lwd = 2)
            legend("bottomleft", legend = c("Average", "Median", "Q1 & Q3"),
                   bty = "o", bg = "white",
                   lwd = c(2, 2, NA), col = c(color1, color2, color3),
                   pch = c(1, NA, 15))
           } else {
               return("It cannot calculate quantiles per time point")
       }
   }
    
   myplot(ts03, "tsplot", "black", "yellow", "pink")
   d <- c(11,2,3,2,5,3,5)
 ?polygon
 ?rev
  class(ts01)
  a <-  EuStockMarkets 
  class(a)
is.matrix(EuStockMarkets)
is.data.frame(ts03)
is.numeric(ts03) 
as.data.frame(a)
ncol(EuStockMarkets)
