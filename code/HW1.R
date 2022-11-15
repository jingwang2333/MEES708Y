library(dplyr)
library(ggplot2)
function(ggplot2)
install.packages("matrixStats")
library(matrixStats)
    
    
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


ad1 <- as.numeric(ad)
mean <- rowMeans(ad[,2:5])
median <- rowMedians(as.matrix(ad[,2:5]))
Q1Q3 <- t(apply(ad, 1, quantile, c(0.25, 0.75)))
Q1 <- t(apply(ad, 1, quantile, 0.1))
q1 <- quantile(ad[], na.rm = T, 0.1)

is.data.frame(ad)

rowQuantiles(ad)


ad_new <- cbind(ad, mean, median, Q1Q3)


mean(c(0, 9.59381, 10.32942, 14.83350))

plot <- ggplot()

my_plot <- ggplot(ad_new, aes(x = Year, y = mean)) +
    geom_line(size = 0.8, aes(y = mean, group = 1, color = "black")) +
    geom_line(linetype = "solid", size = 0.8, aes(y = median, group = 1, color = "grey")) +
    geom_line(size = 0.8, aes(y = mean, group = 1, color = "black")) +
    labs(y = "Attainment deficit (%)", 
         title = "Cluster 3") +
    scale_color_manual(name = "Legend", values = colors)
  
?geom_quantile()

  
                     
ggplot(ad_new, aes(x = Year, y = mean)) +
    geom_line(linetype = "solid", size = 0.8, color = "black") +
    geom_point(size = 2, shape = 21, color = "black") +
    geom_line(size = 0.8, aes(y = median), color = "grey") +
    geom_line(size = 0.8, aes_string(y = ad_new[,7]), color = "blue") +
    geom_line(size = 0.8, aes_string(y = ad_new[,8]), color = "light blue") +
    geom_ribbon(ad_new, aes(ymax = ad_new[,7], ymin = ad_new[,8], fill = "blue", alpha = 0.5))
    labs(y = "Attainment deficit (%)", 
         title = "Cluster 3") +
    scale_color_manual(name = "Legend", values = colors)
    
    
# real code
    
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
    
colors <- c("mean" = "black", "median" = "dark grey", "Q1Q3" = "light blue")
    ggplot(ad_new, aes(x = Year, y = mean, color = "mean", linetype = "solid", size = 0.8 )) +
        geom_ribbon(ad_new, mapping = aes(ymax = ad_new[,8], ymin = ad_new[,9]), fill = "light blue", alpha = 0.5) +
        geom_point(size = 2, shape = 21, color = "black", show.legend = T) +
        geom_line(size = 0.8, aes(y = median, color = "median")) +
    labs(y = "Attainment deficit (%)", 
         title = "Cluster 3") +
        scale_color_manual(values = colors) +
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
        
    
    
    # use base R to plot
    plot(ad_new$Year, ad_new$mean, type = "n",
         las = 1, cex = 1, ylim = c(-82, 0),
         xlab = "Year", ylab = "Attainment deficit(%)",
         main = "Cluster 3")
    abline(h = c(0, -20, -40, -60, -80),
           lty = 1, lwd = 1 , col = "lightgrey")
    polygon(c(1985:2014, 2014:1985), c(ad_new$'25%',rev(ad_new$'75%')),
            col = 13, border = NA)
    points(ad_new$Year, ad_new$mean, type = "o",
           col = "black", lwd = 2)
    lines(ad_new$Year, ad_new$median, col = 16, lwd = 2)
    legend("bottomleft", legend = c("Average", "Median", "Q1 & Q3"),
           bty = "o", bg = "white",
           lwd = c(2, 2, NA), col = c("black", 16, 13),
           pch = c(1, NA, 15))
    
    # write the function
    
    plot(ad_new$Year, ad_new$mean, type = "n",
         las = 1, cex = 1, ylim = c(-82, 0),
         xlab = "Year", ylab = "Attainment deficit(%)",
         main = "Cluster 3")
    abline(h = c(0, -20, -40, -60, -80),
           lty = 1, lwd = 1 , col = "lightgrey")
    polygon(c(1985:2014, 2014:1985), c(ad_new$'25%',rev(ad_new$'75%')),
            col = 13, border = NA)
    points(ad_new$Year, ad_new$mean, type = "o",
           col = "black", lwd = 2)
    lines(ad_new$Year, ad_new$median, col = 16, lwd = 2)
    legend("bottomleft", legend = c("Average", "Median", "Q1 & Q3"),
           bty = "o", bg = "white",
           lwd = c(2, 2, NA), col = c("black", 16, 13),
           pch = c(1, NA, 15))
    