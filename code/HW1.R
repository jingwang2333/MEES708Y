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
Q1Q3 <- t(apply(ad, 1, quantile, c(0.1, 0.3)))
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
    

    ggplot(ad_new, aes(x = Year, y = mean)) +
        geom_ribbon(ad_new, mapping = aes(ymax = ad_new[,7], ymin = ad_new[,8]), fill = "light blue", alpha = 0.5) +
        geom_line(linetype = "solid", size = 0.8, color = "black") +
        geom_point(size = 2, shape = 21, color = "black") +
        geom_line(size = 0.8, aes(y = median), color = "grey") +
    labs(y = "Attainment deficit (%)", 
         title = "Cluster 3") +
        theme_bw()
        scale_color_manual(name = "Legend", values = colors)
