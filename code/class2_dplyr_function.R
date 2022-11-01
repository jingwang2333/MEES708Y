worm <- read.table("./dataRaw/worms.missing.txt", head=TRUE)
head(worm)
setwd("/Users/wangjing/Desktop/Rstudio/dataraw-20221025")
worm <- read.table("./dataRaw/worms.missing.txt", head=TRUE)
worm <- read.table("worms.missing.txt", head=TRUE)
head(worm)
is.data.frame(worm)

## data.table

view(worm)
worm[order(worm$Area), ] #sort the values  by the column. 

tmp <- scan("Lengths.dat",
            what=list(Name="",Family="",length=0),
            na.string=".")
tmp <- scan("./dataraw/Lengths.dat",
            what=list(Name="",Family="",Length=0),
            na.string=".")
marine = as.data.frame(tmp)
head(marine)
marine[order(marine$Family, marine$length), ]
tmp
marine1 <- aggregate(length ~ Family, data = marine, mean, trim = 0.3)

?aggregate

library(dplyr)
worm_tbl = as_tibble(worm)
head(worm_tbl)

marine %>%
    arrange(Family, length) %>%
    group_by(Family) %>%
    summarise(length = mean(length))

#merging
descriptions <- read.fwf("Chocolate.dat",
                         widths = c(4, 10, 46),
                         col.names = c("CodeNum", "Name", "Description"))
descriptions <- na.omit(descriptions)
sales <- read.table("chocsales.dat", header = FALSE,
                    col.names = c("CodeNum", "PiecesSold"))

# base R
x <- merge(sales, descriptions, all = TRUE)
dim(x)
?merge
?dim

# dplyr

x2 <- sales %>%
    full_join(descriptions, by = "CodeNum")

?inner_join


library(reshape2)
baseball <- read.table("Transpos.dat",
                       head = FALSE, col.names = c("Team", "Player", "Type", "Entry"))
baseball.m <- melt(baseball,
                   idvars=c("Team", "Player", "Type"), measure.vars = "Entry")
head(baseball.m)
