funtimes::ccf_boot()
library(funtimes)
install.packages("funtimes")

x <- rnorm(10000)
y <- rnorm(10000)
ccf(x,y)
tmp <- ccf_boot(x, y)

?funtimes::ccf_boot


a <- untar(download.packages(pkgs = "funtimes",
                        destdir = ".",
                        type = "source")[,2])


ccf_boot_new <- function(x, y, lag.max = NULL, 
                     plot = c("Pearson", "Spearman", "none"),
                     level = 0.95, B = 1000, ...)
{
    ### Perform various checks
    namex <- deparse(substitute(x))[1L]
    namey <- deparse(substitute(y))[1L]
    if (is.matrix(x) || is.matrix(y)) {
        stop("x and y should be univariate time series only.")
    }
    if (any(is.na(x)) || any(is.na(y))) {
        stop("data should not contain missing values.")
    }
    nx <- length(x)
    ny <- length(y)
    B <- as.integer(B)
    if (B <= 0) {
        stop("number of bootstrap resamples B must be positive.")
    }
    plt <- match.arg(plot)
    
    ### Function
    xrank <- rank(x)
    yrank <- rank(y)
    attributes(xrank) <- attributes(x)
    attributes(yrank) <- attributes(y)
    tmp <- ccf(x, y, lag.max = lag.max, plot = FALSE)
    lags <- tmp$lag[,1,1]
    rP <- tmp$acf[,1,1]
    rS <- ccf(xrank, yrank, lag.max = lag.max, plot = FALSE)$acf[,1,1]
    phetax <- ARest(x, ...)
    phetay <- ARest(y, ...)
    if (length(phetax) > 0) {
        names(phetax) <- paste0(rep("phi_", length(phetax)), c(1:length(phetax)))
        tmp <- stats::filter(x, phetax, sides = 1)
        Zx <- x[(length(phetax) + 1):nx] - tmp[length(phetax):(nx - 1)]
    } else {
        Zx <- x
    }
    Zx <- Zx - mean(Zx)
    if (length(phetay) > 0) {
        names(phetay) <- paste0(rep("phi_", length(phetay)), c(1:length(phetay)))
        tmp <- stats::filter(y, phetay, sides = 1)
        Zy <- y[(length(phetay) + 1):ny] - tmp[length(phetay):(ny - 1)]
    } else {
        Zy <- y
    }
    Zy <- Zy - mean(Zy)
    
    ### Bootstrap
    library(parallel)
    cl <- parallel::makeCluster(parallel::detectCores())
    CCFs <- parallel::parSapply(cl, 1:B, function(b) {
        xboot <- arima.sim(list(order = c(length(phetax), 0, 0), ar = phetax), n = nx, 
                           innov = sample(Zx, size = nx, replace = TRUE))
        yboot <- arima.sim(list(order = c(length(phetay), 0, 0), ar = phetay), n = ny, 
                           innov = sample(Zy, size = ny, replace = TRUE))
        attributes(xboot) <- attributes(x)
        attributes(yboot) <- attributes(y)
        xrankboot <- rank(xboot)
        yrankboot <- rank(yboot)
        attributes(xrankboot) <- attributes(x)
        attributes(yrankboot) <- attributes(y)
        rPboot <- ccf(xboot, yboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
        rSboot <- ccf(xrankboot, yrankboot, lag.max = lag.max, plot = FALSE)$acf[,1,1]
        cbind(rPboot, rSboot)
    }, simplify = "array")
    #CCFs has dimensions of nlags * 2 (Pearson and Spearman) * B
    ### Confidence regions
    alpha <- 1 - level
    crP <- apply(CCFs[,1,], 1, quantile, probs = c(alpha/2, 1 - alpha / 2))
    crS <- apply(CCFs[,2,], 1, quantile, probs = c(alpha/2, 1 - alpha / 2))
    ### p-values
    pP <- sapply(1:dim(CCFs)[1L], function(i) mean(abs(CCFs[i,1,]) > abs(rP[i])))
    pS <- sapply(1:dim(CCFs)[1L], function(i) mean(abs(CCFs[i,2,]) > abs(rS[i])))
    RESULT <- data.frame(Lag = lags,
                         rP = rP, pP = pP, lowerP = crP[1,], upperP = crP[2,], #Pearson
                         rS = rS, pS = pS, lowerS = crS[1,], upperS = crS[2,]) #Spearman
    stopCluster(cl)
    ### Plotting
    if (plt == "Pearson") {
        TMP <- RESULT[,grepl("P", names(RESULT))]
    }
    if (plt == "Spearman") {
        TMP <- RESULT[,grepl("S", names(RESULT))]
    }
    if (plt == "Pearson" || plt == "Spearman") {
        matplot(lags, TMP[,-2], type = "n",
                xlab = "Lag", ylab = "CCF", 
                main = paste0(plt, " correlation of ", namex, "(t + Lag)", " and ", namey, "(t)\n",
                              "with ", level*100, "% bootstrapped confidence region"),
                las = 1)
        grid(nx = 2, ny = NULL, lty = 1)
        polygon(x = c(lags, rev(lags)),
                y = c(TMP[,3], rev(TMP[,4])),
                col =  adjustcolor("deepskyblue", alpha.f = 0.80),
                border = NA)
        lines(lags, TMP[,1], type = "h")
        points(lags, TMP[,1], pch = c(1, 16)[1 + (TMP[,2] < alpha)])
        return(invisible(RESULT))
    } else {
        return(RESULT)
    }
}

Sys.time(ccf_boot(x,y))
system.time(ccf_boot_new(x,y))
ccf_boot(x,y)

Rprof(tmp <- tempfile())
ccf_boot(x,y)
Rprof(NULL)
summaryRprof(tmp)

d = summaryRprof(tmp)
d$by.total[order(d$by.total$self.pct),]


library(parallel)
profvis(ccf_boot(x,y))


system.time(
    sapply(1:1000, function(i){
        x = rnorm(100000)
        mean(x)/sd(x)
    })
    )
cl <- parallel::makeCluster(parallel::detectCores())

system.time(
    parallel::parSapply(cl, 1:1000, function(i){
        x = rnorm(100000)
        mean(x) / sd(x)
    })
)

?parallel::par

?sapply
?ccf_boot
ccf_boot(car)
iris
