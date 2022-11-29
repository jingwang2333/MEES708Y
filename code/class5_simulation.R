library(gamlss.tr)
gamlss.tr::trun(family = "NBI",
                par = 100L,
                type = "right")
rNBItr(5)
?ts.plot

MC <- 10000

RES <- as.numeric(rep(NA, length = MC))
for (mc in 1:MC){
    #x= rnorm(15)
    #y= rnorm(15)
    x= rNBI(15)
    y= rNBI(15)
    RES[mc] = t.test(x, y)$p.value
}
alpha = 0.05
mean(RES < alpha)

M <- matrix(NA, nrow = n, ncol = MC)
system.time({
    for(mc in 1:MC){
        M[,mc] <- arima.sim(n + 100, model = list(order = c(1,0,0),
                                                  ar = pho))[101:(n + 100)]
    }
})

system.time({
    M2 <- sapply(1:MC, function(x))
    arima.sim(n + 100, model = list(order = c(1,0,0),
                                    ar = phi))[101:(n + 100)])
})

# sample function
c(1:5)
sample(c(1:5))
sample(c(1:5),3)
sample(c(1:5),3, replace = T)

?sapply

# loop function -----
lapply(list, function)
sapply(list, function)
tapply(vector, index, function)
?tapply
