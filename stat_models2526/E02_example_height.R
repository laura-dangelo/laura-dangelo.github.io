x = c(153.7,156.7,173.5,157.0,161.8,140.7,179.8,150.9,154.4,162.3,166.6)
y =  c(163.1,159.5,169.4,158.0,164.3,150.0,170.3,158.9,161.5,160.8,160.6)
n = length(y)

cbind(y,x)

plot(x, y, xlab = "x (mother height)", ylab = "y (daughter height)", col="steelblue2",
     pch=19,
     cex=2)

mod = lm(y~x)
abline(mod, col=2, lwd=2)

summary(mod)

beta2_hat = cov(x, y) / var(x)
beta1_hat = mean(y) - beta2_hat * mean(x)

# var_beta1_hat = sigma2 * (1/n + mean(x)^2/((n-1)*var(x)))
# var_beta2_hat = sigma2 / ((n-1) * var(x))


yhat = predict(mod)
yhat
points(x, yhat, pch = 19)


s2 = sum( (y - yhat)^2 ) / (n-2)
sqrt(s2) # residual standard error

var_beta1_hat = s2 * (1/n + mean(x)^2/((n-1)*var(x)))
var_beta2_hat = s2 / ((n-1) * var(x))

sqrt(var_beta1_hat)
sqrt(var_beta2_hat)

# t value
t1_obs = beta1_hat/sqrt(var_beta1_hat)
t2_obs = beta2_hat/sqrt(var_beta2_hat)

# pvalue
2 * (1 - pt(abs(t1_obs), n-2))
2 * (1 - pt(abs(t2_obs), n-2))

# sum of squares decomposition
SSE = sum( (y - yhat)^2 ) 
SST = sum( (y - mean(y))^2 ) 
SSR = sum( (yhat - mean(y))^2 ) 

SSE + SSR 
SST

plot(x, y, xlab = "x (mother height)", ylab = "y (daughter height)", col="steelblue2",
     pch=19,
     cex=2)
abline(mod, col="coral", lwd=2) # model
abline(lm(y~1), col = "forestgreen" , lwd=2) # null model

points(x, yhat, cex=1.1, lwd=2, col="coral") # predicted
points(x, predict(lm(y~1)), cex=1.1, lwd=2 , col = "forestgreen") # predicted under the null model: overall mean

segments(x0 = x, y0 = y, x1 = x, y1 = yhat, lty = 2)
segments(x0 = x, y0 = y, x1 = x, y1 = mean(y), lty = 3)
segments(x0 = x, y0 = yhat, x1 = x, y1 = mean(y), lty = 4)

# R^2
R2 = SSR / SST

# F test
Fobs = R2/(1-R2)*9
1 - pf(Fobs, 1, 9) # pvalue



plot(mod)




