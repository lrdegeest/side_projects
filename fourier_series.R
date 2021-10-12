# DFT
## create the wave
tt = seq(from = 0, to = 5, by = 0.01)
m = matrix(nrow = length(tt), ncol = 10)
for(k in 1:ncol(m)) m[,k] = 1/k * sin(2 * pi *  k * tt + k^2)
y = rowSums(m)

## plot the wave and harmonics
par(mfrow = c(1,2))
colors = colorRampPalette(c("darkblue", "lightblue"))(10)
plot(y, type = 'l', lwd = 2, col = 'black', xlab = 'Time', ylab = 'y(t)', main = 'Observed Wave')
plot(y, type = 'l', col = 'white', xlab = 'Time', ylab = 'y(t)', main = 'Unobserved Harmonics')
for(i in 1:ncol(m)) lines(m[,i], type = 'l', col= colors[i]) 
dev.off()

## plot the wave and inverse DFT on all coefficients
z = fft(y)
par(mfrow = c(1,2))
plot(y, type = 'l', lwd = 2, col = 'black', xlab = 'Time', ylab = 'y(t)', main = 'Observed Wave')
plot(Re(fft(z, inverse = TRUE)/length(z)), type = 'l', lwd = 2, col = 'blue', xlab = 'Time', ylab = 'y(t)', main = 'DFT Recovered Wave')
dev.off()

## plot absolute values of the coefficients
### Mod(z) is just sqrt(Re(z)^2 + Im(z^2))
hist(Mod(z), breaks = 200, xlab = "Absolute value of Fourier coefficients")

## approximate wave with largest coefficients
plot(y, type = 'l', ylab = "y(t)", xlab = "Time", main = 'Observed Wave (black) and Approximation (blue)')
zr = round(z)
zr[Mod(zr)<sort(Mod(zr), TRUE)[10]] = 0
lines(Re(fft(zr, inverse = T)/length(y)), type = 'l', col = 'blue', lwd = 2)


# dynamic harmonic regression
library(forecast)
library(ggplot2)
data("wineind")

df = window(wineind, start = 1985)
autoplot(df) + labs(y = "Wine Sales") + theme_minimal()

# plot forecasts with various fourier orders
# this is adapted from RH's book
n_k = 6L
plots = vector(mode = "list", length = n_k)
for (i in 1:n_k){
  fit = auto.arima(df, xreg = fourier(df, K = i),
                   seasonal = FALSE, lambda = 0)
  plots[[i]] = autoplot(
    forecast(fit, xreg=fourier(df, K = i, h=24)), title = NULL) +
    labs(title = paste("K = ", i), y = "Wine Sales") + 
    theme_minimal()
}
patchwork::wrap_plots(plots)

# show the decreasing AIC
res_aic = rep(NA, 6)
for(i in seq_along(res_aic)){
  fit = auto.arima(df, xreg = fourier(df, K = i), seasonal = FALSE, lambda = 0)
  res_aic[i] = fit$aicc
}
plot(res_aic, type = 'l', xlab = "K (Fourier order, # of sines/cosines)", ylab = "AIC")


harmonics = fourier(df, K = 6)
par(mar = c(1,4,1,1), mfrow = c(6,2))
for(i in 1:ncol(harmonics)){
  plot(harmonics[,i], type = 'l', xlab = "Time", ylab = colnames(harmonics)[i])
}
par(mar = rep(4, 4), mfrow=c(1,1))
