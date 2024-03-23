library(Deriv)

# sin wave
f  = function(x) sin(x)

# taylor approximation
taylor = function(x, fn, order, a = 0) {
  result = 0
  for (n in 0:order) {
    f_prime = Deriv(fn, c("x"), n = n)
    result = result + (eval(f_prime)(a) / factorial(n)) * (x - a)^n
  }
  return(result)
}

# data
x = seq(-2*pi, 2*pi, length.out = 100)

# taylor orders
order = c(1,10,20)

# plot
par(mfrow = c(2,2))
plot(x, f(x), type = "l", col = "black", lwd = 2, ylim = c(-2, 2), main = "sin(x)", xlab = "x", ylab = "f(x)")
colors = c('red', 'blue', 'purple')
for(i in seq_along(order)){
  plot(x, f(x), type = "l", col = "gray", lwd = 2, ylim = c(-2, 2), main = paste0("Taylor Approximation of sin(x): order ", order[i]), xlab = "x", ylab = "f(x)")
  lines(x, taylor(x, fn = f, order[i]), col = colors[i], lty = 2, lwd = 2)
}
