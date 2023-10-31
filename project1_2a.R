library(ggplot2)
library(latex2exp)

N <- 1000
claims <- 100

lambda <- 1.5
t_end <- 59

probability <- sum(rpois(N, lambda = lambda * t_end) > claims) / N
probability

probability_exact <- 1 - ppois(100, lambda = lambda * t_end)
probability_exact


sim <- 10

labels <- c('1','2','3','4','5','6','7','8','9','10')
colors <- c('red','green','blue','yellow','brown','purple','cyan','violet','lightblue','pink')
plot(NULL, NULL, xlim = c(0, t_end), ylim = c(0,100), xlab = 'days (t)', ylab = 'claim amount (mill. kr)', main = TeX(r'(X(t), $0 \leq t \leq 59$, $\lambda = 1.5$)'))
for (j in 1:sim) {
  
  nP <- rpois(1, lambda = lambda * t_end)
  t_val <- runif(nP, min = 0, max = t_end)
  t_val <- c(0,sort(t_val), t_end)
  x_val <- c(0:nP)
  
  for (i in 1:(length(x_val)-1)) {
    lines(t_val[i:(i+1)], rep(x_val[i], 2), col = colors[j], lwd = 2)
  }
}

legend('topleft', legend = labels, col = colors, lty=1:2, cex=0.8, title = 'simulation', title.cex = 1, title.font = 1)
