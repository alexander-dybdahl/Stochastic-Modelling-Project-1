library(ggplot2)
library(latex2exp)

N <- 1000

lambda <- 1.5
gamma <- 10
t_end <- 59

Z <- rep(0, N)
X <- rpois(N,  lambda * t_end)

for (j in 1:N) {
  C <- rexp(X[j], gamma)
  Z[j] <- sum(C)
}

Z_prob <- sum(Z > 8) / N
Z_prob

C <- rexp(N, gamma)

X <- rpois(N, lambda)

Y_prob <- mean(X) * sum(C > 0.25) / N
Y_prob

Y_prob_exact <- lambda * exp(-0.25*gamma)
Y_prob_exact

sim <- 10

labels <- c('1','2','3','4','5','6','7','8','9','10')
colors <- c('red','green','blue','yellow','brown','purple','cyan','violet','lightblue','pink')
plot(NULL, NULL, xlim = c(0, t_end), ylim = c(0,10), xlab = 'days (t)', ylab = 'claim amount (mill. kr)', main = TeX(r'(Z(t), $0 \leq t \leq 59$, $\lambda = 1.5$, $\gamma = 10$)'))
for (j in 1:sim) {
  
  nP <- rpois(1, lambda = lambda * t_end)
  t_val <- runif(nP, min = 0, max = t_end)
  t_val <- c(0,sort(t_val), t_end)
  x_val <- c(0:nP, nP)
  z_val <- rep(0,nP)
  
  for(i in 2:nP) {
    c_val <- rexp(1, gamma)
    z_val[i] <- z_val[i-1] + c_val
  }
  
  for (i in 1:nP) {
    lines(t_val[i:(i+1)], rep(z_val[i], 2), col = colors[j], lwd = 2)
  }
}
lines(c(0,t_end), c(8,8), col = 'red', lwd = 2, lty = 2)
legend('topleft', legend = labels, col = colors, lty=1:2, cex=0.8, title = 'simulation', title.cex = 1, title.font = 1)


