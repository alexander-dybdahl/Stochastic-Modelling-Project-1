library(ggplot2)
library(tidyr)
library(Hmisc)

n <- 300

gamma <- 0.1
alpha <- 0.005

I_0_vac = rep(0,n)
I_100_vac = rep(0,n)
I_600_vac = rep(0,n)
I_800_vac = rep(0,n)

I = matrix(c(I_0_vac, I_100_vac, I_600_vac, I_800_vac), nrow = n, ncol = 4)

vaccinated = c(0, 100, 600, 800)

for (k in 1:length(vaccinated)) {

  Y_0 <- c(950 - vaccinated[k], 50, 0)
  Y <- matrix(rep(0,3*n), nrow = n, ncol = 3)
  Y[1,] <- Y_0

  for (i in 2:n) {
    
    beta <- 0.5 * Y[i-1,2] / N
    
    RS <- sum(rbinom(Y[i-1,3], 1, alpha))
    SI <- sum(rbinom(Y[i-1,1], 1, beta))
    IR <- sum(rbinom(Y[i-1,2], 1, gamma))
    
    Y[i,] <- c(Y[i-1,1] + RS - SI, Y[i-1,2] + SI - IR, Y[i-1,3] + IR - RS)
  }
  I[,k] = Y[,2]
}

data <- data.frame(I)
colors <- c('0 vaccinated' = 'red', '100 vaccinated' = 'blue', '600 vaccinated' = 'green', '800 vaccinated' = 'yellow')

ggplot(data, aes(x = 1:n)) + 
  geom_line(aes(y = I[,1], color = '0 vaccinated'), size = 1) +
  geom_line(aes(y = I[,2], color = '100 vaccinated'), size = 1) +
  geom_line(aes(y = I[,3], color = '600 vaccinated'), size = 1) +
  geom_line(aes(y = I[,4], color = '800 vaccinated'), size = 1) +
  
  labs(title = 'number of infected with different vaccination programs', color = 'states') +
  xlab('steps (n)') + 
  ylab('number of infected') +
  scale_color_manual(values = colors)
