library(ggplot2)
library(tidyr)
library(Hmisc)


N <- 1000
sim <- 1000
n <- 300

gamma <- 0.1
alpha <- 0.005

Y_0 <- c(950, 50, 0)
Y <- matrix(rep(0,3*n), nrow = n, ncol = 3)
Y[1,] <- Y_0

I_max = rep(0,sim)
I_max_time = rep(0,sim)


for (j in 0:sim) {
  
  for (i in 2:n) {
    
    beta <- 0.5 * Y[i-1,2] / N
    
    RS <- sum(rbinom(Y[i-1,3], 1, alpha))
    SI <- sum(rbinom(Y[i-1,1], 1, beta))
    IR <- sum(rbinom(Y[i-1,2], 1, gamma))
    
    Y[i,] <- c(Y[i-1,1] + RS - SI, Y[i-1,2] + SI - IR, Y[i-1,3] + IR - RS)
  }
  
  I_max[j] = max(Y[,2])
  I_max_time[j] = which(Y[,2] == max(Y[,2]))
  
}

{
  data <- data.frame(Y[,1], Y[,2], Y[,3])
  colors <- c('susceptiple' = 'red', 'infected' = 'blue', 'recovered' = 'green')
  
  ggplot(data, aes(x = 1:n)) + 
    geom_line(aes(y = Y[,1], color = 'susceptiple'), size = 1) +
    geom_line(aes(y = Y[,2], color = 'infected'), size = 1) +
    
    geom_hline(yintercept = I_max[sim], linetype = 'dashed', color = 'red') + 
    geom_vline(xintercept = I_max_time[sim], linetype = 'dashed', color = 'red') + 
    
    geom_line(aes(y = Y[,3], color = 'recovered'), size = 1) +
    labs(title = 'simulation of states for N individuals', color = 'states') +
    xlab('steps (n)') + 
    ylab('individuals') +
    scale_color_manual(values = colors)
  
}


error = qnorm(.975) * sqrt(var(I_max))/sqrt(N)
mean(I_max) - error
mean(I_max) + error


error = qnorm(.975) * sqrt(var(I_max_time))/sqrt(N)
mean(I_max_time) - error
mean(I_max_time) + error

