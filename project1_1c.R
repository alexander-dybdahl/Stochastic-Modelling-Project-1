library(ggplot2)

beta <- 0.01
gamma <- 0.1
alpha <- 0.005
P <- matrix(c(1 - beta, 0, alpha, beta, 1 - gamma,
              0, 0, gamma, 1 - alpha), nrow <- 3, ncol <- 3)

N <- 30
days <- 7300

states <- matrix(rep(0, 3 * N), nrow <- N, ncol <- 3)

for (j in 1:N) {
  state <- rep(0, days + 1)
  state[1] <- 0

  for (i in 1:days) {
    previous_day <- state[i]
    state_today_probabilities <- P[previous_day + 1, ]

    state[i + 1] <- sample(c(0, 1, 2),
    size <- 1, replace = FALSE, prob <- state_today_probabilities)
  }

  states[j, ] <- table(state[3651:days + 1]) / 3650
}

state_mean_exact <- round(c(10, 1, 20) / 31, 4)
state_mean_exact * 365

state_mean <- round(c(mean(states[, 1]),
      mean(states[, 2]), mean(states[, 3])), 4)
state_mean * 365

error1 <- round(qnorm(.975) * sqrt(var(states[, 1])) / sqrt(N), 4)
(state_mean[1] - error1) * 365
(state_mean[1] + error1) * 365

error2 <- round(qnorm(.975) * sqrt(var(states[, 2])) / sqrt(N), 4)
(state_mean[2] - error2) * 365
(state_mean[2] + error2) * 365

error3 <- round(qnorm(.975) * sqrt(var(states[, 3])) / sqrt(N), 4)
(state_mean[3] - error3) * 365
(state_mean[3] + error3) * 365


df <- data.frame(x <- states[, 1],
                  y <- states[, 2],
                  z <- states[, 3])

colors <- c("susceptiple" <- "red",
  "infected" <- "blue", "recovered" <- "green")

ggplot(df, aes(x <- 1:N)) +
  geom_line(aes(y <- states[, 1], color <- "susceptiple"), size <- 1) +
  geom_hline(yintercept <- state_mean[1],
    linetype <- "dashed", color <- "black") +
  geom_text(x <- 6, y <- state_mean[1] - 0.015,
    label <- sprintf("empirical mean <- %s +- %s",
    state_mean[1], error1), size <- 3) +
  geom_text(x <- 5, y <- state_mean[1] - 0.04,
    label <- sprintf("theoretical mean <- %s",
    state_mean_exact[1]), size <- 3, color <- "red") +

  geom_line(aes(y <- states[, 2], color <- "infected"), size <- 1) +
  geom_hline(yintercept <- state_mean[2],
    linetype <- "dashed", color <- "black") +
  geom_text(x <- 6, y <- state_mean[2] - 0.015,
    label <- sprintf("empirical mean <- %s +- %s",
    state_mean[2], error2), size <- 3) +
  geom_text(x <- 5, y <- state_mean[2] - 0.04,
    label <- sprintf("theoretical mean <- %s",
    state_mean_exact[2]), size <- 3, color <- "red") +

  geom_line(aes(y <- states[, 3], color <- "recovered"), size <- 1) +
  geom_hline(yintercept <- state_mean[3],
    linetype <- "dashed", color <- "black") +
  geom_text(x <- 6, y <- state_mean[3] - 0.015,
    label <- sprintf("empirical mean <- %s +- %s",
    state_mean[3], error3), size <- 3) +
  geom_text(x <- 5, y <- state_mean[3] - 0.04,
    label <- sprintf("theoretical mean <- %s",
    state_mean_exact[3]), size <- 3, color <- "red") +

  labs(title <- "long-run of states", color <- "states") +
  xlab("simulation") +
  ylab("probability of states in the long-run")
