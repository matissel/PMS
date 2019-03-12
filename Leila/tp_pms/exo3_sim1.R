x <- scan("Peche.txt")
#question 1
n <- 1000
n0 <- 100
s <- sum(x)
theta_n <- n * n0 / s
sprintf("l'estimation de theta est %f",theta_n)
alpha = 0.05
A1 <- n0 * (1 + (((n - s + 1) * qf(1 - alpha / 2, 2 * (n - s + 1), 2 * s)) / s))
A2 <- n0 * (1 + (((n - s) * qf(alpha / 2, 2 * (n - s), 2 * (s + 1))) / (s + 1)))
sprintf("l'intervalle de confiance exact est [%f, %f]", A2, A1)
A3 <- n0 / (s / n + qnorm(1 - alpha / 2) * sqrt((s / n) * (1 - s / n) / n))
A4 <- n0 / (s / n - qnorm(1 - alpha / 2) * sqrt((s / n) * (1 - s / n) / n))
sprintf("l'intervalle de confiance asymptotique est [%f, %f]", A3, A4)
#question2
fonction <- function(x) {
    y <- integer(sum(x))
    j <- 1
    for (i in 1:length(x)) {
        if (j > sum(x)) {
            break
        }
        y[j] <- y[j] + 1
        if (x[i] == 1) {
            j <- j + 1
        }
    }
    return(y)
}
y <- fonction(x)
m <- sum(x)
N <- sum(y)
theta_m <- n0 * N / m
sprintf("l'estimateur de la deuxieme strategie donne: %f", theta_m)
Im <- m / (theta_m * (theta_m - n0))
A5 <- theta_m - qnorm(1 - alpha / 2) / sqrt(Im)
A6 <- theta_m + qnorm(1 - alpha / 2) / sqrt(Im)
sprintf("l'intervalle de confiance avec la deuxieme strategie est: [%f, %f]", A5, A6)
#graphe de probabilité
# la fonction de répartition de Y est F(x) = 1 - (1-p)^(int(x)) => log(1-p)*int(x) = log(1-F(x)) et int(yi) = yi donc le graphe est (yi*,log(1-i/n))
plot(sort(y)[1:(m - 1)], log(1 - seq(1:(m - 1))/m ))
#hypothese pertinente ...
