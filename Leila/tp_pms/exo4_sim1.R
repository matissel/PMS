n <- 1000
theta <- 2000
n0 <- 200
m <- 40
alpha <- .2
#A et B bornes des intervalles exacts
A <- integer(m)
B <- integer(m)
#C et D bornes pour les intervalles asymptotiques
C <- integer(m)
D <- integer(m)
#proportions des intervalles exacts
pe <- 0
#proportions des intervalles asymptotiques
pa <- 0
for (i in 1:m) {
    x <- rbinom(n, 1, n0 / theta)
    s <- sum(x)
    A[i] <- n0 * (1 + (((n - s) * qf(alpha / 2, 2 * (n - s), 2 * (s + 1))) / (s + 1)))
    B[i] <- n0 * (1 + (((n - s + 1) * qf(1 - alpha / 2, 2 * (n - s + 1), 2 * s)) / s))
    cat("intervalle exact :[", A[i], B[i],"]\n")
    if (A[i] < theta & theta  < B[i]) {
      pe <- pe + 1
    }
    C[i] <- n0 / (s / n + qnorm(1 - alpha / 2) * sqrt((s / n) * (1 - s / n) / n))
    D[i] <- n0 / (s / n - qnorm(1 - alpha / 2) * sqrt((s / n) * (1 - s / n) / n))
    cat("intervalle asymptotique :[", C[i], D[i],"]\n")
    if (C[i] < theta & theta < D[i]) {
      pa <- pa + 1
    }
    
}
cat("1 - alpha = ", 1 - alpha)
cat("la proportion des intervalles exacts contenant theta est :", pe/m)
cat("la proportion des intervalles asymptotiques contenant theta est :", pa/m)

