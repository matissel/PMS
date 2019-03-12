n0 <- 50
theta <- 1000
m <- 200
p <- n0 / theta
x <- rgeom(m, p) + 1
moy <- mean(x)
vari <- var(x)
E <- 1 / p
V <- (1 - p) / (p ** 2)
sprintf("la moyenne empirique est: %f", moy)
sprintf("la moyenne théorique est: %f", E)
sprintf("la variance empirique est: %f", vari)
sprintf("la variance théorique est: %f", V)
N <- sum(x)
sprintf("La réalisation de N est n = %d", N)
theta_m <- n0 * moy
sprintf("l'estimateur pour cette experience est theta_m = %f", theta_m)
Im <- m / (theta_m * (theta_m - n0))
alpha <- 0.01
sprintf("l'intervalle de confiance est [%f, %f].", theta_m - qnorm(1-alpha/2)/sqrt(Im), theta_m + qnorm(1-alpha/2)/sqrt(Im))